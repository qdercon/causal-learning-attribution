import pandas as pd
import os
import random
import itertools
import json

## we have a csv with the trials, with columns for valence ("positive" or "negative"), interpersonal content ("yes" or "no")
all_imgs = pd.read_csv(os.path.join(os.path.dirname(__file__), 'objects.csv'))

## first we want to select the images - to match with learning task we need 180 images for 90 trials-worth of images
## of these, 90 are the 'internal-global' images (natural-smaller than shoebox), and then 30 each of the other three categories
## 'internal-specific' (natural-larger), 'external-global' (humanmade-smaller), 'external-specific' (humanmade-larger)
## in the all_imgs dataframe, first select the 90 lowest scores for the 'natural-smaller' score (as not quite enough for all perfect matches)

internal_global = [f"{os.path.splitext(img)[0]}.png" for img in all_imgs.sort_values(by='natural_small').head(90)['assignedName'].tolist()]
internal_specific = [f"{os.path.splitext(img)[0]}.png" for img in all_imgs[(all_imgs['humanmade_response'] == 'natural') & (all_imgs['shoebox_response'] == 'larger than a shoebox')].head(30)['assignedName'].tolist()]
external_global = [f"{os.path.splitext(img)[0]}.png" for img in all_imgs[(all_imgs['humanmade_response'] == 'human-made') & (all_imgs['shoebox_response'] == 'smaller than a shoebox')].head(30)['assignedName'].tolist()]
external_specific = [f"{os.path.splitext(img)[0]}.png" for img in all_imgs[(all_imgs['humanmade_response'] == 'human-made') & (all_imgs['shoebox_response'] == 'larger than a shoebox')].head(30)['assignedName'].tolist()]

# --- Generate 6 sessions with all constraints ---
random.seed(123)

def make_blocks(images, n_blocks, block_size, n_sessions=None, max_retries=200):
    """
    Assign each image to two different blocks.
    If n_sessions is provided, ensures an image is not used twice in the same session.
    Retries until a valid assignment is found.
    """
    if n_sessions:
        if n_blocks % n_sessions != 0:
            raise ValueError("n_blocks must be divisible by n_sessions")
        n_blocks_per_session = n_blocks // n_sessions
        def get_session_idx(block_idx):
            return block_idx // n_blocks_per_session

    for attempt in range(max_retries):
        img_pool = images * 2
        random.shuffle(img_pool)
        blocks = [[] for _ in range(n_blocks)]
        img_placements = {img: [] for img in images}
        
        possible = True
        for img in img_pool:
            placed = False
            # Try to place in less full blocks first to improve chances
            block_indices = list(range(n_blocks))
            random.shuffle(block_indices)
            block_indices.sort(key=lambda i: len(blocks[i]))

            for block_idx in block_indices:
                if len(blocks[block_idx]) >= block_size:
                    continue
                if img in blocks[block_idx]:
                    continue
                
                # If session-awareness is on, check that we're not placing in the same session twice
                if n_sessions and len(img_placements[img]) == 1:
                    first_block_idx = img_placements[img][0]
                    if get_session_idx(block_idx) == get_session_idx(first_block_idx):
                        continue

                blocks[block_idx].append(img)
                img_placements[img].append(block_idx)
                placed = True
                break
            
            if not placed:
                possible = False
                break
        
        if not possible:
            continue

        # Check if all constraints are met
        if all(len(b) == block_size for b in blocks):
            all_imgs_in_blocks = [i for b in blocks for i in b]
            if all(all_imgs_in_blocks.count(img) == 2 for img in images):
                return blocks  # Success

    raise RuntimeError(f"Failed to create valid blocks after {max_retries} retries.")


def make_valence_labels(n_blocks, block_size):
    """Return a list of n_blocks lists, each with 5 'positive' and 5 'negative', shuffled."""
    labels = []
    for _ in range(n_blocks):
        block = ['positive'] * (block_size // 2) + ['negative'] * (block_size // 2)
        random.shuffle(block)
        labels.append(block)
    return labels

def make_sessions():
    n_sessions = 6
    n_total_blocks = n_sessions * 3
    block_size = 10

    # Make blocks for each group
    int_glob_blocks = make_blocks(internal_global, n_blocks=n_total_blocks, block_size=block_size, n_sessions=n_sessions)
    
    # For comparators, each of the 6 blocks corresponds to a session, so no intra-session repetition is possible
    int_spec_blocks = make_blocks(internal_specific, n_blocks=n_sessions, block_size=block_size)
    ext_glob_blocks = make_blocks(external_global, n_blocks=n_sessions, block_size=block_size)
    ext_spec_blocks = make_blocks(external_specific, n_blocks=n_sessions, block_size=block_size)
    
    valence_blocks = make_valence_labels(n_total_blocks, block_size)

    comparator_block_map = {
        'int_spec': int_spec_blocks,
        'ext_glob': ext_glob_blocks,
        'ext_spec': ext_spec_blocks,
    }
    
    comp_types = ['int_spec', 'ext_glob', 'ext_spec']
    orderings = list(itertools.permutations(comp_types)) # 6 permutations for 6 sessions

    sessions = []
    for s in range(n_sessions):
        session = {
            'int_glob': [],
            'int_spec': ['dummy'] * (block_size * 3),
            'ext_glob': ['dummy'] * (block_size * 3),
            'ext_spec': ['dummy'] * (block_size * 3),
            'event_valence': []
        }
        
        # Assign a unique ordering of comparator types to each session
        ordering = orderings[s]

        for b in range(3): # 3 blocks per session
            # Assign int_glob and valence
            block_idx = s * 3 + b
            session['int_glob'].extend(int_glob_blocks[block_idx])
            session['event_valence'].extend(valence_blocks[block_idx])

            # Assign comparator images for this block based on the session's ordering
            comp_type = ordering[b]
            comp_block_images = comparator_block_map[comp_type][s]
            start_index = b * block_size
            end_index = start_index + block_size
            session[comp_type][start_index:end_index] = comp_block_images
            
        sessions.append(session)
    return sessions

def verify_sessions(sessions, all_images_map):
    """Comprehensive check of all constraints across all sessions."""
    print("\n--- Running Verification ---")
    n_sessions = len(sessions)
    
    all_used_images = {cat: [] for cat in all_images_map.keys()}
    
    for i, s in enumerate(sessions):
        print(f"\nVerifying Session {i}...")
        # Check counts per category
        for cat, expected_count in [('int_glob', 30), ('int_spec', 10), ('ext_glob', 10), ('ext_spec', 10)]:
            if cat == 'int_glob':
                actual_count = len(s[cat])
            else:
                actual_count = len([img for img in s[cat] if img != 'dummy'])
            assert actual_count == expected_count, f"Session {i}, {cat}: expected {expected_count}, got {actual_count}"
            print(f"  - {cat} count: OK ({actual_count})")

        # Check valence balance for the whole session
        total_valence_labels = len(s['event_valence'])
        pos_count = s['event_valence'].count('positive')
        neg_count = s['event_valence'].count('negative')
        assert total_valence_labels == 30, f"Session {i}: Expected 30 valence labels, got {total_valence_labels}"
        assert pos_count == 15 and neg_count == 15, f"Session {i}: Valence not balanced. Pos: {pos_count}, Neg: {neg_count}"
        print(f"  - Session valence balance: OK (15 pos, 15 neg)")

        # Check valence balance per block
        for b in range(3):
            block_start = b * 10
            block_end = block_start + 10
            block_valence = s['event_valence'][block_start:block_end]
            pos_count = block_valence.count('positive')
            neg_count = block_valence.count('negative')
            assert pos_count == 5 and neg_count == 5, f"Session {i}, Block {b}: Valence not balanced. Pos: {pos_count}, Neg: {neg_count}"
        print(f"  - Per-block valence balance: OK (5 pos, 5 neg for all 3 blocks)")

        # Check for duplicates within a session's int_glob images
        assert len(s['int_glob']) == len(set(s['int_glob'])), f"Session {i}: Duplicate int_glob images found within the session."
        print(f"  - No duplicate int_glob images in session: OK")

        # Collect all images used in this session
        for cat in all_images_map.keys():
            if cat == 'int_glob':
                all_used_images[cat].extend(s[cat])
            else:
                all_used_images[cat].extend([img for img in s[cat] if img != 'dummy'])

    print("\nVerifying image usage across all sessions...")
    for cat, images in all_images_map.items():
        usage_counts = {img: all_used_images[cat].count(img) for img in images}
        if not all(count == 2 for count in usage_counts.values()):
             raise AssertionError(f"Category {cat}: Not all images used exactly twice. Counts: { {k:v for k,v in usage_counts.items() if v != 2} }")
        print(f"  - {cat}: All {len(images)} images used exactly twice: OK")

    print("\n--- Verification Complete: All constraints met! ---")

def export_to_js(sessions, file_path):
    """Exports the sessions and image paths to a JavaScript file with comments."""
    # Create allimgs structure for preloading
    allimgs_per_session = []
    for s in sessions:
        session_images = set()
        session_images.update(s['int_glob'])
        session_images.update(img for img in s['int_spec'] if img != 'dummy')
        session_images.update(img for img in s['ext_glob'] if img != 'dummy')
        session_images.update(img for img in s['ext_spec'] if img != 'dummy')
        
        # Format paths to pngs in the tlc folder
        formatted_paths = [f"./assets/img/tlc/{os.path.splitext(img)[0]}.png" for img in sorted(list(session_images))]
        allimgs_per_session.append(formatted_paths)

    # Get the sessions as a JSON string and add comments
    js_sessions_object = json.dumps(sessions, indent=4)
    js_sessions_object = js_sessions_object.replace('"int_glob":', '"int_glob": [      // natural-smaller')
    js_sessions_object = js_sessions_object.replace('"int_spec":', '"int_spec": [      // natural-larger')
    js_sessions_object = js_sessions_object.replace('"ext_glob":', '"ext_glob": [      // humanmade-smaller')
    js_sessions_object = js_sessions_object.replace('"ext_spec":', '"ext_spec": [      // humanmade-bigger')
    js_sessions_object = js_sessions_object.replace('"event_valence":', '"event_valence": [      // blue vs red')

    js_allimgs_object = json.dumps(allimgs_per_session, indent=4)
    
    js_code = f"export var controlSessions = {js_sessions_object};\n\nexport var allimgs = {js_allimgs_object};"
    
    js_dir = os.path.dirname(file_path)
    if not os.path.exists(js_dir):
        os.makedirs(js_dir)

    with open(file_path, 'w') as f:
        f.write(js_code)
    print(f"\nSuccessfully exported sessions and image paths to {file_path}")


if __name__ == "__main__":
    sessions = make_sessions()
    
    all_images_map = {
        'int_glob': internal_global,
        'int_spec': internal_specific,
        'ext_glob': external_global,
        'ext_spec': external_specific,
    }
    
    # Verification
    verify_sessions(sessions, all_images_map)

    # JS Export
    script_dir = os.path.dirname(__file__)
    js_export_path = os.path.abspath(os.path.join(script_dir, '..', '..', 'public', 'js', 'trialsControl.js'))
    export_to_js(sessions, js_export_path)
