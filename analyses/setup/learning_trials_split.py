## quick script to split learning trials into 6 training sets of 30 trials (3 blocks of 10 trials each)
## we want the blocks to be balanced in terms of (1) valence and (2) interpersonal content
## in addition, we have some trials from a test set that we want to include in the training sets, these are higher discriminability, so want them
## to be balanced across the training sets as well
## finally, we don't want to use the same trial more than twice (we have 92, and we need 180 total)

import pandas as pd
import os
import numpy as np
import math
from itertools import combinations

## we have a csv with the trials, with columns for valence ("positive" or "negative"), interpersonal content ("yes" or "no")
event_coding = pd.read_csv(os.path.join(os.path.dirname(__file__), 'event_coding.csv'))

# these are the **zero-indexed** numbers of (1) the high discriminability trials (to be evenly split over training sessions) and (2) the test set trials (DO NOT include in these training sets)
# i.e., they correspond to the row numbers in the event_coding dataframe (excluding the header) OR the "item" column minus 1
high_discrim = [11, 21, 25, 28, 29, 31, 35, 37, 44, 45, 47, 53, 55, 56, 59, 61, 67, 73, 75, 76, 77, 78, 83, 84, 97, 100, 102, 108, 110, 115, 120, 125]
test_set = [0, 1, 2, 3, 5, 7, 8, 13, 15, 17, 18, 22, 24, 32, 39, 41, 43, 54, 62, 68, 72, 74, 79, 93, 99, 101, 104, 105, 112, 113, 116, 127]

# Function to split learning trials into 6 sets of 30, balanced for valence, interpersonal, and high_discrim
def split_learning_trials(event_coding, high_discrim, test_set, n_sets=6, set_size=30, max_uses=2, random_state=42, block_balance=False):
    """
    Returns a list of 6 sets (each a list of 30 zero-indexed item numbers) for learning trials,
    balanced for valence and interpersonal content, and distributing high_discrim trials evenly.
    Also prints the counts of positive/negative, interpersonal/non-interpersonal, and high_discrim/non-high_discrim for each set.
    """
    np.random.seed(random_state)
    # Get available items (not in test_set)
    all_items = set(range(len(event_coding)))
    available_items = np.array(sorted(list(all_items - set(test_set))))
    # Track how many times each item is used
    item_use_count = {i: 0 for i in available_items}
    # Prepare masks for balancing
    valence_mask = {v: set(event_coding[event_coding['valence'] == v].index) & set(available_items) for v in ['positive', 'negative']}
    interpersonal_mask = {v: set(event_coding[event_coding['interpersonal'] == v].index) & set(available_items) for v in ['yes', 'no']}
    high_discrim_set = set(high_discrim) & set(available_items)
    non_high_discrim = set(available_items) - high_discrim_set

    sets = []
    # Assign each high_discrim item to two different sets, round-robin, then shuffle within each set
    high_discrim_items = list(high_discrim_set)
    np.random.shuffle(high_discrim_items)
    high_discrim_assignments = [[] for _ in range(n_sets)]
    for idx, item in enumerate(high_discrim_items * max_uses):
        set_idx = idx % n_sets
        high_discrim_assignments[set_idx].append(item)
    for s in high_discrim_assignments:
        seen = set()
        s[:] = [x for x in s if not (x in seen or seen.add(x))]

    # Track last usage for each non-high_discrim item
    last_used = {i: -math.inf for i in non_high_discrim}
    non_high_discrim_list = list(non_high_discrim)
    np.random.shuffle(non_high_discrim_list)
    for set_num in range(n_sets):
        this_set = list(high_discrim_assignments[set_num])
        for i in this_set:
            item_use_count[i] += 1
        needed = set_size - len(this_set)
        # For available items, sort by last_used (prefer never used, then least recently used)
        available_for_set = [i for i in non_high_discrim_list if item_use_count[i] < max_uses and i not in this_set]
        available_for_set.sort(key=lambda x: last_used[x])
        # Now balance valence/interpersonal as before, but with this order
        pos_yes = [i for i in available_for_set if i in valence_mask['positive'] and i in interpersonal_mask['yes']]
        pos_no = [i for i in available_for_set if i in valence_mask['positive'] and i in interpersonal_mask['no']]
        neg_yes = [i for i in available_for_set if i in valence_mask['negative'] and i in interpersonal_mask['yes']]
        neg_no = [i for i in available_for_set if i in valence_mask['negative'] and i in interpersonal_mask['no']]
        block_targets = [needed // 4] * 4
        for i in range(needed % 4):
            block_targets[i] += 1
        picks = []
        for group, target in zip([pos_yes, pos_no, neg_yes, neg_no], block_targets):
            for _ in range(target):
                if group:
                    picks.append(group.pop(0))
        if len(picks) < needed:
            for i in available_for_set:
                if i not in picks:
                    picks.append(i)
                if len(picks) == needed:
                    break
        for i in picks:
            item_use_count[i] += 1
            last_used[i] = set_num
        this_set.extend(picks[:needed])
        sets.append(this_set)

    # --- Replace non-high_discrim items used twice with unused items, to minimize unused items ---
    unused_items = [i for i, count in item_use_count.items() if count == 0 and i not in high_discrim_set]
    used_twice = [i for i, count in item_use_count.items() if count == 2 and i not in high_discrim_set]
    for unused in unused_items:
        replaced = False
        for set_idx, s in enumerate(sets):
            for idx, item in enumerate(s):
                if item in used_twice and item not in high_discrim_set:
                    # Replace this item with the unused one
                    sets[set_idx][idx] = unused
                    item_use_count[unused] = 1
                    item_use_count[item] -= 1
                    replaced = True
                    used_twice.remove(item)
                    break
            if replaced:
                break

    # --- For each set, permute order to optimize block-level balance ---
    if block_balance:
        def block_score(block):
            vals = event_coding.loc[block, 'valence'].value_counts()
            inters = event_coding.loc[block, 'interpersonal'].value_counts()
            n_high = len([i for i in block if i in high_discrim_set])
            # Score is sum of absolute differences from ideal (5/5 valence, 5/5 interpersonal, 3 or 4 high_discrim)
            score = abs(vals.get('positive',0) - 5) + abs(vals.get('negative',0) - 5)
            score += abs(inters.get('yes',0) - 5) + abs(inters.get('no',0) - 5)
            score += min(abs(n_high-3), abs(n_high-4))
            return score

        for set_idx, s in enumerate(sets):
            # Try to find a permutation with good block-level balance
            best_order = s[:]
            best_score = float('inf')
            # Use a greedy shuffle: try 1000 random permutations, keep the best
            for _ in range(1000):
                np.random.shuffle(s)
                total_score = 0
                for b in range(0, set_size, 10):
                    block = s[b:b+10]
                    total_score += block_score(block)
                if total_score < best_score:
                    best_score = total_score
                    best_order = s[:]
            sets[set_idx] = best_order

        # Print summary for each set
        for set_num, this_set in enumerate(sets):
            vals = event_coding.loc[this_set, 'valence'].value_counts().to_dict()
            inters = event_coding.loc[this_set, 'interpersonal'].value_counts().to_dict()
            n_high = len([i for i in this_set if i in high_discrim_set])
            n_non_high = len(this_set) - n_high
            print(f"Set {set_num+1}: pos={vals.get('positive',0)}, neg={vals.get('negative',0)}, inter_yes={inters.get('yes',0)}, inter_no={inters.get('no',0)}, high_discrim={n_high}, non_high_discrim={n_non_high}")
        # Print unused/used-once items
        unused_items = [i for i, count in item_use_count.items() if count == 0 and i not in high_discrim_set]
        used_once = [i for i, count in item_use_count.items() if count == 1 and i not in high_discrim_set]
        if unused_items:
            print(f"Unused items (not used at all): {unused_items}")
        if used_once:
            print(f"Items used only once: {used_once}")
    return sets

# Example usage:
if __name__ == "__main__":
    # import sys
    # import time
    # try:
    #     from tqdm import tqdm
    #     use_tqdm = True
    # except ImportError:
    #     use_tqdm = False

    N_TRIES = 10000
    best_score = float('inf')
    best_seed = None
    best_sets = None


    def set_balance_score(sets, output=False):
        # For each set, compute squared error from 15/15 for valence and interpersonal
        errors = []
        for i, s in enumerate(sets):
            vals = event_coding.loc[s, 'valence'].value_counts()
            inters = event_coding.loc[s, 'interpersonal'].value_counts()
            pos = vals.get('positive', 0)
            neg = vals.get('negative', 0)
            yes = inters.get('yes', 0)
            no = inters.get('no', 0)
            # Squared error for valence and interpersonal (target is 15 of each)
            err = (pos - 15) ** 2 + (neg - 15) ** 2 + (yes - 15) ** 2 + (no - 15) ** 2
            errors.append(err)
            if output:
                print(f"Set {i+1} balance error: {err}")
        # Normalized RMSE: sqrt(mean squared error per category), divided by set size (max possible is 1)
        rmse = (sum(errors) / (len(errors) * 4)) ** 0.5
        balance_score = rmse / 30

        # Session similarity penalty: RMSE of actual number of overlapping trials, with extra penalty for sessions within the same week (gap <= 2)
        n_sets = len(sets)
        set_size = len(sets[0]) if sets else 1
        overlap_errors = []
        for i in range(n_sets):
            for j in range(i+1, n_sets):
                overlap = len(set(sets[i]) & set(sets[j]))
                penalty = 1.0
                gap = abs(i - j)
                if gap == 1:
                    penalty = 2.0
                elif gap == 2:
                    penalty = 1.5
                overlap_errors.append((overlap * penalty) ** 2)
                if output:
                    print(f"Sets {i+1} & {j+1} overlap: {overlap} (penalty factor: {penalty})")
        # RMSE, normalized by set size (so max possible is 1)
        if overlap_errors:
            overlap_rmse = (sum(overlap_errors) / len(overlap_errors)) ** 0.5 / set_size
        else:
            overlap_rmse = 0.0

        # Final score: equal weight to balance and similarity
        return overlap_rmse + balance_score

    # iterator = tqdm(range(N_TRIES), desc="Searching seeds") if use_tqdm else range(N_TRIES)
    # for seed in iterator:
    #     sets = split_learning_trials(event_coding, high_discrim, test_set, random_state=seed)
    #     score = set_balance_score(sets)
    #     if score < best_score:
    #         best_score = score
    #         best_seed = seed
    #         best_sets = sets
    # print(f"Best seed: {best_seed} with score: {best_score}")
    # best seed from 100000 tries is 9059 with score 0.3513
    final_sets = split_learning_trials(event_coding, high_discrim, test_set, random_state=9059, block_balance=True)
    set_balance_score(final_sets, output=True)
    # final sets, print and save them
    # with open(os.path.join(os.path.dirname(__file__), 'final_learning_sets.txt'), 'w') as f:
    #     for i, s in enumerate(final_sets):
    #         f.write(f"Set {i+1}: {s}\n")
    #         print(f"Set {i+1}: {s}")