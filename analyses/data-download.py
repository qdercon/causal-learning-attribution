import firebase_admin
from firebase_admin import credentials, firestore
import json
import pandas as pd
import argparse
import os

# --- Argument Parsing ---
parser = argparse.ArgumentParser(description="Download data from Firebase.")
parser.add_argument("--key", type=str, default=os.environ.get("GOOGLE_APPLICATION_CREDENTIALS"),
                    help="Path to the Firebase admin SDK service-account key (never commit this file). "
                         "Defaults to $GOOGLE_APPLICATION_CREDENTIALS.")
parser.add_argument("--data", type=str, required=True, choices=['prescreen', 'training-control', 'training-causal', 'postscreen'], help="The dataset to download.")
parser.add_argument("--pilot", action="store_true", help="Whether to download pilot data.")
parser.add_argument("--session", type=str, help="Session to download data from.")
parser.add_argument("--export", type=str, nargs="*", help="Name(s) of Prolific export CSVs to filter IDs from.")
parser.add_argument("--path", type=str, help="Path to the directory containing Prolific exports.")
parser.add_argument("--subdir", type=str, default="", help="Subdirectory within 'data' to save files.")
args = parser.parse_args()

# Set up Firebase credentials
if not args.key:
    parser.error("no service-account key: pass --key or set GOOGLE_APPLICATION_CREDENTIALS")
if not firebase_admin._apps:
    cred = credentials.Certificate(args.key)
    firebase_admin.initialize_app(cred)

# --- Version and Task Setup ---
taskname = 'causal-train-mh'
version_nm = f"{args.data}-pilot" if args.pilot else args.data
session_no = f"-session-{args.session}" if args.session else ''
version = version_nm + session_no

# --- Prolific ID Filtering ---
ids = []
if args.export:
    path_prefix = args.path or ''
    export_files = [os.path.join(path_prefix, f) for f in args.export]
    try:
        export_df = pd.concat((pd.read_csv(f) for f in export_files), ignore_index=True)
        ids = export_df[export_df['Status'] == 'APPROVED']['Participant id'].tolist()
    except FileNotFoundError as e:
        print(f"Error: Prolific export file not found at {e.filename}. Proceeding without filtering.")

# --- Data Directory and ID Lists Setup ---
data_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'data', args.subdir)
os.makedirs(data_dir, exist_ok=True)

def read_ids_from_file(filename):
    """Reads IDs from a file, returns empty list if it doesn't exist."""
    filepath = os.path.join(data_dir, filename)
    try:
        with open(filepath, 'r') as f:
            return f.read().splitlines()
    except FileNotFoundError:
        return []

all_ids = read_ids_from_file('all_ids.txt')
control_ids = read_ids_from_file('control_ids.txt')
causal_ids = read_ids_from_file('causal_ids.txt')
if args.data == 'prescreen':
    ineligible_ids = read_ids_from_file('ineligible_ids.txt')

# --- Data Download Loop ---
new_datasets = 0
new_randomised = 0
client = firestore.client()
collection_ref = client.collection('tasks', taskname, version)

for sub in collection_ref.stream():
    idinfo = sub.to_dict()
    try:
        fsid = idinfo['firebaseUID']
        subid = idinfo['prolificSubID']
    except (KeyError, TypeError):
        continue

    # Skip if participant should not be processed
    if fsid == 'init' or fsid in all_ids or not idinfo.get('expCompleted') == 1:
        continue
    if args.export and subid not in ids:
        continue

    condition = idinfo.get('interventionCondition')
    if not condition and args.data not in ['postscreen']:
        continue

    # Download and write data
    questdata_ref = collection_ref.document(sub.id).collection('quest-data').document('data')
    questdata = questdata_ref.get().to_dict()
    
    output_filename = os.path.join(data_dir, f'{version}-{subid}-{fsid}-self-reports.txt')
    with open(output_filename, 'w') as f:
        f.write(json.dumps({**idinfo, **(questdata or {})}))
    
    new_datasets += 1

    # Update ID lists and files
    all_ids.append(fsid)
    with open(os.path.join(data_dir, 'all_ids.txt'), 'a') as f:
        f.write(f'{fsid}\n')

    id_file_map = {
        'control': ('control_ids.txt', control_ids),
        'causal': ('causal_ids.txt', causal_ids),
    }
    if args.data == 'prescreen':
        id_file_map['ineligible'] = ('ineligible_ids.txt', ineligible_ids)

    if condition in id_file_map:
        filename, id_list = id_file_map[condition]
        id_list.append(fsid)
        with open(os.path.join(data_dir, filename), 'a') as f:
            f.write(f'{fsid}\n')
        if condition in ['control', 'causal']:
            new_randomised += 1
            # download task data for these participants
            task_ref = collection_ref.document(sub.id).collection('task-data').document('data')
            task_data = task_ref.get().to_dict()
            task_output_filename = os.path.join(data_dir, f'{version}-{subid}-{fsid}-task-data.txt')
            with open(task_output_filename, 'w') as f:
                f.write(json.dumps({**idinfo, **(task_data or {})}))

# --- Summary ---
if args.data == 'prescreen':
    total_randomised = len(control_ids) + len(causal_ids)
    total_prescreen_completed = len(all_ids)
    prop_eligible = (total_randomised / total_prescreen_completed) * 100 if total_prescreen_completed > 0 else 0

    print(
        f"Downloaded {new_datasets} new datasets, of which {new_randomised} were eligible for the study and randomised.\n"
        f"Total randomised: {total_randomised}, of which {len(control_ids)} were randomised to control training and {len(causal_ids)} to causal training.\n"
        f"Of those who completed the screening, {prop_eligible:.2f}% were eligible and randomised."
    )

else:
    print(f"Downloaded {new_datasets} new datasets, of which {len(control_ids)} were in the control condition and {len(causal_ids)} in the causal condition.")