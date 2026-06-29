#!/usr/bin/env python3
"""
Upload Parquet data to a HF Dataset repo (adumitrescu/powervision-data).
This avoids the 1 GB Space repo limit by storing data separately.
The Dockerfile downloads from this dataset at container build time.
"""
import os
from huggingface_hub import HfApi, login

token = os.environ.get("HF_TOKEN")
if not token:
    print("ERROR: Set HF_TOKEN environment variable")
    exit(1)

login(token=token)
api = HfApi()

# Create the dataset repo if it doesn't exist
repo_id = "adumitrescu/powervision-data"
try:
    api.create_repo(repo_id=repo_id, repo_type="dataset", exist_ok=True)
    print(f"✅ Dataset repo ready: {repo_id}")
except Exception as e:
    print(f"Repo creation: {e}")

# Upload the entire pecd directory
data_dir = "/data/powervision/app/www/data/pecd"
print(f"📊 Uploading {data_dir} to {repo_id}...")
print("   This may take several minutes for 1.5 GB...")

api.upload_folder(
    folder_path=data_dir,
    repo_id=repo_id,
    repo_type="dataset",
    path_in_repo="pecd",
    commit_message="upload: PECD Parquet data for PowerVision",
)

print("✅ Upload complete!")
print(f"   Dataset: https://huggingface.co/datasets/{repo_id}")
