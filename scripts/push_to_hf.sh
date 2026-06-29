#!/usr/bin/env bash
# ==============================================================================
# push_to_hf.sh — Push PowerVision CODE to Hugging Face Spaces
# ==============================================================================
# Run this script from the server where the project lives (/data/powervision).
# This pushes CODE ONLY — the Parquet data lives in a separate HF Dataset repo
# (adumitrescu/powervision-data) and is downloaded during Docker build.
#
# To update the Parquet data, run: scripts/upload_data_to_hf.py
#
# Prerequisites:
#   - git installed on the server
#   - HF_TOKEN environment variable set
#     export HF_TOKEN="hf_xxxxx"
#
# Usage:
#   ./scripts/push_to_hf.sh
#   HF_TOKEN=hf_xxxxx ./scripts/push_to_hf.sh
# ==============================================================================
set -euo pipefail

# Ensure locally-installed tools are on PATH
export PATH="$HOME/.local/bin:$PATH"

# --- Configuration ---
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK_DIR="${PROJECT_ROOT}/.hf_push_tmp"

# --- Check prerequisites ---
if [ -z "${HF_TOKEN:-}" ]; then
    echo "❌ HF_TOKEN is not set. Export it first:"
    echo "   export HF_TOKEN=\"hf_your_token_here\""
    echo "   Generate one at: https://huggingface.co/settings/tokens"
    exit 1
fi

echo "🚀 PowerVision → Hugging Face Spaces push (code only)"
echo "   Project root: ${PROJECT_ROOT}"
echo ""

# --- Clean up any previous temporary clone ---
if [ -d "${WORK_DIR}" ]; then
    echo "🧹 Cleaning up previous temporary clone..."
    rm -rf "${WORK_DIR}"
fi

# --- Clone the HF Space repo ---
echo "📥 Cloning HF Space repo..."
git clone \
    "https://adumitrescu:${HF_TOKEN}@huggingface.co/spaces/adumitrescu/powervision" \
    "${WORK_DIR}"

cd "${WORK_DIR}"

# --- Copy the root Dockerfile (HF-specific, port 7860) ---
echo "📄 Copying Dockerfile..."
cp "${PROJECT_ROOT}/Dockerfile" "${WORK_DIR}/Dockerfile"

# --- Copy README_HF.md as README.md (HF frontmatter) ---
echo "📄 Copying README..."
cp "${PROJECT_ROOT}/README_HF.md" "${WORK_DIR}/README.md"

# --- Sync the app directory (code only, NO Parquet data) ---
echo "📂 Syncing app files (R code, CSS, JS, GeoJSON, CSVs)..."
rsync -av --delete \
    --exclude='renv/library/' \
    --exclude='renv/staging/' \
    --exclude='renv/sandbox/' \
    --exclude='www/data/pecd/' \
    --exclude='.Rhistory' \
    --exclude='.RData' \
    --exclude='.posit/' \
    --exclude='.pytest_cache/' \
    "${PROJECT_ROOT}/app/" "${WORK_DIR}/app/"

# --- Remove old template files if they exist ---
rm -f "${WORK_DIR}/app.R" "${WORK_DIR}/penguins.csv" 2>/dev/null

# --- Stage everything ---
echo "📝 Staging all files..."
git add -A

# --- Show what's being pushed ---
echo ""
echo "📋 Changes to push:"
git status --short | head -30
TOTAL_CHANGES=$(git status --short | wc -l)
if [ "${TOTAL_CHANGES}" -gt 30 ]; then
    echo "   ... and $((TOTAL_CHANGES - 30)) more files"
fi

# --- Commit and push ---
if git diff --cached --quiet; then
    echo ""
    echo "✅ No changes to push — HF Space is already up to date."
else
    echo ""
    echo "⬆️  Committing and pushing to HF Space..."
    git config user.name "PowerVision Deploy"
    git config user.email "deploy@powervision"
    git commit -m "deploy: code sync $(date -u +'%Y-%m-%d %H:%M UTC')"
    git push
    echo ""
    echo "✅ Push complete!"
    echo "   Build logs: https://huggingface.co/spaces/adumitrescu/powervision"
    echo "   App URL:    https://adumitrescu-powervision.hf.space"
fi

# --- Cleanup ---
echo ""
echo "🧹 Cleaning up temporary clone..."
cd "${PROJECT_ROOT}"
rm -rf "${WORK_DIR}"

echo ""
echo "Done! Monitor the build at:"
echo "  https://huggingface.co/spaces/adumitrescu/powervision"
