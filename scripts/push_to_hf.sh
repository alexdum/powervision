#!/usr/bin/env bash
# ==============================================================================
# push_to_hf.sh — Push PowerVision (code + data) to Hugging Face Spaces
# ==============================================================================
# Run this script from the server where the Parquet data lives (/data/powervision).
# This handles the FULL push: code, GeoJSON, CSVs, AND Parquet data (via Git LFS).
#
# The GitHub Actions workflow (sync-to-hf.yml) only pushes code changes.
# Run this script whenever:
#   1. First-time setup of the HF Space
#   2. Parquet data files change (new variables, reprocessed data, etc.)
#
# Prerequisites:
#   - git, git-lfs installed on the server
#   - HF_TOKEN environment variable set (or pass as argument)
#     export HF_TOKEN="hf_xxxxx"
#
# Usage:
#   ./scripts/push_to_hf.sh
#   HF_TOKEN=hf_xxxxx ./scripts/push_to_hf.sh
# ==============================================================================
set -euo pipefail

# Ensure locally-installed git-lfs is on PATH
export PATH="$HOME/.local/bin:$PATH"

# --- Configuration ---
HF_REPO="https://huggingface.co/spaces/adumitrescu/powervision"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK_DIR="${PROJECT_ROOT}/.hf_push_tmp"

# --- Check prerequisites ---
if ! command -v git-lfs &> /dev/null; then
    echo "❌ git-lfs is not installed. Install it with:"
    echo "   sudo dnf install git-lfs   # Rocky Linux"
    echo "   sudo apt install git-lfs   # Ubuntu/Debian"
    exit 1
fi

if [ -z "${HF_TOKEN:-}" ]; then
    echo "❌ HF_TOKEN is not set. Export it first:"
    echo "   export HF_TOKEN=\"hf_your_token_here\""
    echo "   Generate one at: https://huggingface.co/settings/tokens"
    exit 1
fi

echo "🚀 PowerVision → Hugging Face Spaces push"
echo "   Project root: ${PROJECT_ROOT}"
echo ""

# --- Clean up any previous temporary clone ---
if [ -d "${WORK_DIR}" ]; then
    echo "🧹 Cleaning up previous temporary clone..."
    rm -rf "${WORK_DIR}"
fi

# --- Clone the HF Space repo ---
echo "📥 Cloning HF Space repo..."
GIT_LFS_SKIP_SMUDGE=1 git clone \
    "https://adumitrescu:${HF_TOKEN}@huggingface.co/spaces/adumitrescu/powervision" \
    "${WORK_DIR}"

cd "${WORK_DIR}"

# --- Initialize Git LFS ---
echo "📦 Setting up Git LFS for Parquet files..."
git lfs install
git lfs track "app/www/data/pecd/**/*.parquet"
git add .gitattributes

# --- Copy the root Dockerfile (HF-specific, port 7860) ---
echo "📄 Copying Dockerfile..."
cp "${PROJECT_ROOT}/Dockerfile" "${WORK_DIR}/Dockerfile"

# --- Copy README_HF.md as README.md (HF frontmatter) ---
echo "📄 Copying README..."
cp "${PROJECT_ROOT}/README_HF.md" "${WORK_DIR}/README.md"

# --- Sync the app directory (everything including data) ---
echo "📂 Syncing app files (R code, CSS, JS, GeoJSON, CSVs)..."
rsync -av --delete \
    --exclude='renv/library/' \
    --exclude='renv/staging/' \
    --exclude='renv/sandbox/' \
    --exclude='.Rhistory' \
    --exclude='.RData' \
    --exclude='.posit/' \
    --exclude='.pytest_cache/' \
    "${PROJECT_ROOT}/app/" "${WORK_DIR}/app/"

# --- Copy Parquet data (the big 1.5 GB payload, tracked by LFS) ---
echo "📊 Syncing Parquet data (this may take a while for 1.5 GB)..."
# The rsync above already copied everything from app/ including www/data/pecd/
# So the Parquet files are already in place. Git LFS will handle them.

# --- Stage everything ---
echo "📝 Staging all files..."
git add -A

# --- Show what's being pushed ---
echo ""
echo "📋 Changes to push:"
git status --short | head -50
TOTAL_CHANGES=$(git status --short | wc -l)
if [ "${TOTAL_CHANGES}" -gt 50 ]; then
    echo "   ... and $((TOTAL_CHANGES - 50)) more files"
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
    git commit -m "deploy: full sync $(date -u +'%Y-%m-%d %H:%M UTC')"
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
