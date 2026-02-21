#!/bin/bash
# setup_github.sh — run once on your local machine
# Creates GitHub repo and pushes all files
#
# Prerequisites:
#   gh auth login          (GitHub CLI, install: sudo apt install gh)
#   git config --global user.name "Simon Dedman"
#   git config --global user.email "your@email.com"
#
# Usage:
#   bash setup_github.sh
#   bash setup_github.sh --private    # make repo private

set -e

REPO_NAME="campbell-hero-radar"
GITHUB_USER="SimonDedman"
VISIBILITY="--public"
[[ "$1" == "--private" ]] && VISIBILITY="--private"

echo "=== Campbell Hero Radar — GitHub Setup ==="
echo "Repo: $GITHUB_USER/$REPO_NAME ($VISIBILITY)"
echo ""

# Check dependencies
command -v git >/dev/null || { echo "ERROR: git not found"; exit 1; }
command -v gh  >/dev/null || { echo "ERROR: gh CLI not found. Install: sudo apt install gh"; exit 1; }

# Check gh auth
gh auth status >/dev/null 2>&1 || {
  echo "Not authenticated. Running: gh auth login"
  gh auth login
}

# Init git if needed
if [ ! -d ".git" ]; then
  git init
  echo "Git repo initialised"
fi

# .gitignore safety check
if ! grep -q "\.env" .gitignore 2>/dev/null; then
  echo ".env" >> .gitignore
fi
if ! grep -q "scoring_log" .gitignore 2>/dev/null; then
  echo "scoring_log.txt" >> .gitignore
  echo "plots/" >> .gitignore
fi

# Initial commit
git add -A
git diff --cached --quiet || git commit -m "Initial commit: Campbell monomyth radar app + 10 pre-scored films"

# Create GitHub repo and push
if gh repo view "$GITHUB_USER/$REPO_NAME" >/dev/null 2>&1; then
  echo "Repo already exists on GitHub. Adding remote and pushing..."
  git remote remove origin 2>/dev/null || true
  git remote add origin "https://github.com/$GITHUB_USER/$REPO_NAME.git"
  git push -u origin main 2>/dev/null || git push -u origin master
else
  echo "Creating GitHub repo..."
  gh repo create "$GITHUB_USER/$REPO_NAME" $VISIBILITY \
    --description "Joseph Campbell monomyth film archetype radar — Shiny app + clustering analysis" \
    --source=. \
    --push
fi

echo ""
echo "=== Done ==="
echo "Repo: https://github.com/$GITHUB_USER/$REPO_NAME"
echo ""
echo "Next steps:"
echo "  1. Install R packages:  Rscript install_deps.R"
echo "  2. Dry-run scorer:      Rscript batch_score_claudecode.R --dry-run"
echo "  3. Score queue:         Rscript batch_score_claudecode.R"
echo "  4. Run analysis:        Rscript analyse_clusters.R"
echo "  5. Launch Shiny app:    Rscript -e \"shiny::runApp('.')\"" 
