# 🗡️ Campbell Hero Radar — Monomyth Film Analysis

Shiny app + analysis pipeline scoring films against Joseph Campbell's **Hero's Journey / Monomyth**, with PCA and hierarchical clustering to reveal structural archetypes across cinema.

## Repository Structure

```
campbell-hero-radar/
├── app.R                        # Shiny app (3-tab: compare / single film / AI score)
├── batch_score_claudecode.R     # Batch scorer via claude CLI (Max sub, free)
├── batch_score_films.R          # Batch scorer via Anthropic API (paid, ~$0.007/film)
├── analyse_clusters.R           # PCA + clustering + heatmap analysis
├── install_deps.R               # One-shot package installer
├── setup_github.sh              # Git init + GitHub push script
├── data/
│   ├── films.json               # Film database (pre-scored + AI-scored)
│   ├── axis_sets.json           # 3 axis set definitions
│   └── scoring_queue.txt        # Films to batch-score (edit this)
└── plots/                       # Output dir for analyse_clusters.R
```

## Axis Sets (3 schemas, all scored per film)

| ID | Name | Axes |
|----|------|------|
| `vogler_12` | Vogler 12-Stage (Film Standard) | 12 |
| `campbell_17` | Campbell 17-Stage (Academic) | 17 |
| `mixed_archetype` | Mixed: 8 Stages + 4 Character Archetypes | 12 |

## Quick Start

```bash
# 1. Clone
git clone https://github.com/SimonDedman/campbell-hero-radar
cd campbell-hero-radar

# 2. Install R packages
Rscript install_deps.R

# 3. Launch Shiny app (uses 10 pre-scored films)
Rscript -e "shiny::runApp('.')"

# 4. Score more films (edit data/scoring_queue.txt first)
#    Via Claude Code / Max subscription (FREE):
Rscript batch_score_claudecode.R

#    Or via direct API (~$0.007/film, needs API key):
export ANTHROPIC_API_KEY=sk-ant-...
Rscript batch_score_films.R

# 5. Run clustering analysis
Rscript analyse_clusters.R --k 5
```

## Scoring Methods

### Option A: Claude Code / Max Subscription (recommended, free)
Uses the `claude` CLI — charges against your Max plan, no per-call billing.
```bash
npm install -g @anthropic-ai/claude-code
claude login
Rscript batch_score_claudecode.R --model claude-opus-4-6
```

### Option B: Direct API (cheap)
~$0.007/film × 3 axis sets using Haiku. ~$4.37 for 600 films total.
```bash
export ANTHROPIC_API_KEY=sk-ant-...
Rscript batch_score_films.R
```

Both scripts are **resumable** — skip already-scored films automatically. Safe to re-run after interruption.

## Batch Scorer Options

```bash
Rscript batch_score_claudecode.R --model claude-opus-4-6   # model override
Rscript batch_score_claudecode.R --dry-run                  # test without scoring
Rscript batch_score_claudecode.R --only 10                  # score first 10 only
```

## Analysis Outputs (analyse_clusters.R)

| File | Description |
|------|-------------|
| `plots/pca_<axisset>.png` | PCA biplot, films coloured by cluster |
| `plots/dendro_<axisset>.png` | Ward D2 dendrogram |
| `plots/cluster_radars_<axisset>.png` | Mean radar per cluster |
| `plots/heatmap_all.png` | All films × all axes combined heatmap |
| `plots/score_distributions.png` | Violin + boxplot per axis |
| `plots/cluster_membership.csv` | Film → cluster assignments |

```bash
Rscript analyse_clusters.R                      # all axis sets, k=5
Rscript analyse_clusters.R --k 7               # 7 clusters
Rscript analyse_clusters.R --axis vogler_12    # one axis set only
```

## Adding Films

Edit `data/scoring_queue.txt` — one film per line as `Title (Year)`, then run batch scorer.

## Scoring Scale

| Score | Meaning |
|-------|---------|
| 10 | Structurally central, fully realised |
| 7-9 | Clearly present |
| 4-6 | Partial or modified |
| 1-3 | Trace or subverted |
| 0 | Absent or antithetical |

## GitHub Setup (first time)

```bash
bash setup_github.sh           # public repo
bash setup_github.sh --private # private repo
```

Requires `gh` CLI: `sudo apt install gh && gh auth login`
