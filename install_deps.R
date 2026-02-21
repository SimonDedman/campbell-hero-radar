#!/usr/bin/env Rscript
# install_deps.R — run once to install all required packages

pkgs <- c(
  # Core
  "shiny", "jsonlite", "ggplot2", "dplyr", "tidyr", "tibble",
  # Radar
  "fmsb",
  # API
  "httr2",
  # Shiny UI
  "bslib", "shinyWidgets",
  # Clustering/analysis
  "ggrepel", "factoextra", "dendextend", "pheatmap", "RColorBrewer"
)

missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(missing) == 0) {
  cat("All packages already installed.\n")
} else {
  cat("Installing", length(missing), "packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
  still_missing <- missing[!missing %in% installed.packages()[, "Package"]]
  if (length(still_missing)) {
    cat("WARNING: failed to install:", paste(still_missing, collapse = ", "), "\n")
  } else {
    cat("All packages installed successfully.\n")
  }
}
