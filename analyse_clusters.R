#!/usr/bin/env Rscript
# analyse_clusters.R
#
# Loads films.json, runs PCA + hierarchical clustering on score vectors,
# produces publication-ready ggplot2 outputs:
#   - PCA biplot per axis set (films as points, coloured by cluster)
#   - Dendrogram per axis set
#   - Heatmap of all films × all axes (combined)
#   - Cluster profile radar plots
#
# Usage:
#   Rscript analyse_clusters.R
#   Rscript analyse_clusters.R --axis vogler_12 --k 5 --out plots/

suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggrepel)
  library(fmsb)
  library(factoextra)   # PCA vis; install if missing
  library(dendextend)   # dendrogram colouring
  library(pheatmap)     # heatmap
  library(RColorBrewer)
})

# ── CLI args ───────────────────────────────────────────────────────────────────
args      <- commandArgs(trailingOnly = TRUE)
AX_FILTER <- if ("--axis" %in% args)  args[which(args == "--axis")  + 1] else NULL   # NULL = all
K_CLUST   <- if ("--k"    %in% args)  as.integer(args[which(args == "--k") + 1]) else 5
OUT_DIR   <- if ("--out"  %in% args)  args[which(args == "--out")  + 1] else "plots"

DATA_DIR  <- "data"
FILMS_FILE  <- file.path(DATA_DIR, "films.json")
AXES_FILE   <- file.path(DATA_DIR, "axis_sets.json")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Campbell colour palette ────────────────────────────────────────────────────
campbell_pal <- c("#E63946","#457B9D","#2A9D8F","#E9C46A","#F4A261",
                  "#A8DADC","#C77DFF","#FF9F1C","#CFFFB0","#6D6875")

# ── Load data ──────────────────────────────────────────────────────────────────
films_raw  <- fromJSON(FILMS_FILE, simplifyVector = FALSE)$films
axis_defs  <- fromJSON(AXES_FILE,  simplifyVector = FALSE)

cat("Loaded", length(films_raw), "films\n")

# ── Build score matrix per axis set ───────────────────────────────────────────
build_matrix <- function(films, ax_id) {
  axes   <- axis_defs[[ax_id]]$axes
  ax_ids <- sapply(axes, `[[`, "id")
  labs   <- sapply(axes, `[[`, "label")

  rows <- lapply(films, function(f) {
    sc <- f$scores[[ax_id]]
    if (is.null(sc)) return(NULL)
    vals <- sapply(ax_ids, function(i) {
      v <- sc[[i]]; if (is.null(v)) NA_real_ else as.numeric(v)
    })
    setNames(vals, labs)
  })

  valid   <- !sapply(rows, is.null)
  titles  <- sapply(films[valid], `[[`, "title")
  years   <- sapply(films[valid], function(f) { y <- f$year; if(is.null(y)) NA_integer_ else as.integer(y) })
  mat     <- do.call(rbind, rows[valid])
  rownames(mat) <- paste0(titles, " (", years, ")")
  mat
}

# ── Cluster helper ─────────────────────────────────────────────────────────────
get_clusters <- function(mat, k) {
  mat_sc  <- scale(mat)
  hc      <- hclust(dist(mat_sc, method = "euclidean"), method = "ward.D2")
  cutree(hc, k = k)
}

# ── Theme ──────────────────────────────────────────────────────────────────────
theme_campbell <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", colour = NA),
      panel.background = element_rect(fill = "#16213e", colour = NA),
      panel.grid       = element_line(colour = "#0f3460", linewidth = 0.4),
      text             = element_text(colour = "#e0e0e0"),
      axis.text        = element_text(colour = "#b0b0c0"),
      plot.title       = element_text(colour = "#E9C46A", face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(colour = "#a0a0b0", size = base_size - 1),
      legend.background = element_rect(fill = "#16213e", colour = NA),
      legend.key       = element_rect(fill = NA),
      strip.text       = element_text(colour = "#E9C46A")
    )
}

# ── 1. PCA biplot ──────────────────────────────────────────────────────────────
plot_pca <- function(mat, ax_id, k, out_dir) {
  ax_label <- axis_defs[[ax_id]]$label
  mat_sc   <- scale(mat)
  pca      <- prcomp(mat_sc, center = FALSE, scale. = FALSE)
  clusters <- get_clusters(mat, k)

  var_exp <- round(summary(pca)$importance[2, 1:2] * 100, 1)

  df <- data.frame(
    PC1     = pca$x[, 1],
    PC2     = pca$x[, 2],
    label   = rownames(mat),
    cluster = factor(clusters)
  )

  p <- ggplot(df, aes(PC1, PC2, colour = cluster, label = label)) +
    geom_point(size = 3, alpha = 0.85) +
    geom_text_repel(size = 2.8, max.overlaps = 20,
                    segment.colour = "#505060", show.legend = FALSE) +
    stat_ellipse(aes(group = cluster), type = "t", level = 0.75,
                 linetype = 2, linewidth = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = campbell_pal[seq_len(k)],
                        name = "Cluster") +
    labs(
      title    = paste0("PCA: ", ax_label),
      subtitle = paste0("PC1 ", var_exp[1], "% | PC2 ", var_exp[2], "% variance explained"),
      x = paste0("PC1 (", var_exp[1], "%)"),
      y = paste0("PC2 (", var_exp[2], "%)")
    ) +
    theme_campbell()

  fname <- file.path(out_dir, paste0("pca_", ax_id, ".png"))
  ggsave(fname, p, width = 12, height = 8, dpi = 150, bg = "#1a1a2e")
  cat("Saved:", fname, "\n")
  p
}

# ── 2. Dendrogram ─────────────────────────────────────────────────────────────
plot_dendrogram <- function(mat, ax_id, k, out_dir) {
  ax_label <- axis_defs[[ax_id]]$label
  mat_sc   <- scale(mat)
  hc       <- hclust(dist(mat_sc), method = "ward.D2")
  dend     <- as.dendrogram(hc)
  dend     <- color_branches(dend, k = k, col = campbell_pal[seq_len(k)])
  dend     <- set(dend, "labels_cex", 0.65)

  fname <- file.path(out_dir, paste0("dendro_", ax_id, ".png"))
  png(fname, width = 1400, height = 700, res = 120, bg = "#1a1a2e")
  par(bg = "#1a1a2e", col.axis = "#e0e0e0", col.lab = "#e0e0e0",
      col.main = "#E9C46A", mar = c(8, 3, 3, 1))
  plot(dend, main = paste0("Hierarchical Clustering: ", ax_label),
       ylab = "Ward D2 Distance", nodePar = list(cex = 0.5))
  dev.off()
  cat("Saved:", fname, "\n")
}

# ── 3. Combined heatmap (all axis sets concatenated) ──────────────────────────
plot_heatmap <- function(films, out_dir) {
  ax_ids <- names(axis_defs)

  # Build combined matrix, only films present in all axis sets
  mats  <- lapply(ax_ids, function(ax) build_matrix(films, ax))
  films_in_all <- Reduce(intersect, lapply(mats, rownames))
  if (length(films_in_all) < 3) {
    cat("Not enough films with all axis sets for heatmap\n"); return(invisible(NULL))
  }

  combined <- do.call(cbind, lapply(mats, function(m) m[films_in_all, ]))
  # Short labels for columns: prefix with axis set abbreviation
  prefixes <- c("V", "C", "M")
  colnames(combined) <- unlist(lapply(seq_along(ax_ids), function(i) {
    axes <- axis_defs[[ax_ids[i]]]$axes
    paste0(prefixes[i], ".", sapply(axes, `[[`, "label"))
  }))

  # Shorten film labels for readability
  rownames(combined) <- sub(" \\(\\d{4}\\)$", "", rownames(combined))

  # Annotation: cluster per film (using vogler as primary)
  clust_primary <- get_clusters(mats[[1]][films_in_all, ], K_CLUST)
  ann_row <- data.frame(Cluster = factor(clust_primary), row.names = rownames(combined))
  ann_col <- data.frame(
    AxisSet = rep(c("Vogler 12", "Campbell 17", "Mixed"), sapply(mats, ncol)),
    row.names = colnames(combined)
  )

  ann_colours <- list(
    Cluster = setNames(campbell_pal[seq_len(K_CLUST)], levels(ann_row$Cluster)),
    AxisSet = c("Vogler 12" = "#457B9D", "Campbell 17" = "#2A9D8F", "Mixed" = "#E63946")
  )

  fname <- file.path(out_dir, "heatmap_all.png")
  pheatmap(
    combined,
    color            = colorRampPalette(c("#16213e", "#457B9D", "#E9C46A", "#E63946"))(100),
    clustering_method = "ward.D2",
    annotation_row   = ann_row,
    annotation_col   = ann_col,
    annotation_colors = ann_colours,
    fontsize_row     = 7,
    fontsize_col     = 6,
    angle_col        = 45,
    border_color     = NA,
    filename         = fname,
    width            = 22, height = max(8, nrow(combined) * 0.25 + 4)
  )
  cat("Saved:", fname, "\n")
}

# ── 4. Cluster profile radars ─────────────────────────────────────────────────
plot_cluster_radars <- function(mat, ax_id, k, out_dir) {
  ax_label  <- axis_defs[[ax_id]]$label
  clusters  <- get_clusters(mat, k)
  n_axes    <- ncol(mat)

  means <- lapply(seq_len(k), function(cl) {
    colMeans(mat[clusters == cl, , drop = FALSE])
  })

  df <- as.data.frame(do.call(rbind, means))
  colnames(df) <- colnames(mat)
  rownames(df) <- paste0("Cluster ", seq_len(k),
                          " (n=", table(clusters)[seq_len(k)], ")")
  df_plot <- rbind(rep(10, n_axes), rep(0, n_axes), df)
  rownames(df_plot)[1:2] <- c("max", "min")

  fname <- file.path(out_dir, paste0("cluster_radars_", ax_id, ".png"))
  cex_lab <- if (n_axes > 14) 0.6 else if (n_axes > 10) 0.7 else 0.8

  png(fname, width = 1200, height = 900, res = 120, bg = "#1a1a2e")
  par(bg = "#1a1a2e", mfrow = c(ceiling(k / 3), min(k, 3)),
      mar = c(1, 1, 2, 1), oma = c(0, 0, 3, 0))

  for (i in seq_len(k)) {
    df_i <- rbind(rep(10, n_axes), rep(0, n_axes), df[i, ])
    rownames(df_i) <- c("max", "min", rownames(df)[i])
    n_in_cluster <- sum(clusters == i)
    fmsb::radarchart(
      df_i,
      axistype    = 1,
      pcol        = campbell_pal[i],
      pfcol       = adjustcolor(campbell_pal[i], 0.3),
      plwd        = 2.5,
      cglcol      = "grey40",
      cglty       = 1,
      axislabcol  = "grey60",
      vlcex       = cex_lab,
      caxislabels = c("", "2.5", "5", "7.5", ""),
      calcex      = 0.6,
      seg         = 4
    )
    title(main = paste0("Cluster ", i, " (n=", n_in_cluster, ")"),
          col.main = campbell_pal[i], cex.main = 0.9)
  }
  mtext(paste0("Cluster Profiles: ", ax_label),
        outer = TRUE, col = "#E9C46A", cex = 1.1, font = 2)
  dev.off()
  cat("Saved:", fname, "\n")
}

# ── 5. Score distribution violin plot ─────────────────────────────────────────
plot_score_distributions <- function(films, out_dir) {
  rows <- lapply(films, function(f) {
    lapply(names(axis_defs), function(ax_id) {
      axes <- axis_defs[[ax_id]]$axes
      ids  <- sapply(axes, `[[`, "id")
      labs <- sapply(axes, `[[`, "label")
      sc   <- f$scores[[ax_id]]
      if (is.null(sc)) return(NULL)
      vals <- sapply(ids, function(i) { v <- sc[[i]]; if(is.null(v)) NA else as.numeric(v) })
      data.frame(
        film     = f$title,
        axis_set = axis_defs[[ax_id]]$label,
        axis     = labs,
        score    = vals,
        stringsAsFactors = FALSE
      )
    })
  })
  df <- bind_rows(unlist(rows, recursive = FALSE))

  p <- ggplot(df, aes(x = reorder(axis, score, median), y = score, fill = axis_set)) +
    geom_violin(alpha = 0.6, colour = NA) +
    geom_boxplot(width = 0.15, outlier.size = 0.8,
                 colour = "#e0e0e0", fill = NA) +
    facet_wrap(~axis_set, scales = "free_x", ncol = 1) +
    coord_flip() +
    scale_fill_manual(values = c("#457B9D", "#2A9D8F", "#E63946"), guide = "none") +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    labs(
      title    = "Score Distributions Across All Films",
      subtitle = paste0("n = ", length(films), " films"),
      x = NULL, y = "Score (0-10)"
    ) +
    theme_campbell(base_size = 9)

  fname <- file.path(out_dir, "score_distributions.png")
  ggsave(fname, p, width = 14, height = max(10, length(unique(df$axis)) * 0.4 + 4),
         dpi = 150, bg = "#1a1a2e")
  cat("Saved:", fname, "\n")
}

# ── 6. Cluster membership table ───────────────────────────────────────────────
export_cluster_table <- function(films, out_dir) {
  results <- lapply(names(axis_defs), function(ax_id) {
    mat      <- build_matrix(films, ax_id)
    if (nrow(mat) < K_CLUST + 1) return(NULL)
    clusters <- get_clusters(mat, K_CLUST)
    data.frame(
      film      = sub(" \\(\\d{4}\\)$", "", rownames(mat)),
      axis_set  = ax_id,
      cluster   = clusters,
      stringsAsFactors = FALSE
    )
  })
  df <- bind_rows(results)

  # Pivot wide: one row per film, one col per axis set
  df_wide <- df |>
    mutate(axis_set = paste0("cluster_", axis_set)) |>
    pivot_wider(names_from = axis_set, values_from = cluster) |>
    arrange(cluster_vogler_12)

  fname <- file.path(out_dir, "cluster_membership.csv")
  write.csv(df_wide, fname, row.names = FALSE)
  cat("Saved:", fname, "\n")
  df_wide
}

# ── Main ───────────────────────────────────────────────────────────────────────
ax_to_run <- if (!is.null(AX_FILTER)) AX_FILTER else names(axis_defs)

cat("Running analysis for axis sets:", paste(ax_to_run, collapse=", "), "\n")
cat("K clusters:", K_CLUST, "\n")
cat("Output dir:", OUT_DIR, "\n\n")

for (ax_id in ax_to_run) {
  if (!ax_id %in% names(axis_defs)) {
    cat("Unknown axis set:", ax_id, "— skipping\n"); next
  }
  mat <- build_matrix(films_raw, ax_id)
  if (nrow(mat) < K_CLUST + 1) {
    cat("Not enough films for", ax_id, "(need >", K_CLUST, ") — skipping\n"); next
  }
  cat("\n--- ", axis_defs[[ax_id]]$label, "(n=", nrow(mat), ") ---\n")
  plot_pca(mat, ax_id, K_CLUST, OUT_DIR)
  plot_dendrogram(mat, ax_id, K_CLUST, OUT_DIR)
  plot_cluster_radars(mat, ax_id, K_CLUST, OUT_DIR)
}

plot_heatmap(films_raw, OUT_DIR)
plot_score_distributions(films_raw, OUT_DIR)
cluster_tbl <- export_cluster_table(films_raw, OUT_DIR)

cat("\n=== Analysis complete. Outputs in:", OUT_DIR, "===\n")
print(cluster_tbl)
