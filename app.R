library(shiny)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)
library(httr2)
library(ggrepel)
library(cluster)

# ── Load data ──────────────────────────────────────────────────────────────────
films_data   <- fromJSON("data/films.json", simplifyVector = FALSE)$films
axis_data    <- fromJSON("data/axis_sets.json", simplifyVector = FALSE)

# Deduplicate by title (keep first occurrence — the hand-scored version)
seen_titles <- character(0)
keep <- logical(length(films_data))
for (i in seq_along(films_data)) {
  ttl <- films_data[[i]]$title
  if (ttl %in% seen_titles) {
    keep[i] <- FALSE
  } else {
    keep[i] <- TRUE
    seen_titles <- c(seen_titles, ttl)
  }
}
films_data <- films_data[keep]

# Build choices sorted alphabetically
film_display <- sapply(films_data, function(f) {
  yr <- f$year
  if (is.null(yr) || is.na(yr)) f$title else paste0(f$title, " (", yr, ")")
})
film_ids <- sapply(films_data, `[[`, "id")
film_titles <- setNames(film_ids, film_display)
film_titles <- film_titles[order(gsub("^(A |An |The )", "", names(film_titles)))]

# Ordered axis set labels (vogler, campbell, mixed) — consistent everywhere
axis_set_order <- c("vogler_12", "campbell_17", "mixed_archetype")
axis_set_labels <- setNames(
  axis_set_order,
  sapply(axis_set_order, function(id) axis_data[[id]]$label)
)

# ── Colour palette ───────────────────────────────────────────────────────────
bg_main    <- "#F5F0E8"
bg_panel   <- "#EDE6D6"
bg_well    <- "#E8E0D0"
border_col <- "#C4B8A4"
text_main  <- "#2D2418"
text_muted <- "#7A6E5D"
accent_1   <- "#B22234"
accent_2   <- "#1B4965"
accent_3   <- "#D4A03C"
accent_4   <- "#6B8F71"
accent_5   <- "#C06C38"

radar_colours <- c(
  "#B22234", "#1B4965", "#6B8F71", "#C06C38", "#D4A03C",
  "#7B2D8E", "#2A9D8F", "#D4644E", "#4A7C59", "#8B6914"
)

# ── Shared ggplot theme ──────────────────────────────────────────────────────
theme_campbell <- function(base_size = 17) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = bg_main, colour = NA),
      panel.background = element_rect(fill = bg_panel, colour = NA),
      panel.grid       = element_line(colour = border_col, linewidth = 0.3),
      text             = element_text(colour = text_main),
      axis.text        = element_text(colour = text_main, size = base_size - 2),
      strip.text       = element_text(colour = accent_2, size = base_size - 1, face = "bold"),
      plot.title       = element_text(colour = accent_2, size = base_size + 2, face = "bold"),
      plot.subtitle    = element_text(colour = text_muted, size = base_size - 1),
      legend.background = element_rect(fill = bg_main, colour = NA),
      legend.key       = element_rect(fill = NA),
      legend.text      = element_text(size = base_size - 2)
    )
}

# ── Helpers ──────────────────────────────────────────────────────────────────
get_film_scores <- function(film_id, axis_set_id) {
  film <- Filter(function(f) f$id == film_id, films_data)[[1]]
  scores <- film$scores[[axis_set_id]]
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  ids    <- sapply(axes, `[[`, "id")
  vals   <- sapply(ids, function(i) scores[[i]])
  setNames(vals, labels)
}

id_to_title <- setNames(
  sapply(films_data, function(f) {
    yr <- f$year
    if (is.null(yr) || is.na(yr)) f$title else paste0(f$title, " (", yr, ")")
  }),
  sapply(films_data, `[[`, "id")
)

make_radar_plot <- function(film_ids, axis_set_id) {
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  n_axes <- length(labels)

  score_list <- lapply(film_ids, function(fid) get_film_scores(fid, axis_set_id))
  titles <- sapply(film_ids, function(fid) id_to_title[[fid]])

  df <- as.data.frame(do.call(rbind, score_list))
  colnames(df) <- labels
  rownames(df) <- make.unique(titles, sep = " #")

  df_plot <- rbind(rep(10, n_axes), rep(0, n_axes), df)
  rownames(df_plot)[1:2] <- c("max", "min")

  colours_fill   <- adjustcolor(radar_colours[seq_along(film_ids)], alpha.f = 0.12)
  colours_border <- radar_colours[seq_along(film_ids)]

  # Abbreviate long labels for dense radars to prevent overlap at bottom
  if (n_axes > 14) {
    labels <- sub("^Meeting with the ", "", labels)
    labels <- sub("^Woman as ", "", labels)
    labels <- sub("^Atonement with the ", "Atonement: ", labels)
    labels <- sub("^Crossing First ", "1st ", labels)
    labels <- sub("^Refusal of the ", "Refusal: ", labels)
    labels <- sub("^The Ultimate ", "", labels)
    labels <- sub("^The Magic ", "", labels)
    labels <- sub("^Return Threshold Crossing", "Return Threshold", labels)
    labels <- sub("^Master of Two ", "Master ", labels)
    colnames(df) <- labels
    colnames(df_plot) <- labels
  }

  cex_lab <- if (n_axes > 14) 1.10 else if (n_axes > 10) 1.20 else 1.30

  par(mar = c(1, 1, 1, 1), family = "sans")
  fmsb::radarchart(
    df_plot, axistype = 1,
    pcol = colours_border, pfcol = colours_fill, plwd = 2.5, plty = 1,
    cglcol = border_col, cglty = 1, cglwd = 0.6,
    axislabcol = text_muted, vlcex = cex_lab,
    caxislabels = seq(0, 10, 2.5), calcex = 0.9, seg = 4, title = ""
  )

  if (length(film_ids) > 1) {
    legend(
      x = "bottomleft", inset = c(0, 0),
      legend = titles, col = colours_border,
      lty = 1, lwd = 2.5, pch = 20, pt.cex = 2.0,
      bty = "n", cex = 1.15, text.col = text_main, xpd = TRUE
    )
  }
}

# ── Build score matrix ───────────────────────────────────────────────────────
build_score_matrix <- function(ax_id) {
  axes   <- axis_data[[ax_id]]$axes
  ax_ids <- sapply(axes, `[[`, "id")
  labs   <- sapply(axes, `[[`, "label")
  rows <- lapply(films_data, function(f) {
    sc <- f$scores[[ax_id]]
    if (is.null(sc)) return(NULL)
    vals <- sapply(ax_ids, function(i) { v <- sc[[i]]; if (is.null(v)) NA_real_ else as.numeric(v) })
    setNames(vals, labs)
  })
  valid  <- !sapply(rows, is.null)
  titles <- sapply(films_data[valid], function(f) {
    yr <- f$year
    if (is.null(yr) || is.na(yr)) f$title else paste0(f$title, " (", yr, ")")
  })
  mat <- do.call(rbind, rows[valid])
  rownames(mat) <- titles
  mat
}

# ── Build long-form scores ───────────────────────────────────────────────────
build_scores_long <- function() {
  rows <- lapply(films_data, function(f) {
    lapply(names(axis_data), function(ax_id) {
      axes <- axis_data[[ax_id]]$axes
      ids  <- sapply(axes, `[[`, "id")
      labs <- sapply(axes, `[[`, "label")
      sc   <- f$scores[[ax_id]]
      if (is.null(sc)) return(NULL)
      vals <- sapply(ids, function(i) { v <- sc[[i]]; if (is.null(v)) NA else as.numeric(v) })
      data.frame(film = f$title, year = if (is.null(f$year)) NA_integer_ else as.integer(f$year),
                 axis_set = axis_data[[ax_id]]$label, axis_id = ax_id,
                 axis = labs, score = vals, stringsAsFactors = FALSE)
    })
  })
  bind_rows(unlist(rows, recursive = FALSE))
}

# ── Gap statistic for optimal k (Van Moorter et al. 2010 / Tibshirani 2001) ─
find_optimal_k <- function(mat, k_max = 8) {
  mat_sc <- scale(mat)
  gap <- tryCatch(
    clusGap(mat_sc, FUN = kmeans, K.max = min(k_max, nrow(mat_sc) - 1),
            B = 50, nstart = 25),
    error = function(e) NULL
  )
  if (is.null(gap)) return(list(k_opt = 2, gap_obj = NULL))
  # Tibshirani "1-SE" rule: smallest k where gap(k) >= gap(k+1) - SE(k+1)
  k_opt <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")
  list(k_opt = max(2L, k_opt), gap_obj = gap)
}

# ── US political/mood data by decade (for correlation analysis) ──────────────
# Sources: U. Michigan Consumer Sentiment, Gallup Presidential Approval,
# Congressional Research Service conflict data
us_mood_data <- data.frame(
  decade = c("1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s"),
  consumer_sentiment = c(NA, NA, 90, 95, 75, 85, 96, 82, 88, 65),
  presidential_approval = c(NA, 65, 60, 55, 42, 52, 55, 48, 46, 43),
  conflict_intensity = c(2, 10, 7, 8, 6, 3, 4, 8, 5, 2),
  stringsAsFactors = FALSE
)

# ── Score via Anthropic API ──────────────────────────────────────────────────
score_film_api <- function(film_title, axis_set_id, api_key) {
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  ids    <- sapply(axes, `[[`, "id")
  axes_str <- paste(
    mapply(function(id, lbl) sprintf('  "%s": <score 0-10>  // %s', id, lbl), ids, labels),
    collapse = "\n"
  )
  prompt <- sprintf(
    'You are an expert in Joseph Campbell\'s monomyth and comparative mythology.
Score the film "%s" on each of the following narrative dimensions from 0-10,
where 10 = this element is strongly and explicitly present, 0 = absent or inverted.

Return ONLY valid JSON, no commentary, no markdown:
{
%s
}

Base scores on the film\'s actual narrative structure. Be discriminating.',
    film_title, axes_str
  )
  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers("Content-Type" = "application/json", "anthropic-version" = "2023-06-01",
                "x-api-key" = api_key) |>
    req_body_json(list(model = "claude-sonnet-4-20250514", max_tokens = 800,
                       messages = list(list(role = "user", content = prompt)))) |>
    req_error(is_error = function(resp) FALSE) |> req_perform()
  if (resp_status(resp) != 200) return(list(error = paste("API error:", resp_status(resp))))
  body <- resp_body_json(resp)
  raw <- gsub("```json|```", "", body$content[[1]]$text)
  parsed <- tryCatch(fromJSON(raw), error = function(e) NULL)
  if (is.null(parsed)) return(list(error = "Failed to parse API response"))
  scores <- sapply(ids, function(i) { v <- parsed[[i]]; if (is.null(v)) 5L else as.integer(v) })
  setNames(scores, labels)
}

# ── Axis set descriptions ────────────────────────────────────────────────────
axis_descriptions <- list(
  vogler_12 = list(
    ordinary_world = "The hero's normal life before the story begins. Establishes stakes, sympathy, and the world that will be disrupted.",
    call_to_adventure = "An event or messenger that disrupts the ordinary world and presents a challenge, quest, or journey.",
    refusal = "The hero hesitates or outright refuses the call, showing fear, obligation, or attachment to the status quo.",
    mentor = "A wise figure provides guidance, training, gifts, or confidence. May be a person, memory, or inner voice.",
    threshold = "The hero commits to the journey and leaves the known world. There is no turning back.",
    tests_allies_enemies = "The hero faces challenges, makes allies, and confronts enemies in the special world. Learns the new rules.",
    inmost_cave = "The hero approaches the central danger or ordeal. A moment of preparation, reflection, or dread.",
    ordeal = "The hero faces the greatest challenge, confronts death (literal or symbolic), and is transformed.",
    reward = "Having survived the ordeal, the hero claims the prize: knowledge, treasure, reconciliation, or power.",
    road_back = "The hero begins the return journey but faces renewed danger or pursuit. Stakes are raised.",
    resurrection = "A final test where the hero must use everything learned. A climactic purification or transformation.",
    return_elixir = "The hero returns to the ordinary world, changed, bringing a gift, lesson, or boon to share."
  ),
  campbell_17 = list(
    ordinary_world = "The hero's mundane existence before the call. The known world that anchors the departure.",
    call_to_adventure = "A herald, event, or crisis that summons the hero to cross into the unknown.",
    refusal = "The hero's initial reluctance, driven by fear, duty, or comfort in the familiar.",
    supernatural_aid = "A protective figure or magical gift appears once the hero commits to the quest.",
    threshold_crossing = "Passage into the zone of magnified power: a gateway guarded by a threshold guardian.",
    belly_whale = "Total separation from the known world. The hero is swallowed into the unknown, a symbolic death.",
    road_trials = "A series of tests the hero must undergo. Often three in number, the hero may fail some.",
    goddess = "An encounter with a figure of unconditional love, wholeness, or ultimate knowledge.",
    temptress = "A temptation (not necessarily female) that could derail the hero from the quest. Material or spiritual.",
    atonement_father = "Confrontation with whatever holds ultimate power over the hero's life. A reckoning with authority.",
    apotheosis = "A period of rest, peace, and expanded consciousness. The hero achieves a god-like state of understanding.",
    ultimate_boon = "The goal of the quest is achieved. The hero obtains the object, knowledge, or power sought.",
    refusal_return = "Having found bliss or enlightenment, the hero may not want to return to ordinary life.",
    magic_flight = "The return journey may be just as adventurous as the quest. The hero may need to escape with the boon.",
    rescue_without = "The hero may need to be rescued by guides or forces from the ordinary world.",
    return_threshold = "The hero crosses back into the ordinary world and must integrate the wisdom gained.",
    master_two_worlds = "The hero achieves balance between the inner (spiritual) and outer (material) worlds."
  ),
  mixed_archetype = list(
    departure_depth = "How deeply the story establishes the hero's departure from normalcy. Richness of the 'before' state.",
    threshold_crossing = "Strength and clarity of the moment the hero enters the unknown or commits irrevocably.",
    trials_intensity = "The severity, variety, and transformative power of the challenges faced.",
    descent_transformation = "Depth of the hero's descent into darkness, the unconscious, or symbolic death.",
    climax_ordeal = "Intensity of the supreme ordeal: the stakes, sacrifice, and confrontation with the shadow.",
    return_changed = "Degree to which the hero returns transformed. How different is the hero at the end?",
    gift_to_world = "Whether the hero brings a tangible or spiritual gift back to the community.",
    psychological_death_rebirth = "Explicit psychological death and rebirth: the old self dies, a new self emerges.",
    arch_mentor = "Presence and influence of a Mentor figure: wise old man/woman, teacher, guide, conscience.",
    arch_shadow = "Presence and power of the Shadow: antagonist, dark mirror, repressed self, destructive force.",
    arch_shapeshifter = "Presence of a Shapeshifter: a character whose loyalty, identity, or nature shifts. Creates doubt.",
    arch_trickster = "Presence of a Trickster: a comic or subversive figure who disrupts conventions and catalyses change."
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = bg_main, fg = text_main,
    primary = accent_1, secondary = accent_2,
    base_font = bslib::font_google("Source Serif Pro"),
    heading_font = bslib::font_google("Source Sans Pro")
  ),
  tags$head(tags$style(HTML(sprintf("
    body { background-color: %s; font-size: 1.08rem; }
    .well { background-color: %s !important; border: 1px solid %s; }

    .nav-tabs {
      position: sticky; top: 0; z-index: 1000;
      background-color: %s; padding-top: 5px; margin-bottom: 0;
    }
    .nav-tabs > li > a { color: %s; font-weight: 600; font-size: 1.3rem; }
    .nav-tabs > li.active > a { color: %s !important; border-bottom: 3px solid %s; }

    .radar-title {
      font-size: 1.35rem; font-weight: 700; color: %s;
      margin-bottom: 0; margin-top: 0; padding-left: 2px;
    }
    .film-note { font-size: 1.05rem; color: %s; font-style: italic; }
    hr { border-color: %s; }

    .selectize-input { font-size: 1.1rem; }
    .selectize-dropdown-content { max-height: 350px !important; font-size: 1.05rem; }
    .selectize-input .item .remove { font-size: 1.15rem; margin-left: 4px; color: %s; }

    /* Tighten column gutters on radar plots */
    .radar-col { padding-left: 4px !important; padding-right: 4px !important; }

    .film-list-container {
      max-height: 420px; overflow-y: auto;
      border: 1px solid %s; border-radius: 4px; background: %s;
    }
    .film-list-item {
      padding: 5px 10px; cursor: pointer; font-size: 1.1rem;
      color: %s; border-bottom: 1px solid %s;
    }
    .film-list-item:hover { background-color: %s; }
    .film-list-item.selected { background-color: %s; color: white; font-weight: 600; }

    .axis-desc-section { margin-bottom: 36px; }
    .axis-desc-section h3 {
      color: %s; font-weight: 700; font-size: 1.55rem;
      border-bottom: 2px solid %s; padding-bottom: 8px;
    }
    .axis-desc-item { margin-bottom: 14px; padding-left: 12px; border-left: 3px solid %s; }
    .axis-desc-item strong { color: %s; font-size: 1.2rem; }
    .axis-desc-item p { color: %s; margin: 3px 0 0 0; font-size: 1.12rem; line-height: 1.55; }

    .section-header { color: %s; font-size: 1.4rem; font-weight: 700; margin: 18px 0 8px 0; }
    .analytics-note { color: %s; font-size: 1.05rem; font-style: italic; margin: 4px 0 14px 0; }

    /* Checkbox label sizing */
    .checkbox label, .radio label { font-size: 1.1rem; }
    .form-group label { font-size: 1.1rem; }
    .shiny-input-container label { font-size: 1.1rem; }
    .btn { font-size: 1.1rem; }
    .table { font-size: 1.05rem; }
  ",
    bg_main, bg_well, border_col,
    bg_main, text_muted, accent_1, accent_1,
    accent_2, text_muted, border_col, accent_1,
    border_col, bg_main, text_main, border_col, bg_well, accent_2,
    accent_1, accent_1, border_col, accent_2, text_muted,
    accent_2, text_muted
  )))),

  titlePanel(
    div(
      h2("Campbell's Monomyth \u2014 Film Archetype Radar",
         style = sprintf("color:%s; margin:0; font-weight:700; font-size:1.7rem", accent_2)),
      p("Scoring films against Joseph Campbell's Hero's Journey framework",
        style = sprintf("color:%s; font-size:1.05rem; margin:0", text_muted))
    )
  ),

  tabsetPanel(
    # ══ TAB 1: Compare Films ═════════════════════════════════════════════════
    tabPanel("Compare Films",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Film Selection", style = sprintf("color:%s; font-size:1.3rem", accent_2)),
            selectizeInput("selected_films", label = NULL,
              choices = film_titles,
              selected = c("star_wars_iv", "the_matrix", "no_country_for_old_men"),
              multiple = TRUE,
              options = list(maxItems = 10, placeholder = "Type to search films...",
                             plugins = list("remove_button"))
            ),
            hr(),
            h4("Axis Sets", style = sprintf("color:%s; font-size:1.25rem", accent_2)),
            checkboxGroupInput("selected_axis_sets", label = NULL,
              choices = axis_set_labels, selected = names(axis_set_labels)[1:2]
            ),
            hr(),
            sliderInput("plot_height", "Plot height (px)", 350, 800, 480, step = 50)
          )
        ),
        column(9, uiOutput("comparison_plots"))
      )
    ),

    # ══ TAB 2: Single Film ════════════════════════════════════════════════════
    tabPanel("Single Film",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Select Film", style = sprintf("color:%s; font-size:1.3rem", accent_2)),
            textInput("film_search", NULL, placeholder = "Type to filter..."),
            uiOutput("film_list_ui"),
            hr(),
            uiOutput("film_info_box")
          )
        ),
        column(9,
          fluidRow(
            column(4, class = "radar-col",
              div(class = "radar-title", axis_data[["vogler_12"]]$label),
              plotOutput("solo_radar_v12", height = "400px")
            ),
            column(4, class = "radar-col",
              div(class = "radar-title", axis_data[["campbell_17"]]$label),
              plotOutput("solo_radar_c17", height = "400px")
            ),
            column(4, class = "radar-col",
              div(class = "radar-title", axis_data[["mixed_archetype"]]$label),
              plotOutput("solo_radar_mix", height = "400px")
            )
          ),
          plotOutput("solo_bar_all", height = "380px")
        )
      )
    ),

    # ══ TAB 3: Cluster Analysis ═══════════════════════════════════════════════
    tabPanel("Cluster Analysis",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Settings", style = sprintf("color:%s; font-size:1.3rem", accent_2)),
            selectInput("cluster_axis", "Axis set",
              choices = axis_set_labels, selected = names(axis_set_labels)[1]
            ),
            checkboxInput("cluster_auto_k", "Auto-detect optimal k (Gap Statistic)", value = TRUE),
            conditionalPanel(
              condition = "!input.cluster_auto_k",
              sliderInput("cluster_k", "Number of clusters", 2, 8, 5, step = 1)
            ),
            hr(),
            uiOutput("cluster_summary_box")
          )
        ),
        column(9,
          div(class = "section-header", "Gap Statistic (Tibshirani 2001 / Van Moorter et al. 2010)"),
          p(class = "analytics-note",
            "Optimal k selected by the 1-SE rule: smallest k where gap(k) \u2265 gap(k+1) \u2212 SE(k+1)."),
          plotOutput("gap_plot", height = "340px"),
          div(class = "section-header", "PCA Biplot"),
          plotOutput("pca_plot", height = "540px"),
          div(class = "section-header", "Cluster Profile Radars"),
          plotOutput("cluster_radars", height = "500px"),
          div(class = "section-header", "Cluster Interpretation"),
          p(class = "analytics-note",
            "Key distinguishing axes per cluster vs. overall mean. Positive = above average, negative = below."),
          uiOutput("cluster_interpretation"),
          div(class = "section-header", "Cluster Membership"),
          uiOutput("cluster_membership_text")
        )
      )
    ),

    # ══ TAB 4: Analytics Dashboard ════════════════════════════════════════════
    tabPanel("Analytics",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Dashboard", style = sprintf("color:%s; font-size:1.3rem", accent_2)),
            selectInput("analytics_axis", "Axis set",
              choices = axis_set_labels, selected = names(axis_set_labels)[1]
            ),
            hr(),
            p(sprintf("Films loaded: %d", length(films_data)),
              style = sprintf("color:%s; font-size:1.1rem", text_muted)),
            p(sprintf("Year range: %s\u2013%s",
                min(sapply(films_data, function(f) if(is.null(f$year)) NA else f$year), na.rm=TRUE),
                max(sapply(films_data, function(f) if(is.null(f$year)) NA else f$year), na.rm=TRUE)),
              style = sprintf("color:%s; font-size:1.1rem", text_muted))
          )
        ),
        column(9,
          div(class = "section-header", "Mean Score by Category (sorted)"),
          plotOutput("analytics_category_bars", height = "420px"),

          div(class = "section-header", "Total Monomyth Score by Film"),
          p(class = "analytics-note",
            "Sum of all axis scores. Higher = closer to textbook monomyth structure."),
          plotOutput("analytics_total_score", height = "500px"),

          div(class = "section-header", "Score Distributions by Category"),
          plotOutput("analytics_distributions", height = "520px"),

          div(class = "section-header", "Generational Trends: Mean Scores by Decade"),
          p(class = "analytics-note",
            "How does narrative structure shift across eras?"),
          plotOutput("analytics_decade_heatmap", height = "440px"),

          div(class = "section-header", "Total Monomyth Score by Decade"),
          plotOutput("analytics_decade_totals", height = "360px"),

          div(class = "section-header",
              "US Political Mood vs. Monomyth Score (contemporaneous & 20-year lag)"),
          p(class = "analytics-note",
            "Does the cultural mood when filmmakers grew up (~20 years prior) predict monomyth adherence?
             Sources: U. Michigan Consumer Sentiment, Gallup Presidential Approval, CRS conflict data."),
          plotOutput("analytics_mood_contemp", height = "360px"),
          plotOutput("analytics_mood_lagged", height = "360px"),

          div(class = "section-header", "Film Similarity: Total Score vs. Distinctiveness"),
          p(class = "analytics-note",
            "Each point is a film. X = total monomyth score. Y = mean distance to all other films (higher = more unique profile)."),
          plotOutput("analytics_similarity_scatter", height = "540px"),

          div(class = "section-header", "Most Similar Film Pairs"),
          p(class = "analytics-note",
            "Top 25 most similar pairs based on standardised Euclidean distance across all category scores."),
          tableOutput("analytics_similar_pairs")
        )
      )
    ),

    # ══ TAB 5: AI Score New Film ══════════════════════════════════════════════
    tabPanel("AI Score New Film",
      br(),
      fluidRow(
        column(4,
          wellPanel(
            h4("Score any film via Claude API",
               style = sprintf("color:%s; font-size:1.3rem", accent_2)),
            textInput("ai_film_title", "Film title",
                      placeholder = "e.g. Everything Everywhere All at Once"),
            selectInput("ai_axis_set", "Axis set",
              choices = axis_set_labels, selected = names(axis_set_labels)[1]
            ),
            passwordInput("api_key", "Anthropic API key"),
            actionButton("run_ai_score", "Score Film", class = "btn-primary", width = "100%"),
            br(), br(),
            tags$small(style = sprintf("color:%s; font-size:1.05rem", text_muted),
              "API key is used only for this request and not stored.")
          )
        ),
        column(8,
          uiOutput("ai_status"),
          plotOutput("ai_radar", height = "500px"),
          verbatimTextOutput("ai_scores_table")
        )
      )
    ),

    # ══ TAB 6: ? Axis Sets ═══════════════════════════════════════════════════
    tabPanel("? Axis Sets",
      br(),
      fluidRow(
        column(10, offset = 1,
          h3("Understanding the Three Scoring Frameworks",
             style = sprintf("color:%s; margin-bottom:20px; font-size:1.65rem", accent_2)),
          p("Each film is scored on three complementary frameworks that capture
             different aspects of narrative structure. Scores range from 0
             (absent/antithetical) to 10 (structurally central, fully realised).",
            style = sprintf("color:%s; font-size:1.15rem; margin-bottom:28px; line-height:1.6",
                            text_muted)),
          uiOutput("axis_descriptions_ui")
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Single Film: state ─────────────────────────────────────────────────────
  selected_film <- reactiveVal("star_wars_iv")
  observeEvent(input$single_film_click, { selected_film(input$single_film_click) })

  output$film_list_ui <- renderUI({
    search <- tolower(input$film_search %||% "")
    current <- selected_film()
    matched <- if (nchar(search) == 0) film_titles
               else film_titles[grepl(search, tolower(names(film_titles)), fixed = TRUE)]
    if (length(matched) == 0) {
      return(div(class = "film-list-container",
        p("No films match.", style = sprintf("padding:10px; color:%s", text_muted))))
    }
    items <- lapply(seq_along(matched), function(i) {
      fid <- unname(matched[i]); fname <- names(matched)[i]
      cls <- if (fid == current) "film-list-item selected" else "film-list-item"
      tags$div(class = cls,
        onclick = sprintf("Shiny.setInputValue('single_film_click','%s',{priority:'event'})", fid),
        fname)
    })
    div(class = "film-list-container", tagList(items))
  })

  # ── TAB 1: Compare Films ──────────────────────────────────────────────────
  output$comparison_plots <- renderUI({
    req(input$selected_films, input$selected_axis_sets)
    if (length(input$selected_films) == 0)
      return(p("Select at least one film.", style = sprintf("color:%s; font-size:1.15rem", text_muted)))
    if (length(input$selected_axis_sets) == 0)
      return(p("Select at least one axis set.", style = sprintf("color:%s; font-size:1.15rem", text_muted)))
    ph <- input$plot_height
    plot_rows <- lapply(input$selected_axis_sets, function(ax_id) {
      plot_id <- paste0("cmp_", ax_id)
      div(style = "margin-bottom: 0px;",
        div(class = "radar-title", axis_data[[ax_id]]$label),
        plotOutput(plot_id, height = paste0(ph, "px"), width = "88%")
      )
    })
    tagList(plot_rows)
  })

  observe({
    req(input$selected_films, input$selected_axis_sets)
    for (ax_id in names(axis_data)) {
      local({
        aid <- ax_id; pid <- paste0("cmp_", aid)
        output[[pid]] <- renderPlot({
          if (!aid %in% input$selected_axis_sets || length(input$selected_films) == 0) return(NULL)
          make_radar_plot(input$selected_films, aid)
        }, bg = bg_main)
      })
    }
  })

  # ── TAB 2: Single Film ────────────────────────────────────────────────────
  output$film_info_box <- renderUI({
    fid <- selected_film(); req(fid)
    film <- Filter(function(f) f$id == fid, films_data)[[1]]
    director <- if (is.null(film$director) || film$director == "") "Unknown" else film$director
    genres <- if (is.null(film$genre) || length(film$genre) == 0) "" else paste(film$genre, collapse = " \u00b7 ")
    tagList(
      p(strong(film$title), " (", film$year, ")",
        style = sprintf("color:%s; font-size:1.25rem", accent_2)),
      p(director, style = sprintf("color:%s; font-size:1.1rem", text_muted)),
      if (nchar(genres) > 0) p(genres, style = sprintf("color:%s; font-size:1.05rem", accent_4)),
      hr(),
      if (!is.null(film$notes) && nchar(film$notes) > 0) p(film$notes, class = "film-note")
    )
  })

  solo_radar_render <- function(ax_id) {
    renderPlot({
      fid <- selected_film(); req(fid)
      par(mar = c(1, 3, 1, 3))
      make_radar_plot(fid, ax_id)
    }, bg = bg_main)
  }
  output$solo_radar_v12 <- solo_radar_render("vogler_12")
  output$solo_radar_c17 <- solo_radar_render("campbell_17")
  output$solo_radar_mix <- solo_radar_render("mixed_archetype")

  output$solo_bar_all <- renderPlot({
    fid <- selected_film(); req(fid)
    film <- Filter(function(f) f$id == fid, films_data)[[1]]
    # Build in explicit order: vogler, campbell, mixed
    rows <- lapply(axis_set_order, function(ax_id) {
      axes <- axis_data[[ax_id]]$axes
      labels <- sapply(axes, `[[`, "label")
      ids <- sapply(axes, `[[`, "id")
      scores <- film$scores[[ax_id]]
      vals <- sapply(ids, function(i) scores[[i]])
      data.frame(axis_set = axis_data[[ax_id]]$label, label = labels,
                 score = vals, stringsAsFactors = FALSE)
    })
    df <- bind_rows(rows)
    # Enforce facet order
    set_levels <- sapply(axis_set_order, function(id) axis_data[[id]]$label)
    df$axis_set <- factor(df$axis_set, levels = set_levels)

    ggplot(df, aes(x = reorder(label, score), y = score, fill = axis_set)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~axis_set, scales = "free_y", ncol = 3) +
      coord_flip() +
      scale_fill_manual(values = setNames(c(accent_2, accent_4, accent_1), set_levels)) +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(x = NULL, y = "Score (0\u201310)",
           title = paste0(film$title, " \u2014 All Axis Sets")) +
      theme_campbell(17) +
      theme(
        strip.text = element_text(colour = accent_2, size = 15, face = "bold",
                                  hjust = 0, margin = margin(b = 4)),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 0)
      )
  }, bg = bg_main)

  # ── TAB 3: Cluster Analysis ────────────────────────────────────────────────
  cluster_data <- reactive({
    ax_id <- input$cluster_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    if (nrow(mat) < 4) return(NULL)

    # Determine k
    if (isTRUE(input$cluster_auto_k)) {
      gap_res <- find_optimal_k(mat, k_max = 8)
      k <- gap_res$k_opt
      gap_obj <- gap_res$gap_obj
    } else {
      k <- input$cluster_k
      gap_obj <- tryCatch(
        clusGap(scale(mat), FUN = kmeans, K.max = min(8, nrow(mat) - 1), B = 50, nstart = 25),
        error = function(e) NULL
      )
    }
    if (nrow(mat) <= k) return(NULL)

    mat_sc <- scale(mat)
    pca <- prcomp(mat_sc, center = FALSE, scale. = FALSE)
    hc <- hclust(dist(mat_sc), method = "ward.D2")
    clusters <- cutree(hc, k = k)
    list(mat = mat, mat_sc = mat_sc, pca = pca, hc = hc,
         clusters = clusters, k = k, ax_id = ax_id, gap_obj = gap_obj)
  })

  output$cluster_summary_box <- renderUI({
    cd <- cluster_data()
    if (is.null(cd)) return(p("Not enough films for clustering.",
                               style = sprintf("color:%s; font-size:1.1rem", accent_1)))
    var_exp <- round(summary(cd$pca)$importance[2, 1:2] * 100, 1)
    tagList(
      p(sprintf("Films: %d", nrow(cd$mat)), style = sprintf("color:%s; font-size:1.1rem", text_muted)),
      p(sprintf("Optimal k: %d", cd$k),
        style = sprintf("color:%s; font-size:1.15rem; font-weight:600", accent_1)),
      p(sprintf("PC1: %.1f%% | PC2: %.1f%%", var_exp[1], var_exp[2]),
        style = sprintf("color:%s; font-size:1.1rem", text_muted)),
      hr(),
      p("Cluster sizes:", style = sprintf("color:%s; font-size:1.1rem; margin-bottom:4px", accent_2)),
      tags$ul(style = sprintf("color:%s; font-size:1.05rem", text_muted),
        lapply(seq_len(cd$k), function(i) {
          tags$li(sprintf("Cluster %d: %d films", i, sum(cd$clusters == i)))
        })
      )
    )
  })

  output$gap_plot <- renderPlot({
    cd <- cluster_data(); req(cd, cd$gap_obj)
    tab <- cd$gap_obj$Tab
    df <- data.frame(k = seq_len(nrow(tab)), gap = tab[, "gap"], se = tab[, "SE.sim"])

    ggplot(df, aes(x = k, y = gap)) +
      geom_line(colour = accent_2, linewidth = 1) +
      geom_point(size = 3, colour = accent_2) +
      geom_errorbar(aes(ymin = gap - se, ymax = gap + se), width = 0.3, colour = accent_2) +
      geom_vline(xintercept = cd$k, linetype = 2, colour = accent_1, linewidth = 0.8) +
      annotate("text", x = cd$k + 0.3, y = max(df$gap), label = sprintf("k = %d", cd$k),
               colour = accent_1, size = 5.5, hjust = 0, fontface = "bold") +
      scale_x_continuous(breaks = df$k) +
      labs(title = "Gap Statistic by Number of Clusters",
           x = "Number of Clusters (k)", y = "Gap Statistic") +
      theme_campbell(17)
  }, bg = bg_main)

  output$pca_plot <- renderPlot({
    cd <- cluster_data(); req(cd)
    var_exp <- round(summary(cd$pca)$importance[2, 1:2] * 100, 1)
    df <- data.frame(PC1 = cd$pca$x[,1], PC2 = cd$pca$x[,2],
                     label = rownames(cd$mat), cluster = factor(cd$clusters))
    ggplot(df, aes(PC1, PC2, colour = cluster, label = label)) +
      geom_point(size = 4, alpha = 0.85) +
      geom_text_repel(size = 4.2, max.overlaps = 25, segment.colour = text_muted,
                      show.legend = FALSE) +
      stat_ellipse(aes(group = cluster), type = "t", level = 0.75,
                   linetype = 2, linewidth = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = radar_colours[seq_len(cd$k)], name = "Cluster") +
      labs(title = paste0("PCA: ", axis_data[[cd$ax_id]]$label),
           subtitle = sprintf("PC1 %.1f%% | PC2 %.1f%% variance", var_exp[1], var_exp[2]),
           x = sprintf("PC1 (%.1f%%)", var_exp[1]), y = sprintf("PC2 (%.1f%%)", var_exp[2])) +
      theme_campbell(17) +
      theme(legend.text = element_text(size = 14), legend.title = element_text(size = 15))
  }, bg = bg_main)

  output$cluster_radars <- renderPlot({
    cd <- cluster_data(); req(cd)
    k <- cd$k; mat <- cd$mat; clusters <- cd$clusters; n_axes <- ncol(mat)
    cex_lab <- if (n_axes > 14) 1.10 else if (n_axes > 10) 1.20 else 1.30

    par(bg = bg_main, mfrow = c(ceiling(k / 3), min(k, 3)),
        mar = c(1, 1, 2.5, 1), oma = c(0, 0, 3, 0), family = "sans")
    for (i in seq_len(k)) {
      means <- colMeans(mat[clusters == i, , drop = FALSE])
      df_i <- rbind(rep(10, n_axes), rep(0, n_axes), as.data.frame(t(means)))
      colnames(df_i) <- colnames(mat)
      rownames(df_i) <- c("max", "min", paste0("C", i))
      fmsb::radarchart(df_i, axistype = 1,
        pcol = radar_colours[i], pfcol = adjustcolor(radar_colours[i], 0.25), plwd = 2.5,
        cglcol = border_col, cglty = 1, axislabcol = text_muted, vlcex = cex_lab,
        caxislabels = c("", "2.5", "5", "7.5", ""), calcex = 0.75, seg = 4)
      title(main = sprintf("Cluster %d (n=%d)", i, sum(clusters == i)),
            col.main = radar_colours[i], cex.main = 1.5)
    }
    mtext(paste0("Cluster Profiles: ", axis_data[[cd$ax_id]]$label),
          outer = TRUE, col = accent_2, cex = 1.6, font = 2)
  }, bg = bg_main)

  output$cluster_interpretation <- renderUI({
    cd <- cluster_data(); req(cd)
    mat <- cd$mat; clusters <- cd$clusters; k <- cd$k
    overall_mean <- colMeans(mat)
    overall_total <- mean(rowSums(mat))

    # PCA loadings summary
    loadings <- cd$pca$rotation[, 1:min(2, ncol(cd$pca$rotation))]
    pc1_top <- names(sort(abs(loadings[,1]), decreasing = TRUE))[1:3]
    pc1_desc <- paste0("PC1 driven by: ", paste(pc1_top, collapse = ", "))

    interp_items <- lapply(seq_len(k), function(i) {
      cl_mat <- mat[clusters == i, , drop = FALSE]
      cl_mean <- colMeans(cl_mat)
      diff <- cl_mean - overall_mean

      top_high <- head(names(sort(diff, decreasing = TRUE)), 3)
      top_low  <- head(names(sort(diff)), 3)
      high_str <- paste(sprintf("%s (+%.1f)", top_high, diff[top_high]), collapse = "; ")
      low_str  <- paste(sprintf("%s (%.1f)", top_low, diff[top_low]), collapse = "; ")

      mean_total <- mean(rowSums(cl_mat))
      # Sample films in this cluster
      cl_films <- sub(" \\(\\d{4}\\)$", "", rownames(cl_mat))
      sample_films <- paste(head(cl_films, 5), collapse = ", ")
      if (length(cl_films) > 5) sample_films <- paste0(sample_films, ", ...")

      div(style = sprintf("margin-bottom:16px; padding:12px; border-left:4px solid %s; background:%s;",
                           radar_colours[i], bg_well),
        strong(sprintf("Cluster %d", i),
               style = sprintf("color:%s; font-size:1.25rem", radar_colours[i])),
        span(sprintf(" \u2014 %d films, mean total: %.0f (overall: %.0f)",
                     sum(clusters == i), mean_total, overall_total),
             style = sprintf("color:%s; font-size:1.1rem", text_muted)),
        br(), br(),
        strong("Above average: ", style = "font-size:1.1rem"), high_str,
        br(),
        strong("Below average: ", style = "font-size:1.1rem"), low_str,
        br(),
        span(paste0("e.g. ", sample_films),
             style = sprintf("color:%s; font-size:1.05rem; font-style:italic", text_muted))
      )
    })
    tagList(
      p(pc1_desc, style = sprintf("color:%s; font-size:1.1rem; margin-bottom:12px", accent_2)),
      interp_items
    )
  })

  output$cluster_membership_text <- renderUI({
    cd <- cluster_data(); req(cd)
    films <- sub(" \\(\\d{4}\\)$", "", rownames(cd$mat))
    clusters <- cd$clusters
    paras <- lapply(seq_len(cd$k), function(i) {
      cl_films <- sort(films[clusters == i])
      p(
        strong(sprintf("Cluster %d (%d): ", i, length(cl_films)),
               style = sprintf("color:%s", radar_colours[i])),
        paste(cl_films, collapse = ", "),
        style = sprintf("color:%s; font-size:1.08rem; line-height:1.6; margin-bottom:10px", text_main)
      )
    })
    tagList(paras)
  })

  # ── TAB 4: Analytics ──────────────────────────────────────────────────────
  output$analytics_category_bars <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    means <- sort(colMeans(mat, na.rm = TRUE), decreasing = TRUE)
    df <- data.frame(axis = factor(names(means), levels = names(means)), mean_score = means)
    ggplot(df, aes(x = axis, y = mean_score, fill = mean_score)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = accent_2, high = accent_1, guide = "none") +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(x = NULL, y = "Mean Score",
           title = paste0("Mean Score per Category \u2014 ", axis_data[[ax_id]]$label),
           subtitle = sprintf("Across %d films", nrow(mat))) +
      theme_campbell(17) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 13))
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_total_score <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    totals <- sort(rowSums(mat, na.rm = TRUE), decreasing = TRUE)
    df <- data.frame(film = factor(names(totals), levels = names(totals)), total = totals)
    ggplot(df, aes(x = film, y = total, fill = total)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = accent_4, high = accent_1, guide = "none") +
      labs(x = NULL, y = "Total Score",
           title = paste0("Total Monomyth Score \u2014 ", axis_data[[ax_id]]$label)) +
      theme_campbell(17) +
      theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 10)) +
      coord_cartesian(ylim = c(0, max(totals) * 1.05))
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_distributions <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    scores_long <- build_scores_long() |> filter(axis_id == ax_id)
    ggplot(scores_long, aes(x = reorder(axis, score, median), y = score)) +
      geom_violin(fill = accent_2, alpha = 0.3, colour = NA) +
      geom_boxplot(width = 0.2, outlier.size = 1.5, colour = text_main, fill = NA) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(x = NULL, y = "Score (0\u201310)",
           title = paste0("Score Distributions \u2014 ", axis_data[[ax_id]]$label),
           subtitle = sprintf("n = %d films", length(unique(scores_long$film)))) +
      theme_campbell(17)
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_decade_heatmap <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    scores_long <- build_scores_long() |>
      filter(axis_id == ax_id, !is.na(year)) |>
      mutate(decade = paste0(floor(year / 10) * 10, "s"))
    decade_means <- scores_long |>
      group_by(decade, axis) |>
      summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")
    if (nrow(decade_means) == 0) return(NULL)
    ggplot(decade_means, aes(x = decade, y = axis, fill = mean_score)) +
      geom_tile(colour = bg_main, linewidth = 1) +
      geom_text(aes(label = sprintf("%.1f", mean_score)), size = 4.5, colour = text_main) +
      scale_fill_gradient2(low = accent_2, mid = accent_3, high = accent_1,
                           midpoint = 5, limits = c(0, 10), name = "Mean") +
      labs(x = "Decade", y = NULL,
           title = paste0("Generational Trends \u2014 ", axis_data[[ax_id]]$label)) +
      theme_campbell(17) +
      theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 13),
            panel.grid = element_blank(), legend.text = element_text(size = 13))
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_decade_totals <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    years <- as.integer(gsub(".*\\((\\d{4})\\).*", "\\1", rownames(mat)))
    totals <- rowSums(mat, na.rm = TRUE)
    df <- data.frame(year = years, total = totals) |>
      filter(!is.na(year)) |>
      mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
      group_by(decade) |> summarise(mean_total = mean(total), n = n(), .groups = "drop")
    ggplot(df, aes(x = decade, y = mean_total)) +
      geom_col(fill = accent_2, width = 0.6) +
      geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 5, colour = text_muted) +
      labs(x = "Decade", y = "Mean Total Score",
           title = paste0("Mean Total Score by Decade \u2014 ", axis_data[[ax_id]]$label)) +
      theme_campbell(17) +
      coord_cartesian(ylim = c(0, max(df$mean_total) * 1.15))
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  # ── Political mood correlation plots ───────────────────────────────────────
  mood_decade_scores <- reactive({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    years <- as.integer(gsub(".*\\((\\d{4})\\).*", "\\1", rownames(mat)))
    totals <- rowSums(mat, na.rm = TRUE)
    df <- data.frame(year = years, total = totals) |>
      filter(!is.na(year)) |>
      mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
      group_by(decade) |>
      summarise(mean_total = mean(total), n = n(), .groups = "drop") |>
      inner_join(us_mood_data, by = "decade")
    df
  })

  output$analytics_mood_contemp <- renderPlot({
    df <- mood_decade_scores(); req(nrow(df) >= 3)

    # Reshape for faceting
    df_long <- df |>
      select(decade, mean_total, consumer_sentiment, presidential_approval, conflict_intensity) |>
      pivot_longer(cols = c(consumer_sentiment, presidential_approval, conflict_intensity),
                   names_to = "index", values_to = "value") |>
      filter(!is.na(value)) |>
      mutate(index = case_match(index,
        "consumer_sentiment"    ~ "Consumer Sentiment",
        "presidential_approval" ~ "Presidential Approval %",
        "conflict_intensity"    ~ "Conflict Intensity (0-10)"
      ))

    # Compute correlations per index
    cors <- df_long |> group_by(index) |>
      summarise(r = cor(mean_total, value, use = "complete.obs"),
                n = sum(!is.na(value)), .groups = "drop") |>
      mutate(label = sprintf("r = %.2f (n=%d)", r, n))

    ggplot(df_long, aes(x = value, y = mean_total)) +
      geom_point(size = 4, colour = accent_1) +
      geom_smooth(method = "lm", se = TRUE, colour = accent_2, fill = accent_2, alpha = 0.15) +
      geom_text_repel(aes(label = decade), size = 4, colour = text_muted) +
      facet_wrap(~index, scales = "free_x", ncol = 3) +
      geom_text(data = cors, aes(label = label, x = Inf, y = Inf),
                hjust = 1.1, vjust = 1.5, size = 5, colour = accent_2, inherit.aes = FALSE) +
      labs(title = "Contemporaneous: US Mood vs. Monomyth Score (same decade)",
           x = "Index Value", y = "Mean Total Monomyth Score") +
      theme_campbell(16)
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_mood_lagged <- renderPlot({
    df <- mood_decade_scores()
    req(nrow(df) >= 3)

    # Create 20-year lag: film decade's score vs. mood 2 decades prior
    df_lag <- df |> mutate(
      decade_num = as.integer(gsub("s$", "", decade)),
      lag_decade = paste0(decade_num - 20, "s")
    ) |>
      select(decade, mean_total, lag_decade) |>
      inner_join(us_mood_data, by = c("lag_decade" = "decade"))

    if (nrow(df_lag) < 3) return(NULL)

    df_long <- df_lag |>
      pivot_longer(cols = c(consumer_sentiment, presidential_approval, conflict_intensity),
                   names_to = "index", values_to = "value") |>
      filter(!is.na(value)) |>
      mutate(index = case_match(index,
        "consumer_sentiment"    ~ "Consumer Sentiment (lag 20y)",
        "presidential_approval" ~ "Pres. Approval % (lag 20y)",
        "conflict_intensity"    ~ "Conflict Intensity (lag 20y)"
      ))

    cors <- df_long |> group_by(index) |>
      summarise(r = cor(mean_total, value, use = "complete.obs"),
                n = sum(!is.na(value)), .groups = "drop") |>
      mutate(label = sprintf("r = %.2f (n=%d)", r, n))

    ggplot(df_long, aes(x = value, y = mean_total)) +
      geom_point(size = 4, colour = accent_5) +
      geom_smooth(method = "lm", se = TRUE, colour = accent_2, fill = accent_2, alpha = 0.15) +
      geom_text_repel(aes(label = decade), size = 4, colour = text_muted) +
      facet_wrap(~index, scales = "free_x", ncol = 3) +
      geom_text(data = cors, aes(label = label, x = Inf, y = Inf),
                hjust = 1.1, vjust = 1.5, size = 5, colour = accent_2, inherit.aes = FALSE) +
      labs(title = "Lagged 20 years: US Mood when filmmakers grew up vs. Monomyth Score",
           subtitle = "e.g. 2000s films correlated with 1980s mood indices",
           x = "Index Value (20 years prior)", y = "Mean Total Monomyth Score") +
      theme_campbell(16)
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  # ── Similarity analysis ──────────────────────────────────────────────────
  output$analytics_similarity_scatter <- renderPlot({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    mat_sc <- scale(mat)
    d <- as.matrix(dist(mat_sc))

    labels <- sub(" \\(\\d{4}\\)$", "", rownames(mat))
    total_scores <- rowSums(mat)
    mean_dist <- rowMeans(d)

    df <- data.frame(film = labels, total = total_scores, mean_dist = mean_dist)

    ggplot(df, aes(x = total, y = mean_dist, label = film)) +
      geom_point(size = 3.5, colour = accent_1, alpha = 0.8) +
      geom_text_repel(size = 4, max.overlaps = 25, colour = text_muted,
                      segment.colour = border_col) +
      labs(title = paste0("Total Score vs. Distinctiveness \u2014 ", axis_data[[ax_id]]$label),
           subtitle = "Bottom = typical profile (similar to many films). Top = unique profile. Right = high total monomyth adherence.",
           x = "Total Monomyth Score", y = "Mean Distance to All Other Films") +
      theme_campbell(16)
  }, bg = bg_main) |> bindCache(input$analytics_axis)

  output$analytics_similar_pairs <- renderTable({
    ax_id <- input$analytics_axis; req(ax_id)
    mat <- build_score_matrix(ax_id)
    mat_sc <- scale(mat)
    d <- as.matrix(dist(mat_sc))
    labels <- sub(" \\(\\d{4}\\)$", "", rownames(d))
    rownames(d) <- labels; colnames(d) <- labels

    pairs <- which(upper.tri(d), arr.ind = TRUE)
    df <- data.frame(
      Film_1 = labels[pairs[, 1]],
      Film_2 = labels[pairs[, 2]],
      Distance = round(d[pairs], 2)
    ) |> arrange(Distance) |> head(25)
    df$Rank <- seq_len(nrow(df))
    df[, c("Rank", "Film_1", "Film_2", "Distance")]
  }, striped = TRUE, hover = TRUE, width = "100%", spacing = "s", digits = 2) |>
    bindCache(input$analytics_axis)

  # ── TAB 5: AI Scoring ──────────────────────────────────────────────────────
  ai_result <- reactiveVal(NULL)
  observeEvent(input$run_ai_score, {
    req(input$ai_film_title, input$api_key, input$ai_axis_set)
    ai_result(NULL)
    output$ai_status <- renderUI(
      p("Querying Claude API...", style = sprintf("color:%s; font-size:1.15rem", accent_3)))
    result <- tryCatch(score_film_api(input$ai_film_title, input$ai_axis_set, input$api_key),
                       error = function(e) list(error = conditionMessage(e)))
    if (!is.null(result$error)) {
      output$ai_status <- renderUI(
        p(paste("Error:", result$error), style = sprintf("color:%s; font-size:1.15rem", accent_1)))
    } else {
      ai_result(result)
      output$ai_status <- renderUI(
        p(paste0("Scored: ", input$ai_film_title), style = sprintf("color:%s; font-size:1.15rem", accent_4)))
    }
  })

  output$ai_radar <- renderPlot({
    scores <- ai_result(); req(scores); n <- length(scores)
    df <- as.data.frame(t(data.frame(film = scores)))
    rownames(df) <- input$ai_film_title
    df_plot <- rbind(rep(10, n), rep(0, n), df)
    rownames(df_plot)[1:2] <- c("max", "min")
    colnames(df_plot) <- names(scores)
    cex_lab <- if (n > 14) 1.10 else if (n > 10) 1.20 else 1.30
    par(mar = c(1, 2, 3, 2))
    fmsb::radarchart(df_plot, axistype = 1,
      pcol = accent_1, pfcol = adjustcolor(accent_1, 0.15), plwd = 2.5,
      cglcol = border_col, cglty = 1, axislabcol = text_muted,
      vlcex = cex_lab, caxislabels = seq(0, 10, 2.5), calcex = 0.9, seg = 4)
    title(main = paste0(input$ai_film_title, "\n", axis_data[[input$ai_axis_set]]$label),
          col.main = accent_2, cex.main = 1.4)
  }, bg = bg_main)

  output$ai_scores_table <- renderPrint({
    scores <- ai_result(); req(scores)
    df <- data.frame(Axis = names(scores), Score = as.integer(scores))
    print(df[order(df$Score, decreasing = TRUE), ], row.names = FALSE)
  })

  # ── TAB 6: Axis Descriptions ──────────────────────────────────────────────
  output$axis_descriptions_ui <- renderUI({
    sections <- lapply(names(axis_data), function(ax_id) {
      ax_def <- axis_data[[ax_id]]; descs <- axis_descriptions[[ax_id]]
      axes <- ax_def$axes; n_axes <- length(axes)
      items <- lapply(axes, function(ax) {
        desc_text <- descs[[ax$id]]; if (is.null(desc_text)) desc_text <- ""
        div(class = "axis-desc-item", strong(ax$label), p(desc_text))
      })
      div(class = "axis-desc-section",
        h3(paste0(ax_def$label, " (", n_axes, " axes)")), tagList(items))
    })
    tagList(sections)
  })
}

shinyApp(ui, server)
