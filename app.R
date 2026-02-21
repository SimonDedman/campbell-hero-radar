library(shiny)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)
library(httr2)
library(shinydashboard)
library(shinyWidgets)

# ── Load data ──────────────────────────────────────────────────────────────────
films_data   <- fromJSON("data/films.json", simplifyVector = FALSE)$films
axis_data    <- fromJSON("data/axis_sets.json", simplifyVector = FALSE)

film_titles  <- setNames(
  sapply(films_data, `[[`, "id"),
  sapply(films_data, `[[`, "title")
)

axis_set_labels <- setNames(
  names(axis_data),
  sapply(axis_data, `[[`, "label")
)

# ── Helpers ────────────────────────────────────────────────────────────────────
get_film_scores <- function(film_id, axis_set_id) {
  film <- Filter(function(f) f$id == film_id, films_data)[[1]]
  scores <- film$scores[[axis_set_id]]
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  ids    <- sapply(axes, `[[`, "id")
  vals   <- sapply(ids, function(i) scores[[i]])
  setNames(vals, labels)
}

campbell_colours <- c(
  "#E63946", "#457B9D", "#2A9D8F", "#E9C46A",
  "#F4A261"
)

make_radar_plot <- function(film_ids, axis_set_id, film_lookup) {
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  n_axes <- length(labels)

  score_list <- lapply(film_ids, function(fid) {
    get_film_scores(fid, axis_set_id)
  })

  titles <- sapply(film_ids, function(fid) {
    film_lookup[[fid]]
  })

  df <- as.data.frame(do.call(rbind, score_list))
  colnames(df) <- labels
  rownames(df) <- titles

  # fmsb needs max/min rows prepended
  df_plot <- rbind(rep(10, n_axes), rep(0, n_axes), df)
  rownames(df_plot)[1:2] <- c("max", "min")

  colours_fill   <- adjustcolor(campbell_colours[seq_along(film_ids)], alpha.f = 0.2)
  colours_border <- campbell_colours[seq_along(film_ids)]

  # Dynamic label sizing
  cex_lab <- if (n_axes > 14) 0.65 else if (n_axes > 10) 0.75 else 0.85

  fmsb::radarchart(
    df_plot,
    axistype   = 1,
    pcol       = colours_border,
    pfcol      = colours_fill,
    plwd       = 2.5,
    plty       = 1,
    cglcol     = "grey80",
    cglty      = 1,
    cglwd      = 0.6,
    axislabcol = "grey50",
    vlcex      = cex_lab,
    caxislabels = seq(0, 10, 2.5),
    calcex     = 0.7,
    seg        = 4,
    title      = ""
  )

  if (length(film_ids) > 1) {
    legend(
      x = "bottomright", inset = c(-0.12, 0),
      legend = titles,
      col    = colours_border,
      lty    = 1, lwd = 2.5,
      pch    = 20, pt.cex = 1.5,
      bty    = "n",
      cex    = 0.8,
      xpd    = TRUE
    )
  }
}

# ── Score via Anthropic API ────────────────────────────────────────────────────
score_film_api <- function(film_title, axis_set_id, api_key) {
  axes   <- axis_data[[axis_set_id]]$axes
  labels <- sapply(axes, `[[`, "label")
  ids    <- sapply(axes, `[[`, "id")

  axes_str <- paste(
    mapply(function(id, lbl) sprintf('  "%s": <score 0-10>', id, lbl), ids, labels),
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

Base scores on the film\'s actual narrative structure. Be discriminating — most films won\'t score high on all axes.',
    film_title, axes_str
  )

  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "Content-Type"      = "application/json",
      "anthropic-version" = "2023-06-01"
    ) |>
    req_body_json(list(
      model      = "claude-sonnet-4-20250514",
      max_tokens = 800,
      messages   = list(list(role = "user", content = prompt))
    )) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    return(list(error = paste("API error:", resp_status(resp))))
  }

  body   <- resp_body_json(resp)
  raw    <- body$content[[1]]$text
  # Strip markdown fences if present
  raw    <- gsub("```json|```", "", raw)
  parsed <- tryCatch(fromJSON(raw), error = function(e) NULL)

  if (is.null(parsed)) return(list(error = "Failed to parse API response"))

  # Map axis ids to labels
  scores <- sapply(ids, function(i) {
    v <- parsed[[i]]
    if (is.null(v)) 5L else as.integer(v)
  })
  setNames(scores, labels)
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(
    bg       = "#1a1a2e",
    fg       = "#e0e0e0",
    primary  = "#457B9D",
    base_font = bslib::font_google("Inter")
  ),
  tags$head(tags$style(HTML("
    .well { background-color: #16213e !important; border: 1px solid #0f3460; }
    .nav-tabs > li > a { color: #a0a0c0; }
    .radar-title { font-size: 1.1rem; font-weight: 600; color: #E9C46A; margin-bottom: 4px; }
    .film-note  { font-size: 0.78rem; color: #a0a0b0; font-style: italic; }
    hr { border-color: #0f3460; }
  "))),

  titlePanel(
    div(
      h2("🗡️ Campbell's Monomyth — Film Archetype Radar", style = "color:#E9C46A; margin:0"),
      p("Scoring films against Joseph Campbell's Hero's Journey framework", style = "color:#a0a0c0; font-size:0.85rem; margin:0")
    )
  ),

  tabsetPanel(
    # ── TAB 1: Multi-film comparison ──────────────────────────────────────────
    tabPanel("Compare Films",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Film Selection", style = "color:#E9C46A"),
            checkboxGroupInput("selected_films",
              label    = NULL,
              choices  = film_titles,
              selected = c("star_wars_iv", "the_matrix", "no_country_for_old_men")
            ),
            hr(),
            h4("Axis Sets to Display", style = "color:#E9C46A"),
            checkboxGroupInput("selected_axis_sets",
              label    = NULL,
              choices  = axis_set_labels,
              selected = names(axis_set_labels)[1:2]
            ),
            hr(),
            sliderInput("plot_height", "Plot height (px)", 350, 800, 500, step = 50)
          )
        ),
        column(9,
          uiOutput("comparison_plots")
        )
      )
    ),

    # ── TAB 2: Single film deep dive ──────────────────────────────────────────
    tabPanel("Single Film",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Select Film", style = "color:#E9C46A"),
            selectInput("single_film", NULL,
              choices  = film_titles,
              selected = "star_wars_iv"
            ),
            hr(),
            uiOutput("film_info_box")
          )
        ),
        column(9,
          fluidRow(
            column(4, plotOutput("solo_radar_v12",  height = "380px")),
            column(4, plotOutput("solo_radar_c17",  height = "380px")),
            column(4, plotOutput("solo_radar_mix",  height = "380px"))
          ),
          br(),
          plotOutput("solo_bar_all", height = "320px")
        )
      )
    ),

    # ── TAB 3: AI Score new film ──────────────────────────────────────────────
    tabPanel("🤖 Score New Film (AI)",
      br(),
      fluidRow(
        column(4,
          wellPanel(
            h4("Score any film via Claude API", style = "color:#E9C46A"),
            textInput("ai_film_title", "Film title", placeholder = "e.g. Everything Everywhere All at Once"),
            selectInput("ai_axis_set", "Axis set",
              choices  = axis_set_labels,
              selected = names(axis_set_labels)[1]
            ),
            passwordInput("api_key", "Anthropic API key"),
            actionButton("run_ai_score", "Score Film", class = "btn-primary", width = "100%"),
            br(), br(),
            tags$small(style = "color:#a0a0b0",
              "API key is used only for this request and not stored."
            )
          )
        ),
        column(8,
          uiOutput("ai_status"),
          plotOutput("ai_radar", height = "480px"),
          verbatimTextOutput("ai_scores_table")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reverse lookup: id -> title
  id_to_title <- setNames(
    sapply(films_data, `[[`, "title"),
    sapply(films_data, `[[`, "id")
  )

  # ── Comparison tab ───────────────────────────────────────────────────────────
  output$comparison_plots <- renderUI({
    req(input$selected_films, input$selected_axis_sets)
    n_films    <- length(input$selected_films)
    n_axsets   <- length(input$selected_axis_sets)

    if (n_films == 0) return(p("Select at least one film.", style = "color:#a0a0b0"))
    if (n_axsets == 0) return(p("Select at least one axis set.", style = "color:#a0a0b0"))

    ph <- input$plot_height

    # One row per axis set
    plot_rows <- lapply(input$selected_axis_sets, function(ax_id) {
      ax_label <- axis_data[[ax_id]]$label
      plot_id  <- paste0("cmp_", ax_id)
      fluidRow(
        column(12,
          div(class = "radar-title", ax_label),
          plotOutput(plot_id, height = paste0(ph, "px"))
        )
      )
    })

    tagList(plot_rows)
  })

  observe({
    req(input$selected_films, input$selected_axis_sets)
    for (ax_id in names(axis_data)) {
      local({
        aid <- ax_id
        pid <- paste0("cmp_", aid)
        output[[pid]] <- renderPlot({
          if (!aid %in% input$selected_axis_sets) return(NULL)
          if (length(input$selected_films) == 0) return(NULL)
          par(mar = c(2, 2, 2, 6))
          make_radar_plot(input$selected_films, aid, id_to_title)
          title(main = "", cex.main = 1)
        }, bg = "#1a1a2e")
      })
    }
  })

  # ── Single film tab ──────────────────────────────────────────────────────────
  output$film_info_box <- renderUI({
    req(input$single_film)
    film <- Filter(function(f) f$id == input$single_film, films_data)[[1]]
    tagList(
      p(strong(film$title), " (", film$year, ")", style = "color:#E9C46A"),
      p(film$director, style = "color:#a0a0c0; font-size:0.85rem"),
      p(paste(film$genre, collapse = " · "), style = "color:#457B9D; font-size:0.8rem"),
      hr(),
      p(film$notes, class = "film-note")
    )
  })

  solo_radar_render <- function(ax_id) {
    renderPlot({
      req(input$single_film)
      par(mar = c(2, 2, 3, 2))
      make_radar_plot(input$single_film, ax_id, id_to_title)
      title(main = axis_data[[ax_id]]$label,
            col.main = "#E9C46A", cex.main = 0.9)
    }, bg = "#1a1a2e")
  }

  output$solo_radar_v12 <- solo_radar_render("vogler_12")
  output$solo_radar_c17 <- solo_radar_render("campbell_17")
  output$solo_radar_mix <- solo_radar_render("mixed_archetype")

  output$solo_bar_all <- renderPlot({
    req(input$single_film)
    film <- Filter(function(f) f$id == input$single_film, films_data)[[1]]

    rows <- lapply(names(axis_data), function(ax_id) {
      axes   <- axis_data[[ax_id]]$axes
      labels <- sapply(axes, `[[`, "label")
      ids    <- sapply(axes, `[[`, "id")
      scores <- film$scores[[ax_id]]
      vals   <- sapply(ids, function(i) scores[[i]])
      data.frame(
        axis_set = axis_data[[ax_id]]$label,
        label    = labels,
        score    = vals,
        stringsAsFactors = FALSE
      )
    })
    df <- bind_rows(rows)

    ggplot(df, aes(x = reorder(label, score), y = score, fill = axis_set)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~axis_set, scales = "free_y", ncol = 3) +
      coord_flip() +
      scale_fill_manual(values = c("#457B9D", "#2A9D8F", "#E63946")) +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(x = NULL, y = "Score (0–10)",
           title = paste0(film$title, " — All Axis Sets")) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#1a1a2e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid       = element_line(colour = "#0f3460"),
        text             = element_text(colour = "#e0e0e0"),
        axis.text        = element_text(colour = "#c0c0d0", size = 8),
        strip.text       = element_text(colour = "#E9C46A", size = 9),
        plot.title       = element_text(colour = "#E9C46A", size = 12, face = "bold")
      )
  }, bg = "#1a1a2e")

  # ── AI scoring tab ───────────────────────────────────────────────────────────
  ai_result <- reactiveVal(NULL)

  observeEvent(input$run_ai_score, {
    req(input$ai_film_title, input$api_key, input$ai_axis_set)
    ai_result(NULL)
    output$ai_status <- renderUI(
      p("⏳ Querying Claude API...", style = "color:#E9C46A")
    )
    result <- tryCatch(
      score_film_api(input$ai_film_title, input$ai_axis_set, input$api_key),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      output$ai_status <- renderUI(
        p(paste("❌ Error:", result$error), style = "color:#E63946")
      )
    } else {
      ai_result(result)
      output$ai_status <- renderUI(
        p(paste0("✅ Scored: ", input$ai_film_title), style = "color:#2A9D8F")
      )
    }
  })

  output$ai_radar <- renderPlot({
    scores <- ai_result()
    req(scores)
    n  <- length(scores)
    df <- as.data.frame(t(data.frame(film = scores)))
    rownames(df) <- input$ai_film_title
    df_plot <- rbind(rep(10, n), rep(0, n), df)
    rownames(df_plot)[1:2] <- c("max", "min")
    colnames(df_plot) <- names(scores)

    cex_lab <- if (n > 14) 0.65 else if (n > 10) 0.75 else 0.85
    par(mar = c(2, 2, 3, 2))
    fmsb::radarchart(
      df_plot,
      axistype    = 1,
      pcol        = "#E63946",
      pfcol       = adjustcolor("#E63946", 0.25),
      plwd        = 2.5,
      cglcol      = "grey80",
      cglty       = 1,
      axislabcol  = "grey50",
      vlcex       = cex_lab,
      caxislabels = seq(0, 10, 2.5),
      calcex      = 0.7,
      seg         = 4
    )
    title(main = paste0(input$ai_film_title, "\n", axis_data[[input$ai_axis_set]]$label),
          col.main = "#E9C46A", cex.main = 1)
  }, bg = "#1a1a2e")

  output$ai_scores_table <- renderPrint({
    scores <- ai_result()
    req(scores)
    df <- data.frame(Axis = names(scores), Score = as.integer(scores))
    df <- df[order(df$Score, decreasing = TRUE), ]
    print(df, row.names = FALSE)
  })
}

shinyApp(ui, server)
