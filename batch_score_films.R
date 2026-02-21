#!/usr/bin/env Rscript
# batch_score_films.R
# Run via: Rscript batch_score_films.R
# Or in Claude Code: claude --print "run batch_score_films.R"
#
# Scores a list of films against all 3 Campbell axis sets via Anthropic API.
# Appends to films.json. Skips already-scored films. Rate-limited & resumable.
#
# Setup: export ANTHROPIC_API_KEY=sk-ant-...
# Or:    create .env file with ANTHROPIC_API_KEY=sk-ant-...

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
})

# ── Config ─────────────────────────────────────────────────────────────────────
MODEL        <- "claude-haiku-4-5-20251001"   # cheapest capable model; swap to sonnet for quality
MAX_RETRIES  <- 3
RETRY_WAIT   <- 5   # seconds between retries
BATCH_PAUSE  <- 1   # seconds between API calls (rate limit buffer)
DATA_DIR     <- "data"
FILMS_FILE   <- file.path(DATA_DIR, "films.json")
AXES_FILE    <- file.path(DATA_DIR, "axis_sets.json")
QUEUE_FILE   <- file.path(DATA_DIR, "scoring_queue.txt")  # one "Title (Year)" per line
LOG_FILE     <- "scoring_log.txt"

# ── Load API key ───────────────────────────────────────────────────────────────
get_api_key <- function() {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(key) > 0) return(key)
  env_file <- ".env"
  if (file.exists(env_file)) {
    lines <- readLines(env_file)
    kv <- lines[grepl("^ANTHROPIC_API_KEY=", lines)]
    if (length(kv)) return(sub("^ANTHROPIC_API_KEY=", "", kv[1]))
  }
  stop("No ANTHROPIC_API_KEY found. Set env var or create .env file.")
}

# ── Logging ────────────────────────────────────────────────────────────────────
log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  cat(msg, "\n")
  write(msg, LOG_FILE, append = TRUE)
}

# ── Make slug ID from title ────────────────────────────────────────────────────
make_id <- function(title) {
  gsub("[^a-z0-9]+", "_", tolower(title)) |> gsub("_+$", "", x = _) |> gsub("^_", "", x = _)
}

# ── Score one film × one axis set ──────────────────────────────────────────────
score_film_axisset <- function(film_title, film_year, axis_set_id, axis_def, api_key) {
  axes   <- axis_def$axes
  labels <- sapply(axes, `[[`, "label")
  ids    <- sapply(axes, `[[`, "id")

  axes_json <- paste(
    sprintf('  "%s": <integer 0-10>', ids),
    collapse = ",\n"
  )

  prompt <- sprintf(
'You are an expert in Joseph Campbell\'s monomyth, comparative mythology, and film narrative analysis.

Score the film "%s" (%s) on each narrative dimension below. 
Return ONLY valid JSON — no preamble, no markdown fences, no commentary.

Scoring guide:
- 10 = element is explicit, structurally central, and fully realised
- 7-9 = clearly present, meaningful role
- 4-6 = partial, implied, or modified version present
- 1-3 = trace elements only, or deliberately subverted
- 0 = absent or antithetical

Be discriminating. Most films should have high variance across axes, not uniform scores.

{
%s
}',
    film_title, film_year, axes_json
  )

  for (attempt in seq_len(MAX_RETRIES)) {
    resp <- tryCatch({
      request("https://api.anthropic.com/v1/messages") |>
        req_headers(
          "Content-Type"       = "application/json",
          "anthropic-version"  = "2023-06-01",
          "x-api-key"          = api_key
        ) |>
        req_body_json(list(
          model      = MODEL,
          max_tokens = 600,
          messages   = list(list(role = "user", content = prompt))
        )) |>
        req_error(is_error = function(r) FALSE) |>
        req_perform()
    }, error = function(e) {
      log_msg("  Network error attempt ", attempt, ": ", conditionMessage(e))
      NULL
    })

    if (is.null(resp)) {
      Sys.sleep(RETRY_WAIT * attempt); next
    }

    status <- resp_status(resp)
    if (status == 429) {
      wait <- RETRY_WAIT * attempt * 3
      log_msg("  Rate limited. Waiting ", wait, "s...")
      Sys.sleep(wait); next
    }
    if (status != 200) {
      log_msg("  HTTP ", status, " on attempt ", attempt)
      Sys.sleep(RETRY_WAIT); next
    }

    body <- resp_body_json(resp)
    raw  <- body$content[[1]]$text
    raw  <- trimws(gsub("```json|```", "", raw))

    parsed <- tryCatch(fromJSON(raw, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(parsed)) {
      log_msg("  JSON parse failed attempt ", attempt, ". Raw: ", substr(raw, 1, 100))
      Sys.sleep(RETRY_WAIT); next
    }

    # Build named score vector, clamped 0-10
    scores <- lapply(ids, function(i) {
      v <- parsed[[i]]
      if (is.null(v)) 5L else as.integer(max(0, min(10, v)))
    })
    names(scores) <- ids
    return(scores)
  }

  log_msg("  FAILED after ", MAX_RETRIES, " attempts for ", film_title, " / ", axis_set_id)
  NULL
}

# ── Score one full film (all axis sets) ────────────────────────────────────────
score_film <- function(film_title, film_year, axis_data, api_key) {
  scores <- list()
  for (ax_id in names(axis_data)) {
    log_msg("  Scoring ", ax_id, "...")
    result <- score_film_axisset(film_title, film_year, ax_id, axis_data[[ax_id]], api_key)
    if (is.null(result)) return(NULL)
    scores[[ax_id]] <- result
    Sys.sleep(BATCH_PAUSE)
  }
  list(
    id        = make_id(film_title),
    title     = film_title,
    year      = film_year,
    director  = "",           # fill in manually or extend prompt
    genre     = list(),
    notes     = "AI-scored via batch_score_films.R",
    scores    = scores
  )
}

# ── Parse queue file ───────────────────────────────────────────────────────────
# Format: one film per line, "Title (Year)" or just "Title"
parse_queue <- function(queue_file) {
  lines <- readLines(queue_file)
  lines <- trimws(lines[nchar(trimws(lines)) > 0 & !grepl("^#", lines)])
  lapply(lines, function(l) {
    m <- regmatches(l, regexpr("\\((\\d{4})\\)\\s*$", l))
    if (length(m) && nchar(m) > 0) {
      year  <- as.integer(gsub("[^0-9]", "", m))
      title <- trimws(sub("\\s*\\(\\d{4}\\)\\s*$", "", l))
    } else {
      title <- l; year <- NA_integer_
    }
    list(title = title, year = year)
  })
}

# ── Main ───────────────────────────────────────────────────────────────────────
main <- function() {
  api_key   <- get_api_key()
  axis_data <- fromJSON(AXES_FILE, simplifyVector = FALSE)

  # Load existing films (to skip already scored)
  existing  <- fromJSON(FILMS_FILE, simplifyVector = FALSE)$films
  done_ids  <- sapply(existing, `[[`, "id")
  log_msg("Loaded ", length(existing), " existing films from ", FILMS_FILE)

  # Load queue
  if (!file.exists(QUEUE_FILE)) {
    stop("Queue file not found: ", QUEUE_FILE,
         "\nCreate it with one film per line, e.g.:\n  Blade Runner (1982)\n  Parasite (2019)")
  }
  queue <- parse_queue(QUEUE_FILE)
  log_msg("Queue has ", length(queue), " films")

  new_films <- list()
  skipped   <- 0
  failed    <- character(0)

  for (i in seq_along(queue)) {
    film_title <- queue[[i]]$title
    film_year  <- queue[[i]]$year
    film_id    <- make_id(film_title)

    year_str <- if (is.na(film_year)) "?" else film_year

    if (film_id %in% done_ids) {
      log_msg("[", i, "/", length(queue), "] SKIP (exists): ", film_title)
      skipped <- skipped + 1
      next
    }

    log_msg("[", i, "/", length(queue), "] Scoring: ", film_title, " (", year_str, ")")
    result <- score_film(film_title, film_year, axis_data, api_key)

    if (is.null(result)) {
      log_msg("  -> FAILED")
      failed <- c(failed, film_title)
    } else {
      new_films <- c(new_films, list(result))
      done_ids  <- c(done_ids, film_id)
      log_msg("  -> OK")
    }

    Sys.sleep(BATCH_PAUSE)
  }

  # Merge and write
  all_films <- c(existing, new_films)
  out <- list(films = all_films)
  write(toJSON(out, pretty = TRUE, auto_unbox = TRUE), FILMS_FILE)

  log_msg("=== Done ===")
  log_msg("Scored: ", length(new_films), " | Skipped: ", skipped, " | Failed: ", length(failed))
  if (length(failed)) {
    log_msg("Failed films: ", paste(failed, collapse = "; "))
  }
  log_msg("Output: ", FILMS_FILE)
}

main()
