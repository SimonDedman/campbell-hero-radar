#!/usr/bin/env Rscript
# batch_score_claudecode.R
#
# Scores films using the `claude` CLI (Claude Code / Max subscription).
# NO API KEY NEEDED. Uses your Claude Max subscription via claude CLI.
#
# Prerequisites:
#   npm install -g @anthropic-ai/claude-code   # installs `claude` CLI
#   claude login                                # authenticate once
#
# Usage:
#   Rscript batch_score_claudecode.R
#   Rscript batch_score_claudecode.R --model claude-opus-4-6  # use Opus
#   Rscript batch_score_claudecode.R --dry-run                # test without scoring
#
# Resumable: already-scored films are skipped automatically.

suppressPackageStartupMessages(library(jsonlite))

# ── CLI args ───────────────────────────────────────────────────────────────────
args        <- commandArgs(trailingOnly = TRUE)
MODEL       <- if ("--model" %in% args) args[which(args == "--model") + 1] else "claude-sonnet-4-6"
DRY_RUN     <- "--dry-run" %in% args
QUEUE_ONLY  <- if ("--only" %in% args) as.integer(args[which(args == "--only") + 1]) else Inf

DATA_DIR    <- "data"
FILMS_FILE  <- file.path(DATA_DIR, "films.json")
AXES_FILE   <- file.path(DATA_DIR, "axis_sets.json")
QUEUE_FILE  <- file.path(DATA_DIR, "scoring_queue.txt")
LOG_FILE    <- "scoring_log.txt"

BATCH_PAUSE <- 2   # seconds between claude CLI calls

# ── Logging ────────────────────────────────────────────────────────────────────
log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  cat(msg, "\n")
  write(msg, LOG_FILE, append = TRUE)
}

# ── Check claude CLI is available ─────────────────────────────────────────────
check_claude_cli <- function() {
  result <- suppressWarnings(system2("claude", "--version", stdout = TRUE, stderr = TRUE))
  if (length(result) == 0 || inherits(result, "error")) {
    stop(
      "claude CLI not found.\n",
      "Install with: npm install -g @anthropic-ai/claude-code\n",
      "Then auth:    claude login"
    )
  }
  log_msg("claude CLI found: ", result[1])
}

# ── Slug ID from title ─────────────────────────────────────────────────────────
make_id <- function(title) {
  gsub("[^a-z0-9]+", "_", tolower(title)) |>
    gsub("_+$", "", x = _) |>
    gsub("^_+", "", x = _)
}

# ── Score one film × all axis sets via claude CLI ─────────────────────────────
score_film_claudecode <- function(film_title, film_year, axis_data, model) {
  year_str <- if (is.na(film_year)) "unknown year" else as.character(film_year)

  # Build full prompt requesting all 3 axis sets in one call (saves calls)
  axis_blocks <- lapply(names(axis_data), function(ax_id) {
    axes   <- axis_data[[ax_id]]$axes
    ids    <- sapply(axes, `[[`, "id")
    labels <- sapply(axes, `[[`, "label")
    fields <- paste(sprintf('      "%s": <integer 0-10>', ids), collapse = ",\n")
    sprintf('  "%s": {\n%s\n  }', ax_id, fields)
  })

  prompt <- sprintf(
'You are an expert in Joseph Campbell\'s monomyth and comparative film analysis.

Score the film "%s" (%s) on all three narrative frameworks below.

SCORING GUIDE:
10 = explicitly and structurally central
7-9 = clearly present, meaningful role  
4-6 = partial, implied, or modified
1-3 = trace elements or subverted
0 = absent or antithetical

Be discriminating — films should show HIGH VARIANCE across axes, not uniform scores.
A film that inverts the monomyth (e.g. No Country for Old Men) should score low on resolution axes.

Return ONLY valid JSON matching this exact structure, no markdown, no commentary:
{
%s
}',
    film_title, year_str,
    paste(axis_blocks, collapse = ",\n")
  )

  # Write prompt to temp file (avoids shell escaping nightmares)
  tmp_prompt <- tempfile(fileext = ".txt")
  tmp_out    <- tempfile(fileext = ".txt")
  on.exit({ unlink(tmp_prompt); unlink(tmp_out) })
  writeLines(prompt, tmp_prompt)

  # Call claude CLI: --print for non-interactive, pipe stdin from file
  cmd <- sprintf(
    'claude --print --model %s < "%s" > "%s" 2>&1',
    model, tmp_prompt, tmp_out
  )

  exit_code <- system(cmd)

  if (exit_code != 0) {
    output <- if (file.exists(tmp_out)) readLines(tmp_out) else "no output"
    log_msg("  claude CLI error (exit ", exit_code, "): ", paste(output[1:min(3,length(output))], collapse=" | "))
    return(NULL)
  }

  raw    <- paste(readLines(tmp_out), collapse = "\n")
  raw    <- trimws(gsub("```json|```", "", raw))

  # Extract just the JSON block if there's surrounding text
  json_match <- regmatches(raw, regexpr("\\{[\\s\\S]*\\}", raw, perl = TRUE))
  if (length(json_match) == 0 || nchar(json_match) == 0) {
    log_msg("  No JSON found in response. Raw: ", substr(raw, 1, 200))
    return(NULL)
  }

  parsed <- tryCatch(
    fromJSON(json_match, simplifyVector = TRUE),
    error = function(e) { log_msg("  JSON parse error: ", conditionMessage(e)); NULL }
  )
  if (is.null(parsed)) return(NULL)

  # Validate and clamp all scores
  scores <- list()
  for (ax_id in names(axis_data)) {
    if (is.null(parsed[[ax_id]])) {
      log_msg("  Missing axis set '", ax_id, "' in response")
      return(NULL)
    }
    axes <- axis_data[[ax_id]]$axes
    ids  <- sapply(axes, `[[`, "id")
    ax_scores <- lapply(ids, function(i) {
      v <- parsed[[ax_id]][[i]]
      if (is.null(v)) 5L else as.integer(max(0L, min(10L, as.integer(v))))
    })
    names(ax_scores) <- ids
    scores[[ax_id]] <- ax_scores
  }

  scores
}

# ── Parse queue file ───────────────────────────────────────────────────────────
parse_queue <- function(queue_file) {
  lines <- readLines(queue_file)
  lines <- trimws(lines[nchar(trimws(lines)) > 0 & !grepl("^#", lines)])
  # Deduplicate
  lines <- unique(lines)
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
  if (!DRY_RUN) check_claude_cli()

  axis_data <- fromJSON(AXES_FILE, simplifyVector = FALSE)
  existing  <- fromJSON(FILMS_FILE, simplifyVector = FALSE)$films
  done_ids  <- sapply(existing, `[[`, "id")

  log_msg("Loaded ", length(existing), " existing films")
  log_msg("Model: ", MODEL, if (DRY_RUN) " [DRY RUN]" else "")

  queue <- parse_queue(QUEUE_FILE)
  if (is.finite(QUEUE_ONLY)) queue <- queue[seq_len(min(QUEUE_ONLY, length(queue)))]
  log_msg("Queue: ", length(queue), " films (", sum(sapply(queue, function(f) make_id(f$title)) %in% done_ids), " already done)")

  new_films <- list()
  skipped   <- 0
  failed    <- character(0)

  for (i in seq_along(queue)) {
    film_title <- queue[[i]]$title
    film_year  <- queue[[i]]$year
    film_id    <- make_id(film_title)
    year_str   <- if (is.na(film_year)) "?" else film_year

    if (film_id %in% done_ids) {
      log_msg("[", i, "/", length(queue), "] SKIP: ", film_title)
      skipped <- skipped + 1
      next
    }

    log_msg("[", i, "/", length(queue), "] Scoring: ", film_title, " (", year_str, ")")

    if (DRY_RUN) {
      log_msg("  [DRY RUN] would score here")
      next
    }

    scores <- tryCatch(
      score_film_claudecode(film_title, film_year, axis_data, MODEL),
      error = function(e) { log_msg("  ERROR: ", conditionMessage(e)); NULL }
    )

    if (is.null(scores)) {
      log_msg("  -> FAILED")
      failed <- c(failed, film_title)
    } else {
      entry <- list(
        id       = film_id,
        title    = film_title,
        year     = if (is.na(film_year)) NULL else film_year,
        director = "",
        genre    = list(),
        notes    = paste0("AI-scored via claude CLI (", MODEL, ")"),
        scores   = scores
      )
      new_films <- c(new_films, list(entry))
      done_ids  <- c(done_ids, film_id)
      log_msg("  -> OK")
    }

    Sys.sleep(BATCH_PAUSE)
  }

  if (!DRY_RUN && length(new_films) > 0) {
    all_films <- c(existing, new_films)
    write(toJSON(list(films = all_films), pretty = TRUE, auto_unbox = TRUE), FILMS_FILE)
    log_msg("Wrote ", length(all_films), " total films to ", FILMS_FILE)
  }

  log_msg("=== Complete: +", length(new_films), " scored | ",
          skipped, " skipped | ", length(failed), " failed ===")
  if (length(failed)) log_msg("Failed: ", paste(failed, collapse = "; "))
}

main()
