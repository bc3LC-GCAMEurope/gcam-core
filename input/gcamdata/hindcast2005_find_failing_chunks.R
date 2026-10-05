# hindcast2005_find_failing_chunks.R
#
# Runs every gcamdata chunk once, like driver(), but does NOT stop at the first error:
# each chunk is wrapped in tryCatch, its error is recorded and the run continues with
# every chunk whose inputs are still available. At the end it writes a report that
# separates ROOT failures (the chunk itself errored: fix these) from BLOCKED chunks
# (never ran because an upstream chunk failed: they usually disappear once the root
# failures are fixed).
#
# Nothing is written to outputs/ or xml/.
#
# Usage, from input/gcamdata (R >= 4, devtools installed):
#   Rscript hindcast2005_find_failing_chunks.R
# or inside R / RStudio with the working directory at input/gcamdata:
#   source("hindcast2005_find_failing_chunks.R")
#
# Output: hindcast2005_chunk_report.csv (one row per chunk) and a short console summary.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)   # loads gcamdata with the constants of this branch
  library(dplyr)
})

message("Base years: ", paste(MODEL_BASE_YEARS, collapse = ", "),
        " | first future periods: ", paste(head(MODEL_FUTURE_YEARS, 4), collapse = ", "))

tolerant_driver <- function(report_file = "hindcast2005_chunk_report.csv") {

  chunklist    <- find_chunks()
  chunkinputs  <- chunk_inputs(chunklist$name)
  chunkoutputs <- chunk_outputs(chunklist$name)

  unfound_inputs <- filter(chunkinputs, !input %in% chunkoutputs$output)
  cic <- table(chunkinputs$input)           # remaining uses of each dataset (for memory pruning)
  cic <- setNames(as.integer(cic), names(cic))

  all_data      <- empty_data()
  to_run        <- c(unique(unfound_inputs$input), chunklist$name)
  status        <- setNames(rep("pending", length(chunklist$name)), chunklist$name)
  errors        <- setNames(rep(NA_character_, length(chunklist$name)), chunklist$name)
  seconds       <- setNames(rep(NA_real_, length(chunklist$name)), chunklist$name)
  failed_inputs <- character(0)             # file inputs that could not be read

  repeat {
    progressed <- FALSE
    for (chunk in to_run) {
      inputs   <- filter(chunkinputs, name == chunk)
      required <- inputs$input[!inputs$optional]

      # --- raw file inputs: load on demand -------------------------------------------
      if (chunk %in% unfound_inputs$input) {
        uf <- unfound_inputs[unfound_inputs$input == chunk, ][1, ]
        res <- tryCatch(load_csv_files(uf$input, uf$optional, quiet = TRUE),
                        error = function(e) e)
        if (inherits(res, "error")) {
          failed_inputs <- c(failed_inputs, chunk)
          message("  FILE ERROR ", chunk, ": ", conditionMessage(res))
        } else {
          all_data <- add_data(res, all_data)
        }
        to_run <- setdiff(to_run, chunk); progressed <- TRUE
        next
      }

      if (!all(required %in% names(all_data))) next   # not runnable (yet, or ever)

      t0  <- Sys.time()
      res <- tryCatch(run_chunk(chunk, all_data[inputs$input]), error = function(e) e)
      seconds[chunk] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

      if (!inherits(res, "error")) {
        po  <- subset(chunkoutputs, name == chunk)
        chk <- tryCatch(check_chunk_outputs(chunk, res, inputs$input,
                                            promised_outputs = po$output,
                                            outputs_xml = po$to_xml),
                        error = function(e) e)
        if (inherits(chk, "error")) res <- chk
      }

      if (inherits(res, "error")) {
        status[chunk] <- "FAILED"
        errors[chunk] <- gsub("[\r\n]+", " ", conditionMessage(res))
        message("  FAILED ", chunk, ": ", substr(errors[chunk], 1, 200))
      } else {
        status[chunk] <- "ok"
        all_data <- add_data(res, all_data)
        # drop outputs nobody consumes (XML objects etc.) to keep memory low
        po <- subset(chunkoutputs, name == chunk)$output
        all_data <- remove_data(po[!po %in% names(cic)], all_data)
      }

      # prune inputs that are not needed any more (same logic as driver())
      cic[inputs$input] <- cic[inputs$input] - 1
      done <- names(cic[inputs$input])[cic[inputs$input] == 0]
      all_data <- remove_data(intersect(done, names(all_data)), all_data)

      to_run <- setdiff(to_run, chunk); progressed <- TRUE
    }
    if (!progressed || !length(to_run)) break
  }

  # --- report --------------------------------------------------------------------------
  producer_of <- setNames(chunkoutputs$name, chunkoutputs$output)
  failed      <- names(status)[status == "FAILED"]

  # upstream root causes of every chunk that never ran
  root_causes <- function(chunk, seen = character(0)) {
    if (chunk %in% seen) return(character(0))
    ins <- filter(chunkinputs, name == chunk, !optional)$input
    out <- character(0)
    for (i in ins) {
      if (i %in% failed_inputs) { out <- c(out, paste0("FILE:", i)); next }
      p <- producer_of[i]
      if (is.na(p)) next
      if (p %in% failed) out <- c(out, p)
      else if (status[p] != "ok") out <- c(out, root_causes(p, c(seen, chunk)))
    }
    unique(out)
  }

  blocked <- names(status)[status == "pending"]
  status[blocked] <- "blocked"
  blocked_by <- setNames(rep(NA_character_, length(status)), names(status))
  for (b in blocked) blocked_by[b] <- paste(root_causes(b), collapse = "; ")

  report <- tibble(chunk = names(status), status = unname(status),
                   error = unname(errors[names(status)]),
                   blocked_by = unname(blocked_by),
                   seconds = round(unname(seconds[names(status)]), 1)) %>%
    mutate(n_blocked = vapply(chunk, function(ch) sum(grepl(ch, blocked_by, fixed = TRUE)), integer(1))) %>%
    arrange(factor(status, levels = c("FAILED", "blocked", "ok")), desc(n_blocked), chunk)

  readr::write_csv(report, report_file)

  cat("\n=== HINDCAST2005 chunk report ===\n")
  cat(sum(report$status == "ok"), "ok |", sum(report$status == "FAILED"), "FAILED (root) |",
      sum(report$status == "blocked"), "blocked |", length(failed_inputs), "unreadable input files\n")
  if (length(failed)) {
    cat("\nRoot failures (fix these first; n_blocked = downstream chunks they block):\n")
    print(as.data.frame(select(filter(report, status == "FAILED"), chunk, n_blocked, error)),
          right = FALSE, row.names = FALSE)
  }
  cat("\nFull report written to", normalizePath(report_file), "\n")
  invisible(report)
}

report <- tolerant_driver()
