#!/usr/bin/env Rscript

parse_numeric_tokens <- function(x) {
  txt <- paste(as.character(x), collapse = " ")
  if (is.null(x) || !nzchar(trimws(txt))) return(numeric(0))
  m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", txt, perl = TRUE)
  toks <- regmatches(txt, m)[[1]]
  vals <- suppressWarnings(as.numeric(toks))
  vals[is.finite(vals)]
}

read_indexed_chain_scalers <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("chain_count", Sys.getenv("CHAIN_COUNT", ""))))
  if (!is.finite(n) || n < 1) return(numeric(0))
  out <- numeric(0)
  for (i in seq_len(n)) {
    key_lo <- paste0("chain_scaler_", i)
    key_up <- paste0("CHAIN_SCALER_", i)
    v <- suppressWarnings(as.numeric(Sys.getenv(key_lo, Sys.getenv(key_up, ""))))
    if (is.finite(v)) out <- c(out, v)
  }
  out
}

resolve_chain_from_scalers <- function(all_scalers, chain_name, chain_anchor) {
  all_scalers <- sort(unique(all_scalers[is.finite(all_scalers)]))
  if (length(all_scalers) == 0) return(numeric(0))
  anch <- suppressWarnings(as.numeric(chain_anchor))
  if (!is.finite(anch)) anch <- 100
  anchor_eff <- all_scalers[which.min(abs(all_scalers - anch))]
  lower <- sort(all_scalers[all_scalers < anchor_eff], decreasing = TRUE)
  upper <- sort(all_scalers[all_scalers > anchor_eff], decreasing = FALSE)
  nm <- tolower(trimws(as.character(chain_name)))
  if (identical(nm, "down")) {
    return(c(anchor_eff, lower))
  }
  if (identical(nm, "up")) {
    return(upper)
  }
  c(anchor_eff, lower, upper)
}

chain_name <- Sys.getenv("chain_name", "chain")
chain_scalers <- parse_numeric_tokens(Sys.getenv("chain_scalers", ""))
if (length(chain_scalers) == 0) {
  chain_scalers <- read_indexed_chain_scalers()
}
chain_first_init_from <- suppressWarnings(as.numeric(Sys.getenv("chain_first_init_from", "")))
chain_anchor <- Sys.getenv("chain_anchor", "")

if (length(chain_scalers) == 0) {
  all_scalers <- parse_numeric_tokens(Sys.getenv("scalers", ""))
  chain_scalers <- resolve_chain_from_scalers(all_scalers, chain_name, chain_anchor)
  if (length(chain_scalers) == 0) {
    stop("No chain scalers found: missing indexed chain_scaler_* and failed to rebuild from scalers/chain_anchor.")
  }
}

cat("=== Profile Chain Run ===\n")
cat("chain_name:", chain_name, "\n")
cat("chain_scalers_raw_env:", Sys.getenv("chain_scalers", "<none>"), "\n")
cat("chain_scalers:", paste(chain_scalers, collapse = " "), "\n")
cat("chain_first_init_from:", ifelse(is.finite(chain_first_init_from), as.character(chain_first_init_from), "<none>"), "\n")
cat("chain_anchor:", ifelse(nzchar(chain_anchor), chain_anchor, "<none>"), "\n")

project_root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) getwd())
run_prof_script <- file.path(project_root, "runners", "run_prof.R")
if (!file.exists(run_prof_script)) {
  stop("Cannot find runners/run_prof.R under project root: ", project_root)
}

prev_scaler <- NA_real_
for (i in seq_along(chain_scalers)) {
  sc <- chain_scalers[[i]]
  donor <- if (i == 1L) chain_first_init_from else prev_scaler

  env_kv <- c(
    paste0("scaler=", format(sc, scientific = FALSE, trim = TRUE))
  )
  if (is.finite(donor)) {
    env_kv <- c(env_kv, paste0("init_from_scaler=", format(donor, scientific = FALSE, trim = TRUE)))
  }

  cat("\n--- chain step", i, "/", length(chain_scalers), " scaler=", sc,
      " donor=", ifelse(is.finite(donor), as.character(donor), "<none>"), " ---\n", sep = "")

  status <- system2(
    "Rscript",
    args = c("runners/run_prof.R"),
    env = env_kv,
    stdout = "",
    stderr = ""
  )

  if (!is.numeric(status) || length(status) != 1 || is.na(status) || as.integer(status) != 0L) {
    stop("run_prof.R failed in chain ", chain_name, " at scaler ", sc, " (status=", status, ").")
  }

  prev_scaler <- sc
}

cat("\n✅ Profile chain completed: ", chain_name, "\n", sep = "")
