#!/usr/bin/env Rscript

parse_numeric_tokens <- function(x) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) return(numeric(0))
  vals <- suppressWarnings(as.numeric(strsplit(as.character(x), "\\s+")[[1]]))
  vals[is.finite(vals)]
}

chain_name <- Sys.getenv("chain_name", "chain")
chain_scalers <- parse_numeric_tokens(Sys.getenv("chain_scalers", ""))
chain_first_init_from <- suppressWarnings(as.numeric(Sys.getenv("chain_first_init_from", "")))

if (length(chain_scalers) == 0) {
  stop("No chain_scalers provided for prof chain run.")
}

cat("=== Profile Chain Run ===\n")
cat("chain_name:", chain_name, "\n")
cat("chain_scalers:", paste(chain_scalers, collapse = " "), "\n")
cat("chain_first_init_from:", ifelse(is.finite(chain_first_init_from), as.character(chain_first_init_from), "<none>"), "\n")

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
