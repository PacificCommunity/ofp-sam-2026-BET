#!/usr/bin/env Rscript

parse_numeric_tokens <- function(x) {
  txt <- paste(as.character(x), collapse = " ")
  if (is.null(x) || !nzchar(trimws(txt))) return(numeric(0))
  m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", txt, perl = TRUE)
  toks <- regmatches(txt, m)[[1]]
  vals <- suppressWarnings(as.numeric(toks))
  vals[is.finite(vals)]
}

read_indexed_chain_scalars <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("chain_count", Sys.getenv("CHAIN_COUNT", ""))))
  if (!is.finite(n) || n < 1) return(numeric(0))
  out <- numeric(0)
  for (i in seq_len(n)) {
    key_lo <- paste0("chain_scalar_", i)
    key_up <- paste0("CHAIN_SCALAR_", i)
    v <- suppressWarnings(as.numeric(Sys.getenv(key_lo, Sys.getenv(key_up, ""))))
    if (is.finite(v)) out <- c(out, v)
  }
  out
}

resolve_chain_from_scalars <- function(all_scalars, chain_name, chain_anchor) {
  all_scalars <- sort(unique(all_scalars[is.finite(all_scalars)]))
  if (length(all_scalars) == 0) return(numeric(0))
  anch <- suppressWarnings(as.numeric(chain_anchor))
  if (!is.finite(anch)) anch <- 100
  anchor_eff <- all_scalars[which.min(abs(all_scalars - anch))]
  lower <- sort(all_scalars[all_scalars < anchor_eff], decreasing = TRUE)
  upper <- sort(all_scalars[all_scalars > anchor_eff], decreasing = FALSE)
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
chain_scalars <- parse_numeric_tokens(Sys.getenv("scalars", Sys.getenv("chain_scalars", "")))
if (length(chain_scalars) == 0) {
  chain_scalars <- read_indexed_chain_scalars()
}
chain_first_init_from <- suppressWarnings(as.numeric(Sys.getenv("chain_first_init_from", "")))
chain_anchor <- Sys.getenv("chain_anchor", "")
init_par_override <- Sys.getenv("init_par_override", "")
prof_fix_indepvar <- Sys.getenv("prof_fix_indepvar", "")
prof_fix_values <- Sys.getenv("prof_fix_values", "")
prof_extra_switch <- Sys.getenv("prof_extra_switch", "")

if (length(chain_scalars) == 0) {
  all_scalars <- parse_numeric_tokens(Sys.getenv("scalars", ""))
  chain_scalars <- resolve_chain_from_scalars(all_scalars, chain_name, chain_anchor)
  if (length(chain_scalars) == 0) {
    stop("No chain scalars found: missing indexed chain_scalar_* and failed to rebuild from scalars/chain_anchor.")
  }
}

cat("=== Profile Chain Run ===\n")
cat("chain_name:", chain_name, "\n")
cat("scalars_raw_env:", Sys.getenv("scalars", "<none>"), "\n")
cat("chain_scalars_raw_env:", Sys.getenv("chain_scalars", "<none>"), "\n")
cat("chain_scalars:", paste(chain_scalars, collapse = " "), "\n")
cat("chain_first_init_from:", ifelse(is.finite(chain_first_init_from), as.character(chain_first_init_from), "<none>"), "\n")
cat("chain_anchor:", ifelse(nzchar(chain_anchor), chain_anchor, "<none>"), "\n")
cat("init_par_override:", ifelse(nzchar(init_par_override), init_par_override, "<none>"), "\n")
cat("prof_fix_indepvar:", ifelse(nzchar(prof_fix_indepvar), prof_fix_indepvar, "<none>"), "\n")
cat("prof_fix_values:", ifelse(nzchar(prof_fix_values), prof_fix_values, "<none>"), "\n")
cat("prof_extra_switch:", ifelse(nzchar(prof_extra_switch), prof_extra_switch, "<none>"), "\n")

project_root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) getwd())
run_prof_script <- file.path(project_root, "runners", "run_prof.R")
if (!file.exists(run_prof_script)) {
  stop("Cannot find runners/run_prof.R under project root: ", project_root)
}

prev_scalar <- NA_real_
for (i in seq_along(chain_scalars)) {
  sc <- chain_scalars[[i]]
  donor <- if (i == 1L) chain_first_init_from else prev_scalar

  env_kv <- c(
    paste0("scalar=", format(sc, scientific = FALSE, trim = TRUE)),
    "skip_condor_archive_cleanup=1"
  )
  if (nzchar(prof_fix_indepvar)) {
    env_kv <- c(env_kv, paste0("prof_fix_indepvar=", prof_fix_indepvar))
  }
  if (nzchar(prof_fix_values)) {
    env_kv <- c(env_kv, paste0("prof_fix_values=", prof_fix_values))
  }
  if (nzchar(prof_extra_switch)) {
    env_kv <- c(env_kv, paste0("prof_extra_switch=", prof_extra_switch))
  }
  if (nzchar(init_par_override) && i == 1L) {
    env_kv <- c(env_kv, paste0("init_par_override=", init_par_override))
  }
  if (is.finite(donor)) {
    env_kv <- c(env_kv, paste0("init_from_scalar=", format(donor, scientific = FALSE, trim = TRUE)))
  }

  cat("\n--- chain step", i, "/", length(chain_scalars), " scalar=", sc,
      " donor=", ifelse(is.finite(donor), as.character(donor), "<none>"), " ---\n", sep = "")

  status <- system2(
    "Rscript",
    args = c("runners/run_prof.R"),
    env = env_kv,
    stdout = "",
    stderr = ""
  )

  if (!is.numeric(status) || length(status) != 1 || is.na(status) || as.integer(status) != 0L) {
    stop("run_prof.R failed in chain ", chain_name, " at scalar ", sc, " (status=", status, ").")
  }

  prev_scalar <- sc
}

cleanup_script <- file.path(project_root, "tools", "condor_archive_cleanup.R")
if (file.exists(cleanup_script)) {
  source(cleanup_script)
  cb_condor_keep_only_model_cleanup()
}

cat("\n✅ Profile chain completed: ", chain_name, "\n", sep = "")
