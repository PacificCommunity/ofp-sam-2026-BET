#!/usr/bin/env Rscript

# Build donor-scalar -> par_lines map from fetched pass-1 profile outputs.
# Default output: <base_dir>/prof_init_map.rds

source("tools/model_defaults.R")

parse_scalar_dirs <- function(prof_dir) {
  dirs <- list.dirs(prof_dir, recursive = FALSE, full.names = TRUE)
  if (length(dirs) == 0) return(data.frame())
  scalar_names <- basename(dirs)
  scalars <- suppressWarnings(as.integer(sub("^scalar_", "", scalar_names)))
  keep <- is.finite(scalars)
  data.frame(
    scalar = as.integer(scalars[keep]),
    scalar_dir = dirs[keep],
    stringsAsFactors = FALSE
  )
}

extract_par_lines <- function(scalar_dir) {
  payload_path <- file.path(scalar_dir, "profile_payload.rds")
  payload_obj <- NULL
  if (file.exists(payload_path)) {
    payload_obj <- tryCatch(readRDS(payload_path), error = function(e) NULL)
    if (!is.null(payload_obj$par_lines) && length(payload_obj$par_lines) > 0) {
      return(as.character(payload_obj$par_lines))
    }
  }

  info_path <- file.path(scalar_dir, "info.rds")
  if (file.exists(info_path)) {
    info_obj <- tryCatch(readRDS(info_path), error = function(e) NULL)
    if (!is.null(info_obj$final_par_lines) && length(info_obj$final_par_lines) > 0) {
      return(as.character(info_obj$final_par_lines))
    }
  }

  if (!is.null(payload_obj$output_par) && nzchar(as.character(payload_obj$output_par))) {
    candidate <- file.path(scalar_dir, as.character(payload_obj$output_par))
    if (file.exists(candidate)) {
      return(tryCatch(readLines(candidate), error = function(e) character(0)))
    }
  }

  par_hits <- list.files(scalar_dir, pattern = "\\.par$", full.names = TRUE)
  if (length(par_hits) > 0) {
    info <- file.info(par_hits)
    latest <- rownames(info)[which.max(info$mtime)]
    if (is.character(latest) && length(latest) == 1 && nzchar(latest) && file.exists(latest)) {
      return(tryCatch(readLines(latest), error = function(e) character(0)))
    }
  }

  character(0)
}

resolve_model_from_config <- function(config_file, model_name) {
  if (!file.exists(config_file)) {
    stop("Config file not found: ", config_file)
  }
  cfg_env <- new.env(parent = globalenv())
  source(config_file, local = cfg_env, chdir = TRUE)
  if (!exists("models", envir = cfg_env, inherits = FALSE) ||
      !is.list(cfg_env$models) ||
      length(cfg_env$models) == 0) {
    stop("No models found in config: ", config_file)
  }
  models <- cfg_env$models
  if (!nzchar(model_name)) {
    model_name <- names(models)[1]
  }
  if (!(model_name %in% names(models))) {
    stop("Model not found in config: ", model_name)
  }
  cfg <- models[[model_name]]
  list(
    model_name = model_name,
    model_dir = as.character(cfg$model_dir),
    base_dir = as.character(cfg$base_dir)
  )
}

config_file <- Sys.getenv("config_file", "configs/2023diag.R")
model_name <- Sys.getenv("model_name", "")
model_dir <- Sys.getenv("model_dir", "")
base_dir <- Sys.getenv("base_dir", "")
out_path <- Sys.getenv("prof_init_map_out", "")

# If model_dir is provided, config is optional.
# If base_dir is missing in this mode, write output under model_dir.
if (nzchar(model_dir)) {
  if (!dir.exists(model_dir)) {
    stop("model_dir does not exist: ", model_dir)
  }
  if (!nzchar(model_name)) {
    model_name <- basename(normalizePath(model_dir, winslash = "/", mustWork = FALSE))
  }
  if (!nzchar(base_dir)) {
    base_dir <- model_dir
  }
} else {
  resolved <- resolve_model_from_config(config_file = config_file, model_name = model_name)
  model_dir <- resolved$model_dir
  if (!nzchar(base_dir)) base_dir <- resolved$base_dir
  if (!nzchar(model_name)) model_name <- resolved$model_name
}

if (!nzchar(model_name)) {
  model_name <- basename(normalizePath(model_dir, winslash = "/", mustWork = FALSE))
}

prof_dir <- file.path(model_dir, "prof")
if (!dir.exists(prof_dir)) {
  stop("Profile directory not found: ", prof_dir)
}
if (!nzchar(out_path)) {
  out_path <- file.path(base_dir, "prof_init_map.rds")
}

tbl <- parse_scalar_dirs(prof_dir)
if (nrow(tbl) == 0) {
  stop("No scalar_* directories found under: ", prof_dir)
}
tbl <- tbl[order(tbl$scalar), , drop = FALSE]

par_by_scalar <- list()
rows <- list()

for (i in seq_len(nrow(tbl))) {
  sc <- tbl$scalar[[i]]
  sc_dir <- tbl$scalar_dir[[i]]
  lines <- extract_par_lines(sc_dir)
  n_lines <- length(lines)
  if (n_lines > 0) {
    par_by_scalar[[as.character(sc)]] <- lines
  }
  rows[[length(rows) + 1]] <- data.frame(
    scalar = sc,
    has_par_lines = n_lines > 0,
    n_lines = n_lines,
    stringsAsFactors = FALSE
  )
}

summary_tbl <- do.call(rbind, rows)
summary_tbl <- summary_tbl[order(summary_tbl$scalar), , drop = FALSE]

if (length(par_by_scalar) == 0) {
  stop(
    "No par_lines found under ", prof_dir, ". ",
    "Need at least some donor scalars rerun with current runners/run_prof.R ",
    "(which stores profile_payload.rds$par_lines / info.rds$final_par_lines)."
  )
}

out <- list(
  version = "prof_init_map_v1",
  created_at = as.character(Sys.time()),
  model_name = model_name,
  source_model_dir = model_dir,
  source_prof_dir = prof_dir,
  par_by_scalar = par_by_scalar,
  entries = par_by_scalar,
  summary = summary_tbl
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(out, file = out_path, compress = "xz")

cat("Built prof init map\n")
cat("- model_name:", model_name, "\n")
cat("- model_dir:", model_dir, "\n")
cat("- base_dir:", base_dir, "\n")
cat("- prof_dir:", prof_dir, "\n")
cat("- scalars with par_lines:", length(par_by_scalar), "/", nrow(summary_tbl), "\n")
cat("- output:", out_path, "\n")
