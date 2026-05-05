#!/usr/bin/env Rscript

source("tools/input_sensitivities/helpers.R")

parse_args <- function(args) {
  out <- list(config = "configs/2023R4.R", overwrite = FALSE)
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (key == "--overwrite") {
      out$overwrite <- TRUE
      i <- i + 1L
      next
    }
    if (!startsWith(key, "--") || i == length(args)) stop("Invalid argument near: ", key)
    if (key == "--config") out$config <- args[[i + 1L]]
    i <- i + 2L
  }
  out
}

as_flag <- function(x) {
  tolower(trimws(as.character(x[[1]]))) %in% c("1", "true", "yes", "y", "on")
}

project_root <- getwd()
args <- parse_args(commandArgs(trailingOnly = TRUE))
config_path <- if (grepl("^/", args$config)) args$config else file.path(project_root, args$config)
if (!file.exists(config_path)) stop("Config not found: ", config_path)

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(dirname(config_path))
source(basename(config_path), local = FALSE)
setwd(project_root)

if (!exists("models") || !is.list(models)) stop("Config did not define a models list.")

rows <- lapply(names(models), function(model_name) {
  m <- models[[model_name]]
  enabled <- if (!is.null(m$input_recipe_enabled)) as_flag(m$input_recipe_enabled) else FALSE
  data.frame(
    model = model_name,
    enabled = enabled,
    output_dir = if (!is.null(m$input_recipe_output_dir) && nzchar(as.character(m$input_recipe_output_dir))) as.character(m$input_recipe_output_dir) else as.character(m$base_dir),
    base = if (!is.null(m$input_recipe_base)) as.character(m$input_recipe_base) else "base",
    base_input_dir = first_nonempty(
      if (!is.null(m$input_recipe_base_input_dir)) as.character(m$input_recipe_base_input_dir) else "",
      if (!is.null(m$input_recipe_base_source)) as.character(m$input_recipe_base_source) else ""
    ),
    base_tokens = if (!is.null(m$input_recipe_base_tokens)) as.character(m$input_recipe_base_tokens) else "",
    movement_pairs = if (!is.null(m$input_recipe_movement_pairs)) as.character(m$input_recipe_movement_pairs) else "",
    sel_nodes = if (!is.null(m$input_recipe_sel_nodes)) as.character(m$input_recipe_sel_nodes) else "",
    index_cv_half = if (!is.null(m$input_recipe_index_cv_half)) as.character(m$input_recipe_index_cv_half) else "0",
    stringsAsFactors = FALSE
  )
})
plan <- do.call(rbind, rows)
plan <- plan[plan$enabled & nzchar(plan$output_dir), , drop = FALSE]
plan <- plan[!duplicated(plan$output_dir), , drop = FALSE]

if (nrow(plan) == 0) {
  cat("No input recipes found in config:", config_path, "\n")
  quit(save = "no", status = 0)
}

for (idx in seq_len(nrow(plan))) {
  p <- plan[idx, , drop = FALSE]
  cmd_args <- c(
    input_sensitivity_script("build_input_recipe.R"),
    "--output-dir", p$output_dir,
    "--base", p$base
  )
  if (nzchar(p$base_input_dir)) cmd_args <- c(cmd_args, "--base-input-dir", p$base_input_dir)
  if (nzchar(p$base_tokens)) cmd_args <- c(cmd_args, "--base-tokens", p$base_tokens)
  if (nzchar(p$movement_pairs)) cmd_args <- c(cmd_args, "--movement-pairs", p$movement_pairs)
  if (nzchar(p$sel_nodes)) cmd_args <- c(cmd_args, "--sel-nodes", p$sel_nodes)
  if (as_flag(p$index_cv_half)) cmd_args <- c(cmd_args, "--index-cv-half")
  if (isTRUE(args$overwrite)) cmd_args <- c(cmd_args, "--overwrite")

  cat(sprintf("[%d/%d] Building %s for %s\n", idx, nrow(plan), p$output_dir, p$model))
  status <- system2("Rscript", cmd_args)
  if (!identical(status, 0L)) stop("Input recipe failed for: ", p$output_dir)
}

cat("Built", nrow(plan), "input recipe folder(s).\n")
