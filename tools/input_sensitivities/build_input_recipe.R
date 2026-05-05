#!/usr/bin/env Rscript

source("tools/input_sensitivities/helpers.R")
source("tools/input_sensitivities/sensitivity_catalog.R")
source("tools/input_sensitivities/recipe_registry.R")
source("tools/input_change_metadata.R")

parse_args <- function(args) {
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (key %in% c("--overwrite", "--index-cv-half", "--with-11par")) {
      out[[sub("^--", "", key)]] <- "1"
      i <- i + 1L
      next
    }
    if (!startsWith(key, "--") || i == length(args)) {
      stop("Invalid argument near: ", key)
    }
    out[[sub("^--", "", key)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  out
}

project_root <- getwd()
args <- parse_args(commandArgs(trailingOnly = TRUE))

sensitivity_output_dir <- first_nonempty(args[["output-dir"]], env_or_default("input_recipe_output_dir"), env_or_default("base_dir"))
recipe_base <- first_nonempty(args[["base"]], env_or_default("input_recipe_base"), "base")
base_input_dir <- first_nonempty(
  args[["base-input-dir"]],
  env_or_default("input_recipe_base_input_dir"),
  args[["base-source"]],
  env_or_default("input_recipe_base_source")
)
base_tokens <- first_nonempty(args[["base-tokens"]], env_or_default("input_recipe_base_tokens"))
movement_pairs <- first_nonempty(args[["movement-pairs"]], env_or_default("input_recipe_movement_pairs"))
sel_nodes <- first_nonempty(args[["sel-nodes"]], env_or_default("input_recipe_sel_nodes"))
index_cv_half <- as_flag(args[["index-cv-half"]], as_flag(env_or_default("input_recipe_index_cv_half"), FALSE))
overwrite <- as_flag(args[["overwrite"]], TRUE)

if (!nzchar(sensitivity_output_dir)) stop("output-dir is required.")
sensitivity_output_dir_abs <- resolve_path(sensitivity_output_dir, project_root)

if (!nzchar(base_input_dir)) {
  base_input_dir <- infer_recipe_source_dir(
    sensitivity_output_dir_abs,
    movement_pairs = movement_pairs,
    sel_nodes = sel_nodes,
    index_cv_half = index_cv_half,
    project_root = project_root
  )
}
base_input_dir_abs <- resolve_path(base_input_dir, project_root)
if (!dir.exists(base_input_dir_abs)) {
  stop(
    "Base input directory does not exist: ", base_input_dir_abs,
    ". Set --base-input-dir/input_recipe_base_input_dir explicitly for this recipe."
  )
}

if (dir.exists(sensitivity_output_dir_abs) && !overwrite) {
  cat("Sensitivity output input already exists, not overwriting:", sensitivity_output_dir_abs, "\n")
  quit(save = "no", status = 0)
}

tmp_root <- tempfile("input_recipe_")
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

filtered_dir <- file.path(tmp_root, "02_release_filtered")
current_dir <- filtered_dir

copy_input_dir(base_input_dir_abs, filtered_dir)

base_change_tokens <- base_recipe_tokens(recipe_base, base_tokens)
if (length(base_change_tokens) > 0) {
  append_input_change_metadata(
    filtered_dir,
    token = base_change_tokens,
    label = paste(base_change_tokens, collapse = " + "),
    operation = "base_variant",
    source_dir = base_input_dir_abs
  )
}

recipe_steps <- build_input_recipe_steps(list(
  movement_pairs = movement_pairs,
  sel_nodes = sel_nodes,
  index_cv_half = index_cv_half
))

for (step in recipe_steps) {
  if (!isTRUE(step$enabled)) next
  next_dir <- file.path(tmp_root, step$subdir)
  run_rscript(step$script, env = step$env(current_dir, next_dir))
  current_dir <- next_dir
}

copy_input_dir(current_dir, sensitivity_output_dir_abs)

final_meta <- read_input_change_metadata(sensitivity_output_dir_abs)
final_tokens <- final_meta$tokens
final_description <- input_change_metadata_description(final_meta)
if (nzchar(final_description) || length(final_tokens) > 0) {
  set_input_change_description(
    sensitivity_output_dir_abs,
    label = if (length(final_tokens) > 0) paste(final_tokens, collapse = " + ") else NULL,
    description = final_description
  )
}

cat("Built sensitivity input recipe:\n")
cat("  base input:", base_input_dir_abs, "\n")
cat("  sensitivity output:", sensitivity_output_dir_abs, "\n")
cat("  base tokens:", ifelse(length(base_change_tokens) > 0, paste(base_change_tokens, collapse = ", "), "<none>"), "\n")
cat("  movement:", ifelse(nzchar(movement_pairs), movement_pairs, "<none>"), "\n")
cat("  sel nodes:", ifelse(nzchar(sel_nodes), sel_nodes, "<none>"), "\n")
cat("  index CV half:", index_cv_half, "\n")
cat("  tokens:", paste(extract_input_change_tokens(sensitivity_output_dir_abs), collapse = ", "), "\n")
cat("  description:", extract_input_change_description(sensitivity_output_dir_abs), "\n")
