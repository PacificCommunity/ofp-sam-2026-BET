#!/usr/bin/env Rscript

source("tools/input_sensitivities/helpers.R")
source("tools/input_sensitivities/sensitivity_catalog.R")
source("tools/input_sensitivities/recipe_registry.R")

catalog <- input_sensitivity_catalog()
if (nrow(catalog) == 0) stop("Sensitivity catalog is empty.")

choices <- input_sensitivity_choices()
if (length(choices) != nrow(catalog)) stop("Sensitivity choices do not match catalog rows.")

sample_name <- "2023_4region_fixVB_M_movement_R2_R3_sel_spline4_index_cv_half"
sample_compact <- compact_input_name(sample_name)
if (!identical(sample_compact, "2023_4region_fixM_fixVB_m23_sel4_cvH")) {
  stop("Unexpected compact_input_name result: ", sample_compact)
}

step_options <- list(
  fixed_params = "VB,M",
  movement_pairs = "2-3",
  sel_nodes = "4",
  index_cv_half = TRUE
)
steps <- build_input_recipe_steps(step_options)
required_step_fields <- c("name", "enabled", "subdir", "script", "env")
for (step in steps) {
  missing <- setdiff(required_step_fields, names(step))
  if (length(missing) > 0) {
    step_name <- if (!is.null(step$name) && nzchar(step$name)) step$name else "<unnamed>"
    stop("Recipe step ", step_name, " is missing: ", paste(missing, collapse = ", "))
  }
  if (!file.exists(step$script)) stop("Recipe step script not found: ", step$script)
  env <- step$env("base_dir_example", "out_dir_example")
  if (!is.character(env) || length(env) == 0) {
    stop("Recipe step env() must return a non-empty character vector for ", step$name)
  }
}

cat("Sensitivity setup OK\n")
cat("  catalog rows:", nrow(catalog), "\n")
cat("  choices:", paste(names(choices), collapse = " | "), "\n")
cat("  compact name:", sample_name, "->", sample_compact, "\n")
cat("  recipe steps:", paste(vapply(steps, `[[`, character(1), "name"), collapse = ", "), "\n")
