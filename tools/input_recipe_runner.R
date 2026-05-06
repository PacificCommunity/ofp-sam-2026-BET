as_input_recipe_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || !nzchar(as.character(x[[1]]))) return(default)
  tolower(trimws(as.character(x[[1]]))) %in% c("1", "true", "yes", "y", "on")
}

first_nonempty_input_recipe_value <- function(...) {
  for (x in list(...)) {
    if (!is.null(x) && length(x) > 0 && nzchar(trimws(as.character(x[[1]])))) {
      return(trimws(as.character(x[[1]])))
    }
  }
  ""
}

ensure_input_dir_available <- function(base_dir, project_root = getwd()) {
  base_dir_abs <- if (grepl("^/", base_dir)) base_dir else file.path(project_root, base_dir)
  if (dir.exists(base_dir_abs)) return(base_dir_abs)

  if (!as_input_recipe_flag(Sys.getenv("build_inputs_on_missing", "0"))) {
    return(base_dir_abs)
  }

  recipe_enabled <- Sys.getenv("input_recipe_enabled", "0")
  if (!as_input_recipe_flag(recipe_enabled)) {
    return(base_dir_abs)
  }

  builder <- Sys.getenv("input_recipe_builder", "tools/input_sensitivities/build_input_recipe.R")
  if (!file.exists(file.path(project_root, builder))) {
    stop("Input recipe builder not found: ", file.path(project_root, builder))
  }

  args <- c(
    builder,
    "--output-dir", base_dir,
    "--base", Sys.getenv("input_recipe_base", "base"),
    "--overwrite"
  )

  add_arg <- function(flag, value) {
    if (!is.null(value) && nzchar(trimws(as.character(value)))) c(flag, as.character(value)) else character(0)
  }
  args <- c(
    args,
    add_arg("--base-input-dir", first_nonempty_input_recipe_value(
      Sys.getenv("input_recipe_base_input_dir", ""),
      Sys.getenv("input_recipe_base_source", "")
    )),
    add_arg("--base-tokens", Sys.getenv("input_recipe_base_tokens", "")),
    add_arg("--fixed-params", Sys.getenv("input_recipe_fixed_params", "")),
    add_arg("--movement-pairs", Sys.getenv("input_recipe_movement_pairs", "")),
    add_arg("--sel-nodes", Sys.getenv("input_recipe_sel_nodes", ""))
  )
  if (as_input_recipe_flag(Sys.getenv("input_recipe_index_cv_half", "0"))) {
    args <- c(args, "--index-cv-half")
  }
  cat("Sensitivity input directory is missing; building from recipe:\n")
  cat("  sensitivity output:", base_dir, "\n")
  cat("  command: Rscript", paste(args, collapse = " "), "\n")
  status <- system2("Rscript", args)
  if (!identical(status, 0L)) {
    stop("Failed to build input recipe for missing base_dir: ", base_dir)
  }
  if (!dir.exists(base_dir_abs)) {
    stop("Input recipe completed but base_dir is still missing: ", base_dir_abs)
  }
  base_dir_abs
}
