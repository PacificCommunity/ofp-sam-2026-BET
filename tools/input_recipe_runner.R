as_input_recipe_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || !nzchar(as.character(x[[1]]))) return(default)
  tolower(trimws(as.character(x[[1]]))) %in% c("1", "true", "yes", "y", "on")
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

  builder <- Sys.getenv("input_recipe_builder", "tools/build_4region_input_recipe.R")
  if (!file.exists(file.path(project_root, builder))) {
    stop("Input recipe builder not found: ", file.path(project_root, builder))
  }

  args <- c(
    builder,
    "--output-dir", base_dir,
    "--base", Sys.getenv("input_recipe_base", "base"),
    "--release-regions", Sys.getenv("input_recipe_release_regions", "9"),
    "--overwrite"
  )

  add_arg <- function(flag, value) {
    if (!is.null(value) && nzchar(trimws(as.character(value)))) c(flag, as.character(value)) else character(0)
  }
  args <- c(
    args,
    add_arg("--base-source", Sys.getenv("input_recipe_base_source", "")),
    add_arg("--movement-pairs", Sys.getenv("input_recipe_movement_pairs", "")),
    add_arg("--sel-nodes", Sys.getenv("input_recipe_sel_nodes", "")),
    add_arg("--program-path", Sys.getenv("program_path", Sys.getenv("MFCL_EXE", "")))
  )
  if (as_input_recipe_flag(Sys.getenv("input_recipe_index_cv_half", "0"))) {
    args <- c(args, "--index-cv-half")
  }
  if (as_input_recipe_flag(Sys.getenv("input_recipe_with_11par", "1"), default = TRUE)) {
    args <- c(args, "--with-11par")
  }

  cat("Base input directory is missing; building from recipe:\n")
  cat("  base_dir:", base_dir, "\n")
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
