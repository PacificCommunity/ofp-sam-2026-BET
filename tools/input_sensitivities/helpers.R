env_or_default <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || !nzchar(as.character(x[[1]]))) return(default)
  tolower(trimws(as.character(x[[1]]))) %in% c("1", "true", "yes", "y", "on")
}

first_nonempty <- function(...) {
  for (x in list(...)) {
    if (!is.null(x) && length(x) > 0 && nzchar(trimws(as.character(x[[1]])))) {
      return(trimws(as.character(x[[1]])))
    }
  }
  ""
}

resolve_path <- function(path, root = getwd()) {
  if (grepl("^/", path)) path else file.path(root, path)
}

movement_pairs_to_suffix <- function(movement_pairs) {
  movement_pairs <- trimws(as.character(movement_pairs[[1]]))
  if (!nzchar(movement_pairs)) return("")
  pairs <- trimws(unlist(strsplit(movement_pairs, "[,[:space:]]+")))
  pairs <- pairs[nzchar(pairs)]
  if (length(pairs) == 0) return("")
  parts <- lapply(pairs, function(pair) {
    vals <- suppressWarnings(as.integer(unlist(strsplit(pair, "[-:>]"))))
    vals <- vals[is.finite(vals)]
    if (length(vals) != 2L) stop("Invalid movement pair: ", pair)
    paste0("R", vals[[1]], "_R", vals[[2]])
  })
  paste0("movement_", paste(unlist(parts, use.names = FALSE), collapse = "_"))
}

strip_suffix_once <- function(x, suffix) {
  if (!nzchar(suffix)) return(x)
  pattern <- paste0("_", suffix, "$")
  sub(pattern, "", x)
}

infer_recipe_source_dir <- function(output_dir,
                                    fixed_params = "",
                                    movement_pairs = "",
                                    sel_nodes = "",
                                    index_cv_half = FALSE,
                                    project_root = getwd()) {
  output_dir_abs <- resolve_path(output_dir, project_root)
  parent <- dirname(output_dir_abs)
  stem <- basename(output_dir_abs)

  if (isTRUE(index_cv_half)) stem <- strip_suffix_once(stem, "index_cv_half")
  if (nzchar(sel_nodes)) stem <- strip_suffix_once(stem, paste0("sel_spline", sel_nodes))

  movement_suffix <- movement_pairs_to_suffix(movement_pairs)
  if (nzchar(movement_suffix)) stem <- strip_suffix_once(stem, movement_suffix)

  fixed_vals <- toupper(trimws(unlist(strsplit(as.character(fixed_params), "[,;[:space:]+]+", perl = TRUE), use.names = FALSE)))
  fixed_vals <- fixed_vals[nzchar(fixed_vals)]
  if ("VBM" %in% fixed_vals || all(c("VB", "M") %in% fixed_vals)) {
    stem <- strip_suffix_once(strip_suffix_once(strip_suffix_once(strip_suffix_once(stem, "fixM_fixVB"), "fixVB_fixM"), "fixVBM"), "fixVB_M")
  } else {
    if ("VB" %in% fixed_vals) stem <- strip_suffix_once(stem, "fixVB")
    if ("M" %in% fixed_vals) stem <- strip_suffix_once(stem, "fixM")
  }

  file.path(parent, stem)
}

base_recipe_tokens <- function(recipe_base = "", base_tokens = "") {
  tokens <- normalize_input_change_tokens(base_tokens)
  if (length(tokens) > 0) return(tokens)

  recipe_base <- trimws(as.character(recipe_base[[1]]))
  if (!nzchar(recipe_base) || identical(recipe_base, "base")) return(character(0))
  if (identical(recipe_base, "fixVBM")) return(c("fixM", "fixVB"))
  if (identical(recipe_base, "fixVB_M")) return(c("fixVB", "fixM"))
  normalize_input_change_tokens(recipe_base)
}

copy_input_dir <- function(src, dst, overwrite = TRUE) {
  if (dir.exists(dst) && isTRUE(overwrite)) unlink(dst, recursive = TRUE, force = TRUE)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- file.copy(files, to = dst, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) stop("Failed to copy some files from ", src, " to ", dst)
  invisible(dst)
}

remove_input_par_files <- function(path) {
  if (!dir.exists(path)) return(character(0))
  par_files <- list.files(path, pattern = "\\.par([0-9]+)?$", full.names = TRUE)
  if (length(par_files) > 0) unlink(par_files, force = TRUE)
  par_files
}

input_sensitivity_script <- function(...) {
  file.path("tools", "input_sensitivities", ...)
}

input_sensitivity_step <- function(script) {
  input_sensitivity_script("steps", script)
}

run_rscript <- function(script, env = character(0), args = character(0)) {
  cmd <- c(env, "Rscript", script, args)
  cat(">>", paste(shQuote(cmd), collapse = " "), "\n")
  status <- system2("Rscript", c(script, args), env = env)
  if (!identical(status, 0L)) stop("Command failed: Rscript ", script)
  invisible(status)
}

resolve_single_or_batch_dirs <- function(inputs_root = "mfcl/inputs",
                                         base_dir = "",
                                         out_dir = "",
                                         suffix = "",
                                         base_pattern = ".",
                                         project_root = getwd()) {
  inputs_root_abs <- resolve_path(inputs_root, project_root)

  if (nzchar(base_dir)) {
    source_dirs <- resolve_path(base_dir, project_root)
    target_dirs <- if (nzchar(out_dir)) {
      resolve_path(out_dir, project_root)
    } else {
      file.path(dirname(source_dirs), paste0(basename(source_dirs), suffix))
    }
    if (!dir.exists(source_dirs)) stop("base_dir does not exist: ", source_dirs)
    return(list(source_dirs = source_dirs, target_dirs = target_dirs))
  }

  if (!dir.exists(inputs_root_abs)) stop("inputs_root does not exist: ", inputs_root_abs)
  base_dirs <- list.dirs(inputs_root_abs, full.names = FALSE, recursive = FALSE)
  base_dirs <- base_dirs[grepl(base_pattern, base_dirs)]
  if (nzchar(suffix)) base_dirs <- base_dirs[!endsWith(base_dirs, suffix)]
  if (length(base_dirs) == 0) stop("No matching input directories found in ", inputs_root_abs)

  list(
    source_dirs = file.path(inputs_root_abs, base_dirs),
    target_dirs = file.path(inputs_root_abs, paste0(base_dirs, suffix))
  )
}
