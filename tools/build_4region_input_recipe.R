#!/usr/bin/env Rscript

source("tools/input_change_metadata.R")

env_or_default <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

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

run_rscript <- function(script, env = character(0), args = character(0)) {
  cmd <- c(env, "Rscript", script, args)
  cat(">>", paste(shQuote(cmd), collapse = " "), "\n")
  status <- system2("Rscript", c(script, args), env = env)
  if (!identical(status, 0L)) stop("Command failed: Rscript ", script)
}

copy_dir <- function(src, dst) {
  if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- file.copy(files, to = dst, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) stop("Failed to copy some files from ", src, " to ", dst)
}

project_root <- getwd()
args <- parse_args(commandArgs(trailingOnly = TRUE))

output_dir <- first_nonempty(args[["output-dir"]], env_or_default("input_recipe_output_dir"), env_or_default("base_dir"))
recipe_base <- first_nonempty(args[["base"]], env_or_default("input_recipe_base"), "base")
base_source <- first_nonempty(args[["base-source"]], env_or_default("input_recipe_base_source"))
movement_pairs <- first_nonempty(args[["movement-pairs"]], env_or_default("input_recipe_movement_pairs"))
sel_nodes <- first_nonempty(args[["sel-nodes"]], env_or_default("input_recipe_sel_nodes"))
release_regions <- first_nonempty(args[["release-regions"]], env_or_default("input_recipe_release_regions"), "9")
index_csv <- first_nonempty(args[["index-csv"]], env_or_default("INDEX_CSV"), file.path(project_root, "mfcl/bet.2023.indices.4-region.csv"))
mfcl_exe <- first_nonempty(args[["program-path"]], env_or_default("MFCL_EXE"), file.path(project_root, "mfcl/exe/mfclo64_2026_02_04_vsn2278"))
index_cv_half <- as_flag(args[["index-cv-half"]], as_flag(env_or_default("input_recipe_index_cv_half"), FALSE))
with_11par <- as_flag(args[["with-11par"]], as_flag(env_or_default("input_recipe_with_11par"), TRUE))
overwrite <- as_flag(args[["overwrite"]], TRUE)

if (!nzchar(output_dir)) stop("output-dir is required.")
output_dir_abs <- if (grepl("^/", output_dir)) output_dir else file.path(project_root, output_dir)

source_map <- list(
  base = c("mfcl/inputs/2023_rep", "mfcl/inputs/2023_4region"),
  fixM = c("mfcl/inputs/2023_fixM", "mfcl/inputs/2023_4region_fixM"),
  fixVB = c("mfcl/inputs/2023_fixVB", "mfcl/inputs/2023_4region_fixVB"),
  fixVB_M = c("mfcl/inputs/2023_fixVB_M", "mfcl/inputs/2023_4region_fixVB_M")
)
if (!nzchar(base_source)) {
  if (!recipe_base %in% names(source_map)) {
    stop("Unknown recipe base: ", recipe_base, ". Use one of: ", paste(names(source_map), collapse = ", "))
  }
  source_candidates <- unname(source_map[[recipe_base]])
  candidate_abs <- ifelse(grepl("^/", source_candidates), source_candidates, file.path(project_root, source_candidates))
  candidate_is_4region <- grepl("^2023_4region", basename(candidate_abs))
  candidate_usable <- dir.exists(candidate_abs) & (candidate_is_4region | file.exists(index_csv))
  hit <- source_candidates[candidate_usable]
  if (length(hit) == 0) {
    stop(
      "No source input exists for recipe base '", recipe_base, "'. Tried: ",
      paste(candidate_abs, collapse = ", "),
      if (!file.exists(index_csv)) paste0(". The 9-region source route also needs index_csv=", index_csv, ".") else ""
    )
  }
  base_source <- hit[[1]]
}
base_source_abs <- if (grepl("^/", base_source)) base_source else file.path(project_root, base_source)
if (!dir.exists(base_source_abs)) stop("Base source input does not exist: ", base_source_abs)
base_source_is_4region <- grepl("^2023_4region", basename(base_source_abs))

if (dir.exists(output_dir_abs) && !overwrite) {
  cat("Input already exists, not overwriting:", output_dir_abs, "\n")
  quit(save = "no", status = 0)
}

tmp_root <- tempfile("input_recipe_")
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

collapsed_dir <- file.path(tmp_root, "01_collapsed")
filtered_dir <- file.path(tmp_root, "02_release_filtered")
current_dir <- filtered_dir

if (isTRUE(base_source_is_4region)) {
  copy_dir(base_source_abs, filtered_dir)
} else {
  run_rscript(
    "tools/collapse_regions_9to4.R",
    args = c(
      "--input-dir", base_source_abs,
      "--output-dir", collapsed_dir,
      "--index-csv", index_csv,
      "--index-comp-mode", "representative",
      "--overwrite"
    )
  )

  run_rscript(
    "tools/apply_release_region_exclusion.R",
    env = c(
      paste0("base_dir=", collapsed_dir),
      paste0("release_region_source_dir=", base_source_abs),
      paste0("release_regions=", release_regions),
      paste0("out_dir=", filtered_dir)
    )
  )
}

if (recipe_base == "fixM") {
  append_input_change_metadata(filtered_dir, "fixM", "M fixed", "base_variant", base_source_abs)
} else if (recipe_base == "fixVB") {
  append_input_change_metadata(filtered_dir, "fixVB", "Growth fixed", "base_variant", base_source_abs)
} else if (recipe_base == "fixVB_M") {
  append_input_change_metadata(filtered_dir, c("fixVB", "fixM"), "Growth and M fixed", "base_variant", base_source_abs)
}

if (isTRUE(with_11par) && !isTRUE(base_source_is_4region)) {
  run_rscript(
    "tools/collapse_par_9to4_representative.R",
    args = c(
      "--source-dir", base_source_abs,
      "--target-dir", current_dir,
      "--program-path", mfcl_exe,
      "--index-csv", index_csv,
      "--overwrite"
    )
  )
}

if (nzchar(movement_pairs)) {
  next_dir <- file.path(tmp_root, "03_movement")
  run_rscript(
    "sensitivities/TagMovementSubset.R",
    env = c(
      paste0("base_dir=", current_dir),
      paste0("movement_pairs=", movement_pairs),
      paste0("out_dir=", next_dir)
    )
  )
  current_dir <- next_dir
}

if (nzchar(sel_nodes)) {
  next_dir <- file.path(tmp_root, "04_sel")
  run_rscript(
    "sensitivities/SelectivitySplineNodes.R",
    env = c(
      paste0("base_dir=", current_dir),
      paste0("out_dir=", next_dir),
      paste0("node_count=", sel_nodes)
    )
  )
  current_dir <- next_dir
}

if (isTRUE(index_cv_half)) {
  next_dir <- file.path(tmp_root, "05_index_cv_half")
  run_rscript(
    "sensitivities/IndexCvHalf.R",
    env = c(
      paste0("base_dir=", current_dir),
      paste0("out_dir=", next_dir)
    )
  )
  current_dir <- next_dir
}

copy_dir(current_dir, output_dir_abs)

cat("Built input recipe:\n")
cat("  output:", output_dir_abs, "\n")
cat("  base:", recipe_base, "(", base_source_abs, ")\n")
cat("  source already 4-region:", base_source_is_4region, "\n")
cat("  movement:", ifelse(nzchar(movement_pairs), movement_pairs, "<none>"), "\n")
cat("  sel nodes:", ifelse(nzchar(sel_nodes), sel_nodes, "<none>"), "\n")
cat("  index CV half:", index_cv_half, "\n")
cat("  tokens:", paste(extract_input_change_tokens(output_dir_abs), collapse = ", "), "\n")
