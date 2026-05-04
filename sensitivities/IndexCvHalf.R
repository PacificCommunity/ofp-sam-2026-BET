# ============================================================
# Create 4-region sensitivities with index CPUE CV flags halved.
#
# This edits doitall.sh only. The survey CPUE CVs are fish flag 92 values
# for fisheries 33:36, stored as round(CV * 100).
# ============================================================

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

source("tools/input_change_metadata.R")

project_root <- getwd()
inputs_root <- env_or_default("inputs_root", "mfcl/inputs")
single_base_dir <- env_or_default("base_dir", "")
single_out_dir <- env_or_default("out_dir", "")
suffix <- env_or_default("suffix", "_index_cv_half")

inputs_root_abs <- if (grepl("^/", inputs_root)) inputs_root else file.path(project_root, inputs_root)
if (nzchar(single_base_dir)) {
  source_dirs <- if (grepl("^/", single_base_dir)) single_base_dir else file.path(project_root, single_base_dir)
  target_dirs <- if (nzchar(single_out_dir)) {
    if (grepl("^/", single_out_dir)) single_out_dir else file.path(project_root, single_out_dir)
  } else {
    file.path(dirname(source_dirs), paste0(basename(source_dirs), suffix))
  }
  if (!dir.exists(source_dirs)) stop("base_dir does not exist: ", source_dirs)
} else {
  if (!dir.exists(inputs_root_abs)) stop("inputs_root does not exist: ", inputs_root_abs)
  base_dirs <- list.dirs(inputs_root_abs, full.names = FALSE, recursive = FALSE)
  base_dirs <- base_dirs[grepl("^2023_4region", base_dirs)]
  base_dirs <- base_dirs[!endsWith(base_dirs, suffix)]
  if (length(base_dirs) == 0) stop("No 4-region input directories found in ", inputs_root_abs)
  source_dirs <- file.path(inputs_root_abs, base_dirs)
  target_dirs <- file.path(inputs_root_abs, paste0(base_dirs, suffix))
}

halve_index_cv_flags <- function(doitall_path) {
  lines <- readLines(doitall_path, warn = FALSE)
  index_fisheries <- 33:36
  original <- integer(length(index_fisheries))
  names(original) <- as.character(index_fisheries)
  updated <- original

  for (fishery in index_fisheries) {
    pattern <- paste0("(-", fishery, "[[:space:]]+92[[:space:]]+)([0-9]+)")
    hit <- grep(pattern, lines)
    if (length(hit) != 1) {
      stop("Expected exactly one fish flag 92 entry for fishery ", fishery, " in ", doitall_path)
    }

    old_value <- suppressWarnings(as.integer(sub(paste0(".*", pattern, ".*"), "\\2", lines[hit])))
    if (!is.finite(old_value)) stop("Could not parse fish flag 92 value for fishery ", fishery)

    new_value <- floor(old_value / 2 + 0.5)
    lines[hit] <- sub(pattern, paste0("\\1", new_value), lines[hit])
    original[as.character(fishery)] <- old_value
    updated[as.character(fishery)] <- new_value
  }

  writeLines(lines, doitall_path, useBytes = TRUE)
  list(original = original, updated = updated)
}

created <- character(0)
for (idx in seq_along(source_dirs)) {
  source_dir <- source_dirs[[idx]]
  target_dir <- target_dirs[[idx]]
  if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- file.copy(files, to = target_dir, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) warning("Some files failed to copy for ", base_name)

  par_files <- list.files(target_dir, pattern = "\\.par$", full.names = TRUE)
  if (length(par_files) > 0) unlink(par_files, force = TRUE)

  doitall_path <- file.path(target_dir, "doitall.sh")
  if (!file.exists(doitall_path)) stop("No doitall.sh found in ", target_dir)
  cv_change <- halve_index_cv_flags(doitall_path)
  change_meta <- append_input_change_metadata(
    target_dir,
    token = "cvH",
    label = "Index CPUE CV flags halved",
    operation = "index_cv_half",
    source_dir = source_dir,
    details = list(
      index_fisheries = 33:36,
      original_flag_92 = cv_change$original,
      updated_flag_92 = cv_change$updated
    )
  )

  info <- list(
    source_dir = source_dir,
    created_at = Sys.time(),
    input_change_tokens = change_meta$tokens,
    index_fisheries = 33:36,
    original_flag_92 = cv_change$original,
    updated_flag_92 = cv_change$updated,
    modified_file = "doitall.sh",
    par_files_removed = TRUE
  )
  saveRDS(info, file = file.path(target_dir, "index_cv_half_info.rds"), compress = "xz")

  created <- c(created, target_dir)
}

cat("Created index CV half sensitivities:\n")
cat(paste0("  ", created, collapse = "\n"), "\n")
cat("fish flag 92: 49,48,26,39 -> 25,24,13,20\n")
