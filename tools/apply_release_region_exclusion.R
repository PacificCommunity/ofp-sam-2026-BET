# ============================================================
# Apply the standard tag release-region exclusion to a 4-region input folder.
#
# This intentionally uses the existing 4-region inputs as the base, so no
# 4-region index CSV is needed. The 9-region source is used only to determine
# which original release groups belong to the requested release region.
# ============================================================

library(FLR4MFCL)
source("tools/tag_sens.R")
source("tools/input_change_metadata.R")

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

parse_int_list <- function(x) {
  vals <- unlist(strsplit(as.character(x), "[,[:space:]]+"))
  vals <- suppressWarnings(as.integer(vals[nzchar(vals)]))
  unique(vals[is.finite(vals) & vals > 0])
}

base_dir <- env_or_default("base_dir", "mfcl/inputs/2023_4region")
release_region_source_dir <- env_or_default("release_region_source_dir", "mfcl/inputs/2023_rep")
out_root <- env_or_default("out_root", "mfcl/inputs")
release_regions <- parse_int_list(env_or_default("release_regions", "9"))

if (length(release_regions) == 0) stop("release_regions is empty")

project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
source_dir_abs <- file.path(project_root, release_region_source_dir)
out_root_abs <- file.path(project_root, out_root)

if (!dir.exists(base_dir_abs)) stop("Base inputs directory does not exist: ", base_dir_abs)
if (!dir.exists(source_dir_abs)) stop("Release-region source directory does not exist: ", source_dir_abs)
dir.create(out_root_abs, recursive = TRUE, showWarnings = FALSE)

cat("Base 4-region inputs dir :", base_dir_abs, "\n")
cat("Release-region source dir:", source_dir_abs, "\n")
cat("Output root dir          :", out_root_abs, "\n")
cat("Release regions to exclude:", paste(release_regions, collapse = ", "), "\n")

detect_single_file <- function(path, pattern, label, required = TRUE) {
  x <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(x) == 0) {
    if (required) stop("No ", label, " found in ", path)
    return(NULL)
  }
  if (length(x) > 1) warning("Multiple ", label, " files found; using first: ", x[1])
  x[1]
}

read_mfcl_input <- function(path) {
  fname <- basename(path)
  if (grepl("\\.ini$", fname, ignore.case = TRUE)) return(read.MFCLIni(path))
  if (grepl("\\.frq$", fname, ignore.case = TRUE)) return(read.MFCLFrq(path))
  if (grepl("\\.par$", fname, ignore.case = TRUE)) return(read.MFCLPar(path))
  if (grepl("(^tag$|\\.tag$)", fname, ignore.case = TRUE)) return(read.MFCLTag(path))
  stop("Unsupported MFCL input file type: ", fname)
}

detect_tag_file <- function(path) {
  tag_candidates <- list.files(path, pattern = "(^tag$|\\.tag$)", full.names = FALSE)
  if (length(tag_candidates) == 0) stop("No tag/.tag file found in ", path)
  if ("tag" %in% tag_candidates) "tag" else tag_candidates[1]
}

source_tag_file <- detect_tag_file(source_dir_abs)
source_tag <- read_mfcl_input(file.path(source_dir_abs, source_tag_file))
source_rel <- source_tag@releases
source_rel$rel.group <- as.numeric(source_rel$rel.group)
source_rel$region <- as.numeric(source_rel$region)

release_groups <- sort(unique(source_rel$rel.group[source_rel$region %in% release_regions]))
release_groups <- normalize_release_groups(release_groups)
if (length(release_groups) == 0) {
  stop("No release groups found for release region(s): ", paste(release_regions, collapse = ", "))
}
cat("Original release groups to exclude:", paste(release_groups, collapse = ", "), "\n")

frq_file <- detect_single_file(base_dir_abs, "\\.frq$", ".frq")
ini_file <- detect_single_file(base_dir_abs, "\\.ini$", ".ini")
tag_file <- detect_tag_file(base_dir_abs)
par_file <- detect_single_file(base_dir_abs, "^11\\.par$", "11.par", required = FALSE)

cat("Detected 4-region files:\n")
cat("  ini :", ini_file, "\n")
cat("  frq :", frq_file, "\n")
cat("  tag :", tag_file, "\n")
cat("  par :", if (is.null(par_file)) "<none>" else par_file, "\n")

ini.obj <- read_mfcl_input(file.path(base_dir_abs, ini_file))
frq.obj <- read_mfcl_input(file.path(base_dir_abs, frq_file))
tag.obj <- read_mfcl_input(file.path(base_dir_abs, tag_file))

case <- exclude_one_release_group_case(
  ini.obj = ini.obj,
  tag.obj = tag.obj,
  frq.obj = frq.obj,
  exclude_release_groups = release_groups
)
check_release_group_exclusion(case, release_groups)

case_name <- paste0("release_region_", paste(release_regions, collapse = "_"), "_excluded")
out_dir <- env_or_default("out_dir", file.path(out_root, basename(base_dir_abs)))
target_dir <- if (grepl("^/", out_dir)) out_dir else file.path(project_root, out_dir)
if (normalizePath(target_dir, winslash = "/", mustWork = FALSE) ==
    normalizePath(base_dir_abs, winslash = "/", mustWork = TRUE)) {
  stop("out_dir must be different from base_dir; write to a temp dir and replace after success.")
}
if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

files_to_copy <- list.files(base_dir_abs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
ok <- file.copy(files_to_copy, to = target_dir, overwrite = TRUE, recursive = TRUE)
if (!all(ok)) warning("Some files failed to copy into: ", target_dir)

write(case$ini, file.path(target_dir, ini_file))
write(case$frq, file.path(target_dir, frq_file))
write(case$tag, file.path(target_dir, tag_file))

change_meta <- append_input_change_metadata(
  target_dir,
  token = paste0("ExRG", paste(release_regions, collapse = "_")),
  label = paste0("Excluded release region(s) ", paste(release_regions, collapse = ", ")),
  operation = "release_region_exclusion",
  source_dir = base_dir_abs,
  details = list(
    excluded_release_regions = release_regions,
    excluded_release_groups = case$excluded_release_groups
  )
)

info <- list(
  source_base_dir = base_dir_abs,
  release_region_source_dir = source_dir_abs,
  created_at = Sys.time(),
  input_change_tokens = change_meta$tokens,
  case_name = case_name,
  excluded_release_regions = release_regions,
  excluded_release_groups = case$excluded_release_groups,
  original_release_groups = case$original_release_groups,
  kept_original_groups = case$kept_original_groups,
  dropped_original_groups = case$dropped_original_groups,
  ini_file = ini_file,
  frq_file = frq_file,
  tag_file = tag_file,
  par_file = par_file,
  par_file_modified = FALSE
)
saveRDS(info, file = file.path(target_dir, "release_region_exclusion_info.rds"), compress = "xz")

cat(sprintf(
  "[%s] created: %s | tag release_groups=%d | ini taggrps=%d | frq n_tag_groups=%s\n",
  case_name,
  target_dir,
  case$tag@release_groups,
  as.numeric(case$ini@dimensions["taggrps"]),
  if (!is.null(case$frq)) as.character(case$frq@n_tag_groups) else "NA"
))
if (!is.null(par_file)) cat("Left par unchanged:", par_file, "\n")

cat("\nDone.\n")
