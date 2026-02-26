# ============================================================
# Create program-exclusion input folders from a base input folder
# - Copy base_dir -> new input folders
# - Exclude tag programs (RTTP / JPTP / PTTP)
# - Overwrite tag / ini / frq in each copied folder
# - Keep all other files unchanged (par, age, maps, etc.)
#
# Requires:
#   library(FLR4MFCL)
#   source("tools/tag_sens.R")
# where tools/tag_sens.R defines:
#   make_program_exclusion_cases()
#   summarize_program_exclusion_cases()
#   check_program_exclusion()
# ============================================================

# -------------------------
# 0) Libraries + sources
# -------------------------
library(FLR4MFCL)
source("tools/tag_sens.R")

# -------------------------
# 1) Local settings + paths
# -------------------------
# Set these directly for local runs (no env vars required)
base_dir <- "mfcl/inputs/2023_rep"
out_root <- "mfcl/inputs"
programs <- c("RTTP", "JPTP", "PTTP")
programs <- toupper(trimws(programs))
programs <- programs[nzchar(programs)]

project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
out_root_abs <- file.path(project_root, out_root)

if (!dir.exists(base_dir_abs)) {
  stop("Base inputs directory does not exist: ", base_dir_abs)
}
dir.create(out_root_abs, recursive = TRUE, showWarnings = FALSE)

cat("Base inputs dir :", base_dir_abs, "\n")
cat("Output root dir :", out_root_abs, "\n")
cat("Programs to exclude:", paste(programs, collapse = ", "), "\n")

# -------------------------
# 2) Auto-detect core files in base_dir
# -------------------------
detect_single_file <- function(path, pattern, label) {
  x <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(x) == 0) stop("No ", label, " found in ", path)
  if (length(x) > 1) warning("Multiple ", label, " files found; using first: ", x[1])
  x[1]
}

# frq: auto-detect *.frq
frq_file <- detect_single_file(base_dir_abs, "\\.frq$", ".frq")

# ini: auto-detect *.ini
ini_file <- detect_single_file(base_dir_abs, "\\.ini$", ".ini")

# tag: auto-detect either exact 'tag' OR '*.tag'
tag_candidates <- list.files(
  base_dir_abs,
  pattern = "(^tag$|\\.tag$)",
  full.names = FALSE
)

if (length(tag_candidates) == 0) {
  stop("No tag/.tag file found in ", base_dir_abs)
}

# Prefer exact 'tag' if present, otherwise first *.tag
if ("tag" %in% tag_candidates) {
  tag_file <- "tag"
} else {
  if (length(tag_candidates) > 1) {
    warning("Multiple tag-like files found; using first: ", tag_candidates[1])
  }
  tag_file <- tag_candidates[1]
}

cat("Detected files:\n")
cat("  ini :", ini_file, "\n")
cat("  frq :", frq_file, "\n")
cat("  tag :", tag_file, "\n")

# -------------------------
# 3) Read MFCL objects
# -------------------------
read_mfcl_input <- function(path) {
  fname <- basename(path)
  
  if (grepl("\\.ini$", fname, ignore.case = TRUE)) {
    return(read.MFCLIni(path))
  }
  if (grepl("\\.frq$", fname, ignore.case = TRUE)) {
    return(read.MFCLFrq(path))
  }
  if (grepl("(^tag$|\\.tag$)", fname, ignore.case = TRUE)) {
    return(read.MFCLTag(path))
  }
  
  stop("Unsupported MFCL input file type: ", fname)
}

ini.obj <- read_mfcl_input(file.path(base_dir_abs, ini_file))
frq.obj <- read_mfcl_input(file.path(base_dir_abs, frq_file))
tag.obj <- read_mfcl_input(file.path(base_dir_abs, tag_file))

# -------------------------
# 4) Build exclusion cases
# -------------------------
cases <- make_program_exclusion_cases(
  ini.obj = ini.obj,
  tag.obj = tag.obj,
  frq.obj = frq.obj,
  programs = programs
)

print(summarize_program_exclusion_cases(cases))

# Optional validation (recommended)
for (p in programs) {
  nm <- paste0("exclude_", p)
  check_program_exclusion(cases[[nm]], p)
}

# -------------------------
# 5) Create copied input folders and overwrite tag/ini/frq
# -------------------------
create_program_exclusion_input_folders <- function(cases,
                                                   base_dir_abs,
                                                   out_root_abs,
                                                   ini_file,
                                                   frq_file,
                                                   tag_file,
                                                   prefix = NULL,
                                                   overwrite_dir = TRUE) {
  if (is.null(cases) || length(cases) == 0) stop("cases is empty")
  if (!dir.exists(base_dir_abs)) stop("base_dir_abs does not exist: ", base_dir_abs)
  
  created <- character(0)
  
  for (nm in names(cases)) {
    x <- cases[[nm]]
    
    folder_name <- if (is.null(prefix) || !nzchar(prefix)) nm else paste0(prefix, "_", nm)
    target_dir <- file.path(out_root_abs, folder_name)
    
    if (dir.exists(target_dir) && overwrite_dir) {
      unlink(target_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Copy all base inputs first
    files_to_copy <- list.files(base_dir_abs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    ok <- file.copy(files_to_copy, to = target_dir, overwrite = TRUE, recursive = TRUE)
    if (!all(ok)) {
      warning("Some files failed to copy into: ", target_dir)
    }
    
    # Overwrite excluded core files
    write(x$ini, file.path(target_dir, ini_file))
    write(x$frq, file.path(target_dir, frq_file))
    write(x$tag, file.path(target_dir, tag_file))
    
    # Traceability metadata
    info <- list(
      source_base_dir = base_dir_abs,
      created_at = Sys.time(),
      case_name = nm,
      excluded_programs = x$excluded_programs,
      original_release_groups = x$original_release_groups,
      kept_original_groups = x$kept_original_groups,
      dropped_original_groups = x$dropped_original_groups,
      ini_file = ini_file,
      frq_file = frq_file,
      tag_file = tag_file
    )
    saveRDS(info, file = file.path(target_dir, "program_exclusion_info.rds"), compress = "xz")
    
    cat(sprintf(
      "[%s] created: %s | tag release_groups=%d | ini taggrps=%d | frq n_tag_groups=%s\n",
      nm,
      target_dir,
      x$tag@release_groups,
      as.numeric(x$ini@dimensions["taggrps"]),
      if (!is.null(x$frq)) as.character(x$frq@n_tag_groups) else "NA"
    ))
    
    created <- c(created, target_dir)
  }
  
  invisible(created)
}

# -------------------------
# 6) Run folder creation
# -------------------------
created_dirs <- create_program_exclusion_input_folders(
  cases = cases,
  base_dir_abs = base_dir_abs,
  out_root_abs = out_root_abs,
  ini_file = ini_file,
  frq_file = frq_file,
  tag_file = tag_file,
  prefix = basename(base_dir_abs),   # e.g. 2023_rep_exclude_RTTP
  overwrite_dir = TRUE
)

cat("\nCreated input folders:\n")
print(created_dirs)

cat("\nDone.\n")
