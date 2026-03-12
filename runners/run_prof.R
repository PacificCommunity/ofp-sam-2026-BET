## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")
source("tools/model_payload.R")
source("tools/post_hessian.R")
source("tools/condor_archive_cleanup.R")

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
program_path_abs <- file.path(project_root, program_path)
base_dir_abs <- file.path(project_root, base_dir)

## Profile likelihood settings
scaler <- as.numeric(Sys.getenv("scaler", "90"))
Reps <- as.integer(unlist(strsplit(Sys.getenv("Reps", "1 1 1 1 1 1"), "\\s+")))
names(Reps) <- paste0("Reps", 1:length(Reps))
QuantityType <- as.numeric(Sys.getenv("QuantityType", "2"))
AgeFlags <- c(
  Af172 = as.numeric(Sys.getenv("Af172", "0")),
  Af173 = as.numeric(Sys.getenv("Af173", "0")),
  Af174 = as.numeric(Sys.getenv("Af174", "0"))
)
init_par_override <- Sys.getenv("init_par_override", "")
init_from_scaler <- suppressWarnings(as.numeric(Sys.getenv("init_from_scaler", "")))
prof_init_map_rds <- Sys.getenv("prof_init_map_rds", "")
prof_hessian <- tolower(Sys.getenv("prof_hessian", Sys.getenv("likelihood_hessian", Sys.getenv("hessian", "0")))) %in% c("1", "true", "yes", "y")

safe_read_scalar <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path) || !file.exists(path)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(read.table(path)))
}

detect_quantity_file <- function(path) {
  avg_bio_path <- file.path(path, "avg_bio")
  if (file.exists(avg_bio_path)) return(list(label = "avg_bio", path = avg_bio_path))
  rel_dep_path <- file.path(path, "relative_depletion")
  if (file.exists(rel_dep_path)) return(list(label = "relative_depletion", path = rel_dep_path))
  stop("No quantity file found in ", path, " (expected avg_bio or relative_depletion)")
}

detect_reference_quantity_file <- function(model_dir, scaler_dir) {
  model_try <- tryCatch(detect_quantity_file(model_dir), error = function(e) NULL)
  if (!is.null(model_try)) return(model_try)
  scaler_try <- tryCatch(detect_quantity_file(scaler_dir), error = function(e) NULL)
  if (!is.null(scaler_try)) return(scaler_try)
  stop("No reference quantity file found in either ", model_dir, " or ", scaler_dir)
}

quantity_label_from_type <- function(quantity_type) {
  if (isTRUE(all.equal(as.numeric(quantity_type), 1))) "relative_depletion" else "avg_bio"
}

resolve_prof_init_map_path <- function(prof_init_map_rds, project_root, base_dir_abs, scaler_dir) {
  if (!nzchar(prof_init_map_rds)) return(NA_character_)
  candidates <- unique(c(
    prof_init_map_rds,
    if (!grepl("^/", prof_init_map_rds)) file.path(project_root, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(base_dir_abs, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(scaler_dir, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(scaler_dir, basename(prof_init_map_rds)) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(base_dir_abs, basename(prof_init_map_rds)) else NA_character_
  ))
  candidates <- candidates[is.character(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

load_prof_init_map <- function(prof_init_map_rds, project_root, base_dir_abs, scaler_dir) {
  map_path <- resolve_prof_init_map_path(
    prof_init_map_rds = prof_init_map_rds,
    project_root = project_root,
    base_dir_abs = base_dir_abs,
    scaler_dir = scaler_dir
  )
  if (!is.character(map_path) || length(map_path) != 1 || !nzchar(map_path) || !file.exists(map_path)) {
    return(list(entries = NULL, path = NA_character_))
  }
  obj <- tryCatch(readRDS(map_path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj)) {
    warning("prof_init_map_rds could not be read as list: ", map_path)
    return(list(entries = NULL, path = map_path))
  }
  entries <- NULL
  if (!is.null(obj$entries) && is.list(obj$entries)) {
    entries <- obj$entries
  } else if (!is.null(obj$par_by_scaler) && is.list(obj$par_by_scaler)) {
    entries <- obj$par_by_scaler
  }
  if (is.null(entries) || !is.list(entries)) {
    warning("prof_init_map_rds has no usable entries/par_by_scaler: ", map_path)
    return(list(entries = NULL, path = map_path))
  }
  list(entries = entries, path = map_path)
}

extract_donor_par_lines <- function(entries, donor_scaler) {
  if (is.null(entries) || !is.list(entries) || !is.finite(donor_scaler)) return(character(0))
  key <- as.character(as.integer(round(donor_scaler)))
  val <- entries[[key]]
  if (is.null(val)) return(character(0))
  if (is.list(val) && !is.null(val$par_lines)) val <- val$par_lines
  if (is.null(val)) return(character(0))
  as.character(val)
}

resolve_init_par <- function(init_par_override, init_from_scaler, scaler_dir, prof_dir, fallback_par, init_map_entries = NULL) {
  # 1) Explicit override path/file
  if (nzchar(init_par_override)) {
    candidate <- if (grepl("^/", init_par_override)) init_par_override else file.path(scaler_dir, init_par_override)
    if (file.exists(candidate)) {
      out <- file.path(scaler_dir, "warm_init_override.par")
      file.copy(candidate, out, overwrite = TRUE)
      return(list(path = out, source = "override", donor = NA_integer_))
    }
    warning("init_par_override file not found: ", candidate, " -> fallback")
  }

  # 2) Donor scaler final par from model_dir/prof/scaler_<donor>
  if (is.finite(init_from_scaler)) {
    donor_lines <- extract_donor_par_lines(init_map_entries, init_from_scaler)
    if (length(donor_lines) > 0) {
      out <- file.path(scaler_dir, paste0("warm_init_from_rds_", as.integer(round(init_from_scaler)), ".par"))
      writeLines(donor_lines, con = out, useBytes = TRUE)
      return(list(path = out, source = "rds", donor = as.integer(round(init_from_scaler))))
    }

    donor_dir <- file.path(prof_dir, paste0("scaler_", as.integer(round(init_from_scaler))))
    donor_par <- mp_final_par(donor_dir)
    if (!is.null(donor_par) && file.exists(donor_par)) {
      out <- file.path(scaler_dir, paste0("warm_init_from_scaler_", as.integer(round(init_from_scaler)), ".par"))
      file.copy(donor_par, out, overwrite = TRUE)
      return(list(path = out, source = "neighbor", donor = as.integer(round(init_from_scaler))))
    }
    warning("init_from_scaler set but donor final par not found in: ", donor_dir, " -> fallback")
  }

  # 3) Default
  list(path = fallback_par, source = "default", donor = NA_integer_)
}

## Create scaler-specific directory inside prof folder
prof_dir <- file.path(model_dir, "prof")
scaler_dir <- file.path(prof_dir, paste0("scaler_", scaler))

cat("Running Profile Likelihood\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Prof directory:", prof_dir, "\n")
cat("Scaler directory:", scaler_dir, "\n")
cat("Scaler:", scaler, "\n")
cat("Reps:", Reps, "\n")
cat("QuantityType:", QuantityType, "\n")
cat("AgeFlags:", AgeFlags, "\n")
cat("init_par_override:", ifelse(nzchar(init_par_override), init_par_override, "<none>"), "\n")
cat("init_from_scaler:", ifelse(is.finite(init_from_scaler), as.character(init_from_scaler), "<none>"), "\n")
cat("prof_init_map_rds:", ifelse(nzchar(prof_init_map_rds), prof_init_map_rds, "<none>"), "\n")
cat("Profile post-hessian:", prof_hessian, "\n")

## Create scaler directory and copy all files from base_dir (inputs)
dir.create(scaler_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = scaler_dir, overwrite = TRUE, recursive = TRUE)

par_files <- list.files(scaler_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(scaler_dir, pattern = "\\.frq$", full.names = FALSE)
if (length(par_files) == 0) stop("No .par files found in directory: ", scaler_dir)

file_info <- file.info(par_files)
most_recent <- rownames(file_info)[which.max(file_info$mtime)]
cat("Most recent par file:", basename(most_recent), "\n")

init_map_obj <- load_prof_init_map(
  prof_init_map_rds = prof_init_map_rds,
  project_root = project_root,
  base_dir_abs = base_dir_abs,
  scaler_dir = scaler_dir
)
if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path) && file.exists(init_map_obj$path)) {
  cat("Loaded prof_init_map_rds:", init_map_obj$path, "\n")
}

init_info <- resolve_init_par(
  init_par_override = init_par_override,
  init_from_scaler = init_from_scaler,
  scaler_dir = scaler_dir,
  prof_dir = prof_dir,
  fallback_par = most_recent,
  init_map_entries = init_map_obj$entries
)
init_par_file <- init_info$path
init_source <- init_info$source
cat("Using Initp:", basename(init_par_file), "(source:", init_source, ")\n")

quantity_label <- quantity_label_from_type(QuantityType)
reference_quantity_path <- file.path(scaler_dir, quantity_label)
if (!file.exists(reference_quantity_path)) {
  cat("No reference quantity file found; refreshing from", basename(most_recent), "\n")
  unlink(reference_quantity_path, force = TRUE)
  ref_par <- file.path(scaler_dir, paste0("reference_", quantity_label, ".par"))
  ref_command <- paste(
    shQuote(program_path_abs),
    shQuote(frq_file),
    shQuote(basename(most_recent)),
    shQuote(basename(ref_par)),
    "-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 1",
    "1 346", QuantityType,
    "1 347 0",
    "1 348 0",
    "2 172", AgeFlags["Af172"],
    "2 173", AgeFlags["Af173"],
    "2 174", AgeFlags["Af174"]
  )
  run_commands(commands = ref_command, work_dirs = scaler_dir, save_log = FALSE, verbose = TRUE)
}

initial_quantity_info <- detect_reference_quantity_file(model_dir, scaler_dir)
reference_quantity <- safe_read_scalar(initial_quantity_info$path)
target_quantity <- reference_quantity * scaler / 100

generate_proflike_script(
  Prog = program_path_abs,
  Reps = Reps,
  AgeFlags = AgeFlags,
  QuantityType = QuantityType,
  Frq = frq_file,
  Mults = scaler,
  Initp = basename(init_par_file),
  filename = file.path(scaler_dir, "ProfLike.sh")
)

run_commands(commands = "./ProfLike.sh", work_dirs = scaler_dir, save_log = FALSE, verbose = TRUE)

final_profile_par <- mp_final_par(scaler_dir)
final_par_lines <- if (!is.null(final_profile_par) && file.exists(final_profile_par)) {
  tryCatch(readLines(final_profile_par), error = function(e) character(0))
} else {
  character(0)
}

hessian_summary <- mp_run_post_hessian(
  work_dir = scaler_dir,
  program_path_abs = program_path_abs,
  program_path = program_path,
  frq_file = frq_file,
  input_par = if (!is.null(final_profile_par)) basename(final_profile_par) else basename(init_par_file),
  project_root = project_root,
  requested = prof_hessian
)

final_quantity_info <- detect_quantity_file(scaler_dir)
actual_quantity <- safe_read_scalar(final_quantity_info$path)
target_rel_err <- suppressWarnings(abs(actual_quantity - target_quantity) / pmax(abs(target_quantity), .Machine$double.eps))

info_list <- list(
  Reps = Reps,
  AgeFlags = AgeFlags,
  scaler = scaler,
  quantity_label = final_quantity_info$label,
  reference_quantity = reference_quantity,
  target_quantity = target_quantity,
  actual_quantity = actual_quantity,
  target_rel_err = target_rel_err,
  frq_file = frq_file,
  program_path = program_path,
  model_dir = model_dir,
  scaler_dir = scaler_dir,
  init_source = init_source,
  init_par_used = basename(init_par_file),
  init_par_override = if (nzchar(init_par_override)) init_par_override else NA_character_,
  init_from_scaler = if (is.finite(init_info$donor)) init_info$donor else NA_integer_,
  prof_init_map_rds = if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path)) init_map_obj$path else NA_character_,
  final_par_lines = final_par_lines,
  hessian = hessian_summary
)

saveRDS(info_list, file = file.path(scaler_dir, "info.rds"), compress = "xz")

profile_payload <- mp_build_profile_payload(scaler_dir)
if (!is.null(profile_payload) && is.list(profile_payload)) {
  profile_payload$par_lines <- final_par_lines
  profile_payload$init_source <- init_source
  profile_payload$init_from_scaler <- if (is.finite(init_info$donor)) init_info$donor else NA_integer_
  profile_payload$prof_init_map_rds <- if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path)) init_map_obj$path else NA_character_
}
saveRDS(profile_payload, file = file.path(scaler_dir, "profile_payload.rds"), compress = "xz")

deleted_n <- mp_cleanup_files(scaler_dir, keep = c("profile_payload.rds", "info.rds"), recursive = TRUE)
cat("Cleanup removed", deleted_n, "non-core files in", scaler_dir, "\n")

cb_condor_keep_only_model_cleanup()
cat("✅ Profile likelihood completed for scaler", scaler, "\n")
