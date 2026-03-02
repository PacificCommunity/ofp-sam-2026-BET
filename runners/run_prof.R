## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")
source("tools/model_payload.R")
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
## Single scaler value for parallel execution via condor
scaler <- as.numeric(Sys.getenv("scaler", "90"))
Reps <- as.integer(unlist(strsplit(Sys.getenv("Reps", "1 1 1 1 1 1"), "\\s+")))
names(Reps) <- paste0("Reps", 1:length(Reps))
QuantityType <- as.numeric(Sys.getenv("QuantityType", "2"))
AgeFlags <- c(
  Af172 = as.numeric(Sys.getenv("Af172", "0")),
  Af173 = as.numeric(Sys.getenv("Af173", "0")),
  Af174 = as.numeric(Sys.getenv("Af174", "0"))
)

safe_read_scalar <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path) || !file.exists(path)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(read.table(path)))
}

detect_quantity_file <- function(path) {
  avg_bio_path <- file.path(path, "avg_bio")
  if (file.exists(avg_bio_path)) {
    return(list(label = "avg_bio", path = avg_bio_path))
  }
  rel_dep_path <- file.path(path, "relative_depletion")
  if (file.exists(rel_dep_path)) {
    return(list(label = "relative_depletion", path = rel_dep_path))
  }
  stop("No quantity file found in ", path, " (expected avg_bio or relative_depletion)")
}

detect_reference_quantity_file <- function(model_dir, scaler_dir) {
  model_try <- tryCatch(detect_quantity_file(model_dir), error = function(e) NULL)
  if (!is.null(model_try)) {
    return(model_try)
  }

  scaler_try <- tryCatch(detect_quantity_file(scaler_dir), error = function(e) NULL)
  if (!is.null(scaler_try)) {
    return(scaler_try)
  }

  stop(
    "No reference quantity file found in either ", model_dir,
    " or ", scaler_dir,
    " (expected avg_bio or relative_depletion)"
  )
}

quantity_label_from_type <- function(quantity_type) {
  if (isTRUE(all.equal(as.numeric(quantity_type), 1))) {
    "relative_depletion"
  } else {
    "avg_bio"
  }
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

## Create scaler directory and copy all files from base_dir (inputs)
dir.create(scaler_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = scaler_dir, overwrite = TRUE, recursive = TRUE)

############################
## run likelihood profile ##
############################

par_files <- list.files(scaler_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(scaler_dir, pattern = "\\.frq$", full.names = FALSE)

if(length(par_files) > 0) {
  # Get file information
  file_info <- file.info(par_files)
  
  # Find the most recently modified file
  most_recent <- rownames(file_info)[which.max(file_info$mtime)]
  
  cat("Most recent par file:", basename(most_recent), "\n")
  cat("Modified time:", as.character(file_info[most_recent, "mtime"]), "\n")
} else {
  stop("No .par files found in directory: ", scaler_dir)
}

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
  run_commands(
    commands = ref_command,
    work_dirs = scaler_dir,
    save_log = FALSE,
    verbose = TRUE
  )
}

initial_quantity_info <- detect_reference_quantity_file(model_dir, scaler_dir)
reference_quantity <- safe_read_scalar(initial_quantity_info$path)
target_quantity <- reference_quantity * scaler / 100

# Generate profile likelihood script inside scaler directory
generate_proflike_script(Prog = program_path_abs,
                         Reps = Reps,
                         AgeFlags = AgeFlags,
                         QuantityType = QuantityType,
                         Frq = frq_file,
                         Mults = scaler,
                         Initp = basename(most_recent),
                         filename = file.path(scaler_dir, "ProfLike.sh"))

# Run in scaler directory - all output files will be created here
run_commands(commands = "./ProfLike.sh",
             work_dirs = scaler_dir,
             save_log = F,
             verbose = T)

final_quantity_info <- detect_quantity_file(scaler_dir)
actual_quantity <- safe_read_scalar(final_quantity_info$path)
target_rel_err <- suppressWarnings(abs(actual_quantity - target_quantity) / pmax(abs(target_quantity), .Machine$double.eps))

# Save profile run info
info_list <- list(
  Reps         = Reps,
  AgeFlags     = AgeFlags,
  scaler       = scaler,
  quantity_label = final_quantity_info$label,
  reference_quantity = reference_quantity,
  target_quantity = target_quantity,
  actual_quantity = actual_quantity,
  target_rel_err = target_rel_err,
  frq_file     = frq_file,
  program_path = program_path,
  model_dir    = model_dir,
  scaler_dir   = scaler_dir
)

saveRDS(
  info_list,
  file = file.path(scaler_dir, "info.rds"),
  compress = "xz"
)

profile_payload <- mp_build_profile_payload(scaler_dir)
saveRDS(
  profile_payload,
  file = file.path(scaler_dir, "profile_payload.rds"),
  compress = "xz"
)

deleted_n <- mp_cleanup_files(
  scaler_dir,
  keep = c("profile_payload.rds", "info.rds"),
  recursive = TRUE
)
cat("Cleanup removed", deleted_n, "non-core files in", scaler_dir, "\n")

cb_condor_keep_only_model_cleanup()

cat("✅ Profile likelihood completed for scaler", scaler, "\n")
