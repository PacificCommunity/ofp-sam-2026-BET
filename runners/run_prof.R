## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")
source("tools/model_payload.R")

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

# Generate profile likelihood script inside scaler directory
generate_proflike_script(Prog = program_path_abs,
                         Reps = Reps,
                         Frq = frq_file,
                         Mults = scaler,
                         Initp = basename(most_recent),
                         filename = file.path(scaler_dir, "ProfLike.sh"))

# Run in scaler directory - all output files will be created here
run_commands(commands = "./ProfLike.sh",
             work_dirs = scaler_dir,
             save_log = F,
             verbose = T)

# Save profile run info
info_list <- list(
  Reps         = Reps,
  scaler       = scaler,
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

cat("✅ Profile likelihood completed for scaler", scaler, "\n")
