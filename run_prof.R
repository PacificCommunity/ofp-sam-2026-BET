## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")

## environment variables
program_path <- Sys.getenv("program_path", "../../mfcl/exe/mfclo64_2026_01_22_vsn2278")
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2026")
model_dir <- Sys.getenv("model_dir", "model/base")

## Profile likelihood settings
## Single scaler value for parallel execution via condor
scaler <- as.numeric(Sys.getenv("scaler", "100"))
Reps <- as.integer(unlist(strsplit(Sys.getenv("Reps", "1 1 5 5 1 1"), "\\s+")))
names(Reps) <- paste0("Reps", 1:length(Reps))

cat("Running Profile Likelihood\n")
cat("Base directory:", base_dir, "\n")
cat("Model directory:", model_dir, "\n")
cat("Scaler:", scaler, "\n")
cat("Reps:", Reps, "\n")

## Create model directory and copy files from base_dir (inputs)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

############################
## run likelihood profile ##
############################

par_files <- list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(model_dir, pattern = "\\.frq$", full.names = FALSE)

if(length(par_files) > 0) {
  # Get file information
  file_info <- file.info(par_files)
  
  # Find the most recently modified file
  most_recent <- rownames(file_info)[which.max(file_info$mtime)]
  
  cat("Most recent par file:", basename(most_recent), "\n")
  cat("Modified time:", as.character(file_info[most_recent, "mtime"]), "\n")
} else {
  stop("No .par files found in directory: ", model_dir)
}

# Generate and run profile likelihood script for single scaler
generate_proflike_script(Prog = program_path,
                         Reps = Reps,
                         Frq = frq_file,
                         Mults = scaler,  # single scaler value
                         Initp = basename(most_recent),
                         filename = paste0(model_dir, "/ProfLike.sh"))

run_commands(commands = "./ProfLike.sh",
             work_dirs = model_dir,
             save_log = F,
             verbose = T)

# Save profile run info
info_list <- list(
  Reps         = Reps,
  scaler       = scaler,
  frq_file     = frq_file,
  program_path = program_path,
  model_dir    = model_dir
)

saveRDS(
  info_list,
  file = file.path(model_dir, paste0("prof_info_", scaler, ".rds")),
  compress = "xz"
)

cat("✅ Profile likelihood completed for scaler", scaler, "\n")
