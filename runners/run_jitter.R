## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/jitter.R")
source("tools/model_payload.R")
source("tools/condor_archive_cleanup.R")


## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

## Jitter settings
## Single seed value for parallel execution via condor
jitter_seed <- as.integer(Sys.getenv("jitter_seed", "11"))
jitter_amount <- as.numeric(Sys.getenv("jitter_amount", "0.1"))

## Create jitter-specific directory inside jitter folder
jitter_dir <- file.path(model_dir, "jitter")
seed_dir <- file.path(jitter_dir, paste0("jitter_seed_", jitter_seed))

cat("Running Jitter Analysis\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Jitter directory:", jitter_dir, "\n")
cat("Seed directory:", seed_dir, "\n")
cat("Jitter seed:", jitter_seed, "\n")
cat("Jitter amount:", jitter_amount, "\n")

## Create seed directory and copy all files from base_dir (inputs)
dir.create(seed_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = seed_dir, overwrite = TRUE, recursive = TRUE)

## Also copy par file from model_dir (converged model)
model_dir_abs <- file.path(project_root, model_dir)
par_in_model <- list.files(model_dir_abs, pattern = "\\.par$", full.names = TRUE)
if(length(par_in_model) > 0) {
  file.copy(par_in_model, to = seed_dir, overwrite = TRUE)
  cat("Copied par files from model directory\n")
}

############################
## Generate jittered par  ##
############################

par_files <- list.files(seed_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(seed_dir, pattern = "\\.frq$", full.names = FALSE)

if(length(par_files) > 0) {
  # Get file information
  file_info <- file.info(par_files)
  
  # Find the most recently modified file
  most_recent <- rownames(file_info)[which.max(file_info$mtime)]
  
  cat("Most recent par file:", basename(most_recent), "\n")
  cat("Modified time:", as.character(file_info[most_recent, "mtime"]), "\n")
} else {
  stop("No .par files found in directory: ", seed_dir)
}

## Read par file and apply jitter
jittered_par_name <- paste0("jittered_", jitter_seed, ".par")
indepvar_in_seed <- file.path(seed_dir, "indepvar.rpt")
jitter_output_prefix <- file.path(seed_dir, paste0("jitter_seed_", jitter_seed))

run_exact_jitter(
  model_dir = seed_dir,
  jitter_bound = as.numeric(jitter_amount),
  seed = as.numeric(jitter_seed),
  base_par_file = most_recent,
  indepvar_file = indepvar_in_seed,
  out_file = file.path(seed_dir, jittered_par_name),
  output_prefix = jitter_output_prefix,
  change_tol = 1e-14
)

cat("Jittered par file written:", jittered_par_name, "\n")

##############
## run MFCL ##
##############

defaultswitch <- paste("-switch 1",
                       "1 1 5",
                       sep=" ")

output_par_name <- paste0("jittered_out_", jitter_seed, ".par")
mfcl_commands <- paste0("../../../../", program_path, " ", frq_file, " ", jittered_par_name, " ", output_par_name, " ", defaultswitch)

cat("Running MFCL with commands:", mfcl_commands, "\n")

run_commands(commands = mfcl_commands,
             work_dirs = seed_dir, 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = file.path(seed_dir, "mfcl_log.txt"))

# Save jitter run info
info_list <- list(
  jitter_seed   = jitter_seed,
  jitter_amount = jitter_amount,
  frq_file      = frq_file,
  program_path  = program_path,
  model_dir     = model_dir,
  seed_dir      = seed_dir,
  input_par     = basename(most_recent),
  jittered_par  = jittered_par_name
)

saveRDS(
  info_list,
  file = file.path(seed_dir, "jitter_info.rds"),
  compress = "xz"
)

jitter_payload <- mp_build_jitter_payload(seed_dir, jitter_seed)
saveRDS(
  jitter_payload,
  file = file.path(seed_dir, "jitter_result.rds"),
  compress = "xz"
)

deleted_n <- mp_cleanup_files(
  seed_dir,
  keep = c(
    "jitter_result.rds",
    "jitter_info.rds",
    jittered_par_name,
    basename(sprintf("%s_label_changes.csv", jitter_output_prefix)),
    basename(sprintf("%s_summary.csv", jitter_output_prefix)),
    basename(sprintf("%s_summary.pdf", jitter_output_prefix))
  ),
  recursive = TRUE
)
cat("Cleanup removed", deleted_n, "non-core files in", seed_dir, "\n")

cb_condor_keep_only_model_cleanup()

cat("✅ Jitter run completed for seed", jitter_seed, "\n")
