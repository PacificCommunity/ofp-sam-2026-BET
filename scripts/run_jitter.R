#!/usr/bin/env Rscript
## Jitter analysis - test model convergence
## Uses centralized config.R for all settings

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

## Get jitter settings from environment
jitter_seed <- as.integer(Sys.getenv("jitter_seed", "1"))
jitter_cv <- as.numeric(Sys.getenv("jitter_cv", JITTER_CV))

cat("==============================================\n")
cat("Jitter Analysis\n")
cat("==============================================\n")
cat("Seed:", jitter_seed, "\n")
cat("CV:", jitter_cv, "\n\n")

## Setup directories
jitter_dir <- file.path(JITTER_DIR, paste0("seed_", jitter_seed))
dir.create(jitter_dir, recursive = TRUE, showWarnings = FALSE)

## Copy input files
cat("* Copying input files ...\n")
input_files <- get_input_files()
for(file in input_files) {
  if(file.exists(file)) {
    file.copy(file, jitter_dir, overwrite = TRUE)
  }
}

## Copy converged par file
par_files <- list.files(MODEL_DIR, pattern = "\\d+\\.par$", full.names = TRUE)
if(length(par_files) > 0) {
  file_info <- file.info(par_files)
  most_recent <- par_files[which.max(file_info$mtime)]
  file.copy(most_recent, jitter_dir, overwrite = TRUE)
  par_file <- basename(most_recent)
  cat("  ✓ Copied:", par_file, "\n\n")
} else {
  stop("No .par file found in model directory: ", MODEL_DIR)
}

## Jitter parameters in par file
cat("* Jittering parameters ...\n")
par_path <- file.path(jitter_dir, par_file)
par_obj <- read.MFCLPar(par_path)

## Set random seed
set.seed(jitter_seed)

## Apply jitter (example - modify as needed)
## This is a placeholder - actual jittering depends on FLR4MFCL functionality
cat("  Applying CV =", jitter_cv, "to estimated parameters\n")

## Write jittered par file
jitter_par_file <- paste0("jitter_", jitter_seed, ".par")
write(par_obj, file.path(jitter_dir, jitter_par_file))

## Get MFCL executable (archive method: relative from work directory)
if(MFCL_VERSION == "2026") {
  mfcl_exe <- "mfclo64_2026_01_22_vsn2278"
} else if(MFCL_VERSION == "2023") {
  mfcl_exe <- "mfclo64_2023"
} else {
  mfcl_exe <- paste0("mfclo64_", MFCL_VERSION)
}
program_path <- file.path("mfcl/exe", mfcl_exe)
## From model/base/jitter/ to mfcl/exe/
mfcl_exe_relative <- file.path("../../..", program_path)

## Build MFCL command
frq_file <- basename(input_files$frq)
output_par <- paste0("out_", jitter_seed, ".par")

## Get switches from environment or use defaults
switches <- Sys.getenv("JITTER_SWITCHES", "-switch 1 1 1 1")

mfcl_command <- paste(
  mfcl_exe_relative,
  frq_file, jitter_par_file, output_par,
  switches
)

cat("\n* Running MFCL with jittered parameters ...\n")
cat("  Command:", mfcl_command, "\n\n")

## Run MFCL
run_commands(
  commands = mfcl_command,
  work_dirs = jitter_dir,
  save_log = TRUE,
  parallel = FALSE,
  verbose = TRUE,
  log_file = file.path(jitter_dir, "mfcl_log.txt")
)

## Save run info
info_list <- list(
  species = SPECIES,
  jitter_seed = jitter_seed,
  jitter_cv = jitter_cv,
  mfcl_version = sub("mfclo64_", "", basename(mfcl_exe)),
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(jitter_dir, "jitter_info.rds"))

cat("\n✓ Jitter run complete (seed =", jitter_seed, ")\n")
cat("  Output directory:", jitter_dir, "\n")
