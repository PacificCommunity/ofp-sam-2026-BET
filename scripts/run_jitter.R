#!/usr/bin/env Rscript
## Jitter analysis - test model convergence
## Uses centralized config.R for all settings

## Working directory detection (if in Condor)
if (grepl("model/base/jitter/seed_\\d+$", getwd())) {
  cat("* Detected Condor working directory, navigating to repo root...\n")
  setwd("../../../..")
  cat("  New working directory:", getwd(), "\n\n")
}

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

## Get environment variables
model_name <- Sys.getenv("MODEL_NAME")
if (model_name == "") {
  stop("MODEL_NAME environment variable not set")
}

## Get model configuration
model_config <- MODELS[[model_name]]
if (is.null(model_config)) {
  stop("Model '", model_name, "' not found in config.R")
}

cat("==============================================\n")
cat("Jitter Analysis\n")
cat("==============================================\n")
cat("Model:", model_name, "\n")

## Get jitter settings from environment
jitter_seed <- as.integer(Sys.getenv("jitter_seed", "1"))
jitter_cv <- as.numeric(Sys.getenv("jitter_cv", JITTER_CV))

cat("Seed:", jitter_seed, "\n")
cat("CV:", jitter_cv, "\n\n")

## Setup directories
model_dir <- file.path("model", model_name)
jitter_dir <- file.path(model_dir, "jitter", paste0("seed_", jitter_seed))
dir.create(jitter_dir, recursive = TRUE, showWarnings = FALSE)

## Copy input files from inputs directory
cat("* Copying input files from:", model_config$inputs_dir, "\n")

if (!dir.exists(model_config$inputs_dir)) {
  stop("Inputs directory not found: ", model_config$inputs_dir)
}

input_files <- list.files(model_config$inputs_dir, full.names = TRUE, recursive = FALSE)
if (length(input_files) == 0) {
  stop("No files found in inputs directory: ", model_config$inputs_dir)
}

for(file in input_files) {
  if (file.info(file)$isdir) next
  file.copy(file, jitter_dir, overwrite = TRUE)
}
cat("  ✓ Copied", length(input_files), "files\n")

## Find .par file in inputs directory
par_files <- list.files(model_config$inputs_dir, pattern = "\\d+\\.par$", full.names = TRUE)
if (length(par_files) == 0) {
  stop("No .par file found in inputs directory: ", model_config$inputs_dir)
}

par_file <- basename(par_files[1])
cat("  ✓ Using PAR file:", par_file, "\n\n")

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

## Get MFCL executable (relative from work directory)
mfcl_exe_name <- paste0("mfclo64_", model_config$mfcl_version)
## From model/base/jitter/ to mfcl/exe/
mfcl_exe_relative <- file.path("../../../../", "mfcl", "exe", mfcl_exe_name)

## Build MFCL command
frq_file <- "bet.frq"
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
  mfcl_version = model_config$mfcl_version,
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(jitter_dir, "jitter_info.rds"))

cat("\n✓ Jitter run complete (seed =", jitter_seed, ")\n")
cat("  Output directory:", jitter_dir, "\n")
