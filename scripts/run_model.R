#!/usr/bin/env Rscript
## Run MFCL model
## Uses centralized config.R for all settings

## Detect if running in Condor (working directory is model/xxx/)
## If so, navigate to repo root
current_dir <- getwd()
if(basename(dirname(current_dir)) == "model") {
  ## In Condor: working dir is model/base/ or similar
  ## Navigate to repo root (two levels up)
  setwd("../..")
  cat("Detected Condor environment, changed to:", getwd(), "\n")
}

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

cat("==============================================\n")
cat("MFCL Model Run\n")
cat("==============================================\n")

## Get model name from environment (for multi-model support)
model_name <- Sys.getenv("MODEL_NAME", DEFAULT_MODEL)

## Get model configuration
if(!(model_name %in% names(MODELS))) {
  stop("Unknown model: ", model_name, 
       "\nAvailable models: ", paste(names(MODELS), collapse = ", "))
}
model_config <- MODELS[[model_name]]

## Get model directory from environment variable (archive method)
## This allows makefile to override the directory
model_dir <- Sys.getenv("model_dir", paste0("model/", model_name))
MODEL_DIR <- model_dir

cat("Model name:", model_name, "\n")
cat("Description:", model_config$description, "\n")
cat("Model directory:", MODEL_DIR, "\n")
cat("Inputs:", model_config$inputs_dir, "\n")
cat("Execution mode:", model_config$exec_mode, "\n")
cat("MFCL version:", model_config$mfcl_version, "\n")
cat("Working directory:", getwd(), "\n\n")

## Create model directory
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

## Copy input files
cat("* Copying input files ...\n")

## Get base_dir from environment or use model config
base_dir <- Sys.getenv("base_dir", model_config$inputs_dir)
## Convert to absolute path if relative
if(!grepl("^/", base_dir)) {
  base_dir <- file.path(getwd(), base_dir)
}

cat("  Base directory:", base_dir, "\n")

## Check if directory exists
if(!dir.exists(base_dir)) {
  stop("Input directory does not exist: ", base_dir, 
       "\nCheck inputs_dir in config.R for model: ", model_name)
}

## Copy all files from base_dir
files_to_copy <- list.files(base_dir, full.names = TRUE)
if(length(files_to_copy) == 0) {
  stop("Input directory is empty: ", base_dir,
       "\nCheck inputs_dir in config.R for model: ", model_name)
}

cat("  Files to copy:", length(files_to_copy), "\n")
copied <- file.copy(files_to_copy, to = MODEL_DIR, overwrite = TRUE, recursive = TRUE)
cat("  ✓ Copied", sum(copied), "files\n")

## Verify PAR file was copied (for par mode)
if(model_config$exec_mode == "par") {
  par_file <- file.path(MODEL_DIR, model_config$par_input)
  if(!file.exists(par_file)) {
    stop("PAR file not found after copy: ", model_config$par_input,
         "\nExpected in: ", base_dir,
         "\nMake sure ", model_config$par_input, " exists in inputs directory")
  }
  cat("  ✓ PAR file verified:", model_config$par_input, "\n")
}

cat("\n")

## Get execution mode from model config
exec_mode <- model_config$exec_mode

## Get MFCL executable path (relative from model directory)
mfcl_exe_name <- paste0("mfclo64_", model_config$mfcl_version)
mfcl_exe_relative <- paste0("../../mfcl/exe/", mfcl_exe_name)

if(exec_mode == "doitall") {
  ## =========================================================================
  ## DOITALL MODE
  ## =========================================================================
  
  cat("\n* Running in DOITALL mode ...\n")
  
  ## Doitall script should already be copied from base_dir
  doitall_path <- file.path(MODEL_DIR, model_config$doitall_script)
  
  if(!file.exists(doitall_path)) {
    stop("Doitall script not found: ", doitall_path)
  }
  
  ## Make executable
  Sys.chmod(doitall_path, mode = "0755")
  
  cat("  Script:", model_config$doitall_script, "\n")
  cat("  Working directory:", MODEL_DIR, "\n\n")
  
  ## Set PROGRAM_PATH environment variable for doitall script
  Sys.setenv(PROGRAM_PATH = mfcl_exe_relative)
  
  ## Run doitall.sh
  run_commands(
    commands = paste0("./", model_config$doitall_script),
    work_dirs = MODEL_DIR,
    save_log = TRUE,
    parallel = FALSE,
    verbose = TRUE,
    log_file = file.path(MODEL_DIR, "mfcl_log.txt")
  )
  
  ## Save run info
  info_list <- list(
    species = SPECIES,
    model_name = model_name,
    description = model_config$description,
    exec_mode = "doitall",
    script = model_config$doitall_script,
    mfcl_version = model_config$mfcl_version,
    inputs_dir = model_config$inputs_dir,
    timestamp = Sys.time(),
    host = Sys.info()["nodename"]
  )
  
} else {
  ## =========================================================================
  ## PAR MODE
  ## =========================================================================
  
  cat("\n* Running in PAR mode ...\n")
  
  ## Get switches from model config
  switches <- model_config$mfcl_switches
  
  ## Build command (like archive: ../../mfcl/exe/... frq par out switches)
  frq_file <- "bet.frq"  # Use basename directly
  mfcl_command <- paste(
    mfcl_exe_relative,
    frq_file,
    model_config$par_input,
    model_config$par_output,
    switches
  )
  
  cat("  Executable:", mfcl_exe_relative, "\n")
  cat("  Input PAR:", model_config$par_input, "\n")
  cat("  Output PAR:", model_config$par_output, "\n")
  cat("  Switches:", switches, "\n")
  cat("  Command:", mfcl_command, "\n")
  cat("  Working directory:", MODEL_DIR, "\n\n")
  
  ## Run MFCL
  run_commands(
    commands = mfcl_command,
    work_dirs = MODEL_DIR,
    save_log = TRUE,
    parallel = FALSE,
    verbose = TRUE,
    log_file = file.path(MODEL_DIR, "mfcl_log.txt")
  )
  
  ## Save run info
  info_list <- list(
    species = SPECIES,
    model_name = model_name,
    description = model_config$description,
    exec_mode = "par",
    mfcl_version = model_config$mfcl_version,
    inputs_dir = model_config$inputs_dir,
    par_input = model_config$par_input,
    par_output = model_config$par_output,
    switches = switches,
    command = mfcl_command,
    timestamp = Sys.time(),
    host = Sys.info()["nodename"]
  )
}

saveRDS(info_list, file = file.path(MODEL_DIR, "model_info.rds"))

cat("\n✓ Model run complete\n")
cat("  Output directory:", MODEL_DIR, "\n")
cat("  Log file:", file.path(MODEL_DIR, "mfcl_log.txt"), "\n")
cat("  Info saved:", file.path(MODEL_DIR, "model_info.rds"), "\n")
