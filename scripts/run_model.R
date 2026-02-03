#!/usr/bin/env Rscript
## Run MFCL model
## Uses centralized config.R for all settings

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

cat("==============================================\n")
cat("MFCL Model Run\n")
cat("==============================================\n")

## Get model name from environment (for multi-model support)
model_name <- Sys.getenv("MODEL_NAME", DEFAULT_MODEL)

## Get model directory from environment variable (archive method)
## This allows makefile to override the directory
model_dir <- Sys.getenv("model_dir", paste0("model/", model_name))
MODEL_DIR <- model_dir

cat("Model name:", model_name, "\n")
cat("Model directory:", MODEL_DIR, "\n\n")

## Create model directory
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

## Copy input files
cat("* Copying input files ...\n")

## Get base_dir from environment or use config (archive method)
base_dir <- Sys.getenv("base_dir", INPUTS_DIR)
## Convert to absolute path
if(!grepl("^/", base_dir)) {
  base_dir <- file.path(getwd(), base_dir)
}

cat("  Base directory:", base_dir, "\n")

## Copy all files from base_dir
files_to_copy <- list.files(base_dir, full.names = TRUE)
file.copy(files_to_copy, to = MODEL_DIR, overwrite = TRUE, recursive = TRUE)
cat("  ✓ Copied", length(files_to_copy), "files\n\n")

## Get execution mode from config
exec_mode <- Sys.getenv("EXEC_MODE", EXEC_MODE)

## Copy PAR file if in par mode
if(exec_mode == "par") {
  ## Try to find par file in base_dir or use PAR_INPUT from config
  par_source <- file.path(base_dir, PAR_INPUT)
  if(!file.exists(par_source)) {
    par_source <- file.path(INPUTS_DIR, PAR_INPUT)
  }
  if(file.exists(par_source)) {
    file.copy(par_source, MODEL_DIR, overwrite = TRUE)
    cat("  ✓", PAR_INPUT, "\n")
  } else {
    cat("  Note: PAR file not found, may already be copied\n")
  }
}

if(exec_mode == "doitall") {
  ## =========================================================================
  ## DOITALL MODE
  ## =========================================================================
  
  cat("\n* Running in DOITALL mode ...\n")
  
  ## Copy doitall script
  doitall_source <- file.path(INPUTS_DIR, DOITALL_SCRIPT)
  if(!file.exists(doitall_source)) {
    stop("Doitall script not found: ", doitall_source)
  }
  
  file.copy(doitall_source, MODEL_DIR, overwrite = TRUE)
  doitall_path <- file.path(MODEL_DIR, DOITALL_SCRIPT)
  
  ## Make executable
  Sys.chmod(doitall_path, mode = "0755")
  
  cat("  Script:", DOITALL_SCRIPT, "\n")
  cat("  Working directory:", MODEL_DIR, "\n\n")
  
  ## Run doitall.sh
  run_commands(
    commands = paste0("./", DOITALL_SCRIPT),
    work_dirs = MODEL_DIR,
    save_log = TRUE,
    parallel = FALSE,
    verbose = TRUE,
    log_file = file.path(MODEL_DIR, "mfcl_log.txt")
  )
  
  ## Save run info
  info_list <- list(
    species = SPECIES,
    exec_mode = "doitall",
    script = DOITALL_SCRIPT,
    timestamp = Sys.time(),
    host = Sys.info()["nodename"]
  )
  
} else {
  ## =========================================================================
  ## PAR MODE
  ## =========================================================================
  
  cat("\n* Running in PAR mode ...\n")
  
  ## Get MFCL executable path
  ## In Condor: work_dir is model/base/, so use ../../mfcl/exe/...
  ## Archive method: always use relative path from model directory
  if(MFCL_VERSION == "2026") {
    mfcl_exe <- "mfclo64_2026_01_22_vsn2278"
  } else if(MFCL_VERSION == "2023") {
    mfcl_exe <- "mfclo64_2023"
  } else {
    mfcl_exe <- paste0("mfclo64_", MFCL_VERSION)
  }
  
  ## Path relative to model directory (model/base/)
  program_path <- file.path("mfcl/exe", mfcl_exe)
  mfcl_exe_relative <- file.path("../..", program_path)
  
  ## Get switc"bet.frq"  # Use basename directlyonfig
  switches <- Sys.getenv("MFCL_SWITCHES", MFCL_SWITCHES)
  
  ## Build command (like archive: ../../mfcl/exe/... frq par out switches)
  frq_file <- basename(input_files$frq)
  mfcl_command <- paste(
    mfcl_exe_relative,
    frq_file,
    PAR_INPUT,
    PAR_OUTPUT,
    switches
  )
  
  cat("  Executable:", mfcl_exe_relative, "\n")
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
    exec_mode = "par",
    mfcl_version = sub("mfclo64_", "", basename(mfcl_exe)),
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
