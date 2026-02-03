#!/usr/bin/env Rscript
## Launch model runs on Condor
## Can be run line-by-line in RStudio or as a script

## =============================================================================
## CONFIGURATION
## =============================================================================

## Which models to run?
model_names <- "base"           # Single model
# model_names <- c("base", "M1")  # Multiple models
# model_names <- "all"            # All models

## Remote directory (optional - auto-generated if NULL)
remote_dir <- NULL
# remote_dir <- "develop/test_run"

## Local test mode?
run_local <- FALSE  # Set to TRUE to test locally before Condor

## =============================================================================
## LOAD SYSTEM
## =============================================================================

source("config.R")

## Convert "all" to all model names
if(length(model_names) == 1 && model_names == "all") {
  model_names <- names(MODELS)
}

## Validate model names
invalid <- setdiff(model_names, names(MODELS))
if(length(invalid) > 0) {
  stop("Invalid model(s): ", paste(invalid, collapse = ", "),
       "\nAvailable: ", paste(names(MODELS), collapse = ", "))
}

## Set default remote directory
if(is.null(remote_dir)) {
  date_str <- format(Sys.Date(), "%b_%d_%Y", usetz = FALSE)
  remote_dir <- paste0("develop/", date_str, "_model")
}

## =============================================================================
## SHOW SUMMARY
## =============================================================================

cat("\n")
cat("==============================================\n")
cat("LAUNCHING MODEL RUNS\n")
cat("==============================================\n")
cat("Models:", paste(model_names, collapse = ", "), "\n")
cat("Remote dir:", remote_dir, "\n")
cat("Mode:", ifelse(run_local, "LOCAL TEST", "CONDOR"), "\n")
cat("\n")

for(m in model_names) {
  cfg <- MODELS[[m]]
  cat("Model:", m, "\n")
  cat("  Description:", cfg$description, "\n")
  cat("  Inputs:", cfg$inputs_dir, "\n")
  cat("  Exec mode:", cfg$exec_mode, "\n")
  cat("  MFCL version:", cfg$mfcl_version, "\n")
  cat("\n")
}

## =============================================================================
## LOCAL TEST (if enabled)
## =============================================================================

if(run_local) {
  cat("Running local test with first model:", model_names[1], "\n\n")
  
  Sys.setenv(MODEL_NAME = model_names[1])
  system2("Rscript", "scripts/run_model.R")
  
  cat("\n✓ Local test complete\n")
  cat("Check: model/", model_names[1], "/\n\n", sep = "")
  
  stop("Local test mode - not submitting to Condor")
}

## =============================================================================
## SUBMIT TO CONDOR
## =============================================================================

cat("Submitting to Condor...\n\n")

## Build command
cmd <- c("Rscript", "launch.R", "model", 
         paste0("--model=", paste(model_names, collapse = ",")),
         paste0("--dir=", remote_dir))

cat("Command:", paste(cmd, collapse = " "), "\n\n")

## Execute
system2(cmd[1], cmd[-1])

cat("\n")
cat("==============================================\n")
cat("Jobs submitted to Condor\n")
cat("==============================================\n\n")

cat("To fetch results later:\n")
cat("  Rscript fetch_results.R ", remote_dir, "\n", sep = "")
cat("  # or\n")
cat("  ./fetch.sh ", remote_dir, "\n\n", sep = "")

cat("To check specific model:\n")
cat("  Rscript fetch_results.R ", remote_dir, " --model=", model_names[1], "\n\n", sep = "")
