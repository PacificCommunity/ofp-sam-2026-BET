#!/usr/bin/env Rscript
## Fetch results from Condor

## =============================================================================
## LOAD CONFIGURATION
## =============================================================================

source("config.R")
library(CondorBox)

## =============================================================================
## PARSE ARGUMENTS
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  cat("\nUsage: Rscript fetch_results.R <remote_dir> [options]\n\n")
  cat("Arguments:\n")
  cat("  <remote_dir>        Remote directory to fetch from (e.g., develop/Feb_03_2026_model)\n\n")
  cat("Options:\n")
  cat("  --local-dir=<path>  Local directory to save results (default: model/)\n")
  cat("  --model=<name>      Fetch specific model(s): 'all', 'base', 'base,M1' (default: all)\n")
  cat("  --no-extract        Don't extract tar.gz archives\n\n")
  cat("Examples:\n")
  cat("  # Fetch all models from a run\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_model\n\n")
  cat("  # Fetch specific model\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_model --model=base\n\n")
  cat("  # Fetch to custom local directory\n")
  cat("  Rscript fetch_results.R develop/run01_base --local-dir=results/run01\n\n")
  cat("  # Fetch without extracting archives\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_model --no-extract\n\n")
  quit(status = 1)
}

remote_dir <- args[1]
local_dir <- "model"
model_names <- names(MODELS)  # Default: all models
extract_archive <- TRUE

## Parse options
for(arg in args[-1]) {
  if(grepl("^--local-dir=", arg)) {
    local_dir <- sub("^--local-dir=", "", arg)
  } else if(grepl("^--model=", arg)) {
    model_str <- sub("^--model=", "", arg)
    if(model_str == "all") {
      model_names <- names(MODELS)
    } else {
      model_names <- strsplit(model_str, ",")[[1]]
      model_names <- trimws(model_names)
      # Validate model names
      invalid <- setdiff(model_names, names(MODELS))
      if(length(invalid) > 0) {
        stop("Invalid model(s): ", paste(invalid, collapse = ", "), 
             "\nAvailable models: ", paste(names(MODELS), collapse = ", "))
      }
    }
  } else if(arg == "--no-extract") {
    extract_archive <- FALSE
  }
}

## =============================================================================
## FETCH RESULTS
## =============================================================================

cat("==============================================\n")
cat("Fetching results from Condor\n")
cat("==============================================\n")
cat("Remote dir:", remote_dir, "\n")
cat("Local dir:", local_dir, "\n")
cat("Models:", paste(model_names, collapse = ", "), "\n")
cat("Extract archives:", extract_archive, "\n\n")

## Create local directory if needed
if(!dir.exists(local_dir)) {
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
}

## Fetch each model
for(model_name in model_names) {
  cat("\n--- Fetching model:", model_name, "---\n")
  
  remote_model_dir <- paste0(GITHUB_REPO, "/", remote_dir, "/", model_name)
  local_model_dir <- file.path(local_dir, model_name)
  
  tryCatch({
    BatchFileHandler(
      remote_user = CONDOR_USER,
      remote_host = CONDOR_HOST,
      folder_name = remote_model_dir,
      action = "fetch",
      fetch_dir = local_model_dir,
      extract_archive = extract_archive
    )
    cat("✓ Successfully fetched:", model_name, "\n")
  }, error = function(e) {
    cat("✗ Error fetching", model_name, ":", conditionMessage(e), "\n")
  })
}

cat("\n==============================================\n")
cat("Fetch complete\n")
cat("==============================================\n\n")

cat("Results saved to:", local_dir, "\n")
cat("Check the following directories:\n")
for(model_name in model_names) {
  cat("  -", file.path(local_dir, model_name), "\n")
}
cat("\n")
