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
  cat("  --job=<type>        Job type(s) to fetch: 'model', 'hessian', 'prof', 'jitter', or 'all' (default: model)\n")
  cat("  --parts=<n>         Number of hessian parts (default: 200)\n")
  cat("  --scalers=<list>    Profile scalers to fetch, comma-separated (default: 100,90,80,70,60,50)\n")
  cat("  --njitter=<n>       Number of jitter runs (default: 100)\n")
  cat("  --no-extract        Don't extract tar.gz archives\n\n")
  cat("Examples:\n")
  cat("  # Fetch model results only\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_model\n\n")
  cat("  # Fetch hessian results\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_hessian --job=hessian --parts=200\n\n")
  cat("  # Fetch profile results\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_prof --job=prof\n\n")
  cat("  # Fetch all job types\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_all --job=all\n\n")
  cat("  # Fetch specific model only\n")
  cat("  Rscript fetch_results.R develop/Feb_03_2026_model --model=base\n\n")
  quit(status = 1)
}

remote_dir <- args[1]
local_dir <- "model"
model_names <- names(MODELS)  # Default: all models
job_types <- c("model")  # Default: model only
extract_archive <- TRUE
nsplit <- 200  # Default hessian parts
scalers_vec <- c(100, 90, 80, 70, 60, 50)  # Default profile scalers
njitter <- 100  # Default jitter runs

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
  } else if(grepl("^--job=", arg)) {
    job_str <- sub("^--job=", "", arg)
    if(job_str == "all") {
      job_types <- c("model", "hessian", "prof", "jitter")
    } else {
      job_types <- strsplit(job_str, ",")[[1]]
      job_types <- trimws(job_types)
      # Validate job types
      valid_jobs <- c("model", "hessian", "prof", "jitter")
      invalid <- setdiff(job_types, valid_jobs)
      if(length(invalid) > 0) {
        stop("Invalid job type(s): ", paste(invalid, collapse = ", "), 
             "\nValid types: ", paste(valid_jobs, collapse = ", "))
      }
    }
  } else if(grepl("^--parts=", arg)) {
    nsplit <- as.integer(sub("^--parts=", "", arg))
  } else if(grepl("^--scalers=", arg)) {
    scalers_str <- sub("^--scalers=", "", arg)
    scalers_vec <- as.numeric(strsplit(scalers_str, ",")[[1]])
  } else if(grepl("^--njitter=", arg)) {
    njitter <- as.integer(sub("^--njitter=", "", arg))
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
cat("Job types:", paste(job_types, collapse = ", "), "\n")
cat("Extract archives:", extract_archive, "\n\n")

## Create local directory if needed
if(!dir.exists(local_dir)) {
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
}

## Helper function to fetch a single directory
fetch_directory <- function(remote_path, local_base_dir) {
  tryCatch({
    BatchFileHandler(
      remote_user = CONDOR_USER,
      remote_host = CONDOR_HOST,
      folder_name = remote_path,
      action = "fetch",
      fetch_dir = local_base_dir,
      extract_archive = extract_archive,
      direct_extract = TRUE,
      archive_name = "output_archive.tar.gz",
      extract_folder = paste0(GITHUB_REPO, "/", local_base_dir)
    )
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error:", conditionMessage(e), "\n")
    return(FALSE)
  })
}

## =============================================================================
## FETCH MODEL RESULTS
## =============================================================================

if("model" %in% job_types) {
  cat("\n========== FETCHING MODEL RESULTS ==========\n")
  
  for(model_name in model_names) {
    cat("\n--- Model:", model_name, "---\n")
    
    remote_model_dir <- paste0(GITHUB_REPO, "/", remote_dir, "/", model_name)
    
    if(fetch_directory(remote_model_dir, local_dir)) {
      cat("✓ Successfully fetched:", model_name, "\n")
    }
  }
}

## =============================================================================
## FETCH HESSIAN RESULTS
## =============================================================================

if("hessian" %in% job_types) {
  cat("\n========== FETCHING HESSIAN RESULTS ==========\n")
  cat("Parts per model:", nsplit, "\n")
  
  for(model_name in model_names) {
    cat("\n--- Model:", model_name, "---\n")
    
    success_count <- 0
    for(part in 1:nsplit) {
      remote_hess_dir <- paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_part", part)
      
      if(fetch_directory(remote_hess_dir, local_dir)) {
        success_count <- success_count + 1
      }
      
      if(part %% 20 == 0) {
        cat("  Fetched", success_count, "/", part, "parts\n")
      }
    }
    
    cat("✓ Completed:", success_count, "/", nsplit, "parts for", model_name, "\n")
  }
}

## =============================================================================
## FETCH PROFILE RESULTS
## =============================================================================

if("prof" %in% job_types) {
  cat("\n========== FETCHING PROFILE RESULTS ==========\n")
  cat("Scalers:", paste(scalers_vec, collapse = ", "), "\n")
  
  for(model_name in model_names) {
    cat("\n--- Model:", model_name, "---\n")
    
    success_count <- 0
    for(sc in scalers_vec) {
      remote_prof_dir <- paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_sc", sc)
      
      if(fetch_directory(remote_prof_dir, local_dir)) {
        success_count <- success_count + 1
      }
    }
    
    cat("✓ Completed:", success_count, "/", length(scalers_vec), "scalers for", model_name, "\n")
  }
}

## =============================================================================
## FETCH JITTER RESULTS
## =============================================================================

if("jitter" %in% job_types) {
  cat("\n========== FETCHING JITTER RESULTS ==========\n")
  cat("Runs per model:", njitter, "\n")
  
  for(model_name in model_names) {
    cat("\n--- Model:", model_name, "---\n")
    
    success_count <- 0
    for(seed in 1:njitter) {
      remote_jitter_dir <- paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_seed", seed)
      
      if(fetch_directory(remote_jitter_dir, local_dir)) {
        success_count <- success_count + 1
      }
      
      if(seed %% 10 == 0) {
        cat("  Fetched", success_count, "/", seed, "runs\n")
      }
    }
    
    cat("✓ Completed:", success_count, "/", njitter, "runs for", model_name, "\n")
  }
}

cat("\n==============================================\n")
cat("Fetch complete\n")
cat("==============================================\n\n")

cat("Results saved to:", local_dir, "\n")
cat("Check the following directories:\n")
for(model_name in model_names) {
  if("model" %in% job_types) {
    cat("  -", file.path(local_dir, model_name), "\n")
  }
  if("hessian" %in% job_types) {
    cat("  -", file.path(local_dir, model_name, "hessian"), "\n")
  }
  if("prof" %in% job_types) {
    cat("  -", file.path(local_dir, model_name, "prof"), "\n")
  }
  if("jitter" %in% job_types) {
    cat("  -", file.path(local_dir, model_name, "jitter"), "\n")
  }
}
cat("\n")
