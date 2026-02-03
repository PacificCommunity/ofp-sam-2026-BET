#!/usr/bin/env Rscript
## Fetch results from Condor
## Can be run line-by-line in RStudio or as a script

## =============================================================================
## CONFIGURATION
## =============================================================================

## Remote directory to fetch from
remote_dir <- "develop/Feb_03_2026_model"

## Which models to fetch?
model_names <- "all"            # All models
# model_names <- "base"           # Single model
# model_names <- c("base", "M1")  # Multiple models

## Which job types to fetch?
job_types <- "model"            # Model results only
# job_types <- "hessian"          # Hessian results
# job_types <- "prof"             # Profile results
# job_types <- "jitter"           # Jitter results
# job_types <- "all"              # All job types

## Local directory to save to
local_dir <- "model"

## Extract archives?
extract_archive <- TRUE

## Job-specific settings (only used if fetching that job type)
nsplit <- 200                   # Hessian parts
scalers <- c(100, 90, 80, 70, 60, 50)  # Profile scalers
njitter <- 100                  # Jitter runs

## =============================================================================
## LOAD SYSTEM AND VALIDATE
## =============================================================================

source("config.R")
library(CondorBox)

cat("\n")
cat("==============================================\n")
cat("FETCHING RESULTS FROM CONDOR\n")
cat("==============================================\n")
cat("Remote dir:", remote_dir, "\n")
cat("Models:", model_names, "\n")
cat("Job types:", job_types, "\n")
cat("Local dir:", local_dir, "\n")
cat("\n")

## =============================================================================
## BUILD AND EXECUTE COMMAND
## =============================================================================

cmd <- c("Rscript", "fetch_results.R", remote_dir,
         paste0("--model=", ifelse(is.character(model_names) && length(model_names) == 1, 
                                   model_names, 
                                   paste(model_names, collapse = ","))),
         paste0("--job=", job_types),
         paste0("--local-dir=", local_dir))

## Add job-specific parameters
if(grepl("hessian", job_types)) {
  cmd <- c(cmd, paste0("--parts=", nsplit))
}
if(grepl("prof", job_types)) {
  cmd <- c(cmd, paste0("--scalers=", paste(scalers, collapse = ",")))
}
if(grepl("jitter", job_types)) {
  cmd <- c(cmd, paste0("--njitter=", njitter))
}

if(!extract_archive) {
  cmd <- c(cmd, "--no-extract")
}

cat("Command:", paste(cmd, collapse = " "), "\n\n")

## Execute
system2(cmd[1], cmd[-1])

cat("\n")
cat("==============================================\n")
cat("Fetch complete\n")
cat("==============================================\n\n")

cat("Results saved to:", local_dir, "\n")
if(model_names == "all") {
  cat("Check: model/base/, model/M1/, model/M2/\n\n")
} else {
  cat("Check:", file.path(local_dir, model_names), "\n\n")
}
