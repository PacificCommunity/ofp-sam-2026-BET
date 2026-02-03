#!/usr/bin/env Rscript
## Launch profile likelihood jobs on Condor
## Can be run line-by-line in RStudio or as a script

## =============================================================================
## CONFIGURATION
## =============================================================================

## Which models to run?
model_names <- "base"           # Single model
# model_names <- c("base", "M1")  # Multiple models
# model_names <- "all"            # All models

## Profile settings
scalers <- c(100, 90, 80, 70, 60, 50)  # Biomass scalers (% of MLE)

## Remote directory (optional - auto-generated if NULL)
remote_dir <- NULL
# remote_dir <- "develop/test_prof"

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

## Check that models have par files
models_without_par <- c()
for(m in model_names) {
  model_dir <- get_model_dir(m)
  par_files <- list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
  if(length(par_files) == 0) {
    models_without_par <- c(models_without_par, m)
  }
}

if(length(models_without_par) > 0) {
  stop("Models missing .par files: ", paste(models_without_par, collapse = ", "),
       "\nRun model first before profile")
}

## Set default remote directory
if(is.null(remote_dir)) {
  date_str <- format(Sys.Date(), "%b_%d_%Y", usetz = FALSE)
  remote_dir <- paste0("develop/", date_str, "_prof")
}

## =============================================================================
## SHOW SUMMARY
## =============================================================================

cat("\n")
cat("==============================================\n")
cat("LAUNCHING PROFILE LIKELIHOOD JOBS\n")
cat("==============================================\n")
cat("Models:", paste(model_names, collapse = ", "), "\n")
cat("Scalers:", paste(scalers, collapse = ", "), "\n")
cat("Total jobs:", length(model_names) * length(scalers), "\n")
cat("Remote dir:", remote_dir, "\n")
cat("Mode:", ifelse(run_local, "LOCAL TEST", "CONDOR"), "\n")
cat("\n")

## =============================================================================
## LOCAL TEST (if enabled)
## =============================================================================

if(run_local) {
  cat("Running local test: model", model_names[1], "scaler", scalers[1], "\n\n")
  
  Sys.setenv(MODEL_NAME = model_names[1],
             scaler = as.character(scalers[1]),
             PROF_START_YEAR = as.character(PROF_START_YEAR),
             PROF_END_YEAR = as.character(PROF_END_YEAR))
  system2("Rscript", "scripts/run_prof.R")
  
  cat("\n✓ Local test complete\n")
  cat("Check: model/", model_names[1], "/prof/\n\n", sep = "")
  
  stop("Local test mode - not submitting to Condor")
}

## =============================================================================
## SUBMIT TO CONDOR
## =============================================================================

cat("Submitting to Condor...\n\n")

## Build command
cmd <- c("Rscript", "launch.R", "prof",
         paste0("--model=", paste(model_names, collapse = ",")),
         paste0("--scalers=", paste(scalers, collapse = ",")),
         paste0("--dir=", remote_dir))

cat("Command:", paste(cmd, collapse = " "), "\n\n")

## Execute
system2(cmd[1], cmd[-1])

cat("\n")
cat("==============================================\n")
cat("Jobs submitted to Condor\n")
cat("==============================================\n\n")

cat("To fetch results later:\n")
cat("  Rscript fetch_results.R ", remote_dir, " --job=prof\n", sep = "")
cat("  # or\n")
cat("  ./fetch.sh ", remote_dir, " --job=prof\n\n", sep = "")
