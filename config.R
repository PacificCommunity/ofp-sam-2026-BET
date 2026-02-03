#!/usr/bin/env Rscript
## MFCL Assessment Configuration - Keep it simple!

## =============================================================================
## BASIC SETTINGS
## =============================================================================

SPECIES <- "bet"
ASSESSMENT_YEAR <- 2026

## =============================================================================
## MODEL DEFINITIONS
## =============================================================================

## Define multiple models - each model can have its own settings
## Add more models as needed for sensitivity runs
MODELS <- list(
  base = list(
    name = "base",
    description = "Base case model",
    inputs_dir = "mfcl/inputs/2026",  # Input files directory
    exec_mode = "par",  # "par" or "doitall"
    mfcl_version = "2026_01_22_vsn2278",  # MFCL executable version
    # Par mode settings (used when exec_mode = "par")
    par_input = "11.par",
    par_output = "12.par",
    mfcl_switches = "-switch 1 1 1 1",
    # Doitall mode settings (used when exec_mode = "doitall")
    doitall_script = "doitall.sh"
  ),
  M1 = list(
    name = "M1", 
    description = "Sensitivity M1 - MFCL 2023",
    inputs_dir = "mfcl/inputs/2023",
    exec_mode = "doitall",
    mfcl_version = "2023",
    par_input = "11.par",
    par_output = "12.par",
    mfcl_switches = "-switch 1 1 1 1",
    doitall_script = "doitall.sh"
  ),
  M2 = list(
    name = "M2",
    description = "Sensitivity M2 - 6 regions",
    inputs_dir = "mfcl/inputs/2023_6region",
    exec_mode = "par",
    mfcl_version = "2023",
    par_input = "10.par",
    par_output = "11.par",
    mfcl_switches = "-switch 1 1 1 1",
    doitall_script = "doitall.sh"
  )
  # Add more models here as needed
)

## Default model for single runs (backward compatibility)
DEFAULT_MODEL <- "base"

## Info message about multi-model support
if(interactive()) {
  cat("\n")
  cat("Available models:\n")
  for(m in names(MODELS)) {
    cat("  -", m, ":", MODELS[[m]]$description, "\n")
    cat("     inputs:", MODELS[[m]]$inputs_dir, "\n")
    cat("     mode:", MODELS[[m]]$exec_mode, "\n")
  }
  cat("\n")
  cat("To run all models: Rscript launch.R <job_type>\n")
  cat("To run specific model: Rscript launch.R <job_type> --model=", names(MODELS)[1], "\n", sep="")
  cat("\n")
}

## =============================================================================
## PATHS
## =============================================================================

BASE_DIR <- getwd()
INPUTS_DIR <- file.path(BASE_DIR, "mfcl/inputs/2023_rep")
MFCL_EXE_DIR <- file.path(BASE_DIR, "mfcl/exe")

## Model-specific directory helper functions
get_model_dir <- function(model_name = DEFAULT_MODEL) {
  file.path(BASE_DIR, "model", model_name)
}

get_hessian_dir <- function(model_name = DEFAULT_MODEL) {
  file.path(get_model_dir(model_name), "hessian")
}

get_prof_dir <- function(model_name = DEFAULT_MODEL) {
  file.path(get_model_dir(model_name), "prof")
}

get_jitter_dir <- function(model_name = DEFAULT_MODEL) {
  file.path(get_model_dir(model_name), "jitter")
}

## Backward compatibility with single model
MODEL_DIR <- get_model_dir(DEFAULT_MODEL)
HESSIAN_DIR <- get_hessian_dir(DEFAULT_MODEL)
PROF_DIR <- get_prof_dir(DEFAULT_MODEL)
JITTER_DIR <- get_jitter_dir(DEFAULT_MODEL)

## =============================================================================
## MFCL EXECUTION
## =============================================================================

## Executable version
MFCL_VERSION <- "2026_01_22_vsn2278"  # "2026", "2023", or custom

## Execution mode: "par" or "doitall"
EXEC_MODE <- "par"  # "par" = use .par files, "doitall" = run doitall.sh

## Par mode settings (when EXEC_MODE = "par")
PAR_INPUT <- "11.par"
PAR_OUTPUT <- "12.par"
MFCL_SWITCHES <- "-switch 1 1 1 1"

## Doitall mode settings (when EXEC_MODE = "doitall")
DOITALL_SCRIPT <- "doitall.sh"

## =============================================================================
## ANALYSIS SETTINGS
## =============================================================================

## Hessian
HESSIAN_NSPLIT <- 100
HESSIAN_MEMORY <- "12GB"
HESSIAN_CPUS <- 2

## Profile
PROF_QUANTITY_TYPE <- 2  # 1 = depletion, 2 = average biomass (fixed)
PROF_START_YEAR <- 0     # 0 = entire period start
PROF_END_YEAR <- 0       # 0 = entire period end (for specific period: e.g., 150, 5)
PROF_SCALERS <- seq(50, 130, by = 10)  # Percentage multipliers
PROF_PENALTIES <- c(Pen1 = 100000, Pen2 = 1000000, Pen3 = 10000000)
PROF_REPS <- c(Reps1 = 15, Reps2 = 25, Reps3 = 25, Reps4 = 1000, Reps5 = 100, Reps6 = 500)
PROF_MEMORY <- "10GB"
PROF_CPUS <- 2

## Jitter
JITTER_NRUNS <- 50
JITTER_CV <- 0.1
JITTER_MEMORY <- "10GB"
JITTER_CPUS <- 2

## =============================================================================
## CONDOR SETTINGS
## =============================================================================

CONDOR_USER <- "kyuhank"
CONDOR_HOST <- Sys.getenv("NOU_CONDOR")
GITHUB_PAT <- Sys.getenv("GIT_PAT")
GITHUB_USERNAME <- "kyuhank"
GITHUB_ORG <- "PacificCommunity"
GITHUB_REPO <- "ofp-sam-2026-bet"

## Auto-detect current git branch
GITHUB_BRANCH <- tryCatch({
  branch <- system("git branch --show-current", intern = TRUE)
  if(length(branch) > 0 && nchar(branch) > 0) {
    branch
  } else {
    "develop_lik"  # Fallback if git command fails
  }
}, error = function(e) {
  "develop_lik"  # Fallback if not a git repo
})

DOCKER_IMAGE <- "ghcr.io/pacificcommunity/bet-2026:v1.2"
CONDOR_MEMORY <- "12GB"
CONDOR_DISK <- "10GB"
CONDOR_CPUS <- 2

## NOTE: For local testing, use makefile targets:
##   make test-model, make test-hessian, make test-prof, make test-jitter
##   or: Rscript launch.R <job_type> --local

## =============================================================================
## HELPER FUNCTIONS
## =============================================================================

## Load helper functions from separate file
if(file.exists("helpers.R")) {
  source("helpers.R")
} else if(file.exists(file.path(BASE_DIR, "helpers.R"))) {
  source(file.path(BASE_DIR, "helpers.R"))
}

## =============================================================================
## PRINT CONFIG
## =============================================================================

if(interactive() || !exists(".config_loaded")) {
  cat("==============================================\n")
  cat("MFCL Assessment:", SPECIES, ASSESSMENT_YEAR, "\n")
  cat("==============================================\n")
  cat("Execution mode:", EXEC_MODE, "\n")
  cat("MFCL version:", MFCL_VERSION, "\n")
  cat("Model directory:", MODEL_DIR, "\n")
  cat("==============================================\n\n")
  .config_loaded <- TRUE
}
