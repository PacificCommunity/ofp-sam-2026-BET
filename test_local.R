#!/usr/bin/env Rscript
## Quick local testing script
## Uses settings from config.R

args <- commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  cat("\nUsage: Rscript test_local.R [model|hessian|prof|jitter]\n\n")
  cat("Examples:\n")
  cat("  Rscript test_local.R model\n")
  cat("  Rscript test_local.R hessian\n")
  cat("  Rscript test_local.R prof [scaler]\n")
  cat("  Rscript test_local.R jitter [seed]\n\n")
  quit(save = "no", status = 1)
}

job_type <- args[1]

source("config.R")

cat("==============================================\n")
cat("Local Test:", toupper(job_type), "\n")
cat("==============================================\n\n")

if(job_type == "model") {
  cat("* Running model (", EXEC_MODE, " mode)...\n", sep="")
  Sys.setenv(EXEC_MODE = EXEC_MODE)
  system2("Rscript", c("scripts/run_model.R"))
  
} else if(job_type == "hessian") {
  part <- if(length(args) > 1) args[2] else LOCAL_TEST_HESSIAN_PART
  cat("* Running Hessian part", part, "of", HESSIAN_NSPLIT, "...\n")
  Sys.setenv(hessian_part = as.character(part), nsplit = as.character(HESSIAN_NSPLIT))
  system2("Rscript", c("scripts/run_hessian.R"))
  
} else if(job_type == "prof") {
  scaler <- if(length(args) > 1) args[2] else LOCAL_TEST_PROF_SCALER
  cat("* Running profile with scaler", scaler, "%...\n")
  Sys.setenv(
    scaler = as.character(scaler),
    PROF_START_YEAR = as.character(PROF_START_YEAR),
    PROF_END_YEAR = as.character(PROF_END_YEAR)
  )
  system2("Rscript", c("scripts/run_prof.R"))
  
} else if(job_type == "jitter") {
  cat("* Running jitter with seed 1...\n")
  Sys.setenv(jitter_seed = "1", jitter_cv = as.character(JITTER_CV))
  system2("Rscript", c("scripts/run_jitter.R"))
  
} else {
  cat("Unknown job type:", job_type, "\n")
  cat("Available: model, hessian, prof, jitter\n")
  quit(save = "no", status = 1)
}

cat("\n✓ Test complete\n")
