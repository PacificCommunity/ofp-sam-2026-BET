#!/usr/bin/env Rscript
## Profile likelihood using average biomass penalty
## Uses ProfLike_utils.R to generate and execute profile script

## Detect if running in Condor and navigate to repo root
current_dir <- getwd()
if(basename(dirname(dirname(current_dir))) == "model") {
  ## In Condor: working dir is model/base/prof/
  setwd("../../..")
  cat("Detected Condor environment, changed to:", getwd(), "\n")
}

## Load configuration
source("config.R")
source("tools/ProfLike_utils.R")
library(FLR4MFCL)
library(CondorBox)

## Get model name from environment
model_name <- Sys.getenv("MODEL_NAME", DEFAULT_MODEL)
model_config <- MODELS[[model_name]]

## Get profile settings from environment
scaler <- as.integer(Sys.getenv("scaler", "100"))
start_year <- as.integer(Sys.getenv("PROF_START_YEAR", PROF_START_YEAR))
end_year <- as.integer(Sys.getenv("PROF_END_YEAR", PROF_END_YEAR))

cat("==============================================\n")
cat("Profile Likelihood - Average Biomass\n")
cat("==============================================\n")
cat("Model:", model_name, "\n")
cat("Scaler:", scaler, "% of MLE\n")
cat("Period:", if(start_year == 0 && end_year == 0) "Entire period" else paste(start_year, "-", end_year), "\n\n")

## Setup directories
prof_dir <- file.path(get_prof_dir(model_name), paste0("scaler_", scaler))
dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)

## Copy input files from inputs directory
cat("* Copying input files from inputs directory ...\n")

## Get base_dir (inputs directory)
base_dir <- Sys.getenv("base_dir", model_config$inputs_dir)
if(!grepl("^/", base_dir)) {
  base_dir <- file.path(getwd(), base_dir)
}

cat("  Inputs directory:", base_dir, "\n")

## Check if directory exists
if(!dir.exists(base_dir)) {
  stop("Input directory does not exist: ", base_dir,
       "\nCheck inputs_dir in config.R for model: ", model_name)
}

## Copy all files from base_dir
files_to_copy <- list.files(base_dir, full.names = TRUE)
if(length(files_to_copy) == 0) {
  stop("Input directory is empty: ", base_dir)
}

cat("  Files to copy:", length(files_to_copy), "\n")
copied <- file.copy(files_to_copy, to = prof_dir, overwrite = TRUE, recursive = TRUE)
cat("  ✓ Copied", sum(copied), "files\n")

## Verify PAR file exists
par_file <- file.path(prof_dir, model_config$par_input)
if(!file.exists(par_file)) {
  stop("PAR file not found: ", model_config$par_input,
       "\nExpected in inputs directory: ", base_dir,
       "\nMake sure the PAR file exists in your inputs directory.")
}
cat("  ✓ PAR file verified:", model_config$par_input, "\n\n")

## Get MFCL executable (archive method: relative from work directory)
mfcl_exe_name <- paste0("mfclo64_", model_config$mfcl_version)
## From model/base/prof/ to mfcl/exe/
mfcl_exe_relative <- paste0("../../../mfcl/exe/", mfcl_exe_name)

frq_file <- "bet.frq"

## Get Reps from environment if provided, otherwise use config
reps_env <- Sys.getenv("PROF_REPS_OVERRIDE", "")
if(reps_env != "") {
  reps_vec <- as.integer(unlist(strsplit(reps_env, "\\s+")))
  if(length(reps_vec) == 6) {
    reps_override <- reps_vec
    names(reps_override) <- paste0("Reps", 1:6)
    cat("* Using Reps from environment:", paste(reps_override, collapse = " "), "\n")
  } else {
    reps_override <- PROF_REPS
  }
} else {
  reps_override <- PROF_REPS
}

## Get Penalties from environment if provided
pen_env <- Sys.getenv("PROF_PENALTIES_OVERRIDE", "")
if(pen_env != "") {
  pen_vec <- as.numeric(unlist(strsplit(pen_env, "\\s+")))
  if(length(pen_vec) == 3) {
    pen_override <- pen_vec
    names(pen_override) <- paste0("Pen", 1:3)
    cat("* Using Penalties from environment:", paste(pen_override, collapse = " "), "\n")
  } else {
    pen_override <- PROF_PENALTIES
  }
} else {
  pen_override <- PROF_PENALTIES
}

## Generate ProfLike.sh script using ProfLike_utils.R
cat("\n* Generating ProfLike.sh script ...\n")

## Calculate AgeFlags from start_year and end_year
## AgeFlags 173,174: first and last periods for biomass calculation
## StartYr=nyears-af173+1, EndYr=nyears-af174+1
## For entire period (0,0), use default af173=0, af174=0
age_flags <- c(Af173 = start_year, Af174 = end_year)

generate_proflike_script(
  Penalties = pen_override,
  Reps = reps_override,
  AgeFlags = age_flags,
  Prog = mfcl_exe_relative,
  Frq = frq_file,
  Initp = basename(most_recent),
  Mults = scaler,
  QuantityType = PROF_QUANTITY_TYPE,
  filename = file.path(prof_dir, "ProfLike.sh")
)

cat("  ✓ ProfLike.sh generated\n\n")

## Execute ProfLike.sh
cat("* Running ProfLike.sh ...\n")

setwd(prof_dir)
system2("bash", "ProfLike.sh", stdout = TRUE, stderr = TRUE)
setwd(BASE_DIR)

## Save run info
info_list <- list(
  species = SPECIES,
  quantity_type = "average_biomass",
  scaler = scaler,
  start_year = start_year,
  end_year = end_year,
  mfcl_version = sub("mfclo64_", "", basename(mfcl_exe)),
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(prof_dir, "prof_info.rds"))

cat("\n✓ Profile run complete\n")
cat("  Scaler:", scaler, "%\n")
cat("  Output directory:", prof_dir, "\n")

## Setup directories
prof_dir <- file.path(PROF_DIR, paste0("avg_bio_", scaler))
dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)

## Copy input files
cat("* Copying input files ...\n")
input_files <- get_input_files()
for(file in input_files) {
  if(file.exists(file)) {
    file.copy(file, prof_dir, overwrite = TRUE)
  }
}

## Copy converged par file
par_files <- list.files(MODEL_DIR, pattern = "\\d+\\.par$", full.names = TRUE)
if(length(par_files) > 0) {
  file_info <- file.info(par_files)
  most_recent <- par_files[which.max(file_info$mtime)]
  file.copy(most_recent, prof_dir, overwrite = TRUE)
  par_file <- basename(most_recent)
  cat("  ✓ Copied:", par_file, "\n\n")
} else {
  stop("No .par file found in model directory: ", MODEL_DIR)
}

## Build MFCL command for profile
mfcl_exe <- get_mfcl_exe()
frq_file <- basename(input_files$frq)

## Build MFCL command for average biomass profiling
## Using ProfLike methodology with penalties
output_par <- paste0("avg_bio_", scaler, ".par")

## Get switches from environment or use defaults
switches <- Sys.getenv("PROF_SWITCHES", 
  sprintf("-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 %d 1 346 %d 1 347 %d 1 348 %d 2 173 %d 2 174 %d",
    PROF_REPS["Reps1"], PROF_QUANTITY_TYPE, scaler, PROF_PENALTIES["Pen1"], start_year, end_year))

mfcl_command <- paste(
  mfcl_exe,
  frq_file, par_file, output_par,
  switches
)

cat("* Running MFCL profile ...\n")
cat("  Command:", mfcl_command, "\n\n")

## Run MFCL
run_commands(
  commands = mfcl_command,
  work_dirs = prof_dir,
  save_log = TRUE,
  parallel = FALSE,
  verbose = TRUE,
  log_file = file.path(prof_dir, "mfcl_log.txt")
)

## Save run info
info_list <- list(
  species = SPECIES,
  quantity_type = "average_biomass",
  scaler = scaler,
  start_year = start_year,
  end_year = end_year,
  mfcl_version = sub("mfclo64_", "", basename(mfcl_exe)),
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(prof_dir, "prof_info.rds"))

cat("\n✓ Profile run complete\n")
cat("  Scaler:", scaler, "%\n")
cat("  Output directory:", prof_dir, "\n")
