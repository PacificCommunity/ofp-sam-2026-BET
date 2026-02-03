#!/usr/bin/env Rscript
## Run single Hessian part for parallel calculation
## Uses centralized config.R for all settings

## Detect if running in Condor and navigate to repo root
current_dir <- getwd()
if(basename(dirname(dirname(current_dir))) == "model") {
  ## In Condor: working dir is model/base/hessian/part_X/
  setwd("../../../..")
  cat("Detected Condor environment, changed to:", getwd(), "\n")
}

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

## Get model name from environment
model_name <- Sys.getenv("MODEL_NAME", DEFAULT_MODEL)
model_config <- MODELS[[model_name]]

## Get Hessian calculation settings
hessian_part <- as.integer(Sys.getenv("hessian_part", "1"))
nsplit <- as.integer(Sys.getenv("nsplit", HESSIAN_NSPLIT))

cat("==============================================\n")
cat("Hessian Calculation - Part", hessian_part, "of", nsplit, "\n")
cat("==============================================\n")
cat("Model:", model_name, "\n")

## Setup directories
part_dir <- file.path(get_hessian_dir(model_name), paste0("part_", hessian_part))
dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

cat("Part directory:", part_dir, "\n\n")

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
copied <- file.copy(files_to_copy, to = part_dir, overwrite = TRUE, recursive = TRUE)
cat("  ✓ Copied", sum(copied), "files\n")

## Verify PAR file exists
par_file <- model_config$par_input
par_path <- file.path(part_dir, par_file)
if(!file.exists(par_path)) {
  stop("PAR file not found: ", par_file,
       "\nExpected in inputs directory: ", base_dir,
       "\nMake sure the PAR file exists in your inputs directory.")
}
cat("  ✓ PAR file verified:", par_file, "\n")
par_lines <- readLines(par_path)
npars_line <- grep("# The number of parameters", par_lines)
if(length(npars_line) > 0) {
  npars <- as.integer(scan(par_path, skip = npars_line, nlines = 1, quiet = TRUE))
} else {
  stop("Could not find number of parameters in par file")
}

cat("  Total parameters:", npars, "\n\n")

## Calculate parameter range using balanced distribution
base_size <- floor(npars / nsplit)
remainder <- npars %% nsplit

if(hessian_part <= remainder) {
  start_par <- (hessian_part - 1) * (base_size + 1) + 1
  end_par <- hessian_part * (base_size + 1)
} else {
  offset <- remainder * (base_size + 1)
  start_par <- offset + (hessian_part - remainder - 1) * base_size + 1
  end_par <- offset + (hessian_part - remainder) * base_size
}

cat("* Parameter range for this part:\n")
cat("  Start:", start_par, "\n")
cat("  End:", end_par, "\n")
cat("  Count:", end_par - start_par + 1, "\n\n")

## Get MFCL executable (relative from work directory)
mfcl_exe_name <- paste0("mfclo64_", model_config$mfcl_version)
## From model/base/hessian/part_X/ to mfcl/exe/
mfcl_exe_relative <- file.path("../../../..", "mfcl", "exe", mfcl_exe_name)

## Build MFCL command for Hessian calculation
frq_file <- "bet.frq"

output_par_name <- paste0("hessian_", hessian_part, ".par")
hessian_switch <- paste("-switch 3",
                        "1 145 1",
                        "1 223", start_par,
                        "1 224", end_par,
                        sep = " ")

mfcl_command <- paste(
  mfcl_exe_relative,
  frq_file,
  par_file,
  output_par_name,
  hessian_switch
)

cat("* Running MFCL Hessian calculation ...\n")
cat("  Command:", mfcl_command, "\n\n")

## Run MFCL
run_commands(
  commands = mfcl_command,
  work_dirs = part_dir,
  save_log = TRUE,
  parallel = FALSE,
  verbose = TRUE,
  log_file = file.path(part_dir, "mfcl_log.txt")
)

## Save part info
info_list <- list(
  species = SPECIES,
  hessian_part = hessian_part,
  nsplit = nsplit,
  npars = npars,
  start_par = start_par,
  end_par = end_par,
  mfcl_version = model_config$mfcl_version,
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(part_dir, "hessian_info.rds"))

cat("\n✓ Hessian part", hessian_part, "complete\n")
cat("  Output directory:", part_dir, "\n")
cat("  Info saved:", file.path(part_dir, "hessian_info.rds"), "\n")
