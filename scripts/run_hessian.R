#!/usr/bin/env Rscript
## Run single Hessian part for parallel calculation
## Uses centralized config.R for all settings

## Load configuration
source("config.R")
library(FLR4MFCL)
library(CondorBox)

## Get Hessian calculation settings
hessian_part <- as.integer(Sys.getenv("hessian_part", "1"))
nsplit <- as.integer(Sys.getenv("nsplit", HESSIAN$nsplit))

cat("==============================================\n")
cat("Hessian Calculation - Part", hessian_part, "of", nsplit, "\n")
cat("==============================================\n")

## Setup directories
part_dir <- file.path(HESSIAN_DIR, paste0("part_", hessian_part))
dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

cat("Part directory:", part_dir, "\n\n")

## Copy input files
cat("* Copying input files ...\n")
input_files <- get_input_files()
for(file in input_files) {
  if(file.exists(file)) {
    file.copy(file, part_dir, overwrite = TRUE)
  }
}

## Copy converged par file from model directory
par_files <- list.files(MODEL_DIR, pattern = "\\d+\\.par$", full.names = TRUE)
if(length(par_files) > 0) {
  ## Use most recent par file
  file_info <- file.info(par_files)
  most_recent <- par_files[which.max(file_info$mtime)]
  file.copy(most_recent, part_dir, overwrite = TRUE)
  cat("  ✓ Copied:", basename(most_recent), "\n")
  par_file <- basename(most_recent)
} else {
  stop("No .par file found in model directory: ", MODEL_DIR)
}

## Read number of parameters from par file
par_path <- file.path(part_dir, par_file)
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

## Get MFCL executable (archive method: relative from work directory)
if(MFCL_VERSION == "2026") {
  mfcl_exe <- "mfclo64_2026_01_22_vsn2278"
} else if(MFCL_VERSION == "2023") {
  mfcl_exe <- "mfclo64_2023"
} else {
  mfcl_exe <- paste0("mfclo64_", MFCL_VERSION)
}
program_path <- file.path("mfcl/exe", mfcl_exe)
## From model/base/hessian/part_X/ to mfcl/exe/
mfcl_exe_relative <- file.path("../../../..", program_path)

## Build MFCL command for Hessian calculation
frq_file <- basename(input_files$frq)

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
  mfcl_version = sub("mfclo64_", "", basename(mfcl_exe)),
  timestamp = Sys.time()
)

saveRDS(info_list, file = file.path(part_dir, "hessian_info.rds"))

cat("\n✓ Hessian part", hessian_part, "complete\n")
cat("  Output directory:", part_dir, "\n")
cat("  Info saved:", file.path(part_dir, "hessian_info.rds"), "\n")
