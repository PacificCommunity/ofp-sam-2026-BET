## load libraries
library(FLR4MFCL)
library(CondorBox)
source("tools/condor_archive_cleanup.R")

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

## Hessian calculation settings
## Single part number for parallel execution via condor
hessian_part <- as.integer(Sys.getenv("hessian_part", "1"))
nsplit <- as.integer(Sys.getenv("nsplit", "5"))

## Create hessian-specific directory inside hessian folder
hessian_dir <- file.path(model_dir, "hessian")
part_dir <- file.path(hessian_dir, paste0("part_", hessian_part))

cat("Running Hessian Calculation\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Hessian directory:", hessian_dir, "\n")
cat("Part directory:", part_dir, "\n")
cat("Hessian part:", hessian_part, "of", nsplit, "\n")

## Create part directory and copy all files from base_dir (inputs)
dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = part_dir, overwrite = TRUE, recursive = TRUE)

## Also copy par file from model_dir (converged model)
model_dir_abs <- file.path(project_root, model_dir)
par_in_model <- list.files(model_dir_abs, pattern = "\\.par$", full.names = TRUE)
if(length(par_in_model) > 0) {
  file.copy(par_in_model, to = part_dir, overwrite = TRUE)
  cat("Copied par files from model directory\n")
}

###############################
## Calculate parameter range ##
###############################

par_files <- list.files(part_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(part_dir, pattern = "\\.frq$", full.names = FALSE)

if(length(par_files) > 0) {
  # Get file information
  file_info <- file.info(par_files)
  
  # Find the most recently modified file
  most_recent <- rownames(file_info)[which.max(file_info$mtime)]
  
  cat("Most recent par file:", basename(most_recent), "\n")
  cat("Modified time:", as.character(file_info[most_recent, "mtime"]), "\n")
} else {
  stop("No .par files found in directory: ", part_dir)
}

## Read par file to get number of parameters
par_lines <- readLines(most_recent)
npars_line <- grep("# The number of parameters", par_lines)
if(length(npars_line) > 0) {
  npars <- as.integer(scan(most_recent, skip = npars_line, nlines = 1, quiet = TRUE))
  cat("Total number of parameters:", npars, "\n")
} else {
  stop("Could not find number of parameters in par file")
}

## Calculate parameter range for this part using balanced distribution
## This ensures all parts are used without skipping
base_size <- floor(npars / nsplit)
remainder <- npars %% nsplit

cat("Base chunk size:", base_size, "\n")
cat("Parts with +1 extra:", remainder, "\n")

## First 'remainder' parts get (base_size + 1) parameters
## Remaining parts get base_size parameters
if(hessian_part <= remainder) {
  ## Larger chunks for first 'remainder' parts
  start_par <- (hessian_part - 1) * (base_size + 1) + 1
  end_par <- hessian_part * (base_size + 1)
} else {
  ## Smaller chunks for remaining parts
  offset <- remainder * (base_size + 1)
  start_par <- offset + (hessian_part - remainder - 1) * base_size + 1
  end_par <- offset + (hessian_part - remainder) * base_size
}

cat("Calculating Hessian for parameters", start_par, "to", end_par, 
    "(", end_par - start_par + 1, "parameters )\n")

##############
## run MFCL ##
##############

## Hessian calculation switches
## -switch 3: phase 3 (Hessian calculation)
## 1 145 1: standard flags
## 1 223 start: set starting parameter
## 1 224 end: set ending parameter
hessian_switch <- paste("-switch 3",
                        "1 145 1",
                        "1 223", start_par,
                        "1 224", end_par,
                        sep = " ")

output_par_name <- paste0("hessian_", hessian_part, ".par")
mfcl_commands <- paste0("../../../../", program_path, " ", 
                        frq_file, " ", 
                        basename(most_recent), " ", 
                        output_par_name, " ", 
                        hessian_switch)

cat("Running MFCL with commands:", mfcl_commands, "\n")

run_commands(commands = mfcl_commands,
             work_dirs = part_dir, 
             save_log = TRUE, 
             parallel = FALSE, 
             verbose = TRUE, 
             log_file = file.path(part_dir, "mfcl_hessian_log.txt"))

# Save hessian run info
info_list <- list(
  hessian_part  = hessian_part,
  nsplit        = nsplit,
  start_par     = start_par,
  end_par       = end_par,
  npars         = npars,
  frq_file      = frq_file,
  program_path  = program_path,
  model_dir     = model_dir,
  part_dir      = part_dir,
  base_dir      = base_dir,
  input_par     = basename(most_recent),
  output_par    = output_par_name
)

saveRDS(
  info_list,
  file = file.path(part_dir, "hessian_info.rds"),
  compress = "xz"
)

## Optional compact cleanup within each part directory.
## Keep only files needed for downstream collate/stitch and diagnostics.
part_compact_cleanup <- tolower(Sys.getenv("hessian_part_compact_cleanup", "true")) %in% c("1", "true", "yes", "y")
if (isTRUE(part_compact_cleanup)) {
  keep_ext_files <- list.files(
    part_dir,
    pattern = "\\.(hes|rds)$",
    full.names = FALSE
  )
  keep_named_files <- c("depgrad.rpt", "Hess.rpt")
  keep_files <- unique(c(keep_ext_files, keep_named_files))

  entries <- list.files(part_dir, full.names = TRUE, recursive = FALSE, all.files = FALSE, no.. = TRUE)
  entry_base <- basename(entries)
  drop_entries <- entries[!(entry_base %in% keep_files)]
  if (length(drop_entries) > 0) {
    unlink(drop_entries, recursive = TRUE, force = TRUE)
  }
  cat("Compact cleanup complete for part directory; kept", length(keep_files), "files\n")
}

cb_condor_keep_only_model_cleanup()

cat("✅ Hessian calculation completed for part", hessian_part, "\n")
