## load libraries
library(FLR4MFCL)
library(CondorBox)

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_01_22_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

## Hessian calculation settings
## Single part number for parallel execution via condor
hessian_part <- as.integer(Sys.getenv("hessian_part", "2"))
nsplit <- as.integer(Sys.getenv("nsplit", "3067"))

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
copy_result <- file.copy(files_to_copy, to = part_dir, overwrite = TRUE, recursive = TRUE)
cat("Copied", sum(copy_result), "files from base directory\n")

## Also copy par file from model_dir (converged model) - REQUIRED for Hessian
model_dir_abs <- file.path(project_root, model_dir)
if(dir.exists(model_dir_abs)) {
  par_in_model <- list.files(model_dir_abs, pattern = "\\.par$", full.names = TRUE)
  if(length(par_in_model) > 0) {
    file.copy(par_in_model, to = part_dir, overwrite = TRUE)
    cat("Copied", length(par_in_model), "par file(s) from model directory\n")
  } else {
    cat("Warning: No par files found in model directory:", model_dir_abs, "\n")
  }
} else {
  cat("Warning: Model directory does not exist:", model_dir_abs, "\n")
  cat("Run 'make model' first to generate a converged par file\n")
}

###############################
## Calculate parameter range ##
###############################

par_files <- list.files(part_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(part_dir, pattern = "\\.frq$", full.names = FALSE)

cat("\nLooking for par files in:", part_dir, "\n")
cat("Found", length(par_files), "par file(s)\n")

if(length(par_files) == 0) {
  cat("\n❌ ERROR: No .par files found in directory:", part_dir, "\n")
  cat("\nTroubleshooting:\n")
  cat("1. Check if base directory has par files:", base_dir_abs, "\n")
  cat("2. Check if model directory has par files:", model_dir_abs, "\n")
  cat("3. Run 'make model' first to generate a converged par file\n")
  cat("4. Or copy a par file manually to the model directory\n\n")
  stop("No .par files found in directory: ", part_dir)
}

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

## Calculate parameter range for this part
arg2 <- (1:nsplit) * ceiling(npars / nsplit)
arg1 <- arg2
arg1[2:nsplit] <- arg2[1:(nsplit-1)] + 1
arg2[nsplit] <- npars
arg1[1] <- 1

start_par <- arg1[hessian_part]
end_par <- arg2[hessian_part]

cat("Calculating Hessian for parameters", start_par, "to", end_par, "\n")

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
  input_par     = basename(most_recent),
  output_par    = output_par_name
)

saveRDS(
  info_list,
  file = file.path(part_dir, "hessian_info.rds"),
  compress = "xz"
)

cat("✅ Hessian calculation completed for part", hessian_part, "\n")
