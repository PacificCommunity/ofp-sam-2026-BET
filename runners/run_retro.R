## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/retro.R")
source("tools/model_payload.R")
source("tools/condor_archive_cleanup.R")

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
program_path_abs <- file.path(project_root, program_path)
base_dir_abs <- file.path(project_root, base_dir)

## Retrospective analysis settings
## Single peel value for parallel execution via condor
## peel = number of years to remove from the end of the time series
retro_peel <- as.integer(Sys.getenv("retro_peel", "4"))

## mixing period
n_mixing_periods <- as.integer(Sys.getenv("n_mixing_periods", "2"))

## Create retro-specific directory inside retro folder
retro_dir <- file.path(model_dir, "retro")
peel_dir <- file.path(retro_dir, paste0("peel_", retro_peel))

cat("Running Retrospective Analysis\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Retro directory:", retro_dir, "\n")
cat("Peel directory:", peel_dir, "\n")
cat("Retro peel:", retro_peel, "years\n")

## Create peel directory and copy all files from base_dir (inputs)
dir.create(peel_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = peel_dir, overwrite = TRUE, recursive = TRUE)

####################################
## Generate retrospective inputs  ##
####################################

## Automatically detect file names by extension
frq_file <- list.files(peel_dir, pattern = "\\.frq$", full.names = FALSE)
tag_file <- list.files(peel_dir, pattern = "\\.tag$", full.names = FALSE)
age_file <- list.files(peel_dir, pattern = "\\.age_length$", full.names = FALSE)
ini_file <- list.files(peel_dir, pattern = "\\.ini$", full.names = FALSE)

## Check if files exist
if(length(frq_file) == 0) stop("No .frq file found in directory")
if(length(tag_file) == 0) stop("No .tag file found in directory")
if(length(age_file) == 0) stop("No .age_length file found in directory")
if(length(ini_file) == 0) stop("No .ini file found in directory")

## Use first file if multiple files found
if(length(frq_file) > 1) {
  warning("Multiple .frq files found, using: ", frq_file[1])
  frq_file <- frq_file[1]
}
if(length(tag_file) > 1) {
  warning("Multiple .tag files found, using: ", tag_file[1])
  tag_file <- tag_file[1]
}
if(length(age_file) > 1) {
  warning("Multiple .age_length files found, using: ", age_file[1])
  age_file <- age_file[1]
}
if(length(ini_file) > 1) {
  warning("Multiple .ini files found, using: ", ini_file[1])
  ini_file <- ini_file[1]
}

cat("Detected files:\n")
cat("  FRQ:", frq_file, "\n")
cat("  TAG:", tag_file, "\n")
cat("  AGE:", age_file, "\n")
cat("  INI:", ini_file, "\n")

## Get max_year from original data
frq_path <- file.path(peel_dir, frq_file)
frq_orig <- read.MFCLFrq(frq_path)

## Calculate new max_year (terminal year - peel)
## peel=1 means remove 1 year, so new max_year = terminal_year - 1
terminal_year <- frq_orig@range["maxyear"]
new_max_year <- terminal_year - retro_peel

cat("Terminal year:", terminal_year, "\n")
cat("New max year:", new_max_year, "\n")

## Read all input files from peel directory
tag_data <- read.MFCLTag(file.path(peel_dir, tag_file))
age_data <- read.MFCLALK(file.path(peel_dir, age_file))
ini_data <- read.MFCLIni(file.path(peel_dir, ini_file))

## Apply retrospective modifications
retro_tag <- retro.tag(tag_data, new_max_year, n_mixing_periods = n_mixing_periods)
retro_frq <- retro.frq(frq_orig, new_max_year, retro_tag)
retro_age <- retro.age(age_data, new_max_year)
retro_ini <- retro.ini(ini_data, tag.obj = tag_data, max_year = new_max_year, n_mixing_periods = n_mixing_periods)


# # check modifications 
# cat("Before write:\n")
# head(retro_age@ALK, 20)
# cat("\nFirst year:", min(retro_age@ALK$year), "\n")
# cat("ESS length:", length(retro_age@ESS), "\n")

## Write modified input files
FLR4MFCL::write(retro_ini$ini, file = file.path(peel_dir, ini_file))
FLR4MFCL::write(retro_tag, file = file.path(peel_dir, tag_file))
FLR4MFCL::write(retro_frq, file = file.path(peel_dir, frq_file))
FLR4MFCL::write(retro_age, file = file.path(peel_dir, age_file))

cat("Retrospective input files written\n")

##############
## run MFCL ##
##############

mfcl_commands <- "./doitall.sh"

cat("Running MFCL with commands:", mfcl_commands, "\n")

run_commands(commands = mfcl_commands,
             work_dirs = peel_dir, 
             save_log = TRUE, 
             parallel = FALSE, 
             verbose = TRUE, 
             log_file = file.path(peel_dir, "mfcl_log.txt"))

# Save retro run info
info_list <- list(
  retro_peel    = retro_peel,
  new_max_year  = new_max_year,
  terminal_year = terminal_year,
  frq_file      = frq_file,
  tag_file      = tag_file,
  age_file      = age_file,
  ini_file      = ini_file,
  program_path  = program_path,
  model_dir     = model_dir,
  peel_dir      = peel_dir
)

saveRDS(
  info_list,
  file = file.path(peel_dir, "retro_info.rds"),
  compress = "xz"
)

retro_rep_file <- mp_final_rep(peel_dir)
retro_rep_obj <- if (!is.null(retro_rep_file) && file.exists(retro_rep_file)) {
  tryCatch(read.MFCLRep(retro_rep_file), error = function(e) NULL)
} else {
  NULL
}
retro_metrics <- mp_extract_rep_timeseries(
  retro_rep_obj,
  scenario = basename(model_dir),
  peel = retro_peel
)
saveRDS(
  retro_metrics,
  file = file.path(peel_dir, "retro_metrics.rds"),
  compress = "xz"
)

deleted_n <- mp_cleanup_files(
  peel_dir,
  keep = c("retro_metrics.rds", "retro_info.rds"),
  recursive = TRUE
)
cat("Cleanup removed", deleted_n, "non-core files in", peel_dir, "\n")

cb_condor_keep_only_model_cleanup()

cat("✅ Retrospective run completed for peel", retro_peel, "\n")
