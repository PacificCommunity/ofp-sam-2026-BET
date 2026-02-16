## load libraries
library(FLR4MFCL)
library(CondorBox)
source("tools/model_payload.R")

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base2")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

## Find .frq file automatically
frq_files <- list.files(base_dir_abs, pattern = "\\.frq$", full.names = FALSE)

if(length(frq_files) == 0) {
  stop("No .frq file found in ", base_dir_abs)
} else if(length(frq_files) > 1) {
  warning("Multiple .frq files found, using first one: ", frq_files[1])
  frq_file <- frq_files[1]
} else {
  frq_file <- frq_files[1]
}

cat("Found .frq file:", frq_file, "\n")

defaultswitch <- paste("-switch 1",
                       "1 1 10",
                       #"1 145 5",
                       #"1 223 3039",
                       #"1 224 3067",
                       sep=" ")

## mfcl_commands contains only arguments (frq, par, switches), NOT program path
mfcl_commands <- Sys.getenv("mfcl_commands", 
                            paste(program_path, frq_file, "11.par 12.par", defaultswitch))
mfcl_commands <- paste0("../../", mfcl_commands)

## create model directory and copy files
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

cat("Running MFCL with commands:", mfcl_commands, "\n")
cat("Using .frq file:", frq_file, "\n")
cat("Base inputs directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")

##############
## run MFCL ##
##############

run_commands(commands=mfcl_commands,
             #commands="./doitall.sh",
             work_dirs=model_dir, 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = paste0(model_dir,"/mfcl_log.txt"))

# Save model run info
info_list <- list(
  program_path  = program_path,
  mfcl_commands = mfcl_commands,
  frq_file      = frq_file,
  base_dir      = base_dir,
  model_dir     = model_dir
)

saveRDS(
  info_list,
  file = file.path(model_dir, "model_info.rds"),
  compress = "xz"
)

payload <- mp_build_model_payload(model_dir, tag_report_year1 = 1952)
saveRDS(
  payload,
  file = file.path(model_dir, "model_payload.rds"),
  compress = "xz"
)

# Keep only core artifacts at top level; downstream folders (jitter/prof/retro/hessian) are untouched.
keep_top <- c(
  "model_payload.rds",
  "model_info.rds",
  "fishery_map.R",
  "fishery_map.r"
)

if (!is.null(payload$files$par) && file.exists(payload$files$par)) {
  keep_top <- c(keep_top, basename(payload$files$par))
}

deleted_n <- mp_cleanup_files(model_dir, keep = keep_top, recursive = FALSE)
cat("Cleanup removed", deleted_n, "non-core top-level files in", model_dir, "\n")

cat("✅ Model run completed for", basename(model_dir), "\n")
