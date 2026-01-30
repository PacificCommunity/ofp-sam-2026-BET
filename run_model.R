## load libraries
library(FLR4MFCL)
library(CondorBox)

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_01_22_vsn2278"); program_path <- paste0("../../",program_path)
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2026")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

defaultswitch <- paste("-switch 1",
                       "1 1 1", 
                       sep=" ")

## mfcl_commands contains only arguments (frq, par, switches), NOT program path
mfcl_commands <- Sys.getenv("mfcl_commands", paste(program_path, "bet.frq 11.par 12.par", defaultswitch))

## create model directory and copy files
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

cat("Running MFCL with commands:", mfcl_commands, "\n")
cat("Base inputs directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")

##############
## run MFCL ##
##############

run_commands(commands=mfcl_commands,
             work_dirs=model_dir, 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = paste0(model_dir,"/mfcl_log.txt"))

# Save model run info
info_list <- list(
  program_path  = program_path,
  mfcl_commands = mfcl_commands,
  base_dir      = base_dir,
  model_dir     = model_dir
)

saveRDS(
  info_list,
  file = file.path(model_dir, "model_info.rds"),
  compress = "xz"
)

cat("✅ Model run completed for", basename(model_dir), "\n")
