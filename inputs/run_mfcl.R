
## load libraries
library(FLR4MFCL)
library(CondorBox)

## environment variables
program_path=Sys.getenv("program_path", "../../exe/mfclo64_2023")
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir<-Sys.getenv("base_dir", "base/2023")
model_dir<-Sys.getenv("model_dir", "mfcl/base")
mfcl_commands <- Sys.getenv("mfcl_commands", paste(program_path,"bet.frq 11.par 12.par"))

## create model directory and copy files
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

cat("Running MFCL with commands:", mfcl_commands, "\n")
cat("Base inputs directory:", base_dir, "\n")
cat("Model directory:", model_dir, "\n")

## run MFCL
run_commands(commands=mfcl_commands,
             work_dirs=model_dir, 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = paste0(model_dir,"/mfcl_log.txt"))

