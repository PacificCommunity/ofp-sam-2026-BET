
## load libraries
library(FLR4MFCL)
library(CondorBox)

## environment variables
program_path=Sys.getenv("program_path", "../../exe/mfclo64_2023")
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir<-Sys.getenv("base_dir", "base/2023")
model_dir<-Sys.getenv("model_dir", "mfcl/base")
input_dir<-Sys.getenv("input_dir", "mfcl_inputs.R")
mfcl_commands <- Sys.getenv("mfcl_commands", "../../exe/mfclo64_2023 bet.frq 11.par 12.par")
mfcl_dir <- Sys.getenv("mfcl_dir", "mfcl/base")

## create model directory and copy files
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)


## run MFCL
run_commands(commands=mfcl_commands,
             work_dirs=mfcl_dir, 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = paste0(model_dir,"/mfcl_log.txt"))

