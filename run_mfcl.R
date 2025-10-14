
## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")

## environment variables
program_path=Sys.getenv("program_path", "../../mfcl/exe/mfclo64_2026")
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir<-Sys.getenv("base_dir", "mfcl/inputs/2023")
model_dir<-Sys.getenv("model_dir", "model/base")
mfcl_commands <- Sys.getenv("mfcl_commands", paste(program_path,"bet.frq 11.par 12.par -switch 1 1 1 1"))
run_prof<-as.integer(Sys.getenv("run_prof", "1"))
Reps<-as.numeric(unlist(strsplit(Sys.getenv("Reps", "1 1 1 1 1 1"), "\\s+")))



## create model directory and copy files
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir, full.names = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

cat("Running MFCL with commands:", mfcl_commands, "\n")
cat("Base inputs directory:", base_dir, "\n")
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

############################
## run likelihood profile ##
############################

if(run_prof==1) {
  
  par_files <- list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
  frq_file <- list.files(model_dir, pattern = "\\.frq$", full.names = FALSE)
  
  if(length(par_files) > 0) {
    # Get file information
    file_info <- file.info(par_files)
    
    # Find the most recently modified file
    most_recent <- rownames(file_info)[which.max(file_info$mtime)]
    
    cat("Most recent file:", basename(most_recent), "\n")
    cat("Modified time:", as.character(file_info[most_recent, "mtime"]), "\n")
  } else {
    cat("No .par files found in directory\n")
  }
  
  generate_proflike_script(Prog = program_path,
                           Reps =Reps,
                           Frq = frq_file,
                           Initp = basename(most_recent),
                           filename = paste0(model_dir,"/ProfLike.sh"))
  
  run_commands(commands="./ProfLike.sh",
               work_dirs = model_dir,
               save_log = F,
               verbose = T)
  
}
