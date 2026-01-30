## load libraries
library(FLR4MFCL)
library(CondorBox)

## environment variables
program_path=Sys.getenv("program_path", "../../mfcl/exe/mfclo64_2026_01_22_vsn2278")
Sys.setenv("PROGRAM_PATH" = program_path)
base_dir<-Sys.getenv("base_dir", "mfcl/inputs/2026")
model_dir<-Sys.getenv("model_dir", "model/base")
defaultswitch<- paste("-switch 1",
                      "1 1 1", 
                      sep=" ")

mfcl_commands <- Sys.getenv("mfcl_commands", paste(program_path,"bet.frq 11.par 12.par", defaultswitch))

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

# Save model run info (compatible with plot code)
scalers <- Sys.getenv("scalers", "120 110 100 90 80 70")
Reps <- as.integer(unlist(strsplit(Sys.getenv("Reps", "1 1 5 5 1 1"), "\\s+")))
names(Reps) <- paste0("Reps", 1:length(Reps))
frq_file <- list.files(model_dir, pattern = "\\.frq$", full.names = FALSE)
run_prof <- as.integer(Sys.getenv("run_prof", "0"))

info_list <- list(
  Reps          = Reps,
  scalers       = as.numeric(unlist(strsplit(scalers, "\\s+"))),
  frq_file      = frq_file,
  program_path  = program_path,
  mfcl_commands = mfcl_commands,
  base_dir      = base_dir,
  run_prof      = run_prof
)

saveRDS(
  info_list,
  file = file.path(model_dir, "info.rds"),
  compress = "xz"
)

cat("✅ Model run completed for", basename(model_dir), "\n")
