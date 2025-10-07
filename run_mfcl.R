

mfcl_commands <- Sys.getenv("mfcl_commands", "./doitall.sh")
mfcl_dir <- Sys.getenv("mfcl_dir", "MFCL/2023")

CondorBox::run_commands(commands=mfcl_commands,
                        work_dirs=mfcl_dir, 
                        save_log = T, 
                        parallel = F, 
                        verbose = T, 
                        log_file = "MFCL_run.txt")
