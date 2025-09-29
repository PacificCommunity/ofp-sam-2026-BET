
library(CondorBox)

run_commands("./doitall.sh",
             work_dirs="MFCL/2023/", 
             save_log = T, 
             parallel = F, 
             verbose = T, 
             log_file = "MFCL_run.txt")
