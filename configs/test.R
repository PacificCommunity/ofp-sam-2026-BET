

models=list("base"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                             "-switch 1 1 1 1",
                                             sep = " ")),
            
            "fixK"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                           "-switch 2 1 1 50",
                                           "1 14 0",
                                           sep = " ")),
            
            "noAge"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                           "-switch 2 1 1 50",
                                           "1 240 0",
                                           sep = " "))
            )


program_path <- "../../mfcl/exe/mfclo64_2026"
base_dir <- "mfcl/inputs/2023"
run_prof <- "1"
Reps <- "2 2 2 2 2 2"
scalers <- "110 100 90 80 70 60 50"

### post-processing 

ModelIDs <- names(models)
models <- Map(function(x, nm) {
  x$mfcl_commands <- paste(program_path, x$mfcl_commands)
  x$model_dir <- paste0("model/",nm)
  x$base_dir <- base_dir
  x$program_path <- program_path
  x$run_prof <-run_prof
  x$Reps <-Reps
  x$scalers <-scalers
  x
}, models, names(models))




