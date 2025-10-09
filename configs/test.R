

models=list("base"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                             "-switch 1 1 1 10",
                                             sep = " ")),
            
            "m1"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                           "-switch 1 1 1 20",
                                           sep = " ")),
            
            "m2"=list(mfcl_commands=paste("bet.frq 11.par 12.par",
                                           "-switch 1 1 1 30",
                                           sep = " "))
            )


program_path <- "../../mfcl/exe/mfclo64_2023"
base_dir <- "base/2023"

### post-processing 

ModelIDs <- names(models)
models <- Map(function(x, nm) {
  x$mfcl_commands <- paste(program_path, x$mfcl_commands)
  x$model_dir <- paste0("mfcl/",nm)
  x$base_dir <- base_dir
  x$program_path <- program_path
  x
}, models, names(models))


