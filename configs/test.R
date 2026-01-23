
models <- list(
  "mfcl2023" = list(
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 2 1 1 1",
                          "1 246 1",
                          sep = " "),
    program_path = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023"                   # Model-specific dir
   ),
  
  
  "mfcl2026" = list(
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1 1 1 50",
                          "1 246 1",
                          sep = " "),
    program_path = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023"                   # Model-specific dir
  )
  
  
  # 
  #   "base2026" = list(
  #     mfcl_commands = paste("bet.frq 11.par 12.par",
  #                          "-switch 1 1 1 1000",
  #                          sep = " "),
  #     program_path = "../../mfcl/exe/mfclo64_2026",
  #     base_dir = "mfcl/inputs/2023"
  #  )
)

# Default values
default_program_path <- "../../mfcl/exe/mfclo64_2023"
default_base_dir <- "mfcl/inputs/2023"
run_prof <- "1"
Reps <- "100 100 1000 1000 5000 5000"
scalers <- paste0((seq(150, 50, by=-5)), collapse = " ")

### Post-processing with defaults

ModelIDs <- names(models)
models <- Map(function(x, nm) {
  # Use model-specific paths if provided, otherwise use defaults
  prog_path <- if (!is.null(x$program_path)) x$program_path else default_program_path
  b_dir <- if (!is.null(x$base_dir)) x$base_dir else default_base_dir
  
  x$mfcl_commands <- paste(prog_path, x$mfcl_commands)
  x$model_dir <- paste0("model/", nm)
  x$base_dir <- b_dir
  x$program_path <- prog_path
  x$run_prof <- run_prof
  x$Reps <- Reps
  x$scalers <- scalers
  x
}, models, names(models))




