models <- list(
  
  # "mixP1" = list(
  #   mfcl_args = paste("bet.frq 11.par 12.par",
  #                         "-switch 2",
  #                         "1 1 10000", 
  #                         "-9999 1 1",
  #                         sep = " "),
  #   program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
  #   base_dir = "mfcl/inputs/2026"                   # Model-specific dir
  # ),
  
  "base" = list(
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1",
                          "1 1 1", 
                       #   "-9999 1 2",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep"                   # Model-specific dir
  ))

  # "mixP3" = list(
  #   mfcl_commands = paste("bet.frq 11.par 12.par",
  #                         "-switch 2",
  #                         "1 1 10000", 
  #                         "-9999 1 3",
  #                         sep = " "),
  #   program_path = "../../mfcl/exe/mfclo64_2026_01_22_vsn2278",  # Model-specific path
  #   base_dir = "mfcl/inputs/2026"                   # Model-specific dir
  # ))


# 
# defaultswitch<- paste("-switch 11",
#                       "1 1 100", 
#                       "1 246 1",  
#                       ## round(sqrt(5/penalty)*100)
#                       "-33 92 24",
#                       "-34 92 31",
#                       "-35 92 20",
#                       "-36 92 21",
#                       "-37 92 26",
#                       "-38 92 23",
#                       "-39 92 20",
#                       "-40 92 25",
#                       "-41 92 47",
#                       sep=" ")
# 
# 
# 




# Default values
default_program_path <- "mfcl/exe/mfclo64_2023"
default_base_dir <- "mfcl/inputs/2023_rep"
run_prof <- "1"
Reps <- "15 25 25 1000 500 500"
#Reps <- "15 25 25 100 10 50"
#scalers <- paste0((seq(120, 70, by=-10)), collapse = " ")
scalers <- paste0((seq(140, 50, by=-5)), collapse = " ")



### Post-processing with defaults

ModelIDs <- names(models)
models <- Map(function(x, nm) {
  # Use model-specific paths if provided, otherwise use defaults
  prog_path <- if (!is.null(x$program_path)) x$program_path else default_program_path
  b_dir <- if (!is.null(x$base_dir)) x$base_dir else default_base_dir
  
  if(x$mfcl_commands == "./doitall.sh") {
    x$mfcl_commands <- x$mfcl_commands
  } else {
  x$mfcl_commands <- paste(prog_path, x$mfcl_commands)
  }
  x$model_dir <- paste0("model/", nm)
  x$base_dir <- b_dir
  x$program_path <- prog_path
  x$run_prof <- run_prof
  x$Reps <- Reps
  x$scalers <- scalers
  x
}, models, names(models))




