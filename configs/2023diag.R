summary <-"replicate the 2023 diagnostic model"

models <- list(
  
  "2023diag" = list(
    
    "description"="2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 500 500",
    scalers = paste0((seq(170, 50, by=-5)), collapse = " "),
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:50, collapse = " "),
    jitter_amount = "0.2",
    
    ## hessian parallel settings
    nsplit="5"
  )
  
  )




# Default values
default_program_path <- "mfcl/exe/mfclo64_2023"
default_base_dir <- "mfcl/inputs/2023_rep"
Reps <- "15 25 25 1000 500 500"
scalers <- paste0((seq(140, 50, by=-5)), collapse = " ")
retro_peels <- "1 2 3 4 5"  # Default retrospective peels (years to remove)
jitter_seeds <- 1:10
jitter_amount <- 0.01
nsplit <- 5

### Post-processing with defaults

ModelIDs <- names(models)
models <- Map(function(x, nm) {
  # Use model-specific paths if provided, otherwise use defaults
  prog_path <- if (!is.null(x$program_path)) x$program_path else default_program_path
  b_dir <- if (!is.null(x$base_dir)) x$base_dir else default_base_dir
  scalers <- if (!is.null(x$scalers)) x$scalers else scalers
  retro_peels <- if (!is.null(x$retro_peels)) x$retro_peels else retro_peels
  Reps <- if (!is.null(x$Reps)) x$Reps else Reps
  jitter_seeds <- if (!is.null(x$jitter_seeds)) x$jitter_seeds else jitter_seeds
  jitter_amount <- if (!is.null(x$jitter_amount)) x$jitter_amount else jitter
  nsplit <- if (!is.null(x$nsplit)) x$nsplit else nsplit
  
  if(x$mfcl_commands == "./doitall.sh") {
    x$mfcl_commands <- x$mfcl_commands
  } else {
  x$mfcl_commands <- paste(prog_path, x$mfcl_commands)
  }
  x$model_dir <- paste0("model/", nm)
  x$base_dir <- b_dir
  x$program_path <- prog_path
  x$Reps <- Reps
  x$scalers <- scalers
  x$retro_peels <- retro_peels
  x$jitter_seeds <- jitter_seeds
  x$jitter_amount <- jitter_amount
  x$nsplit <- nsplit
  x
}, models, names(models))




