summary <-"replicate the 2023 diagnostic model"

source("../tools/model_defaults.R")

models <- list(
  
  "2023diag" = list(
    
    "description"="2023 BET diagnostic model",
    
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1",
                          "1 1 100",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 500 500",
    scalers = paste0((seq(140, 60, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "50",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:50, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "1",
    model_hessian = "1",
    prof_hessian = "1",
    retro_hessian = "1",
    
    ## hessian parallel settings
    nsplit="5"
  )
  
  )




models <- apply_model_defaults(models)
ModelIDs <- names(models)
