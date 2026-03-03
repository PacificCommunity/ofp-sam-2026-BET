summary <-"Assessing the impact of tagging data on the 2023 BET diagnostic model across tagging programmes"

source("../tools/model_defaults.R")

models <- list(
  
  "2023ExRTTP" = list(
    
    "description"="Exclude RTTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_RTTP",                   # Model-specific dir
    
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
  ),
  
  
  
  "2023ExPTTP" = list(
    
    "description"="Exclude RTTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_PTTP",                   # Model-specific dir
    
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
  ),
  
  
  
  "2023ExJPTP" = list(
    
    "description"="Exclude JPTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_JPTP",                   # Model-specific dir
    
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




models <- apply_model_defaults(models)
ModelIDs <- names(models)


