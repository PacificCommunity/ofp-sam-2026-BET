summary <-"Impact of tagging programmes on estimates from the 2023 BET diagnostic model and the 6-region model."

source("../tools/model_defaults.R")

models <- list(
  
  "2023R9" = list(
    
    "description"="2023 BET diagnostic model (all tags included)",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "0",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  "2023R9_ExRTTP" = list(
    
    "description"="Exclude RTTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_RTTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  
  
  "2023R9_ExPTTP" = list(
    
    "description"="Exclude PTTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_PTTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 500 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  
  
  "2023R9_ExJPTP" = list(
    
    "description"="Exclude JPTP tags from 2023 BET diagnostic model",
    
    mfcl_commands = "./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep_exclude_JPTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 500 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  
  "2023R6" = list(
    
    "description"="6-region model using the doitall script from the 2023 BET legacy assessment",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_6region",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "0",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  "2023R6_ExRTTP" = list(
    
    "description"="Exclude RTTP tags from the 6-region model",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_6region_exclude_RTTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "0",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  "2023R6_ExPTTP" = list(
    
    "description"="Exclude PTTP tags from the 6-region model",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_6region_exclude_PTTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "0",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  
  "2023R6_ExJPTP" = list(
    
    "description"="Exclude JPTP tags from the 6-region model",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_6region_exclude_JPTP",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 1000 1000 500",
    scalers = paste0((seq(130, 70, by=-5)), collapse = " "),
    Af172 = "0",
    Af173 = "0",
    Af174 = "0",
    
    ## retrospective configuration
    retro_peels = "1 2 3 4 5 6 7",
    
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  )
  
  
  )


models <- apply_model_defaults(models)
ModelIDs <- names(models)
