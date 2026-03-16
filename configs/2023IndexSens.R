summary <-"Investigating the influence of the model on the R9 index"

source("../tools/model_defaults.R")

models <- list(
  
  "2023_ExR9Index" = list(
    
    "description"="Relaxed R9 index by assuming a very high CV",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_exclude_R9index",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "15 25 25 300 300 300",
    scalars = paste0((seq(150, 50, by=-5)), collapse = " "),
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
    jitter_seeds = paste0(1:50, collapse = " "),
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
