summary <-"Test configurations for MFCL models"

source("../tools/model_defaults.R")

models <- list(
  
  "mixP1" = list(
    
    "description"="mixing 1",
    
    mfcl_commands = paste("bet.frq 11.par 12.par",
                      "-switch 2",
                      "1 1 10000",
                      "-9999 1 1",
                      sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "2 2 2 2 2 2",
    scalars = paste0((seq(110, 90, by=-10)), collapse = " "),
    prof_fix_indepvar = "",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    indepvar_reps = "",           # Reps per optimizer call for indepvar fixed-parameter profile (blank = Reps4)
    prof_extra_switch = "",
    
    ## retrospective configuration
    retro_peels = "1 2",
    
    ## n_mixing_periods (this is only for retrospective runs, so should match with what is specified in doitall.sh)
    n_mixing_periods = "1",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:3, collapse = " "),
    jitter_cv = "0.05",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),
  
  "base" = list(
    
    
    "description"="base",
    
    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1",
                          "1 1 1", 
                          #   "-9999 1 2",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir
    
    ## configuration for profiling
    Reps = "2 2 2 2 2 2",
    scalars = paste0((seq(110, 90, by=-10)), collapse = " "),
    prof_fix_indepvar = "",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    indepvar_reps = "",           # Reps per optimizer call for indepvar fixed-parameter profile (blank = Reps4)
    prof_extra_switch = "",
    
    ## retrospective configuration
    retro_peels = "1 2 3",
    
    ## n_mixing_periods
    n_mixing_periods = "2",
    
    ## min_year
    min_year= "1952",
    
    ## Jitter settings
    jitter_seeds = paste0(1:2, collapse = " "),
    jitter_cv = "0.0001",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5"
  ),

  # Simultaneous operation example:
  # - base: standard profile outputs -> model/base/prof/
  # - base_indepvar: indepvar-fixed profile outputs -> model/base_indepvar/prof_indepvar/
  # Even with the same inputs/scalars, both results are shown together in shiny_plot.
  "base_indepvar" = list(

    "description"="base + indepvar-fixed profile",

    mfcl_commands = paste("bet.frq 11.par 12.par",
                          "-switch 1",
                          "1 1 1",
                          sep = " "),
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",
    base_dir = "mfcl/inputs/2023_rep",

    ## configuration for profiling
    Reps = "2 2 2 2 2 2",
    scalars = paste0((seq(110, 90, by=-10)), collapse = " "),
    prof_fix_indepvar = "M(1) M(2)",   # Specify by indepvar.rpt Var_name or Index
    # prof_fix_indepvar = "101 205",    # Alternative: specify by Index number
    prof_fix_values = "0.2 0.2",       # If empty, values are taken from the initial .par
    prof_fix_indepvar_file = "",       # Optionally set an explicit indepvar.rpt path
    indepvar_reps = "300",             # Reps per optimizer call (2 calls per scalar); blank = Reps4
    prof_extra_switch = "",            # Optionally append extra switch triplets

    ## retrospective configuration
    retro_peels = "1 2 3",

    ## n_mixing_periods
    n_mixing_periods = "2",

    ## min_year
    min_year= "1952",

    ## Jitter settings
    jitter_seeds = paste0(1:2, collapse = " "),
    jitter_cv = "0.0001",

    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",

    ## hessian parallel settings
    nsplit="5"
  ))





models <- apply_model_defaults(models)
ModelIDs <- names(models)
