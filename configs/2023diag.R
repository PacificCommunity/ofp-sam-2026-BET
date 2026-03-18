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
    Reps = "1 1 1 1 1 1",
    scalars = paste0((seq(140, 60, by=-5)), collapse = " "),
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
    jitter_seeds = paste0(1:10, collapse = " "),
    jitter_cv = "0.2",
    
    ## post-run hessian toggles
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    ## hessian parallel settings
    nsplit="5",
    
    
    prof_init_map_rds="",
    init_from_scalar_map="",

    # Profile launch sets:
    # - set enabled="TRUE" to launch that set
    # - standard uses prof_fix_indepvar=""
    # - each set can override profile-related fields independently
    #   (e.g., scalars, Reps, indepvar_reps, prof_extra_switch,
    #    prof_hessian, prof_fix_* ...)
    profile_sets = list(
      standard = list(
        enabled = "FALSE",
        prof_fix_indepvar = "",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(140, 60, by = -5), collapse = " "),
        prof_extra_switch = "1 121 0"
      ),
      age_pars_5 = list(
        enabled = "TRUE",
        prof_fix_indepvar = "age_pars(5)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "200",
        prof_extra_switch = "1 121 0"
      )
      # ,vb_coff_1 = list(
      #   enabled = "FALSE",
      #   prof_fix_indepvar = "vb_coff_1",
      #   prof_fix_values = "",
      #   prof_fix_indepvar_file = "",
      #   scalars = paste0(seq(110, 90, by = -2.5), collapse = " "),
      #   indepvar_reps = "100",
      #   prof_extra_switch = "1 121 0"
      # )
    ),

    # Backward-compatible top-level defaults (used when a profile set does not override them)
    prof_fix_indepvar="",
    prof_fix_values="",
    prof_fix_indepvar_file="",
    indepvar_reps="200",        # Default indepvar reps if not overridden in profile_sets
    prof_extra_switch="1 121 0"
    
  )
  
  )




models <- apply_model_defaults(models)
ModelIDs <- names(models)
