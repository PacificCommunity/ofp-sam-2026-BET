summary <-"4-region version of the 2023 BET diagnostic model"

source("../tools/model_defaults.R")

models <- list(
  
  "2023R4" = list(
    
    "description"="4-region version of the 2023 BET diagnostic model",
    
    mfcl_commands ="./doitall.sh",
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",  # Model-specific path
    base_dir = "mfcl/inputs/2023_rep",                   # Model-specific dir

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
        enabled = "TRUE",
        Reps = "15 25 25 500 500 200",
        scalars = paste0(seq(140, 60, by = -5), collapse = " "),
        Af172 = "0",
        Af173 = "0",
        Af174 = "0",
        prof_fix_indepvar = "",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        prof_extra_switch = ""
      ),
      LorenM = list(
        enabled = "TRUE",
        prof_fix_indepvar = "age_pars(5)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 121 0"
      )
      ,L1 = list(
        enabled = "TRUE",
        prof_fix_indepvar = "vb_coff(1)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 12 0"
      )
      ,L2 = list(
        enabled = "TRUE",
        prof_fix_indepvar = "vb_coff(2)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 13 0"
      )
      ,kappa = list(
        enabled = "TRUE",
        prof_fix_indepvar = "vb_coff(3)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 14 0"
      )
      ,s1 = list(
        enabled = "TRUE",
        prof_fix_indepvar = "var_coff(1)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 15 0"
      )
      ,s2 = list(
        enabled = "TRUE",
        prof_fix_indepvar = "var_coff(2)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "1 16 0"
      )
      ,totpop = list(
        enabled = "TRUE",
        prof_fix_indepvar = "totpop",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "2 32 0"
      )
      ,BetaScale = list(
        enabled = "TRUE",
        prof_fix_indepvar = "sv(21)",
        prof_fix_values = "",
        prof_fix_indepvar_file = "",
        scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
        indepvar_reps = "1000",
        prof_extra_switch = "2 146 0"
      )
      ,vb2d_example = list(
        enabled = "FALSE",
        prof_2d_indepvar = "vb_coff(1) vb_coff(2)",
        prof_2d_scalars_x = "80 90 100 110 120",
        prof_2d_scalars_y = "80 90 100 110 120",
        prof_2d_path = "axis_chains",
        prof_2d_anchor_x = "100",
        prof_2d_anchor_y = "100",
        prof_2d_parallel_jobs = "6",
        indepvar_reps = "1000",
        prof_2d_extra_switch = "1 12 0 1 13 0"
      )
      
    )
    # When using profile_sets, all profile-related fields (prof_fix_indepvar,
    # indepvar_reps, prof_extra_switch, etc.) should be specified per set.
    # Top-level fallbacks for those fields are not needed.

  )
  
  )




models <- apply_model_defaults(models)
ModelIDs <- names(models)
