summary <- "2023 BET diagnostic model: dedicated 2D indepvar profile config"

source("../tools/model_defaults.R")

models <- list(
  
  "2023diag_2d" = list(
    
    description = "2023 BET diagnostic model (2D indepvar profile example)",
    
    mfcl_commands = paste(
      "bet.frq 11.par 12.par",
      "-switch 1",
      "1 1 100",
      sep = " "
    ),
    program_path = "mfcl/exe/mfclo64_2026_02_04_vsn2278",
    base_dir = "mfcl/inputs/2023_rep",
    
    retro_peels = "1 2 3 4 5 6 7",
    n_mixing_periods = "2",
    min_year = "1952",
    
    jitter_seeds = paste0(1:30, collapse = " "),
    jitter_cv = "0.2",
    
    jitter_hessian = "0",
    model_hessian = "0",
    prof_hessian = "0",
    retro_hessian = "0",
    
    nsplit = "5",
    
    prof_init_map_rds = "",
    init_from_scalar_map = "",
    
    profile_sets = list(
      kappa_LorenM = list(
        enabled = "TRUE",
        prof_2d_indepvar = "vb_coff(3) age_pars(5)",
        prof_2d_scalars_x = paste0(seq(120, 80, by = -2.5), collapse = " "),
        prof_2d_scalars_y = paste0(seq(120, 80, by = -2.5), collapse = " "),
        prof_2d_path = "axis_chains",
        prof_2d_anchor_x = "100",
        prof_2d_anchor_y = "100",
        prof_2d_parallel_jobs = "4",
        indepvar_reps = "1000",
        prof_2d_extra_switch = "1 14 0 1 121 0"
      )
    )
  )
)

models <- apply_model_defaults(models)
ModelIDs <- names(models)
