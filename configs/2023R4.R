summary <- "4-region versions of the 2023 BET diagnostic model and sensitivities"

source("../tools/model_defaults.R")

program_path <- "mfcl/exe/mfclo64_2026_02_04_vsn2278"

clone_list <- function(x) {
  unserialize(serialize(x, NULL))
}

drop_profiles <- function(profile_sets, profile_names) {
  profile_sets[profile_names] <- NULL
  profile_sets
}

profile_sets_base <- list(
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
  ),
  L1 = list(
    enabled = "TRUE",
    prof_fix_indepvar = "vb_coff(1)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "1 12 0"
  ),
  L2 = list(
    enabled = "TRUE",
    prof_fix_indepvar = "vb_coff(2)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "1 13 0"
  ),
  kappa = list(
    enabled = "TRUE",
    prof_fix_indepvar = "vb_coff(3)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "1 14 0"
  ),
  s1 = list(
    enabled = "TRUE",
    prof_fix_indepvar = "var_coff(1)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "1 15 0"
  ),
  s2 = list(
    enabled = "TRUE",
    prof_fix_indepvar = "var_coff(2)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "1 16 0"
  ),
  totpop = list(
    enabled = "TRUE",
    prof_fix_indepvar = "totpop",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "2 32 0"
  ),
  BetaScale = list(
    enabled = "TRUE",
    prof_fix_indepvar = "sv(21)",
    prof_fix_values = "",
    prof_fix_indepvar_file = "",
    scalars = paste0(seq(120, 80, by = -2.5), collapse = " "),
    indepvar_reps = "1000",
    prof_extra_switch = "2 146 0"
  ),
  vb2d_example = list(
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

profile_sets_fixM <- drop_profiles(clone_list(profile_sets_base), "LorenM")
profile_sets_fixVB <- drop_profiles(clone_list(profile_sets_base), c("L1", "L2", "kappa"))
profile_sets_fixVB_M <- drop_profiles(clone_list(profile_sets_base), c("LorenM", "L1", "L2", "kappa", "BetaScale", "vb2d_example"))

make_model <- function(description, base_dir, profile_sets = profile_sets_base) {
  list(
    description = description,
    mfcl_commands = "./doitall.sh",
    program_path = program_path,
    base_dir = base_dir,
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
    profile_sets = clone_list(profile_sets)
  )
}

models <- list(
  "2023R4" = make_model(
    description = "4-region version of the 2023 BET diagnostic model",
    base_dir = "mfcl/inputs/2023_4region"
  ),
  "2023R4_fixVB" = make_model(
    description = "4-region model with growth parameters fixed at the 2023 diagnostic values",
    base_dir = "mfcl/inputs/2023_4region_fixVB",
    profile_sets = profile_sets_fixVB
  ),
  "2023R4_fixVB_M" = make_model(
    description = "4-region model with both M-at-age and growth parameters fixed at the 2023 diagnostic values",
    base_dir = "mfcl/inputs/2023_4region_fixVB_M",
    profile_sets = profile_sets_fixVB_M
  ),
  "2023R4_fixM" = make_model(
    description = "4-region model with M-at-age fixed at the 2023 diagnostic values",
    base_dir = "mfcl/inputs/2023_4region_fixM",
    profile_sets = profile_sets_fixM
  ),
  "2023R4_ExJPTP" = make_model(
    description = "4-region model excluding JPTP tags",
    base_dir = "mfcl/inputs/2023_4region_exclude_JPTP"
  ),
  "2023R4_ExPTTP" = make_model(
    description = "4-region model excluding PTTP tags",
    base_dir = "mfcl/inputs/2023_4region_exclude_PTTP"
  ),
  "2023R4_ExRG21_25" = make_model(
    description = "4-region model excluding release groups 21 and 25",
    base_dir = "mfcl/inputs/2023_4region_exclude_rg_21_25"
  ),
  "2023R4_ExRTTP" = make_model(
    description = "4-region model excluding RTTP tags",
    base_dir = "mfcl/inputs/2023_4region_exclude_RTTP"
  )
)

models <- apply_model_defaults(models)
ModelIDs <- names(models)
