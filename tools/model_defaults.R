profile_sets_all_define <- function(profile_sets, field) {
  if (is.null(profile_sets) || length(profile_sets) == 0) return(FALSE)

  enabled_sets <- Filter(function(spec) {
    if (!is.list(spec)) return(FALSE)
    enabled_raw <- spec$enabled
    if (is.null(enabled_raw) || length(enabled_raw) == 0) return(TRUE)
    enabled_chr <- tolower(trimws(as.character(enabled_raw[[1]])))
    !(identical(enabled_raw[[1]], FALSE) || enabled_chr %in% c("0", "false", "no", "off"))
  }, profile_sets)

  if (length(enabled_sets) == 0) return(FALSE)

  all(vapply(enabled_sets, function(spec) {
    val <- spec[[field]]
    !is.null(val) && length(val) > 0 && nzchar(trimws(paste(as.character(val), collapse = " ")))
  }, logical(1)))
}

profile_sets_cover_reps <- function(profile_sets) {
  if (is.null(profile_sets) || length(profile_sets) == 0) return(FALSE)

  enabled_sets <- Filter(function(spec) {
    if (!is.list(spec)) return(FALSE)
    enabled_raw <- spec$enabled
    if (is.null(enabled_raw) || length(enabled_raw) == 0) return(TRUE)
    enabled_chr <- tolower(trimws(as.character(enabled_raw[[1]])))
    !(identical(enabled_raw[[1]], FALSE) || enabled_chr %in% c("0", "false", "no", "off"))
  }, profile_sets)

  if (length(enabled_sets) == 0) return(FALSE)

  has_value <- function(x) {
    !is.null(x) && length(x) > 0 && nzchar(trimws(paste(as.character(x), collapse = " ")))
  }

  all(vapply(enabled_sets, function(spec) {
    has_value(spec$Reps) || has_value(spec$indepvar_reps)
  }, logical(1)))
}

apply_model_defaults <- function(models, defaults = list()) {
  stopifnot(is.list(models))

  defaults <- modifyList(
    list(
      program_path = "mfcl/exe/mfclo64_2023",
      base_dir = "mfcl/inputs/2023_rep",
      Reps = "15 25 25 1000 500 500",
      scalars = paste0(seq(140, 50, by = -5), collapse = " "),
      retro_peels = "1 2 3 4 5",
      jitter_seeds = paste0(1:10, collapse = " "),
      jitter_cv = "0.2",
      jitter_hessian = "0",
      model_hessian = "0",
      prof_hessian = "0",
      prof_init_map_rds = "",
      init_from_scalar_map = "",
      init_par_override_map = "",
      prof_fix_indepvar = "",
      prof_fix_values = "",
      prof_fix_indepvar_file = "",
      profile_sets = list(),
      indepvar_reps = "",
      prof_extra_switch = "",
      retro_hessian = "0",
      nsplit = "5"
    ),
    defaults
  )

  Map(function(model, model_name) {
    program_path <- if (!is.null(model$program_path)) model$program_path else defaults$program_path
    base_dir <- if (!is.null(model$base_dir)) model$base_dir else defaults$base_dir
    reps <- if (!is.null(model$Reps)) model$Reps else defaults$Reps
    scalars <- if (!is.null(model$scalars)) model$scalars else defaults$scalars
    retro_peels <- if (!is.null(model$retro_peels)) model$retro_peels else defaults$retro_peels
    jitter_seeds <- if (!is.null(model$jitter_seeds)) model$jitter_seeds else defaults$jitter_seeds
    jitter_cv <- if (!is.null(model$jitter_cv)) model$jitter_cv else defaults$jitter_cv
    jitter_hessian <- if (!is.null(model$jitter_hessian)) model$jitter_hessian else defaults$jitter_hessian
    model_hessian <- if (!is.null(model$model_hessian)) model$model_hessian else defaults$model_hessian
    prof_hessian <- if (!is.null(model$prof_hessian)) model$prof_hessian else defaults$prof_hessian
    prof_init_map_rds <- if (!is.null(model$prof_init_map_rds)) model$prof_init_map_rds else defaults$prof_init_map_rds
    init_from_scalar_map <- if (!is.null(model$init_from_scalar_map)) model$init_from_scalar_map else defaults$init_from_scalar_map
    init_par_override_map <- if (!is.null(model$init_par_override_map)) model$init_par_override_map else defaults$init_par_override_map
    prof_fix_indepvar <- if (!is.null(model$prof_fix_indepvar)) model$prof_fix_indepvar else defaults$prof_fix_indepvar
    prof_fix_values <- if (!is.null(model$prof_fix_values)) model$prof_fix_values else defaults$prof_fix_values
    prof_fix_indepvar_file <- if (!is.null(model$prof_fix_indepvar_file)) model$prof_fix_indepvar_file else defaults$prof_fix_indepvar_file
    profile_sets <- if (!is.null(model$profile_sets)) model$profile_sets else defaults$profile_sets
    indepvar_reps <- if (!is.null(model$indepvar_reps)) model$indepvar_reps else defaults$indepvar_reps
    prof_extra_switch <- if (!is.null(model$prof_extra_switch)) model$prof_extra_switch else defaults$prof_extra_switch
    retro_hessian <- if (!is.null(model$retro_hessian)) model$retro_hessian else defaults$retro_hessian
    nsplit <- if (!is.null(model$nsplit)) model$nsplit else defaults$nsplit

    if (!identical(model$mfcl_commands, "./doitall.sh")) {
      model$mfcl_commands <- paste(program_path, model$mfcl_commands)
    }

    model$model_dir <- file.path("model", model_name)
    model$base_dir <- base_dir
    model$program_path <- program_path
    model$Reps <- reps
    model$scalars <- scalars
    model$retro_peels <- retro_peels
    model$jitter_seeds <- jitter_seeds
    model$jitter_cv <- jitter_cv
    model$jitter_hessian <- jitter_hessian
    model$model_hessian <- model_hessian
    model$prof_hessian <- prof_hessian
    model$prof_init_map_rds <- prof_init_map_rds
    model$init_from_scalar_map <- init_from_scalar_map
    model$init_par_override_map <- init_par_override_map
    model$prof_fix_indepvar <- prof_fix_indepvar
    model$prof_fix_values <- prof_fix_values
    model$prof_fix_indepvar_file <- prof_fix_indepvar_file
    model$profile_sets <- profile_sets
    model$indepvar_reps <- indepvar_reps
    model$prof_extra_switch <- prof_extra_switch
    model$retro_hessian <- retro_hessian
    model$nsplit <- nsplit

    if (profile_sets_all_define(profile_sets, "scalars")) {
      model$scalars <- NULL
    }
    if (profile_sets_cover_reps(profile_sets)) {
      model$Reps <- NULL
    }

    model
  }, models, names(models))
}
