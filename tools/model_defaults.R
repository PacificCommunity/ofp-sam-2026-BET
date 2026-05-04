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

infer_4region_input_recipe <- function(base_dir) {
  base_dir_chr <- if (is.null(base_dir) || length(base_dir) == 0) "" else as.character(base_dir[[1]])
  b <- basename(base_dir_chr)
  is_4region <- grepl("^2023_4region", b)

  recipe_base <- if (grepl("fixVB_M", b, fixed = TRUE)) {
    "fixVB_M"
  } else if (grepl("fixVB", b, fixed = TRUE)) {
    "fixVB"
  } else if (grepl("fixM", b, fixed = TRUE)) {
    "fixM"
  } else {
    "base"
  }

  movement_pairs <- if (grepl("movement_R1_R2_R1_R3_R2_R3", b, fixed = TRUE)) {
    "1-2,1-3,2-3"
  } else if (grepl("movement_R2_R3", b, fixed = TRUE)) {
    "2-3"
  } else {
    ""
  }

  sel_nodes <- if (grepl("sel_spline4", b, fixed = TRUE)) "4" else ""
  index_cv_half <- if (grepl("index_cv_half", b, fixed = TRUE)) "1" else "0"

  list(
    input_recipe_enabled = if (is_4region) "1" else "0",
    input_recipe_base = recipe_base,
    input_recipe_output_dir = base_dir_chr,
    input_recipe_movement_pairs = movement_pairs,
    input_recipe_sel_nodes = sel_nodes,
    input_recipe_index_cv_half = index_cv_half
  )
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
      prof_2d_indepvar = "",
      prof_2d_scalars_x = "",
      prof_2d_scalars_y = "",
      prof_2d_values_x = "",
      prof_2d_values_y = "",
      prof_2d_extra_switch = "",
      prof_2d_path = "snake_x",
      prof_2d_anchor_x = "",
      prof_2d_anchor_y = "",
      prof_2d_parallel_jobs = "",
      profile_sets = list(),
      indepvar_reps = "",
      prof_extra_switch = "",
      retro_hessian = "0",
      nsplit = "5",
      build_inputs_on_missing = "1",
      input_recipe_enabled = "auto",
      input_recipe_builder = "tools/build_4region_input_recipe.R",
      input_recipe_base = "",
      input_recipe_base_source = "",
      input_recipe_output_dir = "",
      input_recipe_movement_pairs = "",
      input_recipe_sel_nodes = "",
      input_recipe_index_cv_half = "",
      input_recipe_release_regions = "9",
      input_recipe_with_11par = "1"
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
    prof_2d_indepvar <- if (!is.null(model$prof_2d_indepvar)) model$prof_2d_indepvar else defaults$prof_2d_indepvar
    prof_2d_scalars_x <- if (!is.null(model$prof_2d_scalars_x)) model$prof_2d_scalars_x else defaults$prof_2d_scalars_x
    prof_2d_scalars_y <- if (!is.null(model$prof_2d_scalars_y)) model$prof_2d_scalars_y else defaults$prof_2d_scalars_y
    prof_2d_values_x <- if (!is.null(model$prof_2d_values_x)) model$prof_2d_values_x else defaults$prof_2d_values_x
    prof_2d_values_y <- if (!is.null(model$prof_2d_values_y)) model$prof_2d_values_y else defaults$prof_2d_values_y
    prof_2d_extra_switch <- if (!is.null(model$prof_2d_extra_switch)) model$prof_2d_extra_switch else defaults$prof_2d_extra_switch
    prof_2d_path <- if (!is.null(model$prof_2d_path)) model$prof_2d_path else defaults$prof_2d_path
    prof_2d_anchor_x <- if (!is.null(model$prof_2d_anchor_x)) model$prof_2d_anchor_x else defaults$prof_2d_anchor_x
    prof_2d_anchor_y <- if (!is.null(model$prof_2d_anchor_y)) model$prof_2d_anchor_y else defaults$prof_2d_anchor_y
    prof_2d_parallel_jobs <- if (!is.null(model$prof_2d_parallel_jobs)) model$prof_2d_parallel_jobs else defaults$prof_2d_parallel_jobs
    profile_sets <- if (!is.null(model$profile_sets)) model$profile_sets else defaults$profile_sets
    indepvar_reps <- if (!is.null(model$indepvar_reps)) model$indepvar_reps else defaults$indepvar_reps
    prof_extra_switch <- if (!is.null(model$prof_extra_switch)) model$prof_extra_switch else defaults$prof_extra_switch
    retro_hessian <- if (!is.null(model$retro_hessian)) model$retro_hessian else defaults$retro_hessian
    nsplit <- if (!is.null(model$nsplit)) model$nsplit else defaults$nsplit
    inferred_recipe <- infer_4region_input_recipe(base_dir)
    build_inputs_on_missing <- if (!is.null(model$build_inputs_on_missing)) model$build_inputs_on_missing else defaults$build_inputs_on_missing
    input_recipe_enabled <- if (!is.null(model$input_recipe_enabled)) model$input_recipe_enabled else defaults$input_recipe_enabled
    input_recipe_builder <- if (!is.null(model$input_recipe_builder)) model$input_recipe_builder else defaults$input_recipe_builder
    input_recipe_base <- if (!is.null(model$input_recipe_base)) model$input_recipe_base else defaults$input_recipe_base
    input_recipe_base_source <- if (!is.null(model$input_recipe_base_source)) model$input_recipe_base_source else defaults$input_recipe_base_source
    input_recipe_output_dir <- if (!is.null(model$input_recipe_output_dir)) model$input_recipe_output_dir else defaults$input_recipe_output_dir
    input_recipe_movement_pairs <- if (!is.null(model$input_recipe_movement_pairs)) model$input_recipe_movement_pairs else defaults$input_recipe_movement_pairs
    input_recipe_sel_nodes <- if (!is.null(model$input_recipe_sel_nodes)) model$input_recipe_sel_nodes else defaults$input_recipe_sel_nodes
    input_recipe_index_cv_half <- if (!is.null(model$input_recipe_index_cv_half)) model$input_recipe_index_cv_half else defaults$input_recipe_index_cv_half
    input_recipe_release_regions <- if (!is.null(model$input_recipe_release_regions)) model$input_recipe_release_regions else defaults$input_recipe_release_regions
    input_recipe_with_11par <- if (!is.null(model$input_recipe_with_11par)) model$input_recipe_with_11par else defaults$input_recipe_with_11par

    if (identical(input_recipe_enabled, "auto")) input_recipe_enabled <- inferred_recipe$input_recipe_enabled
    if (!nzchar(as.character(input_recipe_base))) input_recipe_base <- inferred_recipe$input_recipe_base
    if (!nzchar(as.character(input_recipe_output_dir))) input_recipe_output_dir <- inferred_recipe$input_recipe_output_dir
    if (!nzchar(as.character(input_recipe_movement_pairs))) input_recipe_movement_pairs <- inferred_recipe$input_recipe_movement_pairs
    if (!nzchar(as.character(input_recipe_sel_nodes))) input_recipe_sel_nodes <- inferred_recipe$input_recipe_sel_nodes
    if (!nzchar(as.character(input_recipe_index_cv_half))) input_recipe_index_cv_half <- inferred_recipe$input_recipe_index_cv_half

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
    model$prof_2d_indepvar <- prof_2d_indepvar
    model$prof_2d_scalars_x <- prof_2d_scalars_x
    model$prof_2d_scalars_y <- prof_2d_scalars_y
    model$prof_2d_values_x <- prof_2d_values_x
    model$prof_2d_values_y <- prof_2d_values_y
    model$prof_2d_extra_switch <- prof_2d_extra_switch
    model$prof_2d_path <- prof_2d_path
    model$prof_2d_anchor_x <- prof_2d_anchor_x
    model$prof_2d_anchor_y <- prof_2d_anchor_y
    model$prof_2d_parallel_jobs <- prof_2d_parallel_jobs
    model$profile_sets <- profile_sets
    model$indepvar_reps <- indepvar_reps
    model$prof_extra_switch <- prof_extra_switch
    model$retro_hessian <- retro_hessian
    model$nsplit <- nsplit
    model$build_inputs_on_missing <- build_inputs_on_missing
    model$input_recipe_enabled <- input_recipe_enabled
    model$input_recipe_builder <- input_recipe_builder
    model$input_recipe_base <- input_recipe_base
    model$input_recipe_base_source <- input_recipe_base_source
    model$input_recipe_output_dir <- input_recipe_output_dir
    model$input_recipe_movement_pairs <- input_recipe_movement_pairs
    model$input_recipe_sel_nodes <- input_recipe_sel_nodes
    model$input_recipe_index_cv_half <- input_recipe_index_cv_half
    model$input_recipe_release_regions <- input_recipe_release_regions
    model$input_recipe_with_11par <- input_recipe_with_11par

    if (profile_sets_all_define(profile_sets, "scalars")) {
      model$scalars <- NULL
    }
    if (profile_sets_cover_reps(profile_sets)) {
      model$Reps <- NULL
    }

    model
  }, models, names(models))
}
