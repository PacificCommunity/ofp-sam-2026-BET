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

first_nonempty_model_value <- function(...) {
  for (x in list(...)) {
    if (!is.null(x) && length(x) > 0 && nzchar(trimws(as.character(x[[1]])))) {
      return(trimws(as.character(x[[1]])))
    }
  }
  ""
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

infer_movement_pairs_from_name <- function(name) {
  hit <- regmatches(name, regexpr("movement_R[0-9]+_R[0-9]+(_R[0-9]+_R[0-9]+)*", name))
  if (length(hit) == 0 || is.na(hit) || !nzchar(hit)) return("")
  nums <- suppressWarnings(as.integer(regmatches(hit, gregexpr("[0-9]+", hit))[[1]]))
  if (length(nums) < 2 || length(nums) %% 2 != 0) return("")
  pairs <- paste(nums[seq(1, length(nums), by = 2)], nums[seq(2, length(nums), by = 2)], sep = "-")
  paste(pairs, collapse = ",")
}

infer_sel_nodes_from_name <- function(name) {
  hit <- regmatches(name, regexpr("sel_spline[0-9]+", name))
  if (length(hit) == 0 || is.na(hit) || !nzchar(hit)) return("")
  sub("^sel_spline", "", hit)
}

strip_recipe_suffixes_from_name <- function(name, movement_pairs, sel_nodes, index_cv_half) {
  if (identical(index_cv_half, "1")) name <- sub("_index_cv_half$", "", name)
  if (nzchar(sel_nodes)) name <- sub(paste0("_sel_spline", sel_nodes, "$"), "", name)
  if (nzchar(movement_pairs)) {
    movement_suffix <- paste0(
      "movement_",
      paste(
        vapply(strsplit(movement_pairs, ",")[[1]], function(pair) {
          vals <- trimws(strsplit(pair, "-", fixed = TRUE)[[1]])
          paste0("R", vals[[1]], "_R", vals[[2]])
        }, character(1)),
        collapse = "_"
      )
    )
    name <- sub(paste0("_", movement_suffix, "$"), "", name)
  }
  name
}

infer_base_tokens_from_name <- function(name) {
  if (grepl("fixVB_M", name, fixed = TRUE)) {
    c("fixVB", "fixM")
  } else if (grepl("fixVB", name, fixed = TRUE)) {
    "fixVB"
  } else if (grepl("fixM", name, fixed = TRUE)) {
    "fixM"
  } else {
    character(0)
  }
}

infer_input_recipe <- function(base_dir) {
  base_dir_chr <- if (is.null(base_dir) || length(base_dir) == 0) "" else as.character(base_dir[[1]])
  b <- basename(base_dir_chr)
  movement_pairs <- infer_movement_pairs_from_name(b)
  sel_nodes <- infer_sel_nodes_from_name(b)
  index_cv_half <- if (grepl("index_cv_half", b, fixed = TRUE)) "1" else "0"
  source_name <- strip_recipe_suffixes_from_name(b, movement_pairs, sel_nodes, index_cv_half)
  source_dir <- file.path(dirname(base_dir_chr), source_name)
  base_tokens <- infer_base_tokens_from_name(source_name)

  list(
    input_recipe_enabled = if (nzchar(movement_pairs) || nzchar(sel_nodes) || identical(index_cv_half, "1")) "1" else "0",
    input_recipe_base = if (length(base_tokens) > 0) paste(base_tokens, collapse = ",") else "base",
    input_recipe_base_input_dir = source_dir,
    input_recipe_base_source = source_dir,
    input_recipe_base_tokens = paste(base_tokens, collapse = ","),
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
      input_recipe_builder = "tools/input_sensitivities/build_input_recipe.R",
      input_recipe_base = "",
      input_recipe_base_input_dir = "",
      input_recipe_base_source = "",
      input_recipe_base_tokens = "",
      input_recipe_output_dir = "",
      input_recipe_movement_pairs = "",
      input_recipe_sel_nodes = "",
      input_recipe_index_cv_half = ""
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
    inferred_recipe <- infer_input_recipe(base_dir)
    build_inputs_on_missing <- if (!is.null(model$build_inputs_on_missing)) model$build_inputs_on_missing else defaults$build_inputs_on_missing
    input_recipe_enabled <- if (!is.null(model$input_recipe_enabled)) model$input_recipe_enabled else defaults$input_recipe_enabled
    input_recipe_builder <- if (!is.null(model$input_recipe_builder)) model$input_recipe_builder else defaults$input_recipe_builder
    input_recipe_base <- if (!is.null(model$input_recipe_base)) model$input_recipe_base else defaults$input_recipe_base
    input_recipe_base_input_dir <- if (!is.null(model$input_recipe_base_input_dir)) model$input_recipe_base_input_dir else defaults$input_recipe_base_input_dir
    input_recipe_base_source <- if (!is.null(model$input_recipe_base_source)) model$input_recipe_base_source else defaults$input_recipe_base_source
    input_recipe_base_tokens <- if (!is.null(model$input_recipe_base_tokens)) model$input_recipe_base_tokens else defaults$input_recipe_base_tokens
    input_recipe_output_dir <- if (!is.null(model$input_recipe_output_dir)) model$input_recipe_output_dir else defaults$input_recipe_output_dir
    input_recipe_movement_pairs <- if (!is.null(model$input_recipe_movement_pairs)) model$input_recipe_movement_pairs else defaults$input_recipe_movement_pairs
    input_recipe_sel_nodes <- if (!is.null(model$input_recipe_sel_nodes)) model$input_recipe_sel_nodes else defaults$input_recipe_sel_nodes
    input_recipe_index_cv_half <- if (!is.null(model$input_recipe_index_cv_half)) model$input_recipe_index_cv_half else defaults$input_recipe_index_cv_half

    if (identical(input_recipe_enabled, "auto")) input_recipe_enabled <- inferred_recipe$input_recipe_enabled
    if (!nzchar(as.character(input_recipe_base))) input_recipe_base <- inferred_recipe$input_recipe_base
    if (!nzchar(as.character(input_recipe_base_input_dir))) input_recipe_base_input_dir <- first_nonempty_model_value(
      input_recipe_base_source,
      inferred_recipe$input_recipe_base_input_dir
    )
    if (!nzchar(as.character(input_recipe_base_source))) input_recipe_base_source <- inferred_recipe$input_recipe_base_source
    if (!nzchar(as.character(input_recipe_base_tokens))) input_recipe_base_tokens <- inferred_recipe$input_recipe_base_tokens
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
    model$input_recipe_base_input_dir <- input_recipe_base_input_dir
    model$input_recipe_base_source <- input_recipe_base_source
    model$input_recipe_base_tokens <- input_recipe_base_tokens
    model$input_recipe_output_dir <- input_recipe_output_dir
    model$input_recipe_movement_pairs <- input_recipe_movement_pairs
    model$input_recipe_sel_nodes <- input_recipe_sel_nodes
    model$input_recipe_index_cv_half <- input_recipe_index_cv_half

    if (profile_sets_all_define(profile_sets, "scalars")) {
      model$scalars <- NULL
    }
    if (profile_sets_cover_reps(profile_sets)) {
      model$Reps <- NULL
    }

    model
  }, models, names(models))
}
