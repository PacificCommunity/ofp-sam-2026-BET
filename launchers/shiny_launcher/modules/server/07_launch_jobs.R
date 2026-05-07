  # ========== LAUNCH JOB HANDLERS ==========

  local_env_strings <- function(job_env) {
    env_names <- names(job_env)
    if (is.null(env_names) || length(env_names) == 0) {
      return(character(0))
    }
    vals <- vapply(job_env, function(x) {
      if (length(x) == 0 || is.null(x)) {
        ""
      } else {
        paste(as.character(x), collapse = " ")
      }
    }, character(1))
    paste0(env_names, "=", shQuote(vals))
  }

  local_job_runner <- function(job_type) {
    switch(
      job_type,
      stage_check = "tools/condor_stage_check.R",
      model = "runners/run_model.R",
      jitter = "runners/run_jitter.R",
      hessian = "runners/run_hessian.R",
      retro = "runners/run_retro.R",
      selftest = "runners/run_selftest.R",
      prof = "runners/run_prof.R",
      prof_chain = "runners/run_prof_chain.R",
      prof_2d = "runners/run_prof_2d.R",
      stop("Unsupported local job type: ", job_type)
    )
  }

  parse_prof_target_map <- function(txt) {
    out <- list()
    if (is.null(txt) || !nzchar(trimws(txt))) return(out)
    parts <- trimws(unlist(strsplit(as.character(txt), ",")))
    parts <- parts[nzchar(parts)]
    for (p in parts) {
      kv <- trimws(unlist(strsplit(p, ":", fixed = TRUE)))
      if (length(kv) < 2) next
      k <- suppressWarnings(as.numeric(kv[1]))
      v <- paste(kv[-1], collapse = ":")
      if (is.finite(k) && nzchar(v)) out[[format(k, scientific = FALSE, trim = TRUE)]] <- v
    }
    out
  }

  parse_prof_target_scalars <- function(txt) {
    mp <- parse_prof_target_map(txt)
    if (length(mp) == 0) return(numeric(0))
    keys <- suppressWarnings(as.numeric(names(mp)))
    keys[is.finite(keys)]
  }

  parse_first_nonempty_numeric <- function(...) {
    items <- list(...)
    for (it in items) {
      vals <- parse_numeric_tokens(it)
      if (length(vals) > 0) return(vals)
    }
    numeric(0)
  }

  first_scalar_string <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    txt <- trimws(as.character(x[[1]]))
    if (!nzchar(txt)) default else txt
  }

  truthy_scalar <- function(x, default = FALSE) {
    if (is.null(x) || length(x) == 0) return(default)
    txt <- tolower(trimws(as.character(x[[1]])))
    if (!nzchar(txt)) return(default)
    txt %in% c("1", "true", "yes", "y", "on")
  }

  launch_prefer_par_start <- function() {
    if (is.null(input$prefer_par_start)) return(TRUE)
    isTRUE(input$prefer_par_start)
  }

  set_prefer_par_start_env <- function(job_env, prefer = TRUE) {
    job_env$prefer_par_start <- if (isTRUE(prefer)) "1" else "0"
    job_env
  }

  positive_int_input <- function(input_id, default, min_value = 1L) {
    value <- tryCatch(input[[input_id]], error = function(e) NULL)
    value <- suppressWarnings(as.integer(value))
    if (!is.finite(value) || value < min_value) value <- as.integer(default)
    if (!is.finite(value) || value < min_value) value <- as.integer(min_value)
    as.integer(value)
  }

  selftest_reps_for_launch <- function() {
    n <- positive_int_input("selftest_reps_n", default = 20L)
    as.integer(seq_len(n))
  }

  selftest_output_dir_for_env <- function(model_name, model_env) {
    configured <- first_scalar_string(model_env$selftest_dir, default = "")
    if (nzchar(configured)) return(configured)
    requested <- first_scalar_string(input$selftest_output_dir, default = "")
    if (nzchar(requested)) return(requested)
    file.path(selftest_model_dir_for_env(model_name, model_env), "selftest")
  }

  selftest_model_dir_for_env <- function(model_name, model_env) {
    configured <- first_scalar_string(model_env$selftest_model_dir, default = "")
    if (nzchar(configured)) return(configured)
    model_dir <- first_scalar_string(model_env$model_dir, default = "")
    if (nzchar(model_dir)) return(model_dir)
    file.path("model", canonical_launch_basename(launch_model_name(model_name, model_env)))
  }

  set_selftest_env_pair <- function(env, key, value) {
    env[[key]] <- as.character(value)
    env[[toupper(key)]] <- as.character(value)
    env
  }

	  apply_selftest_option_overrides <- function(model_name, model_env) {
	    if (is.null(model_env)) return(model_env)
	    env <- as.list(model_env, all.names = TRUE)

    base_dir <- first_scalar_string(env$selftest_base_dir, default = "")
    if (!nzchar(base_dir)) base_dir <- first_scalar_string(env$base_dir, default = "")
    if (nzchar(base_dir)) {
      env <- set_selftest_env_pair(env, "selftest_base_dir", base_dir)
    }

    env <- set_selftest_env_pair(env, "selftest_model_dir", selftest_model_dir_for_env(model_name, env))

    source_par <- first_scalar_string(env$selftest_source_par, default = "")
    if (nzchar(source_par)) {
      env <- set_selftest_env_pair(env, "selftest_source_par", source_par)
    }

	    source_mode <- first_scalar_string(input$selftest_source_mode, default = "last_par")
	    if (!source_mode %in% c("last_par", "doitall")) source_mode <- "last_par"
	    refit_mode <- first_scalar_string(input$selftest_refit_mode, default = "last_par")
	    if (!refit_mode %in% c("last_par", "doitall")) refit_mode <- "last_par"

	    env <- set_selftest_env_pair(env, "selftest_dir", selftest_output_dir_for_env(model_name, env))
	    env <- set_selftest_env_pair(env, "selftest_source_mode", source_mode)
	    env <- set_selftest_env_pair(env, "selftest_refit_mode", refit_mode)
	    if (identical(refit_mode, "last_par")) {
	      env <- set_selftest_env_pair(env, "selftest_refit_fevals", positive_int_input("selftest_refit_fevals", default = 20L))
	    }
	    env <- set_selftest_env_pair(env, "selftest_require_native_tags", "1")
	    env <- set_selftest_env_pair(env, "selftest_update_catch", "auto")
	    env <- set_selftest_env_pair(env, "selftest_update_effort", "auto")

    projection_years <- first_scalar_string(input$selftest_projection_years, default = "")
    if (nzchar(projection_years)) {
      env <- set_selftest_env_pair(env, "selftest_native_tag_projection_years", projection_years)
    }

    env
  }

  apply_selftest_rep_to_env <- function(job_env, selftest_rep = NULL) {
    if (is.null(selftest_rep)) return(job_env)
    rep_txt <- as.character(as.integer(selftest_rep))
    job_env$selftest_reps <- rep_txt
    job_env$SELFTEST_REPS <- rep_txt
    job_env
  }

  launch_job_sequence <- function(n) {
    n <- suppressWarnings(as.integer(n))
    if (!is.finite(n) || n < 1L) return("")
    paste(seq_len(n), collapse = " ")
  }

  launch_hessian_parallel <- function() {
    value <- tryCatch(input$hessian_parallel, error = function(e) NULL)
    if (is.null(value)) return(TRUE)
    isTRUE(value)
  }

  apply_launch_job_option_overrides <- function(model_env) {
    if (is.null(model_env)) return(model_env)
    env <- as.list(model_env, all.names = TRUE)

    jitter_n <- positive_int_input("job_jitter_n", default = 30L)
    retro_n <- positive_int_input("retro_peels_n", default = 7L)
    hessian_parallel <- launch_hessian_parallel()
    hessian_nsplit <- if (isTRUE(hessian_parallel)) {
      positive_int_input("hessian_nsplit", default = 5L)
    } else {
      1L
    }

    env$jitter_seeds <- launch_job_sequence(jitter_n)
    env$retro_peels <- launch_job_sequence(retro_n)
    env$nsplit <- as.character(hessian_nsplit)
    env$launcher_jitter_runs <- as.character(jitter_n)
    env$launcher_retro_peels <- as.character(retro_n)
    env$launcher_hessian_parallel <- if (isTRUE(hessian_parallel)) "1" else "0"
    env$launcher_hessian_parts <- as.character(hessian_nsplit)
    env
  }

  # ----- Input and sensitivity launch units -----
  # The config file now supplies one common set of launch defaults. The actual
  # launch units are discovered input folders, with optional on-the-fly
  # sensitivity recipes layered on top.

  if (!exists("launch_input_mode", mode = "function")) {
    launch_input_mode <- function() {
      mode <- first_scalar_string(input$input_launch_mode, default = "existing")
      if (mode %in% c("existing", "sensitivity")) mode else "existing"
    }
  }

  if (!exists("sensitivity_mode_enabled", mode = "function")) {
    sensitivity_mode_enabled <- function() {
      identical(launch_input_mode(), "sensitivity")
    }
  }

  movement_suffix_from_pairs <- function(movement_pairs) {
    movement_pairs <- trimws(as.character(movement_pairs[[1]]))
    if (!nzchar(movement_pairs)) return("")
    pairs <- trimws(unlist(strsplit(movement_pairs, "[,[:space:]]+")))
    pairs <- pairs[nzchar(pairs)]
    if (length(pairs) == 0) return("")
    paste0(
      "movement_",
      paste(vapply(pairs, function(pair) {
        vals <- trimws(strsplit(pair, "-", fixed = TRUE)[[1]])
        if (length(vals) != 2L) return("")
        paste0("R", vals[[1]], "_R", vals[[2]])
      }, character(1)), collapse = "_")
    )
  }

  strip_recipe_suffixes <- function(path, fixed_params = "", movement_pairs = "", sel_nodes = "", index_cv_half = "0") {
    parent <- dirname(path)
    stem <- basename(path)
    if (truthy_scalar(index_cv_half)) stem <- sub("_index_cv_half$", "", stem)
    if (nzchar(sel_nodes)) stem <- sub(paste0("_sel_spline", sel_nodes, "$"), "", stem)
    movement_suffix <- movement_suffix_from_pairs(movement_pairs)
    if (nzchar(movement_suffix)) stem <- sub(paste0("_", movement_suffix, "$"), "", stem)
    fixed_vals <- toupper(trimws(unlist(strsplit(as.character(fixed_params), "[,;[:space:]+]+", perl = TRUE), use.names = FALSE)))
    fixed_vals <- fixed_vals[nzchar(fixed_vals)]
    if ("VBM" %in% fixed_vals || all(c("VB", "M") %in% fixed_vals)) {
      stem <- sub("_fixM_fixVB$", "", stem)
      stem <- sub("_fixVB_fixM$", "", stem)
      stem <- sub("_fixVBM$", "", stem)
      stem <- sub("_fixVB_M$", "", stem)
    } else {
      if ("VB" %in% fixed_vals) stem <- sub("_fixVB$", "", stem)
      if ("M" %in% fixed_vals) stem <- sub("_fixM$", "", stem)
    }
    file.path(parent, stem)
  }

  infer_recipe_base_from_env <- function(model_env) {
    configured <- first_scalar_string(model_env$input_recipe_base, default = "")
    if (nzchar(configured)) return(configured)
    b <- basename(first_scalar_string(model_env$base_dir, default = ""))
    if (grepl("fixVB_M", b, fixed = TRUE)) {
      "fixVB_M"
    } else if (grepl("fixVB", b, fixed = TRUE)) {
      "fixVB"
    } else if (grepl("fixM", b, fixed = TRUE)) {
      "fixM"
    } else {
      "base"
    }
  }

  infer_recipe_base_input_dir_from_env <- function(model_env) {
    configured <- first_scalar_string(model_env$input_recipe_base_input_dir, default = "")
    if (nzchar(configured)) return(configured)
    configured <- first_scalar_string(model_env$input_recipe_base_source, default = "")
    if (nzchar(configured)) return(configured)
    base_dir <- first_scalar_string(model_env$base_dir, default = "")
    if (!nzchar(base_dir)) return("")
    strip_recipe_suffixes(
      base_dir,
      fixed_params = first_scalar_string(model_env$input_recipe_fixed_params, default = ""),
      movement_pairs = first_scalar_string(model_env$input_recipe_movement_pairs, default = ""),
      sel_nodes = first_scalar_string(model_env$input_recipe_sel_nodes, default = ""),
      index_cv_half = first_scalar_string(model_env$input_recipe_index_cv_half, default = "0")
    )
  }

  recipe_base_input_dir_for_choice <- function(model_env, recipe_base) {
    base_input_dir <- if (!is.null(model_env)) infer_recipe_base_input_dir_from_env(model_env) else ""
    if (is.null(model_env)) return(base_input_dir)
    if (!nzchar(base_input_dir)) base_input_dir <- first_scalar_string(model_env$base_dir, default = "mfcl/inputs/input")
    if (identical(recipe_base, "config") || identical(recipe_base, infer_recipe_base_from_env(model_env))) {
      return(base_input_dir)
    }
    if (identical(recipe_base, "base")) return(strip_recipe_suffixes(base_input_dir))
    file.path(dirname(base_input_dir), paste0(basename(strip_recipe_suffixes(base_input_dir)), "_", recipe_base))
  }

  recipe_base_tokens <- function(recipe_base) {
    switch(
      recipe_base,
      fixM = "fixM",
      fixVB = "fixVB",
      fixVB_M = "fixVB,fixM",
      ""
    )
  }

  input_token_set <- function(model_env) {
    if (is.null(model_env)) return(character(0))
    txt <- first_scalar_string(model_env$input_recipe_base_tokens, default = "")
    if (!nzchar(txt)) return(character(0))
    vals <- trimws(unlist(strsplit(txt, "[,;[:space:]]+")))
    unique(vals[nzchar(vals)])
  }

  sensitivity_launch_sep <- "___sens___"
  sensitivity_combo_sep <- "___and___"

  ordered_sensitivity_ids <- function(ids) {
    ids <- unique(as.character(ids))
    if (length(ids) == 0) return(character(0))
    catalog <- input_sensitivity_catalog()
    catalog$id[catalog$id %in% ids]
  }

  make_sensitivity_launch_id <- function(input_id, sensitivity_ids) {
    ids <- ordered_sensitivity_ids(sensitivity_ids)
    paste0(input_id, sensitivity_launch_sep, paste(ids, collapse = sensitivity_combo_sep))
  }

  parse_sensitivity_launch_id <- function(model_name) {
    txt <- as.character(model_name[[1]])
    if (!grepl(sensitivity_launch_sep, txt, fixed = TRUE)) {
      return(list(is_sensitivity = FALSE, input_id = txt, sensitivity_ids = character(0), sensitivity_id = ""))
    }
    pieces <- strsplit(txt, sensitivity_launch_sep, fixed = TRUE)[[1]]
    ids <- if (length(pieces) >= 2L && nzchar(pieces[[2]])) {
      strsplit(pieces[[2]], sensitivity_combo_sep, fixed = TRUE)[[1]]
    } else {
      character(0)
    }
    list(
      is_sensitivity = length(pieces) == 2L && length(ids) > 0,
      input_id = pieces[[1]],
      sensitivity_ids = ordered_sensitivity_ids(ids),
      sensitivity_id = if (length(ids) > 0) ids[[1]] else ""
    )
  }

  selected_sensitivity_ids <- function() {
    sens <- input$input_recipe_sensitivities
    if (is.null(sens)) return(character(0))
    unique(as.character(sens))
  }

  selected_sensitivity_expansion <- function() {
    mode <- first_scalar_string(input$input_recipe_expansion, default = "oneoff")
    if (mode %in% c("oneoff", "factorial")) mode else "oneoff"
  }

  include_base_launch_units <- function() {
    isTRUE(input$input_recipe_include_base)
  }

  existing_sensitivity_rows <- function(model_env) {
    existing_tokens <- input_token_set(model_env)
    if (length(existing_tokens) == 0) return(input_sensitivity_catalog()[0, , drop = FALSE])
    rows <- input_sensitivity_catalog()
    rows[rows$token %in% existing_tokens, , drop = FALSE]
  }

  sensitivity_applicability <- function(model_env, sensitivity_ids) {
    sensitivity_ids <- unique(as.character(sensitivity_ids))
    rows <- input_sensitivity_by_id(sensitivity_ids)
    if (nrow(rows) == 0) {
      return(data.frame(
        id = character(0),
        token = character(0),
        label = character(0),
        factor = character(0),
        applicable = logical(0),
        reason = character(0),
        stringsAsFactors = FALSE
      ))
    }

    existing_tokens <- input_token_set(model_env)
    rows$applicable <- TRUE
    rows$reason <- ""

    for (idx in seq_len(nrow(rows))) {
      if (rows$token[[idx]] %in% existing_tokens) {
        rows$applicable[[idx]] <- FALSE
        rows$reason[[idx]] <- paste0("already present: ", rows$token[[idx]])
        next
      }
    }

    rows
  }

  sensitivity_applicable_ids <- function(model_env, sensitivity_ids) {
    rows <- sensitivity_applicability(model_env, sensitivity_ids)
    rows$id[rows$applicable]
  }

  sensitivity_skip_summary <- function(input_ids, sensitivity_ids) {
    pieces <- lapply(input_ids, function(input_id) {
      env <- model_env_for_input(input_id)
      rows <- sensitivity_applicability(env, sensitivity_ids)
      skipped <- rows[!rows$applicable, , drop = FALSE]
      if (nrow(skipped) == 0) return(NULL)
      base_label <- launch_unit_label(input_id)
      data.frame(
        base = base_label,
        sensitivity = skipped$label,
        token = skipped$token,
        reason = skipped$reason,
        stringsAsFactors = FALSE
      )
    })
    pieces <- Filter(Negate(is.null), pieces)
    if (length(pieces) == 0) {
      return(data.frame(base = character(0), sensitivity = character(0), token = character(0), reason = character(0)))
    }
    do.call(rbind, pieces)
  }

  sensitivity_replacement_rows <- function(model_env, sensitivity_ids) {
    sens_rows <- input_sensitivity_by_id(sensitivity_ids)
    if (nrow(sens_rows) == 0) return(sens_rows[0, , drop = FALSE])
    existing_rows <- existing_sensitivity_rows(model_env)
    if (nrow(existing_rows) == 0) return(existing_rows)
    selected_factors <- unique(sens_rows$factor)
    existing_rows[
      existing_rows$factor %in% selected_factors &
        !existing_rows$token %in% sens_rows$token,
      ,
      drop = FALSE
    ]
  }

  sensitivity_replacement_summary <- function(input_ids, sensitivity_ids) {
    pieces <- lapply(input_ids, function(input_id) {
      env <- model_env_for_input(input_id)
      selected_rows <- input_sensitivity_by_id(sensitivity_ids)
      replaced <- sensitivity_replacement_rows(env, sensitivity_ids)
      if (nrow(replaced) == 0) return(NULL)
      base_label <- launch_unit_label(input_id)
      replacement_text <- vapply(unique(replaced$factor), function(fac) {
        old_tokens <- replaced$token[replaced$factor == fac]
        new_tokens <- selected_rows$token[selected_rows$factor == fac]
        paste0(fac, ": ", paste(old_tokens, collapse = ", "), " -> ", paste(new_tokens, collapse = ", "))
      }, character(1))
      data.frame(base = base_label, replacement = replacement_text, stringsAsFactors = FALSE)
    })
    pieces <- Filter(Negate(is.null), pieces)
    if (length(pieces) == 0) {
      return(data.frame(base = character(0), replacement = character(0), stringsAsFactors = FALSE))
    }
    do.call(rbind, pieces)
  }

  strip_sensitivity_suffixes_from_path <- function(path, sensitivity_rows) {
    if (!is.character(path) || length(path) == 0 || !nzchar(path[[1]]) || nrow(sensitivity_rows) == 0) {
      return(path)
    }
    parent <- dirname(path)
    stem <- basename(path)
    suffixes <- unique(c(sensitivity_rows$input_suffix, sensitivity_rows$suffix))
    suffixes <- suffixes[nzchar(suffixes)]
    suffixes <- suffixes[order(nchar(suffixes), decreasing = TRUE)]
    for (suffix in suffixes) {
      stem <- gsub(suffix, "", stem, fixed = TRUE)
    }
    stem <- gsub("__+", "_", stem)
    stem <- gsub("^_+|_+$", "", stem)
    file.path(parent, stem)
  }

  sensitivity_factor_choices <- function(sensitivity_ids) {
    rows <- input_sensitivity_by_id(sensitivity_ids)
    if (nrow(rows) == 0) return(list())
    factors <- split(rows, rows$factor)
    lapply(factors, function(group_rows) {
      if (nrow(group_rows) == 0) return(list(character(0)))
      is_nested_group <- any(group_rows$nested)
      if (is_nested_group) {
        c(list(character(0)), lapply(group_rows$id, function(id) id))
      } else {
        ids <- group_rows$id
        combos <- unlist(lapply(seq_along(ids), function(k) {
          combn(ids, k, simplify = FALSE)
        }), recursive = FALSE)
        c(list(character(0)), combos)
      }
    })
  }

  expand_factorial_sensitivities <- function(sensitivity_ids) {
    sensitivity_ids <- ordered_sensitivity_ids(sensitivity_ids)
    if (length(sensitivity_ids) == 0) return(list())

    factor_choices <- sensitivity_factor_choices(sensitivity_ids)
    if (length(factor_choices) == 0) return(list())

    expand_one_factor <- function(options) {
      lapply(options, function(x) {
        x <- as.character(x)
        x[nzchar(x)]
      })
    }
    factor_choices <- lapply(factor_choices, expand_one_factor)

    combos <- list(character(0))
    for (choices in factor_choices) {
      combos <- unlist(lapply(combos, function(prefix) {
        lapply(choices, function(choice) ordered_sensitivity_ids(c(prefix, choice)))
      }), recursive = FALSE)
    }

    combos <- Filter(function(x) length(x) > 0, combos)
    keys <- vapply(combos, paste, character(1), collapse = "|")
    combos[!duplicated(keys)]
  }

  sensitivity_launch_combinations <- function(model_env, sensitivity_ids) {
    applicable <- sensitivity_applicable_ids(model_env, sensitivity_ids)
    if (length(applicable) == 0) return(list())

    if (identical(selected_sensitivity_expansion(), "factorial")) {
      expand_factorial_sensitivities(applicable)
    } else {
      lapply(applicable, function(id) id)
    }
  }

  selected_input_ids_from_checkboxes <- function() {
    if (length(rv$models) == 0) return(character(0))
    rows <- scan_launch_input_dirs()
    if (nrow(rows) == 0) return(character(0))
    if (!isTRUE(sensitivity_mode_enabled())) {
      if (exists("selected_existing_input_ids", mode = "function")) {
        return(selected_existing_input_ids())
      }
      selected <- input$existing_input_choices
      if (is.null(selected)) return(rows$id[[1]])
      if (length(selected) == 0) return(character(0))
      selected <- as.character(selected)
      selected <- selected[selected %in% rows$id]
      if (length(selected) > 0) return(unique(selected))
      return(character(0))
    }
    selected_base <- input$input_recipe_base_input_choice
    if (is.null(selected_base)) return(rows$id[[1]])
    if (length(selected_base) == 0) return(character(0))
    selected_base <- as.character(selected_base)
    selected_base <- selected_base[selected_base %in% rows$id]
    if (length(selected_base) > 0) return(unique(selected_base))
    character(0)
  }

  selected_launch_units_raw <- function(input_ids = selected_input_ids_from_checkboxes()) {
    if (!isTRUE(sensitivity_mode_enabled())) return(input_ids)
    sens <- selected_sensitivity_ids()
    base_units <- if (isTRUE(include_base_launch_units())) input_ids else character(0)
    if (length(sens) == 0) return(unique(base_units))

    units <- unlist(lapply(input_ids, function(input_id) {
      env <- model_env_for_input(input_id)
      combos <- sensitivity_launch_combinations(env, sens)
      if (length(combos) == 0) return(character(0))
      vapply(combos, function(sids) make_sensitivity_launch_id(input_id, sids), character(1), USE.NAMES = FALSE)
    }), use.names = FALSE)
    unique(c(base_units, units))
  }

  selected_launch_units <- function(input_ids = selected_input_ids_from_checkboxes()) {
    if (!isTRUE(sensitivity_mode_enabled())) return(input_ids)
    dedupe_launch_units(selected_launch_units_raw(input_ids))
  }

  launch_unit_label <- function(model_name) {
    parsed <- parse_sensitivity_launch_id(model_name)
    if (!isTRUE(parsed$is_sensitivity)) {
      row <- launch_input_row(parsed$input_id)
      return(if (!is.null(row)) row$display_name else parsed$input_id)
    }
    row <- launch_input_row(parsed$input_id)
    sens <- input_sensitivity_by_id(parsed$sensitivity_ids)
    base_label <- if (!is.null(row)) row$display_name else parsed$input_id
    sens_token <- if (nrow(sens) > 0) paste(sens$token, collapse = " + ") else paste(parsed$sensitivity_ids, collapse = " + ")
    paste0(base_label, " + ", sens_token)
  }

  launch_unit_labels <- function(model_names) {
    vapply(model_names, launch_unit_label, character(1), USE.NAMES = FALSE)
  }

  launch_unit_display_env <- function(model_name) {
    parsed <- parse_sensitivity_launch_id(model_name)
    if (isTRUE(parsed$is_sensitivity)) {
      base_env <- model_env_for_input(parsed$input_id)
      return(apply_input_recipe_override(parsed$input_id, base_env, sensitivity_id = parsed$sensitivity_ids, base_input_id = parsed$input_id))
    }
    model_env_for_input(model_name)
  }

  launch_unit_preview_line <- function(model_name) {
    env <- launch_unit_display_env(model_name)
    if (is.null(env)) return(as.character(model_name[[1]]))
    display_dir <- first_scalar_string(env$display_base_dir, default = first_scalar_string(env$base_dir, default = ""))
    paste0(launch_model_name(model_name, env), " -> ", display_dir)
  }

  canonical_launch_basename <- function(name) {
    x <- basename(as.character(name[[1]]))
    if (exists("compact_input_name", mode = "function")) {
      x <- compact_input_name(x)
    }
    catalog <- input_sensitivity_catalog()
    known_tokens <- as.character(catalog$token)
    parts <- strsplit(x, "_", fixed = TRUE)[[1]]
    present <- unique(parts[parts %in% known_tokens])
    root_parts <- parts[!parts %in% known_tokens]
    ordered_tokens <- known_tokens[known_tokens %in% present]
    paste(c(root_parts, ordered_tokens), collapse = "_")
  }

  launch_unit_output_key <- function(model_name) {
    env <- tryCatch(launch_unit_display_env(model_name), error = function(e) NULL)
    if (is.null(env)) return(as.character(model_name[[1]]))
    display_dir <- first_scalar_string(env$display_base_dir, default = first_scalar_string(env$base_dir, default = ""))
    display_name <- if (nzchar(display_dir)) basename(display_dir) else launch_model_name(model_name, env)
    display_name <- canonical_launch_basename(display_name)
    paste(dirname(display_dir), display_name, sep = "/")
  }

  dedupe_launch_units <- function(units) {
    units <- unique(as.character(units))
    if (length(units) < 2) return(units)
    keys <- vapply(units, launch_unit_output_key, character(1), USE.NAMES = FALSE)
    units[!duplicated(keys)]
  }

  duplicate_launch_unit_summary <- function(input_ids = selected_input_ids_from_checkboxes()) {
    raw_units <- selected_launch_units_raw(input_ids)
    if (length(raw_units) < 2) {
      return(data.frame(dropped = character(0), kept = character(0), output = character(0)))
    }
    keys <- vapply(raw_units, launch_unit_output_key, character(1), USE.NAMES = FALSE)
    duplicated_rows <- duplicated(keys)
    if (!any(duplicated_rows)) {
      return(data.frame(dropped = character(0), kept = character(0), output = character(0)))
    }
    kept_idx <- match(keys[duplicated_rows], keys)
    data.frame(
      dropped = launch_unit_labels(raw_units[duplicated_rows]),
      kept = launch_unit_labels(raw_units[kept_idx]),
      output = keys[duplicated_rows],
      stringsAsFactors = FALSE
    )
  }

  # Build the input-recipe plan for one selected base input. This is metadata
  # only; the actual input folder is created later by build_input_recipe.R if
  # the job lands on a machine where that folder is missing.
  input_recipe_plan <- function(model_env = NULL, sensitivity_ids = NULL, base_input_id = "") {
    override <- isTRUE(sensitivity_mode_enabled())
    if (!override) return(list(override = FALSE))

    sens <- if (is.null(sensitivity_ids)) selected_sensitivity_ids() else unique(as.character(sensitivity_ids))

    recipe_base <- "base"
    selected_base_input_id <- first_scalar_string(base_input_id, default = "")
    if (!nzchar(selected_base_input_id)) {
      selected_base_input_id <- first_scalar_string(input$input_recipe_base_input_choice, default = "")
    }
    selected_base_row <- if (nzchar(selected_base_input_id)) launch_input_row(selected_base_input_id) else NULL
    if (!is.null(selected_base_row)) {
      base_input_dir <- selected_base_row$base_dir
      model_env <- model_env_for_input(selected_base_input_id)
    } else {
      base_input_dir <- recipe_base_input_dir_for_choice(model_env, recipe_base)
    }

    sens_rows <- input_sensitivity_by_id(sensitivity_applicable_ids(model_env, sens))
    existing_tokens <- input_token_set(model_env)
    replaced_existing_rows <- sensitivity_replacement_rows(model_env, sens_rows$id)
    if (nrow(replaced_existing_rows) > 0) {
      base_input_dir <- strip_sensitivity_suffixes_from_path(base_input_dir, replaced_existing_rows)
      existing_tokens <- setdiff(existing_tokens, replaced_existing_rows$token)
    }

    recipe_options <- input_sensitivity_recipe_options(sens_rows$id)
    fixed_params <- recipe_options$fixed_params
    movement_pairs <- recipe_options$movement_pairs
    sel_nodes <- recipe_options$sel_nodes
    index_cv_half <- if (isTRUE(recipe_options$index_cv_half)) "1" else "0"
    suffix_parts <- recipe_options$suffix_parts
    output_base_name <- if (exists("compact_input_name", mode = "function")) {
      compact_input_name(basename(base_input_dir))
    } else {
      basename(base_input_dir)
    }
    output_name <- canonical_launch_basename(paste0(output_base_name, paste0(suffix_parts, collapse = "")))
    output_dir <- file.path(dirname(base_input_dir), output_name)

    tokens <- recipe_options$tokens
    key <- if (length(tokens) > 0) paste(tokens, collapse = "_") else "base"
    label <- if (length(tokens) > 0) paste(tokens, collapse = " + ") else "base"
    description <- if (length(recipe_options$labels) > 0) {
      paste(recipe_options$labels, collapse = "; ")
    } else {
      label
    }

    list(
      override = TRUE,
      base = recipe_base,
      base_input_dir = base_input_dir,
      base_tokens = paste(existing_tokens, collapse = ","),
      output_dir = output_dir,
      fixed_params = fixed_params,
      movement_pairs = movement_pairs,
      sel_nodes = sel_nodes,
      index_cv_half = index_cv_half,
      tokens = tokens,
      key = key,
      label = label,
      description = description
    )
  }

  drop_profile_sets_for_fixed_tokens <- function(profile_sets, tokens) {
    if (is.null(profile_sets) || length(profile_sets) == 0) return(profile_sets)
    tokens <- unique(as.character(tokens))
    drop <- character(0)
    if ("fixM" %in% tokens) drop <- c(drop, "LorenM")
    if ("fixVB" %in% tokens) drop <- c(drop, "L1", "L2", "kappa")
    drop <- unique(drop)
    if (length(drop) == 0) return(profile_sets)
    if (is.null(names(profile_sets))) return(profile_sets)
    profile_sets[setdiff(names(profile_sets), drop)]
  }

  launch_model_name <- function(model_name, model_env = NULL) {
    candidate <- if (!is.null(model_env)) first_scalar_string(model_env$launcher_model_name, default = "") else ""
    if (nzchar(candidate)) candidate else model_name
  }

  launch_input_row <- function(input_id) {
    rows <- scan_launch_input_dirs()
    hit <- rows[rows$id == input_id, , drop = FALSE]
    if (nrow(hit) == 0) return(NULL)
    hit[1, , drop = FALSE]
  }

  model_env_for_input <- function(input_id) {
    row <- launch_input_row(input_id)
    defaults <- launch_defaults_env()
    if (is.null(row) || is.null(defaults)) return(NULL)

    env <- as.list(defaults, all.names = TRUE)
    env$base_dir <- row$base_dir
    env$display_base_dir <- row$display_base_dir
    env$model_dir <- file.path("model", row$display_name)
    env$launcher_model_name <- row$display_name
    env$launcher_input_label <- row$label
    env$input_recipe_enabled <- "0"
    env$input_recipe_base <- "base"
    env$input_recipe_base_input_dir <- row$base_dir
    env$input_recipe_base_source <- row$base_dir
    env$input_recipe_base_tokens <- row$tokens
    env$input_recipe_output_dir <- row$base_dir
    env$input_recipe_fixed_params <- ""
    if (!is.null(row$description) && nzchar(as.character(row$description[[1]]))) {
      env$description <- row$description[[1]]
    } else if (is.null(env$description) || !nzchar(as.character(env$description[[1]]))) {
      env$description <- paste("Input", row$name)
    }
    env
  }

  # Convert a selected input folder into the runner environment used by all job
  # types. Sensitivity selections rewrite base_dir/model_dir and mark the input
  # recipe to be built on demand.
  apply_input_recipe_override <- function(model_name, model_env, sensitivity_id = NULL, base_input_id = "") {
    if (is.null(model_env)) return(model_env)
    env <- as.list(model_env, all.names = TRUE)
    plan <- input_recipe_plan(env, sensitivity_ids = sensitivity_id, base_input_id = base_input_id)
    if (!isTRUE(plan$override) || identical(plan$key, "base")) return(env)

    base_launch_name <- launch_model_name(model_name, env)
    launch_name <- if (identical(plan$key, "base")) base_launch_name else paste0(base_launch_name, "_", plan$key)
    launch_name <- canonical_launch_basename(launch_name)
    env$launcher_model_name <- launch_name
    env$base_dir <- plan$output_dir
    env$display_base_dir <- plan$output_dir
    env$model_dir <- file.path("model", launch_name)
    env$build_inputs_on_missing <- "1"
    env$input_recipe_enabled <- "1"
    env$input_recipe_builder <- first_scalar_string(env$input_recipe_builder, default = "tools/input_sensitivities/build_input_recipe.R")
    env$input_recipe_base <- plan$base
    env$input_recipe_base_input_dir <- plan$base_input_dir
    env$input_recipe_base_source <- plan$base_input_dir
    env$input_recipe_base_tokens <- plan$base_tokens
    env$input_recipe_output_dir <- plan$output_dir
    env$input_recipe_fixed_params <- plan$fixed_params
    env$input_recipe_movement_pairs <- plan$movement_pairs
    env$input_recipe_sel_nodes <- plan$sel_nodes
    env$input_recipe_index_cv_half <- plan$index_cv_half
    env$launcher_input_recipe_label <- plan$label
    env$launcher_input_recipe_description <- plan$description
    env$description <- plan$description
    env$profile_sets <- drop_profile_sets_for_fixed_tokens(env$profile_sets, plan$tokens)

    summary_txt <- first_scalar_string(env$config_summary, default = "")
    recipe_txt <- paste0(
      "On-demand sensitivity input: ", plan$description,
      "; base input=", plan$base_input_dir,
      "; output input=", plan$output_dir
    )
    env$config_summary <- if (nzchar(summary_txt)) paste(summary_txt, recipe_txt, sep = " | ") else recipe_txt
    env
  }

  # ----- Fitted-output source matching -----
  # Dependent jobs such as jitter/profile can either reuse an existing fitted
  # model output or first run a prerequisite model and then use that output.

  scan_launcher_fitted_sources <- function() {
    if (!exists("scan_fitted_model_dirs", mode = "function")) {
      return(data.frame(
        id = character(0),
        name = character(0),
        source_dir = character(0),
        label = character(0),
        par_file = character(0),
        has_indepvar = logical(0),
        has_quantity = logical(0),
        base_dir = character(0),
        tokens = character(0),
        stringsAsFactors = FALSE
      ))
    }
    scan_fitted_model_dirs(model_root = "model", repo_root = resolve_repo_path("."))
  }

  fitted_source_choice <- function() {
    choice <- first_scalar_string(input$fitted_model_source_choice, default = "__auto__")
    if (nzchar(choice)) choice else "__auto__"
  }

  matched_fitted_source_row <- function(model_name, model_env, choice = fitted_source_choice()) {
    rows <- scan_launcher_fitted_sources()
    if (!is.data.frame(rows) || nrow(rows) == 0) return(NULL)

    if (!identical(choice, "__auto__")) {
      hit <- rows[rows$id == choice, , drop = FALSE]
      return(if (nrow(hit) > 0) hit[1, , drop = FALSE] else NULL)
    }

    launch_name <- launch_model_name(model_name, model_env)
    candidates <- unique(c(
      first_scalar_string(model_env$model_dir, default = ""),
      file.path("model", launch_name),
      launch_name,
      model_name,
      basename(first_scalar_string(model_env$base_dir, default = ""))
    ))
    candidates <- candidates[nzchar(candidates)]
    candidate_basenames <- basename(candidates)
    candidate_compact <- if (exists("compact_input_name", mode = "function")) {
      vapply(candidate_basenames, compact_input_name, character(1))
    } else {
      candidate_basenames
    }
    candidate_ids <- vapply(candidates, safe_launch_id, character(1))

    row_display <- if (exists("compact_input_name", mode = "function")) {
      vapply(rows$name, compact_input_name, character(1))
    } else {
      rows$name
    }
    row_base_display <- if (exists("compact_input_name", mode = "function")) {
      vapply(basename(rows$base_dir), compact_input_name, character(1))
    } else {
      basename(rows$base_dir)
    }
    hit <- rows[
      rows$source_dir %in% candidates |
        rows$base_dir %in% candidates |
        basename(rows$base_dir) %in% candidate_basenames |
        row_base_display %in% candidate_compact |
        rows$name %in% candidate_basenames |
        row_display %in% candidate_compact |
        rows$id %in% candidate_ids,
      ,
      drop = FALSE
    ]
    if (nrow(hit) > 0) hit[1, , drop = FALSE] else NULL
  }

  apply_fitted_model_source_selection <- function(model_name, model_env) {
    if (is.null(model_env)) return(model_env)
    env <- as.list(model_env, all.names = TRUE)
    if (!isTRUE(input$fitted_model_source_enabled)) {
      env$fitted_model_source_enabled <- "0"
      env$fitted_model_source_dir <- ""
      env$fitted_model_bundle <- ""
      env$fitted_model_source_id <- ""
      env$fitted_model_source_label <- "run prerequisite model first"
      env$auto_run_model_before_dependency <- "1"
      env$auto_fitted_model_dir <- first_scalar_string(env$model_dir, default = file.path("model", model_name))
      return(env)
    }

    choice <- fitted_source_choice()
    row <- matched_fitted_source_row(model_name, env, choice = choice)
    env$fitted_model_source_enabled <- "1"
    env$auto_run_model_before_dependency <- "0"
    env$auto_fitted_model_dir <- ""
    env$fitted_model_source_choice <- choice
    env$fitted_model_source_dir <- ""
    env$fitted_model_source_id <- ""
    env$fitted_model_source_label <- if (identical(choice, "__auto__")) "auto-match" else choice

    if (!is.null(row) && nrow(row) > 0) {
      env$fitted_model_source_dir <- row$source_dir[[1]]
      env$fitted_model_source_id <- row$id[[1]]
      env$fitted_model_source_label <- row$label[[1]]
      summary_txt <- first_scalar_string(env$config_summary, default = "")
      fitted_txt <- paste0(
        "Fitted source: ", row$source_dir[[1]],
        "; par=", row$par_file[[1]]
      )
      env$config_summary <- if (nzchar(summary_txt)) paste(summary_txt, fitted_txt, sep = " | ") else fitted_txt
    } else {
      env$fitted_model_source_dir <- ""
      env$fitted_model_bundle <- ""
      env$fitted_model_source_id <- ""
      env$fitted_model_source_label <- if (identical(choice, "__auto__")) {
        "no auto-matched fitted output; run prerequisite model first"
      } else {
        "selected fitted output not found; run prerequisite model first"
      }
      env$auto_run_model_before_dependency <- "1"
      env$auto_fitted_model_dir <- first_scalar_string(env$model_dir, default = file.path("model", model_name))
    }
    env
  }

  active_model_env <- function(model_name) {
    apply_selftest_option_overrides(
      model_name,
      apply_launch_job_option_overrides(
        apply_fitted_model_source_selection(model_name, launch_unit_display_env(model_name))
      )
    )
  }

  observeEvent({
    list(repo_root_val(), rv$config_loaded, input$input_launch_mode)
  }, {
    rows <- scan_launch_input_dirs()
    choices <- if (nrow(rows) > 0) stats::setNames(rows$id, rows$label) else character(0)
    current_base <- isolate(input$input_recipe_base_input_choice)
    current_base <- if (is.null(current_base) || length(current_base) == 0) character(0) else as.character(current_base)
    selected_inputs <- isolate(rv$selected_models)
    selected_base <- current_base[current_base %in% rows$id]
    if (length(selected_base) > 0) {
      selected_base <- unique(selected_base)
    } else if (length(selected_inputs) > 0 && selected_inputs[[1]] %in% rows$id) {
      selected_base <- as.character(selected_inputs[selected_inputs %in% rows$id])
    } else if (length(choices) > 0) {
      selected_base <- unname(choices[[1]])
    } else {
      selected_base <- character(0)
    }
    updateSelectizeInput(
      session,
      "input_recipe_base_input_choice",
      choices = choices,
      selected = selected_base,
      server = TRUE
    )

    current_existing <- isolate(input$existing_input_choices)
    current_existing <- if (is.null(current_existing) || length(current_existing) == 0) character(0) else as.character(current_existing)
    selected_existing <- current_existing[current_existing %in% rows$id]
    if (length(selected_existing) == 0 && length(selected_inputs) > 0) {
      selected_existing <- as.character(selected_inputs[selected_inputs %in% rows$id])
    }
    if (length(selected_existing) == 0 && length(choices) > 0) {
      selected_existing <- unname(choices[[1]])
    }
    updateSelectizeInput(
      session,
      "existing_input_choices",
      choices = choices,
      selected = selected_existing,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  observeEvent({
    list(repo_root_val(), input$fitted_model_source_enabled, rv$launcher_job_log_trigger)
  }, {
    rows <- scan_launcher_fitted_sources()
    choices <- c("Auto-match by launch model name" = "__auto__")
    if (is.data.frame(rows) && nrow(rows) > 0) {
      choices <- c(choices, stats::setNames(rows$id, rows$label))
    }
    current <- isolate(first_scalar_string(input$fitted_model_source_choice, default = "__auto__"))
    selected <- if (nzchar(current) && current %in% unname(choices)) current else "__auto__"
    updateSelectizeInput(
      session,
      "fitted_model_source_choice",
      choices = choices,
      selected = selected,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  output$input_recipe_preview_ui <- renderUI({
    if (!isTRUE(sensitivity_mode_enabled())) return(NULL)
    base_inputs <- selected_input_ids_from_checkboxes()
    sens <- selected_sensitivity_ids()
    include_base <- include_base_launch_units()
    if (length(base_inputs) == 0) {
      return(div(class = "input-recipe-preview", tags$div("Select a base input to preview the sensitivity output.")))
    }
    if (length(sens) == 0 && !isTRUE(include_base)) {
      return(div(class = "input-recipe-preview", tags$div("Select sensitivities, or include the selected base input(s), to create launch units.")))
    }
    units <- selected_launch_units(base_inputs)
    skipped <- sensitivity_skip_summary(base_inputs, sens)
    duplicates <- duplicate_launch_unit_summary(base_inputs)
    replacements <- sensitivity_replacement_summary(base_inputs, sens)
    generated_units <- units[vapply(units, function(unit) {
      isTRUE(parse_sensitivity_launch_id(unit)$is_sensitivity)
    }, logical(1))]
    exact_skipped_lines <- if (is.data.frame(skipped) && nrow(skipped) > 0) {
      apply(skipped, 1, function(row) {
        paste0(row[["base"]], ": ", row[["sensitivity"]], " skipped (", row[["reason"]], ")")
      })
    } else {
      character(0)
    }
    duplicate_skipped_lines <- if (is.data.frame(duplicates) && nrow(duplicates) > 0) {
      apply(duplicates, 1, function(row) {
        paste0(row[["dropped"]], " skipped (same final input as ", row[["kept"]], ")")
      })
    } else {
      character(0)
    }
    all_skipped_lines <- unique(c(exact_skipped_lines, duplicate_skipped_lines))
    skipped_lines <- head(all_skipped_lines, 8)
    skipped_total <- length(all_skipped_lines)
    replacement_lines <- if (is.data.frame(replacements) && nrow(replacements) > 0) {
      apply(head(replacements, 6), 1, function(row) {
        paste0(row[["base"]], ": ", row[["replacement"]])
      })
    } else {
      character(0)
    }
    if (length(units) == 0) {
      return(div(
        class = "input-recipe-preview",
        tags$div("No launch units: selected base inputs already contain the selected token(s), and base inputs are not included."),
        if (length(skipped_lines) > 0) tags$ul(lapply(skipped_lines, function(x) tags$li(x)))
      ))
    }
    preview_units <- head(units, 6)
    preview_lines <- vapply(preview_units, function(unit) {
      launch_unit_preview_line(unit)
    }, character(1))
    div(
      class = "input-recipe-preview",
      tags$div(strong("Launch units:"), " ", length(units)),
      tags$div(strong("Base inputs included:"), " ", if (isTRUE(include_base)) "yes" else "no"),
      if (length(sens) > 0) tags$div(strong("Sensitivity variants:"), " ", length(generated_units)),
      if (length(sens) > 0) tags$div(
        strong("Expansion:"), " ",
        if (identical(selected_sensitivity_expansion(), "factorial")) "factorial combinations" else "one-off",
        if (input_sensitivity_has_nested_levels(sens)) " (nested levels enforced)" else ""
      ),
      if (length(generated_units) > 0) tags$div(strong("Generated if missing:"), " yes"),
      if (length(replacement_lines) > 0) tags$div(strong("Replacing existing factor levels:")),
      if (length(replacement_lines) > 0) tags$ul(lapply(replacement_lines, function(x) tags$li(x))),
      if (is.data.frame(replacements) && nrow(replacements) > length(replacement_lines)) {
        tags$div("... ", nrow(replacements) - length(replacement_lines), " more replacements")
      },
      tags$ul(lapply(preview_lines, function(x) tags$li(tags$code(x)))),
      if (length(units) > length(preview_units)) tags$div("... ", length(units) - length(preview_units), " more"),
      if (length(skipped_lines) > 0) tags$div(strong("Skipped overlaps:")),
      if (length(skipped_lines) > 0) tags$ul(lapply(skipped_lines, function(x) tags$li(x))),
      if (skipped_total > length(skipped_lines)) {
        tags$div("... ", skipped_total - length(skipped_lines), " more skipped")
      }
    )
  })

  output$fitted_model_source_preview_ui <- renderUI({
    if (!isTRUE(input$fitted_model_source_enabled)) return(NULL)
    rows <- scan_launcher_fitted_sources()
    if (!is.data.frame(rows) || nrow(rows) == 0) {
      return(div(class = "input-recipe-preview", "No fitted model outputs found under ", tags$code("model/"), "."))
    }
    choice <- fitted_source_choice()
    selected_units <- selected_models_from_checkboxes()
    if (length(selected_units) == 0) {
      return(div(class = "input-recipe-preview", "Select input folders to preview fitted-source matching."))
    }
    if (!identical(choice, "__auto__")) {
      row <- matched_fitted_source_row(selected_units[[1]], active_model_env(selected_units[[1]]), choice = choice)
      if (is.null(row)) {
        return(div(class = "input-recipe-preview", "Selected fitted source was not found."))
      }
      return(div(
        class = "input-recipe-preview",
        tags$div(strong("Fitted source:"), " ", tags$code(row$source_dir[[1]])),
        tags$div(strong("Par:"), " ", row$par_file[[1]]),
        tags$div("This fitted output will be overlaid on each selected input for dependent jobs.")
      ))
    }

    preview_units <- head(selected_units, 6)
    preview_lines <- vapply(preview_units, function(unit) {
      env <- active_model_env(unit)
      row <- matched_fitted_source_row(unit, env, choice = "__auto__")
      paste0(
        launch_unit_label(unit),
        " -> ",
        if (!is.null(row)) row$source_dir[[1]] else "no match; will run prerequisite model first"
      )
    }, character(1), USE.NAMES = FALSE)
    div(
      class = "input-recipe-preview",
      tags$div(strong("Auto-match:"), " by launch model name / model_dir"),
      tags$ul(lapply(preview_lines, function(x) tags$li(tags$code(x)))),
      if (length(selected_units) > length(preview_units)) tags$div("... ", length(selected_units) - length(preview_units), " more")
    )
  })

  sanitize_profile_job_tag <- function(x) {
    txt <- trimws(paste(as.character(x), collapse = "_"))
    if (!nzchar(txt)) return("")
    txt <- gsub("[^A-Za-z0-9]+", "_", txt)
    txt <- gsub("^_+|_+$", "", txt)
    txt
  }

  resolve_profile_job_envs <- function(model_env) {
    base_env <- as.list(model_env, all.names = TRUE)
    base_env$profile_sets <- NULL

    raw_sets <- model_env$profile_sets
    if (is.null(raw_sets) || length(raw_sets) == 0) {
      return(list(base_env))
    }

    set_names <- names(raw_sets)
    if (is.null(set_names)) set_names <- rep("", length(raw_sets))

    out <- list()
    for (i in seq_along(raw_sets)) {
      spec <- raw_sets[[i]]
      if (!is.list(spec)) next

      enabled_raw <- spec$enabled
      enabled <- TRUE
      if (!is.null(enabled_raw) && length(enabled_raw) > 0) {
        enabled_chr <- tolower(trimws(as.character(enabled_raw[[1]])))
        enabled <- !(identical(enabled_raw[[1]], FALSE) || enabled_chr %in% c("0", "false", "no", "off"))
      }
      if (!enabled) next

      spec_name <- trimws(as.character(set_names[[i]]))
      if (!nzchar(spec_name) && !is.null(spec$name) && length(spec$name) > 0) {
        spec_name <- trimws(as.character(spec$name[[1]]))
      }

      env <- modifyList(base_env, spec)
      env$profile_sets <- NULL
      env$enabled <- NULL
      env$name <- NULL
      env$label <- NULL
      env$key <- NULL

      label <- if (!is.null(spec$label) && length(spec$label) > 0 && nzchar(trimws(as.character(spec$label[[1]])))) {
        trimws(as.character(spec$label[[1]]))
      } else if (nzchar(spec_name)) {
        spec_name
      } else if (!is.null(env$prof_fix_indepvar) && length(env$prof_fix_indepvar) > 0 && nzchar(trimws(as.character(env$prof_fix_indepvar[[1]])))) {
        trimws(as.character(env$prof_fix_indepvar[[1]]))
      } else {
        "standard"
      }

      key_txt <- if (!is.null(spec$key) && length(spec$key) > 0 && nzchar(trimws(as.character(spec$key[[1]])))) {
        trimws(as.character(spec$key[[1]]))
      } else {
        label
      }

      env$profile_set_name <- label
      env$profile_set_label <- label
      env$profile_set_tag <- sanitize_profile_job_tag(key_txt)
      out[[length(out) + 1L]] <- env
    }

    if (length(out) == 0) list(base_env) else out
  }

  profile_launch_name <- function(model_name, profile_env = NULL) {
    model_name <- launch_model_name(model_name, profile_env)
    if (is.null(profile_env)) return(model_name)
    set_name <- first_scalar_string(profile_env$profile_set_name, default = "")
    if (!nzchar(set_name)) return(model_name)
    paste0(model_name, " [", set_name, "]")
  }

  has_prof_2d_spec <- function(profile_env) {
    if (is.null(profile_env) || !is.list(profile_env)) return(FALSE)
    indepvar_txt <- first_scalar_string(profile_env$prof_2d_indepvar, default = "")
    x_scalars_txt <- first_scalar_string(profile_env$prof_2d_scalars_x, default = "")
    y_scalars_txt <- first_scalar_string(profile_env$prof_2d_scalars_y, default = "")
    x_vals_txt <- first_scalar_string(profile_env$prof_2d_values_x, default = "")
    y_vals_txt <- first_scalar_string(profile_env$prof_2d_values_y, default = "")
    has_scalar_grid <- nzchar(x_scalars_txt) && nzchar(y_scalars_txt)
    has_value_grid <- nzchar(x_vals_txt) && nzchar(y_vals_txt)
    nzchar(indepvar_txt) && (has_scalar_grid || has_value_grid)
  }

  profile_env_components <- function(profile_env) {
    components <- character(0)
    has_2d <- isTRUE(has_prof_2d_spec(profile_env))
    if (has_2d) components <- c(components, "prof_2d")
    fix_txt <- first_scalar_string(profile_env$prof_fix_indepvar, default = "")
    one_d_text <- vapply(
      list(profile_env$scalars, profile_env$scalar, profile_env$prof_scalars, profile_env$profile_scalars),
      first_scalar_string,
      character(1),
      default = ""
    )
    has_1d_spec <- nzchar(fix_txt) || any(nzchar(one_d_text))
    if (!has_2d || has_1d_spec) {
      components <- c(components, if (nzchar(fix_txt)) "individual" else "standard")
    }
    unique(components)
  }

  selected_profile_components <- function() {
    comps <- tryCatch(input$profile_components, error = function(e) NULL)
    if (is.null(comps) || length(comps) == 0) return("standard")
    unique(as.character(comps))
  }

  effective_selected_job_types <- function(selected_job_types) {
    if (isTRUE(input$stage_check_only)) {
      return("stage_check")
    }
    selected_job_types <- unique(as.character(selected_job_types))
    if ("prof" %in% selected_job_types) {
      profile_components <- selected_profile_components()
      if (!any(profile_components %in% c("standard", "individual"))) {
        selected_job_types <- setdiff(selected_job_types, "prof")
      }
      if ("prof_2d" %in% profile_components) {
        selected_job_types <- unique(c(selected_job_types, "prof_2d"))
      }
    }
    selected_job_types
  }

  selected_profile_envs <- function(model_env, components = selected_profile_components()) {
    envs <- resolve_profile_job_envs(model_env)
    keep <- vapply(envs, function(env) any(profile_env_components(env) %in% components), logical(1))
    envs[keep]
  }

  selected_profile_1d_envs <- function(model_env) {
    selected_profile_envs(model_env, intersect(selected_profile_components(), c("standard", "individual")))
  }

  selected_profile_2d_envs <- function(model_env) {
    selected_profile_envs(model_env, "prof_2d")
  }

  resolve_prof_scalars <- function(model_env) {
    default_scalars <- parse_first_nonempty_numeric(
      model_env$scalars,
      model_env$scalar,
      model_env$prof_scalars,
      model_env$profile_scalars,
      model_env$Scalars
    )
    map_scalars <- unique(c(
      parse_prof_target_scalars(model_env$init_from_scalar_map),
      parse_prof_target_scalars(model_env$init_par_override_map)
    ))
    map_scalars <- sort(unique(map_scalars[is.finite(map_scalars)]))
    if (length(map_scalars) > 0) {
      return(map_scalars)
    }
    default_scalars
  }

  resolve_prof_scalars_for_mode <- function(model_env, prof_chain_mode = FALSE) {
    # In sequential-chain mode, always use explicit profile scalars.
    if (isTRUE(prof_chain_mode)) {
      return(parse_first_nonempty_numeric(
        model_env$scalars,
        model_env$scalar,
        model_env$prof_scalars,
        model_env$profile_scalars,
        model_env$Scalars
      ))
    }
    resolve_prof_scalars(model_env)
  }

  resolve_prof_anchor_and_chains <- function(scalars, anchor) {
    scalars <- sort(unique(scalars[is.finite(scalars)]))
    if (length(scalars) == 0) {
      return(list(anchor = NA_real_, lower = numeric(0), upper = numeric(0)))
    }
    anchor <- suppressWarnings(as.numeric(anchor))
    if (!is.finite(anchor)) anchor <- 100
    anchor_eff <- scalars[which.min(abs(scalars - anchor))]
    lower <- sort(scalars[scalars < anchor_eff], decreasing = TRUE)
    upper <- sort(scalars[scalars > anchor_eff], decreasing = FALSE)
    list(anchor = anchor_eff, lower = lower, upper = upper)
  }

  apply_prof_init_mapping <- function(job_env, scalar_value) {
    sc <- suppressWarnings(as.numeric(scalar_value))
    if (!is.finite(sc)) return(job_env)
    sc_key <- format(sc, scientific = FALSE, trim = TRUE)

    override_map <- parse_prof_target_map(job_env$init_par_override_map)
    donor_map <- parse_prof_target_map(job_env$init_from_scalar_map)

    ov <- override_map[[sc_key]]
    if (!is.null(ov) && nzchar(ov)) {
      job_env$init_par_override <- ov
      return(job_env)
    }

    dn <- donor_map[[sc_key]]
    if (!is.null(dn) && nzchar(dn)) {
      job_env$init_from_scalar <- dn
    }
    job_env
  }

  local_run_command <- function(spec, common_params, job_env, log_file) {
    if (isTRUE(common_params$local_use_docker)) {
      uid <- tryCatch(system2("id", "-u", stdout = TRUE), error = function(e) character(0))
      gid <- tryCatch(system2("id", "-g", stdout = TRUE), error = function(e) character(0))
      env_values <- vapply(job_env, function(x) {
        if (length(x) == 0 || is.null(x)) "" else paste(as.character(x), collapse = " ")
      }, character(1))
      env_args <- unlist(
        Map(function(k, v) c("-e", shQuote(paste0(k, "=", v))), names(env_values), env_values),
        use.names = FALSE
      )
      user_args <- if (length(uid) == 1 && nzchar(uid) && length(gid) == 1 && nzchar(gid)) {
        c("--user", shQuote(paste0(uid, ":", gid)))
      } else {
        character(0)
      }
      list(
        command = "docker",
        args = c(
          "run", "--rm",
          user_args,
          "-v", shQuote(paste0(common_params$repo_root, ":/workspace")),
          "-w", shQuote("/workspace"),
          env_args,
          shQuote(common_params$docker_image),
          "Rscript",
          shQuote(local_job_runner(spec$job_type))
        ),
        env = character(0),
        log_file = log_file
      )
    } else {
      list(
        command = "make",
        args = spec$job_type,
        env = local_env_strings(job_env),
        log_file = log_file
      )
    }
  }

  local_command_preview <- function(run_spec) {
    args <- if (length(run_spec$args) > 0) paste(shQuote(run_spec$args), collapse = " ") else ""
    paste(trimws(paste(run_spec$command, args)), collapse = " ")
  }

  default_condor_exclude_slots <- function() {
    c(
      "slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
      "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"
    )
  }

  parse_condor_exclude_patterns <- function(raw_pattern) {
    if (is.null(raw_pattern) || !nzchar(trimws(raw_pattern))) {
      return(character(0))
    }
    pats <- trimws(unlist(strsplit(raw_pattern, "[,\n;]+")))
    pats[nzchar(pats)]
  }

  fetch_condor_slot_table <- function(remote_user, remote_host) {
    if (is.null(remote_user) || !nzchar(trimws(remote_user)) || is.null(remote_host) || !nzchar(trimws(remote_host))) {
      return(data.frame(slot = character(0), machine = character(0), stringsAsFactors = FALSE))
    }

    ssh_target <- sprintf("%s@%s", trimws(remote_user), trimws(remote_host))
    slot_lines <- tryCatch(
      suppressWarnings(system2(
        "ssh",
        args = c("-o", "BatchMode=yes", "-o", "ConnectTimeout=8", ssh_target, "condor_status -af Name Machine"),
        stdout = TRUE,
        stderr = FALSE,
        timeout = 20
      )),
      error = function(e) character(0)
    )
    if (length(slot_lines) == 0) {
      return(data.frame(slot = character(0), machine = character(0), stringsAsFactors = FALSE))
    }

    slot_names <- trimws(sub("\\s+.*$", "", slot_lines))
    machine_names <- trimws(sub("^\\S+\\s*", "", slot_lines))
    keep <- nzchar(slot_names)
    data.frame(
      slot = slot_names[keep],
      machine = machine_names[keep],
      stringsAsFactors = FALSE
    )
  }

  match_condor_slots_by_patterns <- function(slot_table, patterns) {
    if (!is.data.frame(slot_table) || nrow(slot_table) == 0 || length(patterns) == 0) {
      return(character(0))
    }
    matched <- vapply(seq_len(nrow(slot_table)), function(i) {
      target <- paste(slot_table$slot[[i]], slot_table$machine[[i]], sep = " ")
      any(vapply(patterns, function(pat) grepl(pat, target, ignore.case = TRUE, perl = TRUE), logical(1)))
    }, logical(1))
    unique(slot_table$slot[matched])
  }

  condor_target_patterns <- function(run_target) {
    target <- if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
    switch(
      target,
      nouofp = c("nouofp", "nou"),
      suvofp = c("suvofp", "suv"),
      character(0)
    )
  }

  condor_target_label <- function(run_target) {
    target <- if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
    switch(
      target,
      all = "all (no extra filtering)",
      nouofp = "nouofp",
      suvofp = "suvofp",
      target
    )
  }

  format_selected_condor_nodes <- function(launch_mode, condor_target_info) {
    if (!identical(launch_mode, "condor")) {
      return("NA")
    }
    target_mode <- if (!is.null(condor_target_info$target_mode)) condor_target_info$target_mode else "all"
    label <- condor_target_label(target_mode)
    matched_n <- length(condor_target_info$matched_slots)
    exclude_n <- length(condor_target_info$exclude_slots)
    if (identical(target_mode, "all")) {
      return(label)
    }
    paste0(label, " (matched slots: ", matched_n, "; excluded slots: ", exclude_n, ")")
  }

  fitted_source_job_types <- function() {
    c("stage_check", "model", "jitter", "hessian", "prof", "prof_chain", "prof_2d", "selftest")
  }

  job_uses_fitted_source <- function(job_env, job_type) {
    truthy_scalar(job_env$fitted_model_source_enabled) && job_type %in% fitted_source_job_types()
  }

  ensure_fitted_source_functions_loaded <- function(repo_root) {
    if (!exists("fms_create_bundle", mode = "function")) {
      source(file.path(repo_root, "tools", "fitted_model_source.R"), local = parent.frame())
    }
    invisible(TRUE)
  }

  prepare_job_fitted_source <- function(job_env, job_type, work_dir, repo_root) {
    if (!job_uses_fitted_source(job_env, job_type)) {
      return(list(job_env = job_env, extra_transfer_files = character(0)))
    }

    ensure_fitted_source_functions_loaded(repo_root)
    source_dir <- first_scalar_string(job_env$fitted_model_source_dir, default = "")
    if (!nzchar(source_dir)) {
      job_env$fitted_model_source_enabled <- "0"
      job_env$fitted_model_bundle <- ""
      job_env$fitted_model_source_label <- "no matched fitted source; model will run first if this job needs .par/indepvar.rpt"
      return(list(job_env = job_env, extra_transfer_files = character(0)))
    }
    source_abs <- if (grepl("^/", source_dir)) source_dir else file.path(repo_root, source_dir)
    if (!dir.exists(source_abs)) {
      stop("Selected fitted model source does not exist: ", source_abs)
    }

    source_id <- first_scalar_string(job_env$fitted_model_source_id, default = fms_safe_id(source_dir))
    bundle <- fms_create_bundle(
      source_dir = source_abs,
      bundle_dir = work_dir,
      bundle_name = paste0("fitted_source_", fms_safe_id(source_id), ".tar.gz")
    )

    job_env$fitted_model_source_enabled <- "1"
    job_env$fitted_model_source_dir <- ""
    job_env$fitted_model_bundle <- file.path("..", basename(bundle))
    job_env$fitted_model_source_id <- source_id
    list(job_env = job_env, extra_transfer_files = bundle)
  }

  condorbox_with_extra_transfer <- function(remote_user, remote_host, remote_dir, github_pat, github_username,
                                            github_org, github_repo, stream_error = "FALSE", branch = "main",
                                            docker_image, target_folder = NULL, condor_cpus = NULL, condor_memory = NULL,
                                            condor_disk = NULL, make_options = "all", rmclone_script = "yes",
                                            ghcr_login = FALSE, remote_os = "linux", condor_environment = NULL,
                                            custom_batch_name = NULL, slot_requirements = NULL, exclude_machines = NULL,
                                            exclude_slots = NULL, extra_transfer_files = character(0)) {
    clone_script <- "clone_job.sh"
    run_script <- "run_job.sh"
    env_file <- "job_env.txt"
    submit_file <- "condor_job.submit"
    extra_transfer_files <- unique(extra_transfer_files[file.exists(extra_transfer_files)])
    extra_transfer_names <- basename(extra_transfer_files)

    if (.Platform$OS.type == "windows") {
      remote_dir <- normalizePath(remote_dir, winslash = "/", mustWork = FALSE)
    }

    clone_script_content <- sprintf(
      "\n#!/bin/bash\nset -o pipefail\nexport HOME=\"$PWD\"\nexport XDG_CACHE_HOME=\"$PWD/.cache\"\nexport GIT_CONFIG_NOSYSTEM=1\nexport GIT_CONFIG_GLOBAL=/dev/null\nexport GIT_TERMINAL_PROMPT=0\nexport GCM_INTERACTIVE=Never\nmkdir -p \"$XDG_CACHE_HOME/git\" 2>/dev/null || true\n\nexport GITHUB_PAT='%s'\nexport GITHUB_USERNAME='%s'\nexport GITHUB_ORGANIZATION='%s'\nexport GITHUB_REPO='%s'\nexport GITHUB_BRANCH='%s'\n%s\n\nGIT_CMD=(git -c credential.helper= -c credential.useHttpPath=false)\necho \"clone HOME: ${HOME:-<unset>}\"\necho \"clone XDG_CACHE_HOME: ${XDG_CACHE_HOME:-<unset>}\"\necho \"clone git credential helpers: $(\"${GIT_CMD[@]}\" config --show-origin --get-all credential.helper 2>/dev/null || echo none)\"\n\nif [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then\n    \"${GIT_CMD[@]}\" init\n    \"${GIT_CMD[@]}\" remote add origin https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git\n    \"${GIT_CMD[@]}\" config core.sparseCheckout true\n    echo \"$GITHUB_TARGET_FOLDER/\" >> .git/info/sparse-checkout\n    \"${GIT_CMD[@]}\" pull origin $GITHUB_BRANCH\nelse\n    \"${GIT_CMD[@]}\" clone -b $GITHUB_BRANCH https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git\nfi\n",
      github_pat, github_username, github_org, github_repo,
      branch, if (!is.null(target_folder)) sprintf("export GITHUB_TARGET_FOLDER='%s'", target_folder) else ""
    )
    writeLines(clone_script_content, con = clone_script, sep = "\n")

    run_script_content <- sprintf(
      "\n#!/usr/bin/env bash\nset -o pipefail\nexport HOME=\"$PWD\"\nexport XDG_CACHE_HOME=\"$PWD/.cache\"\nexport GIT_CONFIG_NOSYSTEM=1\nexport GIT_CONFIG_GLOBAL=/dev/null\nexport GIT_TERMINAL_PROMPT=0\nexport GCM_INTERACTIVE=Never\nmkdir -p \"$XDG_CACHE_HOME/git\" 2>/dev/null || true\n\n# Execute the clone script\nsource %s\n\n# Load environment variables from job_env.txt if present\nif [[ -f \"%s\" ]]; then\n  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' \"%s\" | sed 's/^/export /' > env_exports.sh\n  source env_exports.sh\nfi\n\n# Condor transfer audit. With stream_output enabled, these lines are visible\n# in condor_job.out shortly after the job starts, before any long MFCL run.\necho \"=== Condor transfer audit ===\"\necho \"started_at: $(date -Is 2>/dev/null || date)\"\necho \"submit_dir: $PWD\"\necho \"HOME: ${HOME:-<unset>}\"\necho \"XDG_CACHE_HOME: ${XDG_CACHE_HOME:-<unset>}\"\necho \"transferred files:\"\nls -lah . || true\nif [[ -f \"%s\" ]]; then\n  echo \"job_env keys:\"\n  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' \"%s\" | cut -d= -f1 | sed 's/^/  /' || true\nfi\nif [[ -n \"$fitted_model_bundle\" ]]; then\n  echo \"fitted_model_bundle: $fitted_model_bundle\"\n  ls -lah \"$fitted_model_bundle\" 2>/dev/null || ls -lah \"../$(basename \"$fitted_model_bundle\")\" 2>/dev/null || true\nfi\n\n# Determine working directory\nif [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then\n    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"\nelse\n    WORK_DIR=\"$GITHUB_REPO\"\nfi\n\n# Unset GitHub PAT for security\nunset GITHUB_PAT\n\n# Change to working directory and run make\ncd \"$WORK_DIR\" || exit 1\necho \"repo_dir: $PWD\"\necho \"git_branch: $(git -c credential.helper= rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)\"\necho \"git_head: $(git -c credential.helper= rev-parse --short HEAD 2>/dev/null || echo unknown)\"\necho \"base_dir: ${base_dir:-<unset>}\"\necho \"model_dir: ${model_dir:-<unset>}\"\necho \"input_recipe_enabled: ${input_recipe_enabled:-0}\"\necho \"build_inputs_on_missing: ${build_inputs_on_missing:-0}\"\necho \"Running make with options: %s\"\nmake %s\n\n# Archive the results\ncd ..\necho \"Archiving folder: $WORK_DIR...\"\ntar -czvf output_archive.tar.gz \"$WORK_DIR\"\n",
      clone_script, env_file, env_file, env_file, env_file, make_options, make_options
    )
    writeLines(run_script_content, con = run_script, sep = "\n")

    if (!is.null(condor_environment)) {
      if (is.list(condor_environment)) {
        env_lines <- vapply(names(condor_environment), function(name) {
          sprintf("%s=\"%s\"", name, condor_environment[[name]])
        }, character(1))
        writeLines(env_lines, env_file)
      } else if (is.character(condor_environment)) {
        env_pairs <- unlist(strsplit(condor_environment, "\\s+"))
        valid_env <- env_pairs[grepl("^[A-Za-z_][A-Za-z0-9_]*=", env_pairs)]
        quoted_env <- vapply(valid_env, function(pair) {
          parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
          if (length(parts) >= 2) {
            key <- parts[1]
            value <- paste(parts[-1], collapse = "=")
            sprintf("%s=\"%s\"", key, value)
          } else {
            pair
          }
        }, character(1))
        writeLines(quoted_env, env_file)
      }
    }

    condor_options <- c()
    if (!is.null(condor_cpus)) condor_options <- c(condor_options, sprintf("request_cpus = %s", condor_cpus))
    if (!is.null(condor_memory)) condor_options <- c(condor_options, sprintf("request_memory = %s", condor_memory))
    if (!is.null(condor_disk)) condor_options <- c(condor_options, sprintf("request_disk = %s", condor_disk))

    environment_string <- ""
    git_safe_env <- c(
      "HOME=.",
      "XDG_CACHE_HOME=.cache",
      "GIT_CONFIG_NOSYSTEM=1",
      "GIT_CONFIG_GLOBAL=/dev/null",
      "GIT_TERMINAL_PROMPT=0",
      "GCM_INTERACTIVE=Never"
    )
    if (!is.null(condor_environment)) {
      if (is.list(condor_environment)) {
        env_vars <- vapply(names(condor_environment), function(name) {
          sprintf("%s=%s", name, condor_environment[[name]])
        }, character(1))
        environment_string <- paste(c(git_safe_env, env_vars), collapse = " ")
      } else if (is.character(condor_environment)) {
        environment_string <- paste(c(git_safe_env, condor_environment), collapse = " ")
      }
    } else {
      environment_string <- paste(git_safe_env, collapse = " ")
    }

    batch_name_template <- if (!is.null(custom_batch_name)) custom_batch_name else "$(ClusterId)"
    condor_options <- paste(condor_options, collapse = "\n")
    exclusion_requirements <- c()
    if (!is.null(exclude_machines)) {
      exclusion_requirements <- c(exclusion_requirements, sprintf("Machine != \"%s\"", exclude_machines))
    }
    if (!is.null(exclude_slots)) {
      exclusion_requirements <- c(exclusion_requirements, sprintf("Name != \"%s\"", exclude_slots))
    }
    if (!is.null(slot_requirements)) exclusion_requirements <- c(exclusion_requirements, slot_requirements)
    requirements_string <- if (length(exclusion_requirements) > 0) {
      sprintf("Requirements = %s\n", paste(exclusion_requirements, collapse = " && "))
    } else {
      ""
    }

    transfer_inputs <- paste(c(clone_script, run_script, env_file, extra_transfer_names), collapse = ", ")
    submit_file_content <- sprintf(
      "\nUniverse   = docker\nDockerImage = %s\nExecutable = /bin/bash\nArguments  = %s\nShouldTransferFiles = YES\nTransferInputFiles = %s\nTransferOutputFiles = output_archive.tar.gz\nOutput     = condor_job.out\nError      = condor_job.err\nLog        = condor_job.log\nstream_output = %s\nstream_error = %s\ngetenv = True\nbatch_name = %s\n%s%s%s\nQueue\n",
      docker_image, run_script, transfer_inputs, stream_error, stream_error, batch_name_template,
      requirements_string,
      if (nzchar(environment_string)) sprintf("environment = %s\n", environment_string) else "",
      if (nzchar(condor_options)) paste0(condor_options, "\n") else ""
    )
    writeLines(submit_file_content, con = submit_file, sep = "\n")

    message("Creating remote directory...")
    system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
    message("Transferring files...")
    for (f in c(clone_script, run_script, env_file, submit_file, extra_transfer_files)) {
      system(sprintf("scp %s %s@%s:%s/%s", shQuote(f), remote_user, remote_host, remote_dir, basename(f)))
    }
    Sys.sleep(1)

    if (ghcr_login) {
      message("Performing docker login...")
      if (tolower(remote_os) == "windows") {
        config_cmd <- "mkdir %USERPROFILE%\\.docker && echo \"{\\\"credsStore\\\": \\\"wincred\\\"}\" > %USERPROFILE%\\.docker\\config.json"
        system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
        login_cmd <- sprintf("echo %s | docker login ghcr.io -u %s --password-stdin", shQuote(github_pat), github_username)
      } else {
        config_cmd <- "mkdir -p /tmp/docker_config && echo '{}' > /tmp/docker_config/config.json"
        system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
        login_cmd <- sprintf("echo %s | DOCKER_CONFIG=/tmp/docker_config docker login ghcr.io -u %s --password-stdin", shQuote(github_pat), github_username)
      }
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, login_cmd))
    }

    message("Submitting job...")
    submit_result <- system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", remote_user, remote_host, remote_dir, submit_file), intern = TRUE)
    job_id <- NULL
    message("Submit result:")
    for (line in submit_result) message(line)
    for (line in submit_result) {
      if (grepl("submitted to cluster|cluster", line, ignore.case = TRUE)) {
        numbers <- regmatches(line, gregexpr("\\d+", line))[[1]]
        if (length(numbers) > 0) {
          job_id <- numbers[length(numbers)]
          break
        }
      }
    }
    if (is.null(job_id)) {
      for (line in submit_result) {
        if (grepl("\\d+", line)) {
          numbers <- regmatches(line, gregexpr("\\d+", line))[[1]]
          if (length(numbers) > 0) {
            job_id <- numbers[length(numbers)]
            break
          }
        }
      }
    }
    actual_batch_name <- if (!is.null(custom_batch_name)) custom_batch_name else job_id
    if (!is.null(job_id) && nzchar(job_id)) {
      message(sprintf("Job submitted successfully! Job ID: %s, Batch Name: %s", job_id, actual_batch_name))
      if (identical(rmclone_script, "yes")) {
        message("Skipping clone script monitoring in fitted-source launcher wrapper.")
      }
    } else {
      message("Job submitted but could not extract job ID")
      job_id <- "unknown"
    }
    unlink(c(clone_script, run_script, env_file, submit_file, extra_transfer_files), force = TRUE)
    message("Process completed.")
    job_id
  }

  submit_condorbox_job <- function(..., extra_transfer_files = character(0)) {
    extra_transfer_files <- extra_transfer_files[file.exists(extra_transfer_files)]
    if (length(extra_transfer_files) == 0) {
      return(CondorBox::CondorBox(...))
    }
    condorbox_with_extra_transfer(..., extra_transfer_files = extra_transfer_files)
  }

  build_condor_exclude_slots <- function(remote_user, remote_host, run_target) {
    base_exclude <- default_condor_exclude_slots()
    patterns <- condor_target_patterns(run_target = run_target)

    slot_table <- fetch_condor_slot_table(remote_user = remote_user, remote_host = remote_host)
    all_slots <- unique(slot_table$slot)
    matched_slots <- match_condor_slots_by_patterns(slot_table, patterns)
    if (length(all_slots) == 0 || length(matched_slots) == 0) {
      return(list(
        exclude_slots = base_exclude,
        matched_slots = matched_slots,
        all_slots = all_slots,
        target_mode = if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
      ))
    }
    additional_exclude <- setdiff(all_slots, matched_slots)
    list(
      exclude_slots = unique(c(base_exclude, additional_exclude)),
      matched_slots = matched_slots,
      all_slots = all_slots,
      target_mode = if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
    )
  }

  launch_single_job_local_raw <- function(spec, common_params) {
    model_env_list <- if (!is.null(spec$profile_env) && is.list(spec$profile_env)) spec$profile_env else common_params$model_env_lists[[spec$model_name]]
    if (is.null(model_env_list)) {
      stop(paste("Model env not found for", spec$model_name))
    }

    job_env <- model_env_list
    job_env <- set_prefer_par_start_env(job_env, isTRUE(common_params$prefer_par_start))
    launch_name <- launch_model_name(spec$model_name, job_env)
    # Profile sets are launch-time metadata, not runtime env vars.
    # Remove to avoid passing large/non-scalar values to env.
    job_env$profile_sets <- NULL
    batch_suffix <- ""
    profile_tag <- if (identical(spec$job_type, "prof") || identical(spec$job_type, "prof_chain") || identical(spec$job_type, "prof_2d")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""

    if (!is.null(spec$seed)) {
      job_env$jitter_seed <- as.character(spec$seed)
      batch_suffix <- paste0("-jitter", spec$seed)
    } else if (!is.null(spec$part)) {
      job_env$hessian_part <- as.character(spec$part)
      batch_suffix <- paste0("-hess", spec$part)
    } else if (!is.null(spec$peel)) {
      job_env$retro_peel <- as.character(spec$peel)
      batch_suffix <- paste0("-retro", spec$peel)
    } else if (!is.null(spec$selftest_rep)) {
      job_env <- apply_selftest_rep_to_env(job_env, spec$selftest_rep)
      batch_suffix <- sprintf("-selftest-rep%03d", as.integer(spec$selftest_rep))
    } else if (!is.null(spec$scalar)) {
      job_env$scalar <- as.character(spec$scalar)
      if (identical(spec$job_type, "prof")) {
        job_env <- apply_prof_init_mapping(job_env, spec$scalar)
        if (!is.null(spec$init_from_scalar_override) && is.finite(suppressWarnings(as.numeric(spec$init_from_scalar_override)))) {
          job_env$init_from_scalar <- as.character(spec$init_from_scalar_override)
        }
      }
      batch_suffix <- paste0(profile_suffix, "-sc", spec$scalar)
    } else if (identical(spec$job_type, "prof_chain")) {
      if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) {
        job_env$chain_name <- as.character(spec$chain_name)
      }
      if (!is.null(spec$chain_scalars) && nzchar(as.character(spec$chain_scalars))) {
        job_env$chain_scalars <- as.character(spec$chain_scalars)
      }
      if (!is.null(spec$chain_first_init_from) && nzchar(as.character(spec$chain_first_init_from))) {
        job_env$chain_first_init_from <- as.character(spec$chain_first_init_from)
      }
      if (!is.null(spec$chain_anchor) && nzchar(as.character(spec$chain_anchor))) {
        job_env$chain_anchor <- as.character(spec$chain_anchor)
      }
      batch_suffix <- paste0(profile_suffix, "-profchain", if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) paste0("-", as.character(spec$chain_name)) else "")
    } else if (identical(spec$job_type, "prof_2d")) {
      batch_suffix <- paste0(profile_suffix, "-prof2d")
    } else if (identical(spec$job_type, "stage_check")) {
      batch_suffix <- "-stagecheck"
    }

    batch_name <- paste0(launch_name, batch_suffix, "-local-", format(Sys.time(), "%H:%M:%S"), "-", Sys.getpid())
    log_dir <- file.path(common_params$repo_root, "logs", "shiny_launcher_local")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(log_dir, paste0("shiny_launcher_local_", gsub("[^A-Za-z0-9_\\-]", "_", batch_name), ".log"))

    if (!dir.exists(common_params$repo_root)) {
      stop("Repo root does not exist for local run: ", common_params$repo_root)
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(common_params$repo_root)

    run_spec <- local_run_command(spec, common_params, job_env, log_file)

    writeLines(
      c(
        paste("mode:", if (isTRUE(common_params$local_use_docker)) "docker" else "native"),
        paste("job_type:", spec$job_type),
        paste("model_name:", launch_name)
      ),
      con = log_file
    )

    run_spec <- local_run_command(spec, common_params, job_env, log_file)
    write(paste("command:", local_command_preview(run_spec)), file = log_file, append = TRUE)
    write("", file = log_file, append = TRUE)

    exit_status <- tryCatch(
      system2(
        run_spec$command,
        args = run_spec$args,
        env = run_spec$env,
        stdout = run_spec$log_file,
        stderr = run_spec$log_file
      ),
      error = function(e) {
        structure(127L, message = conditionMessage(e))
      }
    )

    if (!is.numeric(exit_status) || length(exit_status) != 1 || is.na(exit_status)) {
      exit_status <- 127L
    }
    if (!identical(as.integer(exit_status), 0L)) {
      output <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
      cmd_error <- attr(exit_status, "message")
      stop(
        paste(
          c(
            sprintf("Local %s run failed for %s (status %s).", spec$job_type, launch_name, exit_status),
            if (!is.null(cmd_error) && nzchar(cmd_error)) paste("Command error:", cmd_error),
            paste("Log file:", log_file),
            tail(as.character(output), 20)
          ),
          collapse = "\n"
        )
      )
    }

    list(
      batch_name = batch_name,
      remote_dir = if (!is.null(job_env$model_dir)) as.character(job_env$model_dir) else "",
      job_id = batch_name,
      mode = "local",
      log_file = log_file,
      model_name = launch_name,
      job_type = spec$job_type,
      job_env = job_env
    )
  }

  launch_single_job_local <- function(model_name, model_env, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL, selftest_rep = NULL, log = TRUE) {
    launch_name <- launch_model_name(model_name, model_env)
    display_name <- if (identical(job_type, "prof") || identical(job_type, "prof_chain") || identical(job_type, "prof_2d")) profile_launch_name(launch_name, model_env) else launch_name
    if (isTRUE(log)) {
      rv$launch_log <- paste0(
        rv$launch_log,
        "  → ",
        display_name,
        if (!is.null(seed)) paste0(" seed ", seed) else if (!is.null(part)) paste0(" part ", part) else if (!is.null(peel)) paste0(" peel ", peel) else if (!is.null(scalar)) paste0(" scalar ", scalar) else "",
        if (!is.null(selftest_rep)) paste0(" self-test rep ", selftest_rep) else "",
        " [local",
        if (identical(input$launch_mode, "local_docker")) ", docker" else ", native",
        "]\n"
      )
    }

    common_params <- list(
      repo_root = isolate(repo_root_val()),
      local_use_docker = identical(input$launch_mode, "local_docker"),
      docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
      prefer_par_start = launch_prefer_par_start(),
      model_env_lists = setNames(list(as.list(model_env, all.names = TRUE)), model_name)
    )

    launch_single_job_local_raw(
      spec = list(
        model_name = model_name,
        job_type = job_type,
        seed = seed,
        part = part,
        peel = peel,
        scalar = scalar,
        selftest_rep = selftest_rep
      ),
      common_params = common_params
    )
  }

  parse_numeric_tokens <- function(x) {
    txt <- paste(as.character(x), collapse = " ")
    if (is.null(x) || !nzchar(trimws(txt))) return(numeric(0))
    m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", txt, perl = TRUE)
    toks <- regmatches(txt, m)[[1]]
    vals <- suppressWarnings(as.numeric(toks))
    vals[is.finite(vals)]
  }

  collapse_model_field <- function(x) {
    if (is.null(x) || length(x) == 0) return("NA")
    if (is.list(x)) return(paste(vapply(x, collapse_model_field, character(1)), collapse = " | "))
    vals <- as.character(x)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) "NA" else paste(vals, collapse = " ")
  }

  build_model_config_details <- function(model_names) {
    if (length(model_names) == 0) return("No selected models.")
    sections <- lapply(model_names, function(model_name) {
      model_env <- active_model_env(model_name)
      if (is.null(model_env) || !is.list(model_env)) {
        return(paste0(launch_unit_label(model_name), "\n  <model config not found>"))
      }
      fields <- names(model_env)
      has_profile_sets <- !is.null(model_env$profile_sets) && length(model_env$profile_sets) > 0
      if (has_profile_sets) {
        fields <- setdiff(
          fields,
          c(
            "Reps",
            "scalars",
            "prof_fix_indepvar",
            "prof_fix_values",
            "prof_fix_indepvar_file",
            "indepvar_reps",
            "prof_extra_switch"
          )
        )
      }
      if (is.null(fields) || length(fields) == 0) {
        return(paste0(launch_unit_label(model_name), "\n  <empty model config>"))
      }
      field_lines <- vapply(fields, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(model_env[[nm]]))
      }, character(1))
      paste(c(launch_unit_label(model_name), field_lines), collapse = "\n")
    })
    paste(sections, collapse = "\n\n")
  }

  build_single_job_config_block <- function(job_result) {
    if (is.null(job_result)) return("NA")
    model_name <- if (!is.null(job_result$model_name) && nzchar(as.character(job_result$model_name))) as.character(job_result$model_name) else "NA"
    job_type <- if (!is.null(job_result$job_type) && nzchar(as.character(job_result$job_type))) as.character(job_result$job_type) else "NA"
    batch_name <- if (!is.null(job_result$batch_name) && nzchar(as.character(job_result$batch_name))) as.character(job_result$batch_name) else "NA"
    env <- job_result$job_env
    if (is.null(env) || !is.list(env) || length(env) == 0) {
      env_lines <- "  <job env not captured>"
    } else {
      env_names_all <- names(env)
      env_names_all <- env_names_all[!is.na(env_names_all) & nzchar(env_names_all)]

      common_fields <- c(
        "description",
        "config_summary",
        "profile_set_name",
        "base_dir",
        "model_dir",
        "launcher_input_recipe_label",
        "launcher_input_recipe_description",
        "input_recipe_base",
        "input_recipe_base_input_dir",
        "input_recipe_base_source",
        "input_recipe_base_tokens",
        "input_recipe_fixed_params",
        "input_recipe_movement_pairs",
        "input_recipe_sel_nodes",
        "input_recipe_index_cv_half",
        "build_inputs_on_missing",
        "fitted_model_source_enabled",
        "fitted_model_source_label",
        "fitted_model_source_dir",
        "fitted_model_bundle",
        "auto_run_model_before_dependency",
        "auto_fitted_model_dir",
        "prefer_par_start",
        "program_path",
        "mfcl_commands",
        "Reps",
        "scalars",
        "min_year",
        "n_mixing_periods",
        "DOCKER_IMAGE"
      )
    type_fields <- switch(
      job_type,
      stage_check = c("build_inputs_on_missing", "input_recipe_enabled", "fitted_model_source_enabled", "fitted_model_bundle", "auto_run_model_before_dependency"),
      model = c("model_hessian"),
	      selftest = c("selftest_reps", "selftest_source_mode", "selftest_refit_mode", "selftest_refit_fevals", "selftest_base_dir", "selftest_model_dir", "selftest_source_par", "selftest_dir", "selftest_require_native_tags", "selftest_update_catch", "selftest_update_effort"),
      retro = c("retro_peel", "retro_peels", "retro_hessian"),
      jitter = c("jitter_seed", "jitter_cv", "jitter_seeds", "jitter_hessian", "jitter_base_source"),
      hessian = c("hessian_part", "nsplit", "model_hessian"),
      prof = c("profile_set_name", "scalar", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "init_from_scalar", "init_par_override", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
      prof_chain = c("profile_set_name", "chain_name", "chain_anchor", "chain_scalars", "chain_first_init_from", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
      prof_2d = c("profile_set_name", "prof_hessian", "prof_init_map_rds", "init_par_override", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch", "prof_2d_extra_switch", "prof_2d_indepvar", "prof_2d_scalars_x", "prof_2d_scalars_y", "prof_2d_values_x", "prof_2d_values_y", "prof_2d_path", "prof_2d_anchor_x", "prof_2d_anchor_y", "prof_2d_parallel_jobs"),
      character(0)
      )

      preferred_order <- unique(c(common_fields, type_fields))
      env_names <- intersect(preferred_order, env_names_all)
      if (length(env_names) == 0) {
        env_names <- sort(env_names_all)
      }

      env_lines <- vapply(env_names, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(env[[nm]]))
      }, character(1))
    }
    paste(c(
      paste0(launch_unit_label(model_name), " [", job_type, "]"),
      paste0("  batch_name : ", batch_name),
      env_lines
    ), collapse = "\n")
  }

  build_launched_config_details <- function(job_results) {
    if (length(job_results) == 0) return("No launched jobs.")

    common_fields <- c(
      "description",
      "config_summary",
      "profile_set_name",
      "base_dir",
      "model_dir",
      "launcher_input_recipe_label",
      "launcher_input_recipe_description",
      "input_recipe_base",
      "input_recipe_base_input_dir",
      "input_recipe_base_source",
      "input_recipe_base_tokens",
      "input_recipe_fixed_params",
      "input_recipe_movement_pairs",
      "input_recipe_sel_nodes",
      "input_recipe_index_cv_half",
      "build_inputs_on_missing",
      "fitted_model_source_enabled",
      "fitted_model_source_label",
      "fitted_model_source_dir",
      "fitted_model_bundle",
      "auto_run_model_before_dependency",
      "auto_fitted_model_dir",
      "prefer_par_start",
      "program_path",
      "mfcl_commands",
      "Reps",
      "scalars",
      "min_year",
      "n_mixing_periods",
      "DOCKER_IMAGE"
    )
    type_fields_for <- function(job_type) {
      switch(
        job_type,
        stage_check = c("build_inputs_on_missing", "input_recipe_enabled", "fitted_model_source_enabled", "fitted_model_bundle", "auto_run_model_before_dependency"),
        model = c("model_hessian"),
	        selftest = c("selftest_reps", "selftest_source_mode", "selftest_refit_mode", "selftest_refit_fevals", "selftest_base_dir", "selftest_model_dir", "selftest_source_par", "selftest_dir", "selftest_require_native_tags", "selftest_update_catch", "selftest_update_effort"),
        retro = c("retro_peel", "retro_peels", "retro_hessian"),
        jitter = c("jitter_seed", "jitter_cv", "jitter_seeds", "jitter_hessian", "jitter_base_source"),
        hessian = c("hessian_part", "nsplit", "model_hessian"),
        prof = c("profile_set_name", "scalar", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "init_from_scalar", "init_par_override", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
        prof_chain = c("profile_set_name", "chain_name", "chain_anchor", "chain_scalars", "chain_first_init_from", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
        prof_2d = c("profile_set_name", "prof_hessian", "prof_init_map_rds", "init_par_override", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch", "prof_2d_extra_switch", "prof_2d_indepvar", "prof_2d_scalars_x", "prof_2d_scalars_y", "prof_2d_values_x", "prof_2d_values_y", "prof_2d_path", "prof_2d_anchor_x", "prof_2d_anchor_y", "prof_2d_parallel_jobs"),
        character(0)
      )
    }
    format_env_lines <- function(env, fields) {
      if (is.null(env) || !is.list(env) || length(env) == 0) return(character(0))
      env_names <- names(env)
      env_names <- env_names[!is.na(env_names) & nzchar(env_names)]
      keep <- intersect(fields, env_names)
      vapply(keep, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(env[[nm]]))
      }, character(1))
    }

    valid <- Filter(function(x) !is.null(x) && is.list(x), job_results)
    if (length(valid) == 0) return("No launched jobs.")

    model_keys <- vapply(valid, function(x) {
      if (!is.null(x$model_name) && nzchar(as.character(x$model_name))) as.character(x$model_name) else "NA"
    }, character(1))
    grouped <- split(valid, model_keys)

    sections <- lapply(names(grouped), function(model_name) {
      jobs <- grouped[[model_name]]
      first_env <- NULL
      for (j in jobs) {
        if (!is.null(j$job_env) && is.list(j$job_env) && length(j$job_env) > 0) {
          first_env <- j$job_env
          break
        }
      }
      common_lines <- format_env_lines(first_env, common_fields)
      if (length(common_lines) == 0) {
        common_lines <- "  <common config not captured>"
      }

      job_blocks <- vapply(jobs, function(j) {
        jt <- if (!is.null(j$job_type) && nzchar(as.character(j$job_type))) as.character(j$job_type) else "NA"
        bn <- if (!is.null(j$batch_name) && nzchar(as.character(j$batch_name))) as.character(j$batch_name) else "NA"
        env_use <- j$job_env
        if ((is.null(env_use) || !is.list(env_use) || length(env_use) == 0) &&
            !is.null(j$spec) && is.list(j$spec) && length(j$spec) > 0) {
          env_use <- j$spec
        }
        type_lines <- format_env_lines(env_use, type_fields_for(jt))
        if (length(type_lines) == 0) {
          if (!is.null(env_use) && is.list(env_use) && length(env_use) > 0) {
            env_names_all <- names(env_use)
            env_names_all <- env_names_all[!is.na(env_names_all) & nzchar(env_names_all)]
            fallback_fields <- setdiff(env_names_all, common_fields)
            if (length(fallback_fields) > 0) {
              type_lines <- vapply(fallback_fields, function(nm) {
                paste0("  ", nm, " : ", collapse_model_field(env_use[[nm]]))
              }, character(1))
            } else {
              type_lines <- "  <no job-type specific config>"
            }
          } else {
            type_lines <- "  <no job-type specific config>"
          }
        }
        if (identical(jt, "prof_chain")) {
          has_chain <- any(grepl("^\\s+chain_(name|anchor|scalars|first_init_from)\\s*:", type_lines))
          if (!has_chain) {
            chain_nm <- if (!is.null(j$spec$chain_name) && nzchar(as.character(j$spec$chain_name))) {
              as.character(j$spec$chain_name)
            } else if (grepl("profchain-up", bn, fixed = TRUE)) {
              "up"
            } else if (grepl("profchain-down", bn, fixed = TRUE)) {
              "down"
            } else {
              "NA"
            }
            chain_sc <- if (!is.null(j$spec$chain_scalars) && nzchar(as.character(j$spec$chain_scalars))) as.character(j$spec$chain_scalars) else "NA"
            chain_init <- if (!is.null(j$spec$chain_first_init_from) && nzchar(as.character(j$spec$chain_first_init_from))) as.character(j$spec$chain_first_init_from) else "NA"
            chain_anchor <- if (!is.null(j$spec$chain_anchor) && nzchar(as.character(j$spec$chain_anchor))) as.character(j$spec$chain_anchor) else "NA"
            type_lines <- c(
              paste0("  chain_name : ", chain_nm),
              paste0("  chain_anchor : ", chain_anchor),
              paste0("  chain_scalars : ", chain_sc),
              paste0("  chain_first_init_from : ", chain_init),
              type_lines
            )
          }
        }
        paste(c(
      paste0(launch_unit_label(model_name), " [", jt, "]"),
          paste0("  batch_name : ", bn),
          type_lines
        ), collapse = "\n")
      }, character(1))

      paste(
        c(
      paste0(launch_unit_label(model_name), " [common]"),
          common_lines,
          "",
          paste(job_blocks, collapse = "\n\n----------------------------------------\n\n")
        ),
        collapse = "\n"
      )
    })

    paste(sections, collapse = "\n\n========================================\n\n")
  }

  selected_models_from_checkboxes <- function() {
    selected_launch_units()
  }

  observe({
    if (length(rv$models) == 0) return()
    rv$selected_models <- selected_models_from_checkboxes()
  })

  estimate_total_jobs <- function(selected_models, selected_job_types, prof_chain_mode = FALSE, is_local_mode = FALSE, prof_anchor_requested = 100, local_prof_chain_parallel = FALSE) {
    total_jobs <- 0L
    selected_job_types <- effective_selected_job_types(selected_job_types)
    if (length(selected_models) == 0 || length(selected_job_types) == 0) return(total_jobs)

    for (model_name in selected_models) {
      model_env <- active_model_env(model_name)
      if (is.null(model_env)) next
      for (job_type in selected_job_types) {
        if (identical(job_type, "jitter")) {
          total_jobs <- total_jobs + length(parse_numeric_tokens(model_env$jitter_seeds))
        } else if (identical(job_type, "hessian")) {
          nsplit <- suppressWarnings(as.integer(model_env$nsplit))
          if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
          total_jobs <- total_jobs + nsplit
        } else if (identical(job_type, "retro")) {
          total_jobs <- total_jobs + length(parse_numeric_tokens(model_env$retro_peels))
        } else if (identical(job_type, "selftest")) {
          total_jobs <- total_jobs + length(selftest_reps_for_launch())
        } else if (identical(job_type, "prof")) {
          for (prof_env in selected_profile_1d_envs(model_env)) {
            scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
            if (isTRUE(prof_chain_mode) && (!isTRUE(is_local_mode) || isTRUE(local_prof_chain_parallel))) {
              plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
              down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
              up_chain <- plan$upper
              if (length(down_chain) > 0) total_jobs <- total_jobs + 1L
              if (length(up_chain) > 0) total_jobs <- total_jobs + 1L
            } else {
              total_jobs <- total_jobs + length(scalars)
            }
          }
        } else if (identical(job_type, "prof_2d")) {
          total_jobs <- total_jobs + length(selected_profile_2d_envs(model_env))
        } else {
          total_jobs <- total_jobs + 1L
        }
      }
    }
    as.integer(total_jobs)
  }

  estimate_jobs_breakdown <- function(selected_models, selected_job_types,
                                      prof_chain_mode = FALSE,
                                      is_local_mode = FALSE,
                                      prof_anchor_requested = 100,
                                      local_prof_chain_parallel = FALSE) {
    rows <- list()
    selected_job_types <- effective_selected_job_types(selected_job_types)
    if (length(selected_models) == 0 || length(selected_job_types) == 0) {
      return(data.frame(model = character(0), item = character(0), jobs = integer(0), stringsAsFactors = FALSE))
    }

    for (model_name in selected_models) {
      model_env <- active_model_env(model_name)
      if (is.null(model_env)) next

      for (job_type in selected_job_types) {
        if (identical(job_type, "jitter")) {
          jobs <- length(parse_numeric_tokens(model_env$jitter_seeds))
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = "jitter", jobs = as.integer(jobs), stringsAsFactors = FALSE)
        } else if (identical(job_type, "hessian")) {
          nsplit <- suppressWarnings(as.integer(model_env$nsplit))
          if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = "hessian", jobs = as.integer(nsplit), stringsAsFactors = FALSE)
        } else if (identical(job_type, "retro")) {
          jobs <- length(parse_numeric_tokens(model_env$retro_peels))
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = "retro", jobs = as.integer(jobs), stringsAsFactors = FALSE)
        } else if (identical(job_type, "selftest")) {
          jobs <- length(selftest_reps_for_launch())
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = "self-test", jobs = as.integer(jobs), stringsAsFactors = FALSE)
        } else if (identical(job_type, "prof")) {
          prof_envs <- selected_profile_1d_envs(model_env)
          for (prof_env in prof_envs) {
            nm <- first_scalar_string(prof_env$profile_set_name, default = "profile")
            scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
            if (isTRUE(prof_chain_mode) && (!isTRUE(is_local_mode) || isTRUE(local_prof_chain_parallel))) {
              plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
              down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
              up_chain <- plan$upper
              jobs <- as.integer((length(down_chain) > 0) + (length(up_chain) > 0))
              rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = paste0("prof:", nm, " (chains)"), jobs = jobs, stringsAsFactors = FALSE)
            } else {
              rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = paste0("prof:", nm), jobs = as.integer(length(scalars)), stringsAsFactors = FALSE)
            }
          }
        } else if (identical(job_type, "prof_2d")) {
          prof_envs <- selected_profile_2d_envs(model_env)
          for (prof_env in prof_envs) {
            nm <- first_scalar_string(prof_env$profile_set_name, default = "profile")
            rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = paste0("prof_2d:", nm), jobs = 1L, stringsAsFactors = FALSE)
          }
        } else if (identical(job_type, "model")) {
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = "model", jobs = 1L, stringsAsFactors = FALSE)
        } else {
          rows[[length(rows) + 1L]] <- data.frame(model = model_name, item = job_type, jobs = 1L, stringsAsFactors = FALSE)
        }
      }
    }

    if (length(rows) == 0) {
      return(data.frame(model = character(0), item = character(0), jobs = integer(0), stringsAsFactors = FALSE))
    }
    do.call(rbind, rows)
  }

  output$estimated_jobs_breakdown_text <- renderText({
    if (length(rv$models) == 0) return("")
    selected_job_types <- input$job_types
    selected_job_types <- effective_selected_job_types(selected_job_types)
    if (is.null(selected_job_types) || length(selected_job_types) == 0) return("")
    selected_models <- selected_models_from_checkboxes()
    if (length(selected_models) == 0) return("")

    launch_mode <- if (!is.null(input$launch_mode) && nzchar(input$launch_mode)) input$launch_mode else "condor"
    is_local_mode <- identical(launch_mode, "local_native") || identical(launch_mode, "local_docker")
    prof_chain_mode <- identical(input$prof_launch_strategy, "seq_anchor_bidir") && "prof" %in% selected_job_types
    prof_anchor_requested <- suppressWarnings(as.numeric(input$prof_anchor_scalar))
    if (!is.finite(prof_anchor_requested)) prof_anchor_requested <- 100

    bd <- estimate_jobs_breakdown(
      selected_models,
      selected_job_types,
      prof_chain_mode = prof_chain_mode,
      is_local_mode = is_local_mode,
      prof_anchor_requested = prof_anchor_requested,
      local_prof_chain_parallel = isTRUE(input$parallel_launch)
    )
    if (!is.data.frame(bd) || nrow(bd) == 0) return("")

    model_split <- split(bd, bd$model)
    model_text <- vapply(names(model_split), function(mn) {
      piece <- model_split[[mn]]
      item_txt <- paste0(piece$item, "=", piece$jobs, collapse = "; ")
      paste0(launch_unit_label(mn), ": ", item_txt)
    }, character(1), USE.NAMES = FALSE)
    paste(model_text, collapse = " | ")
  })

  output$estimated_jobs_text <- renderText({
    if (length(rv$models) == 0) return("0 (load config first)")
    selected_job_types <- input$job_types
    selected_job_types <- effective_selected_job_types(selected_job_types)
    if (is.null(selected_job_types) || length(selected_job_types) == 0) return("0 (select job type)")
    selected_models <- selected_models_from_checkboxes()
    if (length(selected_models) == 0 &&
        isTRUE(sensitivity_mode_enabled()) &&
        length(selected_sensitivity_ids()) == 0 &&
        !isTRUE(include_base_launch_units())) {
      return("0 (select sensitivity or include base)")
    }
    if (length(selected_models) == 0 && isTRUE(sensitivity_mode_enabled())) {
      return("0 (no launch units)")
    }
    if (length(selected_models) == 0) {
      return(if (identical(launch_input_mode(), "existing")) "0 (select existing input)" else "0 (select base input)")
    }
    launch_mode <- if (!is.null(input$launch_mode) && nzchar(input$launch_mode)) input$launch_mode else "condor"
    is_local_mode <- identical(launch_mode, "local_native") || identical(launch_mode, "local_docker")
    prof_chain_mode <- identical(input$prof_launch_strategy, "seq_anchor_bidir") && "prof" %in% selected_job_types
    prof_anchor_requested <- suppressWarnings(as.numeric(input$prof_anchor_scalar))
    if (!is.finite(prof_anchor_requested)) prof_anchor_requested <- 100

    est <- estimate_total_jobs(
      selected_models,
      selected_job_types,
      prof_chain_mode = prof_chain_mode,
      is_local_mode = is_local_mode,
      prof_anchor_requested = prof_anchor_requested,
      local_prof_chain_parallel = isTRUE(input$parallel_launch)
    )

    if (isTRUE(prof_chain_mode) && !isTRUE(is_local_mode) && length(selected_models) == 1 && "prof" %in% selected_job_types) {
      m <- selected_models[[1]]
      me <- active_model_env(m)
      prof_envs <- selected_profile_1d_envs(me)
      if (length(prof_envs) == 1) {
        sc <- resolve_prof_scalars_for_mode(prof_envs[[1]], prof_chain_mode = TRUE)
        plan <- resolve_prof_anchor_and_chains(sc, prof_anchor_requested)
        down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
        up_chain <- plan$upper
        return(sprintf(
          "%d [seq:%s raw=%s class=%s direct=%s parsed=%s anchor=%s req=%s down=%d up=%d]",
          as.integer(est),
          m,
          paste(as.character(me$scalars), collapse = "|"),
          paste(class(me$scalars), collapse = "/"),
          {
            dvals <- parse_numeric_tokens(me$scalars)
            if (length(dvals) > 0) paste(dvals, collapse = ",") else "<none>"
          },
          if (length(sc) > 0) paste(sc, collapse = ",") else "<none>",
          ifelse(is.finite(plan$anchor), format(plan$anchor, scientific = FALSE, trim = TRUE), "NA"),
          format(prof_anchor_requested, scientific = FALSE, trim = TRUE),
          length(down_chain),
          length(up_chain)
        ))
      }

      prof_desc <- vapply(prof_envs, function(env) {
        nm <- first_scalar_string(env$profile_set_name, default = "profile")
        sc <- resolve_prof_scalars_for_mode(env, prof_chain_mode = TRUE)
        sprintf("%s:%d", nm, length(sc))
      }, character(1))
      return(sprintf("%d [seq:%s multi-profile %s]", as.integer(est), m, paste(prof_desc, collapse = "; ")))
    }

    as.character(est)
  })
  
  observeEvent(input$launch_btn, {
    if (length(rv$models) == 0) { 
      showNotification("Please load launch settings first", type = "error")
      return() 
    }
    
    # Recompute selected models from current checkbox inputs
    selected_models <- selected_models_from_checkboxes()
    rv$selected_models <- selected_models
    
    # Validate model selection
    if (length(selected_models) == 0) { 
      msg <- if (isTRUE(sensitivity_mode_enabled()) &&
                 length(selected_sensitivity_ids()) == 0 &&
                 !isTRUE(include_base_launch_units())) {
        "Please select at least one sensitivity, or include the selected base input(s)"
      } else if (isTRUE(sensitivity_mode_enabled())) {
        "No launch units for the selected base input(s)"
      } else if (identical(launch_input_mode(), "existing")) {
        "Please select one or more existing inputs"
      } else {
        "Please select a base input"
      }
      showNotification(msg, type = "error")
      return() 
    }
    
    # Disable button during processing
    shinyjs::disable("launch_btn")
    shinyjs::addClass("launch_btn", "loading")
    
    selected_job_types <- effective_selected_job_types(input$job_types)
    if (is.null(selected_job_types) || length(selected_job_types) == 0) {
      showNotification("Please select at least one job type", type = "error")
      shinyjs::enable("launch_btn")
      shinyjs::removeClass("launch_btn", "loading")
      return()
    }

    launch_mode <- if (!is.null(input$launch_mode) && nzchar(input$launch_mode)) input$launch_mode else "condor"
    is_local_mode <- launch_mode %in% c("local_native", "local_docker")
    is_local_docker <- identical(launch_mode, "local_docker")
    prof_chain_mode <- identical(input$prof_launch_strategy, "seq_anchor_bidir") && "prof" %in% selected_job_types
    allow_local_prof_chain_parallel <- isTRUE(prof_chain_mode) && isTRUE(is_local_mode) && isTRUE(input$parallel_launch)
    prof_anchor_requested <- suppressWarnings(as.numeric(input$prof_anchor_scalar))
    if (!is.finite(prof_anchor_requested)) prof_anchor_requested <- 100
    
    # Calculate total number of jobs to be launched
    total_jobs <- 0
    job_specs <- list()
  add_job_spec <- function(model_name, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL,
                           selftest_rep = NULL,
                           chain_name = NULL, chain_scalars = NULL, chain_first_init_from = NULL, chain_anchor = NULL,
                           profile_env = NULL) {
      job_specs[[length(job_specs) + 1]] <<- list(
        model_name = model_name,
        job_type = job_type,
        seed = seed,
        part = part,
        peel = peel,
        scalar = scalar,
        selftest_rep = selftest_rep,
        chain_name = chain_name,
        chain_scalars = chain_scalars,
        chain_first_init_from = chain_first_init_from,
        chain_anchor = chain_anchor,
        profile_env = profile_env
      )
    }
    for (model_name in selected_models) {
      model_env <- active_model_env(model_name)
      for (job_type in selected_job_types) {
        if (job_type == "jitter") {
          seeds <- parse_numeric_tokens(model_env$jitter_seeds)
          total_jobs <- total_jobs + length(seeds)
          for (seed in seeds) {
            add_job_spec(model_name, job_type, seed = seed)
          }
        } else if (job_type == "hessian") {
          nsplit <- suppressWarnings(as.integer(model_env$nsplit))
          if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
          total_jobs <- total_jobs + nsplit
          for (part in seq_len(nsplit)) {
            add_job_spec(model_name, job_type, part = part)
          }
        } else if (job_type == "retro") {
          peels <- parse_numeric_tokens(model_env$retro_peels)
          total_jobs <- total_jobs + length(peels)
          for (peel in peels) {
            add_job_spec(model_name, job_type, peel = peel)
          }
        } else if (job_type == "selftest") {
          reps <- selftest_reps_for_launch()
          total_jobs <- total_jobs + length(reps)
          for (rep_id in reps) {
            add_job_spec(model_name, job_type, selftest_rep = rep_id)
          }
        } else if (job_type == "prof") {
          for (prof_env in selected_profile_1d_envs(model_env)) {
            scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
            if (isTRUE(prof_chain_mode) && (!is_local_mode || isTRUE(allow_local_prof_chain_parallel))) {
              plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
              down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
              up_chain <- plan$upper
              if (length(down_chain) > 0) {
                total_jobs <- total_jobs + 1L
                add_job_spec(
                  model_name = model_name,
                  job_type = "prof_chain",
                  chain_name = "down",
                  chain_scalars = paste(down_chain, collapse = ","),
                  chain_first_init_from = NULL,
                  chain_anchor = as.character(plan$anchor),
                  profile_env = prof_env
                )
              }
              if (length(up_chain) > 0) {
                total_jobs <- total_jobs + 1L
                add_job_spec(
                  model_name = model_name,
                  job_type = "prof_chain",
                  chain_name = "up",
                  chain_scalars = paste(up_chain, collapse = ","),
                  chain_first_init_from = as.character(plan$anchor),
                  chain_anchor = as.character(plan$anchor),
                  profile_env = prof_env
                )
              }
            } else {
              total_jobs <- total_jobs + length(scalars)
              for (sc in scalars) {
                add_job_spec(model_name, job_type, scalar = sc, profile_env = prof_env)
              }
            }
          }
        } else if (job_type == "prof_2d") {
          for (prof_env in selected_profile_2d_envs(model_env)) {
            total_jobs <- total_jobs + 1L
            add_job_spec(model_name, job_type, profile_env = prof_env)
          }
        } else {
          total_jobs <- total_jobs + 1
          add_job_spec(model_name, job_type)
        }
      }
    }
    
    model_env_lists <- lapply(selected_models, function(m) {
      set_prefer_par_start_env(
        as.list(active_model_env(m), all.names = TRUE),
        launch_prefer_par_start()
      )
    })
    names(model_env_lists) <- selected_models

    action_word <- if (is_local_mode) "run" else "launch"
    completion_word <- if (is_local_mode) "completed" else "submitted"
    progress_prefix <- if (is_local_mode) "Running local" else "Launching"
    effective_output_dir <- if (is_local_mode) {
      paste(unique(vapply(selected_models, function(m) {
        env <- active_model_env(m)
        md <- env$model_dir
        if (is.null(md) || !nzchar(md)) m else as.character(md)
      }, character(1))), collapse = ", ")
    } else {
      input$output_dir
    }
    condor_target <- "all"
    condor_target_info <- list(
      exclude_slots = default_condor_exclude_slots(),
      matched_slots = character(0),
      all_slots = character(0),
      target_mode = "all"
    )
    if (identical(launch_mode, "condor")) {
      condor_target <- if (!is.null(input$condor_run_target) && nzchar(input$condor_run_target)) input$condor_run_target else "all"
      condor_target_info <- build_condor_exclude_slots(
        remote_user = input$remote_user,
        remote_host = input$remote_host,
        run_target = condor_target
      )
    }
    condor_exclude_slots <- condor_target_info$exclude_slots

    # Initialize log with total job count
    rv$launch_log <- paste0(
      Sys.time(), " - Starting ", action_word, "...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📊 Total jobs to ", action_word, ": ", total_jobs, "\n",
      "Mode: ", if (identical(launch_mode, "local_docker")) "Local Docker" else if (identical(launch_mode, "local_native")) "Local Native" else "Condor", "\n",
      if (identical(launch_mode, "condor")) {
        paste0(
          "Run target: ", condor_target, "\n",
          "Matched slots: ", length(condor_target_info$matched_slots), "\n",
          "Exclude slots: ", length(condor_exclude_slots), "\n"
        )
      } else {
        ""
      },
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    cancel_launch <- reactiveVal(FALSE)

    update_launch_notification <- function(text, type = "message") {
      showNotification(text, type = type, duration = NULL, id = "launch_progress")
    }

    update_launch_notification(sprintf("%s jobs... (0/%d)", progress_prefix, total_jobs))
    
    tryCatch({
      batch_names <- c()
      remote_dirs <- c()
      launched_job_results <- list()
      current_job <- 0  # Track current job number
      progress_details <- c()  # Store progress messages
      progress_every_n <- 5
      progress_keep <- 50
      
      maybe_send_progress_details <- function(force = FALSE) {
        if (length(progress_details) > progress_keep) {
          progress_details <<- tail(progress_details, progress_keep)
        }
        if (force || current_job %% progress_every_n == 0 || current_job == total_jobs || current_job == 1) {
          session$sendCustomMessage(
            type = "updateProgress",
            message = list(
              id = "launch_progress_details",
              text = paste(progress_details, collapse = "<br/>")
            )
          )
        }
      }

      collect_job_result <- function(result) {
        if (is.null(result)) return(invisible(NULL))
        batch_names <<- c(batch_names, as.character(result$batch_name))
        remote_dirs <<- c(remote_dirs, as.character(result$remote_dir))
        launched_job_results[[length(launched_job_results) + 1]] <<- result
        invisible(NULL)
      }
      
      did_parallel <- FALSE
      run_parallel_submit <- (
        isTRUE(prof_chain_mode) && length(job_specs) >= 1 && (!is_local_mode || isTRUE(allow_local_prof_chain_parallel))
      ) || (
        isTRUE(input$parallel_launch) && length(job_specs) > 1 && !(isTRUE(prof_chain_mode) && is_local_mode && !isTRUE(allow_local_prof_chain_parallel))
      )
      if (isTRUE(run_parallel_submit)) {
        max_cores <- max(1, parallel::detectCores() - 2)
        cores <- max(1, min(as.integer(input$launch_parallel_cores), max_cores))
        if (isTRUE(prof_chain_mode) && !is_local_mode) cores <- max(2L, cores)
        if (cores <= 1) {
          rv$launch_log <- paste0(
            rv$launch_log,
            "⚠️ Parallel launch requested but not used (cores=",
            cores,
            ", jobs=",
            length(job_specs),
            "). Using sequential.\n"
          )
        } else {
          rv$launch_log <- paste0(
            rv$launch_log,
            "⚡ Parallel ",
              if (is_local_mode) "local run" else "launch",
              " ON (cores: ",
              cores,
              ")\n"
          )
          
          update_launch_notification(
            sprintf(
              "%s %d jobs in parallel (cores: %d)",
              if (is_local_mode) "Running" else "Launching",
              total_jobs,
              cores
            )
          )
          
          if (cancel_launch()) stop("Launch cancelled")

          results <- tryCatch({
            cl <- parallel::makeCluster(cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)

            if (identical(launch_mode, "condor")) {
              common_params <- list(
                repo_root = isolate(repo_root_val()),
                remote_user = input$remote_user,
                remote_host = input$remote_host,
                github_pat = Sys.getenv("GIT_PAT"),
                github_username = input$github_username,
                github_org = input$github_org,
                github_repo = input$github_repo,
                docker_image = input$docker_image,
                condor_cpus = as.integer(input$condor_cpus),
                condor_memory = paste0(input$condor_memory, "GB"),
                condor_disk = paste0(input$condor_disk, "GB"),
                branch = input$branch,
                ghcr_login = isTRUE(input$ghcr_login),
                output_dir = input$output_dir,
                model_env_lists = model_env_lists,
                prefer_par_start = launch_prefer_par_start(),
                exclude_slots = condor_exclude_slots
              )
              parallel::clusterEvalQ(cl, { library(CondorBox) })
              parallel::clusterExport(
                cl,
                varlist = c(
                  "launch_single_job_raw",
                  "submit_condorbox_job",
                  "condorbox_with_extra_transfer",
                  "prepare_job_fitted_source",
                  "job_uses_fitted_source",
                  "fitted_source_job_types",
                  "ensure_fitted_source_functions_loaded",
                  "truthy_scalar",
                  "first_scalar_string",
                  "set_prefer_par_start_env",
                  "apply_selftest_rep_to_env",
                  "launch_model_name",
                  "sanitize_profile_job_tag",
                  "apply_prof_init_mapping",
                  "common_params"
                ),
                envir = environment()
              )
              parallel::parLapply(cl, job_specs, function(spec) {
                tryCatch({
                  launch_single_job_raw(spec, common_params)
                }, error = function(e) {
                  list(batch_name = NA_character_, remote_dir = NA_character_, job_id = NA_character_, error = e$message, spec = spec)
                })
              })
            } else {
              common_params <- list(
                repo_root = isolate(repo_root_val()),
                local_use_docker = is_local_docker,
                local_async = TRUE,
                docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
                prefer_par_start = launch_prefer_par_start(),
                model_env_lists = model_env_lists
              )
              parallel::clusterExport(
                cl,
                varlist = c(
                  "local_env_strings",
                  "local_job_runner",
                  "local_run_command",
                  "local_command_preview",
                  "launch_single_job_local_raw",
                  "set_prefer_par_start_env",
                  "apply_selftest_rep_to_env",
                  "first_scalar_string",
                  "launch_model_name",
                  "sanitize_profile_job_tag",
                  "apply_prof_init_mapping",
                  "common_params"
                ),
                envir = environment()
              )
              parallel::parLapply(cl, job_specs, function(spec) {
                tryCatch({
                  launch_single_job_local_raw(spec, common_params)
                }, error = function(e) {
                  list(batch_name = NA_character_, remote_dir = NA_character_, job_id = NA_character_, error = e$message, spec = spec)
                })
              })
            }
          }, error = function(e) {
            rv$launch_log <- paste0(
              rv$launch_log,
              "⚠️ Parallel ",
              if (is_local_mode) "local run" else "launch",
              " failed: ",
              e$message,
              "\n"
            )
            NULL
          })
          
          if (!is.null(results)) {
            ok_mask <- vapply(results, function(x) is.null(x$error), logical(1))
            results_ok <- results[ok_mask]
            if (length(results_ok) > 0) {
              for (res in results_ok) collect_job_result(res)
            }
            current_job <- length(results_ok)
            did_parallel <- TRUE
            
            if (!all(ok_mask)) {
              err_msgs <- unique(vapply(results[!ok_mask], function(x) x$error, character(1)))
              rv$launch_log <- paste0(
                rv$launch_log,
                "⚠️ Parallel launch errors:\n",
                paste0("  - ", err_msgs, collapse = "\n"),
                "\n"
              )
            }
          }
        }
      }
      if (!did_parallel) {
        for (model_name in selected_models) {
          if (cancel_launch()) stop("Launch cancelled")
          model_env <- active_model_env(model_name)
          
          for (job_type in selected_job_types) {
            if (cancel_launch()) stop("Launch cancelled")
            
            if (job_type == "jitter") {
              seeds <- parse_numeric_tokens(model_env$jitter_seeds)
              for (seed in seeds) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (seed %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, seed)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (seed %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, seed)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (seed %d)", 
                          current_job, total_jobs, model_name, seed)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, seed = seed, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "hessian") {
              nsplit <- suppressWarnings(as.integer(model_env$nsplit))
              if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
              for (part in seq_len(nsplit)) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (part %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, part)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (part %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, part)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (part %d)", 
                          current_job, total_jobs, model_name, part)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, part = part, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "retro") {
              peels <- parse_numeric_tokens(model_env$retro_peels)
              for (peel in peels) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (peel %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, peel)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (peel %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, peel)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (peel %d)", 
                          current_job, total_jobs, model_name, peel)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, peel = peel, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "selftest") {
              reps <- selftest_reps_for_launch()
              for (rep_id in reps) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1

                update_launch_notification(
                  sprintf("%s job %d/%d: %s (self-test rep %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, rep_id)
                )

                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (self-test rep %d)\n",
                          current_job, total_jobs, progress_prefix, model_name, rep_id)
                )

                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (self-test rep %d)",
                          current_job, total_jobs, model_name, rep_id)
                )

                result <- launch_single_job(model_name, model_env, job_type = job_type, selftest_rep = rep_id, exclude_slots = condor_exclude_slots)
                collect_job_result(result)

                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()

                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "prof") {
              for (prof_env in selected_profile_1d_envs(model_env)) {
                profile_name <- profile_launch_name(model_name, prof_env)
                scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
                if (isTRUE(prof_chain_mode) && is_local_mode && length(scalars) > 0) {
                  chain_plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
                  anchor_sc <- chain_plan$anchor

                  rv$launch_log <- paste0(
                    rv$launch_log,
                    sprintf(
                      "🔗 Profile sequential strategy [%s]: anchor=%g (requested=%g), lower=%s, upper=%s\n",
                      profile_name,
                      anchor_sc,
                      prof_anchor_requested,
                      if (length(chain_plan$lower) > 0) paste(chain_plan$lower, collapse = " ") else "<none>",
                      if (length(chain_plan$upper) > 0) paste(chain_plan$upper, collapse = " ") else "<none>"
                    )
                  )

                  run_prof_local_one <- function(sc, donor = NA_real_) {
                    common_params_prof <- list(
                      repo_root = isolate(repo_root_val()),
                      local_use_docker = is_local_docker,
                      local_async = FALSE,
                      docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
                      model_env_lists = setNames(list(as.list(prof_env, all.names = TRUE)), model_name)
                    )
                    launch_single_job_local_raw(
                      spec = list(
                        model_name = model_name,
                        job_type = "prof",
                        scalar = sc,
                        init_from_scalar_override = if (is.finite(donor)) donor else NULL,
                        profile_env = as.list(prof_env, all.names = TRUE)
                      ),
                      common_params = common_params_prof
                    )
                  }

                  run_chain_local <- function(chain_scalars, start_donor) {
                    out <- list()
                    donor <- start_donor
                    for (sc in chain_scalars) {
                      res <- run_prof_local_one(sc = sc, donor = donor)
                      res$chain <- if (sc < anchor_sc) "down" else "up"
                      res$donor_scalar <- donor
                      out[[length(out) + 1L]] <- res
                      donor <- sc
                    }
                    out
                  }

                  if (cancel_launch()) stop("Launch cancelled")
                  current_job <- current_job + 1
                  update_launch_notification(sprintf("%s job %d/%d: %s (anchor scalar %g)", progress_prefix, current_job, total_jobs, profile_name, anchor_sc))
                  progress_details <- c(progress_details, sprintf("[%d/%d] 🔄 %s (anchor scalar %g)", current_job, total_jobs, profile_name, anchor_sc))
                  result_anchor <- run_prof_local_one(sc = anchor_sc, donor = NA_real_)
                  collect_job_result(result_anchor)
                  progress_details[length(progress_details)] <- paste0(progress_details[length(progress_details)], " ✓")
                  maybe_send_progress_details()
                  rv$launch_log <- paste0(rv$launch_log, sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result_anchor$batch_name))

                  chain_results <- list()
                  run_two_chains_parallel <- isTRUE(input$parallel_launch) &&
                    length(chain_plan$lower) > 0 &&
                    length(chain_plan$upper) > 0 &&
                    .Platform$OS.type != "windows"

                  if (run_two_chains_parallel) {
                    rv$launch_log <- paste0(rv$launch_log, "⚡ Running lower/upper profile chains in parallel (2 forks)\n")
                    parts <- parallel::mclapply(
                      list(
                        list(scalars = chain_plan$lower, donor = anchor_sc),
                        list(scalars = chain_plan$upper, donor = anchor_sc)
                      ),
                      function(ch) run_chain_local(ch$scalars, ch$donor),
                      mc.cores = 2
                    )
                    chain_results <- unlist(parts, recursive = FALSE)
                  } else {
                    chain_results <- c(
                      run_chain_local(chain_plan$lower, anchor_sc),
                      run_chain_local(chain_plan$upper, anchor_sc)
                    )
                  }

                  for (res in chain_results) {
                    if (cancel_launch()) stop("Launch cancelled")
                    current_job <- current_job + 1
                    progress_details <- c(
                      progress_details,
                      sprintf("[%d/%d] 🔄 %s (scalar %s, donor %s)",
                              current_job, total_jobs, profile_name,
                              if (!is.null(res$job_env$scalar)) as.character(res$job_env$scalar) else "?",
                              if (!is.null(res$donor_scalar) && is.finite(res$donor_scalar)) as.character(res$donor_scalar) else "<none>")
                    )
                    collect_job_result(res)
                    progress_details[length(progress_details)] <- paste0(progress_details[length(progress_details)], " ✓")
                    maybe_send_progress_details()
                    rv$launch_log <- paste0(rv$launch_log, sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), res$batch_name))
                  }
                } else {
                  for (sc in scalars) {
                    if (cancel_launch()) stop("Launch cancelled")
                    current_job <- current_job + 1

                    update_launch_notification(
                      sprintf("%s job %d/%d: %s (scalar %g)",
                              progress_prefix,
                              current_job, total_jobs, profile_name, sc)
                    )

                    rv$launch_log <- paste0(
                      rv$launch_log,
                      sprintf("[%d/%d] 🔄 %s: %s (scalar %g)\n",
                              current_job, total_jobs, progress_prefix, profile_name, sc)
                    )

                    progress_details <- c(
                      progress_details,
                      sprintf("[%d/%d] 🔄 %s (scalar %g)",
                              current_job, total_jobs, profile_name, sc)
                    )

                    result <- launch_single_job(model_name, prof_env, job_type = job_type, scalar = sc, exclude_slots = condor_exclude_slots)
                    collect_job_result(result)

                    progress_details[length(progress_details)] <- paste0(
                      progress_details[length(progress_details)], " ✓"
                    )
                    maybe_send_progress_details()

                    rv$launch_log <- paste0(
                      rv$launch_log,
                      sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                    )
                  }
                }
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "prof_2d") {
              for (prof_env in selected_profile_2d_envs(model_env)) {
                profile_name <- profile_launch_name(model_name, prof_env)
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1

                update_launch_notification(
                  sprintf("%s job %d/%d: %s (2D profile)",
                          progress_prefix,
                          current_job, total_jobs, profile_name)
                )

                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (2D profile)\n",
                          current_job, total_jobs, progress_prefix, profile_name)
                )

                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (2D profile)", current_job, total_jobs, profile_name)
                )

                result <- launch_single_job(model_name, prof_env, job_type = job_type, exclude_slots = condor_exclude_slots)
                collect_job_result(result)

                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()

                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else {
              if (cancel_launch()) stop("Launch cancelled")
              current_job <- current_job + 1
              
              update_launch_notification(
                sprintf("%s job %d/%d: %s",
                        progress_prefix,
                        current_job, total_jobs, model_name)
              )
              
              rv$launch_log <- paste0(
                rv$launch_log,
                sprintf("[%d/%d] 🔄 %s: %s\n", 
                        current_job, total_jobs, progress_prefix, model_name)
              )
              
              progress_details <- c(
                progress_details,
                sprintf("[%d/%d] 🔄 %s", 
                        current_job, total_jobs, model_name)
              )
              
              result <- launch_single_job(model_name, model_env, job_type = job_type, exclude_slots = condor_exclude_slots)
              collect_job_result(result)
              
              progress_details[length(progress_details)] <- paste0(
                progress_details[length(progress_details)], " ✓"
              )
              maybe_send_progress_details()
              
              rv$launch_log <- paste0(
                rv$launch_log,
                sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
              )
            }
          }
        }
      }
      
      # Save launch records
      launch_status <- if (cancel_launch()) {
        "cancelled"
      } else if (is_local_mode && !isTRUE(prof_chain_mode)) {
        "launched_local_async"
      } else if (is_local_mode) {
        "completed_local"
      } else {
        "launched"
      }
      job_record <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        job_type = paste(selected_job_types, collapse = ", "),
        model_names = paste(launch_unit_labels(selected_models), collapse = ", "),
        output_dir = effective_output_dir,
        batch_names = paste(batch_names, collapse = ", "),
        remote_dirs = paste(remote_dirs, collapse = ", "),
        branch = input$branch,
        status = launch_status,
        launch_mode = launch_mode,
        stringsAsFactors = FALSE
      )

      if (!is.null(rv$current_config_file)) {
        save_job_history(rv$current_config_file, job_record)
        rv$launch_log <- paste0(rv$launch_log, "📝 Job history saved to config file\n")
      }

      launcher_log_record <- data.frame(
        run_at = job_record$timestamp,
        output_dir = as.character(effective_output_dir),
        summary = if (!is.null(rv$run_metadata$summary) && nzchar(as.character(rv$run_metadata$summary))) {
          as.character(rv$run_metadata$summary)
        } else {
          "NA"
        },
        run_description = if (!is.null(input$run_description) && nzchar(trimws(as.character(input$run_description)))) {
          as.character(input$run_description)
        } else {
          "NA"
        },
        config_file = if (!is.null(rv$config_path) && nzchar(as.character(rv$config_path))) {
          as.character(rv$config_path)
        } else {
          "NA"
        },
        job_types = as.character(job_record$job_type),
        model_names = as.character(job_record$model_names),
        total_jobs = as.integer(total_jobs),
        launch_mode = as.character(job_record$launch_mode),
        selected_condor_nodes = format_selected_condor_nodes(
          launch_mode = launch_mode,
          condor_target_info = condor_target_info
        ),
        status = as.character(job_record$status),
        branch = as.character(job_record$branch),
        batch_names = as.character(job_record$batch_names),
        local_pid_files = if (length(launched_job_results) > 0) {
          paste(
            vapply(launched_job_results, function(x) {
              if (!is.null(x$pid_file) && nzchar(as.character(x$pid_file))) as.character(x$pid_file) else ""
            }, character(1)),
            collapse = ", "
          )
        } else {
          ""
        },
        local_log_files = if (length(launched_job_results) > 0) {
          paste(
            vapply(launched_job_results, function(x) {
              if (!is.null(x$log_file) && nzchar(as.character(x$log_file))) as.character(x$log_file) else ""
            }, character(1)),
            collapse = ", "
          )
        } else {
          ""
        },
        config_details = if (length(launched_job_results) > 0) {
          build_launched_config_details(launched_job_results)
        } else {
          build_model_config_details(selected_models)
        },
        remote_dirs = as.character(job_record$remote_dirs),
        stringsAsFactors = FALSE
      )
      save_launcher_job_log(launcher_log_record)
      rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1
      rv$launch_log <- paste0(rv$launch_log, "📝 Launcher job log updated\n")
      
      if (cancel_launch()) {
        removeNotification("launch_progress")
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "⚠️ ", Sys.time(), " - ", tools::toTitleCase(action_word), " cancelled by user after ", current_job, " job(s)\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #f39c12;",
            icon("ban"), " Launch Cancelled"
          ),
          size = "m",
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #f39c12;",
              sprintf("⚠️ Cancelled after %d job(s).", current_job)
            )
          ),
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-warning")
          )
        ))
      } else {
        removeNotification("launch_progress")
        # Final completion message
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "✅ ", Sys.time(), " - All ", total_jobs, " jobs ", completion_word, " successfully!\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        # Update modal to show completion
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #00a65a;",
            icon("check-circle"), if (is_local_mode) " Local Run Complete" else " Launch Complete"
          ),
          size = "m",
          
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #00a65a;",
              sprintf("✅ Successfully %s all %d jobs!", completion_word, total_jobs)
            )
          ),
          
          div(
            style = "background: #f0f9f0; border: 1px solid #c3e6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
            strong("Summary:"),
            tags$ul(
              tags$li(paste("Total jobs:", total_jobs)),
              tags$li(paste("Inputs:", paste(launch_unit_labels(selected_models), collapse = ", "))),
              tags$li(paste("Mode:", if (identical(launch_mode, "local_docker")) "Local Docker" else if (identical(launch_mode, "local_native")) "Local Native" else "Condor")),
              tags$li(paste(if (is_local_mode) "Model dir:" else "Output directory:", effective_output_dir)),
              tags$li(paste("Branch:", input$branch))
            )
          ),
          
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-success")
          )
        ))
      }
      
    }, error = function(e) {
      removeNotification("launch_progress")
      if (grepl("Launch cancelled", e$message)) {
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "⚠️ ", Sys.time(), " - ", tools::toTitleCase(action_word), " cancelled by user after ", current_job, " job(s)\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #f39c12;",
            icon("ban"), " Launch Cancelled"
          ),
          size = "m",
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #f39c12;",
              sprintf("⚠️ Cancelled after %d job(s).", current_job)
            )
          ),
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-warning")
          )
        ))
      } else {
        rv$launch_log <- paste0(rv$launch_log, "\n❌ ERROR: ", e$message, "\n")
        
        # Show error modal
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #dd4b39;",
            icon("times-circle"), " Launch Failed"
          ),
          size = "m",
          
          div(
            style = "background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
            h4(style = "color: #721c24;", "Error occurred during job launch:"),
            p(style = "font-family: monospace; color: #721c24;", e$message)
          ),
          
          footer = actionButton("close_launch_error_modal", "Close", class = "btn-danger")
        ))
      }
      
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("launch_btn")
      shinyjs::removeClass("launch_btn", "loading")
    })
  })
  
  # Handler to close completion modal
  observeEvent(input$close_launch_modal, {
    removeModal()
  })
  
  # Handler to close error modal
  observeEvent(input$close_error_modal, {
    removeModal()
  })
  
  
  
  launch_single_job_raw <- function(spec, common_params) {
    model_env_list <- if (!is.null(spec$profile_env) && is.list(spec$profile_env)) spec$profile_env else common_params$model_env_lists[[spec$model_name]]
    if (is.null(model_env_list)) {
      stop(paste("Model env not found for", spec$model_name))
    }
    job_env <- list2env(model_env_list, parent = emptyenv())
    job_env$prefer_par_start <- if (isTRUE(common_params$prefer_par_start)) "1" else "0"
    launch_name <- launch_model_name(spec$model_name, as.list(job_env, all.names = TRUE))
    # Profile sets are launch-time metadata, not runtime env vars.
    # Remove to avoid passing large/non-scalar values to CondorBox.
    if (exists("profile_sets", envir = job_env, inherits = FALSE)) {
      rm(profile_sets, envir = job_env)
    }
    job_env$DOCKER_IMAGE <- common_params$docker_image
    remote_dir_suffix <- launch_name
    batch_suffix <- ""
    profile_tag <- if (identical(spec$job_type, "prof") || identical(spec$job_type, "prof_chain") || identical(spec$job_type, "prof_2d")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("_", profile_tag) else ""
    profile_batch_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""
    
    if (!is.null(spec$seed)) {
      job_env$jitter_seed <- as.character(spec$seed)
      remote_dir_suffix <- paste0(launch_name, "_seed", spec$seed)
      batch_suffix <- paste0("-jitter", spec$seed)
    } else if (!is.null(spec$part)) {
      job_env$hessian_part <- as.character(spec$part)
      remote_dir_suffix <- paste0(launch_name, "_part", spec$part)
      batch_suffix <- paste0("-hess", spec$part)
    } else if (!is.null(spec$peel)) {
      job_env$retro_peel <- as.character(spec$peel)
      remote_dir_suffix <- paste0(launch_name, "_peel", spec$peel)
      batch_suffix <- paste0("-retro", spec$peel)
    } else if (!is.null(spec$selftest_rep)) {
      job_env <- list2env(
        apply_selftest_rep_to_env(as.list(job_env, all.names = TRUE), spec$selftest_rep),
        parent = emptyenv()
      )
      remote_dir_suffix <- sprintf("%s_selftest_rep%03d", launch_name, as.integer(spec$selftest_rep))
      batch_suffix <- sprintf("-selftest-rep%03d", as.integer(spec$selftest_rep))
    } else if (!is.null(spec$scalar)) {
      job_env$scalar <- as.character(spec$scalar)
      if (identical(spec$job_type, "prof")) {
        mapped_env <- apply_prof_init_mapping(as.list(job_env, all.names = TRUE), spec$scalar)
        job_env <- list2env(mapped_env, parent = emptyenv())
      }
      remote_dir_suffix <- paste0(launch_name, profile_suffix, "_sc", spec$scalar)
      batch_suffix <- paste0(profile_batch_suffix, "-sc", spec$scalar)
    } else if (identical(spec$job_type, "prof_chain")) {
      if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) {
        job_env$chain_name <- as.character(spec$chain_name)
      }
        if (!is.null(spec$chain_scalars) && nzchar(as.character(spec$chain_scalars))) {
          chain_vals <- parse_numeric_tokens(spec$chain_scalars)
          job_env$chain_scalars <- as.character(spec$chain_scalars)
          if (length(chain_vals) > 0) {
            job_env$chain_count <- as.character(length(chain_vals))
            job_env$CHAIN_COUNT <- as.character(length(chain_vals))
            for (ii in seq_along(chain_vals)) {
              v <- format(chain_vals[[ii]], scientific = FALSE, trim = TRUE)
              job_env[[paste0("chain_scalar_", ii)]] <- v
              job_env[[paste0("CHAIN_SCALAR_", ii)]] <- v
            }
          }
        }
      if (!is.null(spec$chain_first_init_from) && nzchar(as.character(spec$chain_first_init_from))) {
        job_env$chain_first_init_from <- as.character(spec$chain_first_init_from)
      }
      if (!is.null(spec$chain_anchor) && nzchar(as.character(spec$chain_anchor))) {
        job_env$chain_anchor <- as.character(spec$chain_anchor)
      }
      remote_dir_suffix <- paste0(launch_name, profile_suffix, "_profchain_", if (!is.null(spec$chain_name)) as.character(spec$chain_name) else "chain")
      batch_suffix <- paste0(profile_batch_suffix, "-profchain", if (!is.null(spec$chain_name)) paste0("-", as.character(spec$chain_name)) else "")
    } else if (identical(spec$job_type, "prof_2d")) {
      remote_dir_suffix <- paste0(launch_name, profile_suffix, "_prof2d")
      batch_suffix <- paste0(profile_batch_suffix, "-prof2d")
    } else if (identical(spec$job_type, "stage_check")) {
      remote_dir_suffix <- paste0(launch_name, "_stagecheck")
      batch_suffix <- "-stagecheck"
    } else {
      # model job only
      remote_dir_suffix <- paste0(launch_name, "_model")
    }
    
    remote_dir <- paste0(common_params$github_repo, "/", common_params$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(
      launch_name,
      batch_suffix,
      "-",
      format(Sys.time(), "%H:%M:%S"),
      "-",
      Sys.getpid()
    )
    
    work_dir <- file.path(
      tempdir(),
      paste0("condorbox_", Sys.getpid(), "_", gsub("[^a-zA-Z0-9]", "_", launch_name))
    )
    if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)
    old_wd <- getwd()
    setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)

    fitted_source <- prepare_job_fitted_source(
      job_env = as.list(job_env, all.names = TRUE),
      job_type = spec$job_type,
      work_dir = work_dir,
      repo_root = common_params$repo_root
    )
    job_env <- list2env(fitted_source$job_env, parent = emptyenv())
    
    job_id <- submit_condorbox_job(
      make_options = spec$job_type, 
      remote_user = common_params$remote_user, 
      remote_host = common_params$remote_host,
      remote_dir = remote_dir, 
      github_pat = common_params$github_pat, 
      github_username = common_params$github_username,
      github_org = common_params$github_org, 
      github_repo = common_params$github_repo, 
      docker_image = common_params$docker_image,
      condor_cpus = common_params$condor_cpus,
      condor_memory = common_params$condor_memory,
      condor_disk = common_params$condor_disk,
      stream_error = "TRUE", 
      branch = common_params$branch, 
      rmclone_script = "no", 
      ghcr_login = common_params$ghcr_login,
      exclude_slots = common_params$exclude_slots,
      custom_batch_name = batch_name, 
      condor_environment = as.list(job_env, all.names = TRUE),
      extra_transfer_files = fitted_source$extra_transfer_files
    )
    
    return(list(
      batch_name = batch_name,
      remote_dir = remote_dir,
      job_id = job_id,
      model_name = launch_name,
      job_type = spec$job_type,
      job_env = as.list(job_env, all.names = TRUE)
    ))
  }
  
  launch_single_job <- function(model_name, model_env, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL, selftest_rep = NULL, log = TRUE, exclude_slots = NULL) {
    if (input$launch_mode %in% c("local_native", "local_docker")) {
      return(
        launch_single_job_local(
          model_name = model_name,
          model_env = model_env,
          job_type = job_type,
          seed = seed,
          part = part,
          peel = peel,
          scalar = scalar,
          selftest_rep = selftest_rep,
          log = log
        )
      )
    }

    job_env <- set_prefer_par_start_env(model_env, launch_prefer_par_start())
    launch_name <- launch_model_name(model_name, job_env)
    # Profile sets are launch-time metadata only; Condor env vars must be scalar/text.
    if (!is.null(job_env$profile_sets)) {
      job_env$profile_sets <- NULL
    }
    job_env$DOCKER_IMAGE <- input$docker_image
    remote_dir_suffix <- launch_name
    batch_suffix <- ""
    profile_tag <- if (identical(job_type, "prof") || identical(job_type, "prof_chain") || identical(job_type, "prof_2d")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("_", profile_tag) else ""
    profile_batch_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""
    
    if (!is.null(seed)) {
      job_env$jitter_seed <- as.character(seed)
      remote_dir_suffix <- paste0(launch_name, "_seed", seed)
      batch_suffix <- paste0("-jitter", seed)
    } else if (!is.null(part)) {
      job_env$hessian_part <- as.character(part)
      remote_dir_suffix <- paste0(launch_name, "_part", part)
      batch_suffix <- paste0("-hess", part)
    } else if (!is.null(peel)) {
      job_env$retro_peel <- as.character(peel)
      remote_dir_suffix <- paste0(launch_name, "_peel", peel)
      batch_suffix <- paste0("-retro", peel)
    } else if (!is.null(selftest_rep)) {
      job_env <- apply_selftest_rep_to_env(job_env, selftest_rep)
      remote_dir_suffix <- sprintf("%s_selftest_rep%03d", launch_name, as.integer(selftest_rep))
      batch_suffix <- sprintf("-selftest-rep%03d", as.integer(selftest_rep))
    } else if (!is.null(scalar)) {
      job_env$scalar <- as.character(scalar)
      if (identical(job_type, "prof")) {
        job_env <- apply_prof_init_mapping(job_env, scalar)
      }
      remote_dir_suffix <- paste0(launch_name, profile_suffix, "_sc", scalar)
      batch_suffix <- paste0(profile_batch_suffix, "-sc", scalar)
    } else if (identical(job_type, "prof_2d")) {
      remote_dir_suffix <- paste0(launch_name, profile_suffix, "_prof2d")
      batch_suffix <- paste0(profile_batch_suffix, "-prof2d")
    } else if (identical(job_type, "stage_check")) {
      remote_dir_suffix <- paste0(launch_name, "_stagecheck")
      batch_suffix <- "-stagecheck"
    } else {
      # model job only
      remote_dir_suffix <- paste0(launch_name, "_model")
    }
    
    remote_dir <- paste0(input$github_repo, "/", input$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(launch_name, batch_suffix, "-", format(Sys.time(), "%H:%M:%S"))
    
    if (isTRUE(log)) {
      rv$launch_log <- paste0(rv$launch_log, "  → ", batch_name, "\n")
    }
    
    work_dir <- file.path(
      tempdir(),
      paste0("condorbox_", Sys.getpid(), "_", gsub("[^a-zA-Z0-9]", "_", launch_name))
    )
    if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)
    old_wd <- getwd()
    setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)

    fitted_source <- prepare_job_fitted_source(
      job_env = job_env,
      job_type = job_type,
      work_dir = work_dir,
      repo_root = isolate(repo_root_val())
    )
    job_env <- fitted_source$job_env

    job_id <- submit_condorbox_job(
      make_options = job_type, 
      remote_user = input$remote_user, 
      remote_host = input$remote_host,
      remote_dir = remote_dir, 
      github_pat = Sys.getenv("GIT_PAT"), 
      github_username = input$github_username,
      github_org = input$github_org, 
      github_repo = input$github_repo, 
      docker_image = input$docker_image,
      condor_cpus = as.integer(input$condor_cpus),
      condor_memory = paste0(input$condor_memory, "GB"),
      condor_disk = paste0(input$condor_disk, "GB"),
      stream_error = "TRUE", 
      branch = input$branch, 
      rmclone_script = "no", 
      ghcr_login = isTRUE(input$ghcr_login),
      exclude_slots = if (!is.null(exclude_slots)) exclude_slots else default_condor_exclude_slots(),
      custom_batch_name = batch_name, 
      condor_environment = as.list(job_env, all.names = TRUE),
      extra_transfer_files = fitted_source$extra_transfer_files
    )
    
    return(list(
      batch_name = batch_name,
      remote_dir = remote_dir,
      job_id = job_id,
      model_name = launch_name,
      job_type = job_type,
      job_env = as.list(job_env, all.names = TRUE)
    ))
  }
  
