  # ========== LAUNCH INPUT PREFLIGHT ==========

  launch_preflight_empty <- function(message = "Select input folder(s) and job type(s).") {
    data.frame(
      Model = "NA",
      JobType = "NA",
      Status = "info",
      Required = "NA",
      Found = "NA",
      Detail = message,
      stringsAsFactors = FALSE
    )
  }

  launch_preflight_first <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    x <- as.character(x[[1]])
    if (is.na(x) || !nzchar(trimws(x))) return(default)
    trimws(x)
  }

  launch_preflight_truthy <- function(x, default = FALSE) {
    txt <- launch_preflight_first(x, default = if (isTRUE(default)) "1" else "")
    if (!nzchar(txt)) return(default)
    tolower(txt) %in% c("1", "true", "yes", "y", "on")
  }

  launch_preflight_recipe_ready <- function(model_env) {
    launch_preflight_truthy(model_env$build_inputs_on_missing) &&
      launch_preflight_truthy(model_env$input_recipe_enabled)
  }

  launch_preflight_recipe_detail <- function(model_env) {
    paste(
      c(
        paste0("base_input=", launch_preflight_first(model_env$input_recipe_base_input_dir, launch_preflight_first(model_env$base_dir, "<none>"))),
        paste0("movement=", launch_preflight_first(model_env$input_recipe_movement_pairs, "<none>")),
        paste0("sel_nodes=", launch_preflight_first(model_env$input_recipe_sel_nodes, "<none>")),
        paste0("index_cv_half=", launch_preflight_first(model_env$input_recipe_index_cv_half, "0"))
      ),
      collapse = "; "
    )
  }

  launch_preflight_recipe_source <- function(model_env, model_needs_par = FALSE) {
    source_dir <- launch_preflight_first(
      model_env$input_recipe_base_input_dir,
      launch_preflight_first(model_env$input_recipe_base_source, "")
    )
    source_abs <- if (nzchar(source_dir)) resolve_repo_path(source_dir) else ""
    source_exists <- nzchar(source_abs) && dir.exists(source_abs)
    source_frq_n <- length(launch_preflight_files(source_abs, "\\.frq$"))
    source_ini_n <- length(launch_preflight_files(source_abs, "\\.ini$"))
    source_par <- launch_preflight_latest_par(source_abs)
    source_par_exists <- !is.na(source_par) && file.exists(source_par)
    source_doitall <- file.exists(file.path(source_abs, "doitall.sh"))
    source_model_ready <- source_exists &&
      source_frq_n > 0 &&
      (source_par_exists || (source_ini_n > 0 && source_doitall && !isTRUE(model_needs_par)))
    list(
      dir = source_dir,
      exists = source_exists,
      frq_n = source_frq_n,
      ini_n = source_ini_n,
      par = source_par,
      par_exists = source_par_exists,
      doitall = source_doitall,
      model_ready = source_model_ready,
      found = paste(
        c(
          paste0("recipe source=", if (source_exists) source_dir else paste0("missing ", source_dir)),
          paste0("source par=", if (source_par_exists) basename(source_par) else "missing"),
          paste0("source frq=", source_frq_n),
          paste0("source ini=", source_ini_n),
          paste0("source doitall=", as.integer(source_doitall))
        ),
        collapse = "; "
      )
    )
  }

  launch_preflight_dependent_job_needs_par <- function(job_type) {
    job_type %in% c("jitter", "hessian", "prof", "prof_chain", "prof_2d")
  }

  launch_preflight_dependent_job_needs_indepvar <- function(job_type, model_env) {
    if (identical(job_type, "jitter")) return(TRUE)
    if (identical(job_type, "prof") || identical(job_type, "prof_chain")) {
      profile_components <- if (exists("selected_profile_components", mode = "function")) {
        intersect(selected_profile_components(), c("standard", "individual"))
      } else {
        c("standard", "individual")
      }
      prof_envs <- launch_preflight_profile_envs(model_env, profile_components)
      return(any(vapply(prof_envs, function(env) nzchar(launch_preflight_first(env$prof_fix_indepvar)), logical(1))))
    }
    if (identical(job_type, "prof_2d")) {
      prof_envs <- launch_preflight_profile_envs(model_env, "prof_2d")
      return(any(vapply(prof_envs, function(env) {
        nzchar(launch_preflight_first(env$prof_2d_scalars_x)) ||
          nzchar(launch_preflight_first(env$prof_2d_scalars_y))
      }, logical(1))))
    }
    FALSE
  }

  launch_preflight_files <- function(dir_path, pattern) {
    if (!is.character(dir_path) || length(dir_path) != 1 || !nzchar(dir_path) || !dir.exists(dir_path)) return(character(0))
    list.files(dir_path, pattern = pattern, full.names = TRUE)
  }

  launch_preflight_latest_par <- function(dir_path) {
    pars <- launch_preflight_files(dir_path, "\\.par$")
    if (length(pars) == 0) return(NA_character_)
    base_names <- basename(pars)
    stems <- sub("\\.par$", "", base_names)
    nums <- suppressWarnings(as.integer(stems))
    info <- file.info(pars)
    ord <- order(
      -ifelse(is.finite(nums), nums, -1L),
      -ifelse(is.finite(as.numeric(info$mtime)), as.numeric(info$mtime), -Inf),
      base_names
    )
    pars[ord][[1]]
  }

  launch_preflight_has_indepvar <- function(model_dir_abs, base_dir_abs) {
    paths <- c(file.path(model_dir_abs, "indepvar.rpt"), file.path(base_dir_abs, "indepvar.rpt"))
    paths[file.exists(paths)]
  }

  launch_preflight_indepvar_source_label <- function(base_indepvar, fitted_indepvar_exists, auto_model_ready) {
    sources <- character(0)
    if (length(base_indepvar) > 0) sources <- c(sources, "base/model")
    if (isTRUE(fitted_indepvar_exists)) sources <- c(sources, "fitted payload")
    if (isTRUE(auto_model_ready)) sources <- c(sources, "auto model first")
    if (length(sources) == 0) "missing" else paste(unique(sources), collapse = " + ")
  }

  launch_preflight_profile_envs <- function(model_env, components = NULL) {
    if (!is.null(components) && exists("selected_profile_envs", mode = "function")) {
      return(selected_profile_envs(model_env, components))
    }
    if (exists("resolve_profile_job_envs", mode = "function")) {
      return(resolve_profile_job_envs(model_env))
    }
    list(model_env)
  }

  launch_preflight_has_prof_2d <- function(prof_env) {
    if (exists("has_prof_2d_spec", mode = "function")) {
      return(isTRUE(has_prof_2d_spec(prof_env)))
    }
    indepvar_txt <- launch_preflight_first(prof_env$prof_2d_indepvar)
    x_scalars <- launch_preflight_first(prof_env$prof_2d_scalars_x)
    y_scalars <- launch_preflight_first(prof_env$prof_2d_scalars_y)
    x_values <- launch_preflight_first(prof_env$prof_2d_values_x)
    y_values <- launch_preflight_first(prof_env$prof_2d_values_y)
    nzchar(indepvar_txt) && ((nzchar(x_scalars) && nzchar(y_scalars)) || (nzchar(x_values) && nzchar(y_values)))
  }

  launch_preflight_row <- function(model, job_type, status, required, found, detail) {
    data.frame(
      Model = model,
      JobType = job_type,
      Status = status,
      Required = required,
      Found = found,
      Detail = detail,
      stringsAsFactors = FALSE
    )
  }

  launch_preflight_display_name <- function(model_name, model_env = NULL) {
    if (exists("launch_unit_label", mode = "function")) {
      label <- tryCatch(launch_unit_label(model_name), error = function(e) "")
      if (nzchar(label)) return(label)
    }
    if (exists("launch_model_name", mode = "function")) {
      label <- tryCatch(launch_model_name(model_name, model_env), error = function(e) "")
      if (nzchar(label)) return(label)
    }
    as.character(model_name[[1]])
  }

  launch_preflight_status <- function(ok, warn = FALSE) {
    if (isTRUE(ok) && isTRUE(warn)) "warning" else if (isTRUE(ok)) "ready" else "blocked"
  }

  launch_preflight_check_one <- function(model_name, model_env, job_type) {
    display_name <- launch_preflight_display_name(model_name, model_env)
    base_dir <- launch_preflight_first(model_env$base_dir)
    model_dir <- launch_preflight_first(model_env$model_dir, default = file.path("model", model_name))
    base_dir_abs <- resolve_repo_path(base_dir)
    model_dir_abs <- resolve_repo_path(model_dir)

    base_exists <- dir.exists(base_dir_abs)
    model_exists <- dir.exists(model_dir_abs)
    base_par <- launch_preflight_latest_par(base_dir_abs)
    model_par <- launch_preflight_latest_par(model_dir_abs)
    base_par_exists <- !is.na(base_par) && file.exists(base_par)
    model_par_exists <- !is.na(model_par) && file.exists(model_par)
    fitted_enabled <- launch_preflight_truthy(model_env$fitted_model_source_enabled)
    fitted_source_dir <- launch_preflight_first(model_env$fitted_model_source_dir)
    fitted_source_abs <- if (nzchar(fitted_source_dir)) resolve_repo_path(fitted_source_dir) else ""
    fitted_par <- launch_preflight_latest_par(fitted_source_abs)
    fitted_par_exists <- fitted_enabled && !is.na(fitted_par) && file.exists(fitted_par)
    auto_run_model <- launch_preflight_truthy(model_env$auto_run_model_before_dependency)
    frq_n <- length(launch_preflight_files(base_dir_abs, "\\.frq$"))
    ini_n <- length(launch_preflight_files(base_dir_abs, "\\.ini$"))
    tag_n <- length(launch_preflight_files(base_dir_abs, "\\.tag$"))
    age_n <- length(launch_preflight_files(base_dir_abs, "\\.age_length$"))
    doitall <- file.exists(file.path(base_dir_abs, "doitall.sh"))
    base_indepvar <- launch_preflight_has_indepvar(model_dir_abs, base_dir_abs)
    indepvar <- base_indepvar
    fitted_indepvar_exists <- FALSE
    if (fitted_enabled && nzchar(fitted_source_abs) && dir.exists(fitted_source_abs)) {
      fitted_indepvar <- file.path(fitted_source_abs, "indepvar.rpt")
      fitted_indepvar_exists <- file.exists(fitted_indepvar)
      if (fitted_indepvar_exists) indepvar <- unique(c(indepvar, fitted_indepvar))
    }
    mfcl_commands_for_model <- launch_preflight_first(model_env$mfcl_commands)
    model_needs_par <- nzchar(mfcl_commands_for_model) && !identical(trimws(mfcl_commands_for_model), "./doitall.sh")
    auto_model_ready <- auto_run_model && base_exists && frq_n > 0 && (base_par_exists || (ini_n > 0 && doitall && !model_needs_par))
    input_or_fitted_par_exists <- base_par_exists || fitted_par_exists || auto_model_ready
    dependency_indepvar_ok <- length(indepvar) > 0 || auto_model_ready
    indepvar_source <- launch_preflight_indepvar_source_label(base_indepvar, fitted_indepvar_exists, auto_model_ready)

    found_common <- paste(
      c(
        if (base_exists) paste0("base_dir=", base_dir) else paste0("missing base_dir=", base_dir),
        if (base_par_exists) paste0("base par=", basename(base_par)) else "base par=missing",
        if (model_par_exists) paste0("model par=", basename(model_par)) else if (model_exists) "model par=missing" else "model_dir=missing",
        if (fitted_enabled && fitted_par_exists) paste0("fitted par=", basename(fitted_par)) else if (fitted_enabled) "fitted par=missing",
        if (auto_run_model) paste0("auto model first=", if (auto_model_ready) "ready" else "not ready"),
        paste0("frq=", frq_n),
        paste0("ini=", ini_n),
        paste0("indepvar=", length(indepvar)),
        paste0("indepvar source=", indepvar_source)
      ),
      collapse = "; "
    )

    fitted_required_jobs <- if (exists("fitted_source_job_types", mode = "function")) fitted_source_job_types() else c("stage_check", "jitter", "hessian", "prof", "prof_chain", "prof_2d")
    if (fitted_enabled && job_type %in% fitted_required_jobs && !identical(job_type, "stage_check") && !fitted_par_exists) {
      return(launch_preflight_row(
        display_name,
        job_type,
        "blocked",
        "fitted .par",
        found_common,
        "Fitted model source is enabled, but no matched/selected fitted .par was found."
      ))
    }

    if (!base_exists && launch_preflight_recipe_ready(model_env)) {
      recipe_source <- launch_preflight_recipe_source(model_env, model_needs_par = model_needs_par)
      needs_par_after_build <- launch_preflight_dependent_job_needs_par(job_type)
      needs_indepvar_after_build <- launch_preflight_dependent_job_needs_indepvar(job_type, model_env)
      can_build_input <- isTRUE(recipe_source$exists)
      can_run_model_first <- isTRUE(auto_run_model) && isTRUE(recipe_source$model_ready)
      can_supply_par_after_build <- !isTRUE(needs_par_after_build) || isTRUE(fitted_par_exists) || isTRUE(can_run_model_first)
      can_supply_indepvar_after_build <- !isTRUE(needs_indepvar_after_build) ||
        isTRUE(fitted_indepvar_exists) ||
        isTRUE(can_run_model_first)
      ok <- can_build_input && can_supply_par_after_build && can_supply_indepvar_after_build
      required_bits <- c(
        "on-demand input build",
        if (isTRUE(needs_par_after_build) && !isTRUE(fitted_par_exists)) "prerequisite model .par" else character(0),
        if (isTRUE(needs_indepvar_after_build) && !isTRUE(fitted_indepvar_exists)) "prerequisite indepvar.rpt" else character(0)
      )
      found_common_recipe <- if (isTRUE(can_run_model_first)) {
        sub("auto model first=not ready", "auto model first=after input build", found_common, fixed = TRUE)
      } else {
        found_common
      }
      found_recipe <- paste(
        found_common_recipe,
        launch_preflight_recipe_detail(model_env),
        recipe_source$found,
        if (isTRUE(needs_par_after_build) || isTRUE(needs_indepvar_after_build)) {
          paste0("run model first after build=", if (can_run_model_first) "ready" else "not ready")
        } else {
          "run model first after build=not needed"
        },
        sep = "; "
      )
      detail <- if (!can_build_input) {
        "Input folder is not present locally yet, but the recipe source input is missing; the sensitivity input cannot be built."
      } else if (isTRUE(needs_par_after_build) && isTRUE(needs_indepvar_after_build) && isTRUE(can_run_model_first)) {
        paste0(
          "Input folder is not present locally yet; the Condor job will build this sensitivity input, ",
          "then run the prerequisite model first to create .par and indepvar.rpt before running ", job_type, "."
        )
      } else if (isTRUE(needs_par_after_build) && isTRUE(can_run_model_first)) {
        paste0(
          "Input folder is not present locally yet; the Condor job will build this sensitivity input, ",
          "then run the prerequisite model first to create .par before running ", job_type, "."
        )
      } else if (isTRUE(needs_indepvar_after_build) && isTRUE(can_run_model_first)) {
        paste0(
          "Input folder is not present locally yet; the Condor job will build this sensitivity input, ",
          "then run the prerequisite model first to create indepvar.rpt before running ", job_type, "."
        )
      } else if (isTRUE(fitted_par_exists)) {
        "Input folder is not present locally yet; the Condor job will build this sensitivity input, then use the selected fitted .par source."
      } else if (isTRUE(needs_par_after_build) || isTRUE(needs_indepvar_after_build)) {
        "Input folder is not present locally yet, but this job also needs .par/indepvar.rpt and no prerequisite model or fitted source is ready."
      } else if (identical(job_type, "model")) {
        "Input folder is not present locally yet; the Condor job will build this sensitivity input and then run the model."
      } else {
        "Input folder is not present locally yet; the Condor job will build this sensitivity input before running."
      }
      return(launch_preflight_row(
        display_name,
        job_type,
        launch_preflight_status(ok),
        paste(required_bits, collapse = " + "),
        found_recipe,
        detail
      ))
    }

    if (!base_exists) {
      return(launch_preflight_row(display_name, job_type, "blocked", "base_dir", found_common, "Input base_dir does not exist."))
    }

    if (identical(job_type, "stage_check")) {
      ok <- base_exists && frq_n > 0
      detail <- if (ok) {
        "Setup check will verify Condor transfer, input availability, on-demand recipe metadata, and fitted-source overlay, then stop before MFCL."
      } else {
        "Setup check needs at least a staged/buildable input folder with .frq."
      }
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), "transfer audit + .frq", found_common, detail))
    }

    if (identical(job_type, "model")) {
      ok <- frq_n > 0 && (base_par_exists || (ini_n > 0 && doitall && !model_needs_par))
      detail <- if (ok && base_par_exists) {
        "Model can start from existing input .par."
      } else if (ok) {
        "Model can start from .ini/doitall; no input .par found."
      } else if (model_needs_par && !base_par_exists) {
        "Configured MFCL command needs a .par, but no input .par was found."
      } else {
        "Need .frq plus either input .par or .ini/doitall."
      }
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), ".frq and .par or .ini/doitall", found_common, detail))
    }

    if (identical(job_type, "jitter")) {
      ok <- input_or_fitted_par_exists && frq_n > 0 && ini_n > 0 && dependency_indepvar_ok
      warn <- ok && !doitall
      detail <- if (ok && !warn) {
        if (fitted_par_exists && isTRUE(fitted_indepvar_exists)) {
          "Jitter will use the selected fitted .par; the fitted-source payload includes indepvar.rpt."
        } else if (fitted_par_exists && length(base_indepvar) > 0) {
          "Jitter will use the selected fitted .par with indepvar.rpt from the base/model input."
        } else if (fitted_par_exists && auto_model_ready) {
          "Jitter will run the prerequisite model first to provide indepvar.rpt, then overlay the selected fitted .par."
        } else if (auto_model_ready && !base_par_exists) {
          "Jitter will run the prerequisite model first, then use that fitted .par."
        } else {
          "Jitter has reference .par and .ini inputs."
        }
      } else if (ok) {
        "Jitter has .par/.ini but doitall.sh was not found; check this model uses the current jitter workflow."
      } else if (!dependency_indepvar_ok) {
        "Jitter needs indepvar.rpt for reference mapping. It can come from base/model input, fitted-source payload, or an automatic prerequisite model run."
      } else {
        "Jitter needs an input .par for reference mapping plus .frq/.ini/indepvar.rpt."
      }
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok, warn), ".par, .frq, .ini, indepvar.rpt", found_common, detail))
    }

    if (identical(job_type, "hessian")) {
      ok <- input_or_fitted_par_exists
      detail <- if (ok && fitted_par_exists && !base_par_exists) {
        "Hessian will overlay the selected fitted .par onto the base input files."
      } else if (ok && auto_model_ready && !base_par_exists) {
        "Hessian will run the prerequisite model first, then use that fitted .par."
      } else if (ok) {
        "Hessian can use the latest input .par."
      } else {
        "Hessian needs at least one input .par."
      }
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), ".par", found_common, detail))
    }

    if (identical(job_type, "retro")) {
      ok <- frq_n > 0 && tag_n > 0 && age_n > 0 && ini_n > 0
      detail <- if (ok) {
        "Retro starts from peeled .ini inputs; copied .par files are removed before run."
      } else {
        "Retro needs .frq, .tag, .age_length, and .ini. It does not require an input .par."
      }
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), ".frq, .tag, .age_length, .ini", found_common, detail))
    }

    if (identical(job_type, "prof")) {
      profile_components <- if (exists("selected_profile_components", mode = "function")) {
        intersect(selected_profile_components(), c("standard", "individual"))
      } else {
        c("standard", "individual")
      }
      prof_envs <- launch_preflight_profile_envs(model_env, profile_components)
      if (length(prof_envs) == 0) {
        return(launch_preflight_row(display_name, job_type, "info", "profile component", found_common, "No standard or individual parameter profile component selected."))
      }
      needs_indepvar <- any(vapply(prof_envs, function(env) nzchar(launch_preflight_first(env$prof_fix_indepvar)), logical(1)))
      ok <- input_or_fitted_par_exists && (!needs_indepvar || dependency_indepvar_ok)
      detail <- if (ok && needs_indepvar) {
        if (fitted_par_exists && isTRUE(fitted_indepvar_exists)) {
          "Profile will use the selected fitted .par; the fitted-source payload includes indepvar.rpt for fixed-parameter profile."
        } else if (fitted_par_exists && length(base_indepvar) > 0) {
          "Profile will use the selected fitted .par with indepvar.rpt from the base/model input."
        } else if (fitted_par_exists && auto_model_ready) {
          "Profile will run the prerequisite model first to provide indepvar.rpt, then overlay the selected fitted .par."
        } else if (auto_model_ready && !base_par_exists) {
          "Profile will run the prerequisite model first, then use its fitted .par/indepvar.rpt."
        } else {
          "Profile has input .par and indepvar.rpt for fixed-parameter profile."
        }
      } else if (ok) {
        if (fitted_par_exists && !base_par_exists) {
          "Profile will overlay the selected fitted .par onto the base input files."
        } else if (auto_model_ready && !base_par_exists) {
          "Profile will run the prerequisite model first, then use that fitted .par."
        } else {
          "Profile has input .par."
        }
      } else if (!input_or_fitted_par_exists) {
        "Profile needs at least one input .par in base_dir."
      } else {
        "Fixed-parameter profile needs indepvar.rpt in model_dir or base_dir."
      }
      req <- if (needs_indepvar) ".par and indepvar.rpt" else ".par"
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), req, found_common, detail))
    }

    if (identical(job_type, "prof_2d")) {
      prof_2d_envs <- launch_preflight_profile_envs(model_env, "prof_2d")
      if (!exists("selected_profile_envs", mode = "function")) {
        prof_2d_envs <- Filter(launch_preflight_has_prof_2d, prof_2d_envs)
      }
      has_spec <- length(prof_2d_envs) > 0
      uses_scalars <- any(vapply(prof_2d_envs, function(env) {
        nzchar(launch_preflight_first(env$prof_2d_scalars_x)) || nzchar(launch_preflight_first(env$prof_2d_scalars_y))
      }, logical(1)))
      baseline_par <- if (model_par_exists) model_par else if (base_par_exists) base_par else fitted_par
      baseline_ok <- (!is.na(baseline_par) && file.exists(baseline_par)) || auto_model_ready
      ok <- has_spec && input_or_fitted_par_exists && baseline_ok && (!uses_scalars || dependency_indepvar_ok)
      detail <- if (!has_spec) {
        "No 2D profile spec found in selected profile_sets."
      } else if (!input_or_fitted_par_exists) {
        "2D profile eventually calls profile runs, so base_dir needs an input .par."
      } else if (!baseline_ok) {
        "2D profile needs a baseline .par in model_dir or base_dir."
      } else if (uses_scalars && !dependency_indepvar_ok) {
        "2D scalar mode needs indepvar.rpt from base/model input, fitted-source payload, or an automatic prerequisite model run."
      } else {
        if (auto_model_ready && (is.na(baseline_par) || !file.exists(baseline_par))) {
          "2D profile will run the prerequisite model first, then use that fitted .par."
        } else {
          paste0("2D profile ready; baseline par=", basename(baseline_par), ".")
        }
      }
      req <- if (uses_scalars) ".par, 2D spec, indepvar.rpt" else ".par and 2D spec"
      return(launch_preflight_row(display_name, job_type, launch_preflight_status(ok), req, found_common, detail))
    }

    launch_preflight_row(display_name, job_type, "warning", "unknown", found_common, "No preflight rule for this job type.")
  }

  launch_preflight_current <- reactive({
    input$refresh_launch_preflight
    if (length(rv$models) == 0) return(launch_preflight_empty("Load a common launch settings config first."))
    job_types <- input$job_types
    if (is.null(job_types) || length(job_types) == 0) return(launch_preflight_empty("Select one or more job types."))
    if (exists("effective_selected_job_types", mode = "function")) job_types <- effective_selected_job_types(job_types)
    if (length(job_types) == 0) return(launch_preflight_empty("Select one or more profile components."))
    selected <- if (exists("selected_models_from_checkboxes", mode = "function")) selected_models_from_checkboxes() else rv$selected_models
    if (length(selected) == 0) return(launch_preflight_empty("Select one or more input folders."))

    rows <- list()
    for (m in selected) {
      model_env <- if (exists("active_model_env", mode = "function")) active_model_env(m) else rv$models[[m]]
      if (is.null(model_env)) next
      for (jt in job_types) {
        rows[[length(rows) + 1L]] <- launch_preflight_check_one(m, model_env, jt)
      }
    }
    if (length(rows) == 0) return(launch_preflight_empty("No selected model/job pairs."))
    do.call(rbind, rows)
  })

  output$launch_preflight_summary <- renderText({
    df <- launch_preflight_current()
    if (!is.data.frame(df) || nrow(df) == 0 || !"Status" %in% names(df)) return("ready=0, warning=0, blocked=0")
    counts <- table(df$Status)
    status_order <- c("ready", "warning", "blocked")
    values <- setNames(rep(0L, length(status_order)), status_order)
    overlap <- intersect(names(counts), status_order)
    values[overlap] <- as.integer(counts[overlap])
    paste(paste(names(values), values, sep = "=", collapse = ", "))
  })

  output$launch_preflight_table <- DT::renderDT({
    df <- launch_preflight_current()
    if (!is.data.frame(df) || nrow(df) == 0) df <- launch_preflight_empty()
    DT::datatable(
      df,
      rownames = FALSE,
      selection = "none",
      escape = TRUE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE,
        scrollY = "320px",
        scrollCollapse = TRUE,
        columnDefs = list(
          list(width = "70px", targets = 2),
          list(width = "220px", targets = 5)
        )
      ),
      class = "compact stripe hover"
    ) |>
      DT::formatStyle(
        "Status",
        color = DT::styleEqual(
          c("ready", "warning", "blocked", "info"),
          c("#0f5132", "#664d03", "#842029", "#41464b")
        ),
        backgroundColor = DT::styleEqual(
          c("ready", "warning", "blocked", "info"),
          c("#d1e7dd", "#fff3cd", "#f8d7da", "#e2e3e5")
        ),
        fontWeight = "700"
      )
  })
