  # ========== LAUNCH INPUT PREFLIGHT ==========

  launch_preflight_empty <- function(message = "Select model(s) and job type(s).") {
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
        paste0("recipe base=", launch_preflight_first(model_env$input_recipe_base, "base")),
        paste0("movement=", launch_preflight_first(model_env$input_recipe_movement_pairs, "<none>")),
        paste0("sel_nodes=", launch_preflight_first(model_env$input_recipe_sel_nodes, "<none>")),
        paste0("index_cv_half=", launch_preflight_first(model_env$input_recipe_index_cv_half, "0"))
      ),
      collapse = "; "
    )
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

  launch_preflight_status <- function(ok, warn = FALSE) {
    if (isTRUE(ok) && isTRUE(warn)) "warning" else if (isTRUE(ok)) "ready" else "blocked"
  }

  launch_preflight_check_one <- function(model_name, model_env, job_type) {
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
    frq_n <- length(launch_preflight_files(base_dir_abs, "\\.frq$"))
    ini_n <- length(launch_preflight_files(base_dir_abs, "\\.ini$"))
    tag_n <- length(launch_preflight_files(base_dir_abs, "\\.tag$"))
    age_n <- length(launch_preflight_files(base_dir_abs, "\\.age_length$"))
    doitall <- file.exists(file.path(base_dir_abs, "doitall.sh"))
    indepvar <- launch_preflight_has_indepvar(model_dir_abs, base_dir_abs)

    found_common <- paste(
      c(
        if (base_exists) paste0("base_dir=", base_dir) else paste0("missing base_dir=", base_dir),
        if (base_par_exists) paste0("base par=", basename(base_par)) else "base par=missing",
        if (model_par_exists) paste0("model par=", basename(model_par)) else if (model_exists) "model par=missing" else "model_dir=missing",
        paste0("frq=", frq_n),
        paste0("ini=", ini_n),
        paste0("indepvar=", length(indepvar))
      ),
      collapse = "; "
    )

    if (!base_exists && launch_preflight_recipe_ready(model_env)) {
      found_recipe <- paste(found_common, launch_preflight_recipe_detail(model_env), sep = "; ")
      return(launch_preflight_row(
        model_name,
        job_type,
        "warning",
        "input recipe",
        found_recipe,
        "Input base_dir is missing locally, but the runner is configured to build it from the input recipe at job start."
      ))
    }

    if (!base_exists) {
      return(launch_preflight_row(model_name, job_type, "blocked", "base_dir", found_common, "Input base_dir does not exist."))
    }

    if (identical(job_type, "model")) {
      mfcl_commands <- launch_preflight_first(model_env$mfcl_commands)
      needs_par <- nzchar(mfcl_commands) && !identical(trimws(mfcl_commands), "./doitall.sh")
      ok <- frq_n > 0 && (base_par_exists || (ini_n > 0 && doitall && !needs_par))
      detail <- if (ok && base_par_exists) {
        "Model can start from existing input .par."
      } else if (ok) {
        "Model can start from .ini/doitall; no input .par found."
      } else if (needs_par && !base_par_exists) {
        "Configured MFCL command needs a .par, but no input .par was found."
      } else {
        "Need .frq plus either input .par or .ini/doitall."
      }
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok), ".frq and .par or .ini/doitall", found_common, detail))
    }

    if (identical(job_type, "jitter")) {
      ok <- base_par_exists && frq_n > 0 && ini_n > 0
      warn <- ok && !doitall
      detail <- if (ok && !warn) {
        "Jitter has reference .par and .ini inputs."
      } else if (ok) {
        "Jitter has .par/.ini but doitall.sh was not found; check this model uses the current jitter workflow."
      } else {
        "Jitter needs an input .par for reference mapping plus .frq/.ini."
      }
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok, warn), ".par, .frq, .ini", found_common, detail))
    }

    if (identical(job_type, "hessian")) {
      ok <- base_par_exists
      detail <- if (ok) "Hessian can use the latest input .par." else "Hessian needs at least one input .par."
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok), ".par", found_common, detail))
    }

    if (identical(job_type, "retro")) {
      ok <- frq_n > 0 && tag_n > 0 && age_n > 0 && ini_n > 0
      detail <- if (ok) {
        "Retro starts from peeled .ini inputs; copied .par files are removed before run."
      } else {
        "Retro needs .frq, .tag, .age_length, and .ini. It does not require an input .par."
      }
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok), ".frq, .tag, .age_length, .ini", found_common, detail))
    }

    if (identical(job_type, "prof")) {
      profile_components <- if (exists("selected_profile_components", mode = "function")) {
        intersect(selected_profile_components(), c("standard", "individual"))
      } else {
        c("standard", "individual")
      }
      prof_envs <- launch_preflight_profile_envs(model_env, profile_components)
      if (length(prof_envs) == 0) {
        return(launch_preflight_row(model_name, job_type, "info", "profile component", found_common, "No standard or individual parameter profile component selected."))
      }
      needs_indepvar <- any(vapply(prof_envs, function(env) nzchar(launch_preflight_first(env$prof_fix_indepvar)), logical(1)))
      ok <- base_par_exists && (!needs_indepvar || length(indepvar) > 0)
      detail <- if (ok && needs_indepvar) {
        "Profile has input .par and indepvar.rpt for fixed-parameter profile."
      } else if (ok) {
        "Profile has input .par."
      } else if (!base_par_exists) {
        "Profile needs at least one input .par in base_dir."
      } else {
        "Fixed-parameter profile needs indepvar.rpt in model_dir or base_dir."
      }
      req <- if (needs_indepvar) ".par and indepvar.rpt" else ".par"
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok), req, found_common, detail))
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
      baseline_par <- if (model_par_exists) model_par else base_par
      baseline_ok <- !is.na(baseline_par) && file.exists(baseline_par)
      ok <- has_spec && base_par_exists && baseline_ok && (!uses_scalars || length(indepvar) > 0)
      detail <- if (!has_spec) {
        "No 2D profile spec found in selected profile_sets."
      } else if (!base_par_exists) {
        "2D profile eventually calls profile runs, so base_dir needs an input .par."
      } else if (!baseline_ok) {
        "2D profile needs a baseline .par in model_dir or base_dir."
      } else if (uses_scalars && length(indepvar) == 0) {
        "2D scalar mode needs indepvar.rpt in model_dir or base_dir."
      } else {
        paste0("2D profile ready; baseline par=", basename(baseline_par), ".")
      }
      req <- if (uses_scalars) ".par, 2D spec, indepvar.rpt" else ".par and 2D spec"
      return(launch_preflight_row(model_name, job_type, launch_preflight_status(ok), req, found_common, detail))
    }

    launch_preflight_row(model_name, job_type, "warning", "unknown", found_common, "No preflight rule for this job type.")
  }

  launch_preflight_current <- reactive({
    input$refresh_launch_preflight
    if (length(rv$models) == 0) return(launch_preflight_empty("Load a model config first."))
    job_types <- input$job_types
    if (is.null(job_types) || length(job_types) == 0) return(launch_preflight_empty("Select one or more job types."))
    if (exists("effective_selected_job_types", mode = "function")) job_types <- effective_selected_job_types(job_types)
    if (length(job_types) == 0) return(launch_preflight_empty("Select one or more profile components."))
    selected <- if (exists("selected_models_from_checkboxes", mode = "function")) selected_models_from_checkboxes() else rv$selected_models
    if (length(selected) == 0) return(launch_preflight_empty("Select one or more models."))

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
