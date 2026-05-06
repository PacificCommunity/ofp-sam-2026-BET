  # ===== SETTINGS SAVE/LOAD =====
  # Repo root + path helpers
  repo_root_default <- normalizePath(file.path(app_dir, "..", ".."), mustWork = FALSE)

  normalize_repo_root_candidate <- function(path_in) {
    p <- normalizePath(path_in, mustWork = FALSE)
    if (basename(p) == "launchers" && dir.exists(file.path(p, "shiny_launcher"))) {
      parent <- dirname(p)
      if (dir.exists(parent)) return(parent)
    }
    p
  }
  
  repo_root_val <- reactive({
    if (!is.null(input$repo_root) && input$repo_root != "") {
      normalize_repo_root_candidate(input$repo_root)
    } else {
      repo_root_default
    }
  })
  
  is_abs_path <- function(p) {
    if (is.null(p) || p == "") return(FALSE)
    if (grepl("^[A-Za-z]:[\\\\/]", p)) return(TRUE)
    startsWith(p, "/")
  }
  
  resolve_repo_path <- function(p) {
    if (is.null(p) || p == "") return(p)
    if (is_abs_path(p)) return(p)
    file.path(repo_root_val(), p)
  }
  
  settings_path <- function() {
    file.path(repo_root_val(), ".shiny_launcher_settings.rds")
  }
  
  launcher_settings_path <- function() {
    file.path(app_dir, ".launcher_settings.rds")
  }

  read_rds_safe <- function(path) {
    if (!file.exists(path)) return(list())
    tryCatch(readRDS(path), error = function(e) list())
  }

  ensure_named_list <- function(x) {
    if (!is.list(x)) return(list())
    x
  }

  file_mtime_safe <- function(path) {
    if (!file.exists(path)) return(as.POSIXct(NA))
    file.info(path)$mtime
  }

  read_makefile_docker_image <- function(root_dir = repo_root_default) {
    paths <- c(file.path(root_dir, "Makefile"), file.path(root_dir, "makefile"))
    for (mk in paths) {
      if (!file.exists(mk)) next
      lines <- readLines(mk, warn = FALSE)
      idx <- grep("^\\s*DOCKER_IMAGE\\s*=", lines)
      if (length(idx) == 0) next
      v <- sub("^\\s*DOCKER_IMAGE\\s*=\\s*", "", lines[idx[1]])
      if (nzchar(v)) return(v)
    }
    NULL
  }

  current_git_branch <- function(root_dir = repo_root_val()) {
    out <- tryCatch(
      system2("git", c("-C", root_dir, "branch", "--show-current"), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    out <- trimws(out)
    out <- out[nzchar(out)]
    if (length(out) == 0) "" else out[[1]]
  }

  preferred_branch_value <- function(saved = NULL, root_dir = repo_root_val()) {
    saved <- if (is.null(saved) || length(saved) == 0) "" else as.character(saved[[1]])
    current <- current_git_branch(root_dir)
    if (nzchar(saved)) saved else current
  }

  nonempty_first <- function(...) {
    vals <- list(...)
    for (v in vals) {
      if (is.null(v) || length(v) == 0) next
      v <- as.character(v[[1]])
      if (!is.na(v) && nzchar(v)) return(v)
    }
    NULL
  }

  update_branch_selectize <- function(selected = NULL, launcher_obj = list(), root_obj = list(), root_dir = repo_root_val()) {
    branch_choices <- unique(c(
      current_git_branch(root_dir),
      "main",
      "develop",
      "develop_lik",
      launcher_obj[["branch"]],
      root_obj[["branch"]],
      selected,
      tryCatch(input$branch, error = function(e) NULL)
    ))
    branch_choices <- branch_choices[!is.na(branch_choices) & nzchar(branch_choices)]
    if (is.null(selected) || !nzchar(selected)) {
      selected <- if (current_git_branch(root_dir) %in% branch_choices && nzchar(current_git_branch(root_dir))) {
        current_git_branch(root_dir)
      } else {
        branch_choices[[1]]
      }
    }
    updateSelectizeInput(
      session,
      "branch",
      choices = branch_choices,
      selected = selected,
      options = list(create = TRUE, persist = TRUE, placeholder = "Select or type a branch"),
      server = FALSE
    )
  }

  resource_choice_fields <- c(
    "remote_user",
    "remote_host",
    "github_username",
    "github_org",
    "github_repo",
    "docker_image"
  )

  resource_choice_defaults <- list(
    remote_user = unique(c(Sys.getenv("USER", "kyuhank"), "kyuhank")),
    remote_host = unique(c(
      Sys.getenv("NOU_CONDOR", ""),
      "nouofpsubmit.corp.spc.int",
      "suvofpsubmit.corp.spc.int",
      "nouofpsubmit",
      "suvofpsubmit"
    )),
    github_username = "kyuhank",
    github_org = "PacificCommunity",
    github_repo = "ofp-sam-2026-bet",
    docker_image = "ghcr.io/pacificcommunity/bet-2026:v1.9"
  )

  resource_choice_state <- new.env(parent = emptyenv())
  resource_choice_state$choices <- setNames(vector("list", length(resource_choice_fields)), resource_choice_fields)

  normalize_resource_choices <- function(x) {
    x <- unique(trimws(as.character(unlist(x, use.names = FALSE))))
    x[!is.na(x) & nzchar(x)]
  }

  saved_resource_choices <- function(key, launcher_obj, root_obj) {
    normalize_resource_choices(c(
      resource_choice_defaults[[key]],
      launcher_obj[["resource_choices"]][[key]],
      root_obj[["resource_choices"]][[key]],
      launcher_obj[[paste0(key, "_choices")]],
      root_obj[[paste0(key, "_choices")]],
      launcher_obj[[key]],
      root_obj[[key]]
    ))
  }

  current_resource_choices <- function(key) {
    input_value <- tryCatch(input[[key]], error = function(e) NULL)
    normalize_resource_choices(c(
      resource_choice_defaults[[key]],
      resource_choice_state$choices[[key]],
      input_value
    ))
  }

  update_resource_selectize <- function(key, selected = NULL) {
    choices <- normalize_resource_choices(c(current_resource_choices(key), selected))
    resource_choice_state$choices[[key]] <- choices
    updateSelectizeInput(
      session,
      key,
      choices = choices,
      selected = selected,
      options = list(create = TRUE, persist = TRUE),
      server = FALSE
    )
  }

  latest_setting_value <- function(key, launcher_obj, root_obj, launcher_mtime, root_mtime) {
    ordered <- character(0)
    if (!is.na(launcher_mtime) && !is.na(root_mtime)) {
      ordered <- if (launcher_mtime >= root_mtime) c("launcher", "root") else c("root", "launcher")
    } else if (!is.na(launcher_mtime)) {
      ordered <- "launcher"
    } else if (!is.na(root_mtime)) {
      ordered <- "root"
    }

    for (src in ordered) {
      v <- if (src == "launcher") launcher_obj[[key]] else root_obj[[key]]
      if (!is.null(v) && !(is.character(v) && length(v) == 1 && !nzchar(v))) return(v)
    }
    NULL
  }

  as_bool_setting <- function(x, default = FALSE) {
    if (is.null(x) || length(x) == 0) return(default)
    if (is.logical(x)) return(isTRUE(x[[1]]))
    txt <- tolower(trimws(as.character(x[[1]])))
    if (!nzchar(txt)) return(default)
    txt %in% c("1", "true", "yes", "y", "on")
  }

  job_type_choices <- function() {
    c("Model" = "model",
      "Jitter" = "jitter",
      "Hessian" = "hessian",
      "Retrospective" = "retro",
      "Profile" = "prof")
  }

  normalize_saved_job_types <- function(x, fallback = NULL) {
    choices <- job_type_choices()
    vals <- as.character(x)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if ("prof_2d" %in% vals) vals <- unique(c(vals, "prof"))
    vals <- intersect(vals, unname(choices))
    if (length(vals) == 0 && !is.null(fallback)) {
      vals <- intersect(as.character(fallback), unname(choices))
    }
    vals
  }

  update_job_type_choices <- function(selected = NULL) {
    choices <- job_type_choices()
    selected <- normalize_saved_job_types(selected, fallback = tryCatch(input$job_types, error = function(e) NULL))
    if (length(selected) == 0) selected <- "model"
    rv$last_job_type <- selected
    updateCheckboxGroupInput(session, "job_types", choices = choices, selected = selected)
  }
  
  load_launcher_settings <- function() {
    rv$settings_loading <- TRUE
    rv$settings_ready <- FALSE
    on.exit({
      session$onFlushed(function() {
        rv$settings_loading <- FALSE
        rv$settings_ready <- TRUE
      }, once = TRUE)
      tryCatch({
        later::later(function() {
          rv$settings_loading <- FALSE
          rv$settings_ready <- TRUE
        }, delay = 0.5)
      }, error = function(e) {
        rv$settings_loading <- FALSE
        rv$settings_ready <- TRUE
      })
    }, add = TRUE)

    launcher_file <- launcher_settings_path()

    launcher_obj <- ensure_named_list(read_rds_safe(launcher_file))
    startup_repo_root <- launcher_obj[["last_repo_root"]]
    if (is.null(startup_repo_root) || !nzchar(startup_repo_root)) startup_repo_root <- repo_root_default
    startup_repo_root <- normalize_repo_root_candidate(startup_repo_root)

    root_file <- file.path(startup_repo_root, ".shiny_launcher_settings.rds")
    root_obj <- ensure_named_list(read_rds_safe(root_file))
    launcher_mtime <- file_mtime_safe(launcher_file)
    root_mtime <- file_mtime_safe(root_file)

    rv$launcher_settings_loaded <- TRUE

    resource_choice_state$choices <- setNames(vector("list", length(resource_choice_fields)), resource_choice_fields)
    for (k in resource_choice_fields) {
      resource_choice_state$choices[[k]] <- saved_resource_choices(k, launcher_obj, root_obj)
    }

    latest_repo_root <- latest_setting_value("last_repo_root", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (is.null(latest_repo_root)) latest_repo_root <- latest_setting_value("repo_root", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(latest_repo_root) && nzchar(latest_repo_root)) {
      updateTextInput(session, "repo_root", value = normalize_repo_root_candidate(latest_repo_root))
    }

    latest_download <- latest_setting_value("last_download_location", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (is.null(latest_download)) latest_download <- latest_setting_value("download_location", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(latest_download) && nzchar(latest_download)) {
      updateTextInput(session, "download_location", value = latest_download)
    }

    branch_value <- preferred_branch_value(
      latest_setting_value("branch", launcher_obj, root_obj, launcher_mtime, root_mtime),
      startup_repo_root
    )
    update_branch_selectize(branch_value, launcher_obj = launcher_obj, root_obj = root_obj, root_dir = startup_repo_root)

    output_dir_value <- latest_setting_value("output_dir", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(output_dir_value)) {
      updateTextInput(session, "output_dir", value = output_dir_value)
    }

    run_description_value <- latest_setting_value("run_description", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(run_description_value)) {
      updateTextAreaInput(session, "run_description", value = run_description_value)
    }

    job_types_value <- latest_setting_value("job_types", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(job_types_value)) {
      update_job_type_choices(job_types_value)
    }

    for (k in c("job_jitter_n", "hessian_nsplit", "retro_peels_n")) {
      v <- latest_setting_value(k, launcher_obj, root_obj, launcher_mtime, root_mtime)
      if (!is.null(v)) updateNumericInput(session, k, value = v)
    }

    hessian_parallel_value <- latest_setting_value("hessian_parallel", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(hessian_parallel_value)) {
      updateCheckboxInput(session, "hessian_parallel", value = as_bool_setting(hessian_parallel_value, default = TRUE))
    }

    fitted_source_enabled_value <- latest_setting_value("fitted_model_source_enabled", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(fitted_source_enabled_value)) {
      updateCheckboxInput(session, "fitted_model_source_enabled", value = as_bool_setting(fitted_source_enabled_value, default = FALSE))
    }

    fitted_source_choice_value <- latest_setting_value("fitted_model_source_choice", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(fitted_source_choice_value)) {
      updateSelectizeInput(session, "fitted_model_source_choice", selected = as.character(fitted_source_choice_value[[1]]))
    }

    for (k in c("remote_user", "remote_host", "github_username", "github_org", "github_repo")) {
      v <- latest_setting_value(k, launcher_obj, root_obj, launcher_mtime, root_mtime)
      if (!is.null(v)) {
        update_resource_selectize(k, selected = v)
      } else {
        update_resource_selectize(k, selected = resource_choice_defaults[[k]][1])
      }
    }

    launch_mode_value <- latest_setting_value("launch_mode", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(launch_mode_value)) {
      updateRadioButtons(session, "launch_mode", selected = launch_mode_value)
    }

    profile_components_value <- latest_setting_value("profile_components", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(profile_components_value)) {
      updateCheckboxGroupInput(session, "profile_components", selected = as.character(profile_components_value))
    }

    prof_launch_strategy_value <- latest_setting_value("prof_launch_strategy", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(prof_launch_strategy_value)) {
      updateSelectInput(session, "prof_launch_strategy", selected = prof_launch_strategy_value)
    }

    prof_anchor_scalar_value <- latest_setting_value("prof_anchor_scalar", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(prof_anchor_scalar_value)) {
      updateNumericInput(session, "prof_anchor_scalar", value = prof_anchor_scalar_value)
    }

    parallel_launch_value <- latest_setting_value("parallel_launch", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(parallel_launch_value)) {
      updateCheckboxInput(session, "parallel_launch", value = isTRUE(parallel_launch_value))
    }

    launch_parallel_cores_value <- latest_setting_value("launch_parallel_cores", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(launch_parallel_cores_value)) {
      updateSelectInput(session, "launch_parallel_cores", selected = as.character(launch_parallel_cores_value))
    }

    ghcr_login_value <- latest_setting_value("ghcr_login", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(ghcr_login_value)) {
      updateCheckboxInput(session, "ghcr_login", value = isTRUE(ghcr_login_value))
    }

    condor_run_target_value <- latest_setting_value("condor_run_target", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(condor_run_target_value)) {
      updateSelectInput(session, "condor_run_target", selected = condor_run_target_value)
    }

    docker_from_makefile <- read_makefile_docker_image(startup_repo_root)
    docker_from_settings <- latest_setting_value("docker_image", launcher_obj, root_obj, launcher_mtime, root_mtime)
    docker_value <- if (!is.null(docker_from_makefile) && nzchar(docker_from_makefile)) docker_from_makefile else docker_from_settings
    if (!is.null(docker_value) && nzchar(docker_value)) {
      update_resource_selectize("docker_image", selected = docker_value)
    } else {
      update_resource_selectize("docker_image", selected = resource_choice_defaults$docker_image[1])
    }

    local_docker_value <- latest_setting_value("local_docker_image", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (is.null(local_docker_value)) {
      local_docker_value <- docker_value
    }
    if (!is.null(local_docker_value) && nzchar(local_docker_value)) {
      updateTextInput(session, "local_docker_image", value = local_docker_value)
    }

    for (k in c("condor_cpus", "condor_memory", "condor_disk")) {
      v <- latest_setting_value(k, launcher_obj, root_obj, launcher_mtime, root_mtime)
      if (!is.null(v)) updateNumericInput(session, k, value = v)
    }
  }
  
  save_launcher_settings <- function(force = FALSE) {
    if ((isTRUE(rv$settings_loading) || !isTRUE(rv$settings_ready)) && !isTRUE(force)) return(invisible(FALSE))

    existing <- ensure_named_list(read_rds_safe(launcher_settings_path()))
    last_config_file <- nonempty_first(rv$config_path, existing$last_config_file, existing$config_path)

    resource_choices <- setNames(vector("list", length(resource_choice_fields)), resource_choice_fields)
    for (k in resource_choice_fields) {
      resource_choices[[k]] <- current_resource_choices(k)
    }
    resource_choice_state$choices <- resource_choices

    ls <- list(
      last_repo_root = repo_root_val(),
      last_download_location = input$download_location,
      remote_user = input$remote_user,
      remote_host = input$remote_host,
      github_username = input$github_username,
      github_org = input$github_org,
      github_repo = input$github_repo,
      branch = input$branch,
      output_dir = input$output_dir,
      run_description = input$run_description,
      job_types = input$job_types,
      job_jitter_n = input$job_jitter_n,
      hessian_parallel = input$hessian_parallel,
      hessian_nsplit = input$hessian_nsplit,
      retro_peels_n = input$retro_peels_n,
      fitted_model_source_enabled = input$fitted_model_source_enabled,
      fitted_model_source_choice = input$fitted_model_source_choice,
      profile_components = input$profile_components,
      prof_launch_strategy = input$prof_launch_strategy,
      prof_anchor_scalar = input$prof_anchor_scalar,
      last_config_file = last_config_file,
      base_config_name = rv$base_config_name,
      current_config_file = rv$current_config_file,
      launch_mode = input$launch_mode,
      parallel_launch = input$parallel_launch,
      launch_parallel_cores = input$launch_parallel_cores,
      docker_image = input$docker_image,
      local_docker_image = input$local_docker_image,
      ghcr_login = input$ghcr_login,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
      condor_run_target = input$condor_run_target,
      resource_choices = resource_choices,
      timestamp = Sys.time()
    )
    tryCatch({
      saveRDS(ls, launcher_settings_path())
    }, error = function(e) {
      # ignore
    })
  }

  save_resource_choices_after_delete <- function(key, selected) {
    launcher_obj <- ensure_named_list(read_rds_safe(launcher_settings_path()))
    launcher_obj$resource_choices <- resource_choice_state$choices
    launcher_obj[[key]] <- selected
    launcher_obj$timestamp <- Sys.time()
    tryCatch({
      saveRDS(launcher_obj, launcher_settings_path())
    }, error = function(e) {
      # ignore
    })

    root_file <- settings_path()
    root_obj <- ensure_named_list(read_rds_safe(root_file))
    root_obj$resource_choices <- resource_choice_state$choices
    root_obj[[key]] <- selected
    root_obj$timestamp <- Sys.time()
    tryCatch({
      saveRDS(root_obj, root_file)
    }, error = function(e) {
      # ignore
    })
  }

  remove_resource_choice <- function(key) {
    selected <- trimws(as.character(if (is.null(input[[key]])) "" else input[[key]]))
    if (!nzchar(selected)) return(invisible(FALSE))

    choices <- setdiff(current_resource_choices(key), selected)
    resource_choice_state$choices[[key]] <- choices
    next_selected <- if (length(choices) > 0) choices[[1]] else NULL

    updateSelectizeInput(
      session,
      key,
      choices = choices,
      selected = next_selected,
      options = list(create = TRUE, persist = TRUE),
      server = FALSE
    )
    save_resource_choices_after_delete(key, next_selected)
    showNotification(sprintf("Removed '%s' from %s dropdown", selected, key), type = "message")
    invisible(TRUE)
  }

  for (key in resource_choice_fields) {
    local({
      key_local <- key
      observeEvent(input[[paste0("remove_", key_local, "_choice")]], {
        remove_resource_choice(key_local)
      }, ignoreInit = TRUE)
    })
  }
  
  observe({
    session$onFlushed(function() {
      load_launcher_settings()
      rv$launcher_settings_loaded <- TRUE
    }, once = TRUE)
  })
  
  # Load saved settings on startup
  observeEvent(repo_root_val(), {
    if (isTRUE(rv$settings_loading)) return()
    settings_file <- settings_path()
    if (file.exists(settings_file)) {
      tryCatch({
        saved_settings <- readRDS(settings_file)
        
        if (!is.null(saved_settings$repo_root) && !isTRUE(rv$launcher_settings_loaded)) {
          updateTextInput(session, "repo_root", value = normalize_repo_root_candidate(saved_settings$repo_root))
        }
        
        
        if (!is.null(saved_settings$scan_output_dir)) {
          updateTextInput(session, "scan_output_dir", value = saved_settings$scan_output_dir)
        }
        
        if (!is.null(saved_settings$download_location) && !isTRUE(rv$launcher_settings_loaded)) {
          updateTextInput(session, "download_location", value = saved_settings$download_location)
        }
        
        if (!is.null(saved_settings$extract_repo_name)) {
          updateTextInput(session, "extract_repo_name", value = saved_settings$extract_repo_name)
        }
        
        if (!is.null(saved_settings$extract_path_manual)) {
          updateTextInput(session, "extract_path_manual", value = saved_settings$extract_path_manual)
        }
        
        if (!is.null(saved_settings$output_dir)) {
          updateTextInput(session, "output_dir", value = saved_settings$output_dir)
        }

        if (!is.null(saved_settings$run_description)) {
          updateTextAreaInput(session, "run_description", value = saved_settings$run_description)
        }

        branch_selected <- preferred_branch_value(saved_settings$branch, repo_root_val())
        update_branch_selectize(branch_selected, root_obj = saved_settings, root_dir = repo_root_val())

        if (!is.null(saved_settings$launch_mode)) {
          updateRadioButtons(session, "launch_mode", selected = saved_settings$launch_mode)
        }

        if (!is.null(saved_settings$profile_components)) {
          updateCheckboxGroupInput(session, "profile_components", selected = as.character(saved_settings$profile_components))
        }

        if (!is.null(saved_settings$prof_launch_strategy)) {
          updateSelectInput(session, "prof_launch_strategy", selected = saved_settings$prof_launch_strategy)
        }

        if (!is.null(saved_settings$prof_anchor_scalar)) {
          updateNumericInput(session, "prof_anchor_scalar", value = saved_settings$prof_anchor_scalar)
        }

        if (!is.null(saved_settings$parallel_launch)) {
          updateCheckboxInput(session, "parallel_launch", value = isTRUE(saved_settings$parallel_launch))
        }

        if (!is.null(saved_settings$local_docker_image)) {
          updateTextInput(session, "local_docker_image", value = saved_settings$local_docker_image)
        }

        if (!is.null(saved_settings$ghcr_login)) {
          updateCheckboxInput(session, "ghcr_login", value = isTRUE(saved_settings$ghcr_login))
        }
        
        if (!is.null(saved_settings$job_types)) {
          rv$last_job_type <- saved_settings$job_types
          update_job_type_choices(saved_settings$job_types)
        }

        for (k in c("job_jitter_n", "hessian_nsplit", "retro_peels_n")) {
          if (!is.null(saved_settings[[k]])) updateNumericInput(session, k, value = saved_settings[[k]])
        }

        if (!is.null(saved_settings$hessian_parallel)) {
          updateCheckboxInput(session, "hessian_parallel", value = as_bool_setting(saved_settings$hessian_parallel, default = TRUE))
        }

        if (!is.null(saved_settings$fitted_model_source_enabled)) {
          updateCheckboxInput(session, "fitted_model_source_enabled", value = as_bool_setting(saved_settings$fitted_model_source_enabled, default = FALSE))
        }

        if (!is.null(saved_settings$fitted_model_source_choice)) {
          updateSelectizeInput(session, "fitted_model_source_choice", selected = as.character(saved_settings$fitted_model_source_choice[[1]]))
        }
        
        if (!is.null(saved_settings$last_browse_path)) {
          rv$last_browse_path <- saved_settings$last_browse_path
        }
        
        if (!is.null(saved_settings$last_config_file)) {
          rv$config_path <- saved_settings$last_config_file
        }
        
        if (!is.null(saved_settings$condor_cpus)) {
          updateNumericInput(session, "condor_cpus", value = saved_settings$condor_cpus)
        }
        
        if (!is.null(saved_settings$condor_memory)) {
          updateNumericInput(session, "condor_memory", value = saved_settings$condor_memory)
        }
        
        if (!is.null(saved_settings$condor_disk)) {
          updateNumericInput(session, "condor_disk", value = saved_settings$condor_disk)
        }

        if (!is.null(saved_settings$condor_run_target)) {
          updateSelectInput(session, "condor_run_target", selected = saved_settings$condor_run_target)
        }
        
        if (!is.null(saved_settings$launch_parallel_cores)) {
          updateSelectInput(session, "launch_parallel_cores", selected = as.character(saved_settings$launch_parallel_cores))
        }
        
        if (!is.null(saved_settings$retrieve_parallel_cores)) {
          updateSelectInput(session, "retrieve_parallel_cores", selected = as.character(saved_settings$retrieve_parallel_cores))
        }
        
      }, error = function(e) {
        
        # If settings file is corrupted, ignore and use defaults
      })
    }
  }, ignoreInit = FALSE)
  
  
  # Function to save settings
  save_settings <- function(force = FALSE) {
    if ((isTRUE(rv$settings_loading) || !isTRUE(rv$settings_ready)) && !isTRUE(force)) return(invisible(FALSE))

    existing <- ensure_named_list(read_rds_safe(settings_path()))
    last_config_file <- nonempty_first(rv$config_path, existing$last_config_file, existing$config_path)

    resource_choices <- setNames(vector("list", length(resource_choice_fields)), resource_choice_fields)
    for (k in resource_choice_fields) {
      resource_choices[[k]] <- current_resource_choices(k)
    }
    resource_choice_state$choices <- resource_choices

    settings <- list(
      repo_root = repo_root_val(),
      scan_output_dir = input$scan_output_dir,
      download_location = input$download_location,
      extract_repo_name = input$extract_repo_name,
      extract_path_manual = input$extract_path_manual,
      output_dir = input$output_dir,
      run_description = input$run_description,
      branch = input$branch,
      remote_user = input$remote_user,
      remote_host = input$remote_host,
      github_username = input$github_username,
      github_org = input$github_org,
      github_repo = input$github_repo,
      job_types = input$job_types,
      job_jitter_n = input$job_jitter_n,
      hessian_parallel = input$hessian_parallel,
      hessian_nsplit = input$hessian_nsplit,
      retro_peels_n = input$retro_peels_n,
      fitted_model_source_enabled = input$fitted_model_source_enabled,
      fitted_model_source_choice = input$fitted_model_source_choice,
      profile_components = input$profile_components,
      prof_launch_strategy = input$prof_launch_strategy,
      prof_anchor_scalar = input$prof_anchor_scalar,
      launch_mode = input$launch_mode,
      parallel_launch = input$parallel_launch,
      last_browse_path = rv$last_browse_path,
      last_config_file = last_config_file,
      base_config_name = rv$base_config_name,
      current_config_file = rv$current_config_file,
      local_docker_image = input$local_docker_image,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
      docker_image = input$docker_image,
      ghcr_login = input$ghcr_login,
      condor_run_target = input$condor_run_target,
      launch_parallel_cores = input$launch_parallel_cores,
      retrieve_parallel_cores = input$retrieve_parallel_cores,
      resource_choices = resource_choices,
      timestamp = Sys.time()
    )
    
    
    
    tryCatch({
      saveRDS(settings, settings_path())
    }, error = function(e) {
      # Silently fail if can't save
    })
    
    save_launcher_settings(force = force)
  }

  session$onSessionEnded(function() {
    if (isTRUE(rv$settings_ready)) {
      try(save_settings(force = TRUE), silent = TRUE)
    }
  })
  
  # Auto-save settings when they change
  observeEvent(input$scan_output_dir, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$repo_root, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$download_location, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$extract_repo_name, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$extract_path_manual, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$output_dir, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$run_description, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$branch, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$launch_mode, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$local_docker_image, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$parallel_launch, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$job_types, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$job_jitter_n, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$hessian_parallel, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$hessian_nsplit, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$retro_peels_n, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$fitted_model_source_enabled, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$fitted_model_source_choice, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$profile_components, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$prof_launch_strategy, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$prof_anchor_scalar, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$ghcr_login, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$condor_cpus, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$condor_memory, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$condor_disk, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$condor_run_target, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$launch_parallel_cores, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$retrieve_parallel_cores, {
    save_settings()
  }, ignoreInit = TRUE)

  # Persist launcher-level connection/repo settings immediately on edit.
  observeEvent(
    list(
      input$remote_user,
      input$remote_host,
      input$github_username,
      input$github_org,
      input$github_repo,
      input$branch,
      input$output_dir,
      input$run_description,
      input$job_types,
      input$job_jitter_n,
      input$hessian_parallel,
      input$hessian_nsplit,
      input$retro_peels_n,
      input$fitted_model_source_enabled,
      input$fitted_model_source_choice,
      input$profile_components,
      input$prof_launch_strategy,
      input$prof_anchor_scalar,
      input$launch_mode,
      input$parallel_launch,
      input$launch_parallel_cores,
      input$local_docker_image,
      input$ghcr_login,
      input$docker_image,
      input$condor_cpus,
      input$condor_memory,
      input$condor_disk,
      input$condor_run_target
    ),
    {
      save_launcher_settings()
    },
    ignoreInit = TRUE
  )

  update_makefile_docker_image <- function(new_image) {
    paths <- c(file.path(repo_root_val(), "Makefile"),
               file.path(repo_root_val(), "makefile"))
    updated <- FALSE
    for (mk in paths) {
      if (!file.exists(mk)) next
      lines <- readLines(mk, warn = FALSE)
      idx <- grep("^\\s*DOCKER_IMAGE\\s*=", lines)
      if (length(idx) == 0) next
      lines[idx[1]] <- paste0("DOCKER_IMAGE=", new_image)
      writeLines(lines, mk)
      updated <- TRUE
    }
    updated
  }

  observeEvent(list(input$docker_image, repo_root_val()), {
    if (!is.null(input$docker_image) && input$docker_image != "") {
      ok <- update_makefile_docker_image(input$docker_image)
      if (!ok) {
        showNotification("Makefile/makefile not found or DOCKER_IMAGE not set", type = "warning")
      }
    }
    save_settings()
  }, ignoreInit = TRUE)
  
  # Browse repo root
  show_repo_root_browser <- function(start_path) {
    if (!dir.exists(start_path)) {
      start_path <- "/"
    }
    rv$current_repo_root_browse <- start_path
    parent_path <- dirname(start_path)
    dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
    
    rv$pending_repo_root_path <- NULL
    showModal(modalDialog(
      title = "Select Repository Root",
      size = "m",
      p(strong("Available directories:"), style = "margin-bottom: 10px;"),
      p(paste("Browsing:", start_path), style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
      div(
        style = "max-height: 400px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
        tags$div(
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('repo_root_browse_to', '/', {priority: 'event'}); return false;",
            icon("folder", style = "color: #f39c12;"), " /",
            style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px; font-weight: bold;"
          ),
          if (start_path != "/" && parent_path != start_path) {
            tags$a(
              href = "#",
              onclick = sprintf("Shiny.setInputValue('repo_root_browse_to', '%s', {priority: 'event'}); return false;", parent_path),
              icon("level-up-alt", style = "color: #666;"), " ..",
              style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px; font-weight: bold; margin-left: 10px;"
            )
          },
          style = "padding: 5px 0; margin-bottom: 10px; border-bottom: 2px solid #ddd;"
        ),
        lapply(dirs, function(d) {
          rel_path <- basename(d)
          tags$div(
            tags$a(
              href = "#",
              onclick = sprintf("Shiny.setInputValue('repo_root_browse_to', '%s', {priority: 'event'}); return false;", d),
              icon("folder", style = "color: #3c8dbc;"), " ", rel_path,
              style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
            ),
            style = "padding: 3px 0;"
          )
        })
      ),
      shiny::hr(),
      div(style = "font-size:12px; color:#666;",
          "Selected: ",
          strong(textOutput("repo_root_pending_display", inline = TRUE))
      ),
      actionButton("select_repo_root_current", "Use This Folder", class = "btn-default"),
      textInput("repo_root_manual_path", "Or enter path manually:",
                value = input$repo_root,
                placeholder = "/path/to/repo"),
      footer = tagList(
        modalButton("Close"),
        actionButton("confirm_repo_root_path", "Select", class = "btn-primary")
      )
    ))
  }
  
  observeEvent(input$browse_repo_root, {
    tryCatch({
      start_path <- normalizePath(input$repo_root, mustWork = FALSE)
      show_repo_root_browser(start_path)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$repo_root_browse_to, {
    req(input$repo_root_browse_to)
    show_repo_root_browser(input$repo_root_browse_to)
  }, ignoreInit = TRUE)
  
  observeEvent(input$select_repo_root_current, {
    if (!is.null(rv$current_repo_root_browse) && rv$current_repo_root_browse != "") {
      rv$pending_repo_root_path <- rv$current_repo_root_browse
    }
  }, ignoreInit = TRUE)
  
  output$repo_root_pending_display <- renderText({
    if (!is.null(rv$pending_repo_root_path) && rv$pending_repo_root_path != "") {
      rv$pending_repo_root_path
    } else if (!is.null(rv$current_repo_root_browse) && rv$current_repo_root_browse != "") {
      rv$current_repo_root_browse
    } else {
      "(none)"
    }
  })
  
  observeEvent(input$confirm_repo_root_path, {
    if (!is.null(rv$pending_repo_root_path) && rv$pending_repo_root_path != "") {
      updateTextInput(session, "repo_root", value = rv$pending_repo_root_path)
    } else if (!is.null(rv$current_repo_root_browse) && rv$current_repo_root_browse != "") {
      updateTextInput(session, "repo_root", value = rv$current_repo_root_browse)
    } else if (!is.null(input$repo_root_manual_path) && input$repo_root_manual_path != "") {
      updateTextInput(session, "repo_root", value = input$repo_root_manual_path)
    }
    removeModal()
  }, ignoreInit = TRUE)
  
  
  observeEvent(repo_root_val(), {
    if (is.null(rv$last_browse_path) || !dir.exists(rv$last_browse_path)) {
      rv$last_browse_path <- repo_root_val()
    }
  }, ignoreInit = FALSE)
  
  # ===== END SETTINGS SAVE/LOAD =====
  # Reactive values storage
  rv <- reactiveValues(
    models = list(),
    models_original = list(),
    launch_defaults = NULL,
    launch_log = "",
    retrieval_log = "",
    folders_data = data.frame(),
    archive_contents = list(),
    selected_folders = c(),
    action_status = list(
      scan_results = "",
      download_all = "",
      delete_remote_dir = "",
      download_selected = "",
      delete_selected = ""
    ),
    jobs_status = data.frame(
      Owner = character(),
      BatchName = character(),
      Submitted = character(),
      Done = character(),
      Run = character(),
      Idle = character(),
      Total = character(),
      JobIDs = character(),
      OutputDir = character(),
      RunDescription = character(),
      stringsAsFactors = FALSE
    ),
    config_loaded = FALSE,
    config_path = NULL,
    config_status_msg = "",
    base_config_name = "set_model.R",
    run_metadata = list(
      run_name = "",
      description = "",
      date = NULL,
      base_config = "",
      summary = NULL
    ),
    saved_configs_trigger = 0,
    current_config_file = NULL,
    selected_models = c(),
    # uploaded_filename = NULL,
    # uploaded_temp_path = NULL,
    last_browse_path = repo_root_default,
    last_job_type = NULL,
    pending_repo_root_path = NULL,
    pending_download_path = NULL,
    pending_remote_path = NULL,
    pending_program_path = NULL,
    pending_basedir_path = NULL,
    pending_config_file = NULL,
    launcher_settings_loaded = FALSE
    ,
    settings_loading = TRUE
    ,
    settings_ready = FALSE
    ,
    launcher_job_log_trigger = 0
  )

  observeEvent(repo_root_val(), {
    if (isTRUE(rv$settings_loading)) return()
    update_job_type_choices(rv$last_job_type)
  }, ignoreInit = FALSE)
  
  # Directories are created lazily only when needed (save/download actions).
