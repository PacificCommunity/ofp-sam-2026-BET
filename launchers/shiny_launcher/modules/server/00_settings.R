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
  
  load_launcher_settings <- function() {
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

    for (k in c("remote_user", "remote_host", "github_username", "github_org", "github_repo")) {
      v <- latest_setting_value(k, launcher_obj, root_obj, launcher_mtime, root_mtime)
      if (!is.null(v)) updateTextInput(session, k, value = v)
    }

    launch_mode_value <- latest_setting_value("launch_mode", launcher_obj, root_obj, launcher_mtime, root_mtime)
    if (!is.null(launch_mode_value)) {
      updateRadioButtons(session, "launch_mode", selected = launch_mode_value)
    }

    docker_from_makefile <- read_makefile_docker_image(startup_repo_root)
    docker_from_settings <- latest_setting_value("docker_image", launcher_obj, root_obj, launcher_mtime, root_mtime)
    docker_value <- if (!is.null(docker_from_makefile) && nzchar(docker_from_makefile)) docker_from_makefile else docker_from_settings
    if (!is.null(docker_value) && nzchar(docker_value)) {
      updateTextInput(session, "docker_image", value = docker_value)
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
  
  save_launcher_settings <- function() {
    ls <- list(
      last_repo_root = repo_root_val(),
      last_download_location = input$download_location,
      remote_user = input$remote_user,
      remote_host = input$remote_host,
      github_username = input$github_username,
      github_org = input$github_org,
      github_repo = input$github_repo,
      launch_mode = input$launch_mode,
      docker_image = input$docker_image,
      local_docker_image = input$local_docker_image,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
      timestamp = Sys.time()
    )
    tryCatch({
      saveRDS(ls, launcher_settings_path())
    }, error = function(e) {
      # ignore
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
        
        if (!is.null(saved_settings$branch)) {
          branch_choices <- unique(c("main", "develop", "develop_lik", saved_settings$branch))
          updateSelectizeInput(
            session,
            "branch",
            choices = branch_choices,
            selected = saved_settings$branch,
            server = FALSE
          )
        }

        if (!is.null(saved_settings$launch_mode)) {
          updateRadioButtons(session, "launch_mode", selected = saved_settings$launch_mode)
        }

        if (!is.null(saved_settings$local_docker_image)) {
          updateTextInput(session, "local_docker_image", value = saved_settings$local_docker_image)
        }
        
        if (!is.null(saved_settings$job_types)) {
          rv$last_job_type <- saved_settings$job_types
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
  save_settings <- function() {
    settings <- list(
      repo_root = repo_root_val(),
      scan_output_dir = input$scan_output_dir,
      download_location = input$download_location,
      extract_repo_name = input$extract_repo_name,
      extract_path_manual = input$extract_path_manual,
      output_dir = input$output_dir,
      branch = input$branch,
      job_types = input$job_types,
      launch_mode = input$launch_mode,
      last_browse_path = rv$last_browse_path,
      last_config_file = rv$config_path,
      local_docker_image = input$local_docker_image,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
      docker_image = input$docker_image,
      launch_parallel_cores = input$launch_parallel_cores,
      retrieve_parallel_cores = input$retrieve_parallel_cores,
      timestamp = Sys.time()
    )
    
    
    
    tryCatch({
      saveRDS(settings, settings_path())
    }, error = function(e) {
      # Silently fail if can't save
    })
    
    save_launcher_settings()
  }
  
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
  
  observeEvent(input$branch, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$launch_mode, {
    save_settings()
  }, ignoreInit = TRUE)

  observeEvent(input$local_docker_image, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$job_types, {
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
      input$github_repo
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
  )

  observeEvent(repo_root_val(), {
    choices <- c("Model" = "model",
                 "Jitter" = "jitter",
                 "Hessian" = "hessian",
                 "Retrospective" = "retro",
                 "Profile" = "prof")
    selected <- if (!is.null(rv$last_job_type)) {
      intersect(rv$last_job_type, unname(choices))
    } else {
      "model"
    }
    if (length(selected) == 0) selected <- "model"
    updateCheckboxGroupInput(session, "job_types", choices = choices, selected = selected)
  }, ignoreInit = FALSE)
  
  # Directories are created lazily only when needed (save/download actions).
