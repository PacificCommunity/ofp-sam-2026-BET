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
  
  load_launcher_settings <- function() {
    if (file.exists(launcher_settings_path())) {
      tryCatch({
        ls <- readRDS(launcher_settings_path())
        rv$launcher_settings_loaded <- TRUE
        if (!is.null(ls$last_repo_root) && ls$last_repo_root != "") {
          updateTextInput(session, "repo_root", value = normalize_repo_root_candidate(ls$last_repo_root))
        }
        if (!is.null(ls$last_download_location) && ls$last_download_location != "") {
          updateTextInput(session, "download_location", value = ls$last_download_location)
        }
        if (!is.null(ls$remote_user)) updateTextInput(session, "remote_user", value = ls$remote_user)
        if (!is.null(ls$remote_host)) updateTextInput(session, "remote_host", value = ls$remote_host)
        if (!is.null(ls$github_username)) updateTextInput(session, "github_username", value = ls$github_username)
        if (!is.null(ls$github_org)) updateTextInput(session, "github_org", value = ls$github_org)
        if (!is.null(ls$github_repo)) updateTextInput(session, "github_repo", value = ls$github_repo)
        if (!is.null(ls$docker_image)) updateTextInput(session, "docker_image", value = ls$docker_image)
        if (!is.null(ls$condor_cpus)) updateNumericInput(session, "condor_cpus", value = ls$condor_cpus)
        if (!is.null(ls$condor_memory)) updateNumericInput(session, "condor_memory", value = ls$condor_memory)
        if (!is.null(ls$condor_disk)) updateNumericInput(session, "condor_disk", value = ls$condor_disk)
      }, error = function(e) {
        # ignore
      })
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
      docker_image = input$docker_image,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk
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
      last_browse_path = rv$last_browse_path,
      last_config_file = rv$config_path,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
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
