  # ===== SETTINGS SAVE/LOAD =====
  # Repo root + path helpers
  repo_root_default <- normalizePath(file.path(app_dir, ".."), mustWork = FALSE)
  
  repo_root_val <- reactive({
    if (!is.null(input$repo_root) && input$repo_root != "") {
      normalizePath(input$repo_root, mustWork = FALSE)
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
    file.path(repo_root_val(), "configs", ".last_settings.rds")
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
          updateTextInput(session, "repo_root", value = ls$last_repo_root)
        }
        if (!is.null(ls$last_download_location) && ls$last_download_location != "") {
          updateTextInput(session, "download_location", value = ls$last_download_location)
        }
      }, error = function(e) {
        # ignore
      })
    }
  }
  
  save_launcher_settings <- function() {
    ls <- list(
      last_repo_root = repo_root_val(),
      last_download_location = input$download_location
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
          updateTextInput(session, "repo_root", value = saved_settings$repo_root)
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
          updateSelectInput(session, "branch", selected = saved_settings$branch)
        }
        
        if (!is.null(saved_settings$job_type)) {
          rv$last_job_type <- saved_settings$job_type
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
      job_type = input$job_type,
      last_browse_path = rv$last_browse_path,
      last_config_file = rv$config_path,
      condor_cpus = input$condor_cpus,
      condor_memory = input$condor_memory,
      condor_disk = input$condor_disk,
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
  
  observeEvent(input$condor_cpus, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$condor_memory, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$condor_disk, {
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
      rv$last_browse_path <- file.path(repo_root_val(), "configs")
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
    last_browse_path = file.path(repo_root_default, "configs"),
    last_job_type = NULL,
    pending_repo_root_path = NULL,
    pending_download_path = NULL,
    pending_remote_path = NULL,
    pending_program_path = NULL,
    pending_basedir_path = NULL,
    pending_config_file = NULL,
    launcher_settings_loaded = FALSE
  )

  get_makefile_targets <- function(repo_root) {
    mk <- file.path(repo_root, "Makefile")
    if (!file.exists(mk)) return(character(0))
    lines <- readLines(mk, warn = FALSE)
    # Strip comments
    lines <- sub("#.*$", "", lines)
    # Match target lines like "name: ..."
    m <- regmatches(lines, regexpr("^\\s*([A-Za-z0-9_.-]+)\\s*:", lines, perl = TRUE))
    targets <- gsub("^\\s*|\\s*$", "", sub(":$", "", m))
    targets <- targets[targets != ""]
    # Filter
    targets <- targets[grepl("shiny-", targets)]
    targets <- targets[!grepl("docker", targets)]
    unique(targets)
  }

  observeEvent(repo_root_val(), {
    targets <- get_makefile_targets(repo_root_val())
    if (length(targets) == 0) {
      # fallback to legacy defaults
      choices <- c("Model" = "model",
                   "Jitter" = "jitter",
                   "Hessian" = "hessian",
                   "Retrospective" = "retro",
                   "Profile" = "prof")
      updateSelectInput(session, "job_type", choices = choices, selected = "model")
    } else {
      choices <- setNames(targets, targets)
      selected <- if (!is.null(rv$last_job_type) && rv$last_job_type %in% targets) {
        rv$last_job_type
      } else {
        targets[1]
      }
      updateSelectInput(session, "job_type", choices = choices, selected = selected)
    }
  }, ignoreInit = FALSE)
  
  # Initialize directories
  observeEvent(repo_root_val(), {
    configs_dir <- file.path(repo_root_val(), "configs")
    if (!dir.exists(configs_dir)) {
      dir.create(configs_dir, recursive = TRUE)
    }
    models_ran_dir <- file.path(configs_dir, "models_ran")
    if (!dir.exists(models_ran_dir)) {
      dir.create(models_ran_dir, recursive = TRUE)
    }
    
    model_dir <- normalizePath(file.path(repo_root_val(), "model"), mustWork = FALSE)
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
  }, ignoreInit = FALSE)
