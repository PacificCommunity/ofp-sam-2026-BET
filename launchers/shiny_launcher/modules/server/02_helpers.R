  # ========== JOB HISTORY FUNCTIONS ==========
  
  get_job_history_file <- function(config_file) {
    if (is.null(config_file)) return(NULL)
    config_basename <- tools::file_path_sans_ext(basename(config_file))
    job_history_file <- file.path(resolve_repo_path(".models_ran"), 
                                  paste0(config_basename, "_job_history.rds"))
    return(job_history_file)
  }
  
  load_job_history <- function(config_file) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file) || !file.exists(job_file)) {
      return(data.frame(
        timestamp = character(),
        job_type = character(),
        model_names = character(),
        output_dir = character(),
        batch_names = character(),
        remote_dirs = character(),
        branch = character(),
        status = character(),
        stringsAsFactors = FALSE
      ))
    }
    readRDS(job_file)
  }
  
  save_job_history <- function(config_file, job_record) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file)) return()
    job_dir <- dirname(job_file)
    if (!dir.exists(job_dir)) dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
    history <- load_job_history(config_file)
    history <- rbind(history, job_record)
    saveRDS(history, job_file)
  }
  
  # ========== LOAD MODELS FUNCTION ==========
  
  load_models <- function(config_path = NULL, is_saved_run = FALSE, original_filename = NULL) {
    
    current_wd <- getwd()
    possible_paths <- c()
    
    if (!is.null(config_path)) {
      possible_paths <- c(possible_paths, config_path)
    }
    
    if (!is.null(input$config_file_path) && input$config_file_path != "") {
      possible_paths <- c(possible_paths, input$config_file_path)
    }
    
    if (!is.null(input$launch_config_path) && input$launch_config_path != "") {
      possible_paths <- c(possible_paths, input$launch_config_path)
    }
    
    possible_paths <- c(
      possible_paths,
      "configs/set_model.R",
      "set_model.R"
    )
    
    possible_paths <- unique(possible_paths[!is.na(possible_paths) & possible_paths != ""])
    possible_paths <- vapply(possible_paths, resolve_repo_path, character(1))
    
    rv$config_status_msg <- paste0(
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📁 Current Working Directory:\n",
      current_wd, "\n\n",
      "🔍 Searching for R script with models...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
    )
    
    found_path <- NULL
    for (path in possible_paths) {
      exists <- file.exists(path)
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     ifelse(exists, "✓ ", "✗ "),
                                     path, "\n")
      
      if (exists && is.null(found_path)) {
        found_path <- path
      }
    }
    
    if (is.null(found_path)) {
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     "\n❌ No R script with models found!\n",
                                     "Please provide a path to an R script containing a 'models' list object.")
      showNotification("Config file not found", type = "error")
      return(FALSE)
    }
    
    rv$config_status_msg <- paste0(rv$config_status_msg, 
                                   "\n✅ Found R script at:\n", found_path, "\n\n")
    
    tryCatch({
      if (grepl("\\.rds|\\.RDS", found_path)) {
        saved_data <- readRDS(found_path)
        rv$models <- saved_data$models
        rv$models_original <- saved_data$models
        rv$run_metadata <- saved_data$metadata
        rv$base_config_name <- saved_data$metadata$base_config
        rv$current_config_file <- found_path
        
        # Extract summary from saved config
        if (!is.null(saved_data$metadata$summary)) {
          rv$run_metadata$summary <- saved_data$metadata$summary
        }
        
        # Extract summary from saved config
        if (!is.null(saved_data$metadata$summary)) {
          rv$run_metadata$summary <- saved_data$metadata$summary
        }
        
        job_history <- load_job_history(found_path)
        rv$config_status_msg <- paste0(rv$config_status_msg, 
                                       "\n✓ Loaded saved run configuration",
                                       "\n  - Run Name: ", saved_data$metadata$run_name,
                                       "\n  - Description: ", saved_data$metadata$description,
                                       "\n  - Date: ", format(saved_data$metadata$date, "%Y-%m-%d %H:%M"),
                                       "\n  - Base Config: ", saved_data$metadata$base_config,
                                       "\n  - Created By: ", saved_data$metadata$created_by,
                                       "\n  - Models: ", length(rv$models))
        
        if (nrow(job_history) > 0) {
          rv$config_status_msg <- paste0(rv$config_status_msg,
                                         "\n📊 Job Run History (", nrow(job_history), " runs):\n")
          
          for (i in 1:min(5, nrow(job_history))) {
            row <- job_history[nrow(job_history) - i + 1, ]
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ", i, ". ", row$timestamp, " - ", row$job_type, 
                                           " (", row$model_names, ")\n",
                                           "     Output: ", row$output_dir, "\n",
                                           "     Branch: ", row$branch, "\n")
          }
          
          if (nrow(job_history) > 5) {
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ... and ", nrow(job_history) - 5, " more runs\n")
          }
        }
        
        rv$config_status_msg <- paste0(rv$config_status_msg,
                                       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      } else {
        env <- new.env()
        source(found_path, local = env)
        
        # Extract summary if defined in the script
        if (exists("summary", envir = env)) {
          rv$run_metadata$summary <- env$summary
        } else {
          rv$run_metadata$summary <- NULL
        }
        
        if (exists("models", envir = env)) {
          
          rv$run_metadata$summary <- env$summary
        } else {
          rv$run_metadata$summary <- NULL
        }
        
        if (exists("models", envir = env)) {
          rv$models <- env$models
          rv$models_original <- env$models
          rv$base_config_name <- basename(found_path)
          rv$current_config_file <- NULL
          rv$config_status_msg <- paste0(rv$config_status_msg,
                                         "\n✓ Successfully loaded R script",
                                         "\n  - Script: ", basename(found_path),
                                         "\n  - Models found: ", length(rv$models),
                                         "\n  - ", paste(names(rv$models), collapse = " - "), "\n")
        } else {
          stop("No 'models' list object found in the R script.")
        }
      }
      
      rv$config_loaded <- TRUE
      
      # Store the actual full path for saving
      if (!is.null(original_filename)) {
        rv$config_path <- found_path  # Store full path, not just filename
      } else {
        rv$config_path <- found_path
      }
      
      # Store the original filename for display
      rv$uploaded_filename <- if (!is.null(original_filename)) original_filename else basename(found_path)
      
      
      rv$selected_models <- names(rv$models)
      updateSelectInput(session, "edit_model_select", 
                        choices = names(rv$models), 
                        selected = names(rv$models)[1])
      
      showNotification(paste("Loaded", length(rv$models), "models"), type = "message")
      
      # Save this config path for next time
      save_settings()
      
      
      return(TRUE)
      
      
    }, error = function(e) {
      rv$config_status_msg <- paste0(rv$config_status_msg, "❌ Error loading R script:\n", e$message)
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
  }
  
  observe({
    if (!rv$config_loaded) {
      # Check if we have a saved last config path
      if (file.exists(settings_path())) {
        tryCatch({
          saved_settings <- readRDS(settings_path())
          
          # If we have a saved config file, try to load it
          if (!is.null(saved_settings$last_config_file) && 
              saved_settings$last_config_file != "") {
            
            # Try to find the file
            possible_paths <- c(
              saved_settings$last_config_file,
              file.path(resolve_repo_path(".launcher_configs"), saved_settings$last_config_file),
              file.path(resolve_repo_path(".launcher_configs"), basename(saved_settings$last_config_file)),
              file.path(resolve_repo_path("configs"), saved_settings$last_config_file),
              file.path(resolve_repo_path("configs"), basename(saved_settings$last_config_file))
            )
            
            for (path in possible_paths) {
              if (file.exists(path)) {
                load_models(config_path = path,
                            original_filename = basename(path))
                return()
              }
            }
          }
        }, error = function(e) {
          # Silently ignore errors
        })
      }
      
      # Don't load anything by default - user must choose
      # This prevents auto-loading set_model.R every time
    }
  })
  
  
  
  observeEvent(input$reload_config, { load_models() })
  
  observeEvent(input$config_file_upload, {
    req(input$config_file_upload)
    load_models(input$config_file_upload$datapath)
  })
  
  # ---- Launch tab config loader handlers ----
  # observeEvent(input$launch_load_config, {
  #   load_models()
  # })
  
  # observeEvent(input$launch_config_upload, {
  #   req(input$launch_config_upload)
  #   
  #   # Store the actual uploaded filename and temp path
  #   rv$uploaded_filename <- input$launch_config_upload$name
  #   rv$uploaded_temp_path <- input$launch_config_upload$datapath
  #   
  #   # Load models with original filename
  #   load_models(
  #     config_path = input$launch_config_upload$datapath,
  #     original_filename = input$launch_config_upload$name
  #   )
  # })
  # 
  # 
  
  
