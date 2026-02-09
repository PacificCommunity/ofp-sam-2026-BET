  # ========== LAUNCH JOB HANDLERS ==========
  
  observeEvent(input$launch_btn, {
    # Validate model selection
    if (length(rv$selected_models) == 0) { 
      showNotification("Please select at least one model", type = "error")
      return() 
    }
    if (length(rv$models) == 0) { 
      showNotification("Please load models first", type = "error")
      return() 
    }
    
    # Disable button during processing
    shinyjs::disable("launch_btn")
    shinyjs::addClass("launch_btn", "loading")
    
    # Calculate total number of jobs to be launched
    total_jobs <- 0
    for (model_name in rv$selected_models) {
      model_env <- rv$models[[model_name]]
      if (input$job_type == "jitter") {
        seeds <- as.numeric(strsplit(model_env$jitter_seeds, "\\s+")[[1]])
        total_jobs <- total_jobs + length(seeds)
      } else if (input$job_type == "hessian") {
        total_jobs <- total_jobs + as.numeric(model_env$nsplit)
      } else if (input$job_type == "retro") {
        peels <- as.numeric(strsplit(model_env$retro_peels, "\\s+")[[1]])
        total_jobs <- total_jobs + length(peels)
      } else if (input$job_type == "prof") {
        scalers <- as.numeric(strsplit(model_env$scalers, "\\s+")[[1]])
        total_jobs <- total_jobs + length(scalers)
      } else {
        total_jobs <- total_jobs + 1
      }
    }
    
    # Initialize log with total job count
    rv$launch_log <- paste0(
      Sys.time(), " - Starting job submission...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📊 Total jobs to launch: ", total_jobs, "\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    # Create reactive value for progress tracking
    progress_text <- reactiveVal("")
    
    # Show persistent modal dialog with progress
    showModal(modalDialog(
      title = div(
        style = "font-size: 18px; font-weight: bold;",
        icon("rocket"), " Launching Jobs"
      ),
      size = "m",
      
      # Progress indicator
      div(
        style = "text-align: center; margin: 20px 0;",
        div(class = "spinner"),
        h4(
          id = "launch_progress_text",
          style = "color: #3c8dbc; margin-top: 20px;",
          paste0("Starting... (0/", total_jobs, ")")
        )
      ),
      
      # Progress details box
      div(
        style = "background: #f9f9f9; border: 1px solid #ddd; border-radius: 4px; padding: 15px; max-height: 300px; overflow-y: auto; font-family: monospace; font-size: 12px;",
        div(id = "launch_progress_details", "Initializing...")
      ),
      
      footer = NULL,  # No footer buttons - modal stays until complete
      easyClose = FALSE  # Cannot close by clicking outside
    ))
    
    tryCatch({
      batch_names <- c()
      remote_dirs <- c()
      current_job <- 0  # Track current job number
      progress_details <- c()  # Store progress messages
      
      for (model_name in rv$selected_models) {
        model_env <- rv$models[[model_name]]
        
        if (input$job_type == "jitter") {
          seeds <- as.numeric(strsplit(model_env$jitter_seeds, "\\s+")[[1]])
          for (seed in seeds) {
            current_job <- current_job + 1
            
            # Update progress text in modal
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_text",
                text = sprintf("Launching job %d/%d: %s (seed %d)", 
                               current_job, total_jobs, model_name, seed)
              )
            )
            
            # Update log
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("[%d/%d] 🔄 Launching: %s (seed %d)\n", 
                      current_job, total_jobs, model_name, seed)
            )
            
            # Add to progress details
            progress_details <- c(
              progress_details,
              sprintf("[%d/%d] 🔄 %s (seed %d)", 
                      current_job, total_jobs, model_name, seed)
            )
            
            # Update progress details in modal
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            result <- launch_single_job(model_name, model_env, seed = seed)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
            
            # Mark as completed
            progress_details[length(progress_details)] <- paste0(
              progress_details[length(progress_details)], " ✓"
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
            )
          }
        } else if (input$job_type == "hessian") {
          for (part in 1:as.numeric(model_env$nsplit)) {
            current_job <- current_job + 1
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_text",
                text = sprintf("Launching job %d/%d: %s (part %d)", 
                               current_job, total_jobs, model_name, part)
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("[%d/%d] 🔄 Launching: %s (part %d)\n", 
                      current_job, total_jobs, model_name, part)
            )
            
            progress_details <- c(
              progress_details,
              sprintf("[%d/%d] 🔄 %s (part %d)", 
                      current_job, total_jobs, model_name, part)
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            result <- launch_single_job(model_name, model_env, part = part)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
            
            progress_details[length(progress_details)] <- paste0(
              progress_details[length(progress_details)], " ✓"
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
            )
          }
        } else if (input$job_type == "retro") {
          peels <- as.numeric(strsplit(model_env$retro_peels, "\\s+")[[1]])
          for (peel in peels) {
            current_job <- current_job + 1
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_text",
                text = sprintf("Launching job %d/%d: %s (peel %d)", 
                               current_job, total_jobs, model_name, peel)
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("[%d/%d] 🔄 Launching: %s (peel %d)\n", 
                      current_job, total_jobs, model_name, peel)
            )
            
            progress_details <- c(
              progress_details,
              sprintf("[%d/%d] 🔄 %s (peel %d)", 
                      current_job, total_jobs, model_name, peel)
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            result <- launch_single_job(model_name, model_env, peel = peel)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
            
            progress_details[length(progress_details)] <- paste0(
              progress_details[length(progress_details)], " ✓"
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
            )
          }
        } else if (input$job_type == "prof") {
          scalers <- as.numeric(strsplit(model_env$scalers, "\\s+")[[1]])
          for (sc in scalers) {
            current_job <- current_job + 1
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_text",
                text = sprintf("Launching job %d/%d: %s (scaler %g)", 
                               current_job, total_jobs, model_name, sc)
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("[%d/%d] 🔄 Launching: %s (scaler %g)\n", 
                      current_job, total_jobs, model_name, sc)
            )
            
            progress_details <- c(
              progress_details,
              sprintf("[%d/%d] 🔄 %s (scaler %g)", 
                      current_job, total_jobs, model_name, sc)
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            result <- launch_single_job(model_name, model_env, scaler = sc)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
            
            progress_details[length(progress_details)] <- paste0(
              progress_details[length(progress_details)], " ✓"
            )
            
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "launch_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            rv$launch_log <- paste0(
              rv$launch_log,
              sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
            )
          }
        } else {
          current_job <- current_job + 1
          
          session$sendCustomMessage(
            type = "updateProgress",
            message = list(
              id = "launch_progress_text",
              text = sprintf("Launching job %d/%d: %s", 
                             current_job, total_jobs, model_name)
            )
          )
          
          rv$launch_log <- paste0(
            rv$launch_log,
            sprintf("[%d/%d] 🔄 Launching: %s\n", 
                    current_job, total_jobs, model_name)
          )
          
          progress_details <- c(
            progress_details,
            sprintf("[%d/%d] 🔄 %s", 
                    current_job, total_jobs, model_name)
          )
          
          session$sendCustomMessage(
            type = "updateProgress",
            message = list(
              id = "launch_progress_details",
              text = paste(progress_details, collapse = "<br/>")
            )
          )
          
          result <- launch_single_job(model_name, model_env)
          batch_names <- c(batch_names, result$batch_name)
          remote_dirs <- c(remote_dirs, result$remote_dir)
          
          progress_details[length(progress_details)] <- paste0(
            progress_details[length(progress_details)], " ✓"
          )
          
          session$sendCustomMessage(
            type = "updateProgress",
            message = list(
              id = "launch_progress_details",
              text = paste(progress_details, collapse = "<br/>")
            )
          )
          
          rv$launch_log <- paste0(
            rv$launch_log,
            sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
          )
        }
      }
      
      # Save job history if config file exists
      if (!is.null(rv$current_config_file)) {
        job_record <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          job_type = input$job_type,
          model_names = paste(rv$selected_models, collapse = ", "),
          output_dir = input$output_dir,
          batch_names = paste(batch_names, collapse = ", "),
          remote_dirs = paste(remote_dirs, collapse = ", "),
          branch = input$branch,
          status = "launched",
          stringsAsFactors = FALSE
        )
        save_job_history(rv$current_config_file, job_record)
        rv$launch_log <- paste0(rv$launch_log, "📝 Job history saved to config file\n")
      }
      
      # Final completion message
      rv$launch_log <- paste0(
        rv$launch_log,
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        "✅ ", Sys.time(), " - All ", total_jobs, " jobs submitted successfully!\n",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
      )
      
      # Update modal to show completion
      showModal(modalDialog(
        title = div(
          style = "font-size: 18px; font-weight: bold; color: #00a65a;",
          icon("check-circle"), " Launch Complete"
        ),
        size = "m",
        
        div(
          style = "text-align: center; margin: 20px 0;",
          h3(
            style = "color: #00a65a;",
            sprintf("✅ Successfully launched all %d jobs!", total_jobs)
          )
        ),
        
        div(
          style = "background: #f0f9f0; border: 1px solid #c3e6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
          strong("Summary:"),
          tags$ul(
            tags$li(paste("Total jobs:", total_jobs)),
            tags$li(paste("Models:", paste(rv$selected_models, collapse = ", "))),
            tags$li(paste("Output directory:", input$output_dir)),
            tags$li(paste("Branch:", input$branch))
          )
        ),
        
        footer = tagList(
          actionButton("close_launch_modal", "Close", class = "btn-success")
        )
      ))
      
    }, error = function(e) {
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
        
        footer = actionButton("close_error_modal", "Close", class = "btn-danger")
      ))
      
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
  
  
  launch_single_job <- function(model_name, model_env, seed = NULL, part = NULL, peel = NULL, scaler = NULL) {
    job_env <- model_env
    remote_dir_suffix <- model_name
    batch_suffix <- ""
    
    if (!is.null(seed)) {
      job_env$jitter_seed <- as.character(seed)
      remote_dir_suffix <- paste0(model_name, "_seed", seed)
      batch_suffix <- paste0("-jitter", seed)
    } else if (!is.null(part)) {
      job_env$hessian_part <- as.character(part)
      remote_dir_suffix <- paste0(model_name, "_part", part)
      batch_suffix <- paste0("-hess", part)
    } else if (!is.null(peel)) {
      job_env$retro_peel <- as.character(peel)
      remote_dir_suffix <- paste0(model_name, "_peel", peel)
      batch_suffix <- paste0("-retro", peel)
    } else if (!is.null(scaler)) {
      job_env$scaler <- as.character(scaler)
      remote_dir_suffix <- paste0(model_name, "_sc", scaler)
      batch_suffix <- paste0("-sc", scaler)
    }
    
    remote_dir <- paste0(input$github_repo, "/", input$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(model_name, batch_suffix, "-", format(Sys.time(), "%H:%M:%S"))
    
    rv$launch_log <- paste0(rv$launch_log, "  → ", batch_name, "\n")
    
    CondorBox::CondorBox(
      make_options = input$job_type, 
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
      ghcr_login = TRUE,
      exclude_slots = c("slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
                        "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"),
      custom_batch_name = batch_name, 
      condor_environment = job_env
    )
    
    return(list(batch_name = batch_name, remote_dir = remote_dir))
  }
  
