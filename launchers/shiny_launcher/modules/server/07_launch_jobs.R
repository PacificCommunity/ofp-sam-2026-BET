  # ========== LAUNCH JOB HANDLERS ==========
  
  observeEvent(input$launch_btn, {
    if (length(rv$models) == 0) { 
      showNotification("Please load models first", type = "error")
      return() 
    }
    
    # Recompute selected models from current checkbox inputs
    selected_models <- names(rv$models)[sapply(names(rv$models), function(model_name) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      isTRUE(input[[checkbox_id]])
    })]
    rv$selected_models <- selected_models
    
    # Validate model selection
    if (length(selected_models) == 0) { 
      showNotification("Please select at least one model", type = "error")
      return() 
    }
    
    # Disable button during processing
    shinyjs::disable("launch_btn")
    shinyjs::addClass("launch_btn", "loading")
    
    selected_job_types <- input$job_types
    if (is.null(selected_job_types) || length(selected_job_types) == 0) {
      showNotification("Please select at least one job type", type = "error")
      shinyjs::enable("launch_btn")
      shinyjs::removeClass("launch_btn", "loading")
      return()
    }
    
    # Calculate total number of jobs to be launched
    total_jobs <- 0
    job_specs <- list()
    add_job_spec <- function(model_name, job_type, seed = NULL, part = NULL, peel = NULL, scaler = NULL) {
      job_specs[[length(job_specs) + 1]] <<- list(
        model_name = model_name,
        job_type = job_type,
        seed = seed,
        part = part,
        peel = peel,
        scaler = scaler
      )
    }
    for (model_name in selected_models) {
      model_env <- rv$models[[model_name]]
      for (job_type in selected_job_types) {
        if (job_type == "jitter") {
          seeds <- as.numeric(strsplit(model_env$jitter_seeds, "\\s+")[[1]])
          total_jobs <- total_jobs + length(seeds)
          for (seed in seeds) {
            add_job_spec(model_name, job_type, seed = seed)
          }
        } else if (job_type == "hessian") {
          total_jobs <- total_jobs + as.numeric(model_env$nsplit)
          for (part in 1:as.numeric(model_env$nsplit)) {
            add_job_spec(model_name, job_type, part = part)
          }
        } else if (job_type == "retro") {
          peels <- as.numeric(strsplit(model_env$retro_peels, "\\s+")[[1]])
          total_jobs <- total_jobs + length(peels)
          for (peel in peels) {
            add_job_spec(model_name, job_type, peel = peel)
          }
        } else if (job_type == "prof") {
          scalers <- as.numeric(strsplit(model_env$scalers, "\\s+")[[1]])
          total_jobs <- total_jobs + length(scalers)
          for (sc in scalers) {
            add_job_spec(model_name, job_type, scaler = sc)
          }
        } else {
          total_jobs <- total_jobs + 1
          add_job_spec(model_name, job_type)
        }
      }
    }
    
    model_env_lists <- lapply(selected_models, function(m) {
      as.list(rv$models[[m]], all.names = TRUE)
    })
    names(model_env_lists) <- selected_models

    # Initialize log with total job count
    rv$launch_log <- paste0(
      Sys.time(), " - Starting job submission...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📊 Total jobs to launch: ", total_jobs, "\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    cancel_launch <- reactiveVal(FALSE)

    update_launch_notification <- function(text, type = "message") {
      showNotification(text, type = type, duration = NULL, id = "launch_progress")
    }

    update_launch_notification(sprintf("Launching jobs... (0/%d)", total_jobs))
    
    tryCatch({
      batch_names <- c()
      remote_dirs <- c()
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
      
      did_parallel <- FALSE
      if (isTRUE(input$parallel_launch) && length(job_specs) > 1) {
        max_cores <- max(1, parallel::detectCores() - 2)
        cores <- max(1, min(as.integer(input$launch_parallel_cores), max_cores))
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
          rv$launch_log <- paste0(rv$launch_log, "⚡ Parallel launch ON (cores: ", cores, ")\n")
          
          update_launch_notification(
            sprintf("Launching %d jobs in parallel (cores: %d)", total_jobs, cores)
          )
          
          if (cancel_launch()) stop("Launch cancelled")
          
          common_params <- list(
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
            exclude_slots = c(
              "slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
              "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"
            )
          )
          
          results <- tryCatch({
            cl <- parallel::makeCluster(cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)
            parallel::clusterEvalQ(cl, { library(CondorBox) })
            parallel::clusterExport(cl, varlist = c("launch_single_job_raw", "common_params"), envir = environment())
            parallel::parLapply(cl, job_specs, function(spec) {
              tryCatch({
                launch_single_job_raw(spec, common_params)
              }, error = function(e) {
                list(batch_name = NA_character_, remote_dir = NA_character_, job_id = NA_character_, error = e$message)
              })
            })
          }, error = function(e) {
            rv$launch_log <- paste0(rv$launch_log, "⚠️ Parallel launch failed: ", e$message, "\n")
            NULL
          })
          
          if (!is.null(results)) {
            ok_mask <- vapply(results, function(x) is.null(x$error), logical(1))
            batch_names <- vapply(results[ok_mask], function(x) x$batch_name, character(1))
            remote_dirs <- vapply(results[ok_mask], function(x) x$remote_dir, character(1))
            current_job <- sum(ok_mask)
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
          model_env <- rv$models[[model_name]]
          
          for (job_type in selected_job_types) {
            if (cancel_launch()) stop("Launch cancelled")
            
            if (job_type == "jitter") {
              seeds <- as.numeric(strsplit(model_env$jitter_seeds, "\\s+")[[1]])
              for (seed in seeds) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("Launching job %d/%d: %s (seed %d)",
                          current_job, total_jobs, model_name, seed)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 Launching: %s (seed %d)\n", 
                          current_job, total_jobs, model_name, seed)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (seed %d)", 
                          current_job, total_jobs, model_name, seed)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, seed = seed)
                batch_names <- c(batch_names, result$batch_name)
                remote_dirs <- c(remote_dirs, result$remote_dir)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "hessian") {
              for (part in 1:as.numeric(model_env$nsplit)) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("Launching job %d/%d: %s (part %d)",
                          current_job, total_jobs, model_name, part)
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
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, part = part)
                batch_names <- c(batch_names, result$batch_name)
                remote_dirs <- c(remote_dirs, result$remote_dir)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "retro") {
              peels <- as.numeric(strsplit(model_env$retro_peels, "\\s+")[[1]])
              for (peel in peels) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("Launching job %d/%d: %s (peel %d)",
                          current_job, total_jobs, model_name, peel)
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
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, peel = peel)
                batch_names <- c(batch_names, result$batch_name)
                remote_dirs <- c(remote_dirs, result$remote_dir)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "prof") {
              scalers <- as.numeric(strsplit(model_env$scalers, "\\s+")[[1]])
              for (sc in scalers) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("Launching job %d/%d: %s (scaler %g)",
                          current_job, total_jobs, model_name, sc)
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
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, scaler = sc)
                batch_names <- c(batch_names, result$batch_name)
                remote_dirs <- c(remote_dirs, result$remote_dir)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else {
              if (cancel_launch()) stop("Launch cancelled")
              current_job <- current_job + 1
              
              update_launch_notification(
                sprintf("Launching job %d/%d: %s",
                        current_job, total_jobs, model_name)
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
              
              result <- launch_single_job(model_name, model_env, job_type = job_type)
              batch_names <- c(batch_names, result$batch_name)
              remote_dirs <- c(remote_dirs, result$remote_dir)
              
              progress_details[length(progress_details)] <- paste0(
                progress_details[length(progress_details)], " ✓"
              )
              maybe_send_progress_details()
              
              rv$launch_log <- paste0(
                rv$launch_log,
                sprintf("  ✓ Submitted: %s\n\n", result$batch_name)
              )
            }
          }
        }
      }
      
      # Save job history if config file exists
      if (!is.null(rv$current_config_file)) {
        job_record <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          job_type = paste(selected_job_types, collapse = ", "),
          model_names = paste(selected_models, collapse = ", "),
          output_dir = input$output_dir,
          batch_names = paste(batch_names, collapse = ", "),
          remote_dirs = paste(remote_dirs, collapse = ", "),
          branch = input$branch,
          status = if (cancel_launch()) "cancelled" else "launched",
          stringsAsFactors = FALSE
        )
        save_job_history(rv$current_config_file, job_record)
        rv$launch_log <- paste0(rv$launch_log, "📝 Job history saved to config file\n")
      }
      
      if (cancel_launch()) {
        removeNotification("launch_progress")
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "⚠️ ", Sys.time(), " - Launch cancelled by user after ", current_job, " submissions\n",
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
              sprintf("⚠️ Cancelled after %d job(s) submitted.", current_job)
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
              tags$li(paste("Models:", paste(selected_models, collapse = ", "))),
              tags$li(paste("Output directory:", input$output_dir)),
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
          "⚠️ ", Sys.time(), " - Launch cancelled by user after ", current_job, " submissions\n",
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
              sprintf("⚠️ Cancelled after %d job(s) submitted.", current_job)
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
    model_env_list <- common_params$model_env_lists[[spec$model_name]]
    if (is.null(model_env_list)) {
      stop(paste("Model env not found for", spec$model_name))
    }
    job_env <- list2env(model_env_list, parent = emptyenv())
    job_env$DOCKER_IMAGE <- common_params$docker_image
    remote_dir_suffix <- spec$model_name
    batch_suffix <- ""
    
    if (!is.null(spec$seed)) {
      job_env$jitter_seed <- as.character(spec$seed)
      remote_dir_suffix <- paste0(spec$model_name, "_seed", spec$seed)
      batch_suffix <- paste0("-jitter", spec$seed)
    } else if (!is.null(spec$part)) {
      job_env$hessian_part <- as.character(spec$part)
      remote_dir_suffix <- paste0(spec$model_name, "_part", spec$part)
      batch_suffix <- paste0("-hess", spec$part)
    } else if (!is.null(spec$peel)) {
      job_env$retro_peel <- as.character(spec$peel)
      remote_dir_suffix <- paste0(spec$model_name, "_peel", spec$peel)
      batch_suffix <- paste0("-retro", spec$peel)
    } else if (!is.null(spec$scaler)) {
      job_env$scaler <- as.character(spec$scaler)
      remote_dir_suffix <- paste0(spec$model_name, "_sc", spec$scaler)
      batch_suffix <- paste0("-sc", spec$scaler)
    } else {
      # model job only
      remote_dir_suffix <- paste0(spec$model_name, "_model")
    }
    
    remote_dir <- paste0(common_params$github_repo, "/", common_params$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(
      spec$model_name,
      batch_suffix,
      "-",
      format(Sys.time(), "%H:%M:%S"),
      "-",
      Sys.getpid()
    )
    
    work_dir <- file.path(
      tempdir(),
      paste0("condorbox_", Sys.getpid(), "_", gsub("[^a-zA-Z0-9]", "_", spec$model_name))
    )
    if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)
    old_wd <- getwd()
    setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)
    
    job_id <- CondorBox::CondorBox(
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
      condor_environment = as.list(job_env, all.names = TRUE)
    )
    
    return(list(batch_name = batch_name, remote_dir = remote_dir, job_id = job_id))
  }
  
  launch_single_job <- function(model_name, model_env, job_type, seed = NULL, part = NULL, peel = NULL, scaler = NULL, log = TRUE) {
    job_env <- model_env
    job_env$DOCKER_IMAGE <- input$docker_image
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
    } else {
      # model job only
      remote_dir_suffix <- paste0(model_name, "_model")
    }
    
    remote_dir <- paste0(input$github_repo, "/", input$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(model_name, batch_suffix, "-", format(Sys.time(), "%H:%M:%S"))
    
    if (isTRUE(log)) {
      rv$launch_log <- paste0(rv$launch_log, "  → ", batch_name, "\n")
    }
    
    job_id <- CondorBox::CondorBox(
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
      exclude_slots = c("slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
                        "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"),
      custom_batch_name = batch_name, 
      condor_environment = as.list(job_env, all.names = TRUE)
    )
    
    return(list(batch_name = batch_name, remote_dir = remote_dir, job_id = job_id))
  }
  
