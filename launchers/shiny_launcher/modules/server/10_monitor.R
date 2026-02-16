  # ========== MONITOR JOBS ==========
  
  observeEvent(input$refresh_jobs, {
    showNotification("Refreshing jobs...", type = "message", duration = 1)
    
    tryCatch({
      # SSH command execution (batch format) - add -all flag if showing all users
      if (input$show_all_jobs) {
        cmd <- sprintf("ssh %s@%s 'condor_q -all'", input$remote_user, input$remote_host)
      } else {
        cmd <- sprintf("ssh %s@%s 'condor_q'", input$remote_user, input$remote_host)
      }
      
      jobs_output <- system(cmd, intern = TRUE)
      
      # Parse condor_q output
      jobs_list <- list()
      
      # Find header line starting with "OWNER   BATCH_NAME"
      header_idx <- grep("^OWNER\\s+BATCH_NAME", jobs_output)
      
      if (length(header_idx) > 0 && header_idx < length(jobs_output)) {
        # Parse from line after header
        start_idx <- header_idx + 1
        
        # Find line starting with "Total for"
        total_idx <- grep("^Total", jobs_output)
        if (length(total_idx) > 0) {
          end_idx <- total_idx[1] - 1
        } else {
          end_idx <- length(jobs_output)
        }
        
        if (start_idx <= end_idx) {
          data_lines <- jobs_output[start_idx:end_idx]
          # Remove empty lines
          data_lines <- data_lines[nzchar(trimws(data_lines))]
          
          for (line in data_lines) {
            line_trimmed <- trimws(line)
            if (nzchar(line_trimmed)) {
              # Split by whitespace
              parts <- unlist(strsplit(line_trimmed, "\\s+"))
              
              if (length(parts) >= 8) {
                # condor_q batch output format:
                # OWNER BATCH_NAME SUBMITTED DONE RUN IDLE TOTAL JOB_IDS
                jobs_list[[length(jobs_list) + 1]] <- data.frame(
                  Owner = parts[1],
                  BatchName = parts[2],
                  Submitted = paste(parts[3], parts[4]),
                  Done = ifelse(parts[5] == "_", "0", parts[5]),
                  Run = ifelse(parts[6] == "_", "0", parts[6]),
                  Idle = ifelse(parts[7] == "_", "0", parts[7]),
                  Total = parts[8],
                  JobIDs = ifelse(length(parts) >= 9, parts[9], ""),
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
      
      if (length(jobs_list) > 0) {
        rv$jobs_status <- do.call(rbind, jobs_list)
        
        msg <- if (input$show_all_jobs) {
          paste("Found", nrow(rv$jobs_status), "jobs (all users)")
        } else {
          paste("Found", nrow(rv$jobs_status), "jobs (yours)")
        }
        showNotification(msg, type = "message", duration = 2)
      } else {
        rv$jobs_status <- data.frame(
          Owner = character(),
          BatchName = character(),
          Submitted = character(),
          Done = character(),
          Run = character(),
          Idle = character(),
          Total = character(),
          JobIDs = character(),
          stringsAsFactors = FALSE
        )
        
        msg <- if (input$show_all_jobs) {
          "No jobs found (all users)"
        } else {
          "No jobs found"
        }
        showNotification(msg, type = "warning", duration = 2)
      }
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      rv$jobs_status <- data.frame(
        Owner = character(),
        BatchName = character(),
        Submitted = character(),
        Done = character(),
        Run = character(),
        Idle = character(),
        Total = character(),
        JobIDs = character(),
        stringsAsFactors = FALSE
      )
    })
  })
  
  # Auto-refresh when toggle changes
  observeEvent(input$show_all_jobs, {
    shinyjs::click("refresh_jobs")
  }, ignoreInit = TRUE)
  
  observe({
    invalidateLater(5000, session)
    if (isTRUE(input$auto_refresh_jobs) && !is.null(input$tabs) && input$tabs == "monitor") {
      shinyjs::click("refresh_jobs")
    }
  })
  
  
  output$jobs_table <- renderDT({
    datatable(
      rv$jobs_status, 
      selection = 'multiple',
      options = list(
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  output$job_details <- renderPrint({
    if (nrow(rv$jobs_status) == 0) {
      cat("No jobs found. Click 'Refresh' to check job status.\n")
      return()
    }
    
    # DEBUG: Print raw data structure
    #cat("========== DEBUG INFO ==========\n\n")
    #cat("Structure of rv$jobs_status:\n")
    #print(str(rv$jobs_status))
    #cat("\n")
    #cat("First few rows:\n")
    #print(head(rv$jobs_status, 3))
    #cat("\n")
    #cat("Column classes:\n")
    #print(sapply(rv$jobs_status, class))
    #cat("\n================================\n\n")
    
    # Get full condor_q output
    tryCatch({
      if (input$show_all_jobs) {
        cmd <- sprintf("ssh %s@%s 'condor_q -all'", 
                       input$remote_user, 
                       input$remote_host)
      } else {
        cmd <- sprintf("ssh %s@%s 'condor_q'", 
                       input$remote_user, 
                       input$remote_host)
      }
      
      result <- system(cmd, intern = TRUE)
      
      if (length(result) == 0) {
        cat("No jobs currently in queue.\n")
        return()
      }
      
      # Extract summary lines
      summary_lines <- grep("^Total for", result, value = TRUE)
      
      if (length(summary_lines) > 0) {
        cat("========== Condor Queue Summary ==========\n\n")
        for (line in summary_lines) {
          cat(line, "\n")
        }
        cat("\n==========================================\n\n")
      }
      
      # Simple user count without aggregation
      if (nrow(rv$jobs_status) > 0) {
        cat("========== Jobs by User ==========\n\n")
        
        # Get unique owners
        owners <- unique(rv$jobs_status$Owner)
        my_user <- input$remote_user
        
        for (owner in owners) {
          # Filter jobs for this owner
          owner_jobs <- rv$jobs_status[rv$jobs_status$Owner == owner, ]
          
          # Count jobs
          n_jobs <- nrow(owner_jobs)
          
          # Mark current user
          if (owner == my_user) {
            cat(sprintf("➤ %s (YOU):\n", owner))
          } else {
            cat(sprintf("  %s:\n", owner))
          }
          
          cat(sprintf("    %d job(s) in queue\n", n_jobs))
          
          # Show batch names
          batch_names <- owner_jobs$BatchName
          if (length(batch_names) > 0) {
            cat(sprintf("    Batches: %s\n", paste(batch_names, collapse = ", ")))
          }
          
          cat("\n")
        }
        
        cat("======================================\n\n")
      }
      
      # Show selected job info
      selected_jobs <- input$jobs_table_rows_selected
      
      if (!is.null(selected_jobs) && length(selected_jobs) > 0) {
        cat("========== Selected Jobs ==========\n\n")
        
        for (i in selected_jobs) {
          job <- rv$jobs_status[i, ]
          cat(sprintf("• %s\n", job$BatchName))
          cat(sprintf("  Owner: %s | Submitted: %s\n", job$Owner, job$Submitted))
          cat(sprintf("  Status: %s done, %s running, %s idle (%s total)\n",
                      job$Done, job$Run, job$Idle, job$Total))
          
          if (!is.na(job$JobIDs) && job$JobIDs != "") {
            cat(sprintf("  Job IDs: %s\n", job$JobIDs))
          }
          cat("\n")
        }
      } else {
        cat("💡 Tip: Select a job from the table above to see details\n")
      }
      
    }, error = function(e) {
      cat(sprintf("❌ Error: %s\n", e$message))
    })
  })
  
  
  
  
  output$selected_jobs_info <- renderText({
    selected_rows <- input$jobs_table_rows_selected
    if (length(selected_rows) > 0) {
      paste(length(selected_rows), "job(s) selected")
    } else {
      "No jobs selected"
    }
  })
  
  observeEvent(input$remove_selected_jobs, {
    selected_rows <- input$jobs_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("No jobs selected", type = "warning")
      return()
    }
    
    selected_jobs <- rv$jobs_status[selected_rows, ]
    
    # Extract job IDs
    job_ids <- selected_jobs$JobIDs
    
    showModal(modalDialog(
      title = "Confirm Job Removal",
      size = "m",
      p(strong(paste("Remove", length(selected_rows), "job(s)?")), style = "margin-bottom: 15px;"),
      div(
        style = "max-height: 300px; overflow-y: auto; background: #f9f9f9; padding: 10px; border: 1px solid #ddd; border-radius: 4px;",
        tags$table(
          style = "width: 100%; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("Batch Name", style = "text-align: left; padding: 5px;"),
              tags$th("Job ID", style = "text-align: left; padding: 5px;"),
              tags$th("Status", style = "text-align: center; padding: 5px;")
            )
          ),
          tags$tbody(
            lapply(1:nrow(selected_jobs), function(i) {
              tags$tr(
                tags$td(selected_jobs$BatchName[i], style = "padding: 5px;"),
                tags$td(selected_jobs$JobIDs[i], style = "padding: 5px;"),
                tags$td(paste0("Run: ", selected_jobs$Run[i], " | Idle: ", selected_jobs$Idle[i]), 
                        style = "padding: 5px; text-align: center;")
              )
            })
          )
        )
      ),
      shiny::hr(),
      p(strong("Warning:"), "This will remove the jobs from the Condor queue.", 
        style = "color: #d9534f; margin-top: 15px;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_jobs", "Remove Jobs", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_remove_jobs, {
    selected_rows <- input$jobs_table_rows_selected
    if (length(selected_rows) == 0) {
      removeModal()
      return()
    }
    
    selected_jobs <- rv$jobs_status[selected_rows, ]
    job_ids <- selected_jobs$JobIDs
    
    removeModal()
    
    # Disable remove button during processing
    shinyjs::disable("remove_selected_jobs")
    
    # Count total jobs to remove
    total_to_remove <- sum(sapply(job_ids, function(x) {
      if (nzchar(x) && !is.na(x)) {
        length(strsplit(x, ",")[[1]])
      } else {
        0
      }
    }))
    
    # Show persistent modal dialog with progress
    showModal(modalDialog(
      title = div(
        style = "font-size: 18px; font-weight: bold;",
        icon("trash"), " Deleting Jobs"
      ),
      size = "m",
      
      # Progress indicator
      div(
        style = "text-align: center; margin: 20px 0;",
        div(class = "spinner"),
        h4(
          id = "delete_progress_text",
          style = "color: #dd4b39; margin-top: 20px;",
          paste0("Starting... (0/", total_to_remove, ")")
        )
      ),
      
      # Progress details box
      div(
        style = "background: #f9f9f9; border: 1px solid #ddd; border-radius: 4px; padding: 15px; max-height: 300px; overflow-y: auto; font-family: monospace; font-size: 12px;",
        div(id = "delete_progress_details", "Initializing...")
      ),
      
      footer = NULL,  # No footer buttons - modal stays until complete
      easyClose = FALSE  # Cannot close by clicking outside
    ))
    
    tryCatch({
      removed_count <- 0
      failed_count <- 0
      current_job <- 0
      progress_details <- c()  # Store progress messages
      
      for (jobid in job_ids) {
        if (nzchar(jobid) && !is.na(jobid)) {
          # Handle multiple job IDs (comma-separated)
          individual_ids <- strsplit(jobid, ",")[[1]]
          
          for (single_id in individual_ids) {
            single_id <- trimws(single_id)
            current_job <- current_job + 1
            
            # Update progress text in modal
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "delete_progress_text",
                text = sprintf("Deleting job %d/%d (ID: %s)", 
                               current_job, total_to_remove, single_id)
              )
            )
            
            # Add to progress details
            progress_details <- c(
              progress_details,
              sprintf("[%d/%d] 🗑️ Job ID: %s", 
                      current_job, total_to_remove, single_id)
            )
            
            # Update progress details in modal
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "delete_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
            
            # Remove job using condor_rm
            cmd <- sprintf("ssh %s@%s 'condor_rm %s'", 
                           input$remote_user, input$remote_host, single_id)
            result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
            
            if (length(result) == 0 || !grepl("ERROR|Error", result[1])) {
              removed_count <- removed_count + 1
              progress_details[length(progress_details)] <- paste0(
                progress_details[length(progress_details)], " ✓"
              )
            } else {
              failed_count <- failed_count + 1
              progress_details[length(progress_details)] <- paste0(
                progress_details[length(progress_details)], " ❌"
              )
            }
            
            # Update with success/failure marker
            session$sendCustomMessage(
              type = "updateProgress",
              message = list(
                id = "delete_progress_details",
                text = paste(progress_details, collapse = "<br/>")
              )
            )
          }
        }
      }
      
      # Show completion modal
      showModal(modalDialog(
        title = div(
          style = sprintf("font-size: 18px; font-weight: bold; color: %s;",
                          ifelse(failed_count == 0, "#00a65a", "#f39c12")),
          icon(ifelse(failed_count == 0, "check-circle", "exclamation-triangle")), 
          " Deletion Complete"
        ),
        size = "m",
        
        div(
          style = "text-align: center; margin: 20px 0;",
          h3(
            style = sprintf("color: %s;", ifelse(failed_count == 0, "#00a65a", "#f39c12")),
            sprintf("Deleted %d/%d jobs", removed_count, total_to_remove)
          )
        ),
        
        div(
          style = sprintf("background: %s; border: 1px solid %s; border-radius: 4px; padding: 15px; margin: 15px 0;",
                          ifelse(failed_count == 0, "#f0f9f0", "#fff3cd"),
                          ifelse(failed_count == 0, "#c3e6cb", "#ffc107")),
          tags$ul(
            tags$li(paste("✅ Successfully removed:", removed_count)),
            if (failed_count > 0) tags$li(paste("❌ Failed to remove:", failed_count))
          )
        ),
        
        footer = actionButton("close_delete_modal", "Close", 
                              class = ifelse(failed_count == 0, "btn-success", "btn-warning"))
      ))
      
      # Refresh job list after a short delay
      Sys.sleep(1)
      shinyjs::click("refresh_jobs")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = div(
          style = "font-size: 18px; font-weight: bold; color: #dd4b39;",
          icon("times-circle"), " Deletion Failed"
        ),
        size = "m",
        
        div(
          style = "background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
          h4(style = "color: #721c24;", "Error occurred during job deletion:"),
          p(style = "font-family: monospace; color: #721c24;", e$message)
        ),
        
        footer = actionButton("close_delete_error_modal", "Close", class = "btn-danger")
      ))
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("remove_selected_jobs")
    })
  })
  
  # Handler to close delete completion modal
  observeEvent(input$close_delete_modal, {
    removeModal()
  })
  
  # Handler to close delete error modal
  observeEvent(input$close_delete_error_modal, {
    removeModal()
  })
  
