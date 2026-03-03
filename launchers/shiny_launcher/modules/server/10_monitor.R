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
    jobs_df <- rv$jobs_status

    if (nrow(jobs_df) > 0) {
      jobs_df$Select <- sprintf(
        '<input type="checkbox" class="job-select-checkbox" value="%s"/>',
        seq_len(nrow(jobs_df))
      )
      jobs_df <- jobs_df[, c("Select", setdiff(names(jobs_df), "Select")), drop = FALSE]
    }

    datatable(
      jobs_df,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(orderable = FALSE, targets = 0)
        )
      ),
      callback = JS(
        "var checkedMap = {};",
        "var syncCheckedToShiny = function() {",
        "  var checked = Object.keys(checkedMap)",
        "    .filter(function(key) { return checkedMap[key]; })",
        "    .map(function(key) { return parseInt(key, 10); })",
        "    .sort(function(a, b) { return a - b; });",
        "  Shiny.setInputValue('jobs_checked_rows', checked, {priority: 'event'});",
        "};",
        "var restoreCheckedState = function() {",
        "  table.$('.job-select-checkbox', {page: 'current'}).each(function(){",
        "    this.checked = !!checkedMap[this.value];",
        "  });",
        "};",
        "var updateChecked = function() {",
        "  restoreCheckedState();",
        "  syncCheckedToShiny();",
        "};",
        "table.on('change', '.job-select-checkbox', function() {",
        "  checkedMap[this.value] = this.checked;",
        "  syncCheckedToShiny();",
        "});",
        "restoreCheckedState();",
        "syncCheckedToShiny();",
        "table.on('draw.dt', updateChecked);"
      ),
      class = 'cell-border stripe'
    )
  })

  observe({
    jobs_df <- rv$jobs_status
    if (is.null(jobs_df) || nrow(jobs_df) == 0) {
      updateSelectInput(session, "job_detail_batch", choices = character(0), selected = character(0))
      return()
    }

    detail_keys <- paste(jobs_df$Owner, jobs_df$BatchName, sep = "::")
    choices <- stats::setNames(
      detail_keys,
      paste0(jobs_df$BatchName, " (", jobs_df$Owner, ")")
    )

    current <- isolate(input$job_detail_batch)
    selected <- if (!is.null(current) && nzchar(current) && current %in% detail_keys) {
      current
    } else {
      detail_keys[[1]]
    }

    updateSelectInput(session, "job_detail_batch", choices = choices, selected = selected)
  })
  
  output$job_details <- renderPrint({
    if (nrow(rv$jobs_status) == 0) {
      cat("No jobs found. Click 'Refresh' to check job status.\n")
      return()
    }

    tryCatch({
      cat("========== Jobs by User ==========\n\n")

      owners <- unique(rv$jobs_status$Owner)
      my_user <- input$remote_user

      for (owner in owners) {
        owner_jobs <- rv$jobs_status[rv$jobs_status$Owner == owner, , drop = FALSE]
        n_jobs <- nrow(owner_jobs)

        if (owner == my_user) {
          cat(sprintf("➤ %s (YOU):\n", owner))
        } else {
          cat(sprintf("  %s:\n", owner))
        }

        cat(sprintf("    %d job(s) in last refresh\n", n_jobs))
        batch_names <- owner_jobs$BatchName
        if (length(batch_names) > 0) {
          cat(sprintf("    Batches: %s\n", paste(batch_names, collapse = ", ")))
        }

        cat("\n")
      }

      cat("==================================\n\n")

      selected_batch <- input$job_detail_batch
      detail_keys <- paste(rv$jobs_status$Owner, rv$jobs_status$BatchName, sep = "::")

      if (!is.null(selected_batch) && nzchar(selected_batch) && selected_batch %in% detail_keys) {
        cat("========== Selected Job ==========\n\n")
        selected_jobs <- rv$jobs_status[detail_keys == selected_batch, , drop = FALSE]
        
        for (i in seq_len(nrow(selected_jobs))) {
          job <- selected_jobs[i, ]
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
        cat("💡 Tip: Choose a job from the dropdown above to see details\n")
      }

    }, error = function(e) {
      cat(sprintf("❌ Error: %s\n", e$message))
    })
  })
  
  
  
  
  output$selected_jobs_info <- renderText({
    selected_rows <- input$jobs_checked_rows
    if (length(selected_rows) > 0) {
      paste(length(selected_rows), "job(s) selected")
    } else {
      "No jobs selected"
    }
  })
  
  observeEvent(input$remove_selected_jobs, {
    selected_rows <- input$jobs_checked_rows
    
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
    selected_rows <- input$jobs_checked_rows
    if (length(selected_rows) == 0) {
      removeModal()
      return()
    }
    
    selected_jobs <- rv$jobs_status[selected_rows, ]
    job_ids <- selected_jobs$JobIDs
    
    removeModal()
    
    # Disable remove button during processing
    shinyjs::disable("remove_selected_jobs")

    update_delete_notification <- function(text, type = "message") {
      showNotification(text, type = type, duration = NULL, id = "delete_progress")
    }
    
    # Count total jobs to remove
    total_to_remove <- sum(sapply(job_ids, function(x) {
      if (nzchar(x) && !is.na(x)) {
        length(strsplit(x, ",")[[1]])
      } else {
        0
      }
    }))
    
    update_delete_notification(sprintf("Deleting jobs... (0/%d)", total_to_remove))
    
    tryCatch({
      all_ids <- unique(trimws(unlist(strsplit(paste(job_ids[nzchar(job_ids) & !is.na(job_ids)], collapse = ","), ","))))
      all_ids <- all_ids[nzchar(all_ids)]

      if (length(all_ids) == 0) {
        stop("No valid job IDs found")
      }

      update_delete_notification(
        sprintf("Deleting %d jobs...", length(all_ids))
      )

      cmd <- sprintf(
        "ssh %s@%s 'condor_rm %s'",
        input$remote_user,
        input$remote_host,
        paste(all_ids, collapse = " ")
      )
      result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)

      result_text <- paste(result, collapse = "\n")
      has_error <- grepl("ERROR|Error|Failed|not found", result_text)
      removed_count <- if (has_error) 0 else length(all_ids)
      failed_count <- if (has_error) length(all_ids) else 0

      removeNotification("delete_progress")

      removed_rows <- sort(unique(selected_rows[selected_rows <= nrow(rv$jobs_status)]))
      if (length(removed_rows) > 0) {
        rv$jobs_status <- rv$jobs_status[-removed_rows, , drop = FALSE]
      }

      showNotification(
        sprintf(
          "Deletion complete: %d/%d removed%s. Current table updated locally.",
          removed_count,
          total_to_remove,
          if (failed_count > 0) sprintf(", %d failed", failed_count) else ""
        ),
        type = if (failed_count > 0) "warning" else "message",
        duration = 5
      )
      
    }, error = function(e) {
      removeNotification("delete_progress")
      showNotification(
        paste("Deletion failed:", e$message),
        type = "error",
        duration = 6
      )
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("remove_selected_jobs")
    })
  })
  
