  # ========== MONITOR JOBS ==========

  has_refreshed_jobs <- reactiveVal(FALSE)

  empty_jobs_status <- function() {
    data.frame(
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
    )
  }

  split_csv_values <- function(x) {
    vals <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
    vals[nzchar(vals)]
  }

  build_batch_metadata <- function() {
    log_df <- tryCatch(load_launcher_job_log(), error = function(e) NULL)
    if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0) {
      return(data.frame(
        BatchName = character(),
        OutputDir = character(),
        RunDescription = character(),
        stringsAsFactors = FALSE
      ))
    }

    if (!"batch_names" %in% names(log_df)) return(data.frame(
      BatchName = character(),
      OutputDir = character(),
      RunDescription = character(),
      stringsAsFactors = FALSE
    ))

    run_at <- if ("run_at" %in% names(log_df)) as.character(log_df$run_at) else rep("", nrow(log_df))
    run_at_ts <- suppressWarnings(as.POSIXct(run_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    run_at_num <- suppressWarnings(as.numeric(run_at_ts))
    run_at_num[!is.finite(run_at_num)] <- -Inf
    ord <- order(run_at_num, decreasing = TRUE)

    rows <- list()
    for (i in ord) {
      batches <- split_csv_values(log_df$batch_names[i])
      if (length(batches) == 0) next
      output_dir <- if ("output_dir" %in% names(log_df)) as.character(log_df$output_dir[i]) else "NA"
      run_desc <- if ("run_description" %in% names(log_df)) as.character(log_df$run_description[i]) else "NA"
      if (!nzchar(trimws(output_dir))) output_dir <- "NA"
      if (!nzchar(trimws(run_desc))) run_desc <- "NA"
      for (b in batches) {
        rows[[length(rows) + 1L]] <- data.frame(
          BatchName = b,
          OutputDir = output_dir,
          RunDescription = run_desc,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0) {
      return(data.frame(
        BatchName = character(),
        OutputDir = character(),
        RunDescription = character(),
        stringsAsFactors = FALSE
      ))
    }

    out <- do.call(rbind, rows)
    out <- out[!duplicated(out$BatchName), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  enrich_jobs_status <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_jobs_status())
    meta <- build_batch_metadata()
    if (nrow(meta) == 0) {
      df$OutputDir <- rep("NA", nrow(df))
      df$RunDescription <- rep("NA", nrow(df))
      return(df)
    }
    idx <- match(df$BatchName, meta$BatchName)
    df$OutputDir <- meta$OutputDir[idx]
    df$RunDescription <- meta$RunDescription[idx]
    df$OutputDir[is.na(df$OutputDir) | !nzchar(trimws(df$OutputDir))] <- "NA"
    df$RunDescription[is.na(df$RunDescription) | !nzchar(trimws(df$RunDescription))] <- "NA"
    df
  }

  filtered_jobs_df <- reactive({
    df <- rv$jobs_status
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_jobs_status())
    if (!"OutputDir" %in% names(df)) df$OutputDir <- "NA"
    if (!"RunDescription" %in% names(df)) df$RunDescription <- "NA"
    df$.row_id <- seq_len(nrow(df))
    selected_output <- input$monitor_output_dir_filter
    if (!is.null(selected_output) && nzchar(selected_output) && selected_output != "__all__") {
      df <- df[df$OutputDir == selected_output, , drop = FALSE]
    }
    df
  })
  
  observeEvent(input$refresh_jobs, {
    showNotification("Refreshing jobs...", type = "message", duration = 1)
    has_refreshed_jobs(TRUE)
    
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
        rv$jobs_status <- enrich_jobs_status(do.call(rbind, jobs_list))
        
        msg <- if (input$show_all_jobs) {
          paste("Found", nrow(rv$jobs_status), "jobs (all users)")
        } else {
          paste("Found", nrow(rv$jobs_status), "jobs (yours)")
        }
        showNotification(msg, type = "message", duration = 2)
      } else {
        rv$jobs_status <- empty_jobs_status()
        
        msg <- if (input$show_all_jobs) {
          "No jobs found (all users)"
        } else {
          "No jobs found"
        }
        showNotification(msg, type = "warning", duration = 2)
      }
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      rv$jobs_status <- empty_jobs_status()
    })
  })
  
  # Auto-refresh when toggle changes
  observeEvent(input$show_all_jobs, {
    shinyjs::click("refresh_jobs")
  }, ignoreInit = TRUE)

  observeEvent(rv$launcher_job_log_trigger, {
    if (is.null(rv$jobs_status) || !is.data.frame(rv$jobs_status) || nrow(rv$jobs_status) == 0) return()
    rv$jobs_status <- enrich_jobs_status(rv$jobs_status[, intersect(
      c("Owner", "BatchName", "Submitted", "Done", "Run", "Idle", "Total", "JobIDs"),
      names(rv$jobs_status)
    ), drop = FALSE])
  }, ignoreInit = TRUE)
  
  observe({
    jobs_df <- rv$jobs_status
    if (is.null(jobs_df) || !is.data.frame(jobs_df) || nrow(jobs_df) == 0) {
      updateSelectInput(
        session,
        "monitor_output_dir_filter",
        choices = c("All output directories" = "__all__"),
        selected = "__all__"
      )
      return()
    }

    output_dirs <- unique(as.character(jobs_df$OutputDir))
    output_dirs <- sort(output_dirs[nzchar(output_dirs) & output_dirs != "NA"])
    choices <- c("All output directories" = "__all__")
    if (length(output_dirs) > 0) choices <- c(choices, stats::setNames(output_dirs, output_dirs))

    current <- isolate(input$monitor_output_dir_filter)
    selected <- if (!is.null(current) && current %in% names(choices)) current else "__all__"
    updateSelectInput(session, "monitor_output_dir_filter", choices = choices, selected = selected)
  })

  output$monitor_run_description <- renderUI({
    jobs_df <- rv$jobs_status
    if (is.null(jobs_df) || !is.data.frame(jobs_df) || nrow(jobs_df) == 0) {
      return(tags$span(style = "color:#666;", "Run Description: NA"))
    }

    selected_output <- input$monitor_output_dir_filter
    if (is.null(selected_output) || !nzchar(selected_output) || selected_output == "__all__") {
      return(tags$span(style = "color:#666;", "Run Description: choose an output directory to view"))
    }

    subset_df <- jobs_df[jobs_df$OutputDir == selected_output, , drop = FALSE]
    if (nrow(subset_df) == 0) {
      return(tags$span(style = "color:#666;", "Run Description: NA"))
    }

    desc_vals <- unique(trimws(as.character(subset_df$RunDescription)))
    desc_vals <- desc_vals[nzchar(desc_vals) & desc_vals != "NA"]
    if (length(desc_vals) == 0) {
      tags$span(style = "color:#666;", "Run Description: NA")
    } else {
      tags$span(
        tags$b("Run Description: "),
        paste(desc_vals, collapse = " | ")
      )
    }
  })

  output$monitor_progress_summary <- renderUI({
    if (!isTRUE(has_refreshed_jobs())) {
      return(tags$span(style = "color:#777; font-size:12px;", "Progress: click Refresh to load current queue status"))
    }

    jobs_df <- filtered_jobs_df()
    as_int <- function(x) {
      y <- suppressWarnings(as.integer(as.character(x)))
      y[!is.finite(y)] <- 0L
      y
    }

    run_n <- if (!is.null(jobs_df) && is.data.frame(jobs_df) && nrow(jobs_df) > 0) sum(as_int(jobs_df$Run), na.rm = TRUE) else 0L
    idle_n <- if (!is.null(jobs_df) && is.data.frame(jobs_df) && nrow(jobs_df) > 0) sum(as_int(jobs_df$Idle), na.rm = TRUE) else 0L
    queue_total_n <- if (!is.null(jobs_df) && is.data.frame(jobs_df) && nrow(jobs_df) > 0) sum(as_int(jobs_df$Total), na.rm = TRUE) else 0L

    selected_output <- input$monitor_output_dir_filter
    log_df <- tryCatch(load_launcher_job_log(), error = function(e) NULL)
    expected_total <- NA_integer_

    if (!is.null(log_df) && is.data.frame(log_df) && nrow(log_df) > 0 &&
        "total_jobs" %in% names(log_df) && "batch_names" %in% names(log_df)) {
      # Restrict to selected output directory when chosen.
      if (!is.null(selected_output) && nzchar(selected_output) && selected_output != "__all__" &&
          "output_dir" %in% names(log_df)) {
        keep_out <- trimws(as.character(log_df$output_dir)) == selected_output
        log_df <- log_df[keep_out, , drop = FALSE]
      }

      if (nrow(log_df) > 0) {
        current_batches <- if (!is.null(jobs_df) && is.data.frame(jobs_df) && nrow(jobs_df) > 0) unique(as.character(jobs_df$BatchName)) else character(0)
        row_match <- rep(FALSE, nrow(log_df))
        if (length(current_batches) > 0) {
          for (i in seq_len(nrow(log_df))) {
            bnames <- split_csv_values(log_df$batch_names[i])
            row_match[i] <- length(intersect(bnames, current_batches)) > 0
          }
        }

        matched <- log_df[row_match, , drop = FALSE]
        if (nrow(matched) > 0) {
          expected_total <- sum(as_int(matched$total_jobs), na.rm = TRUE)
        } else {
          # Fallback: use latest run in the selected output directory.
          run_at <- if ("run_at" %in% names(log_df)) as.character(log_df$run_at) else rep("", nrow(log_df))
          run_ts <- suppressWarnings(as.POSIXct(run_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
          run_num <- suppressWarnings(as.numeric(run_ts))
          run_num[!is.finite(run_num)] <- -Inf
          idx <- which.max(run_num)
          if (length(idx) == 1 && is.finite(idx)) {
            expected_total <- as_int(log_df$total_jobs[idx])
          }
        }
      }
    }

    # If log-derived total is unavailable, fall back to queue total.
    total_n <- if (is.finite(expected_total) && expected_total > 0) expected_total else queue_total_n
    done_n <- max(total_n - queue_total_n, 0L)
    pct <- if (total_n > 0) round(100 * done_n / total_n, 1) else NA_real_

    tags$span(
      style = "font-size:12px; color:#333;",
      tags$b("Progress: "),
      sprintf("Done %d / %d", done_n, total_n),
      if (is.finite(pct)) sprintf(" (%.1f%%)", pct) else "",
      sprintf(" | Run %d | Idle %d", run_n, idle_n)
    )
  })

  output$monitor_job_log_details <- renderUI({
    selected_output <- input$monitor_output_dir_filter
    if (is.null(selected_output) || !nzchar(selected_output) || selected_output == "__all__") {
      return(tags$span(style = "color:#777; font-size:12px;", "Job log details: choose an output directory"))
    }

    log_df <- tryCatch(load_launcher_job_log(), error = function(e) NULL)
    if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0) {
      return(tags$span(style = "color:#777; font-size:12px;", "Job log details: NA"))
    }

    if (!"output_dir" %in% names(log_df) || !"config_details" %in% names(log_df)) {
      return(tags$span(style = "color:#777; font-size:12px;", "Job log details: unavailable"))
    }

    out_vals <- trimws(as.character(log_df$output_dir))
    keep <- which(out_vals == selected_output)
    if (length(keep) == 0) {
      return(tags$span(style = "color:#777; font-size:12px;", "Job log details: no matching launch record"))
    }

    run_at_vals <- if ("run_at" %in% names(log_df)) as.character(log_df$run_at) else rep("", nrow(log_df))
    run_desc_vals <- if ("run_description" %in% names(log_df)) as.character(log_df$run_description) else rep("NA", nrow(log_df))
    summary_vals <- if ("summary" %in% names(log_df)) as.character(log_df$summary) else rep("NA", nrow(log_df))
    selected_condor_vals <- if ("selected_condor_nodes" %in% names(log_df)) as.character(log_df$selected_condor_nodes) else rep("NA", nrow(log_df))
    details_vals <- as.character(log_df$config_details)
    details_vals[is.na(details_vals) | !nzchar(trimws(details_vals))] <- "NA"

    ord <- keep
    if (length(ord) > 1) {
      run_ts <- suppressWarnings(as.POSIXct(run_at_vals[keep], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
      run_num <- suppressWarnings(as.numeric(run_ts))
      run_num[!is.finite(run_num)] <- -Inf
      ord <- keep[order(run_num, decreasing = TRUE)]
    }

    # Avoid rendering huge payloads all at once.
    ord <- head(ord, 5)

    blocks <- lapply(ord, function(i) {
      run_title <- if (!is.na(run_at_vals[i]) && nzchar(trimws(run_at_vals[i]))) run_at_vals[i] else "Unknown run time"
      run_desc <- if (!is.na(run_desc_vals[i]) && nzchar(trimws(run_desc_vals[i])) && run_desc_vals[i] != "NA") run_desc_vals[i] else "NA"
      run_summary <- if (!is.na(summary_vals[i]) && nzchar(trimws(summary_vals[i])) && summary_vals[i] != "NA") summary_vals[i] else "NA"
      selected_condor <- if (!is.na(selected_condor_vals[i]) && nzchar(trimws(selected_condor_vals[i])) && selected_condor_vals[i] != "NA") selected_condor_vals[i] else "NA"
      tags$details(
        style = "margin-top:6px; border:1px solid #e1e1e1; border-radius:4px; padding:6px 8px; background:#fafafa;",
        tags$summary(
          style = "cursor:pointer; font-size:12px;",
          paste0(run_title, " | desc: ", run_desc)
        ),
        tags$div(
          style = "margin-top:6px; font-size:12px;",
          tags$div(tags$b("Summary: "), run_summary),
          tags$div(tags$b("Selected Condor Nodes: "), selected_condor),
          tags$pre(
            style = "white-space: pre-wrap; max-height: 220px; overflow: auto; margin-top: 6px; background: #fff; border:1px solid #ddd; padding:8px;",
            details_vals[i]
          )
        )
      )
    })

    tags$div(
      tags$div(style = "font-size:12px; color:#444; margin-top:6px;",
               paste0("Job log details (latest ", length(blocks), " run(s) for selected output dir):")),
      blocks
    )
  })

  observe({
    invalidateLater(5000, session)
    if (isTRUE(input$auto_refresh_jobs) && !is.null(input$tabs) && input$tabs == "monitor") {
      shinyjs::click("refresh_jobs")
    }
  })
  
  
  output$jobs_table <- renderDT({
    jobs_df <- filtered_jobs_df()
    col_defs <- list(
      list(className = 'dt-center', targets = '_all'),
      list(orderable = FALSE, targets = 0)
    )

    if (nrow(jobs_df) > 0) {
      jobs_df$Select <- sprintf(
        '<input type="checkbox" class="job-select-checkbox" value="%s"/>',
        jobs_df$.row_id
      )
      jobs_df <- jobs_df[, c("Select", setdiff(names(jobs_df), c("Select", ".row_id")), ".row_id"), drop = FALSE]
      col_defs <- c(col_defs, list(list(visible = FALSE, targets = ncol(jobs_df) - 1)))
    }

    datatable(
      jobs_df,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 20,
        columnDefs = col_defs
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
    jobs_df <- filtered_jobs_df()
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
    jobs_df <- filtered_jobs_df()
    if (nrow(jobs_df) == 0) {
      cat("No jobs found. Click 'Refresh' to check job status.\n")
      return()
    }

    tryCatch({
      cat("========== Jobs by User ==========\n\n")

      owners <- unique(jobs_df$Owner)
      my_user <- input$remote_user

      for (owner in owners) {
        owner_jobs <- jobs_df[jobs_df$Owner == owner, , drop = FALSE]
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
      detail_keys <- paste(jobs_df$Owner, jobs_df$BatchName, sep = "::")

      if (!is.null(selected_batch) && nzchar(selected_batch) && selected_batch %in% detail_keys) {
        cat("========== Selected Job ==========\n\n")
        selected_jobs <- jobs_df[detail_keys == selected_batch, , drop = FALSE]
        
        for (i in seq_len(nrow(selected_jobs))) {
          job <- selected_jobs[i, ]
          cat(sprintf("• %s\n", job$BatchName))
          cat(sprintf("  Owner: %s | Submitted: %s\n", job$Owner, job$Submitted))
          cat(sprintf("  Status: %s done, %s running, %s idle (%s total)\n",
                      job$Done, job$Run, job$Idle, job$Total))
          if (!is.na(job$OutputDir) && nzchar(job$OutputDir) && job$OutputDir != "NA") {
            cat(sprintf("  Output Dir: %s\n", job$OutputDir))
          }
          if (!is.na(job$RunDescription) && nzchar(job$RunDescription) && job$RunDescription != "NA") {
            cat(sprintf("  Run Description: %s\n", job$RunDescription))
          }
          
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
    selected_rows <- suppressWarnings(as.integer(input$jobs_checked_rows))
    selected_rows <- selected_rows[is.finite(selected_rows)]
    
    if (length(selected_rows) == 0) {
      showNotification("No jobs selected", type = "warning")
      return()
    }
    
    selected_rows <- selected_rows[selected_rows >= 1 & selected_rows <= nrow(rv$jobs_status)]
    selected_jobs <- rv$jobs_status[selected_rows, , drop = FALSE]
    
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
    selected_rows <- suppressWarnings(as.integer(input$jobs_checked_rows))
    selected_rows <- selected_rows[is.finite(selected_rows)]
    if (length(selected_rows) == 0) {
      removeModal()
      return()
    }
    
    selected_rows <- selected_rows[selected_rows >= 1 & selected_rows <= nrow(rv$jobs_status)]
    selected_jobs <- rv$jobs_status[selected_rows, , drop = FALSE]
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
  
