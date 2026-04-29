  # ========== MONITOR JOBS ==========

  has_refreshed_jobs <- reactiveVal(FALSE)
  monitor_capacity <- reactiveVal(data.frame(
    RemoteHost = character(),
    Status = character(),
    TotalNodes = integer(),
    FreeNodes = integer(),
    PartialNodes = integer(),
    RunningNodes = integer(),
    ClaimedNodes = integer(),
    TotalCPUs = integer(),
    FreeCPUs = integer(),
    RunningCPUs = integer(),
    ClaimedCPUs = integer(),
    TotalMemoryGB = numeric(),
    FreeMemoryGB = numeric(),
    TotalDiskGB = numeric(),
    FreeDiskGB = numeric(),
    IdleJobs = integer(),
    RunJobs = integer(),
    Load = character(),
    stringsAsFactors = FALSE
  ))

  empty_jobs_status <- function() {
    data.frame(
      RemoteHost = character(),
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

  monitor_selected_hosts <- function() {
    vals <- input$monitor_remote_hosts
    if (is.null(vals) || length(vals) == 0) vals <- "current"
    vals <- as.character(vals)
    current_host <- if (is.null(input$remote_host)) "" else trimws(as.character(input$remote_host))
    vals[vals == "current"] <- current_host
    vals <- unique(trimws(vals))
    vals[nzchar(vals)]
  }

  empty_monitor_capacity <- function() {
    data.frame(
      RemoteHost = character(),
      Status = character(),
      TotalNodes = integer(),
      FreeNodes = integer(),
      PartialNodes = integer(),
      RunningNodes = integer(),
      ClaimedNodes = integer(),
      TotalCPUs = integer(),
      FreeCPUs = integer(),
      RunningCPUs = integer(),
      ClaimedCPUs = integer(),
      TotalMemoryGB = numeric(),
      FreeMemoryGB = numeric(),
      TotalDiskGB = numeric(),
      FreeDiskGB = numeric(),
      IdleJobs = integer(),
      RunJobs = integer(),
      Load = character(),
      stringsAsFactors = FALSE
    )
  }

  parse_condor_capacity <- function(remote_host, status_output, queue_df = NULL) {
    if (length(status_output) == 0) {
      return(data.frame(
        RemoteHost = remote_host, Status = "unavailable",
        TotalNodes = NA_integer_, FreeNodes = NA_integer_, PartialNodes = NA_integer_, RunningNodes = NA_integer_, ClaimedNodes = NA_integer_,
        TotalCPUs = NA_integer_, FreeCPUs = NA_integer_, RunningCPUs = NA_integer_, ClaimedCPUs = NA_integer_,
        TotalMemoryGB = NA_real_, FreeMemoryGB = NA_real_, TotalDiskGB = NA_real_, FreeDiskGB = NA_real_,
        IdleJobs = NA_integer_, RunJobs = NA_integer_, Load = "NA", stringsAsFactors = FALSE
      ))
    }

    parts <- strsplit(trimws(status_output[nzchar(trimws(status_output))]), "\\s+")
    names <- vapply(parts, function(x) if (length(x) >= 1) x[[1]] else "", character(1))
    states <- vapply(parts, function(x) if (length(x) >= 2) x[[2]] else "", character(1))
    cpus <- suppressWarnings(as.integer(vapply(parts, function(x) if (length(x) >= 3) x[[3]] else "1", character(1))))
    memory_mb <- suppressWarnings(as.numeric(vapply(parts, function(x) if (length(x) >= 4) x[[4]] else "0", character(1))))
    disk_kb <- suppressWarnings(as.numeric(vapply(parts, function(x) if (length(x) >= 5) x[[5]] else "0", character(1))))
    host_lower <- tolower(remote_host)
    target_patterns <- if (grepl("^nou|nouofp", host_lower)) {
      c("nouofp", "nou")
    } else if (grepl("^suv|suvofp", host_lower)) {
      c("suvofp", "suv")
    } else {
      character(0)
    }
    if (length(target_patterns) > 0) {
      keep <- vapply(tolower(names), function(x) any(vapply(target_patterns, grepl, logical(1), x = x, fixed = TRUE)), logical(1))
      names <- names[keep]
      states <- states[keep]
      cpus <- cpus[keep]
      memory_mb <- memory_mb[keep]
      disk_kb <- disk_kb[keep]
    }
    if (length(names) == 0) {
      return(data.frame(
        RemoteHost = remote_host, Status = "no matching nodes",
        TotalNodes = 0L, FreeNodes = 0L, PartialNodes = 0L, RunningNodes = 0L, ClaimedNodes = 0L,
        TotalCPUs = 0L, FreeCPUs = 0L, RunningCPUs = 0L, ClaimedCPUs = 0L,
        TotalMemoryGB = 0, FreeMemoryGB = 0, TotalDiskGB = 0, FreeDiskGB = 0,
        IdleJobs = NA_integer_, RunJobs = NA_integer_, Load = "0/0 CPUs busy", stringsAsFactors = FALSE
      ))
    }
    cpus[!is.finite(cpus)] <- 1L
    memory_mb[!is.finite(memory_mb)] <- 0
    disk_kb[!is.finite(disk_kb)] <- 0
    slot_key <- sub("@.*$", "", names)
    machine_key <- sub("^.*@", "", names)
    node_key <- ifelse(nzchar(machine_key) & machine_key != names, machine_key, slot_key)
    node_key <- node_key[nzchar(node_key)]
    total_cpus <- sum(cpus, na.rm = TRUE)
    free_cpus <- sum(cpus[tolower(states) %in% c("unclaimed", "owner")], na.rm = TRUE)
    claimed_cpus <- sum(cpus[tolower(states) == "claimed"], na.rm = TRUE)
    running_cpus <- claimed_cpus
    state_lower <- tolower(states)
    free_state <- state_lower %in% c("unclaimed", "owner")
    claimed_state <- state_lower == "claimed"
    total_nodes <- length(unique(node_key))
    node_free_any <- stats::aggregate(free_state, by = list(node = node_key), FUN = any)
    node_claimed_any <- stats::aggregate(claimed_state, by = list(node = node_key), FUN = any)
    names(node_free_any)[2] <- "has_free"
    names(node_claimed_any)[2] <- "has_claimed"
    node_state <- merge(node_free_any, node_claimed_any, by = "node", all = TRUE)
    node_state$has_free[is.na(node_state$has_free)] <- FALSE
    node_state$has_claimed[is.na(node_state$has_claimed)] <- FALSE
    free_nodes <- sum(node_state$has_free & !node_state$has_claimed)
    partial_nodes <- sum(node_state$has_free & node_state$has_claimed)
    running_nodes <- sum(node_state$has_claimed)
    claimed_nodes <- sum(!node_state$has_free & node_state$has_claimed)
    total_memory_gb <- sum(memory_mb, na.rm = TRUE) / 1024
    free_memory_gb <- sum(memory_mb[tolower(states) %in% c("unclaimed", "owner")], na.rm = TRUE) / 1024
    total_disk_gb <- sum(disk_kb, na.rm = TRUE) / (1024 * 1024)
    free_disk_gb <- sum(disk_kb[tolower(states) %in% c("unclaimed", "owner")], na.rm = TRUE) / (1024 * 1024)

    idle_jobs <- 0L
    run_jobs <- 0L
    if (!is.null(queue_df) && is.data.frame(queue_df) && nrow(queue_df) > 0) {
      as_int <- function(x) {
        y <- suppressWarnings(as.integer(as.character(x)))
        y[!is.finite(y)] <- 0L
        y
      }
      idle_jobs <- sum(as_int(queue_df$Idle), na.rm = TRUE)
      run_jobs <- sum(as_int(queue_df$Run), na.rm = TRUE)
    }

    data.frame(
      RemoteHost = remote_host,
      Status = "ok",
      TotalNodes = as.integer(total_nodes),
      FreeNodes = as.integer(free_nodes),
      PartialNodes = as.integer(partial_nodes),
      RunningNodes = as.integer(running_nodes),
      ClaimedNodes = as.integer(claimed_nodes),
      TotalCPUs = as.integer(total_cpus),
      FreeCPUs = as.integer(free_cpus),
      RunningCPUs = as.integer(running_cpus),
      ClaimedCPUs = as.integer(claimed_cpus),
      TotalMemoryGB = round(total_memory_gb, 1),
      FreeMemoryGB = round(free_memory_gb, 1),
      TotalDiskGB = round(total_disk_gb, 1),
      FreeDiskGB = round(free_disk_gb, 1),
      IdleJobs = as.integer(idle_jobs),
      RunJobs = as.integer(run_jobs),
      Load = sprintf("%d/%d CPUs busy", running_cpus, total_cpus),
      stringsAsFactors = FALSE
    )
  }

  split_csv_values <- function(x) {
    vals <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
    vals[nzchar(vals)]
  }

  compact_monitor_path <- function(path_txt) {
    path_txt <- trimws(as.character(path_txt))
    path_txt <- path_txt[!is.na(path_txt) & nzchar(path_txt) & !tolower(path_txt) %in% c("undefined", "null", "na")]
    if (length(path_txt) == 0) return("misc")
    p <- path_txt[[1]]
    repo_markers <- unique(c(input$github_repo, basename(repo_root_val())))
    repo_markers <- repo_markers[nzchar(repo_markers)]
    for (marker in repo_markers) {
      hit <- regexpr(paste0("/", marker, "/"), p, fixed = TRUE)
      if (hit[[1]] > 0) {
        return(sub("^/+", "", substr(p, hit[[1]] + nchar(marker) + 2L, nchar(p))))
      }
    }
    p
  }

  infer_output_dir_from_classad <- function(iwd, out, err, user_log, cmd) {
    path_candidates <- c(iwd, dirname(out), dirname(err), dirname(user_log), dirname(cmd))
    path_candidates <- path_candidates[!is.na(path_candidates) & nzchar(path_candidates) & !tolower(path_candidates) %in% c(".", "undefined", "null", "na")]
    compact_monitor_path(path_candidates)
  }

  scan_condor_job_details <- function(ssh_target, remote_host) {
    args <- c(if (isTRUE(input$show_all_jobs)) "-all" else character(0),
              "-af", "Owner", "JobBatchName", "Iwd", "Out", "Err", "UserLog", "Cmd")
    detail_output <- tryCatch(
      system2("ssh", c(ssh_target, paste("condor_q", paste(args, collapse = " "))), stdout = TRUE, stderr = TRUE),
      error = function(e) character(0)
    )
    detail_output <- detail_output[nzchar(trimws(detail_output))]
    if (length(detail_output) == 0) {
      return(data.frame(RemoteHost = character(), Owner = character(), BatchName = character(), InferredOutputDir = character(), InferredDescription = character(), stringsAsFactors = FALSE))
    }

    rows <- lapply(detail_output, function(line) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) < 2) return(NULL)
      fields <- c(parts, rep("", max(0, 7 - length(parts))))[seq_len(7)]
      inferred <- infer_output_dir_from_classad(fields[[3]], fields[[4]], fields[[5]], fields[[6]], fields[[7]])
      data.frame(
        RemoteHost = remote_host,
        Owner = fields[[1]],
        BatchName = fields[[2]],
        InferredOutputDir = inferred,
        InferredDescription = if (identical(inferred, "misc")) "misc / not in launcher job log" else "inferred from condor queue paths",
        stringsAsFactors = FALSE
      )
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) {
      return(data.frame(RemoteHost = character(), Owner = character(), BatchName = character(), InferredOutputDir = character(), InferredDescription = character(), stringsAsFactors = FALSE))
    }
    out <- do.call(rbind, rows)
    out <- out[!duplicated(paste(out$RemoteHost, out$Owner, out$BatchName, sep = "\r")), , drop = FALSE]
    rownames(out) <- NULL
    out
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

  enrich_jobs_status <- function(df, details_df = NULL) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_jobs_status())
    meta <- build_batch_metadata()
    if (nrow(meta) == 0) {
      df$OutputDir <- rep("misc", nrow(df))
      df$RunDescription <- rep("misc / not in launcher job log", nrow(df))
    } else {
      idx <- match(df$BatchName, meta$BatchName)
      df$OutputDir <- meta$OutputDir[idx]
      df$RunDescription <- meta$RunDescription[idx]
    }

    if (!is.null(details_df) && is.data.frame(details_df) && nrow(details_df) > 0) {
      key <- paste(df$RemoteHost, df$Owner, df$BatchName, sep = "\r")
      detail_key <- paste(details_df$RemoteHost, details_df$Owner, details_df$BatchName, sep = "\r")
      didx <- match(key, detail_key)
      need_infer <- is.na(df$OutputDir) | !nzchar(trimws(df$OutputDir)) | df$OutputDir == "misc"
      has_detail <- !is.na(didx)
      replace_idx <- which(need_infer & has_detail)
      if (length(replace_idx) > 0) {
        inferred <- details_df$InferredOutputDir[didx[replace_idx]]
        inferred <- ifelse(is.na(inferred) | !nzchar(trimws(inferred)), "misc", inferred)
        df$OutputDir[replace_idx] <- inferred
        df$RunDescription[replace_idx] <- details_df$InferredDescription[didx[replace_idx]]
      }
    }

    df$OutputDir[is.na(df$OutputDir) | !nzchar(trimws(df$OutputDir))] <- "misc"
    df$RunDescription[is.na(df$RunDescription) | !nzchar(trimws(df$RunDescription))] <- "misc / not in launcher job log"
    df
  }

  filtered_jobs_df <- reactive({
    df <- rv$jobs_status
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_jobs_status())
    if (!"OutputDir" %in% names(df)) df$OutputDir <- "misc"
    if (!"RunDescription" %in% names(df)) df$RunDescription <- "misc / not in launcher job log"
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
      jobs_list <- list()
      capacity_rows <- list()
      hosts <- monitor_selected_hosts()
      if (length(hosts) == 0) stop("No monitor remote host selected")

      for (remote_host in hosts) {
        ssh_target <- sprintf("%s@%s", input$remote_user, remote_host)
        condor_cmd <- if (isTRUE(input$show_all_jobs)) "condor_q -all" else "condor_q"
        jobs_output <- tryCatch(
          system2("ssh", c(ssh_target, condor_cmd), stdout = TRUE, stderr = TRUE),
          error = function(e) {
            showNotification(sprintf("Monitor failed on %s: %s", remote_host, e$message), type = "warning", duration = 5)
            character(0)
          }
        )

        status_output <- tryCatch(
          system2("ssh", c(ssh_target, "condor_status -af Name State Cpus Memory Disk"), stdout = TRUE, stderr = TRUE),
          error = function(e) character(0)
        )

        host_jobs_start <- length(jobs_list)
        header_idx <- grep("^OWNER\\s+BATCH_NAME", jobs_output)
        if (length(header_idx) > 0 && header_idx < length(jobs_output)) {
          start_idx <- header_idx + 1
          total_idx <- grep("^Total", jobs_output)
          end_idx <- if (length(total_idx) > 0) total_idx[1] - 1 else length(jobs_output)

          if (start_idx <= end_idx) {
            data_lines <- jobs_output[start_idx:end_idx]
            data_lines <- data_lines[nzchar(trimws(data_lines))]

            for (line in data_lines) {
              line_trimmed <- trimws(line)
              if (nzchar(line_trimmed)) {
                parts <- unlist(strsplit(line_trimmed, "\\s+"))

                if (length(parts) >= 8) {
                  jobs_list[[length(jobs_list) + 1]] <- data.frame(
                    RemoteHost = remote_host,
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

        host_jobs <- if (length(jobs_list) > host_jobs_start) {
          do.call(rbind, jobs_list[(host_jobs_start + 1L):length(jobs_list)])
        } else {
          empty_jobs_status()
        }
        capacity_rows[[length(capacity_rows) + 1L]] <- parse_condor_capacity(remote_host, status_output, host_jobs)
      }

      monitor_capacity(if (length(capacity_rows) > 0) do.call(rbind, capacity_rows) else empty_monitor_capacity())
      
      if (length(jobs_list) > 0) {
        rv$jobs_status <- enrich_jobs_status(do.call(rbind, jobs_list))
        
        msg <- if (input$show_all_jobs) {
          paste("Found", nrow(rv$jobs_status), "jobs (all users) on", length(hosts), "host(s)")
        } else {
          paste("Found", nrow(rv$jobs_status), "jobs (yours) on", length(hosts), "host(s)")
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
      monitor_capacity(empty_monitor_capacity())
    })
  })
  
  # Auto-refresh when toggle changes
  observeEvent(input$show_all_jobs, {
    shinyjs::click("refresh_jobs")
  }, ignoreInit = TRUE)

  observeEvent(input$monitor_remote_hosts, {
    if (isTRUE(has_refreshed_jobs())) shinyjs::click("refresh_jobs")
  }, ignoreInit = TRUE)

  observeEvent(rv$launcher_job_log_trigger, {
    if (is.null(rv$jobs_status) || !is.data.frame(rv$jobs_status) || nrow(rv$jobs_status) == 0) return()
    rv$jobs_status <- enrich_jobs_status(rv$jobs_status[, intersect(
      c("RemoteHost", "Owner", "BatchName", "Submitted", "Done", "Run", "Idle", "Total", "JobIDs"),
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
    output_dirs <- sort(output_dirs[nzchar(output_dirs)])
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

  output$monitor_capacity_summary <- renderUI({
    cap <- monitor_capacity()
    if (!isTRUE(has_refreshed_jobs()) || is.null(cap) || !is.data.frame(cap) || nrow(cap) == 0) {
      return(tags$span(style = "color:#777; font-size:12px;", "Capacity: click Refresh to scan NOU/SUV compute availability"))
    }

    fmt_host <- function(host) {
      if (grepl("^nou", host, ignore.case = TRUE)) "NOU" else if (grepl("^suv", host, ignore.case = TRUE)) "SUV" else host
    }
    host_palette <- function(host, ok, free) {
      if (!ok) return(list(text = "#842029", bg = "#f8d7da", border = "#f1aeb5"))
      if (grepl("^nou", host, ignore.case = TRUE)) {
        return(list(text = "#052c65", bg = "#b6d4fe", border = "#6ea8fe"))
      }
      if (grepl("^suv", host, ignore.case = TRUE)) {
        return(list(text = "#3d2b00", bg = "#ffe69c", border = "#ffcd39"))
      }
      if (is.finite(free) && free > 0) list(text = "#41464b", bg = "#e2e3e5", border = "#c4c8cb") else list(text = "#664d03", bg = "#fff3cd", border = "#ffda6a")
    }
    blocks <- lapply(seq_len(nrow(cap)), function(i) {
      row <- cap[i, , drop = FALSE]
      ok <- identical(as.character(row$Status), "ok")
      free <- suppressWarnings(as.integer(row$FreeCPUs))
      total <- suppressWarnings(as.integer(row$TotalCPUs))
      free_nodes <- suppressWarnings(as.integer(row$FreeNodes))
      partial_nodes <- suppressWarnings(as.integer(row$PartialNodes))
      running_nodes <- suppressWarnings(as.integer(row$RunningNodes))
      claimed_nodes <- suppressWarnings(as.integer(row$ClaimedNodes))
      total_nodes <- suppressWarnings(as.integer(row$TotalNodes))
      free_mem <- suppressWarnings(as.numeric(row$FreeMemoryGB))
      total_mem <- suppressWarnings(as.numeric(row$TotalMemoryGB))
      free_disk <- suppressWarnings(as.numeric(row$FreeDiskGB))
      total_disk <- suppressWarnings(as.numeric(row$TotalDiskGB))
      idle_jobs <- suppressWarnings(as.integer(row$IdleJobs))
      run_jobs <- suppressWarnings(as.integer(row$RunJobs))
      pal <- host_palette(row$RemoteHost, ok, free)
      title <- fmt_host(row$RemoteHost)
      if (!ok) {
        return(tags$div(
          style = paste0("display:inline-block; vertical-align:top; min-width:180px; margin:3px 6px 3px 0; padding:7px 9px; border-radius:5px; color:", pal$text, "; background:", pal$bg, "; border:1px solid ", pal$border, ";"),
          tags$div(style = "font-weight:700; font-size:12px;", title),
          tags$div(style = "font-size:12px;", "unavailable")
        ))
      }

      tags$div(
        style = paste0("display:inline-block; vertical-align:top; min-width:210px; margin:3px 6px 3px 0; padding:7px 9px; border-radius:5px; color:", pal$text, "; background:", pal$bg, "; border:1px solid ", pal$border, ";"),
        tags$div(style = "font-weight:700; font-size:12px; margin-bottom:3px;", title),
        tags$div(style = "font-size:12px;", sprintf("Nodes: empty %s, partial %s, full %s / total %s", ifelse(is.finite(free_nodes), free_nodes, "NA"), ifelse(is.finite(partial_nodes), partial_nodes, "NA"), ifelse(is.finite(claimed_nodes), claimed_nodes, "NA"), ifelse(is.finite(total_nodes), total_nodes, "NA"))),
        tags$div(style = "font-size:12px;", sprintf("Running nodes: %s", ifelse(is.finite(running_nodes), running_nodes, "NA"))),
        tags$div(style = "font-size:12px;", sprintf("CPUs free: %s / %s", ifelse(is.finite(free), free, "NA"), ifelse(is.finite(total), total, "NA"))),
        tags$div(style = "font-size:12px;", sprintf("Memory free: %s / %s GB", ifelse(is.finite(free_mem), format(round(free_mem, 1), nsmall = 1), "NA"), ifelse(is.finite(total_mem), format(round(total_mem, 1), nsmall = 1), "NA"))),
        tags$div(style = "font-size:12px;", sprintf("Disk free: %s / %s GB", ifelse(is.finite(free_disk), format(round(free_disk, 1), nsmall = 1), "NA"), ifelse(is.finite(total_disk), format(round(total_disk, 1), nsmall = 1), "NA"))),
        tags$div(style = "font-size:12px;", sprintf("Jobs: run %s, idle %s", ifelse(is.finite(run_jobs), run_jobs, "NA"), ifelse(is.finite(idle_jobs), idle_jobs, "NA")))
      )
    })

    tags$div(
      tags$div(style = "font-size:12px; font-weight:700; margin-bottom:2px;", "Capacity"),
      blocks
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
      row_id_col <- which(names(jobs_df) == ".row_id") - 1L
      output_col <- which(names(jobs_df) == "OutputDir") - 1L
      col_defs <- c(
        col_defs,
        list(
          list(visible = FALSE, targets = row_id_col),
          list(className = "dt-left", targets = output_col)
        )
      )
    }

    datatable(
      jobs_df,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        columnDefs = col_defs,
        rowCallback = JS(
          "function(row, data) {",
          "  var headers = this.api().columns().header().toArray().map(function(h){ return $(h).text(); });",
          "  var runIdx = headers.indexOf('Run');",
          "  var idleIdx = headers.indexOf('Idle');",
          "  var doneIdx = headers.indexOf('Done');",
          "  var totalIdx = headers.indexOf('Total');",
          "  var hostIdx = headers.indexOf('RemoteHost');",
          "  var run = runIdx >= 0 ? parseInt(data[runIdx], 10) || 0 : 0;",
          "  var idle = idleIdx >= 0 ? parseInt(data[idleIdx], 10) || 0 : 0;",
          "  var done = doneIdx >= 0 ? parseInt(data[doneIdx], 10) || 0 : 0;",
          "  var total = totalIdx >= 0 ? parseInt(data[totalIdx], 10) || 0 : 0;",
          "  if (run > 0) { $(row).css('background-color', '#d1e7dd'); }",
          "  else if (idle > 0) { $(row).css('background-color', '#fff3cd'); }",
          "  else if (total > 0 && done >= total) { $(row).css('background-color', '#e2e3e5'); }",
          "  if (hostIdx >= 0) {",
          "    var host = String(data[hostIdx]);",
          "    var label = host.match(/^suv/i) ? 'SUV' : (host.match(/^nou/i) ? 'NOU' : host);",
          "    var isSuv = host.match(/^suv/i);",
          "    var bg = isSuv ? '#ffe69c' : '#b6d4fe';",
          "    var fg = isSuv ? '#3d2b00' : '#052c65';",
          "    var bd = isSuv ? '#ffcd39' : '#6ea8fe';",
          "    $('td:eq(' + hostIdx + ')', row).html('<span style=\"display:inline-block;padding:2px 7px;border-radius:4px;background:' + bg + ';color:' + fg + ';border:1px solid ' + bd + ';font-weight:700;\">' + label + '</span>');",
          "  }",
          "}"
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
    jobs_df <- filtered_jobs_df()
    if (is.null(jobs_df) || nrow(jobs_df) == 0) {
      updateSelectInput(session, "job_detail_batch", choices = character(0), selected = character(0))
      return()
    }

    detail_keys <- paste(jobs_df$RemoteHost, jobs_df$Owner, jobs_df$BatchName, sep = "::")
    choices <- stats::setNames(
      detail_keys,
      paste0(jobs_df$BatchName, " (", jobs_df$Owner, " @ ", jobs_df$RemoteHost, ")")
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
      detail_keys <- paste(jobs_df$RemoteHost, jobs_df$Owner, jobs_df$BatchName, sep = "::")

      if (!is.null(selected_batch) && nzchar(selected_batch) && selected_batch %in% detail_keys) {
        cat("========== Selected Job ==========\n\n")
        selected_jobs <- jobs_df[detail_keys == selected_batch, , drop = FALSE]
        
        for (i in seq_len(nrow(selected_jobs))) {
          job <- selected_jobs[i, ]
          cat(sprintf("• %s\n", job$BatchName))
          cat(sprintf("  Remote Host: %s\n", job$RemoteHost))
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
              tags$th("Remote Host", style = "text-align: left; padding: 5px;"),
              tags$th("Batch Name", style = "text-align: left; padding: 5px;"),
              tags$th("Job ID", style = "text-align: left; padding: 5px;"),
              tags$th("Status", style = "text-align: center; padding: 5px;")
            )
          ),
          tags$tbody(
            lapply(1:nrow(selected_jobs), function(i) {
              tags$tr(
                tags$td(selected_jobs$RemoteHost[i], style = "padding: 5px;"),
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
    
    total_to_remove <- sum(sapply(job_ids, function(x) {
      if (nzchar(x) && !is.na(x)) {
        length(strsplit(x, ",")[[1]])
      } else {
        0
      }
    }))
    
    update_delete_notification(sprintf("Deleting jobs... (0/%d)", total_to_remove))
    
    tryCatch({
      if (!"RemoteHost" %in% names(selected_jobs)) selected_jobs$RemoteHost <- input$remote_host
      host_vals <- unique(trimws(as.character(selected_jobs$RemoteHost)))
      host_vals <- host_vals[nzchar(host_vals)]

      if (length(host_vals) == 0) {
        stop("No remote host found for selected jobs")
      }

      all_ids <- character(0)
      ids_by_host <- lapply(host_vals, function(remote_host) {
        host_jobs <- selected_jobs[trimws(as.character(selected_jobs$RemoteHost)) == remote_host, , drop = FALSE]
        ids <- unique(trimws(unlist(strsplit(paste(host_jobs$JobIDs[nzchar(host_jobs$JobIDs) & !is.na(host_jobs$JobIDs)], collapse = ","), ","))))
        ids <- ids[nzchar(ids)]
        all_ids <<- c(all_ids, ids)
        ids
      })
      names(ids_by_host) <- host_vals

      all_ids <- unique(all_ids)
      if (length(all_ids) == 0) {
        stop("No valid job IDs found")
      }

      update_delete_notification(
        sprintf("Deleting %d jobs...", length(all_ids))
      )

      results <- character(0)
      for (remote_host in names(ids_by_host)) {
        ids <- ids_by_host[[remote_host]]
        if (length(ids) == 0) next
        ssh_target <- sprintf("%s@%s", input$remote_user, remote_host)
        results <- c(
          results,
          sprintf("[%s]", remote_host),
          system2("ssh", c(ssh_target, paste("condor_rm", paste(ids, collapse = " "))), stdout = TRUE, stderr = TRUE)
        )
      }

      result_text <- paste(results, collapse = "\n")
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
  
