  # ========== CONDOR COMPLETED RUNS ==========

  condor_completed_empty <- function() {
    data.frame(
      Model = character(),
      JobLog = character(),
      JobType = character(),
      BatchName = character(),
      LaunchAt = character(),
      CompletedAt = character(),
      Duration = character(),
      Status = character(),
      RemoteHost = character(),
      Description = character(),
      ConfigFile = character(),
      ConfigDetails = character(),
      JobLogFile = character(),
      OutputDir = character(),
      LocalDir = character(),
      LocalExists = character(),
      RemoteDir = character(),
      stringsAsFactors = FALSE
    )
  }

  condor_completed_split_csv <- function(x) {
    vals <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
    vals[nzchar(vals)]
  }

  condor_completed_parse_time <- function(x) {
    x <- trimws(as.character(x))
    x[!nzchar(x) | is.na(x)] <- NA_character_
    suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
  }

  condor_completed_first_string <- function(x) {
    x <- as.character(x)
    if (length(x) == 0 || is.na(x[1])) return("")
    trimws(x[1])
  }

  condor_completed_scan_hosts <- function(current_host) {
    hosts <- unique(c(
      condor_completed_first_string(current_host),
      "nouofpsubmit.corp.spc.int",
      "suvofpsubmit.corp.spc.int",
      "nouofpsubmit",
      "suvofpsubmit"
    ))
    hosts[nzchar(hosts)]
  }

  condor_completed_status_label <- function(status, count) {
    status <- condor_completed_first_string(status)
    count <- suppressWarnings(as.integer(count))
    if (identical(status, "completed") && is.finite(count) && count > 0L) {
      "completed"
    } else if (identical(status, "ssh failed")) {
      "ssh failed"
    } else if (identical(status, "parse failed")) {
      "parse failed"
    } else {
      "not completed"
    }
  }

  condor_completed_window_start <- function(window) {
    window <- condor_completed_first_string(window)
    if (!nzchar(window) || identical(window, "all")) return(as.POSIXct(NA))
    hours <- switch(
      window,
      "6h" = 6,
      "12h" = 12,
      "1d" = 24,
      "7d" = 24 * 7,
      "30d" = 24 * 30,
      NA_real_
    )
    if (!is.finite(hours)) return(as.POSIXct(NA))
    Sys.time() - hours * 3600
  }

  condor_completed_filter_by_window <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(condor_completed_empty())
    window_start <- condor_completed_window_start(input$condor_completed_launch_window)
    if (is.na(window_start)) return(df)
    launch_ts <- condor_completed_parse_time(df$LaunchAt)
    keep <- !is.na(launch_ts) & launch_ts >= window_start
    df[keep, , drop = FALSE]
  }

  condor_completed_filter_expected_by_window <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
    window_start <- condor_completed_window_start(input$condor_completed_launch_window)
    if (is.na(window_start)) return(df)
    launch_ts <- condor_completed_parse_time(df$LaunchAt)
    df[!is.na(launch_ts) & launch_ts >= window_start, , drop = FALSE]
  }

  condor_completed_format_duration <- function(start, end) {
    if (is.na(start) || is.na(end)) return("NA")
    secs <- as.numeric(difftime(end, start, units = "secs"))
    if (!is.finite(secs) || secs < 0) return("NA")
    days <- floor(secs / 86400)
    secs <- secs - days * 86400
    hours <- floor(secs / 3600)
    secs <- secs - hours * 3600
    mins <- floor(secs / 60)
    secs <- round(secs - mins * 60)
    if (days > 0) {
      sprintf("%dd %02dh %02dm", days, hours, mins)
    } else {
      sprintf("%02dh %02dm %02ds", hours, mins, secs)
    }
  }

  condor_completed_sort_rows <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
    launch_ts <- condor_completed_parse_time(df$LaunchAt)
    ord_num <- suppressWarnings(as.numeric(launch_ts))
    ord_num[!is.finite(ord_num)] <- -Inf
    df <- df[order(-ord_num, df$Model, df$JobType), , drop = FALSE]
    rownames(df) <- NULL
    df
  }

  condor_completed_job_label <- function(batch_name, remote_dir) {
    label <- trimws(as.character(batch_name))
    if (!nzchar(label) || is.na(label)) label <- basename(trimws(as.character(remote_dir)))
    label <- sub("-[0-9]{2}:[0-9]{2}:[0-9]{2}-[0-9]+$", "", label)
    label
  }

  condor_completed_job_type <- function(label, remote_dir) {
    txt <- paste(label, basename(remote_dir), sep = " ")
    if (grepl("jitter|_seed[0-9]+", txt, ignore.case = TRUE)) return("jitter")
    if (grepl("stage[_-]?check|setup[_-]?check", txt, ignore.case = TRUE)) return("stage_check")
    if (grepl("hess|_part[0-9]+", txt, ignore.case = TRUE)) return("hessian")
    if (grepl("retro|_peel[0-9]+", txt, ignore.case = TRUE)) return("retro")
    if (grepl("prof2d|prof_2d", txt, ignore.case = TRUE)) return("prof_2d")
    if (grepl("prof|_sc[0-9]+|-sc[0-9]+", txt, ignore.case = TRUE)) return("prof")
    "model"
  }

  condor_completed_model_name <- function(label, remote_dir) {
    x <- label
    x <- sub("-(stagecheck|jitter[0-9]+|hess[0-9]+|retro[0-9]+|sc[0-9]+|prof2d)$", "", x, ignore.case = TRUE)
    x <- sub("(_stagecheck|_model|_seed[0-9]+|_part[0-9]+|_peel[0-9]+|_sc[0-9]+|_prof2d)$", "", x, ignore.case = TRUE)
    if (!nzchar(x) || is.na(x)) {
      x <- sub("(_stagecheck|_model|_seed[0-9]+|_part[0-9]+|_peel[0-9]+|_sc[0-9]+|_prof2d)$", "", basename(remote_dir), ignore.case = TRUE)
    }
    x
  }

  condor_completed_local_dir <- function(model_name, job_type, output_dir, remote_dir, download_location = "model") {
    model_name <- condor_completed_first_string(model_name)
    job_type <- condor_completed_first_string(job_type)
    output_dir <- condor_completed_first_string(output_dir)
    remote_dir <- condor_completed_first_string(remote_dir)
    download_location <- condor_completed_first_string(download_location)
    if (!nzchar(download_location)) download_location <- "model"
    if (!nzchar(model_name) || !nzchar(remote_dir)) return("NA")

    local_root <- if (startsWith(download_location, "/") || startsWith(download_location, "~")) {
      path.expand(download_location)
    } else {
      tryCatch(resolve_repo_path(download_location), error = function(e) file.path(getwd(), download_location))
    }

    remote_base <- basename(remote_dir)
    candidates <- c(file.path(local_root, model_name))

    seed <- suppressWarnings(as.integer(sub(".*_seed([0-9]+)$", "\\1", remote_base)))
    part <- suppressWarnings(as.integer(sub(".*_part([0-9]+)$", "\\1", remote_base)))
    peel <- suppressWarnings(as.integer(sub(".*_peel([0-9]+)$", "\\1", remote_base)))
    scalar <- suppressWarnings(as.integer(sub(".*_sc([0-9]+)$", "\\1", remote_base)))

    if (identical(job_type, "jitter") && is.finite(seed)) {
      candidates <- c(file.path(local_root, model_name, "jitter", paste0("jitter_seed_", seed)), candidates)
    } else if (identical(job_type, "hessian") && is.finite(part)) {
      candidates <- c(file.path(local_root, model_name, "hessian", paste0("part_", part)), candidates)
    } else if (identical(job_type, "retro") && is.finite(peel)) {
      candidates <- c(file.path(local_root, model_name, "retro", paste0("peel_", peel)), candidates)
    } else if (identical(job_type, "prof") && is.finite(scalar)) {
      candidates <- c(file.path(local_root, model_name, "prof", paste0("scalar_", scalar)), candidates)
    }

    if (!identical(remote_base, model_name)) {
      remote_model_name <- remote_base
      remote_model_name <- sub("(_model|_seed[0-9]+|_part[0-9]+|_peel[0-9]+|_sc[0-9]+|_prof2d)$", "", remote_model_name, ignore.case = TRUE)
      if (nzchar(remote_model_name)) candidates <- c(candidates, file.path(local_root, remote_model_name))
    }

    if (nzchar(output_dir) && !identical(output_dir, "NA")) {
      output_base <- if (startsWith(output_dir, "/") || startsWith(output_dir, "~")) {
        path.expand(output_dir)
      } else {
        tryCatch(resolve_repo_path(output_dir), error = function(e) file.path(getwd(), output_dir))
      }
      candidates <- c(candidates, file.path(output_base, model_name))
    }

    candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
    existing <- candidates[dir.exists(candidates)]
    if (length(existing) > 0) return(existing[[1]])

    paste0("not found: ", candidates[[1]])
  }

  condor_completed_local_exists <- function(local_dir) {
    local_dir <- condor_completed_first_string(local_dir)
    if (nzchar(local_dir) && !startsWith(local_dir, "not found:") && dir.exists(local_dir)) "yes" else "no"
  }

  condor_completed_detail_html <- function(row) {
    field <- function(name, value, code = FALSE) {
      value <- condor_completed_first_string(value)
      if (!nzchar(value)) value <- "NA"
      val_html <- if (isTRUE(code)) {
        paste0("<code>", htmltools::htmlEscape(value), "</code>")
      } else {
        htmltools::htmlEscape(value)
      }
      paste0(
        "<div style='margin-bottom:6px;'>",
        "<b>", htmltools::htmlEscape(name), ":</b> ",
        val_html,
        "</div>"
      )
    }

    config_details <- condor_completed_first_string(row$ConfigDetails)
    config_html <- if (nzchar(config_details) && !identical(config_details, "NA")) {
      paste0(
        "<details style='margin-top:8px;'>",
        "<summary style='cursor:pointer; color:#2c6d8c;'>Config details</summary>",
        "<pre style='max-height:260px; overflow:auto; margin-top:6px; padding:8px; background:#fff;",
        " border:1px solid #e3edf5; border-radius:3px; font-size:12px; white-space:pre-wrap;'>",
        htmltools::htmlEscape(config_details),
        "</pre>",
        "</details>"
      )
    } else {
      "<div style='font-size:12px; color:#777; margin-top:8px;'>No config details recorded.</div>"
    }

    paste0(
      "<div style='padding:10px 12px; background:#f7fbff; border:1px solid #d9e7f3;",
      " border-radius:4px; font-size:12px; line-height:1.4;'>",
      field("Model", row$Model),
      field("Launcher job log", row$JobLog),
      field("Job log file", row$JobLogFile, code = TRUE),
      field("Job type", row$JobType),
      field("Batch", row$BatchName, code = TRUE),
      field("Description", row$Description),
      field("Config file", row$ConfigFile, code = TRUE),
      field("Launch at", row$LaunchAt),
      field("Completed at", row$CompletedAt),
      field("Duration", row$Duration),
      field("Status", row$Status),
      field("Remote host", row$RemoteHost, code = TRUE),
      field("Output dir", row$OutputDir, code = TRUE),
      field("Local dir", row$LocalDir, code = TRUE),
      field("Remote dir", row$RemoteDir, code = TRUE),
      config_html,
      "</div>"
    )
  }

  condor_completed_build_expected <- function() {
    log_df <- tryCatch(load_launcher_job_log(), error = function(e) NULL)
    if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0) {
      return(data.frame())
    }

    rows <- list()
    for (i in seq_len(nrow(log_df))) {
      if (!"launch_mode" %in% names(log_df) || !identical(tolower(as.character(log_df$launch_mode[i])), "condor")) next
      remote_dirs <- if ("remote_dirs" %in% names(log_df)) condor_completed_split_csv(log_df$remote_dirs[i]) else character(0)
      if (length(remote_dirs) == 0) next
      batch_names <- if ("batch_names" %in% names(log_df)) condor_completed_split_csv(log_df$batch_names[i]) else character(0)
      run_at <- if ("run_at" %in% names(log_df)) as.character(log_df$run_at[i]) else "NA"
      desc <- if ("run_description" %in% names(log_df)) as.character(log_df$run_description[i]) else "NA"
      config_file <- if ("config_file" %in% names(log_df)) as.character(log_df$config_file[i]) else "NA"
      config_details <- if ("config_details" %in% names(log_df)) as.character(log_df$config_details[i]) else "NA"
      out_dir <- if ("output_dir" %in% names(log_df)) as.character(log_df$output_dir[i]) else "NA"

      for (j in seq_along(remote_dirs)) {
        batch <- if (j <= length(batch_names)) batch_names[j] else basename(remote_dirs[j])
        label <- condor_completed_job_label(batch, remote_dirs[j])
        model_name <- condor_completed_model_name(label, remote_dirs[j])
        job_type <- condor_completed_job_type(label, remote_dirs[j])
        download_location <- tryCatch(condor_completed_first_string(input$download_location), error = function(e) "")
        if (!nzchar(download_location)) download_location <- "model"
        local_dir <- condor_completed_local_dir(model_name, job_type, out_dir, remote_dirs[j], download_location)
        rows[[length(rows) + 1L]] <- data.frame(
          Model = model_name,
          JobLog = paste0("row ", i),
          JobType = job_type,
          BatchName = batch,
          LaunchAt = run_at,
          Description = if (!is.na(desc) && nzchar(trimws(desc))) desc else "NA",
          ConfigFile = if (!is.na(config_file) && nzchar(trimws(config_file))) config_file else "NA",
          ConfigDetails = if (!is.na(config_details) && nzchar(trimws(config_details))) config_details else "NA",
          JobLogFile = get_launcher_job_log_file(),
          OutputDir = if (!is.na(out_dir) && nzchar(trimws(out_dir))) out_dir else "NA",
          LocalDir = local_dir,
          LocalExists = condor_completed_local_exists(local_dir),
          RemoteDir = remote_dirs[j],
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0) return(data.frame())
    out <- do.call(rbind, rows)
    out <- out[!duplicated(paste(out$BatchName, out$RemoteDir, out$LaunchAt, sep = "\r")), , drop = FALSE]
    condor_completed_sort_rows(out)
  }

  condor_completed_remote_scan_one <- function(remote_user, remote_host, remote_dir) {
    target <- sprintf("%s@%s", remote_user, remote_host)
    d <- shQuote(remote_dir)
    remote_cmd <- paste(
      paste0("d=", d, ";"),
      "if [ ! -d \"$d\" ]; then echo 'missing|0|||'; exit 0; fi;",
      "archive_count=$(find \"$d\" -maxdepth 3 -type f \\( -name '*.tar.gz' -o -name '*.tgz' \\) 2>/dev/null | wc -l);",
      "latest=$(find \"$d\" -maxdepth 3 -type f \\( -name '*.tar.gz' -o -name '*.tgz' \\) -exec stat -c '%Y|%y|%n' {} \\; 2>/dev/null | sort -nr | head -1);",
      "if [ -n \"$latest\" ]; then printf 'completed|%s|%s\\n' \"$archive_count\" \"$latest\"; exit 0; fi;",
      "latest=$(find \"$d\" -maxdepth 3 -type f \\( -name '*.log' -o -name '*.out' -o -name '*.err' -o -name 'mfcl_log.txt' \\) -exec stat -c '%Y|%y|%n' {} \\; 2>/dev/null | sort -nr | head -1);",
      "if [ -n \"$latest\" ]; then printf 'no archive|0|%s\\n' \"$latest\"; else echo 'no files|0|||'; fi",
      sep = " "
    )

    out <- tryCatch(
      system2("ssh", c(target, remote_cmd), stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 127L)
    )
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    line <- if (length(out) > 0) tail(as.character(out), 1) else ""

    if (as.integer(status) != 0L) {
      return(list(status = "ssh failed", count = 0L, completed_at = NA_character_, completed_file = "", raw = paste(out, collapse = "\n")))
    }

    parts <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(parts) < 5) {
      return(list(status = "parse failed", count = 0L, completed_at = NA_character_, completed_file = "", raw = line))
    }

    status_txt <- parts[1]
    count <- suppressWarnings(as.integer(parts[2]))
    if (!is.finite(count)) count <- 0L
    time_txt <- parts[4]
    path_txt <- parts[5]
    completed_at <- if (nzchar(time_txt)) substr(time_txt, 1, 19) else NA_character_

    list(
      status = status_txt,
      count = count,
      completed_at = completed_at,
      completed_file = path_txt,
      raw = line
    )
  }

  condor_completed_remote_scan_all <- function(remote_user, remote_host, remote_dirs) {
    remote_dirs <- unique(trimws(as.character(remote_dirs)))
    remote_dirs <- remote_dirs[nzchar(remote_dirs) & !grepl("[\r\n\t]", remote_dirs)]
    if (length(remote_dirs) == 0) {
      return(data.frame(
        RemoteDir = character(),
        status = character(),
        count = integer(),
        completed_at = character(),
        completed_file = character(),
        raw = character(),
        stringsAsFactors = FALSE
      ))
    }

    target <- sprintf("%s@%s", remote_user, remote_host)
    dir_block <- paste(remote_dirs, collapse = "\n")
    script <- paste0(
      "while IFS= read -r d; do\n",
      "  [ -z \"$d\" ] && continue\n",
      "  if [ ! -d \"$d\" ]; then printf '%s\\tmissing\\t0\\t\\t\\t\\n' \"$d\"; continue; fi\n",
      "  archive_count=$(find \"$d\" -maxdepth 5 -type f \\( -name '*.tar.gz' -o -name '*.tgz' \\) 2>/dev/null | wc -l)\n",
      "  latest=$(find \"$d\" -maxdepth 5 -type f \\( -name '*.tar.gz' -o -name '*.tgz' \\) -exec stat -c '%Y|%y|%n' {} \\; 2>/dev/null | sort -nr | head -1)\n",
      "  if [ -n \"$latest\" ]; then latest_epoch=${latest%%|*}; latest_rest=${latest#*|}; latest_time=${latest_rest%%|*}; latest_path=${latest_rest#*|}; printf '%s\\tcompleted\\t%s\\t%s\\t%s\\t%s\\n' \"$d\" \"$archive_count\" \"$latest_epoch\" \"$latest_time\" \"$latest_path\"; continue; fi\n",
      "  latest=$(find \"$d\" -maxdepth 5 -type f \\( -name '*.log' -o -name '*.out' -o -name '*.err' -o -name 'mfcl_log.txt' \\) -exec stat -c '%Y|%y|%n' {} \\; 2>/dev/null | sort -nr | head -1)\n",
      "  if [ -n \"$latest\" ]; then latest_epoch=${latest%%|*}; latest_rest=${latest#*|}; latest_time=${latest_rest%%|*}; latest_path=${latest_rest#*|}; printf '%s\\tno archive\\t0\\t%s\\t%s\\t%s\\n' \"$d\" \"$latest_epoch\" \"$latest_time\" \"$latest_path\"; else printf '%s\\tno files\\t0\\t\\t\\t\\n' \"$d\"; fi\n",
      "done <<'CONDOR_COMPLETED_DIRS'\n",
      dir_block,
      "\nCONDOR_COMPLETED_DIRS\n"
    )

    out <- tryCatch(
      system2("ssh", c(target, "bash -s"), input = strsplit(script, "\n", fixed = TRUE)[[1]], stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 127L)
    )
    status_code <- attr(out, "status")
    if (is.null(status_code)) status_code <- 0L
    if (as.integer(status_code) != 0L) {
      return(data.frame(
        RemoteDir = remote_dirs,
        status = "ssh failed",
        count = 0L,
        completed_at = NA_character_,
        completed_file = "",
        raw = paste(as.character(out), collapse = "\n"),
        stringsAsFactors = FALSE
      ))
    }

    lines <- as.character(out)
    lines <- lines[nzchar(lines)]
    rows <- lapply(lines, function(line) {
      parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
      if (length(parts) < 3) {
        return(data.frame(
          RemoteDir = "",
          status = "parse failed",
          count = 0L,
          completed_at = NA_character_,
          completed_file = "",
          raw = line,
          stringsAsFactors = FALSE
        ))
      }
      if (length(parts) < 6) {
        parts <- c(parts, rep("", 6L - length(parts)))
      }
      count <- suppressWarnings(as.integer(parts[3]))
      if (!is.finite(count)) count <- 0L
      time_txt <- parts[5]
      data.frame(
        RemoteDir = parts[1],
        status = parts[2],
        count = count,
        completed_at = if (nzchar(time_txt)) substr(time_txt, 1, 19) else NA_character_,
        completed_file = parts[6],
        raw = line,
        stringsAsFactors = FALSE
      )
    })

    parsed <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
    missing_dirs <- setdiff(remote_dirs, parsed$RemoteDir)
    if (length(missing_dirs) > 0) {
      parsed <- rbind(
        parsed,
        data.frame(
          RemoteDir = missing_dirs,
          status = "parse failed",
          count = 0L,
          completed_at = NA_character_,
          completed_file = "",
          raw = "",
          stringsAsFactors = FALSE
        )
      )
    }
    parsed
  }

  rv$condor_completed <- condor_completed_empty()
  rv$condor_completed_log <- "Click Scan Completed Remote Dirs to compare Condor result archives with the launcher job log."

  output$condor_completed_log <- renderText({
    rv$condor_completed_log
  })

  output$condor_completed_status <- renderText({
    df <- condor_completed_filter_by_window(rv$condor_completed)
    n <- if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
    completed <- if (n > 0 && "Status" %in% names(df)) sum(df$Status == "completed", na.rm = TRUE) else 0L
    total <- if (is.null(rv$condor_completed) || !is.data.frame(rv$condor_completed)) 0L else nrow(rv$condor_completed)
    paste0("Rows: ", n, " / ", total, " | Completed: ", completed)
  })

  output$condor_completed_table <- DT::renderDT({
    df <- condor_completed_filter_by_window(rv$condor_completed)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      df <- condor_completed_empty()
    }
    if (nrow(df) > 0) {
      df$.detail_html <- vapply(seq_len(nrow(df)), function(i) {
        condor_completed_detail_html(df[i, , drop = FALSE])
      }, character(1))
      df$Model <- sprintf(
        "<a href='#' class='condor-model-detail'>%s</a>",
        htmltools::htmlEscape(df$Model)
      )
    } else {
      df$.detail_html <- character()
    }

    hidden_cols <- match(c("JobLog", "ConfigFile", "ConfigDetails", "JobLogFile", "LocalExists", ".detail_html"), names(df)) - 1L
    hidden_cols <- hidden_cols[is.finite(hidden_cols)]

    DT::datatable(
      df,
      rownames = FALSE,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols),
          list(className = "dt-nowrap", targets = "_all")
        )
      ),
      callback = JS(
        "table.on('click', 'a.condor-model-detail', function (e) {",
        "  e.preventDefault();",
        "  var tr = $(this).closest('tr');",
        "  var row = table.row(tr);",
        "  var data = row.data();",
        "  var detailIdx = data.length - 1;",
        "  if (row.child.isShown()) {",
        "    row.child.hide();",
        "    tr.removeClass('shown');",
        "  } else {",
        "    row.child(data[detailIdx]).show();",
        "    tr.addClass('shown');",
        "  }",
        "});"
      ),
      class = "compact stripe hover nowrap"
    ) |>
      DT::formatStyle(
        "Status",
        color = DT::styleEqual(
          c("completed", "not completed", "scan needed", "ssh failed", "parse failed"),
          c("#0f5132", "#842029", "#41464b", "#664d03", "#842029")
        ),
        backgroundColor = DT::styleEqual(
          c("completed", "not completed", "scan needed", "ssh failed", "parse failed"),
          c("#d1e7dd", "#f8d7da", "#e2e3e5", "#fff3cd", "#f8d7da")
        ),
        fontWeight = "700"
      ) |>
      DT::formatStyle(
        "Duration",
        color = DT::styleEqual("NA", "#6c757d")
      ) |>
      DT::formatStyle(
        "RemoteHost",
        color = DT::styleEqual("NA", "#6c757d")
      ) |>
      DT::formatStyle(
        "LocalDir",
        color = DT::styleEqual(c("yes", "no"), c("#0f5132", "#842029")),
        backgroundColor = DT::styleEqual(c("yes", "no"), c("#d1e7dd", "#f8d7da")),
        fontWeight = "700",
        valueColumns = "LocalExists",
        target = "cell"
      )
  })

  observeEvent(input$condor_completed_refresh_log, {
    previous <- if (is.data.frame(rv$condor_completed) && nrow(rv$condor_completed) > 0) {
      rv$condor_completed
    } else {
      condor_completed_empty()
    }
    expected <- condor_completed_build_expected()
    if (nrow(expected) == 0) {
      rv$condor_completed <- condor_completed_empty()
      rv$condor_completed_log <- paste(Sys.time(), "- No Condor remote dirs found in launcher_job_log.rds.")
      showNotification("No Condor job-log rows found.", type = "warning", duration = 3)
    } else {
      previous_key <- if (nrow(previous) > 0) {
        paste(previous$BatchName, previous$RemoteDir, previous$LaunchAt, sep = "\r")
      } else {
        character(0)
      }
      expected_key <- paste(expected$BatchName, expected$RemoteDir, expected$LaunchAt, sep = "\r")
      previous_idx <- match(expected_key, previous_key)
      previous_value <- function(column, default) {
        out <- rep(default, nrow(expected))
        ok <- !is.na(previous_idx) & column %in% names(previous)
        if (any(ok)) out[ok] <- as.character(previous[[column]][previous_idx[ok]])
        out
      }

      reloaded <- data.frame(
        Model = expected$Model,
        JobLog = expected$JobLog,
        JobType = expected$JobType,
        BatchName = expected$BatchName,
        LaunchAt = expected$LaunchAt,
        CompletedAt = previous_value("CompletedAt", "NA"),
        Duration = previous_value("Duration", "NA"),
        Status = previous_value("Status", "scan needed"),
        RemoteHost = previous_value("RemoteHost", "NA"),
        Description = expected$Description,
        ConfigFile = expected$ConfigFile,
        ConfigDetails = expected$ConfigDetails,
        JobLogFile = expected$JobLogFile,
        OutputDir = expected$OutputDir,
        LocalDir = expected$LocalDir,
        LocalExists = expected$LocalExists,
        RemoteDir = expected$RemoteDir,
        stringsAsFactors = FALSE
      )
      rv$condor_completed <- condor_completed_sort_rows(reloaded)
      rv$condor_completed_log <- paste(Sys.time(), "- Reloaded launcher job log:", nrow(expected), "remote dirs ready to scan.")
      showNotification(paste("Found", nrow(expected), "remote dirs in job log."), type = "message", duration = 3)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$condor_completed_scan, {
    expected_all <- condor_completed_build_expected()
    expected <- condor_completed_filter_expected_by_window(expected_all)
    if (nrow(expected) == 0) {
      rv$condor_completed <- condor_completed_empty()
      rv$condor_completed_log <- paste(Sys.time(), "- No Condor remote dirs found in selected launch-time window.")
      showNotification("No Condor job-log rows found.", type = "warning", duration = 4)
      return()
    }

    remote_user <- tryCatch(condor_completed_first_string(input$remote_user), error = function(e) "")
    remote_hosts <- tryCatch(condor_completed_scan_hosts(input$remote_host), error = function(e) character(0))
    if (!nzchar(remote_user) || length(remote_hosts) == 0) {
      rv$condor_completed_log <- paste(Sys.time(), "- Remote user/host is missing.")
      showNotification("Remote user/host is missing.", type = "error", duration = 4)
      return()
    }

    shinyjs::disable("condor_completed_scan")
    on.exit(shinyjs::enable("condor_completed_scan"), add = TRUE)
    showNotification("Scanning remote Condor result archives...", type = "message", duration = NULL, id = "condor_completed_scan_progress")

    log_lines <- c(
      paste(Sys.time(), "- Scanning Condor completed files"),
      paste("Remote hosts:", paste(paste0(remote_user, "@", remote_hosts), collapse = ", ")),
      paste("Remote dirs from job log:", nrow(expected), "/", nrow(expected_all))
    )

    scan_by_host <- lapply(remote_hosts, function(host) {
      res <- condor_completed_remote_scan_all(remote_user, host, expected$RemoteDir)
      res$Host <- host
      res
    })
    scan_results <- do.call(rbind, scan_by_host)

    out_rows <- vector("list", nrow(expected))
    for (i in seq_len(nrow(expected))) {
      spec <- expected[i, , drop = FALSE]
      res_rows <- scan_results[scan_results$RemoteDir == spec$RemoteDir, , drop = FALSE]
      res_row <- NULL
      if (nrow(res_rows) > 0) {
        completed_idx <- which(res_rows$status == "completed")
        if (length(completed_idx) > 0) {
          res_row <- res_rows[completed_idx[1], , drop = FALSE]
        } else {
          order_status <- match(res_rows$status, c("no archive", "no files", "missing", "ssh failed", "parse failed"))
          order_status[is.na(order_status)] <- 99L
          res_row <- res_rows[order(order_status), , drop = FALSE][1, , drop = FALSE]
        }
      }
      res <- if (is.null(res_row) || nrow(res_row) == 0) {
        list(status = "parse failed", count = 0L, completed_at = NA_character_, completed_file = "", host = "NA")
      } else {
        list(
          status = res_row$status[1],
          count = res_row$count[1],
          completed_at = res_row$completed_at[1],
          completed_file = res_row$completed_file[1],
          host = res_row$Host[1]
        )
      }
      start_time <- condor_completed_parse_time(spec$LaunchAt)
      end_time <- condor_completed_parse_time(res$completed_at)
      duration <- condor_completed_format_duration(start_time, end_time)

      out_rows[[i]] <- data.frame(
        Model = spec$Model,
        JobLog = spec$JobLog,
        JobType = spec$JobType,
        BatchName = spec$BatchName,
        LaunchAt = spec$LaunchAt,
        CompletedAt = if (!is.na(res$completed_at) && nzchar(res$completed_at)) res$completed_at else "NA",
        Duration = duration,
        Status = condor_completed_status_label(res$status, res$count),
        RemoteHost = res$host,
        Description = spec$Description,
        ConfigFile = spec$ConfigFile,
        ConfigDetails = spec$ConfigDetails,
        JobLogFile = spec$JobLogFile,
        OutputDir = spec$OutputDir,
        LocalDir = spec$LocalDir,
        LocalExists = spec$LocalExists,
        RemoteDir = spec$RemoteDir,
        stringsAsFactors = FALSE
      )

      if (i %% 100 == 0 || i == nrow(expected)) {
        rv$condor_completed_log <- paste(c(log_lines, paste("Prepared rows:", i, "/", nrow(expected))), collapse = "\n")
      }
    }

    scanned <- do.call(rbind, out_rows)
    scanned <- condor_completed_sort_rows(scanned)
    rv$condor_completed <- scanned

    status_counts <- table(scanned$Status)
    log_lines <- c(
      log_lines,
      paste("Scan complete:", nrow(scanned), "rows"),
      paste("Status:", paste(names(status_counts), as.integer(status_counts), sep = "=", collapse = ", "))
    )
    rv$condor_completed_log <- paste(log_lines, collapse = "\n")
    removeNotification("condor_completed_scan_progress")
    showNotification("Condor completed scan finished.", type = "message", duration = 4)
  }, ignoreInit = TRUE)
