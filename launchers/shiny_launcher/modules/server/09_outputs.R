  # ========== OUTPUT DISPLAYS ==========
  
  output$config_status <- renderText({ rv$config_status_msg })
  output$launch_log <- renderText({ rv$launch_log })
  
  output$models_summary <- renderText({
    if (length(rv$models) == 0) return("No models loaded.")
    
    summary_lines <- lapply(names(rv$models), function(nm) {
      m <- rv$models[[nm]]
      desc_line <- if (!is.null(m$description) && m$description != "") {
        paste0("  Description: ", m$description, "\n")
      } else {
        ""
      }
      paste0("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", "Model: ", nm, "\n", 
             desc_line, "  Program: ", m$program_path, "\n")
    })
    paste(summary_lines, collapse = "\n")
  })

  output$launcher_job_log_status <- renderText({
    rv$launcher_job_log_trigger
    log_df <- load_launcher_job_log()
    if (nrow(log_df) == 0) {
      "No launch jobs logged yet."
    } else {
      selected_count <- if (is.null(input$launcher_job_log_selected_row_ids)) 0L else length(input$launcher_job_log_selected_row_ids)
      paste0("Total runs logged: ", nrow(log_df), " | Selected: ", selected_count)
    }
  })

  output$launcher_job_log_table <- DT::renderDT({
    rv$launcher_job_log_trigger
    log_df <- load_launcher_job_log()
    if (nrow(log_df) == 0) {
      log_df <- data.frame(
        Message = "No launch jobs logged yet.",
        stringsAsFactors = FALSE
      )
      return(DT::datatable(
        log_df,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE),
        rownames = FALSE
      ))
    }

    row_ids <- seq_len(nrow(log_df))

    if ("summary" %in% names(log_df)) {
      log_df$summary[is.na(log_df$summary) | !nzchar(trimws(as.character(log_df$summary)))] <- "NA"
    }
    if ("run_description" %in% names(log_df)) {
      log_df$run_description[is.na(log_df$run_description) | !nzchar(trimws(as.character(log_df$run_description)))] <- "NA"
    }
    if ("config_file" %in% names(log_df)) {
      log_df$config_file[is.na(log_df$config_file) | !nzchar(trimws(as.character(log_df$config_file)))] <- "NA"
    }
    if ("output_dir" %in% names(log_df)) {
      log_df$output_dir[is.na(log_df$output_dir) | !nzchar(trimws(as.character(log_df$output_dir)))] <- "NA"
    }

    details_raw <- if ("config_details" %in% names(log_df)) {
      as.character(log_df$config_details)
    } else {
      rep("NA", nrow(log_df))
    }
    remote_dirs_raw <- if ("remote_dirs" %in% names(log_df)) {
      as.character(log_df$remote_dirs)
    } else {
      rep("NA", nrow(log_df))
    }
    job_types_raw <- if ("job_types" %in% names(log_df)) {
      as.character(log_df$job_types)
    } else {
      rep("", nrow(log_df))
    }

    show_cols <- intersect(
      c("run_at", "output_dir", "summary", "run_description", "config_file", "job_types", "model_names", "total_jobs", "launch_mode", "status", "branch"),
      names(log_df)
    )
    if (length(show_cols) == 0) {
      show_cols <- names(log_df)
    }
    if (length(show_cols) == 0) {
      return(DT::datatable(
        data.frame(Message = "Job log columns could not be rendered.", stringsAsFactors = FALSE),
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE),
        rownames = FALSE
      ))
    }

    display_df <- log_df[, show_cols, drop = FALSE]
    pretty_name_map <- c(
      run_at = "Run At",
      output_dir = "Output Directory",
      summary = "Summary",
      run_description = "Run Description",
      config_file = "Config File",
      job_types = "Job Types",
      model_names = "Models",
      total_jobs = "Total Jobs",
      launch_mode = "Mode",
      status = "Status",
      branch = "Branch"
    )
    display_names <- vapply(show_cols, function(x) {
      if (x %in% names(pretty_name_map)) pretty_name_map[[x]] else x
    }, character(1))
    names(display_df) <- display_names
    display_df$.row_id <- row_ids
    log_df <- display_df[rev(seq_len(nrow(display_df))), , drop = FALSE]
    details_raw <- details_raw[rev(seq_len(length(details_raw)))]
    remote_dirs_raw <- remote_dirs_raw[rev(seq_len(length(remote_dirs_raw)))]
    job_types_raw <- job_types_raw[rev(seq_len(length(job_types_raw)))]

    output_dir_detail_html <- rep(
      "<div style='font-size:12px; color:#666;'>Output directory detail unavailable.</div>",
      nrow(log_df)
    )

    if ("Output Directory" %in% names(log_df)) {
      github_repo <- trimws(as.character(if (is.null(input$github_repo)) "" else input$github_repo))
      output_dir_vals <- as.character(log_df[["Output Directory"]])
      output_dir_vals[is.na(output_dir_vals)] <- ""
      remote_dirs_raw[is.na(remote_dirs_raw)] <- ""

      summarize_numeric_group <- function(vals, label) {
        if (length(vals) == 0) return(NULL)
        vals <- sort(unique(vals[is.finite(vals)]))
        if (length(vals) == 0) return(NULL)
        paste0(label, "(", length(vals), "): ", paste(vals, collapse = ", "))
      }

      build_remote_dirs_html <- function(remote_dirs_txt, job_types_txt = "") {
        txt <- trimws(remote_dirs_txt)
        if (!nzchar(txt) || identical(txt, "NA")) {
          return("<div style='font-size:12px; color:#777;'>No remote job directories recorded.</div>")
        }
        dirs <- trimws(unlist(strsplit(txt, ",", fixed = TRUE)))
        dirs <- dirs[nzchar(dirs)]
        if (length(dirs) == 0) {
          return("<div style='font-size:12px; color:#777;'>No remote job directories recorded.</div>")
        }

        allowed_types <- tolower(trimws(unlist(strsplit(as.character(job_types_txt), ",", fixed = TRUE))))
        allowed_types <- unique(allowed_types[nzchar(allowed_types)])
        base_names <- basename(dirs)
        is_prof_chain <- grepl("_profchain(_|$)", base_names)
        if (length(allowed_types) > 0) {
          keep <- rep(FALSE, length(base_names))
          if ("model" %in% allowed_types) keep <- keep | grepl("_model$", base_names)
          if ("jitter" %in% allowed_types) keep <- keep | grepl("_seed\\d+$", base_names)
          if ("hessian" %in% allowed_types) keep <- keep | grepl("_part\\d+$", base_names)
          if ("retro" %in% allowed_types) keep <- keep | grepl("_peel\\d+$", base_names)
          if ("prof" %in% allowed_types || "profile" %in% allowed_types || "prof_2d" %in% allowed_types) {
            keep <- keep | grepl("_sc\\d+$", base_names) | is_prof_chain | grepl("_prof2d$", base_names)
          }
          if (any(keep)) {
            dirs <- dirs[keep]
            base_names <- base_names[keep]
            is_prof_chain <- is_prof_chain[keep]
          }
        }

        if (length(dirs) == 0) {
          return("<div style='font-size:12px; color:#777;'>No matching directories for selected job types.</div>")
        }

        seed_vals <- suppressWarnings(as.integer(sub(".*_seed(\\d+)$", "\\1", base_names[grepl("_seed\\d+$", base_names)])))
        part_vals <- suppressWarnings(as.integer(sub(".*_part(\\d+)$", "\\1", base_names[grepl("_part\\d+$", base_names)])))
        peel_vals <- suppressWarnings(as.integer(sub(".*_peel(\\d+)$", "\\1", base_names[grepl("_peel\\d+$", base_names)])))
        sc_vals <- suppressWarnings(as.integer(sub(".*_sc(\\d+)$", "\\1", base_names[grepl("_sc\\d+$", base_names)])))
        prof_chain_count <- sum(is_prof_chain)
        model_count <- sum(grepl("_model$", base_names))
        other_count <- sum(!(grepl("_seed\\d+$", base_names) |
                             grepl("_part\\d+$", base_names) |
                             grepl("_peel\\d+$", base_names) |
                             grepl("_sc\\d+$", base_names) |
                             is_prof_chain |
                             grepl("_model$", base_names)))

        summary_lines <- c(
          if (model_count > 0) paste0("model: ", model_count) else NULL,
          summarize_numeric_group(seed_vals, "seed"),
          summarize_numeric_group(part_vals, "part"),
          summarize_numeric_group(peel_vals, "peel"),
          summarize_numeric_group(sc_vals, "scalar"),
          if (prof_chain_count > 0) paste0("prof_chain: ", prof_chain_count) else NULL,
          if (other_count > 0) paste0("other: ", other_count) else NULL
        )
        summary_html <- if (length(summary_lines) > 0) {
          paste0(
            "<ul style='margin:0 0 8px 18px; padding:0;'>",
            paste0("<li>", htmltools::htmlEscape(summary_lines), "</li>", collapse = ""),
            "</ul>"
          )
        } else {
          "<div style='font-size:12px; color:#777; margin-bottom:8px;'>No summary available.</div>"
        }

        full_list_html <- paste0(
          "<details>",
          "<summary style='cursor:pointer; color:#2c6d8c;'>Show full directory list (", length(dirs), ")</summary>",
          "<div style='max-height:220px; overflow:auto; margin-top:6px; padding:8px; background:#fff; border:1px solid #e3edf5; border-radius:3px;'>",
          "<ul style='margin:0; padding-left:18px;'>",
          paste0("<li><code>", htmltools::htmlEscape(dirs), "</code></li>", collapse = ""),
          "</ul>",
          "</div>",
          "</details>"
        )

        paste0(
          "<div style='margin-bottom:6px;'><b>Summary</b></div>",
          summary_html,
          full_list_html
        )
      }

      full_path_vals <- vapply(output_dir_vals, function(x) {
        txt <- trimws(x)
        if (!nzchar(txt) || identical(txt, "NA")) return("NA")
        if (startsWith(txt, "/") || startsWith(txt, "~")) return(txt)
        if (nzchar(github_repo)) {
          paste0(gsub("/+$", "", github_repo), "/", gsub("^/+", "", txt))
        } else {
          txt
        }
      }, character(1))

      log_df[["Output Directory"]] <- vapply(output_dir_vals, function(x) {
        txt <- trimws(x)
        if (!nzchar(txt) || identical(txt, "NA")) return("NA")
        paste0(
          "<a href='#' class='show-output-dir-detail'>",
          htmltools::htmlEscape(txt),
          "</a>"
        )
      }, character(1))

      output_dir_detail_html <- vapply(seq_along(full_path_vals), function(i) {
        full_path <- full_path_vals[[i]]
        remote_dirs <- trimws(remote_dirs_raw[[i]])
        job_types_txt <- if (i <= length(job_types_raw)) job_types_raw[[i]] else ""
        remote_dirs_html <- build_remote_dirs_html(remote_dirs, job_types_txt = job_types_txt)
        paste0(
          "<div style='padding:10px 12px; background:#f7fbff; border:1px solid #d9e7f3; border-radius:4px; font-size:12px;'>",
          "<div style='margin-bottom:6px;'><b>Full Directory</b>: <code>", htmltools::htmlEscape(full_path), "</code></div>",
          "<div style='margin-top:8px;'><b>Remote Job Directories</b></div>",
          "<div style='margin-top:6px;'>", remote_dirs_html, "</div>",
          "</div>"
        )
      }, character(1))
    }

    details_html <- vapply(details_raw, function(x) {
      txt <- if (is.na(x) || !nzchar(trimws(x))) "NA" else x
      paste0(
        "<div style='max-height:320px; overflow:auto; white-space:pre-wrap;",
        "font-family: Menlo, Consolas, monospace; font-size:12px;'>",
        htmltools::htmlEscape(txt),
        "</div>"
      )
    }, character(1))

    log_df <- cbind(
      `Select` = sprintf(
        "<input type='checkbox' class='joblog-select-checkbox' value='%s'/>",
        as.integer(log_df$.row_id)
      ),
      `Details` = "<span class='details-control' style='cursor:pointer; color:#337ab7;'>▶</span>",
      log_df,
      `.output_dir_detail_html` = output_dir_detail_html,
      `.row_id_hidden` = as.integer(log_df$.row_id),
      `.details_html` = details_html,
      stringsAsFactors = FALSE
    )
    log_df$.row_id <- NULL

    hidden_output_detail_col <- ncol(log_df) - 3L
    hidden_row_id_col <- ncol(log_df) - 2L
    hidden_detail_col <- ncol(log_df) - 1L

    DT::datatable(
      log_df,
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(2, "desc")),
        columnDefs = list(
          list(orderable = FALSE, targets = c(0, 1)),
          list(className = "dt-center", targets = c(0, 1)),
          list(visible = FALSE, targets = hidden_output_detail_col),
          list(visible = FALSE, targets = hidden_row_id_col),
          list(visible = FALSE, targets = hidden_detail_col)
        )
      ),
      rownames = FALSE,
      callback = JS(
        "var checkedMap = {};",
        "var syncCheckedToShiny = function() {",
        "  var selected = Object.keys(checkedMap)",
        "    .filter(function(key) { return checkedMap[key]; })",
        "    .map(function(key) { return parseInt(key, 10); })",
        "    .sort(function(a, b) { return a - b; });",
        "  Shiny.setInputValue('launcher_job_log_selected_row_ids', selected, {priority: 'event'});",
        "};",
        "var restoreCheckedState = function() {",
        "  table.$('.joblog-select-checkbox', {page: 'current'}).each(function(){",
        "    this.checked = !!checkedMap[this.value];",
        "  });",
        "};",
        "table.on('change', '.joblog-select-checkbox', function() {",
        "  checkedMap[this.value] = this.checked;",
        "  syncCheckedToShiny();",
        "});",
        "table.on('draw.dt', function() {",
        "  restoreCheckedState();",
        "  syncCheckedToShiny();",
        "});",
        "restoreCheckedState();",
        "syncCheckedToShiny();",
        "table.on('click', 'a.show-output-dir-detail', function (e) {",
        "  e.preventDefault();",
        "  var tr = $(this).closest('tr');",
        "  var row = table.row(tr);",
        "  var data = row.data();",
        "  var outputDetailIdx = data.length - 3;",
        "  var html = data[outputDetailIdx];",
        "  if (row.child.isShown() && tr.hasClass('output-detail-shown')) {",
        "    row.child.hide();",
        "    tr.removeClass('output-detail-shown');",
        "  } else {",
        "    row.child(html).show();",
        "    tr.addClass('output-detail-shown');",
        "  }",
        "});",
        "table.on('click', 'span.details-control', function () {",
        "  var tr = $(this).closest('tr');",
        "  var row = table.row(tr);",
        "  var data = row.data();",
        "  var detailIdx = data.length - 1;",
        "  if (row.child.isShown()) {",
        "    row.child.hide();",
        "    $(this).text('▶');",
        "    tr.removeClass('shown');",
        "    tr.removeClass('output-detail-shown');",
        "  } else {",
        "    row.child(data[detailIdx]).show();",
        "    $(this).text('▼');",
        "    tr.addClass('shown');",
        "  }",
        "});"
      )
    )
  })

  observeEvent(input$refresh_launcher_job_log, {
    rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1
    showNotification("Job log refreshed.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  observeEvent(input$delete_selected_launcher_job_log, {
    selected_row_ids <- if (is.null(input$launcher_job_log_selected_row_ids)) integer(0) else input$launcher_job_log_selected_row_ids
    if (length(selected_row_ids) == 0) {
      showNotification("No log rows selected.", type = "warning", duration = 3)
      return()
    }

    log_df <- load_launcher_job_log()
    idx <- suppressWarnings(as.integer(selected_row_ids))
    idx <- unique(idx[is.finite(idx)])
    idx <- idx[idx >= 1 & idx <= nrow(log_df)]
    if (length(idx) == 0) {
      showNotification("Selected rows are invalid.", type = "warning", duration = 3)
      return()
    }

    if (!isTRUE(input$joblog_delete_with_remote_dir)) {
      removed_n <- delete_launcher_job_log_rows(idx)
      rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1
      showNotification(
        paste0("Deleted ", removed_n, " selected log row(s)."),
        type = "message",
        duration = 3
      )
      return()
    }

    github_repo <- trimws(as.character(if (is.null(input$github_repo)) "" else input$github_repo))
    remote_paths <- normalize_remote_output_paths(log_df$output_dir[idx], github_repo = github_repo)

    showModal(modalDialog(
      title = "Delete Selected Logs and Remote Output Directories",
      paste0(
        "Delete ", length(idx), " selected log row(s) and ",
        length(remote_paths), " remote output director", if (length(remote_paths) == 1) "y" else "ies", "?"
      ),
      if (length(remote_paths) > 0) {
        tags$div(
          style = "max-height:180px; overflow:auto; background:#f9f9f9; border:1px solid #ddd; border-radius:4px; padding:8px; font-size:12px;",
          tags$ul(style = "margin:0; padding-left:18px;",
                  lapply(remote_paths, function(p) tags$li(tags$code(p))))
        )
      },
      tags$p(style = "color:#d9534f; margin-top:10px;", "This action is destructive and cannot be undone."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_selected_launcher_job_log", "Delete", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  normalize_remote_output_paths <- function(output_dirs, github_repo) {
    vals <- trimws(as.character(output_dirs))
    vals <- vals[nzchar(vals) & vals != "NA"]
    if (length(vals) == 0) return(character(0))

    out <- vapply(vals, function(v) {
      if (startsWith(v, "/") || startsWith(v, "~")) return(v)
      if (nzchar(github_repo)) {
        paste0(gsub("/+$", "", github_repo), "/", gsub("^/+", "", v))
      } else {
        v
      }
    }, character(1))
    out <- unique(trimws(out))
    out <- out[nzchar(out)]
    out <- out[!out %in% c("/", "~", ".", "..")]
    out
  }

  delete_remote_output_paths <- function(paths, remote_user, remote_host) {
    if (length(paths) == 0) return(list(ok = 0L, fail = 0L, failed_paths = character(0)))
    if (!nzchar(remote_user) || !nzchar(remote_host)) {
      return(list(ok = 0L, fail = length(paths), failed_paths = paths))
    }

    ok <- 0L
    failed <- character(0)
    for (p in paths) {
      remote_cmd <- sprintf("rm -rf -- %s", shQuote(p))
      cmd <- sprintf("ssh %s@%s %s", remote_user, remote_host, shQuote(remote_cmd))
      status <- tryCatch(
        suppressWarnings(system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)),
        error = function(e) 1
      )
      if (identical(status, 0L)) {
        ok <- ok + 1L
      } else {
        failed <- c(failed, p)
      }
    }
    list(ok = ok, fail = length(failed), failed_paths = failed)
  }

  observeEvent(input$confirm_delete_selected_launcher_job_log, {
    removeModal()
    selected_row_ids <- if (is.null(input$launcher_job_log_selected_row_ids)) integer(0) else input$launcher_job_log_selected_row_ids
    log_df <- load_launcher_job_log()
    idx <- suppressWarnings(as.integer(selected_row_ids))
    idx <- unique(idx[is.finite(idx)])
    idx <- idx[idx >= 1 & idx <= nrow(log_df)]
    if (length(idx) == 0) {
      showNotification("No valid selected rows to delete.", type = "warning", duration = 3)
      return()
    }

    github_repo <- trimws(as.character(if (is.null(input$github_repo)) "" else input$github_repo))
    remote_user <- trimws(as.character(if (is.null(input$remote_user)) "" else input$remote_user))
    remote_host <- trimws(as.character(if (is.null(input$remote_host)) "" else input$remote_host))
    remote_paths <- normalize_remote_output_paths(log_df$output_dir[idx], github_repo = github_repo)
    dir_del <- delete_remote_output_paths(remote_paths, remote_user = remote_user, remote_host = remote_host)

    removed_n <- delete_launcher_job_log_rows(idx)
    rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1

    showNotification(
      paste0(
        "Deleted log rows: ", removed_n,
        " | Remote dirs deleted: ", dir_del$ok,
        if (dir_del$fail > 0) paste0(" | Remote dir delete failed: ", dir_del$fail) else ""
      ),
      type = if (dir_del$fail > 0) "warning" else "message",
      duration = 6
    )
  }, ignoreInit = TRUE)

  observeEvent(input$clear_launcher_job_log, {
    log_df <- load_launcher_job_log()
    use_remote_delete <- isTRUE(input$joblog_delete_with_remote_dir)
    github_repo <- trimws(as.character(if (is.null(input$github_repo)) "" else input$github_repo))
    remote_paths <- if (use_remote_delete) normalize_remote_output_paths(log_df$output_dir, github_repo = github_repo) else character(0)

    showModal(modalDialog(
      title = "Clear Job Log",
      if (use_remote_delete) {
        paste0(
          "Delete all ", nrow(log_df), " log row(s) and ",
          length(remote_paths), " remote output director", if (length(remote_paths) == 1) "y" else "ies", "?"
        )
      } else {
        "Delete all records in the launcher job log?"
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_launcher_job_log", "Clear All", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_clear_launcher_job_log, {
    removeModal()
    use_remote_delete <- isTRUE(input$joblog_delete_with_remote_dir)
    log_df <- load_launcher_job_log()
    github_repo <- trimws(as.character(if (is.null(input$github_repo)) "" else input$github_repo))
    remote_user <- trimws(as.character(if (is.null(input$remote_user)) "" else input$remote_user))
    remote_host <- trimws(as.character(if (is.null(input$remote_host)) "" else input$remote_host))

    dir_del <- list(ok = 0L, fail = 0L)
    if (use_remote_delete) {
      remote_paths <- normalize_remote_output_paths(log_df$output_dir, github_repo = github_repo)
      dir_del <- delete_remote_output_paths(remote_paths, remote_user = remote_user, remote_host = remote_host)
    }

    clear_launcher_job_log()
    rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1
    showNotification(
      if (use_remote_delete) {
        paste0(
          "Launcher job log cleared | Remote dirs deleted: ", dir_del$ok,
          if (dir_del$fail > 0) paste0(" | Remote dir delete failed: ", dir_del$fail) else ""
        )
      } else {
        "Launcher job log cleared."
      },
      type = if (use_remote_delete && dir_del$fail > 0) "warning" else "message",
      duration = 5
    )
  }, ignoreInit = TRUE)
