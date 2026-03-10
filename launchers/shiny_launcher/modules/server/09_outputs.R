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
      paste0("Total runs logged: ", nrow(log_df))
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

    show_cols <- intersect(
      c("run_at", "output_dir", "summary", "run_description", "config_file", "job_types", "model_names", "launch_mode", "status", "branch"),
      names(log_df)
    )
    log_df <- log_df[, show_cols, drop = FALSE]
    log_df <- log_df[rev(seq_len(nrow(log_df))), , drop = FALSE]
    details_raw <- details_raw[rev(seq_len(length(details_raw)))]
    names(log_df) <- c(
      "Run At",
      "Output Directory",
      "Summary",
      "Run Description",
      "Config File",
      "Job Types",
      "Models",
      "Mode",
      "Status",
      "Branch"
    )[seq_along(show_cols)]

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
      `Details` = "<span class='details-control' style='cursor:pointer; color:#337ab7;'>▶</span>",
      log_df,
      `.details_html` = details_html,
      stringsAsFactors = FALSE
    )

    hidden_detail_col <- ncol(log_df) - 1L

    DT::datatable(
      log_df,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(1, "desc")),
        columnDefs = list(
          list(orderable = FALSE, targets = 0),
          list(visible = FALSE, targets = hidden_detail_col)
        )
      ),
      rownames = FALSE,
      callback = JS(
        "table.on('click', 'span.details-control', function () {",
        "  var tr = $(this).closest('tr');",
        "  var row = table.row(tr);",
        "  var data = row.data();",
        "  var detailIdx = data.length - 1;",
        "  if (row.child.isShown()) {",
        "    row.child.hide();",
        "    $(this).text('▶');",
        "    tr.removeClass('shown');",
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
  
