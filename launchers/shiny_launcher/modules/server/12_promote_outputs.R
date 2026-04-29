  promote_script <- file.path(repo_root_default, "tools", "promote_model_outputs.R")
  if (file.exists(promote_script)) {
    source(promote_script, local = TRUE)
  }

  promote_plan_current <- function() {
    if (!exists("pmo_plan_promotions", mode = "function")) {
      stop("Promotion script not available: ", promote_script)
    }
    pmo_plan_promotions(
      source_dirs = input$promote_source_dir,
      repo_root = repo_root_val()
    )
  }

  rv$promote_plan <- data.frame()
  rv$promote_log <- "Click Scan to preview which output files will update mfcl/inputs."
  rv$promote_run_status <- "Idle"

  output$promote_log <- renderText({
    rv$promote_log
  })

  output$promote_selected_status <- renderText({
    n <- if (is.null(input$promote_selected_row_ids)) 0L else length(input$promote_selected_row_ids)
    paste0("Selected: ", n)
  })

  output$promote_run_status <- renderText({
    paste0("Status: ", rv$promote_run_status)
  })

  render_promote_table <- function(plan) {
    if (is.null(plan) || nrow(plan) == 0) {
      return(DT::datatable(
        data.frame(Message = "No model_info.rds files found in the selected output directory.", stringsAsFactors = FALSE),
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE),
        rownames = FALSE
      ))
    }

    display <- plan[, intersect(
      c(
        "model", "output_folder", "run_at", "input_target",
        "par_source_state", "par_target_state", "par_action",
        "indepvar_source_state", "indepvar_target_state", "indepvar_action"
      ),
      names(plan)
    ), drop = FALSE]
    names(display) <- c(
      "Model", "Output Folder", "Run Built At", "Input Target",
      "Output PAR", "Input PAR", "PAR Action",
      "Output indepvar", "Input indepvar", "indepvar Action"
    )[seq_along(names(display))]
    state_cols <- intersect(c("Output PAR", "Input PAR", "Output indepvar", "Input indepvar"), names(display))
    for (col in state_cols) {
      helper <- paste0(".", make.names(col), "_state")
      vals <- as.character(display[[col]])
      display[[helper]] <- ifelse(
        grepl("^exists|^in payload", vals),
        "present",
        ifelse(identical(col, "Output indepvar") & vals == "missing", "missing_source", ifelse(vals == "missing", "missing", "other"))
      )
    }
    display$.row_id <- seq_len(nrow(display))
    display <- cbind(
      Select = sprintf(
        "<input type='checkbox' class='promote-select-checkbox' value='%s'/>",
        as.integer(display$.row_id)
      ),
      display,
      stringsAsFactors = FALSE
    )
    display$.row_id <- NULL

    hidden_cols <- grep("^\\.", names(display)) - 1L

    table <- DT::datatable(
      display,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, targets = 0),
          list(className = "dt-center", targets = 0),
          list(className = "dt-nowrap", targets = "_all"),
          list(visible = FALSE, targets = hidden_cols)
        )
      ),
      class = "compact stripe hover nowrap",
      callback = JS(
        "var checkedMap = {};",
        "var syncCheckedToShiny = function() {",
        "  var selected = Object.keys(checkedMap)",
        "    .filter(function(key) { return checkedMap[key]; })",
        "    .map(function(key) { return parseInt(key, 10); })",
        "    .sort(function(a, b) { return a - b; });",
        "  Shiny.setInputValue('promote_selected_row_ids', selected, {priority: 'event'});",
        "};",
        "var restoreCheckedState = function() {",
        "  table.$('.promote-select-checkbox', {page: 'current'}).each(function(){",
        "    this.checked = !!checkedMap[this.value];",
        "  });",
        "};",
        "table.on('change', '.promote-select-checkbox', function() {",
        "  checkedMap[this.value] = this.checked;",
        "  syncCheckedToShiny();",
        "});",
        "table.on('draw.dt', function() {",
        "  restoreCheckedState();",
        "  syncCheckedToShiny();",
        "});",
        "restoreCheckedState();",
        "syncCheckedToShiny();"
      )
    )

    table |>
      DT::formatStyle(
        c("PAR Action", "indepvar Action"),
        color = DT::styleEqual(
          c("updated", "update available", "up to date", "no source", "no input target", "failed", "skipped"),
          c("#0f5132", "#664d03", "#0f5132", "#842029", "#842029", "#842029", "#41464b")
        ),
        backgroundColor = DT::styleEqual(
          c("updated", "update available", "up to date", "no source", "no input target", "failed", "skipped"),
          c("#d1e7dd", "#fff3cd", "#e8f5e9", "#f8d7da", "#f8d7da", "#f8d7da", "#e2e3e5")
        ),
        fontWeight = "700"
      ) |>
      DT::formatStyle(
        "Output PAR",
        color = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#0f5132", "#842029", "#842029", "#41464b")),
        backgroundColor = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#e8f5e9", "#f8d7da", "#f8d7da", "#e2e3e5")),
        fontWeight = DT::styleEqual(c("missing", "missing_source"), c("700", "700")),
        valueColumns = ".Output.PAR_state"
      ) |>
      DT::formatStyle(
        "Input PAR",
        color = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#0f5132", "#842029", "#842029", "#41464b")),
        backgroundColor = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#e8f5e9", "#f8d7da", "#f8d7da", "#e2e3e5")),
        fontWeight = DT::styleEqual(c("missing", "missing_source"), c("700", "700")),
        valueColumns = ".Input.PAR_state"
      ) |>
      DT::formatStyle(
        "Output indepvar",
        color = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#0f5132", "#842029", "#842029", "#41464b")),
        backgroundColor = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#e8f5e9", "#f8d7da", "#f8d7da", "#e2e3e5")),
        fontWeight = DT::styleEqual(c("missing", "missing_source"), c("700", "700")),
        valueColumns = ".Output.indepvar_state"
      ) |>
      DT::formatStyle(
        "Input indepvar",
        color = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#0f5132", "#842029", "#842029", "#41464b")),
        backgroundColor = DT::styleEqual(c("present", "missing", "missing_source", "other"), c("#e8f5e9", "#f8d7da", "#f8d7da", "#e2e3e5")),
        fontWeight = DT::styleEqual(c("missing", "missing_source"), c("700", "700")),
        valueColumns = ".Input.indepvar_state"
      )
  }

  output$promote_status_table <- DT::renderDT({
    render_promote_table(rv$promote_plan)
  })

  observeEvent(input$promote_select_all, {
    n <- if (is.null(rv$promote_plan)) 0L else nrow(rv$promote_plan)
    session$sendCustomMessage("setPromoteSelection", list(ids = seq_len(n), selected = TRUE))
    if (n > 0) {
      updateTextInput(session, "promote_source_dir", value = input$promote_source_dir)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$promote_deselect_all, {
    n <- if (is.null(rv$promote_plan)) 0L else nrow(rv$promote_plan)
    session$sendCustomMessage("setPromoteSelection", list(ids = seq_len(n), selected = FALSE))
  }, ignoreInit = TRUE)

  summarize_promote_plan <- function(plan, action = "scan") {
    if (is.null(plan) || nrow(plan) == 0) {
      return(paste(Sys.time(), "-", tools::toTitleCase(action), "found no model outputs."))
    }
    par_counts <- table(factor(plan$par_action, levels = unique(c("update available", "updated", "up to date", "no source", "no input target", "failed", plan$par_action))))
    indep_counts <- table(factor(plan$indepvar_action, levels = unique(c("update available", "updated", "up to date", "no source", "no input target", "failed", plan$indepvar_action))))
    paste(
      c(
        paste(Sys.time(), "-", tools::toTitleCase(action), "complete"),
        paste("Models:", nrow(plan)),
        paste("PAR:", paste(names(par_counts), as.integer(par_counts), sep = "=", collapse = ", ")),
        paste("indepvar:", paste(names(indep_counts), as.integer(indep_counts), sep = "=", collapse = ", "))
      ),
      collapse = "\n"
    )
  }

  observeEvent(input$promote_scan, {
    tryCatch({
      rv$promote_plan <- promote_plan_current()
      updateTextInput(session, "promote_source_dir", value = input$promote_source_dir)
      rv$promote_log <- summarize_promote_plan(rv$promote_plan, action = "scan")
      showNotification("Promotion scan complete.", type = "message", duration = 3)
    }, error = function(e) {
      rv$promote_log <- paste(Sys.time(), "- Scan failed:", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  path_relative_to_repo <- function(path, repo_root) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    prefix <- paste0(gsub("/+$", "", root), "/")
    if (startsWith(path, prefix)) {
      substr(path, nchar(prefix) + 1L, nchar(path))
    } else {
      path
    }
  }

  run_git_capture <- function(args, repo_root) {
    quoted_args <- shQuote(as.character(c("-C", repo_root, args)))
    out <- tryCatch(
      system2("git", quoted_args, stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 127L)
    )
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    list(status = as.integer(status), output = paste(as.character(out), collapse = "\n"))
  }

  current_git_branch <- function(repo_root) {
    res <- run_git_capture(c("branch", "--show-current"), repo_root)
    branch <- trimws(res$output)
    if (res$status == 0L && nzchar(branch)) branch else "HEAD"
  }

  git_push_current_branch <- function(repo_root) {
    upstream_res <- run_git_capture(c("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"), repo_root)
    if (upstream_res$status == 0L && nzchar(trimws(upstream_res$output))) {
      push_res <- run_git_capture("push", repo_root)
      push_cmd <- "git push"
    } else {
      branch <- current_git_branch(repo_root)
      push_res <- run_git_capture(c("push", "-u", "origin", branch), repo_root)
      push_cmd <- paste("git push -u origin", branch)
    }
    list(command = push_cmd, status = push_res$status, output = push_res$output)
  }

  git_push_with_retries <- function(repo_root, attempts = 3L, wait_seconds = 2L) {
    attempts <- max(1L, as.integer(attempts))
    logs <- character(0)
    last <- NULL
    for (i in seq_len(attempts)) {
      last <- git_push_current_branch(repo_root)
      logs <- c(
        logs,
        paste0("Git push attempt ", i, "/", attempts, ": ", last$command),
        last$output
      )
      if (last$status == 0L) {
        return(list(status = 0L, command = last$command, output = paste(logs, collapse = "\n")))
      }
      if (i < attempts) Sys.sleep(wait_seconds)
    }
    list(status = last$status, command = last$command, output = paste(logs, collapse = "\n"))
  }

  git_commit_push_promoted_files <- function(plan) {
    steps <- c(paste(Sys.time(), "- Git: collecting promoted files"))
    updated_files <- unique(c(
      plan$par_target[identical("par_action" %in% names(plan), TRUE) & plan$par_action == "updated"],
      plan$indepvar_target[identical("indepvar_action" %in% names(plan), TRUE) & plan$indepvar_action == "updated"]
    ))
    updated_files <- updated_files[!is.na(updated_files) & nzchar(updated_files) & file.exists(updated_files)]
    if (length(updated_files) == 0) {
      return("Git: no promoted files changed, so no commit or push was run.")
    }

    repo_root <- repo_root_val()
    rel_files <- unique(vapply(updated_files, path_relative_to_repo, character(1), repo_root = repo_root))
    steps <- c(steps, paste("Git: staging files:", paste(rel_files, collapse = ", ")))
    add_res <- run_git_capture(c("add", "--", rel_files), repo_root)
    if (add_res$status != 0L) {
      return(paste(c(steps, "Git add failed:", add_res$output), collapse = "\n"))
    }

    diff_res <- run_git_capture(c("diff", "--cached", "--quiet", "--", rel_files), repo_root)
    if (diff_res$status == 0L) {
      return(paste(c(steps, "Git: promoted files matched the index; no commit or push was run."), collapse = "\n"))
    }

    model_names <- unique(plan$model[plan$par_action == "updated" | plan$indepvar_action == "updated"])
    model_label <- if (length(model_names) == 0) {
      ""
    } else if (length(model_names) <= 3) {
      paste0(": ", paste(model_names, collapse = ", "))
    } else {
      paste0(": ", length(model_names), " models")
    }
    msg <- paste0(
      "Promote MFCL inputs from model outputs",
      model_label
    )
    steps <- c(steps, paste("Git: committing with message:", msg))
    commit_res <- run_git_capture(c("commit", "-m", msg, "--", rel_files), repo_root)
    if (commit_res$status != 0L) {
      return(paste(c(steps, "Git commit failed:", commit_res$output), collapse = "\n"))
    }

    steps <- c(steps, "Git: commit succeeded.", commit_res$output)
    push_res <- git_push_with_retries(repo_root, attempts = 3L, wait_seconds = 2L)
    steps <- c(steps, paste("Git: pushing with:", push_res$command))
    if (push_res$status != 0L) {
      return(paste(c(
        steps,
        "Git push failed after retries.",
        "Files were promoted and committed locally, but the commit was not pushed.",
        "Use Retry Git Push after network/auth is back.",
        push_res$output
      ), collapse = "\n"))
    }

    paste(c(steps, "Git: push succeeded.", push_res$output, "Git commit and push complete."), collapse = "\n")
  }

  observeEvent(input$promote_apply, {
    shinyjs::disable("promote_apply")
    on.exit(shinyjs::enable("promote_apply"), add = TRUE)
    rv$promote_run_status <- "Starting promote..."
    rv$promote_log <- paste(Sys.time(), "- Promote selected started.")
    showNotification("Promoting selected outputs...", type = "message", duration = NULL, id = "promote_apply_progress")
    on.exit(removeNotification("promote_apply_progress"), add = TRUE)
    tryCatch({
      rv$promote_run_status <- "Scanning promotion plan..."
      plan <- promote_plan_current()
      if (!exists("pmo_apply_promotions", mode = "function")) {
        stop("Promotion apply function not available.")
      }
      selected_row_ids <- if (is.null(input$promote_selected_row_ids)) integer(0) else input$promote_selected_row_ids
      selected_row_ids <- unique(suppressWarnings(as.integer(selected_row_ids)))
      selected_row_ids <- selected_row_ids[is.finite(selected_row_ids) & selected_row_ids >= 1L & selected_row_ids <= nrow(plan)]
      if (length(selected_row_ids) == 0) {
        showNotification("Select one or more rows to promote.", type = "warning", duration = 4)
        rv$promote_log <- paste(Sys.time(), "- Promote skipped: no rows selected.")
        rv$promote_run_status <- "Skipped: no rows selected"
        return(invisible(NULL))
      }

      selected_plan <- plan[selected_row_ids, , drop = FALSE]
      rv$promote_run_status <- paste("Promoting", length(selected_row_ids), "selected row(s)...")
      applied <- pmo_apply_promotions(selected_plan, backup = isTRUE(input$promote_backup_existing))
      plan[selected_row_ids, names(applied)] <- applied
      rv$promote_plan <- plan

      rv$promote_run_status <- "Committing and pushing promoted files..."
      git_log <- git_commit_push_promoted_files(applied)
      git_failed <- grepl("push failed|commit failed|add failed", git_log, ignore.case = TRUE)
      git_noop <- grepl("no commit or push", git_log, ignore.case = TRUE)
      summary_action <- if (git_failed) {
        "promote selected files only; git incomplete"
      } else {
        "promote selected"
      }
      rv$promote_log <- paste(
        summarize_promote_plan(applied, action = summary_action),
        "",
        git_log,
        sep = "\n"
      )
      if (git_failed) {
        rv$promote_run_status <- "Files updated locally, but git commit/push is incomplete. See Promotion Log."
        showNotification("Files updated, but git commit/push failed. Use Retry Git Push if commit succeeded.", type = "error", duration = 8)
      } else if (git_noop) {
        rv$promote_run_status <- "No changed promoted files; no commit/push needed."
        showNotification("No changed promoted files; no commit/push needed.", type = "warning", duration = 5)
      } else {
        rv$promote_run_status <- "Promote, commit, and push complete."
        showNotification("Promote, commit, and push complete.", type = "message", duration = 4)
      }
    }, error = function(e) {
      rv$promote_log <- paste(Sys.time(), "- Promote failed:", conditionMessage(e))
      rv$promote_run_status <- "Failed. See Promotion Log."
      showNotification(conditionMessage(e), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$promote_retry_push, {
    shinyjs::disable("promote_retry_push")
    on.exit(shinyjs::enable("promote_retry_push"), add = TRUE)
    rv$promote_run_status <- "Retrying git push..."
    rv$promote_log <- paste(rv$promote_log, "", paste(Sys.time(), "- Retry Git Push started."), sep = "\n")
    showNotification("Retrying git push...", type = "message", duration = NULL, id = "promote_retry_push_progress")
    on.exit(removeNotification("promote_retry_push_progress"), add = TRUE)

    tryCatch({
      repo_root <- repo_root_val()
      push_res <- git_push_with_retries(repo_root, attempts = 3L, wait_seconds = 2L)
      retry_log <- paste(
        paste(Sys.time(), "- Retry Git Push complete"),
        paste("Command:", push_res$command),
        push_res$output,
        sep = "\n"
      )
      rv$promote_log <- paste(rv$promote_log, retry_log, sep = "\n")
      if (push_res$status == 0L) {
        rv$promote_run_status <- "Git push complete."
        showNotification("Git push complete.", type = "message", duration = 4)
      } else {
        rv$promote_run_status <- "Git push still failed. See Promotion Log."
        showNotification("Git push still failed. See Promotion Log.", type = "error", duration = 7)
      }
    }, error = function(e) {
      rv$promote_run_status <- "Git push retry failed. See Promotion Log."
      rv$promote_log <- paste(rv$promote_log, paste(Sys.time(), "- Retry Git Push failed:", conditionMessage(e)), sep = "\n")
      showNotification(conditionMessage(e), type = "error", duration = 5)
    })
  }, ignoreInit = TRUE)
