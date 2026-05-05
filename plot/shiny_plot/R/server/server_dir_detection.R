server_dir_detection <- function(input, output, session, rv) {
    # DIRECTORY DETECTION
    # ---------------------------------------------------------------------------
    repo_root <- shiny_plot_repo_root()
    state_file <- file.path(repo_root, ".model_dir_last.rds")
    export_state_file <- file.path(repo_root, ".plot_export_dir_last.rds")
    default_export_dir <- normalizePath(file.path(repo_root, "figure"), mustWork = FALSE)
    default_model_dir <- if (dir.exists(file.path(repo_root, "model"))) file.path(repo_root, "model") else repo_root
    initialized <- FALSE
    export_initialized <- FALSE

    observe({
      if (!initialized && file.exists(state_file)) {
        saved_dir <- tryCatch(readRDS(state_file), error = function(e) NULL)
        if (!is.null(saved_dir) && is.character(saved_dir) && nchar(saved_dir) > 0 && dir.exists(saved_dir)) {
          updateTextInput(session, "model_dir", value = saved_dir)
        } else {
          updateTextInput(session, "model_dir", value = default_model_dir)
        }
        initialized <<- TRUE
      } else if (!initialized && !file.exists(state_file)) {
        updateTextInput(session, "model_dir", value = default_model_dir)
        initialized <<- TRUE
      }
    })

    observe({
      if (!export_initialized) {
        if (!dir.exists(default_export_dir)) {
          try(dir.create(default_export_dir, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
        }
        updateTextInput(session, "plot_export_dir", value = default_export_dir)
        export_initialized <<- TRUE
      }
    })
  
    detect_and_update <- function(model_dir) {
      # Check if directory exists
      if (nchar(model_dir) > 0 && dir.exists(model_dir)) {
        # Get all subdirectories (potential scenarios)
        scenario_folders <- list.dirs(model_dir, full.names = FALSE, recursive = FALSE)
      
        # Filter out hidden folders and common non-model folders
        scenario_folders <- scenario_folders[
          !grepl("^\\.|^__", scenario_folders) & 
            !scenario_folders %in% c("archive", "old", "backup", "test")
        ]

        # Setup-check folders and other launch bookkeeping directories can sit
        # under model/.  Only offer folders with core model output; diagnostics
        # still use the selected model folders after they load.
        if (length(scenario_folders) > 0) {
          has_output <- vapply(
            file.path(model_dir, scenario_folders),
            sp_model_folder_has_core_output,
            logical(1)
          )
          scenario_folders <- scenario_folders[has_output]
        }
      
        # Update reactive values
        if (length(scenario_folders) > 0) {
          rv$scenarios_detected <- TRUE
          rv$detected_scenario_names <- scenario_folders
          rv$detected_scenario_choices <- sp_model_choice_labels(model_dir, scenario_folders)
        
          # Update picker choices
          updatePickerInput(
            session, 
            "models_to_load",
            choices = rv$detected_scenario_choices,
            selected = unname(rv$detected_scenario_choices)  # All selected by default
          )
        } else {
          rv$scenarios_detected <- FALSE
          rv$detected_scenario_names <- NULL
          rv$detected_scenario_choices <- NULL
          updatePickerInput(session, "models_to_load", choices = NULL)
        }
      } else {
        rv$scenarios_detected <- FALSE
        rv$detected_scenario_names <- NULL
        rv$detected_scenario_choices <- NULL
        updatePickerInput(session, "models_to_load", choices = NULL)
      }
    }

    # Automatically detect scenarios when directory path changes or on refresh
    observeEvent(list(input$model_dir, input$refresh_dir), {
      detect_and_update(input$model_dir)
    }, ignoreInit = FALSE)

    observeEvent(input$model_dir, {
      if (nchar(input$model_dir) > 0 && dir.exists(input$model_dir)) {
        try(saveRDS(input$model_dir, state_file), silent = TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$plot_export_dir, {
      if (nchar(input$plot_export_dir) > 0 && dir.exists(input$plot_export_dir)) {
        try(saveRDS(input$plot_export_dir, export_state_file), silent = TRUE)
      }
    }, ignoreInit = TRUE)
  
    # Output: scenarios detected flag
    output$scenarios_detected <- reactive({ rv$scenarios_detected })
    outputOptions(output, "scenarios_detected", suspendWhenHidden = FALSE)
  
    # Display detected models summary
    output$detected_models_summary <- renderText({
      req(rv$detected_scenario_names)
      n_detected <- length(rv$detected_scenario_names)
    
      labels <- names(rv$detected_scenario_choices)
      if (is.null(labels) || length(labels) == 0) labels <- rv$detected_scenario_names

      # Show first few model names as preview
      if (n_detected <= 5) {
        preview <- paste(labels, collapse = ", ")
      } else {
        preview <- paste(
          paste(head(labels, 3), collapse = ", "),
          "... and",
          n_detected - 3,
          "more"
        )
      }
    
      paste0("Found ", n_detected, " model output folder(s): ", preview)
    })
  
    # ---------------------------------------------------------------------------
    # BROWSE DIRECTORY BUTTON (ALL PLATFORMS)
    # ---------------------------------------------------------------------------
  
    volumes <- shinyFiles::getVolumes()
  
    observe({
      shinyFiles::shinyDirChoose(input, "browse_dir", roots = volumes(), session = session)
    })

    observe({
      shinyFiles::shinyDirChoose(input, "browse_plot_export_dir", roots = volumes(), session = session)
    })
  
    observeEvent(input$browse_dir, {
      selected <- shinyFiles::parseDirPath(volumes(), input$browse_dir)
      if (length(selected) > 0 && dir.exists(selected)) {
        updateTextInput(session, "model_dir", value = selected)
      }
    })

    observeEvent(input$browse_plot_export_dir, {
      selected <- shinyFiles::parseDirPath(volumes(), input$browse_plot_export_dir)
      if (length(selected) > 0 && dir.exists(selected)) {
        updateTextInput(session, "plot_export_dir", value = selected)
      }
    })
  
    # ---------------------------------------------------------------------------

}
