server_dir_detection <- function(input, output, session, rv) {
    # DIRECTORY DETECTION
    # ---------------------------------------------------------------------------
  
    # Automatically detect scenarios when directory path changes
    observe({
      model_dir <- input$model_dir
    
      # Check if directory exists
      if (nchar(model_dir) > 0 && dir.exists(model_dir)) {
      
        # Get all subdirectories (potential scenarios)
        scenario_folders <- list.dirs(model_dir, full.names = FALSE, recursive = FALSE)
      
        # Filter out hidden folders and common non-model folders
        scenario_folders <- scenario_folders[
          !grepl("^\\.|^__", scenario_folders) & 
            !scenario_folders %in% c("archive", "old", "backup", "test")
        ]
      
        # Update reactive values
        if (length(scenario_folders) > 0) {
          rv$scenarios_detected <- TRUE
          rv$detected_scenario_names <- scenario_folders
        
          # Update picker choices
          updatePickerInput(
            session, 
            "models_to_load",
            choices = scenario_folders,
            selected = scenario_folders  # All selected by default
          )
        } else {
          rv$scenarios_detected <- FALSE
          rv$detected_scenario_names <- NULL
          updatePickerInput(session, "models_to_load", choices = NULL)
        }
      } else {
        rv$scenarios_detected <- FALSE
        rv$detected_scenario_names <- NULL
        updatePickerInput(session, "models_to_load", choices = NULL)
      }
    })
  
    # Output: scenarios detected flag
    output$scenarios_detected <- reactive({ rv$scenarios_detected })
    outputOptions(output, "scenarios_detected", suspendWhenHidden = FALSE)
  
    # Display detected models summary
    output$detected_models_summary <- renderText({
      req(rv$detected_scenario_names)
      n_detected <- length(rv$detected_scenario_names)
    
      # Show first few model names as preview
      if (n_detected <= 5) {
        preview <- paste(rv$detected_scenario_names, collapse = ", ")
      } else {
        preview <- paste(
          paste(head(rv$detected_scenario_names, 3), collapse = ", "),
          "... and",
          n_detected - 3,
          "more"
        )
      }
    
      paste0("✓ Found ", n_detected, " model(s): ", preview)
    })
  
    # ---------------------------------------------------------------------------
    # BROWSE DIRECTORY BUTTON
    # ---------------------------------------------------------------------------
  
    observeEvent(input$browse_dir, {
    
      # Windows: use choose.dir() for directory dialog
      if (.Platform$OS.type == "windows") {
        tryCatch({
          selected <- choose.dir(default = input$model_dir, 
                                 caption = "Select Model Directory")
          if (!is.na(selected) && !is.null(selected)) {
            updateTextInput(session, "model_dir", value = selected)
          }
        }, error = function(e) {
          showNotification("Please enter path manually", type = "message")
        })
      } else {
        # Mac/Linux: show instruction modal
        showModal(modalDialog(
          title = "📁 Select Directory",
          HTML(paste0(
            "<p>Please enter the path manually in the text box above.</p>",
            "<p><strong>Current path:</strong><br/>",
            "<code>", input$model_dir, "</code></p>",
            "<hr/>",
            "<p><strong>Examples:</strong></p>",
            "<ul>",
            "<li>Mac: <code>/Users/username/Documents/model</code></li>",
            "<li>Linux: <code>/home/username/model</code></li>",
            "<li>Windows: <code>C:/Users/username/model</code></li>",
            "</ul>"
          )),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    })
  
    # ---------------------------------------------------------------------------

}
