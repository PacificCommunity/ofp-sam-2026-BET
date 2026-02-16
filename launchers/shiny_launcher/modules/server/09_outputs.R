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
  
