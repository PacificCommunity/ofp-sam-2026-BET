  # ============================================================
  # "Modify Script" button handler - Open editor in modal
  # ============================================================
  observeEvent(input$edit_script_rstudio, {
    if (!rv$config_loaded) {
      showNotification("Please load a configuration file first", type = "warning")
      return()
    }
    
    # Read the current script content
    script_content <- ""
    current_file <- NULL
    
    if (!is.null(rv$config_path)) {
      # Find the file
      possible_paths <- c(
        rv$config_path,
        file.path(resolve_repo_path("configs"), basename(rv$config_path))
      )
      
      for (p in possible_paths) {
        if (file.exists(p)) {
          script_content <- paste(readLines(p, warn = FALSE), collapse = "\n")
          current_file <- basename(p)
          break
        }
      }
    }
    
    if (script_content == "") {
      showNotification("Cannot read script content", type = "error")
      return()
    }
    
    
    # Show modal with editor
    showModal(modalDialog(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span(icon("edit"), " Edit Script"),
        span(style = "font-size: 12px; color: #666; font-weight: normal;", current_file)
      ),
      size = "l",
      
      div(
        class = "editor-toolbar",
        textInput("save_script_name", "Save as:",
                  value = if(!is.null(current_file)) current_file else "my_model.R",
                  placeholder = "filename.R",
                  width = "300px"),
        actionButton("save_script_btn", "Save", 
                     class = "btn-success btn-sm", 
                     icon = icon("save")),
        actionButton("reload_script_btn", "Save & Reload", 
                     class = "btn-primary btn-sm", 
                     icon = icon("sync"))
      ),
      
      tags$textarea(
        id = "script_editor",
        class = "script-editor-container",
        style = "width: 100%; height: 500px; padding: 10px; 
                 font-family: 'Courier New', monospace; font-size: 13px;
                 border: 1px solid #ddd; border-radius: 4px;
                 background: #f9f9f9; resize: vertical;",
        script_content
      ),
      
      p(style = "color: #666; font-size: 11px; margin-top: 10px;",
        icon("info-circle"), 
        " Edit your script above. Click 'Save' to save changes, or 'Save & Reload' to save and load the updated models."),
      
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  
  # Save script without reloading
  observeEvent(input$save_script_btn, {
    req(input$save_script_name, input$script_editor)
    
    filename <- input$save_script_name
    content <- input$script_editor
    
    # Ensure .R extension
    if (!grepl("\\.R$|\\.r$", filename)) {
      filename <- paste0(filename, ".R")
    }
    
    save_path <- file.path(resolve_repo_path("configs"), filename)
    
    tryCatch({
      # Create directory if needed
      if (!dir.exists(resolve_repo_path("configs"))) {
        dir.create(resolve_repo_path("configs"), recursive = TRUE)
      }
      
      # Write content
      writeLines(content, save_path)
      
      showNotification(
        paste("✓ Saved:", filename),
        type = "message",
        duration = 3
      )
      
      # Update config path
      rv$config_path <- filename
      rv$uploaded_filename <- NULL
      
    }, error = function(e) {
      showNotification(
        paste("Error saving:", e$message),
        type = "error"
      )
    })
  })
  
  # Save and reload script
  observeEvent(input$reload_script_btn, {
    req(input$save_script_name, input$script_editor)
    
    filename <- input$save_script_name
    content <- input$script_editor
    
    # Ensure .R extension
    if (!grepl("\\.R$|\\.r$", filename)) {
      filename <- paste0(filename, ".R")
    }
    
    save_path <- file.path(resolve_repo_path("configs"), filename)
    
    tryCatch({
      # Create directory if needed
      if (!dir.exists(resolve_repo_path("configs"))) {
        dir.create(resolve_repo_path("configs"), recursive = TRUE)
      }
      
      # Write content
      writeLines(content, save_path)
      
      removeModal()
      
      # Reload models
      load_models(config_path = save_path, original_filename = filename)
      
      showNotification(
        paste("✓ Saved and reloaded:", filename),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error:", e$message),
        type = "error"
      )
    })
  })
  
  # ============================================================
  # Browse button for loading config files
  # ============================================================
  observeEvent(input$launch_load_config, {
    showNotification("Loading config files...", type = "message", duration = 2)
    
    tryCatch({
      # Start from last browsed path or default
      start_path <- if (!is.null(rv$last_browse_path) && dir.exists(rv$last_browse_path)) {
        rv$last_browse_path
      } else {
        resolve_repo_path("configs")
      }
      
      # Normalize path
      start_path <- normalizePath(start_path, mustWork = FALSE)
      
      # Get R and RDS files in current directory
      all_files <- list.files(start_path, 
                              pattern = "\\.(R|r|rds|RDS)$", 
                              full.names = TRUE)
      
      # Get subdirectories
      dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
      
      # Get parent directory option
      parent_dir <- dirname(start_path)
      
      showModal(modalDialog(
        title = "Browse for Configuration File",
        size = "l",
        
        p(strong("Current directory:"), 
          style = "color: #666; font-size: 12px; margin-bottom: 15px;",
          code(start_path)),
        
        div(
          style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; 
                   padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
          
          # Parent directory link
          if (start_path != normalizePath("..", mustWork = FALSE)) {
            tags$div(
              style = "padding: 8px; margin-bottom: 10px; border-bottom: 2px solid #ddd;",
              tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('browse_goto_parent_config', '%s', {priority: 'event'}); return false;",
                  parent_dir
                ),
                icon("level-up-alt", style = "color: #f39c12;"), 
                " .. (parent directory)",
                style = "color: #333; font-weight: bold; text-decoration: none; font-size: 14px;"
              )
            )
          },
          
          # Subdirectories
          if (length(dirs) > 0) {
            tagList(
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(icon("folder"), " Folders", 
                        style = "color: #3c8dbc; margin-bottom: 10px;"),
                lapply(dirs, function(d) {
                  dir_name <- basename(d)
                  tags$div(
                    style = "padding: 5px 10px; margin: 2px 0; 
                             transition: background 0.2s; cursor: pointer;",
                    onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px';",
                    onmouseout = "this.style.background='';",
                    tags$a(
                      href = "#",
                      onclick = sprintf(
                        "Shiny.setInputValue('browse_goto_dir_config', '%s', {priority: 'event'}); return false;",
                        d
                      ),
                      icon("folder-open", style = "color: #3c8dbc;"), 
                      " ", dir_name,
                      style = "color: #333; text-decoration: none; font-size: 13px;"
                    )
                  )
                })
              )
            )
          },
          
          # Files
          if (length(all_files) > 0) {
            tagList(
              tags$h5(icon("file-code"), " Configuration Files", 
                      style = "color: #28a745; margin-bottom: 10px; margin-top: 15px;"),
              lapply(all_files, function(f) {
                file_name <- basename(f)
                file_ext <- tools::file_ext(f)
                file_icon <- if (file_ext %in% c("rds", "RDS")) "database" else "file-code"
                file_color <- if (file_ext %in% c("rds", "RDS")) "#9b59b6" else "#28a745"
                
                tags$div(
                  style = "padding: 5px 10px; margin: 2px 0; 
                           transition: background 0.2s; cursor: pointer;",
                  onmouseover = "this.style.background='#e8f8f0'; this.style.borderRadius='3px';",
                  onmouseout = "this.style.background='';",
                  tags$a(
                    href = "#",
                    onclick = sprintf(
                      "Shiny.setInputValue('browse_select_file_config', '%s', {priority: 'event'}); return false;",
                      f
                    ),
                    icon(file_icon, style = paste0("color: ", file_color, ";")), 
                    " ", file_name,
                    style = "color: #333; text-decoration: none; font-size: 13px;"
                  )
                )
              })
            )
          } else {
            tags$p("No R or RDS files in this directory", 
                   style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        footer = tagList(
          modalButton("Cancel")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Error browsing:", e$message), type = "error")
    })
  })
  
  # Handle directory navigation
  observeEvent(input$browse_goto_dir_config, {
    req(input$browse_goto_dir_config)
    
    # Update last browse path
    rv$last_browse_path <- input$browse_goto_dir_config
    
    # Close modal and re-open with new path
    removeModal()
    Sys.sleep(0.1)
    
    # Manually re-trigger browse
    tryCatch({
      start_path <- normalizePath(rv$last_browse_path, mustWork = FALSE)
      
      all_files <- list.files(start_path, 
                              pattern = "\\.(R|r|rds|RDS)$", 
                              full.names = TRUE)
      dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
      parent_dir <- dirname(start_path)
      
      showModal(modalDialog(
        title = "Browse for Configuration File",
        size = "l",
        
        p(strong("Current directory:"), 
          style = "color: #666; font-size: 12px; margin-bottom: 15px;",
          code(start_path)),
        
        div(
          style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; 
                   padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
          
          if (start_path != normalizePath("..", mustWork = FALSE)) {
            tags$div(
              style = "padding: 8px; margin-bottom: 10px; border-bottom: 2px solid #ddd;",
              tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('browse_goto_parent_config', '%s', {priority: 'event'}); return false;",
                  parent_dir
                ),
                icon("level-up-alt", style = "color: #f39c12;"), 
                " .. (parent directory)",
                style = "color: #333; font-weight: bold; text-decoration: none; font-size: 14px;"
              )
            )
          },
          
          if (length(dirs) > 0) {
            tagList(
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(icon("folder"), " Folders", 
                        style = "color: #3c8dbc; margin-bottom: 10px;"),
                lapply(dirs, function(d) {
                  dir_name <- basename(d)
                  tags$div(
                    style = "padding: 5px 10px; margin: 2px 0; 
                             transition: background 0.2s; cursor: pointer;",
                    onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px';",
                    onmouseout = "this.style.background='';",
                    tags$a(
                      href = "#",
                      onclick = sprintf(
                        "Shiny.setInputValue('browse_goto_dir_config', '%s', {priority: 'event'}); return false;",
                        d
                      ),
                      icon("folder-open", style = "color: #3c8dbc;"), 
                      " ", dir_name,
                      style = "color: #333; text-decoration: none; font-size: 13px;"
                    )
                  )
                })
              )
            )
          },
          
          if (length(all_files) > 0) {
            tagList(
              tags$h5(icon("file-code"), " Configuration Files", 
                      style = "color: #28a745; margin-bottom: 10px; margin-top: 15px;"),
              lapply(all_files, function(f) {
                file_name <- basename(f)
                file_ext <- tools::file_ext(f)
                file_icon <- if (file_ext %in% c("rds", "RDS")) "database" else "file-code"
                file_color <- if (file_ext %in% c("rds", "RDS")) "#9b59b6" else "#28a745"
                
                tags$div(
                  style = "padding: 5px 10px; margin: 2px 0; 
                           transition: background 0.2s; cursor: pointer;",
                  onmouseover = "this.style.background='#e8f8f0'; this.style.borderRadius='3px';",
                  onmouseout = "this.style.background='';",
                  tags$a(
                    href = "#",
                    onclick = sprintf(
                      "Shiny.setInputValue('browse_select_file_config', '%s', {priority: 'event'}); return false;",
                      f
                    ),
                    icon(file_icon, style = paste0("color: ", file_color, ";")), 
                    " ", file_name,
                    style = "color: #333; text-decoration: none; font-size: 13px;"
                  )
                )
              })
            )
          } else {
            tags$p("No R or RDS files in this directory", 
                   style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        footer = tagList(
          modalButton("Cancel")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
  
  # Handle parent directory navigation
  observeEvent(input$browse_goto_parent_config, {
    req(input$browse_goto_parent_config)
    
    # Update last browse path
    rv$last_browse_path <- input$browse_goto_parent_config
    
    # Close modal and re-open with new path
    removeModal()
    Sys.sleep(0.1)
    
    # Same code as browse_goto_dir_config
    tryCatch({
      start_path <- normalizePath(rv$last_browse_path, mustWork = FALSE)
      
      all_files <- list.files(start_path, 
                              pattern = "\\.(R|r|rds|RDS)$", 
                              full.names = TRUE)
      dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
      parent_dir <- dirname(start_path)
      
      showModal(modalDialog(
        title = "Browse for Configuration File",
        size = "l",
        
        p(strong("Current directory:"), 
          style = "color: #666; font-size: 12px; margin-bottom: 15px;",
          code(start_path)),
        
        div(
          style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; 
                   padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
          
          if (start_path != normalizePath("..", mustWork = FALSE)) {
            tags$div(
              style = "padding: 8px; margin-bottom: 10px; border-bottom: 2px solid #ddd;",
              tags$a(
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('browse_goto_parent_config', '%s', {priority: 'event'}); return false;",
                  parent_dir
                ),
                icon("level-up-alt", style = "color: #f39c12;"), 
                " .. (parent directory)",
                style = "color: #333; font-weight: bold; text-decoration: none; font-size: 14px;"
              )
            )
          },
          
          if (length(dirs) > 0) {
            tagList(
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(icon("folder"), " Folders", 
                        style = "color: #3c8dbc; margin-bottom: 10px;"),
                lapply(dirs, function(d) {
                  dir_name <- basename(d)
                  tags$div(
                    style = "padding: 5px 10px; margin: 2px 0; 
                             transition: background 0.2s; cursor: pointer;",
                    onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px';",
                    onmouseout = "this.style.background='';",
                    tags$a(
                      href = "#",
                      onclick = sprintf(
                        "Shiny.setInputValue('browse_goto_dir_config', '%s', {priority: 'event'}); return false;",
                        d
                      ),
                      icon("folder-open", style = "color: #3c8dbc;"), 
                      " ", dir_name,
                      style = "color: #333; text-decoration: none; font-size: 13px;"
                    )
                  )
                })
              )
            )
          },
          
          if (length(all_files) > 0) {
            tagList(
              tags$h5(icon("file-code"), " Configuration Files", 
                      style = "color: #28a745; margin-bottom: 10px; margin-top: 15px;"),
              lapply(all_files, function(f) {
                file_name <- basename(f)
                file_ext <- tools::file_ext(f)
                file_icon <- if (file_ext %in% c("rds", "RDS")) "database" else "file-code"
                file_color <- if (file_ext %in% c("rds", "RDS")) "#9b59b6" else "#28a745"
                
                tags$div(
                  style = "padding: 5px 10px; margin: 2px 0; 
                           transition: background 0.2s; cursor: pointer;",
                  onmouseover = "this.style.background='#e8f8f0'; this.style.borderRadius='3px';",
                  onmouseout = "this.style.background='';",
                  tags$a(
                    href = "#",
                    onclick = sprintf(
                      "Shiny.setInputValue('browse_select_file_config', '%s', {priority: 'event'}); return false;",
                      f
                    ),
                    icon(file_icon, style = paste0("color: ", file_color, ";")), 
                    " ", file_name,
                    style = "color: #333; text-decoration: none; font-size: 13px;"
                  )
                )
              })
            )
          } else {
            tags$p("No R or RDS files in this directory", 
                   style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        footer = tagList(
          modalButton("Cancel")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
  
  # Handle file selection
  observeEvent(input$browse_select_file_config, {
    req(input$browse_select_file_config)
    
    selected_file <- input$browse_select_file_config
    
    # Update last browse path to parent directory
    rv$last_browse_path <- dirname(selected_file)
    
    removeModal()
    
    # Load the selected file
    load_models(
      config_path = selected_file,
      original_filename = basename(selected_file)
    )
    
    showNotification(
      paste("Loading:", basename(selected_file)),
      type = "message",
      duration = 2
    )
  }, ignoreInit = TRUE)
  
  
  
  # Handle save and edit for uploaded files
  observeEvent(input$confirm_save_and_edit, {
    req(input$save_script_path)
    
    save_path <- input$save_script_path
    
    tryCatch({
      # Create directory if needed
      save_dir <- dirname(save_path)
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      
      # Copy the uploaded temp file to new location
      if (!is.null(rv$uploaded_temp_path) && file.exists(rv$uploaded_temp_path)) {
        file.copy(rv$uploaded_temp_path, save_path, overwrite = TRUE)
      }
      
      removeModal()
      
      # Open in RStudio
      if (file.exists(save_path)) {
        rstudioapi::navigateToFile(normalizePath(save_path))
        rv$config_path <- save_path
        rv$uploaded_filename <- NULL  # Clear uploaded flag
        showNotification(
          paste("Saved and opened in RStudio:", basename(save_path)),
          type = "message",
          duration = 3
        )
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error saving file:", e$message),
        type = "error"
      )
    })
  })
  
  
  output$launch_config_status_ui <- renderUI({
    req(rv$config_loaded)
    
    n <- length(rv$models)
    src <- if (!is.null(rv$config_path)) basename(rv$config_path) else "unknown"
    
    # Check if summary exists
    has_summary <- !is.null(rv$run_metadata$summary) && rv$run_metadata$summary != ""
    
    if (has_summary) {
      summary_text <- rv$run_metadata$summary
      summary_style <- "color: #28a745; font-weight: bold; font-size: 14px;"
      box_style <- "margin-top: 4px; padding: 8px 12px; background: #f0f9f0; 
                    border-left: 3px solid #28a745; border-radius: 4px;"
    } else {
      summary_text <- "⚠ Summary not provided"
      summary_style <- "color: #f39c12; font-weight: bold; font-size: 14px;"
      box_style <- "margin-top: 4px; padding: 8px 12px; background: #fff9e6; 
                    border-left: 3px solid #f39c12; border-radius: 4px;"
    }
    
    # Model count text
    model_count_text <- paste0(n, " model", if(n != 1) "s", " loaded")
    
    div(
      style = box_style,
      tags$div(
        style = summary_style,
        if (has_summary) icon("check-circle") else icon("exclamation-triangle"),
        paste0(" ", summary_text)
      ),
      tags$div(
        style = "color: #666; font-size: 11px; margin-top: 4px; margin-left: 20px;",
        icon("cube"),
        paste0(" ", model_count_text, " • "),
        tags$span(
          style = "font-style: italic;",
          icon("file-code"),
          paste0(" ", src)
        )
      )
    )
  })
