  # Browse download location
  show_download_browser <- function(start_path) {
    if (!dir.exists(start_path)) {
      start_path <- "/"
    }
    rv$current_download_browse <- start_path
    parent_path <- dirname(start_path)
    dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
    
    rv$pending_download_path <- NULL
    showModal(modalDialog(
      title = "Select Download Location",
      size = "m",
      p(strong("Available directories:"), style = "margin-bottom: 10px;"),
      p(paste("Current location:", normalizePath(getwd())), 
        style = "color: #666; font-size: 11px; margin-bottom: 5px;"),
      p(paste("Browsing:", start_path), 
        style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
      
      div(
        style = "max-height: 400px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
        
        tags$div(
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('download_browse_to', '/', {priority: 'event'}); return false;",
            icon("folder", style = "color: #f39c12;"), " /",
            style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px; font-weight: bold;"
          ),
          if (start_path != "/" && parent_path != start_path) {
            tags$a(
              href = "#",
              onclick = sprintf("Shiny.setInputValue('download_browse_to', '%s', {priority: 'event'}); return false;", parent_path),
              icon("level-up-alt", style = "color: #666;"), " ..",
              style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px; font-weight: bold; margin-left: 10px;"
            )
          },
          style = "padding: 5px 0; margin-bottom: 10px; border-bottom: 2px solid #ddd;"
        ),
        
        if (length(dirs) > 0) {
          lapply(dirs, function(d) {
            dir_name <- basename(d)
            tags$div(
              tags$a(
              href = "#",
              onclick = sprintf("Shiny.setInputValue('download_browse_to', '%s', {priority: 'event'}); return false;", d),
              icon("folder-open", style = "color: #3c8dbc;"), " ", dir_name,
              style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
            ),
            style = "padding: 3px 0; padding-left: 20px; transition: background 0.2s;",
            onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px';",
            onmouseout = "this.style.background='';"
          )
        })
      } else {
        p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
      }
    ),
    
    shiny::hr(),
    div(style = "font-size:12px; color:#666;",
        "Selected: ",
        strong(textOutput("download_path_pending_display", inline = TRUE))
    ),
    actionButton("select_download_current", "Use This Folder", class = "btn-default"),
    textInput("download_manual_path", "Or enter path manually:",
              value = input$download_location,
              placeholder = "/path/to/download"),
      p(style = "color: #666; font-size: 12px;", 
        "Tip: Absolute path recommended. Relative paths resolve to repo root."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_download_path", "Select", class = "btn-primary")
      )
    ))
  }
  
  observeEvent(input$browse_download_location, {
    tryCatch({
      start_path <- normalizePath(input$download_location, mustWork = FALSE)
      if (is.null(input$download_location) || input$download_location == "" || !dir.exists(start_path)) {
        start_path <- repo_root_val()
      }
      show_download_browser(start_path)
    }, error = function(e) {
      showNotification(paste("Error browsing directories:", e$message), type = "error")
    })
  })
  
  observeEvent(input$download_browse_to, {
    req(input$download_browse_to)
    show_download_browser(input$download_browse_to)
  }, ignoreInit = TRUE)
  
  observeEvent(input$select_download_current, {
    if (!is.null(rv$current_download_browse) && rv$current_download_browse != "") {
      rv$pending_download_path <- rv$current_download_browse
    }
  }, ignoreInit = TRUE)
  
  output$download_path_pending_display <- renderText({
    if (!is.null(rv$pending_download_path) && rv$pending_download_path != "") {
      rv$pending_download_path
    } else if (!is.null(rv$current_download_browse) && rv$current_download_browse != "") {
      rv$current_download_browse
    } else {
      "(none)"
    }
  })
  
  observeEvent(input$confirm_download_path, {
    if (!is.null(rv$pending_download_path) && rv$pending_download_path != "") {
      updateTextInput(session, "download_location", value = rv$pending_download_path)
    } else if (!is.null(rv$current_download_browse) && rv$current_download_browse != "") {
      updateTextInput(session, "download_location", value = rv$current_download_browse)
    } else if (!is.null(input$download_manual_path) && input$download_manual_path != "") {
      updateTextInput(session, "download_location", value = input$download_manual_path)
    }
    removeModal()
  })
  
  output$download_location_display <- renderText({
    dl_path <- input$download_location
    if (is.null(dl_path) || dl_path == "") {
      return("(not set - please browse or enter path)")
    }
    normalizePath(resolve_repo_path(dl_path), mustWork = FALSE)
  })
  
  output$scan_results_status <- renderText({
    rv$action_status$scan_results
  })
  
  output$download_all_status <- renderText({
    rv$action_status$download_all
  })
  
  output$delete_remote_dir_status <- renderText({
    rv$action_status$delete_remote_dir
  })
  
  output$download_selected_status <- renderText({
    rv$action_status$download_selected
  })
  
  output$delete_selected_status <- renderText({
    rv$action_status$delete_selected
  })
  
  output$selected_folders_count <- renderText({
    paste(length(rv$selected_folders), "folders selected")
  })
  
  observe({
    updateTextInput(session, "extract_repo_name", 
                    placeholder = input$github_repo)
  })
  
  output$extract_full_path_preview <- renderText({
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- "[repository]"
    }
    
    extract_path <- trimws(input$extract_path_manual)
    
    if (is.null(extract_path) || extract_path == "") {
      return(paste0(repo_name, "/*  (everything from repository root)"))
    }
    
    paste0(repo_name, "/", extract_path, "/*")
  })
  
  output$extract_target_preview <- renderText({
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      return("(download location not set)")
    }
    paste0(normalizePath(resolve_repo_path(download_dir), mustWork = FALSE), "/[folder_name]/*")
  })
  
