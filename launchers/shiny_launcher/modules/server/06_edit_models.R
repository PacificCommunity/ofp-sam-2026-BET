  # ========== MODEL EDITING HANDLERS ==========
  
  # Browse program path - only show mfcl/exe directory
  observeEvent(input$browse_program, {
    tryCatch({
      # Fixed path to mfcl/exe
      target_path <- normalizePath(resolve_repo_path("mfcl/exe"), mustWork = FALSE)
      
      if (!dir.exists(target_path)) {
        showNotification("Directory mfcl/exe not found under repo root", type = "warning")
        return()
      }
      
      # Get all files in mfcl/exe
      all_files <- list.files(target_path, full.names = TRUE)
      
      # Filter executable files or .sh files
      exec_files <- all_files[file.access(all_files, 1) == 0 | grepl("\\.sh$", all_files)]
      
      # Convert to relative paths
      current_dir <- normalizePath(repo_root_val(), mustWork = FALSE)
      exec_files_rel <- sapply(exec_files, function(f) {
        gsub(paste0("^", current_dir, "/?"), "", f)
      })
      
      rv$pending_program_path <- NULL
      showModal(modalDialog(
        title = "Select Program File",
        size = "l",
        p(strong("Executable files in mfcl/exe:"), style = "margin-bottom: 10px;"),
        p(paste("Location:", target_path), 
          style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
        
        div(
          style = "max-height: 400px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
          if (length(exec_files_rel) > 0) {
            lapply(exec_files_rel, function(f) {
              file_name <- basename(f)
              tags$div(
                tags$a(
                  href = "#",
                  onclick = sprintf("Shiny.setInputValue('pending_program_path', '%s', {priority: 'event'}); return false;", f),
                  icon("file-code", style = "color: #e74c3c;"), " ", file_name,
                  style = "color: #333; cursor: pointer; text-decoration: none; font-size: 12px;"
                ),
                style = "padding: 3px 0; transition: background 0.2s;",
                onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
              )
            })
          } else {
            p("No executable files found", style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        shiny::hr(),
        div(style = "font-size:12px; color:#666;",
            "Selected: ",
            strong(textOutput("program_path_pending_display", inline = TRUE))
        ),
        textInput("program_manual_path", "Or enter path manually:",
                  value = input$edit_program_path,
                  placeholder = "mfcl/exe/mfclo64 or ./doitall.sh"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_program_path", "Select", class = "btn-primary")
        )
      ))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$pending_program_path, {
    rv$pending_program_path <- input$pending_program_path
  }, ignoreInit = TRUE)
  
  output$program_path_pending_display <- renderText({
    if (!is.null(rv$pending_program_path) && rv$pending_program_path != "") {
      rv$pending_program_path
    } else {
      "(none)"
    }
  })
  
  observeEvent(input$confirm_program_path, {
    if (!is.null(rv$pending_program_path) && rv$pending_program_path != "") {
      updateTextInput(session, "edit_program_path", value = rv$pending_program_path)
    } else if (!is.null(input$program_manual_path) && input$program_manual_path != "") {
      updateTextInput(session, "edit_program_path", value = input$program_manual_path)
    }
    removeModal()
  })
  
  # Browse base directory - only show mfcl/inputs subdirectories
  observeEvent(input$browse_basedir, {
    tryCatch({
      # Fixed path to mfcl/inputs
      target_path <- normalizePath(resolve_repo_path("mfcl/inputs"), mustWork = FALSE)
      
      if (!dir.exists(target_path)) {
        showNotification("Directory mfcl/inputs not found under repo root", type = "warning")
        return()
      }
      
      # Get all subdirectories in mfcl/inputs
      subdirs <- list.dirs(target_path, recursive = FALSE, full.names = TRUE)
      
      # Convert to relative paths
      current_dir <- normalizePath(repo_root_val(), mustWork = FALSE)
      subdirs_relative <- sapply(subdirs, function(d) {
        gsub(paste0("^", current_dir, "/?"), "", d)
      })
      
      # Sort directories
      subdirs_relative <- sort(subdirs_relative)
      
      rv$pending_basedir_path <- NULL
      showModal(modalDialog(
        title = "Select Base Directory",
        size = "l",
        p(strong("Available directories in mfcl/inputs:"), style = "margin-bottom: 10px;"),
        p(paste("Location:", target_path), 
          style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
        
        div(
          style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
          if (length(subdirs_relative) > 0) {
            lapply(subdirs_relative, function(d) {
              dir_name <- basename(d)
              tags$div(
                tags$a(
                  href = "#",
                  onclick = sprintf("Shiny.setInputValue('pending_basedir_path', '%s', {priority: 'event'}); return false;", d),
                  icon("folder", style = "color: #3c8dbc;"), " ", dir_name,
                  style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
                ),
                style = "padding: 3px 0; transition: background 0.2s;",
                onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
              )
            })
          } else {
            p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        shiny::hr(),
        div(style = "font-size:12px; color:#666;",
            "Selected: ",
            strong(textOutput("basedir_path_pending_display", inline = TRUE))
        ),
        textInput("basedir_manual_path", "Or enter path manually:",
                  value = input$edit_base_dir,
                  placeholder = "mfcl/inputs/2023_rep"),
        p(style = "color: #666; font-size: 12px;", 
          "Tip: Enter the relative path to your model input directory"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_basedir_path", "Select", class = "btn-primary")
        )
      ))
    }, error = function(e) {
      showNotification(paste("Error browsing directories:", e$message), type = "error")
    })
  })
  
  observeEvent(input$pending_basedir_path, {
    rv$pending_basedir_path <- input$pending_basedir_path
  }, ignoreInit = TRUE)
  
  output$basedir_path_pending_display <- renderText({
    if (!is.null(rv$pending_basedir_path) && rv$pending_basedir_path != "") {
      rv$pending_basedir_path
    } else {
      "(none)"
    }
  })
  
  observeEvent(input$confirm_basedir_path, {
    if (!is.null(rv$pending_basedir_path) && rv$pending_basedir_path != "") {
      updateTextInput(session, "edit_base_dir", value = rv$pending_basedir_path)
    } else if (!is.null(input$basedir_manual_path) && input$basedir_manual_path != "") {
      updateTextInput(session, "edit_base_dir", value = input$basedir_manual_path)
    }
    removeModal()
  })
  
  output$model_editor_ui <- renderUI({
    req(input$edit_model_select)
    model <- rv$models[[input$edit_model_select]]
    if (is.null(model)) return(p("No model selected"))
    
    updateTextAreaInput(session, "edit_description", 
                        value = ifelse(is.null(model$description), "", model$description))
    
    mfcl_args <- if (!is.null(model$mfcl_commands) && !is.null(model$program_path)) {
      trimmed_cmd <- trimws(gsub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", model$program_path), "\\s*"), "", model$mfcl_commands))
      trimmed_cmd
    } else {
      ""
    }
    
    tagList(
      h4(paste("Editing:", input$edit_model_select)),
      
      if (!is.null(model$description) && model$description != "") {
        div(class = "description-box", strong("Description: "), model$description)
      },
      
      div(class = "path-input-group",
          div(class = "param-label", "Program Path:"),
          fluidRow(
            column(10,
                   textInput("edit_program_path", NULL, 
                             value = model$program_path, 
                             width = "100%",
                             placeholder = "mfcl/exe/mfclo64 or ./doitall.sh")
            ),
            column(2,
                   actionButton("browse_program", "Browse", 
                                class = "btn-default browse-btn btn-block",
                                icon = icon("folder-open"))
            )
          )
      ),
      
      div(class = "path-input-group",
          div(class = "param-label", "Base Directory:"),
          fluidRow(
            column(10,
                   textInput("edit_base_dir", NULL, 
                             value = model$base_dir, 
                             width = "100%",
                             placeholder = "mfcl/inputs/2023_rep")
            ),
            column(2,
                   actionButton("browse_basedir", "Browse", 
                                class = "btn-default browse-btn btn-block",
                                icon = icon("folder-open"))
            )
          )
      ),
      
      div(class = "param-label", "MFCL Arguments:"),
      textAreaInput("edit_mfcl_args", NULL, 
                    value = mfcl_args,
                    rows = 2, width = "100%",
                    placeholder = "bet.frq 11.par 12.par -switch 2 1 1 10000 (or leave empty for ./doitall.sh)"),
      p(style = "color: #666; font-size: 11px; margin-top: -10px;",
        "💡 If arguments start with './' (like ./doitall.sh), only arguments will be used as command"),
      
      div(class = "commands-preview",
          strong("Full Command Preview:"), br(),
          textOutput("mfcl_commands_preview")
      ),
      
      shiny::hr(),
      
      fluidRow(
        column(6,
               div(class = "param-label", "Jitter Seeds:"),
               textInput("edit_jitter_seeds", NULL, value = model$jitter_seeds)
        ),
        column(6,
               div(class = "param-label", "Jitter CV:"),
               textInput("edit_jitter_cv", NULL, value = if (!is.null(model$jitter_cv)) model$jitter_cv else "0.2")
        )
      ),
      fluidRow(
        column(6,
               div(class = "param-label", "Jitter Hessian (0/1):"),
               textInput("edit_jitter_hessian", NULL, value = if (!is.null(model$jitter_hessian)) model$jitter_hessian else "0")
        ),
        column(6,
               div(class = "param-label", "Jitter Base Source:"),
               selectInput(
                 "edit_jitter_base_source",
                 NULL,
                 choices = c(
                   "makepar_00 (default)" = "makepar_00",
                   "copied_par" = "copied_par"
                 ),
                 selected = if (!is.null(model$jitter_base_source) && model$jitter_base_source %in% c("makepar_00", "copied_par")) {
                   model$jitter_base_source
                 } else {
                   "makepar_00"
                 }
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "param-label", "Model Hessian (0/1):"),
               textInput("edit_model_hessian", NULL, value = if (!is.null(model$model_hessian)) model$model_hessian else "0")
        ),
        column(4,
               div(class = "param-label", "Profile Hessian (0/1):"),
               textInput("edit_prof_hessian", NULL, value = if (!is.null(model$prof_hessian)) model$prof_hessian else "0")
        ),
        column(4,
               div(class = "param-label", "Retro Hessian (0/1):"),
               textInput("edit_retro_hessian", NULL, value = if (!is.null(model$retro_hessian)) model$retro_hessian else "0")
        )
      ),
      fluidRow(
        column(6,
               div(class = "param-label", "Retrospective Peels:"),
               textInput("edit_retro_peels", NULL, value = model$retro_peels)
        ),
        column(6,
               div(class = "param-label", "Hessian Splits:"),
               numericInput("edit_nsplit", NULL, value = as.numeric(model$nsplit), min = 1, max = 20)
        )
      ),
      div(class = "param-label", "Profile Scalers:"),
      textInput("edit_scalers", NULL, value = model$scalers, width = "100%"),
      div(class = "param-label", "Profile Reps:"),
      textInput("edit_reps", NULL, value = model$Reps, width = "100%"),
      shiny::hr(),
      div(class = "param-label", "Profile Biomass Options:"),
      fluidRow(
        column(4,
               numericInput("edit_af172", "Af172", value = as.numeric(if (!is.null(model$Af172)) model$Af172 else 0), step = 1)
        ),
        column(4,
               numericInput("edit_af173", "Af173", value = as.numeric(if (!is.null(model$Af173)) model$Af173 else 0), step = 1)
        ),
        column(4,
               numericInput("edit_af174", "Af174", value = as.numeric(if (!is.null(model$Af174)) model$Af174 else 0), step = 1)
        )
      ),
      p(style = "color: #666; font-size: 11px; margin-top: -6px;",
        "Af172: 0 = total biomass, >0 = adult biomass. Af173/Af174 define the time-period window counting backwards from the end.")
    )
  })
  
  output$mfcl_commands_preview <- renderText({
    if (is.null(input$edit_program_path)) {
      return("")
    }
    
    if (is.null(input$edit_mfcl_args) || trimws(input$edit_mfcl_args) == "") {
      return(input$edit_program_path)
    }
    
    if (grepl("^\\./", trimws(input$edit_mfcl_args))) {
      return(trimws(input$edit_mfcl_args))
    }
    
    paste(input$edit_program_path, input$edit_mfcl_args)
  })
  
  observeEvent(input$save_model, {
    req(input$edit_model_select)
    model_name <- input$edit_model_select
    
    full_commands <- if (is.null(input$edit_mfcl_args) || trimws(input$edit_mfcl_args) == "") {
      input$edit_program_path
    } else if (grepl("^\\./", trimws(input$edit_mfcl_args))) {
      trimws(input$edit_mfcl_args)
    } else {
      paste(input$edit_program_path, input$edit_mfcl_args)
    }
    
    rv$models[[model_name]]$description <- input$edit_description
    rv$models[[model_name]]$mfcl_commands <- full_commands
    rv$models[[model_name]]$program_path <- input$edit_program_path
    rv$models[[model_name]]$base_dir <- input$edit_base_dir
    rv$models[[model_name]]$jitter_seeds <- input$edit_jitter_seeds
    rv$models[[model_name]]$jitter_cv <- input$edit_jitter_cv
    rv$models[[model_name]]$jitter_base_source <- input$edit_jitter_base_source
    rv$models[[model_name]]$jitter_hessian <- input$edit_jitter_hessian
    rv$models[[model_name]]$model_hessian <- input$edit_model_hessian
    rv$models[[model_name]]$prof_hessian <- input$edit_prof_hessian
    rv$models[[model_name]]$retro_hessian <- input$edit_retro_hessian
    rv$models[[model_name]]$retro_peels <- input$edit_retro_peels
    rv$models[[model_name]]$nsplit <- as.character(input$edit_nsplit)
    rv$models[[model_name]]$scalers <- input$edit_scalers
    rv$models[[model_name]]$Reps <- input$edit_reps
    rv$models[[model_name]]$Af172 <- as.character(input$edit_af172)
    rv$models[[model_name]]$Af173 <- as.character(input$edit_af173)
    rv$models[[model_name]]$Af174 <- as.character(input$edit_af174)
    
    showNotification(paste("✓ Saved changes to", model_name), type = "message")
  })
  
  output$saved_configs_ui <- renderUI({
    rv$saved_configs_trigger
    saved_dir <- resolve_repo_path(".models_ran")
    if (!dir.exists(saved_dir)) return(p("No saved run configurations found."))
    
    saved_files <- list.files(saved_dir, pattern = "\\.rds$", full.names = TRUE)
    saved_files <- saved_files[!grepl("_job_history\\.rds$", saved_files)]
    if (length(saved_files) == 0) return(p("No saved run configurations found."))
    
    saved_files <- saved_files[order(file.info(saved_files)$mtime, decreasing = TRUE)]
    
    config_cards <- lapply(saved_files, function(file) {
      config_name <- basename(file)
      tryCatch({
        saved_data <- readRDS(file)
        meta <- saved_data$metadata
        job_history <- load_job_history(file)
        
        div(class = "config-card",
            div(style = "display: flex; justify-content: space-between; align-items: start;",
                div(
                  strong(style = "font-size: 16px; color: #333;", meta$run_name), br(),
                  span(style = "color: #666;", meta$description), br(),
                  small(style = "color: #999;",
                        icon("calendar"), " ", format(meta$date, "%Y-%m-%d %H:%M"), " | ",
                        icon("user"), " ", meta$created_by, " | ",
                        icon("cube"), " ", length(saved_data$models), " models", br(),
                        icon("file"), " Base: ", meta$base_config
                  ),
                  if (nrow(job_history) > 0) {
                    div(class = "job-history",
                        strong(icon("rocket"), " Jobs Run: ", nrow(job_history)), br(),
                        small("Latest: ", job_history[nrow(job_history), "timestamp"],
                              " (", job_history[nrow(job_history), "job_type"], ")", br(),
                              "Output: ", job_history[nrow(job_history), "output_dir"])
                    )
                  }
                ),
                actionButton(paste0("load_saved_", gsub("[^a-zA-Z0-9]", "_", config_name)),
                             "Load", class = "btn-sm btn-primary",
                             onclick = sprintf("Shiny.setInputValue('load_saved_config', '%s', {priority: 'event'})", file))
            )
        )
      }, error = function(e) { 
        div(class = "config-card", p(config_name)) 
      })
    })
    tagList(config_cards)
  })
  
  observeEvent(input$load_saved_config, {
    req(input$load_saved_config)
    load_models(input$load_saved_config, is_saved_run = TRUE)
  })
  
  observeEvent(input$save_config_btn, {
    showModal(modalDialog(
      title = "Save Model Run Configuration", size = "m",
      textInput("modal_run_name", "Run Name:", placeholder = "e.g., Sensitivity Analysis - February 2026"),
      textAreaInput("modal_run_description", "Description:", placeholder = "Why are you running these models?", rows = 4),
      shiny::hr(),
      p(strong("Current Models:"), paste(names(rv$models), collapse = ", ")),
      p(strong("Base Config:"), rv$base_config_name),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_save_run", "Save Run Config", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$confirm_save_run, {
    req(input$modal_run_name)
    if (length(rv$models) == 0) { 
      showNotification("No models to save!", type = "error")
      return() 
    }
    
    tryCatch({
      date_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
      safe_name <- gsub("[^a-zA-Z0-9_-]", "_", input$modal_run_name)
      safe_name <- substr(safe_name, 1, 50)
      filename <- paste0(date_str, "_", safe_name, ".rds")
      
      save_data <- list(
        metadata = list(
          run_name = input$modal_run_name, 
          description = input$modal_run_description,
          date = Sys.time(), 
          created_by = Sys.getenv("USER"), 
          base_config = rv$base_config_name,
          n_models = length(rv$models), 
          model_names = names(rv$models)
        ),
        models = rv$models
      )
      
      saved_dir <- resolve_repo_path(".models_ran")
      if (!dir.exists(saved_dir)) {
        dir.create(saved_dir, recursive = TRUE)
      }
      filepath <- file.path(saved_dir, filename)
      saveRDS(save_data, filepath)
      rv$current_config_file <- filepath
      
      removeModal()
      showNotification(paste0("✓ Saved run configuration:\n", filename), type = "message", duration = 5)
      rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
      
    }, error = function(e) { 
      showNotification(paste("Error saving:", e$message), type = "error") 
    })
  })
  
