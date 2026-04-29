  observeEvent(input$refresh_saved_configs, {
    rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
    showNotification("Refreshed saved configurations list", type = "message", duration = 2)
  })
  
  observeEvent(input$select_all_models, {
    rv$selected_models <- names(rv$models)
  })
  
  observeEvent(input$deselect_all_models, {
    rv$selected_models <- c()
  })
  
  # ========== MODEL SELECTION UI ==========
  
  filtered_models <- reactive({
    if (length(rv$models) == 0) return(character(0))
    
    search_term <- input$model_search
    model_names <- names(rv$models)
    
    if (is.null(search_term) || search_term == "") {
      return(model_names)
    }
    
    grep(search_term, model_names, ignore.case = TRUE, value = TRUE)
  })
  
  output$model_selection_ui <- renderUI({
    if (length(rv$models) == 0) {
      return(div(
        p("No models loaded.", style = "color: red; font-weight: bold;"),
        p("Go to Settings tab to load config file.")
      ))
    }
    
    visible_models <- filtered_models()
    if (length(visible_models) == 0) {
      return(div(
        class = "model-selector-container",
        p("No models match your search.", 
          style = "color: #999; font-style: italic; text-align: center; padding: 20px;")
      ))
    }
    
    model_checkboxes <- lapply(visible_models, function(model_name) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      desc_id <- paste0("desc_", gsub("[^a-zA-Z0-9]", "_", model_name))
      expand_id <- paste0("expand_", gsub("[^a-zA-Z0-9]", "_", model_name))
      
      # Get model description
      m <- rv$models[[model_name]]
      has_desc <- !is.null(m$description) && m$description != ""
      desc_text <- if (has_desc) m$description else NULL
      
      # Checkbox change observer
      observeEvent(input[[checkbox_id]], {
        if (input[[checkbox_id]]) {
          if (!model_name %in% rv$selected_models) {
            rv$selected_models <- c(rv$selected_models, model_name)
          }
        } else {
          rv$selected_models <- rv$selected_models[rv$selected_models != model_name]
        }
      }, ignoreInit = TRUE)
      
      # Expand button observer
      observeEvent(input[[expand_id]], {
        shinyjs::toggleClass(id = desc_id, class = "expanded")
      }, ignoreInit = TRUE)
      
      div(
        class = "model-checkbox-row",
        div(
          class = "model-checkbox-left",
          checkboxInput(
            checkbox_id,
            label = NULL,
            value = model_name %in% isolate(rv$selected_models)
          )
        ),
        div(
          class = "model-checkbox-content",
          div(class = "model-name-label", model_name),
          if (has_desc) {
            tagList(
              div(
                id = desc_id,
                class = "model-desc-inline",
                desc_text
              ),
              tags$a(
                id = expand_id,
                class = "expand-desc-btn",
                href = "#",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random()); return false;",
                  expand_id
                ),
                "show more/less"
              )
            )
          } else {
            span(class = "no-description", "No description")
          }
        )
      )
    })
    
    div(
      class = "model-selector-container",
      model_checkboxes
    )
  })
  
  observe({
    req(length(rv$models) > 0)
    
    visible_models <- filtered_models()
    
    for (model_name in visible_models) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      is_selected <- model_name %in% rv$selected_models
      updateCheckboxInput(session, checkbox_id, value = is_selected)
    }
  })
  
  output$model_details_display <- renderUI({
    if (length(rv$selected_models) == 0) {
      return(p("No models selected. Please select at least one model above.", 
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    
    # Add summary section if available
    summary_section <- NULL
    if (!is.null(rv$run_metadata$summary) && rv$run_metadata$summary != "") {
      summary_section <- div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 padding: 20px; margin-bottom: 25px; 
                 border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        tags$h3(
          icon("star"), 
          " Run Summary",
          style = "color: white; margin-top: 0; margin-bottom: 15px; font-weight: bold;"
        ),
        p(rv$run_metadata$summary, 
          style = "margin: 0; font-size: 15px; line-height: 1.8; color: white;")
      )
    }
    
    model_cards <- lapply(rv$selected_models, function(model_name) {
      m <- rv$models[[model_name]]
      
      div(
        class = "model-details-card",
        div(class = "model-name-header", model_name),
        
        # Description
        if (!is.null(m$description) && m$description != "") {
          div(class = "model-desc", m$description)
        },
        
        # Parameters
        tags$div(
          style = "margin-top: 10px;",
          lapply(names(m), function(param_name) {
            if (param_name == "description") return(NULL)
            
            param_value <- m[[param_name]]
            if (is.null(param_value)) return(NULL)
            
            div(
              class = "model-param",
              tags$strong(param_name, ":"), " ",
              if (is.list(param_value)) {
                paste(names(param_value), "=", param_value, collapse = ", ")
              } else if (length(param_value) > 1) {
                paste(param_value, collapse = ", ")
              } else {
                as.character(param_value)
              }
            )
          })
        )
      )
    })
    
    div(
      class = "model-details-container",
      summary_section,  # Add summary at the top
      model_cards
    )
  })
  
