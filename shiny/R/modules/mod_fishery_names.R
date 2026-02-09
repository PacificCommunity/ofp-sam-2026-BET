mod_fishery_names_ui <- function() {
      tabItem(
        tabName = "fishery_names",
        h2("Fishery Names Manager", style = "color: #17a2b8;"),
        
        fluidRow(
          box(
            title = "Instructions",
            width = 12,
            status = "info",
            collapsible = TRUE,
            collapsed = FALSE,
            HTML("<ul>
              <li>🎯 <strong>Select Model:</strong> Choose which model's fishery names to edit</li>
              <li>📝 <strong>Edit names:</strong> Click on any cell in the 'Fishery_Name' column to edit</li>
              <li>💾 <strong>Save changes:</strong> Click 'Apply Changes' to update the selected model</li>
              <li>🔄 <strong>Apply to All:</strong> Copy current model's names to all other models</li>
              <li>📥 <strong>Export/Import:</strong> Download as CSV, edit in Excel, then upload back</li>
              <li>↩️ <strong>Reset:</strong> Restore default names from helpers.R</li>
            </ul>")
          )
        ),
        
        fluidRow(
          box(
            title = "Fishery Names Table",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            
            # Model selector
            fluidRow(
              column(4,
                     selectInput("fishery_names_model", "Select Model:",
                                 choices = NULL,
                                 selected = NULL)
              ),
              column(8,
                     div(
                       style = "padding-top: 25px;",
                       tags$span(
                         id = "fishery_count_display",
                         style = "font-size: 14px; color: #666; font-weight: bold;",
                         textOutput("fishery_count_text", inline = TRUE)
                       )
                     )
              )
            ),
            
            shiny::hr(),
            
            # Action buttons
            div(
              style = "margin-bottom: 15px;",
              actionButton("apply_fishery_names", "💾 Apply Changes to This Model", 
                           class = "btn-success", icon = icon("check")),
              actionButton("apply_to_all_models", "🔄 Apply to All Models", 
                           class = "btn-warning", icon = icon("copy")),
              actionButton("reset_fishery_names", "↩️ Reset to Default", 
                           class = "btn-danger", icon = icon("undo")),
              downloadButton("download_fishery_names", "📥 Download CSV", 
                             class = "btn-info"),
              tags$div(
                style = "display: inline-block; margin-left: 10px;",
                fileInput("upload_fishery_names", "📤 Upload CSV", 
                          accept = ".csv",
                          buttonLabel = "Browse...",
                          width = "250px")
              )
            ),
            
            # Editable table
            DTOutput("fishery_names_table")
          )
        )
      )
}

mod_fishery_names_server <- function(input, output, session, rv) {
    # TAB 2: FISHERY NAMES EDITOR
    # ===========================================================================
  
    # Display fishery count for selected model
    output$fishery_count_text <- renderText({
      req(input$fishery_names_model, rv$fishery_names_dfs)
      n_fisheries <- nrow(rv$fishery_names_dfs[[input$fishery_names_model]])
      paste0("📊 Total fisheries in this model: ", n_fisheries)
    })
  
    # Render editable fishery names table for selected model
    output$fishery_names_table <- renderDT({
      req(rv$data_loaded, input$fishery_names_model, rv$fishery_names_dfs)
    
      df <- rv$fishery_names_dfs[[input$fishery_names_model]]
    
      datatable(
        df,
        editable = list(target = "cell", disable = list(columns = 0)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = 0),
            list(className = 'editable-cell', targets = 1)
          )
        ),
        rownames = FALSE
      )
    })
  
    # Handle cell edits
    observeEvent(input$fishery_names_table_cell_edit, {
      req(input$fishery_names_model)
    
      info <- input$fishery_names_table_cell_edit
      rv$fishery_names_dfs[[input$fishery_names_model]][info$row, info$col + 1] <- info$value
    })
  
    # Apply fishery name changes to selected model only
    observeEvent(input$apply_fishery_names, {
      req(input$fishery_names_model, rv$fishery_names_dfs)
    
      model_name <- input$fishery_names_model
      df <- rv$fishery_names_dfs[[model_name]]
    
      # Create custom mapping for this model
      custom_mapping <- setNames(df$Fishery_Name, df$Fishery_ID)
    
      # Update FISHERY_MAPS for this model only
      for (fid in names(custom_mapping)) {
        if (fid %in% names(rv$FISHERY_MAPS[[model_name]])) {
          rv$FISHERY_MAPS[[model_name]][fid] <- custom_mapping[fid]
        }
      }
    
      # Update UI elements in other tabs
    
      # 1. Update CPUE fisheries dropdown (if this model is selected)
      if (model_name %in% input$cpue_scenarios) {
        all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
      
        if (length(all_index_fish) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
          choices <- setNames(all_index_fish, 
                              sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
        
          current_selection <- isolate(input$cpue_fisheries)
          updatePickerInput(session, "cpue_fisheries", 
                            choices = choices,
                            selected = current_selection)
        }
      }
    
      # 2. Update Length Frequency fishery dropdown (if this model is selected)
      if (!is.null(input$lf_model) && input$lf_model == model_name) {
        if (!is.null(rv$LengOut_list[[model_name]])) {
          fisheries <- unique(rv$LengOut_list[[model_name]]@lenfits$fishery)
        
          if (length(fisheries) > 0) {
            fishery_map <- rv$FISHERY_MAPS[[model_name]]
            choices <- setNames(fisheries, 
                                sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
          
            current_selection <- isolate(input$lf_fishery)
            if (!is.null(current_selection) && current_selection %in% fisheries) {
              selected <- current_selection
            } else {
              selected <- fisheries[1]
            }
          
            updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
          }
        }
      
        # Re-check compatibility after name change
        all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
        compatible_models <- check_lf_compatibility(input$lf_model, all_models)
      
        updatePickerInput(session, "lf_scenarios",
                          choices = compatible_models,
                          selected = intersect(isolate(input$lf_scenarios), compatible_models))
      }
    
      # 3. Update Weight Frequency fishery dropdown (if this model is selected)
      if (!is.null(input$wf_model) && input$wf_model == model_name) {
        if (!is.null(rv$WeightOut_list[[model_name]])) {
          fisheries <- unique(rv$WeightOut_list[[model_name]]@wgtfits$fishery)
        
          if (length(fisheries) > 0) {
            fishery_map <- rv$FISHERY_MAPS[[model_name]]
            choices <- setNames(fisheries, 
                                sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
          
            current_selection <- isolate(input$wf_fishery)
            if (!is.null(current_selection) && current_selection %in% fisheries) {
              selected <- current_selection
            } else {
              selected <- fisheries[1]
            }
          
            updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
          }
        }
      
        # Re-check compatibility after name change
        all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
        compatible_models <- check_wf_compatibility(input$wf_model, all_models)
      
        updatePickerInput(session, "wf_scenarios",
                          choices = compatible_models,
                          selected = intersect(isolate(input$wf_scenarios), compatible_models))
      }
    
      showNotification(
        HTML(paste0(
          "✓ Fishery names updated for model: <strong>", model_name, "</strong><br/>",
          "📊 UI elements refreshed in all tabs"
        )),
        type = "message",
        duration = 4
      )
    })
  
    # Apply current model's fishery names to all models
    observeEvent(input$apply_to_all_models, {
      req(input$fishery_names_model, rv$fishery_names_dfs)
    
      source_model <- input$fishery_names_model
      source_df <- rv$fishery_names_dfs[[source_model]]
      source_mapping <- setNames(source_df$Fishery_Name, source_df$Fishery_ID)
    
      # Count how many models will be updated
      n_updated <- 0
    
      # Apply to all models
      for (model_name in names(rv$FISHERY_MAPS)) {
        updated <- FALSE
        for (fid in names(source_mapping)) {
          if (fid %in% names(rv$FISHERY_MAPS[[model_name]])) {
            rv$FISHERY_MAPS[[model_name]][fid] <- source_mapping[fid]
          
            # Also update the dataframe
            idx <- which(rv$fishery_names_dfs[[model_name]]$Fishery_ID == fid)
            if (length(idx) > 0) {
              rv$fishery_names_dfs[[model_name]][idx, "Fishery_Name"] <- source_mapping[fid]
            }
            updated <- TRUE
          }
        }
        if (updated) n_updated <- n_updated + 1
      }
    
      # Update all UI elements
    
      # 1. Update CPUE fisheries dropdown
      if (length(input$cpue_scenarios) > 0) {
        all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
      
        if (length(all_index_fish) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
          choices <- setNames(all_index_fish, 
                              sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
        
          current_selection <- isolate(input$cpue_fisheries)
          updatePickerInput(session, "cpue_fisheries", 
                            choices = choices,
                            selected = current_selection)
        }
      }
    
      # 2. Update Length Frequency fishery dropdown
      if (!is.null(input$lf_model) && !is.null(rv$LengOut_list[[input$lf_model]])) {
        fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
      
        if (length(fisheries) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
          choices <- setNames(fisheries, 
                              sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
        
          current_selection <- isolate(input$lf_fishery)
          if (!is.null(current_selection) && current_selection %in% fisheries) {
            selected <- current_selection
          } else {
            selected <- fisheries[1]
          }
        
          updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
        }
      
        # Re-check compatibility
        all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
        compatible_models <- check_lf_compatibility(input$lf_model, all_models)
      
        updatePickerInput(session, "lf_scenarios",
                          choices = compatible_models,
                          selected = intersect(isolate(input$lf_scenarios), compatible_models))
      }
    
      # 3. Update Weight Frequency fishery dropdown
      if (!is.null(input$wf_model) && !is.null(rv$WeightOut_list[[input$wf_model]])) {
        fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
      
        if (length(fisheries) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
          choices <- setNames(fisheries, 
                              sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
        
          current_selection <- isolate(input$wf_fishery)
          if (!is.null(current_selection) && current_selection %in% fisheries) {
            selected <- current_selection
          } else {
            selected <- fisheries[1]
          }
        
          updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
        }
      
        # Re-check compatibility
        all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
        compatible_models <- check_wf_compatibility(input$wf_model, all_models)
      
        updatePickerInput(session, "wf_scenarios",
                          choices = compatible_models,
                          selected = intersect(isolate(input$wf_scenarios), compatible_models))
      }
    
      showNotification(
        HTML(paste0(
          "<strong>✓ Applied fishery names to all models!</strong><br/>",
          "Source: ", source_model, "<br/>",
          "Updated: ", n_updated, " model(s)<br/>",
          "📊 UI elements refreshed in all tabs"
        )),
        type = "message",
        duration = 5
      )
    })
  
    # Reset fishery names to default for selected model
    observeEvent(input$reset_fishery_names, {
      req(rv$data_loaded, input$fishery_names_model)
    
      model_name <- input$fishery_names_model
    
      # Recreate default mapping for this model
      rv$FISHERY_MAPS[[model_name]] <- create_fishery_map(
        rv$ParOut_list[[model_name]], 
        GLOBAL_FISHERY_NAMES
      )
    
      # Reset dataframe
      fishery_map <- rv$FISHERY_MAPS[[model_name]]
      rv$fishery_names_dfs[[model_name]] <- data.frame(
        Fishery_ID = names(fishery_map),
        Fishery_Name = as.character(fishery_map),
        stringsAsFactors = FALSE
      )
    
      showNotification(
        paste0("✓ Fishery names reset to default for: ", model_name),
        type = "warning",
        duration = 3
      )
    })
  
    # Download fishery names CSV for selected model
    output$download_fishery_names <- downloadHandler(
      filename = function() {
        req(input$fishery_names_model)
        paste0("fishery_names_", input$fishery_names_model, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(input$fishery_names_model, rv$fishery_names_dfs)
        df <- rv$fishery_names_dfs[[input$fishery_names_model]]
        write.csv(df, file, row.names = FALSE)
      }
    )
  
    # Upload fishery names CSV for selected model
    observeEvent(input$upload_fishery_names, {
      req(input$upload_fishery_names, input$fishery_names_model)
    
      tryCatch({
        uploaded <- read.csv(input$upload_fishery_names$datapath, stringsAsFactors = FALSE)
      
        # Validate columns
        if (!all(c("Fishery_ID", "Fishery_Name") %in% names(uploaded))) {
          showNotification("❌ CSV must have columns: Fishery_ID, Fishery_Name", 
                           type = "error", duration = 5)
          return()
        }
      
        model_name <- input$fishery_names_model
      
        # Check if Fishery_IDs match the current model
        current_ids <- rv$fishery_names_dfs[[model_name]]$Fishery_ID
        uploaded_ids <- uploaded$Fishery_ID
      
        if (!all(uploaded_ids %in% current_ids)) {
          missing <- setdiff(uploaded_ids, current_ids)
          showNotification(
            HTML(paste0(
              "⚠️ Warning: Some Fishery_IDs not found in current model:<br/>",
              paste(missing, collapse = ", ")
            )),
            type = "warning",
            duration = 5
          )
        }
      
        # Update dataframe (only matching IDs)
        for (i in 1:nrow(uploaded)) {
          fid <- uploaded$Fishery_ID[i]
          fname <- uploaded$Fishery_Name[i]
        
          idx <- which(rv$fishery_names_dfs[[model_name]]$Fishery_ID == fid)
          if (length(idx) > 0) {
            rv$fishery_names_dfs[[model_name]][idx, "Fishery_Name"] <- fname
          }
        }
      
        showNotification(
          paste0("✓ Fishery names uploaded for: ", model_name, 
                 ". Click 'Apply Changes' to save."),
          type = "message",
          duration = 5
        )
      
      }, error = function(e) {
        showNotification(paste("❌ Error uploading CSV:", e$message), 
                         type = "error", duration = 5)
      })
    })
  
    # ===========================================================================

}
