mod_cpue_ui <- function() {
      tabItem(
        tabName = "cpue",
        h2("CPUE Fits", style = "color: #00c0ef;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "info",
            
            # Scenarios selector with dropdown
            pickerInput(
              "cpue_scenarios",
              "Scenarios:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} scenarios selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search scenarios...",
                size = 10
              )
            ),
            
            # Fisheries selector with dropdown
            pickerInput(
              "cpue_fisheries",
              "Fisheries:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} fisheries selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search fisheries...",
                size = 10
              )
            ),
            
            shiny::hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_cpue_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download")),
            helpText("Select scenarios and fisheries to display", style = "margin-top: 10px;")
          ),
          
          # CPUE plot panel
          box(
            title = "CPUE Observed vs Predicted",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput("cpue_plot", height = "600px")
          )
        )
      )
}

mod_cpue_server <- function(input, output, session, rv) {
    # TAB 5: CPUE FITS
    # ===========================================================================
  
    # Update fishery choices when scenarios change (preserve selection)
    observeEvent(input$cpue_scenarios, {
      req(rv$data_loaded)
    
      if (length(input$cpue_scenarios) == 0) {
        updatePickerInput(session, "cpue_fisheries", choices = character(0))
        return()
      }
    
      # Get index fisheries from all selected scenarios
      all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
    
      # Check if index fisheries exist
      if (length(all_index_fish) == 0) {
        updatePickerInput(session, "cpue_fisheries", choices = character(0))
        showNotification("No index fisheries detected in selected scenarios", 
                         type = "warning", duration = 3)
        return()
      }
    
      # Create named choices with fishery labels
      fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
      choices <- setNames(all_index_fish, 
                          sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
    
      # Preserve current selection if it exists in new choices
      current_selection <- isolate(input$cpue_fisheries)
    
      # Determine new selection
      if (is.null(current_selection) || length(current_selection) == 0) {
        new_selection <- all_index_fish
      } else {
        new_selection <- intersect(current_selection, all_index_fish)
        if (length(new_selection) == 0) {
          new_selection <- all_index_fish
        }
      }
    
      updatePickerInput(session, "cpue_fisheries", 
                        choices = choices,
                        selected = new_selection)
    }, ignoreInit = FALSE)
  
    # Reactive: generate CPUE plot
    cpue_plot_reactive <- reactive({
      req(rv$data_loaded, input$cpue_scenarios, input$cpue_fisheries)
    
      # Check if any selections made
      if (length(input$cpue_scenarios) == 0 || length(input$cpue_fisheries) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No scenarios or fisheries selected", 
                   size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      tryCatch({
        # Combine CPUE data from all selected scenarios
        cpue_all <- map_dfr(input$cpue_scenarios, function(scenario) {
          rep_obj <- rv$RepOut_list[[scenario]]
          fishery_map <- rv$FISHERY_MAPS[[scenario]]
        
          # Extract observed and predicted CPUE
          obs <- as.data.frame(cpue_obs(rep_obj))
          fit <- as.data.frame(cpue_pred(rep_obj))
        
          # Standardize column names
          names(obs)[names(obs) == "data"] <- "obs"
          names(fit)[names(fit) == "data"] <- "fit"
        
          # Merge observed and predicted
          cpue <- merge(obs, fit)
          cpue <- type.convert(cpue, as.is = TRUE)
        
          # Filter to selected fisheries
          cpue <- cpue[cpue$unit %in% as.numeric(input$cpue_fisheries), ]
        
          if (nrow(cpue) > 0) {
            cpue$Scenario <- scenario
            cpue$fishery_name <- sapply(as.character(cpue$unit), 
                                        function(x) get_fishery_name(x, fishery_map))
            cpue
          } else {
            NULL
          }
        })
      
        # Check if data exists
        if (is.null(cpue_all) || nrow(cpue_all) == 0) {
          p <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No CPUE data available", 
                     size = 6, color = "#999") +
            theme_void()
          return(p)
        }
      
        # Transform data (convert from log-scale)
        cpue_all <- cpue_all %>%
          mutate(
            year_season = year + (season - 1) / 4,
            obs = exp(obs),
            fit = exp(fit)
          )
      
        # Generate colors
        scenario_colors <- get_scenario_colors(input$cpue_scenarios)
      
        # Create CPUE plot
        p <- ggplot(cpue_all, aes(x = year_season)) +
          geom_point(aes(y = obs), size = 2, alpha = 0.6, color = "#E69F00") +
          geom_line(aes(y = fit, color = Scenario), linewidth = 1.2, alpha = 0.9) +
          facet_wrap(~fishery_name, scales = "free_y", ncol = 3) +
          scale_color_manual(values = scenario_colors) +
          labs(x = "Year + Season", y = "CPUE", 
               title = paste("CPUE Fits -", 
                             paste(input$cpue_scenarios, collapse = ", "))) +
          theme_bw(base_size = 13) +
          theme(
            legend.position = "top", 
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank()
          )
      
        return(p)
      
      }, error = function(e) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error:", e$message), 
                   size = 5, color = "red") +
          theme_void()
        return(p)
      })
    })
  
    # Render CPUE plot
    output$cpue_plot <- renderPlot({
      cpue_plot_reactive()
    })
  
    # ===========================================================================

    # CPUE DOWNLOAD
    # ---------------------------------------------------------------------------

    observeEvent(input$show_cpue_download_modal, {
      show_download_modal("cpue", "CPUE Fits Plot")
    })

    observeEvent(input$cpue_preset_wide, {
      updateNumericInput(session, "cpue_width", value = 16)
      updateNumericInput(session, "cpue_height", value = 9)
    })

    observeEvent(input$cpue_preset_standard, {
      updateNumericInput(session, "cpue_width", value = 12)
      updateNumericInput(session, "cpue_height", value = 9)
    })

    observeEvent(input$cpue_preset_square, {
      updateNumericInput(session, "cpue_width", value = 10)
      updateNumericInput(session, "cpue_height", value = 10)
    })

    output$cpue_download_confirm <- downloadHandler(
      filename = function() {
        format <- input$cpue_format
        paste0("cpue_fits_", Sys.Date(), ".", format)
      },
      content = function(file) {
        p <- cpue_plot_reactive()
        width <- input$cpue_width
        height <- input$cpue_height
        dpi <- as.numeric(input$cpue_dpi)
        format <- input$cpue_format

        if (format == "png") {
          ggsave(file, plot = p, width = width, height = height, dpi = dpi,
                 device = "png", bg = "white")
        } else if (format == "pdf") {
          ggsave(file, plot = p, width = width, height = height,
                 device = "pdf")
        } else if (format == "svg") {
          ggsave(file, plot = p, width = width, height = height,
                 device = "svg", bg = "white")
        } else if (format == "jpeg") {
          ggsave(file, plot = p, width = width, height = height, dpi = dpi,
                 device = "jpeg", bg = "white", quality = 95)
        }

        removeModal()
      }
    )

}
