mod_wf_ui <- function() {
      tabItem(
        tabName = "wf",
        h2("Weight Frequency Fits", style = "color: #dd4b39;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            
            # Model selector (single selection)
            selectInput(
              "wf_model",
              "Model:",
              choices = NULL,
              selected = NULL
            ),
            
            # Fishery selector with navigation buttons
            div(
              style = "margin-bottom: 15px;",
              tags$label("Fishery:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
              div(
                style = "display: flex; align-items: center; gap: 5px;",
                actionButton("wf_prev", "", icon = icon("chevron-left"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;"),
                div(
                  style = "flex: 1;",
                  selectInput("wf_fishery", NULL, choices = NULL)
                ),
                actionButton("wf_next", "", icon = icon("chevron-right"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;")
              )
            ),
            
            # Year selector with Select All / Deselect All
            pickerInput(
              "wf_years",
              "Years:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "All Years",
                deselectAllText = "None",
                selectedTextFormat = "count > 5",
                countSelectedText = "{0} years selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search years...",
                size = 10
              )
            ),
            
            shiny::hr(),
            
            # Scenarios selector for overlay (only compatible models)
            pickerInput(
              "wf_scenarios",
              "Overlay Scenarios:",
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
            
            helpText("💡 Only models with identical fishery structure and names can be overlaid", 
                     style = "font-size: 11px; color: #666; font-style: italic;"),
            
            shiny::hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_wf_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download"))
          ),
          
          # Weight frequency plot panel (DYNAMIC HEIGHT)
          uiOutput("wf_plot_box")
        )
      )

}

mod_wf_server <- function(input, output, session, rv) {
    # TAB 7: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
    # ===========================================================================
  
    # Helper function to check if models are compatible for WF overlay
    check_wf_compatibility <- function(base_model, compare_models) {
      if (is.null(rv$WeightOut_list[[base_model]])) return(character(0))
    
      base_fisheries <- unique(rv$WeightOut_list[[base_model]]@wgtfits$fishery)
      base_years <- unique(rv$WeightOut_list[[base_model]]@wgtfits$year)
    
      # Get base model fishery names
      base_fishery_names <- sapply(base_fisheries, function(f) {
        rv$FISHERY_MAPS[[base_model]][[as.character(f)]]
      })
    
      compatible <- sapply(compare_models, function(m) {
        if (is.null(rv$WeightOut_list[[m]])) return(FALSE)
      
        m_fisheries <- unique(rv$WeightOut_list[[m]]@wgtfits$fishery)
        m_years <- unique(rv$WeightOut_list[[m]]@wgtfits$year)
      
        # Get comparison model fishery names
        m_fishery_names <- sapply(m_fisheries, function(f) {
          rv$FISHERY_MAPS[[m]][[as.character(f)]]
        })
      
        # Check: same fishery IDs, same fishery names, same years
        same_ids <- setequal(base_fisheries, m_fisheries)
        same_names <- identical(sort(base_fishery_names), sort(m_fishery_names))
        same_years <- setequal(base_years, m_years)
      
        same_ids && same_names && same_years
      })
    
      names(compatible)[compatible]
    }
  
    # Update fishery choices when base model changes
    observeEvent(input$wf_model, {
      req(rv$data_loaded, input$wf_model, rv$WeightOut_list[[input$wf_model]])
    
      # Get fisheries from selected model
      fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
      if (length(fisheries) == 0) {
        updateSelectInput(session, "wf_fishery", choices = character(0))
        return()
      }
    
      # Create named choices
      fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
      choices <- setNames(fisheries, 
                          sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    
      # Preserve current selection if valid
      current_selection <- isolate(input$wf_fishery)
      if (!is.null(current_selection) && current_selection %in% fisheries) {
        selected <- current_selection
      } else {
        selected <- fisheries[1]
      }
    
      updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
    
      # Update compatible scenarios for overlay
      all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
      compatible_models <- check_wf_compatibility(input$wf_model, all_models)
    
      # Show notification if some models are incompatible
      n_incompatible <- length(all_models) - length(compatible_models)
      if (n_incompatible > 0) {
        showNotification(
          HTML(paste0(
            "⚠️ <strong>", n_incompatible, " model(s) excluded from overlay</strong><br/>",
            "Reason: Different fishery structure or names"
          )),
          type = "warning",
          duration = 4
        )
      }
    
      updatePickerInput(session, "wf_scenarios",
                        choices = compatible_models,
                        selected = input$wf_model)
    })
  
    # Update year choices when fishery or model change
    observeEvent(list(input$wf_fishery, input$wf_model), {
      req(rv$data_loaded, input$wf_fishery, input$wf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
      df <- rv$WeightOut_list[[input$wf_model]]@wgtfits
      years <- df %>% 
        filter(fishery == as.numeric(input$wf_fishery)) %>% 
        pull(year) %>% 
        unique() %>%
        sort()
    
      if (length(years) == 0) {
        updatePickerInput(session, "wf_years", choices = NULL, selected = NULL)
        return()
      }
    
      # Preserve current selection if valid
      current_selection <- isolate(input$wf_years)
      if (!is.null(current_selection) && all(current_selection %in% years)) {
        selected <- current_selection
      } else {
        selected <- years
      }
    
      updatePickerInput(session, "wf_years", 
                        choices = years,
                        selected = selected)
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for WF
    wf_plot_height <- reactive({
      req(rv$data_loaded, input$wf_years)
    
      n_years <- length(input$wf_years)
    
      if (n_years == 0) return(400)
    
      # Determine number of columns based on year count
      ncol_facet <- case_when(
        n_years <= 6 ~ 2,
        n_years <= 12 ~ 3,
        n_years <= 20 ~ 4,
        n_years <= 30 ~ 5,
        TRUE ~ 6
      )
    
      # Calculate rows needed
      n_rows <- ceiling(n_years / ncol_facet)
    
      # Height formula: base + height per row
      base_height <- 150
      height_per_row <- 200
      total_height <- base_height + (n_rows * height_per_row)
    
      # Constrain between 400 and 3000 pixels
      min(max(total_height, 400), 3000)
    })
  
    # Reactive: generate weight frequency plot
    wf_plot_reactive <- reactive({
      req(rv$data_loaded, input$wf_model, input$wf_fishery, input$wf_scenarios, input$wf_years)
    
      # Check if any scenarios selected
      if (length(input$wf_scenarios) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Check if any years selected
      if (length(input$wf_years) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No years selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Combine data from selected scenarios
      combined_data <- map_dfr(input$wf_scenarios, function(sc) {
        if (is.null(rv$WeightOut_list[[sc]])) return(NULL)
        df <- rv$WeightOut_list[[sc]]@wgtfits
        if (as.numeric(input$wf_fishery) %in% unique(df$fishery)) {
          df %>% 
            filter(fishery == as.numeric(input$wf_fishery)) %>% 
            mutate(Scenario = sc)
        } else {
          NULL
        }
      })
    
      # Check if data exists
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Aggregate by scenario, year, weight
      plot_data <- combined_data %>%
        group_by(Scenario, fishery, year, weight) %>%
        summarise(obs = sum(obs, na.rm = TRUE), 
                  pred = sum(pred, na.rm = TRUE), 
                  .groups = "drop") %>%
        filter(obs > 0 | pred > 0)
    
      # Apply year filter
      plot_data <- plot_data %>%
        filter(year %in% input$wf_years)
    
      # Check if data exists after filtering
      if (nrow(plot_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data for selected years", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Separate observed data
      obs_data <- plot_data %>%
        group_by(year, weight) %>%
        summarise(obs = first(obs), .groups = "drop")
    
      fishery_name <- get_fishery_name(input$wf_fishery, rv$FISHERY_MAPS[[input$wf_model]])
    
      # Determine optimal layout
      n_years <- length(unique(plot_data$year))
      ncol_facet <- case_when(
        n_years <= 6 ~ 2,
        n_years <= 12 ~ 3,
        n_years <= 20 ~ 4,
        n_years <= 30 ~ 5,
        TRUE ~ 6
      )
    
      strip_size <- case_when(
        n_years <= 12 ~ 10,
        n_years <= 20 ~ 9,
        n_years <= 30 ~ 8,
        TRUE ~ 7
      )
    
      # Create overlay plot
      p <- ggplot() +
        geom_col(data = obs_data, 
                 aes(x = weight, y = obs, fill = "Observed"), 
                 alpha = 0.7, width = 2, position = "identity") +
        geom_line(data = plot_data,
                  aes(x = weight, y = pred, color = Scenario), 
                  linewidth = 1.2) +
        facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
        scale_fill_manual(values = c("Observed" = "#E69F00")) +
        scale_color_viridis_d() +
        labs(title = paste(fishery_name, "- Base:", input$wf_model,
                           paste0("(", n_years, " years)")),
             x = "Weight (kg)", y = "Frequency") +
        theme_bw(base_size = 12) +
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          strip.background = element_rect(fill = "grey90"),
          strip.text = element_text(size = strip_size, face = "bold"),
          panel.spacing = unit(0.3, "lines")
        )
    
      return(p)
    })
  
    # Render weight frequency plot
  output$wf_plot <- renderPlot({
    wf_plot_reactive()
  })
  
  # Render dynamic box for WF with calculated height
    output$wf_plot_box <- renderUI({
      height <- wf_plot_height()
    
      box(
        title = "Weight Frequency",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
      plotOutput("wf_plot", height = paste0(height, "px"))
    )
  })
  
  # ===========================================================================

  # WEIGHT FREQUENCY DOWNLOAD
  # ---------------------------------------------------------------------------

  observeEvent(input$show_wf_download_modal, {
    show_download_modal("wf", "Weight Frequency Plot")
  })

  observeEvent(input$wf_preset_wide, {
    updateNumericInput(session, "wf_width", value = 16)
    updateNumericInput(session, "wf_height", value = 10)
  })

  observeEvent(input$wf_preset_standard, {
    updateNumericInput(session, "wf_width", value = 12)
    updateNumericInput(session, "wf_height", value = 9)
  })

  observeEvent(input$wf_preset_square, {
    updateNumericInput(session, "wf_width", value = 10)
    updateNumericInput(session, "wf_height", value = 10)
  })

  output$wf_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$wf_format
      paste0("weight_freq_", input$wf_model, "_", input$wf_fishery, "_",
             Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- wf_plot_reactive()
      width <- input$wf_width
      height <- input$wf_height
      dpi <- as.numeric(input$wf_dpi)
      format <- input$wf_format

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
