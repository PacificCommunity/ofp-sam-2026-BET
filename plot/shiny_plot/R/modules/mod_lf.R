mod_lf_ui <- function() {
      tabItem(
        tabName = "lf",
        h2("Length Frequency Fits", style = "color: #605ca8;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            
            # Model selector (single selection)
            selectInput(
              "lf_model",
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
                actionButton("lf_prev", "", icon = icon("chevron-left"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;"),
                div(
                  style = "flex: 1;",
                  selectInput("lf_fishery", NULL, choices = NULL)
                ),
                actionButton("lf_next", "", icon = icon("chevron-right"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;")
              )
            ),
            
            # Year selector with Select All / Deselect All
            pickerInput(
              "lf_years",
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
              "lf_scenarios",
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

            radioButtons(
              "lf_view_mode",
              "View:",
              choices = c(
                "Overlay scenarios" = "overlay",
                "By scenario" = "by_scenario"
              ),
              selected = "overlay"
            ),
            
            helpText("💡 Only models with identical fishery structure and names can be overlaid", 
                     style = "font-size: 11px; color: #666; font-style: italic;"),
            
            shiny::hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_lf_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download"))
          ),
          
          # Length frequency plot panel (DYNAMIC HEIGHT)
          uiOutput("lf_plot_box")
        )
      )
}

mod_lf_server <- function(input, output, session, rv) {
    # TAB 6: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
    # ===========================================================================
  
    # Update fishery choices when base model changes
    observeEvent(input$lf_model, {
      req(rv$data_loaded, input$lf_model, rv$LengOut_list[[input$lf_model]])
    
      # Get fisheries from selected model
      fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
      if (length(fisheries) == 0) {
        updateSelectInput(session, "lf_fishery", choices = character(0))
        return()
      }
    
      # Create named choices
      fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
      choices <- setNames(fisheries, 
                          sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    
      # Preserve current selection if valid
      current_selection <- isolate(input$lf_fishery)
      if (!is.null(current_selection) && current_selection %in% fisheries) {
        selected <- current_selection
      } else {
        selected <- fisheries[1]
      }
    
      updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
    
      # Update compatible scenarios for overlay
      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      compatible_models <- check_lf_compatibility_global(rv, input$lf_model, all_models)
    
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
    
      updatePickerInput(session, "lf_scenarios",
                        choices = compatible_models,
                        selected = input$lf_model)
    })
  
    # Update year choices when fishery or scenarios change
    observeEvent(list(input$lf_fishery, input$lf_model), {
      req(rv$data_loaded, input$lf_fishery, input$lf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      df <- rv$LengOut_list[[input$lf_model]]@lenfits
      years <- df %>% 
        filter(fishery == as.numeric(input$lf_fishery)) %>% 
        pull(year) %>% 
        unique() %>%
        sort()
    
      if (length(years) == 0) {
        updatePickerInput(session, "lf_years", choices = NULL, selected = NULL)
        return()
      }
    
      # Preserve current selection if valid
      current_selection <- isolate(input$lf_years)
      if (!is.null(current_selection) && all(current_selection %in% years)) {
        selected <- current_selection
      } else {
        selected <- years
      }
    
      updatePickerInput(session, "lf_years", 
                        choices = years,
                        selected = selected)
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for LF
    lf_plot_height <- reactive({
      req(rv$data_loaded, input$lf_years)
    
      n_years <- length(input$lf_years)
    
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
  
    # Reactive: generate length frequency plot
    lf_plot_reactive <- reactive({
      req(rv$data_loaded, input$lf_model, input$lf_fishery, input$lf_scenarios, input$lf_years)
    
      # Check if any scenarios selected
      if (length(input$lf_scenarios) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Check if any years selected
      if (length(input$lf_years) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No years selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Combine data from selected scenarios
      combined_data <- map_dfr(input$lf_scenarios, function(sc) {
        if (is.null(rv$LengOut_list[[sc]])) return(NULL)
        df <- rv$LengOut_list[[sc]]@lenfits
        if (as.numeric(input$lf_fishery) %in% unique(df$fishery)) {
          df %>% 
            filter(fishery == as.numeric(input$lf_fishery)) %>% 
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
    
      # Aggregate by scenario, year, length
      plot_data <- combined_data %>%
        group_by(Scenario, fishery, year, length) %>%
        summarise(obs = sum(obs, na.rm = TRUE), 
                  pred = sum(pred, na.rm = TRUE), 
                  .groups = "drop") %>%
        filter(obs > 0 | pred > 0)
    
      # Apply year filter
      plot_data <- plot_data %>%
        filter(year %in% input$lf_years)
    
      # Check if data exists after filtering
      if (nrow(plot_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data for selected years", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Separate observed data (same across scenarios)
      obs_data <- plot_data %>%
        group_by(year, length) %>%
        summarise(obs = first(obs), .groups = "drop")
    
      fishery_name <- get_fishery_name(input$lf_fishery, rv$FISHERY_MAPS[[input$lf_model]])
    
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
    
      view_mode <- if (is.null(input$lf_view_mode)) "overlay" else input$lf_view_mode

      if (identical(view_mode, "by_scenario")) {
        p <- ggplot() +
          geom_col(
            data = plot_data,
            aes(x = length, y = obs, fill = "Observed"),
            alpha = 0.55, width = 2, position = "identity"
          ) +
          geom_line(
            data = plot_data,
            aes(x = length, y = pred, color = Scenario),
            linewidth = 1
          ) +
          facet_grid(Scenario ~ year, scales = "free_y") +
          scale_fill_manual(values = c("Observed" = "#E69F00")) +
          scale_color_viridis_d() +
          labs(
            title = paste(fishery_name, "- by scenario"),
            x = "Length (cm)", y = "Frequency"
          ) +
          theme_bw(base_size = 11) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = 8, face = "bold"),
            panel.spacing = unit(0.2, "lines")
          )
      } else {
        p <- ggplot() +
          geom_col(data = obs_data,
                   aes(x = length, y = obs, fill = "Observed"),
                   alpha = 0.7, width = 2, position = "identity") +
          geom_line(data = plot_data,
                    aes(x = length, y = pred, color = Scenario),
                    linewidth = 1.2) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = "#E69F00")) +
          scale_color_viridis_d() +
          labs(title = paste(fishery_name, "- Base:", input$lf_model,
                             paste0("(", n_years, " years)")),
               x = "Length (cm)", y = "Frequency") +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = strip_size, face = "bold"),
            panel.spacing = unit(0.3, "lines")
          )
      }
    
      return(p)
    })
  
    # Render length frequency plot
    output$lf_plot <- renderPlot({
      lf_plot_reactive()
    })
  
    # Render dynamic box for LF with calculated height
    output$lf_plot_box <- renderUI({
      height <- lf_plot_height()
    
      box(
        title = "Length Frequency",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
      plotOutput("lf_plot", height = paste0(height, "px"))
    )
  })
  
  # ===========================================================================

  # LENGTH FREQUENCY DOWNLOAD
  # ---------------------------------------------------------------------------

  observeEvent(input$show_lf_download_modal, {
    show_download_modal("lf", "Length Frequency Plot")
  })

  observeEvent(input$lf_preset_wide, {
    updateNumericInput(session, "lf_width", value = 16)
    updateNumericInput(session, "lf_height", value = 10)
  })

  observeEvent(input$lf_preset_standard, {
    updateNumericInput(session, "lf_width", value = 12)
    updateNumericInput(session, "lf_height", value = 9)
  })

  observeEvent(input$lf_preset_square, {
    updateNumericInput(session, "lf_width", value = 10)
    updateNumericInput(session, "lf_height", value = 10)
  })

  output$lf_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$lf_format
      paste0("length_freq_", input$lf_model, "_", input$lf_fishery, "_",
             Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- lf_plot_reactive()
      width <- input$lf_width
      height <- input$lf_height
      dpi <- as.numeric(input$lf_dpi)
      format <- input$lf_format

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
