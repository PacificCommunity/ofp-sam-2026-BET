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

            pickerInput(
              "lf_fisheries_all",
              "Fisheries (All-years view):",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 3",
                countSelectedText = "{0} fisheries selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search fisheries..."
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
                "By scenario" = "by_scenario",
                "All years combined" = "all_years"
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
      updatePickerInput(session, "lf_fisheries_all", choices = choices, selected = fisheries)
    
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
                        selected = compatible_models)
    })
  
    # Update year choices when fishery or scenarios change
    observeEvent(list(input$lf_fishery, input$lf_fisheries_all, input$lf_model, input$lf_view_mode), {
      req(rv$data_loaded, input$lf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      df <- rv$LengOut_list[[input$lf_model]]@lenfits
      if (identical(input$lf_view_mode, "all_years")) {
        selected_fisheries <- suppressWarnings(as.numeric(input$lf_fisheries_all))
        years <- df %>%
          filter(fishery %in% selected_fisheries) %>%
          pull(year) %>%
          unique() %>%
          sort()
      } else {
        years <- df %>% 
          filter(fishery == as.numeric(input$lf_fishery)) %>% 
          pull(year) %>% 
          unique() %>%
          sort()
      }
    
      if (length(years) == 0) {
        updatePickerInput(session, "lf_years", choices = NULL, selected = NULL)
        return()
      }
    
      updatePickerInput(session, "lf_years", 
                        choices = years,
                        selected = years)
    }, ignoreInit = TRUE)

    observeEvent(input$tabs, {
      req(rv$data_loaded)
      if (!identical(input$tabs, "lf")) return()
      req(input$lf_model, rv$LengOut_list[[input$lf_model]])

      df <- rv$LengOut_list[[input$lf_model]]@lenfits
      fisheries <- sort(unique(df$fishery))
      fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
      choices <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
      selected_fishery <- if (length(fisheries) > 0) fisheries[1] else NULL

      updateSelectInput(session, "lf_fishery", choices = choices, selected = selected_fishery)
      updatePickerInput(session, "lf_fisheries_all", choices = choices, selected = fisheries)

      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      compatible_models <- check_lf_compatibility_global(rv, input$lf_model, all_models)
      updatePickerInput(session, "lf_scenarios", choices = compatible_models, selected = compatible_models)

      years <- sort(unique(df$year))
      updatePickerInput(session, "lf_years", choices = years, selected = years)
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for LF
    lf_plot_height <- reactive({
      req(rv$data_loaded, input$lf_years)

      if (identical(input$lf_view_mode, "all_years")) {
        n_fisheries <- length(input$lf_fisheries_all)
        ncol_facet <- 3
        n_rows <- ceiling(max(n_fisheries, 1) / ncol_facet)
        return(min(max(350 + n_rows * 240, 550), 3200))
      }

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
      req(rv$data_loaded, input$lf_model, input$lf_fishery, input$lf_years)

      view_mode <- if (is.null(input$lf_view_mode)) "overlay" else input$lf_view_mode
      scenarios_to_use <- if (identical(view_mode, "by_scenario")) input$lf_model else input$lf_scenarios

      # Check if any scenarios selected (overlay/all-years)
      if (!identical(view_mode, "by_scenario") && length(scenarios_to_use) == 0) {
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
      combined_data <- map_dfr(scenarios_to_use, function(sc) {
        if (is.null(rv$LengOut_list[[sc]])) return(NULL)
        df <- rv$LengOut_list[[sc]]@lenfits
        if (identical(input$lf_view_mode, "all_years")) {
          selected_fisheries <- suppressWarnings(as.numeric(input$lf_fisheries_all))
          df %>%
            filter(fishery %in% selected_fisheries) %>%
            mutate(Scenario = sc)
        } else {
          if (as.numeric(input$lf_fishery) %in% unique(df$fishery)) {
            df %>% 
              filter(fishery == as.numeric(input$lf_fishery)) %>% 
              mutate(Scenario = sc)
          } else {
            NULL
          }
        }
      })
    
      # Check if data exists
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "#999") +
          theme_void()
        return(p)
      }

      # If sample_size exists, convert composition to sample-count scale.
      if ("sample_size" %in% names(combined_data)) {
        combined_data <- combined_data %>%
          mutate(
            obs = obs * sample_size,
            pred = pred * sample_size
          )
      }
    
      # Aggregate by scenario/fishery/year/length
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

      # Dynamic bar width with slight visual separation.
      lf_bar_width <- {
        lvals <- sort(unique(plot_data$length))
        if (length(lvals) <= 1) {
          1.8
        } else {
          d <- diff(lvals)
          d <- d[is.finite(d) & d > 0]
          if (length(d) == 0) 1.8 else min(d) * 0.98
        }
      }
    
      # Separate observed data (same across scenarios)
      obs_data <- plot_data %>%
        group_by(fishery, year, length) %>%
        summarise(obs = first(obs), .groups = "drop")
    
      fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
      fishery_name <- get_fishery_name(input$lf_fishery, fishery_map)
      obs_data <- obs_data %>%
        mutate(fishery_name = sapply(fishery, function(f) get_fishery_name(f, fishery_map)))
      plot_data <- plot_data %>%
        mutate(fishery_name = sapply(fishery, function(f) get_fishery_name(f, fishery_map)))
    
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
        n_years <= 30 ~ 9,
        TRUE ~ 8.5
      )
      observed_fill <- "#08519C"

      if (identical(view_mode, "all_years")) {
        all_year_obs <- obs_data %>%
          group_by(fishery_name, length) %>%
          summarise(obs = sum(obs, na.rm = TRUE), .groups = "drop")

        all_year_pred <- plot_data %>%
          group_by(Scenario, fishery_name, length) %>%
          summarise(pred = sum(pred, na.rm = TRUE), .groups = "drop")

        panel_count <- dplyr::n_distinct(all_year_obs$fishery_name)
        border_lwd <- dplyr::case_when(
          panel_count >= 24 ~ 0.03,
          panel_count >= 12 ~ 0.08,
          TRUE ~ 0.25
        )

        p <- ggplot() +
          geom_col(
            data = all_year_obs,
            aes(x = length, y = obs, fill = "Observed"),
            alpha = 1, width = lf_bar_width, position = "identity",
            colour = "white", linewidth = border_lwd
          ) +
          geom_line(
            data = all_year_pred,
            aes(x = length, y = pred, color = Scenario),
            linewidth = 1.2
          ) +
          facet_wrap(~fishery_name, scales = "free_y", ncol = 3) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_viridis_d() +
          labs(
            title = "All selected fisheries - all selected years combined",
            subtitle = paste0("Years: ", min(input$lf_years), " to ", max(input$lf_years)),
            x = "Length (cm)", y = "Sample count"
          ) +
          theme_bw(base_size = 12.5) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10.5),
            strip.text = element_text(size = 9.5, face = "bold")
          )
      } else if (identical(view_mode, "by_scenario")) {
        panel_count <- dplyr::n_distinct(plot_data$year)
        border_lwd <- dplyr::case_when(
          panel_count >= 24 ~ 0.03,
          panel_count >= 12 ~ 0.08,
          TRUE ~ 0.25
        )
        p <- ggplot() +
          geom_col(
            data = obs_data,
            aes(x = length, y = obs, fill = "Observed"),
            alpha = 1, width = lf_bar_width, position = "identity",
            colour = "white", linewidth = border_lwd
          ) +
          geom_line(
            data = plot_data,
            aes(x = length, y = pred, color = "Predicted"),
            linewidth = 1.2
          ) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_manual(values = c("Predicted" = "#E31A1C")) +
          labs(
            title = paste(fishery_name, "-", input$lf_model),
            x = "Length (cm)", y = "Sample count"
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = strip_size, face = "bold"),
            panel.spacing = unit(0.3, "lines")
          )
      } else {
        panel_count <- dplyr::n_distinct(plot_data$year)
        border_lwd <- dplyr::case_when(
          panel_count >= 24 ~ 0.03,
          panel_count >= 12 ~ 0.08,
          TRUE ~ 0.25
        )

        p <- ggplot() +
          geom_col(data = obs_data,
                   aes(x = length, y = obs, fill = "Observed"),
                   alpha = 1, width = lf_bar_width, position = "identity",
                   colour = "white", linewidth = border_lwd) +
          geom_line(data = plot_data,
                    aes(x = length, y = pred, color = Scenario),
                    linewidth = 1.2) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_viridis_d() +
          labs(title = paste(fishery_name, "- Base:", input$lf_model,
                             paste0("(", n_years, " years)")),
               x = "Length (cm)", y = "Sample count") +
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
    lf_plot_reactive <- bindCache(
      lf_plot_reactive,
      input$lf_model,
      input$lf_view_mode,
      input$lf_fishery,
      input$lf_fisheries_all,
      input$lf_scenarios,
      input$lf_years
    )
  
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
