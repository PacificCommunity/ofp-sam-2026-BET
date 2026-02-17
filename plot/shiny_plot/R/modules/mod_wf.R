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

            pickerInput(
              "wf_fisheries_all",
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

            radioButtons(
              "wf_view_mode",
              "View:",
              choices = c(
                "Overlay scenarios" = "overlay",
                "By scenario" = "by_scenario",
                "All years combined" = "all_years"
              ),
              selected = "all_years"
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
      updatePickerInput(session, "wf_fisheries_all", choices = choices, selected = fisheries)
    
      # Update compatible scenarios for overlay
      all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
      compatible_models <- check_wf_compatibility_global(rv, input$wf_model, all_models)
    
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
                        selected = compatible_models)
    })
  
    # Update year choices when fishery or model change
    observeEvent(list(input$wf_fishery, input$wf_fisheries_all, input$wf_model, input$wf_view_mode), {
      req(rv$data_loaded, input$wf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
      df <- rv$WeightOut_list[[input$wf_model]]@wgtfits
      if (identical(input$wf_view_mode, "all_years")) {
        selected_fisheries <- suppressWarnings(as.numeric(input$wf_fisheries_all))
        years <- df %>%
          filter(fishery %in% selected_fisheries) %>%
          pull(year) %>%
          unique() %>%
          sort()
      } else {
        years <- df %>% 
          filter(fishery == as.numeric(input$wf_fishery)) %>% 
          pull(year) %>% 
          unique() %>%
          sort()
      }
    
      if (length(years) == 0) {
        updatePickerInput(session, "wf_years", choices = NULL, selected = NULL)
        return()
      }
    
      updatePickerInput(session, "wf_years", 
                        choices = years,
                        selected = years)
    }, ignoreInit = TRUE)

    observeEvent(input$tabs, {
      req(rv$data_loaded)
      if (!identical(input$tabs, "wf")) return()
      req(input$wf_model, rv$WeightOut_list[[input$wf_model]])

      df <- rv$WeightOut_list[[input$wf_model]]@wgtfits
      fisheries <- sort(unique(df$fishery))
      fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
      choices <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
      selected_fishery <- if (length(fisheries) > 0) fisheries[1] else NULL

      updateSelectInput(session, "wf_fishery", choices = choices, selected = selected_fishery)
      updatePickerInput(session, "wf_fisheries_all", choices = choices, selected = fisheries)

      all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
      compatible_models <- check_wf_compatibility_global(rv, input$wf_model, all_models)
      updatePickerInput(session, "wf_scenarios", choices = compatible_models, selected = compatible_models)

      years <- sort(unique(df$year))
      updatePickerInput(session, "wf_years", choices = years, selected = years)
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for WF
    wf_plot_height <- reactive({
      req(rv$data_loaded, input$wf_years)

      if (identical(input$wf_view_mode, "all_years")) {
        n_fisheries <- length(input$wf_fisheries_all)
        ncol_facet <- 3
        n_rows <- ceiling(max(n_fisheries, 1) / ncol_facet)
        return(min(max(350 + n_rows * 240, 550), 3200))
      }

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
      req(rv$data_loaded, input$wf_model, input$wf_fishery, input$wf_years)

      view_mode <- if (is.null(input$wf_view_mode)) "overlay" else input$wf_view_mode
      scenarios_to_use <- if (identical(view_mode, "by_scenario")) input$wf_model else input$wf_scenarios

      # Check if any scenarios selected (overlay/all-years)
      if (!identical(view_mode, "by_scenario") && length(scenarios_to_use) == 0) {
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
    
      normalize_name <- function(x) {
        x <- as.character(x)
        x <- trimws(x)
        x <- gsub("\\s+", " ", x)
        tolower(x)
      }

      base_map <- rv$FISHERY_MAPS[[input$wf_model]]
      base_fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
      base_name_df <- data.frame(
        fishery = as.numeric(base_fisheries),
        fishery_display = sapply(base_fisheries, function(f) get_fishery_name(f, base_map)),
        stringsAsFactors = FALSE
      ) %>%
        mutate(fishery_label_norm = normalize_name(fishery_display))

      base_lookup <- base_name_df %>%
        group_by(fishery_label_norm) %>%
        summarise(fishery_display = first(fishery_display), .groups = "drop")

      selected_name_norm <- if (identical(input$wf_view_mode, "all_years")) {
        sel_ids <- suppressWarnings(as.numeric(input$wf_fisheries_all))
        base_name_df %>%
          filter(fishery %in% sel_ids) %>%
          pull(fishery_label_norm) %>%
          unique()
      } else {
        base_name_df %>%
          filter(fishery == as.numeric(input$wf_fishery)) %>%
          pull(fishery_label_norm) %>%
          unique()
      }
      if (length(selected_name_norm) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No fisheries selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }

      # Combine data from selected scenarios (name-based fishery matching across models).
      # Join-based lookup is much faster than row-wise name lookup.
      combined_data <- map_dfr(scenarios_to_use, function(sc) {
        if (is.null(rv$WeightOut_list[[sc]])) return(NULL)
        df <- rv$WeightOut_list[[sc]]@wgtfits
        sc_lookup <- rv$FISHERY_MAPS[[sc]] %>%
          transmute(
            fishery = as.numeric(fishery),
            fishery_display_sc = as.character(fishery_name),
            fishery_label_norm = normalize_name(fishery_display_sc)
          ) %>%
          distinct(fishery, .keep_all = TRUE)

        df %>%
          left_join(sc_lookup, by = "fishery") %>%
          filter(!is.na(fishery_label_norm)) %>%
          filter(fishery_label_norm %in% selected_name_norm) %>%
          left_join(base_lookup, by = "fishery_label_norm") %>%
          mutate(
            fishery_display = ifelse(is.na(fishery_display), fishery_display_sc, fishery_display),
            Scenario = sc
          )
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
    
      # Aggregate by scenario/fishery/year/weight
      plot_data <- combined_data %>%
        group_by(Scenario, fishery_display, fishery_label_norm, year, weight) %>%
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

      # Use bin spacing so bars touch exactly while avoiding overlap.
      wf_bar_width <- {
        wvals <- sort(unique(plot_data$weight))
        if (length(wvals) <= 1) {
          0.5
        } else {
          d <- diff(wvals)
          d <- d[is.finite(d) & d > 0]
          if (length(d) == 0) 0.5 else min(d)
        }
      }
    
      # Separate observed data
      obs_data <- plot_data %>%
        group_by(fishery_display, year, weight) %>%
        summarise(obs = median(obs, na.rm = TRUE), .groups = "drop")
    
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
      observed_fill <- "#08519C"

      if (identical(view_mode, "all_years")) {
        all_year_obs <- obs_data %>%
          group_by(fishery_display, weight) %>%
          summarise(obs = sum(obs, na.rm = TRUE), .groups = "drop")

        all_year_pred <- plot_data %>%
          group_by(Scenario, fishery_display, weight) %>%
          summarise(pred = sum(pred, na.rm = TRUE), .groups = "drop")

        panel_count <- dplyr::n_distinct(all_year_obs$fishery_display)
        border_lwd <- dplyr::case_when(
          panel_count >= 24 ~ 0.03,
          panel_count >= 12 ~ 0.08,
          TRUE ~ 0.25
        )

        p <- ggplot() +
          geom_col(
            data = all_year_obs,
            aes(x = weight, y = obs, fill = "Observed"),
            alpha = 1, width = wf_bar_width, position = "identity",
            colour = "white", linewidth = border_lwd
          ) +
          geom_line(
            data = all_year_pred,
            aes(x = weight, y = pred, color = Scenario),
            linewidth = 1.2
          ) +
          facet_wrap(~fishery_display, scales = "free_y", ncol = 3) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_viridis_d() +
          labs(
            title = "All selected fisheries - all selected years combined",
            subtitle = paste0("Years: ", min(input$wf_years), " to ", max(input$wf_years)),
            x = "Weight (kg)", y = "Sample count"
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            strip.text = element_text(size = 9, face = "bold")
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
            aes(x = weight, y = obs, fill = "Observed"),
            alpha = 1, width = wf_bar_width, position = "identity",
            colour = "white", linewidth = border_lwd
          ) +
          geom_line(
            data = plot_data,
            aes(x = weight, y = pred, color = "Predicted"),
            linewidth = 1.2
          ) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_manual(values = c("Predicted" = "#E31A1C")) +
          labs(
            title = paste(fishery_name, "-", input$wf_model),
            x = "Weight (kg)", y = "Sample count"
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
                   aes(x = weight, y = obs, fill = "Observed"),
                   alpha = 1, width = wf_bar_width, position = "identity",
                   colour = "white", linewidth = border_lwd) +
          geom_line(data = plot_data,
                    aes(x = weight, y = pred, color = Scenario),
                    linewidth = 1.2) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill)) +
          scale_color_viridis_d() +
          labs(title = paste(fishery_name, "- Base:", input$wf_model,
                             paste0("(", n_years, " years)")),
               x = "Weight (kg)", y = "Sample count") +
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
    wf_plot_reactive <- bindCache(
      wf_plot_reactive,
      input$wf_model,
      input$wf_view_mode,
      input$wf_fishery,
      input$wf_fisheries_all,
      input$wf_scenarios,
      input$wf_years
    )
  
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
