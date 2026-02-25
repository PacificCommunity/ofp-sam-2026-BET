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
            
            # Scenario selector (1 selected = single, 2+ selected = overlay)
            pickerInput(
              "lf_scenarios",
              "Models:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} models selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search models...",
                size = 10
              )
            ),
            # Hidden base model selector (first selected scenario) used by existing server logic.
            tags$div(
              style = "display:none;",
              selectInput("lf_model", NULL, choices = NULL, selected = NULL)
            ),
            
            # Fishery selector with navigation buttons
            div(
              style = "margin-bottom: 15px;",
              tags$label("Fishery (individual view):", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
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
            
            radioButtons(
              "lf_view_mode",
              "View:",
              choices = c(
                "All fisheries (years combined)" = "all_fisheries",
                "Individual fishery" = "individual_fishery"
              ),
              selected = "all_fisheries"
            ),
            radioButtons(
              "lf_plot_style",
              "Plot style:",
              choices = c(
                "Histogram fits" = "hist",
                "Bubble residuals" = "bubble"
              ),
              selected = "hist"
            ),
            numericInput("lf_facet_ncol", "Facet columns:", value = 3, min = 1, max = 12, step = 1),
            
            helpText("💡 Compatible models only. Check 1 model for single display, 2+ for overlay.", 
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
  
    observeEvent(input$lf_scenarios, {
      req(rv$data_loaded)
      sc <- input$lf_scenarios
      if (is.null(sc) || length(sc) == 0) {
        updateSelectInput(session, "lf_model", choices = character(0), selected = character(0))
        return()
      }
      cur_base <- isolate(input$lf_model)
      base_sel <- if (!is.null(cur_base) && cur_base %in% sc) cur_base else sc[1]
      updateSelectInput(session, "lf_model", choices = sc, selected = base_sel)
    }, ignoreInit = FALSE)

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
    
      current_scenarios <- isolate(input$lf_scenarios)
      if (is.null(current_scenarios) || length(current_scenarios) == 0) {
        selected_scenarios <- compatible_models
      } else {
        selected_scenarios <- intersect(current_scenarios, compatible_models)
        if (length(selected_scenarios) == 0) selected_scenarios <- compatible_models
      }
      updatePickerInput(session, "lf_scenarios",
                        choices = compatible_models,
                        selected = selected_scenarios)
    })
  
    # Update year choices when fishery or scenarios change
    observeEvent(list(input$lf_fishery, input$lf_fisheries_all, input$lf_model, input$lf_view_mode), {
      req(rv$data_loaded, input$lf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      df <- rv$LengOut_list[[input$lf_model]]@lenfits
      if (identical(input$lf_view_mode, "all_fisheries")) {
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

      current_years <- isolate(input$lf_years)
      if (is.null(current_years)) current_years <- character(0)
      selected_years <- intersect(as.character(current_years), as.character(years))
      if (length(selected_years) == 0) selected_years <- as.character(years)

      # Avoid unnecessary input resets (which trigger an extra re-render) when paging fisheries.
      if (identical(sort(as.character(current_years)), sort(as.character(selected_years))) &&
          length(current_years) > 0) {
        return()
      }

      freezeReactiveValue(input, "lf_years")
      updatePickerInput(session, "lf_years",
                        choices = years,
                        selected = selected_years)
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
      current_scenarios <- isolate(input$lf_scenarios)
      if (is.null(current_scenarios) || length(current_scenarios) == 0) {
        selected_scenarios <- compatible_models
      } else {
        selected_scenarios <- intersect(current_scenarios, compatible_models)
        if (length(selected_scenarios) == 0) selected_scenarios <- compatible_models
      }
      updatePickerInput(session, "lf_scenarios", choices = compatible_models, selected = selected_scenarios)

      years <- sort(unique(df$year))
      updatePickerInput(session, "lf_years", choices = years, selected = years)
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for LF
    lf_plot_height <- reactive({
      req(rv$data_loaded, input$lf_years)
      facet_ncol <- suppressWarnings(as.integer(input$lf_facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
      facet_ncol <- min(max(facet_ncol, 1), 12)
      plot_style <- if (is.null(input$lf_plot_style)) "hist" else input$lf_plot_style

      if (identical(input$lf_view_mode, "all_fisheries")) {
        n_fisheries <- length(input$lf_fisheries_all)
        n_rows <- ceiling(max(n_fisheries, 1) / facet_ncol)
        if (identical(plot_style, "bubble")) {
          n_scen <- max(length(input$lf_scenarios), 1)
          n_panel_rows <- ceiling(n_scen / facet_ncol)
          panel_height <- min(max(180 + max(n_fisheries, 1) * 16, 280), 760)
          return(min(max(120 + n_panel_rows * panel_height, 420), 2600))
        }
        return(min(max(350 + n_rows * 240, 550), 3200))
      }

      n_years <- length(input$lf_years)
    
      if (n_years == 0) return(400)

        if (identical(plot_style, "bubble")) {
          n_scen <- max(length(input$lf_scenarios), 1)
          n_panel_rows <- ceiling(n_scen / facet_ncol)
          panel_height <- min(max(220 + n_years * 8, 320), 820)
          return(min(max(140 + n_panel_rows * panel_height, 420), 2600))
        }
    
      # Calculate rows needed
      n_rows <- ceiling(n_years / facet_ncol)
    
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

      view_mode <- if (is.null(input$lf_view_mode)) "all_fisheries" else input$lf_view_mode
      plot_style <- if (is.null(input$lf_plot_style)) "hist" else input$lf_plot_style
      scenarios_to_use <- input$lf_scenarios

      # Check if any scenarios selected
      if (length(scenarios_to_use) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No models selected", size = 6, color = "#999") +
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
        if (identical(input$lf_view_mode, "all_fisheries")) {
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

      # Dynamic bar width close to bin spacing; use a subtle stroke for separation.
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
      ncol_facet <- suppressWarnings(as.integer(input$lf_facet_ncol))
      if (!is.finite(ncol_facet) || ncol_facet < 1) ncol_facet <- 3
      ncol_facet <- min(max(ncol_facet, 1), 12)
    
      strip_size <- case_when(
        n_years <= 12 ~ 10,
        n_years <= 20 ~ 9,
        n_years <= 30 ~ 9,
        TRUE ~ 8.5
      )
      observed_fill <- "#2C6E63"
      observed_border <- "#173F39"
      bubble_expand_all <- ggplot2::expansion(mult = c(0.02, 0.02))
      bubble_expand_year <- ggplot2::expansion(mult = c(0.08, 0.08))

      if (identical(plot_style, "bubble")) {
        if (identical(view_mode, "all_fisheries")) {
          bubble_data <- plot_data %>%
            group_by(Scenario, fishery_name, length) %>%
            summarise(
              obs = sum(obs, na.rm = TRUE),
              pred = sum(pred, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              resid = pred - obs,
              abs_resid = abs(resid)
            ) %>%
            filter(abs_resid > 0)

          if (nrow(bubble_data) == 0) {
            p <- ggplot() +
              annotate("text", x = 0.5, y = 0.5, label = "No residual differences to display", size = 6, color = "#999") +
              theme_void()
            return(p)
          }

          if (length(unique(bubble_data$Scenario)) <= 1) {
            p <- ggplot(bubble_data, aes(x = length, y = fishery_name)) +
              geom_point(
                aes(size = abs_resid, fill = resid),
                shape = 21, color = "#1f1f1f", stroke = 0.18, alpha = 0.9
              ) +
              scale_size_continuous(name = "|Pred - Obs|", range = c(0.8, 5.8), trans = "sqrt") +
              scale_fill_gradient2(
                name = "Pred - Obs",
                low = "#c43c39",
                mid = "#f7f7f7",
                high = "#2c7fb8",
                midpoint = 0
              ) +
              scale_y_discrete(expand = bubble_expand_all) +
              labs(
                title = "LF Bubble Residuals (all selected fisheries, years combined)",
                subtitle = paste0("Scenario: ", unique(as.character(bubble_data$Scenario))[1],
                                  " | Years: ", min(input$lf_years), " to ", max(input$lf_years)),
                x = "Length (cm)", y = "Fishery"
              ) +
              theme_bw(base_size = 12) +
              theme(
                legend.position = "right",
                plot.title = element_text(hjust = 0.5, face = "bold", size = 13.5),
                plot.subtitle = element_text(hjust = 0.5, size = 10),
                axis.text = element_text(size = 10, colour = "#222"),
                axis.text.y = element_text(size = 10, colour = "#111", face = "bold"),
                panel.grid.minor = element_blank()
              )
          } else {
            p <- ggplot(bubble_data, aes(x = length, y = fishery_name)) +
              geom_point(
                aes(size = abs_resid, fill = resid),
                shape = 21, color = "#1f1f1f", stroke = 0.18, alpha = 0.9
              ) +
              facet_wrap(~Scenario, ncol = ncol_facet) +
              scale_size_continuous(name = "|Pred - Obs|", range = c(0.8, 5.0), trans = "sqrt") +
              scale_fill_gradient2(
                name = "Pred - Obs",
                low = "#c43c39",
                mid = "#f7f7f7",
                high = "#2c7fb8",
                midpoint = 0
              ) +
              scale_y_discrete(expand = bubble_expand_all) +
              labs(
                title = "LF Bubble Residuals (all selected fisheries, years combined)",
                subtitle = paste0("Years: ", min(input$lf_years), " to ", max(input$lf_years)),
                x = "Length (cm)", y = "Fishery"
              ) +
              theme_bw(base_size = 12) +
              theme(
                legend.position = "right",
                plot.title = element_text(hjust = 0.5, face = "bold", size = 13.5),
                plot.subtitle = element_text(hjust = 0.5, size = 10),
                strip.text = element_text(size = 10, face = "bold", colour = "#111"),
                axis.text = element_text(size = 9.5, colour = "#222"),
                axis.text.y = element_text(size = 9.5, colour = "#111", face = "bold"),
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0.15, "lines")
              )
          }
          return(p)
        }

        bubble_data <- plot_data %>%
          group_by(Scenario, year, length) %>%
          summarise(
            obs = sum(obs, na.rm = TRUE),
            pred = sum(pred, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            resid = pred - obs,
            abs_resid = abs(resid),
            year_f = factor(year, levels = sort(unique(year)))
          ) %>%
          filter(abs_resid > 0)

        if (nrow(bubble_data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No residual differences to display", size = 6, color = "#999") +
            theme_void()
          return(p)
        }

        if (length(unique(bubble_data$Scenario)) <= 1) {
          p <- ggplot(bubble_data, aes(x = length, y = year_f)) +
            geom_point(
              aes(size = abs_resid, fill = resid),
              shape = 21, color = "#1f1f1f", stroke = 0.18, alpha = 0.9
            ) +
            scale_size_continuous(name = "|Pred - Obs|", range = c(0.8, 6.2), trans = "sqrt") +
            scale_fill_gradient2(
              name = "Pred - Obs",
              low = "#c43c39",
              mid = "#f7f7f7",
              high = "#2c7fb8",
              midpoint = 0
            ) +
            scale_y_discrete(expand = bubble_expand_year) +
            labs(
              title = paste(fishery_name, "- LF Bubble Residuals -", unique(as.character(bubble_data$Scenario))[1]),
              x = "Length (cm)", y = "Year"
            ) +
            theme_bw(base_size = 12) +
            theme(
              legend.position = "right",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 13.5),
              axis.text = element_text(size = 10, colour = "#222"),
              axis.text.y = element_text(size = 10, colour = "#111", face = "bold"),
              panel.grid.minor = element_blank()
            )
        } else {
          p <- ggplot(bubble_data, aes(x = length, y = year_f)) +
            geom_point(
              aes(size = abs_resid, fill = resid),
              shape = 21, color = "#1f1f1f", stroke = 0.15, alpha = 0.88
            ) +
            facet_wrap(~Scenario, ncol = ncol_facet) +
            scale_size_continuous(name = "|Pred - Obs|", range = c(0.8, 5.6), trans = "sqrt") +
            scale_fill_gradient2(
              name = "Pred - Obs",
              low = "#c43c39",
              mid = "#f7f7f7",
              high = "#2c7fb8",
              midpoint = 0
            ) +
            scale_y_discrete(expand = bubble_expand_year) +
            labs(
              title = paste(fishery_name, "- LF Bubble Residuals"),
              subtitle = paste0("Base: ", input$lf_model, " (", n_years, " years)"),
              x = "Length (cm)", y = "Year"
            ) +
            theme_bw(base_size = 12) +
            theme(
              legend.position = "right",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 13.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              strip.background = element_rect(fill = "grey90"),
              strip.text = element_text(size = 10, face = "bold", colour = "#111"),
              axis.text = element_text(size = 9.5, colour = "#222"),
              axis.text.y = element_text(size = 9.5, colour = "#111", face = "bold"),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0.15, "lines")
            )
        }

        return(p)
      }

      if (identical(view_mode, "all_fisheries")) {
        all_year_obs <- obs_data %>%
          group_by(fishery_name, length) %>%
          summarise(obs = sum(obs, na.rm = TRUE), .groups = "drop")

        all_year_pred <- plot_data %>%
          group_by(Scenario, fishery_name, length) %>%
          summarise(pred = sum(pred, na.rm = TRUE), .groups = "drop")

        p <- ggplot() +
          geom_col(
            data = all_year_obs,
            aes(x = length, y = obs, fill = "Observed"),
            width = lf_bar_width,
            position = "identity",
            colour = observed_border,
            linewidth = 0.12,
            alpha = 0.95
          ) +
          geom_line(
            data = all_year_pred,
            aes(x = length, y = pred, color = Scenario),
            linewidth = 1.2
          ) +
          facet_wrap(~fishery_name, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill), guide = "none") +
          scale_color_viridis_d(name = "Model") +
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
            strip.text = element_text(size = 10.5, face = "bold", colour = "#111"),
            axis.text = element_text(size = 9.5, colour = "#222"),
            axis.text.y = element_text(size = 9.5, colour = "#111")
          )
      } else if (length(unique(plot_data$Scenario)) <= 1) {
        p <- ggplot() +
          geom_col(
            data = obs_data,
            aes(x = length, y = obs, fill = "Observed"),
            width = lf_bar_width,
            position = "identity",
            colour = observed_border,
            linewidth = 0.12,
            alpha = 0.95
          ) +
          geom_line(
            data = plot_data,
            aes(x = length, y = pred, color = "Predicted"),
            linewidth = 1.2
          ) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill), guide = "none") +
          scale_color_manual(name = "Model", values = c("Predicted" = "#E31A1C")) +
          labs(
            title = paste(fishery_name, "-", unique(as.character(plot_data$Scenario))[1]),
            x = "Length (cm)", y = "Sample count"
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = strip_size + 0.8, face = "bold", colour = "#111"),
            axis.text = element_text(size = 9.5, colour = "#222"),
            axis.text.y = element_text(size = 9.5, colour = "#111"),
            panel.spacing = unit(0.3, "lines")
          )
      } else {
        p <- ggplot() +
          geom_col(data = obs_data,
                   aes(x = length, y = obs, fill = "Observed"),
                   width = lf_bar_width,
                   position = "identity",
                   colour = observed_border,
                   linewidth = 0.12,
                   alpha = 0.95) +
          geom_line(data = plot_data,
                    aes(x = length, y = pred, color = Scenario),
                    linewidth = 1.2) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill), guide = "none") +
          scale_color_viridis_d(name = "Model") +
          labs(title = paste(fishery_name, "- Base:", input$lf_model,
                             paste0("(", n_years, " years)")),
               x = "Length (cm)", y = "Sample count") +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = strip_size + 0.8, face = "bold", colour = "#111"),
            axis.text = element_text(size = 9.5, colour = "#222"),
            axis.text.y = element_text(size = 9.5, colour = "#111"),
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
      input$lf_years,
      input$lf_facet_ncol,
      input$lf_plot_style
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
    show_download_modal("lf", "Length Frequency Plot", current_save_dir = input$plot_export_dir)
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

  register_folder_save_button(
    plot_type = "lf",
    plot_reactive = lf_plot_reactive,
    input = input,
    session = session,
    output = output,
    filename_fun = function() {
      format <- input$lf_format
      paste0("length_freq_", input$lf_model, "_", input$lf_fishery, "_", Sys.Date(), ".", format)
    }
  )

}
