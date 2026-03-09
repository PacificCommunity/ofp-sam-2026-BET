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
            selectInput(
              "lf_plot_scale",
              "Plot size:",
              choices = c(
                "120%" = "1.20",
                "110%" = "1.10",
                "100%" = "1.00",
                "95%" = "0.95",
                "90%" = "0.90",
                "85%" = "0.85",
                "80%" = "0.80",
                "75%" = "0.75",
                "70%" = "0.70",
                "65%" = "0.65",
                "60%" = "0.60",
                "55%" = "0.55",
                "50%" = "0.50"
              ),
              selected = "1.00"
            ),
            selectInput("lf_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "3"),
            actionButton("lf_apply_filters", "Apply", class = "btn-primary", style = "width: 100%;"),
            tags$small("Selections update the plot when you click Apply.",
                       style = "display:block; margin-top:6px; color:#666;"),
            
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
          box(
            title = "Length Frequency",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            div(
              class = "plot-loading-container",
              `data-output-id` = "lf_plot_box",
              uiOutput("lf_plot_box"),
              div(
                class = "plot-loading-overlay",
                div(
                  class = "plot-loading-card",
                  HTML("<span class='render-spinner'></span>Rendering length frequency plot...")
                )
              )
            )
          )
        )
      )
}

mod_lf_server <- function(input, output, session, rv) {
    # TAB 6: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
    # ===========================================================================
    lf_filters_current <- reactive({
      list(
        model = input$lf_model,
        view_mode = if (is.null(input$lf_view_mode)) "all_fisheries" else input$lf_view_mode,
        fishery = input$lf_fishery,
        fisheries_all = input$lf_fisheries_all,
        scenarios = input$lf_scenarios,
        years = input$lf_years,
        plot_scale = if (is.null(input$lf_plot_scale)) "1.00" else input$lf_plot_scale,
        facet_ncol = input$lf_facet_ncol,
        plot_style = if (is.null(input$lf_plot_style)) "hist" else input$lf_plot_style
      )
    })
    lf_filters_applied <- reactiveVal(NULL)
    lf_last_initialized_nonce <- reactiveVal(0)
    lf_filters <- reactive({
      lf_filters_applied()
    })
    lf_prepped_outputs <- reactive({
      req(rv$data_loaded)
      model_names <- names(rv$LengOut_list)
      setNames(lapply(model_names, function(sc) {
        obj <- rv$LengOut_list[[sc]]
        if (is.null(obj)) return(NULL)
        df <- obj@lenfits
        if (is.null(df) || nrow(df) == 0) return(NULL)
        df <- df %>%
          mutate(
            fishery = suppressWarnings(as.numeric(as.character(fishery))),
            year = suppressWarnings(as.numeric(as.character(year))),
            length = suppressWarnings(as.numeric(as.character(length))),
            obs_count = if ("sample_size" %in% names(df)) obs * sample_size else obs,
            pred_count = if ("sample_size" %in% names(df)) pred * sample_size else pred
          ) %>%
          filter(is.finite(fishery), is.finite(year), is.finite(length)) %>%
          group_by(fishery, year, length) %>%
          summarise(
            obs = sum(obs_count, na.rm = TRUE),
            pred = sum(pred_count, na.rm = TRUE),
            .groups = "drop"
          )
        fish_ids <- sort(unique(df$fishery))
        fish_ids <- fish_ids[is.finite(fish_ids)]
        fish_lookup <- data.frame(
          fishery = fish_ids,
          fishery_name_lookup = vapply(
            fish_ids,
            function(f) get_fishery_name(f, rv$FISHERY_MAPS[[sc]]),
            character(1)
          ),
          stringsAsFactors = FALSE
        )
        df %>%
          left_join(fish_lookup, by = "fishery") %>%
          mutate(Scenario = sc)
      }), model_names)
    })
    lf_prepped_outputs <- bindCache(
      lf_prepped_outputs,
      rv$data_loaded,
      input$model_dir,
      sort(names(rv$LengOut_list)),
      vapply(rv$LengOut_list, function(x) if (is.null(x)) 0L else nrow(x@lenfits), integer(1))
    )

    observe({
      req(rv$data_loaded)
      pending <- !isTRUE(input$live_update_plots) &&
        !filters_equal(lf_filters_current(), lf_filters())
      set_apply_pending(session, "lf_apply_filters", pending)
    })

    observeEvent(input$lf_apply_filters, {
      lf_filters_applied(isolate(lf_filters_current()))
    }, ignoreInit = TRUE)

    observeEvent(list(input$live_update_plots, input$lf_scenarios, input$lf_model, input$lf_years,
                      input$lf_view_mode, input$lf_fishery, input$lf_fisheries_all,
                      input$lf_plot_style, input$lf_plot_scale, input$lf_facet_ncol), {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()

      ready <- !is.null(input$lf_model) &&
        nzchar(input$lf_model) &&
        length(input$lf_scenarios) > 0 &&
        length(input$lf_years) > 0 &&
        (
          (identical(input$lf_view_mode, "all_fisheries") && length(input$lf_fisheries_all) > 0) ||
          (!identical(input$lf_view_mode, "all_fisheries") && !is.null(input$lf_fishery) && nzchar(input$lf_fishery))
        )

      if (!ready) return()
      lf_filters_applied(isolate(lf_filters_current()))
    }, ignoreInit = TRUE)

    observeEvent(list(rv$initial_render_nonce, input$lf_model, input$lf_years, input$lf_scenarios, input$lf_fishery, input$lf_fisheries_all), {
      req(rv$data_loaded, rv$initial_render_nonce)
      if (rv$initial_render_nonce <= lf_last_initialized_nonce()) return()

      ready <- !is.null(input$lf_model) &&
        nzchar(input$lf_model) &&
        length(input$lf_scenarios) > 0 &&
        length(input$lf_years) > 0 &&
        (
          (identical(input$lf_view_mode, "all_fisheries") && length(input$lf_fisheries_all) > 0) ||
          (!identical(input$lf_view_mode, "all_fisheries") && !is.null(input$lf_fishery) && nzchar(input$lf_fishery))
        )

      if (!ready) return()

      lf_last_initialized_nonce(rv$initial_render_nonce)
      lf_filters_applied(isolate(lf_filters_current()))
    }, ignoreInit = TRUE)
  
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
      fisheries <- sort(unique(suppressWarnings(as.numeric(rv$LengOut_list[[input$lf_model]]@lenfits$fishery))))
    
      if (length(fisheries) == 0) {
        updateSelectInput(session, "lf_fishery", choices = character(0))
        return()
      }
    
      fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
      choices_single <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    
      # Preserve current selection if valid
      current_selection <- isolate(input$lf_fishery)
      if (!is.null(current_selection) && current_selection %in% fisheries) {
        selected <- current_selection
      } else {
        selected <- fisheries[1]
      }
    
      updateSelectInput(session, "lf_fishery", choices = choices_single, selected = selected)
    
      # Keep model list aligned with global Filter Models and available LF outputs.
      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      global_models <- isolate(input$scenarios)
      if (!is.null(global_models) && length(global_models) > 0) {
        all_models <- intersect(global_models, all_models)
      }

      current_scenarios <- isolate(input$lf_scenarios)
      if (is.null(current_scenarios) || length(current_scenarios) == 0) {
        selected_scenarios <- all_models
      } else {
        selected_scenarios <- intersect(current_scenarios, all_models)
        if (length(selected_scenarios) == 0) selected_scenarios <- all_models
      }
      fishery_union <- sort(unique(unlist(lapply(selected_scenarios, function(m) {
        df <- lf_prepped_outputs()[[m]]
        if (is.null(df)) return(numeric(0))
        unique(df$fishery)
      }))))
      fishery_union <- fishery_union[is.finite(fishery_union)]
      choices_all <- build_fishery_picker_choices(fishery_union, selected_scenarios, rv$FISHERY_MAPS)
      current_all <- isolate(input$lf_fisheries_all)
      if (is.null(current_all) || length(current_all) == 0) current_all <- unname(choices_all)
      current_all <- intersect(current_all, unname(choices_all))
      if (length(current_all) == 0) current_all <- unname(choices_all)
      updatePickerInput(session, "lf_fisheries_all", choices = choices_all, selected = current_all)
      updatePickerInput(session, "lf_scenarios",
                        choices = all_models,
                        selected = selected_scenarios)
    })
  
    # Update year choices when fishery or scenarios change
    observeEvent(list(input$lf_fishery, input$lf_fisheries_all, input$lf_model, input$lf_view_mode), {
      req(rv$data_loaded, input$lf_model)
    
      # Extract years for selected fishery from base model
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      df <- rv$LengOut_list[[input$lf_model]]@lenfits
      df <- lf_prepped_outputs()[[input$lf_model]]
      if (identical(input$lf_view_mode, "all_fisheries")) {
        selected_specs <- parse_fishery_picker_values(input$lf_fisheries_all)
        selected_fisheries <- unique(selected_specs$fishery)
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
      df <- lf_prepped_outputs()[[input$lf_model]]
      fisheries <- sort(unique(df$fishery))
      fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
      choices <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
      selected_fishery <- if (length(fisheries) > 0) fisheries[1] else NULL

      updateSelectInput(session, "lf_fishery", choices = choices, selected = selected_fishery)

      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      global_models <- isolate(input$scenarios)
      if (!is.null(global_models) && length(global_models) > 0) {
        all_models <- intersect(global_models, all_models)
      }
      current_scenarios <- isolate(input$lf_scenarios)
      if (is.null(current_scenarios) || length(current_scenarios) == 0) {
        selected_scenarios <- all_models
      } else {
        selected_scenarios <- intersect(current_scenarios, all_models)
        if (length(selected_scenarios) == 0) selected_scenarios <- all_models
      }
      fishery_union <- sort(unique(unlist(lapply(selected_scenarios, function(m) {
        dfm <- lf_prepped_outputs()[[m]]
        if (is.null(dfm)) return(numeric(0))
        unique(dfm$fishery)
      }))))
      fishery_union <- fishery_union[is.finite(fishery_union)]
      choices_all <- build_fishery_picker_choices(fishery_union, selected_scenarios, rv$FISHERY_MAPS)
      updatePickerInput(session, "lf_fisheries_all", choices = choices_all, selected = unname(choices_all))
      updatePickerInput(session, "lf_scenarios", choices = all_models, selected = selected_scenarios)

      years <- sort(unique(df$year))
      updatePickerInput(session, "lf_years", choices = years, selected = years)
      if (is.null(lf_filters_applied())) {
        lf_filters_applied(isolate(lf_filters_current()))
      }
    }, ignoreInit = TRUE)
  
    # Reactive: calculate dynamic plot height for LF
    lf_plot_height <- reactive({
      filters <- lf_filters()
      req(rv$data_loaded, filters, filters$years)
      facet_ncol <- suppressWarnings(as.integer(filters$facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
      facet_ncol <- min(max(facet_ncol, 1), 12)
      plot_style <- filters$plot_style

      if (identical(filters$view_mode, "all_fisheries")) {
        n_fisheries <- length(filters$fisheries_all)
        n_rows <- ceiling(max(n_fisheries, 1) / facet_ncol)
        if (identical(plot_style, "bubble")) {
          n_scen <- max(length(filters$scenarios), 1)
          n_panel_rows <- ceiling(n_scen / facet_ncol)
          panel_height <- min(max(180 + max(n_fisheries, 1) * 16, 280), 760)
          return(min(max(120 + n_panel_rows * panel_height, 420), 2600))
        }
        return(min(max(350 + n_rows * 240, 550), 3200))
      }

      n_years <- length(filters$years)
    
      if (n_years == 0) return(400)

        if (identical(plot_style, "bubble")) {
          n_scen <- max(length(filters$scenarios), 1)
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
      filters <- lf_filters()
      req(rv$data_loaded, filters, filters$model, filters$fishery, filters$years)

      view_mode <- filters$view_mode
      plot_style <- filters$plot_style
      scenarios_to_use <- filters$scenarios

      # Check if any scenarios selected
      if (length(scenarios_to_use) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No models selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      # Check if any years selected
      if (length(filters$years) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No years selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }

      selected_years_num <- suppressWarnings(as.numeric(filters$years))
      selected_years_num <- selected_years_num[is.finite(selected_years_num)]
    
      # Combine data from selected scenarios
      combined_data <- map_dfr(scenarios_to_use, function(sc) {
        df <- lf_prepped_outputs()[[sc]]
        if (is.null(df)) return(NULL)

        if (identical(filters$view_mode, "all_fisheries")) {
          selected_specs <- parse_fishery_picker_values(filters$fisheries_all)
          if (nrow(selected_specs) > 0) {
            any_model_specific <- any(!is.na(selected_specs$Model) & nzchar(selected_specs$Model))
            if (any_model_specific) {
              plain_ids <- selected_specs$fishery[is.na(selected_specs$Model) | !nzchar(selected_specs$Model)]
              model_ids <- selected_specs$fishery[!is.na(selected_specs$Model) & nzchar(selected_specs$Model) & selected_specs$Model == sc]
              keep_ids <- unique(c(plain_ids, model_ids))
              df <- df %>% filter(fishery %in% keep_ids)
            } else {
              df <- df %>% filter(fishery %in% selected_specs$fishery)
            }
          } else {
            df <- df[0, , drop = FALSE]
          }
        } else {
          df <- df %>% filter(fishery == as.numeric(filters$fishery))
        }
        if (length(selected_years_num) > 0) {
          df <- df %>% filter(year %in% selected_years_num)
        }
        if (nrow(df) == 0) return(NULL)
        df
      })
    
      # Check if data exists
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "#999") +
          theme_void()
        return(p)
      }

      # Already pre-aggregated to sample-count scale in lf_prepped_outputs().
      plot_data <- combined_data %>%
        filter(obs > 0 | pred > 0)
    
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
    
      base_fishery_map <- rv$FISHERY_MAPS[[filters$model]]
      fishery_name <- get_fishery_name(filters$fishery, base_fishery_map)
      obs_data <- obs_data %>%
        mutate(fishery_name = sapply(fishery, function(f) get_fishery_name(f, base_fishery_map)))
      if ("fishery_name_lookup" %in% names(plot_data)) {
        plot_data <- plot_data %>% mutate(fishery_name = as.character(fishery_name_lookup))
      } else {
        plot_data <- plot_data %>% mutate(fishery_name = as.character(fishery))
      }

      if (identical(view_mode, "all_fisheries")) {
        plot_data <- build_overlay_fishery_panel_labels(
          plot_data,
          scenario_col = "Scenario",
          id_col = "fishery",
          label_col = "fishery_name",
          out_col = "fishery_panel"
        )
      } else {
        plot_data <- plot_data %>% mutate(fishery_panel = fishery_name)
      }

      # For all-fisheries overlay, keep observed bars aligned with the displayed label
      # (including model-specific fallback/default names).
      obs_data_all_labels <- plot_data %>%
        group_by(fishery, fishery_panel, year, length) %>%
        summarise(obs = first(obs), .groups = "drop")

      fishery_levels <- ordered_fishery_label_levels(
        ids = c(plot_data$fishery, obs_data$fishery, obs_data_all_labels$fishery),
        labels = c(plot_data$fishery_panel, obs_data$fishery_name, obs_data_all_labels$fishery_panel)
      )
      if (length(fishery_levels) > 0) {
        plot_data <- plot_data %>% mutate(fishery_name = factor(fishery_name, levels = fishery_levels))
        plot_data <- plot_data %>% mutate(fishery_panel = factor(as.character(fishery_panel), levels = fishery_levels))
        obs_data <- obs_data %>% mutate(fishery_name = factor(fishery_name, levels = fishery_levels))
        obs_data_all_labels <- obs_data_all_labels %>% mutate(fishery_panel = factor(as.character(fishery_panel), levels = fishery_levels))
      }
    
      # Determine optimal layout
      n_years <- length(unique(plot_data$year))
      ncol_facet <- suppressWarnings(as.integer(filters$facet_ncol))
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
            group_by(Scenario, fishery_panel, length) %>%
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
            p <- ggplot(bubble_data, aes(x = length, y = fishery_panel)) +
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
                                  " | Years: ", min(filters$years), " to ", max(filters$years)),
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
            p <- ggplot(bubble_data, aes(x = length, y = fishery_panel)) +
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
                subtitle = paste0("Years: ", min(filters$years), " to ", max(filters$years)),
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
              subtitle = paste0("Base: ", filters$model, " (", n_years, " years)"),
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
        all_year_obs <- obs_data_all_labels %>%
          group_by(fishery_panel, year, length) %>%
          summarise(obs = first(obs), .groups = "drop") %>%
          group_by(fishery_panel, length) %>%
          summarise(obs = sum(obs, na.rm = TRUE), .groups = "drop")

        all_year_pred <- plot_data %>%
          group_by(Scenario, fishery_panel, length) %>%
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
          facet_wrap(~fishery_panel, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill), guide = "none") +
          scale_color_viridis_d(name = "Model") +
          labs(
            title = "All selected fisheries - all selected years combined",
            subtitle = paste0("Years: ", min(filters$years), " to ", max(filters$years)),
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
        single_model_label <- unique(as.character(plot_data$Scenario))[1]
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
            aes(x = length, y = pred, color = single_model_label),
            linewidth = 1.2
          ) +
          facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
          scale_fill_manual(values = c("Observed" = observed_fill), guide = "none") +
          scale_color_manual(name = "Model", values = setNames("#E31A1C", single_model_label)) +
          labs(
            title = paste(fishery_name, "-", single_model_label),
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
          labs(title = paste(fishery_name, "- Base:", filters$model,
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
      lf_filters()
    )

    # Render length frequency plot
    output$lf_plot <- renderPlot({
      lf_plot_reactive()
    })
  
    # Render dynamic box for LF with calculated height
    output$lf_plot_box <- renderUI({
      height <- lf_plot_height()
      filters <- lf_filters()
      scale_val <- suppressWarnings(as.numeric(filters$plot_scale))
      if (!is.finite(scale_val) || scale_val <= 0) scale_val <- 1
      scaled_height <- max(round(height * scale_val), 320)
      scaled_width_pct <- max(min(round(scale_val * 100), 100), 60)
    
      div(
        style = paste0("width:", scaled_width_pct, "%; margin: 0 auto;"),
        plotOutput("lf_plot", height = paste0(scaled_height, "px"))
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
      filters <- lf_filters()
      paste0("length_freq_", filters$model, "_", filters$fishery, "_",
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
        filters <- lf_filters()
        paste0("length_freq_", filters$model, "_", filters$fishery, "_", Sys.Date(), ".", format)
      }
    )

}
