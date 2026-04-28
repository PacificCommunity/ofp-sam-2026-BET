mod_cpue_ui <- function() {
  tabItem(
    tabName = "cpue",
    h2("CPUE Fits", style = "color: #00c0ef;"),

    fluidRow(
      box(
        title = "Settings",
        width = 3,
        solidHeader = TRUE,
        status = "info",

        pickerInput(
          "cpue_scenarios",
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

        radioButtons(
          "cpue_view_mode",
          "Display:",
          choices = c(
            "Overlay all models" = "overlay",
            "By model" = "by_scenario"
          ),
          selected = "overlay"
        ),

        radioButtons(
          "cpue_metric",
          "Metric:",
          choices = c(
            "Observed vs Fitted" = "fits",
            "Residuals (obs - fit)" = "residuals"
          ),
          selected = "fits"
        ),

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
        conditionalPanel(
          condition = "input.cpue_view_mode == 'by_scenario'",
          checkboxInput(
            "cpue_free_y_panel",
            "Free y per panel (by model)",
            value = FALSE
          )
        ),
        selectInput("cpue_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "3"),
        sliderInput(
          "cpue_plot_height",
          "Plot height (px)",
          min = 450,
          max = 1800,
          value = 900,
          step = 50
        ),
        sliderInput(
          "cpue_plot_width",
          "Plot width (px)",
          min = 700,
          max = 2200,
          value = 1200,
          step = 50
        ),
        actionButton("cpue_apply_filters", "Apply", class = "btn-primary", style = "width: 100%;"),
        tags$small("Selections update the plot when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),

        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_cpue_download_modal", "📥 Download Plot...",
                     class = "btn-info",
                     style = "width: 100%;",
                     icon = icon("download")),
        helpText("Select models and fisheries to display", style = "margin-top: 10px;")
      ),

      box(
        title = "CPUE Observed vs Predicted",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "cpue_plot",
          uiOutput("cpue_plot_ui"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering CPUE plot...")
            )
          )
        )
      )
    )
  )
}

mod_cpue_server <- function(input, output, session, rv) {
  # TAB 5: CPUE FITS
  # ===========================================================================

  cpue_filters_current <- reactive({
    list(
      scenarios = input$cpue_scenarios,
      fisheries = input$cpue_fisheries,
      view_mode = if (is.null(input$cpue_view_mode)) "overlay" else input$cpue_view_mode,
      metric = if (is.null(input$cpue_metric)) "fits" else input$cpue_metric,
      free_y_panel = isTRUE(input$cpue_free_y_panel),
      facet_ncol = input$cpue_facet_ncol,
      plot_height = if (is.null(input$cpue_plot_height)) 900 else suppressWarnings(as.integer(input$cpue_plot_height)),
      plot_width = if (is.null(input$cpue_plot_width)) 1200 else suppressWarnings(as.integer(input$cpue_plot_width))
    )
  })
  cpue_filters_applied <- reactiveVal(NULL)
  cpue_last_initialized_nonce <- reactiveVal(0)
  cpue_filters <- reactive({
    cpue_filters_applied()
  })

  cpue_prepped_outputs <- reactive({
    req(rv$data_loaded)
    model_names <- names(rv$RepOut_list)
    setNames(lapply(model_names, function(scenario) {
      rep_obj <- rv$RepOut_list[[scenario]]
      fishery_map <- rv$FISHERY_MAPS[[scenario]]
      if (is.null(rep_obj)) return(NULL)

      obs <- tryCatch(as.data.frame(cpue_obs(rep_obj)), error = function(e) NULL)
      fit <- tryCatch(as.data.frame(cpue_pred(rep_obj)), error = function(e) NULL)
      if (is.null(obs) || is.null(fit) || nrow(obs) == 0 || nrow(fit) == 0) return(NULL)

      names(obs)[names(obs) == "data"] <- "obs"
      names(fit)[names(fit) == "data"] <- "fit"

      cpue <- merge(obs, fit)
      cpue <- type.convert(cpue, as.is = TRUE)

      cpue <- cpue %>%
        mutate(
          Scenario = scenario,
          unit = suppressWarnings(as.numeric(as.character(unit))),
          year = suppressWarnings(as.numeric(as.character(year))),
          season = suppressWarnings(as.numeric(as.character(season))),
          fishery_name = vapply(
            as.character(unit),
            function(x) get_fishery_name(x, fishery_map),
            character(1)
          ),
          year_season = year + (season - 1) / 4,
          obs_log = suppressWarnings(as.numeric(obs)),
          fit_log = suppressWarnings(as.numeric(fit)),
          obs = exp(obs_log),
          fit = exp(fit_log),
          residual = obs - fit
        ) %>%
        filter(is.finite(unit), is.finite(year_season), is.finite(obs), is.finite(fit))

      cpue
    }), model_names)
  })
  cpue_prepped_outputs <- bindCache(
    cpue_prepped_outputs,
    rv$data_loaded,
    input$model_dir,
    sort(names(rv$RepOut_list)),
    vapply(rv$RepOut_list, function(x) if (is.null(x)) 0 else as.numeric(object.size(x)), numeric(1))
  )

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(cpue_filters_current(), cpue_filters())
    set_apply_pending(session, "cpue_apply_filters", pending)
  })

  get_available_cpue_fisheries <- function(scenarios) {
    # Prefer index fisheries from fishery map; fallback to CPUE units in outputs.
    index_ids <- unique(unlist(rv$INDEX_FISHERIES_MAPS[scenarios]))
    index_ids <- index_ids[!is.na(index_ids)]

    if (length(index_ids) > 0) {
      return(as.character(sort(unique(as.numeric(index_ids)))))
    }

    units <- map(scenarios, function(sc) {
      df <- cpue_prepped_outputs()[[sc]]
      if (is.null(df) || nrow(df) == 0 || !"unit" %in% names(df)) return(character(0))
      as.character(sort(unique(df$unit)))
    })

    sort(unique(unlist(units)))
  }

  build_cpue_df <- function(scenarios, fisheries) {
    map_dfr(scenarios, function(scenario) {
      cpue <- cpue_prepped_outputs()[[scenario]]
      if (is.null(cpue)) return(NULL)
      cpue <- cpue[cpue$unit %in% as.numeric(fisheries), , drop = FALSE]
      if (nrow(cpue) == 0) return(NULL)
      cpue
    })
  }

  observeEvent(input$cpue_scenarios, {
    req(rv$data_loaded)

    if (length(input$cpue_scenarios) == 0) {
      updatePickerInput(session, "cpue_fisheries", choices = character(0), selected = character(0))
      return()
    }

    available_fisheries <- get_available_cpue_fisheries(input$cpue_scenarios)
    if (length(available_fisheries) == 0) {
      updatePickerInput(session, "cpue_fisheries", choices = character(0), selected = character(0))
      showNotification("No CPUE fisheries detected in selected models", type = "warning", duration = 3)
      return()
    }

    fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
    choices <- setNames(
      available_fisheries,
      sapply(available_fisheries, function(x) get_fishery_name(x, fishery_map))
    )

    current_selection <- isolate(input$cpue_fisheries)
    if (is.null(current_selection) || length(current_selection) == 0) {
      new_selection <- available_fisheries
    } else {
      new_selection <- intersect(current_selection, available_fisheries)
      if (length(new_selection) == 0) {
        new_selection <- available_fisheries
      }
    }

    updatePickerInput(
      session,
      "cpue_fisheries",
      choices = choices,
      selected = new_selection
    )
  }, ignoreInit = FALSE)

  observeEvent(input$cpue_apply_filters, {
    cpue_filters_applied(isolate(cpue_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(list(input$live_update_plots, input$cpue_scenarios, input$cpue_fisheries,
                    input$cpue_view_mode, input$cpue_metric, input$cpue_free_y_panel,
                    input$cpue_facet_ncol,
                    input$cpue_plot_height, input$cpue_plot_width), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$cpue_scenarios) == 0 || length(input$cpue_fisheries) == 0) return()
    cpue_filters_applied(isolate(cpue_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(list(rv$initial_render_nonce, input$cpue_scenarios, input$cpue_fisheries), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= cpue_last_initialized_nonce()) return()

    ready <- length(input$cpue_scenarios) > 0 && length(input$cpue_fisheries) > 0
    if (!ready) return()

    cpue_last_initialized_nonce(rv$initial_render_nonce)
    cpue_filters_applied(isolate(cpue_filters_current()))
  }, ignoreInit = TRUE)

  cpue_plot_reactive <- reactive({
    filters <- cpue_filters()
    req(rv$data_loaded, filters, filters$scenarios, filters$fisheries)

    if (length(filters$scenarios) == 0 || length(filters$fisheries) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No models or fisheries selected", size = 6, color = "#999") +
          theme_void()
      )
    }

    tryCatch({
      cpue_all <- build_cpue_df(filters$scenarios, filters$fisheries)

      if (is.null(cpue_all) || nrow(cpue_all) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No CPUE data available", size = 6, color = "#999") +
            theme_void()
        )
      }

      fishery_levels <- ordered_fishery_label_levels(cpue_all$unit, cpue_all$fishery_name)
      if (length(fishery_levels) > 0) {
        cpue_all <- cpue_all %>% mutate(fishery_name = factor(fishery_name, levels = fishery_levels))
      }

      obs_points <- cpue_all %>%
        group_by(fishery_name, year_season) %>%
        summarise(obs = first(obs), .groups = "drop")

      scenario_colors <- get_scenario_colors(filters$scenarios)
      view_mode <- filters$view_mode
      metric <- filters$metric
      free_y_panel <- isTRUE(filters$free_y_panel)
      facet_ncol <- suppressWarnings(as.integer(filters$facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
      facet_ncol <- min(max(facet_ncol, 1), 12)

      by_model_panel_spec <- function(data) {
        fishery_vals <- if (length(fishery_levels) > 0) {
          as.character(fishery_levels)
        } else {
          sort(unique(as.character(data$fishery_name)))
        }
        scenario_vals <- filters$scenarios

        panel_grid <- expand.grid(
          fishery_name = fishery_vals,
          Scenario = scenario_vals,
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
        panel_levels <- paste(panel_grid$Scenario, panel_grid$fishery_name, sep = " | ")
        panel_labels <- setNames(
          paste(panel_grid$fishery_name, panel_grid$Scenario, sep = "\n"),
          panel_levels
        )

        list(
          fishery_vals = fishery_vals,
          scenario_vals = scenario_vals,
          panel_levels = panel_levels,
          panel_labels = panel_labels,
          ncol = max(1, length(fishery_vals))
        )
      }

      format_by_model_panel_data <- function(data, panel_spec) {
        data %>%
          mutate(
            Scenario = factor(as.character(Scenario), levels = panel_spec$scenario_vals),
            fishery_name = factor(as.character(fishery_name), levels = panel_spec$fishery_vals)
          )
      }

      add_by_model_panel <- function(data, panel_spec) {
        data %>%
          mutate(
            Scenario = factor(as.character(Scenario), levels = panel_spec$scenario_vals),
            fishery_name = factor(as.character(fishery_name), levels = panel_spec$fishery_vals),
            scenario_fishery = paste(as.character(Scenario), as.character(fishery_name), sep = " | "),
            scenario_fishery = factor(scenario_fishery, levels = panel_spec$panel_levels)
          )
      }

      if (identical(metric, "residuals") && identical(view_mode, "by_scenario")) {
        plot_df <- cpue_all
        panel_spec <- NULL
        facet_formula <- Scenario ~ fishery_name
        if (isTRUE(free_y_panel)) {
          panel_spec <- by_model_panel_spec(plot_df)
          plot_df <- format_by_model_panel_data(plot_df, panel_spec)

          if (requireNamespace("patchwork", quietly = TRUE)) {
            scenario_plots <- lapply(seq_along(panel_spec$scenario_vals), function(i) {
              scenario <- panel_spec$scenario_vals[[i]]
              row_df <- plot_df %>%
                filter(as.character(Scenario) == scenario) %>%
                droplevels()
              row_ncol <- max(1, length(levels(row_df$fishery_name)))

              row_plot <- ggplot(row_df, aes(x = year_season, y = residual, color = Scenario)) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
                geom_point(size = 1.2, alpha = 0.55) +
                facet_wrap(~fishery_name, scales = "free_y", ncol = row_ncol) +
                scale_color_manual(values = scenario_colors) +
                labs(
                  x = if (i == length(panel_spec$scenario_vals)) "Year + Season" else NULL,
                  y = "Residual (obs - fit)",
                  title = scenario
                ) +
                theme_bw(base_size = 12) +
                theme(
                  legend.position = "none",
                  plot.title = element_text(hjust = 0, face = "bold", size = 12),
                  strip.background = element_rect(fill = "grey90"),
                  strip.text = element_text(face = "bold"),
                  panel.grid.minor = element_blank()
                )

              if (i < length(panel_spec$scenario_vals)) {
                row_plot <- row_plot +
                  theme(
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                  )
              }

              row_plot
            })

            return(
              patchwork::wrap_plots(scenario_plots, ncol = 1) +
                patchwork::plot_annotation(
                  title = "CPUE Residuals by Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
                )
            )
          }

          plot_df <- add_by_model_panel(plot_df, panel_spec)
          facet_formula <- ~ scenario_fishery
        }

        p <- ggplot(plot_df, aes(x = year_season, y = residual, color = Scenario)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
          geom_point(size = 1.2, alpha = 0.55)

        if (isTRUE(free_y_panel)) {
          p <- p + facet_wrap(
            facet_formula,
            scales = "free_y",
            ncol = panel_spec$ncol,
            labeller = as_labeller(panel_spec$panel_labels)
          )
        } else {
          p <- p + facet_grid(facet_formula, scales = "free")
        }

        p <- p +
          scale_color_manual(values = scenario_colors) +
          labs(x = "Year + Season", y = "Residual (obs - fit)", title = "CPUE Residuals by Model") +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank()
          )

        return(p)
      }

      if (identical(metric, "residuals")) {
      p <- ggplot(cpue_all, aes(x = year_season, y = residual, color = Scenario)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
        geom_point(size = 1.6, alpha = 0.55) +
        facet_wrap(~fishery_name, scales = "free_y", ncol = facet_ncol) +
          scale_color_manual(values = scenario_colors) +
          labs(
            x = "Year + Season",
            y = "Residual (obs - fit)",
            title = paste("CPUE Residuals -", paste(filters$scenarios, collapse = ", "))
          ) +
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
      }

      if (identical(view_mode, "by_scenario")) {
        plot_df <- cpue_all
        obs_df <- cpue_all %>%
          group_by(Scenario, fishery_name, year_season) %>%
          summarise(obs = first(obs), .groups = "drop")
        panel_spec <- NULL
        facet_formula <- Scenario ~ fishery_name
        if (isTRUE(free_y_panel)) {
          panel_spec <- by_model_panel_spec(plot_df)
          plot_df <- format_by_model_panel_data(plot_df, panel_spec)
          obs_df <- format_by_model_panel_data(obs_df, panel_spec)

          if (requireNamespace("patchwork", quietly = TRUE)) {
            scenario_plots <- lapply(seq_along(panel_spec$scenario_vals), function(i) {
              scenario <- panel_spec$scenario_vals[[i]]
              row_df <- plot_df %>%
                filter(as.character(Scenario) == scenario) %>%
                droplevels()
              row_obs <- obs_df %>%
                filter(as.character(Scenario) == scenario) %>%
                droplevels()
              row_ncol <- max(1, length(levels(row_df$fishery_name)))

              row_plot <- ggplot(row_df, aes(x = year_season)) +
                geom_point(data = row_obs, aes(y = obs), size = 1.8, alpha = 0.5, color = "#6b7280") +
                geom_line(aes(y = fit, color = Scenario), linewidth = 1.1, alpha = 0.9) +
                facet_wrap(~fishery_name, scales = "free_y", ncol = row_ncol) +
                scale_color_manual(values = scenario_colors) +
                labs(
                  x = if (i == length(panel_spec$scenario_vals)) "Year + Season" else NULL,
                  y = "CPUE",
                  title = scenario
                ) +
                theme_bw(base_size = 12) +
                theme(
                  legend.position = "none",
                  plot.title = element_text(hjust = 0, face = "bold", size = 12),
                  strip.background = element_rect(fill = "grey90"),
                  strip.text = element_text(face = "bold"),
                  panel.grid.minor = element_blank()
                )

              if (i < length(panel_spec$scenario_vals)) {
                row_plot <- row_plot +
                  theme(
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                  )
              }

              row_plot
            })

            return(
              patchwork::wrap_plots(scenario_plots, ncol = 1) +
                patchwork::plot_annotation(
                  title = "CPUE Fits by Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))
                )
            )
          }

          plot_df <- add_by_model_panel(plot_df, panel_spec)
          obs_df <- add_by_model_panel(obs_df, panel_spec)
          facet_formula <- ~ scenario_fishery
        }

        p <- ggplot(plot_df, aes(x = year_season)) +
          geom_point(data = obs_df, aes(y = obs), size = 1.8, alpha = 0.5, color = "#6b7280") +
          geom_line(aes(y = fit, color = Scenario), linewidth = 1.1, alpha = 0.9)

        if (isTRUE(free_y_panel)) {
          p <- p + facet_wrap(
            facet_formula,
            scales = "free_y",
            ncol = panel_spec$ncol,
            labeller = as_labeller(panel_spec$panel_labels)
          )
        } else {
          p <- p + facet_grid(facet_formula, scales = "free")
        }

        p <- p +
          scale_color_manual(values = scenario_colors) +
          labs(x = "Year + Season", y = "CPUE", title = "CPUE Fits by Model") +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank()
          )

        return(p)
      }

      p <- ggplot(cpue_all, aes(x = year_season)) +
        geom_point(data = obs_points, aes(y = obs), size = 2, alpha = 0.5, color = "#6b7280") +
        geom_line(aes(y = fit, color = Scenario), linewidth = 1.2, alpha = 0.9) +
        facet_wrap(~fishery_name, scales = "free_y", ncol = facet_ncol) +
        scale_color_manual(values = scenario_colors) +
        labs(
          x = "Year + Season",
          y = "CPUE",
          title = paste("CPUE Fits -", paste(filters$scenarios, collapse = ", "))
        ) +
        theme_bw(base_size = 13) +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          strip.background = element_rect(fill = "grey90"),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )

      p
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5, color = "red") +
        theme_void()
    })
  })
  cpue_plot_reactive <- bindCache(
    cpue_plot_reactive,
    cpue_filters()
  )

  output$cpue_plot_ui <- renderUI({
    filters <- cpue_filters()
    h <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_height)) else suppressWarnings(as.integer(input$cpue_plot_height))
    w <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_width)) else suppressWarnings(as.integer(input$cpue_plot_width))
    if (!is.finite(h)) h <- 900
    if (!is.finite(w)) w <- 1200
    h <- min(max(h, 450), 1800)
    w <- min(max(w, 700), 2200)

    plotOutput("cpue_plot", height = paste0(h, "px"), width = paste0(w, "px"))
  })

  output$cpue_plot <- renderPlot({
    cpue_plot_reactive()
  })

  # ===========================================================================

  # CPUE DOWNLOAD
  # ---------------------------------------------------------------------------

  observeEvent(input$show_cpue_download_modal, {
    show_download_modal("cpue", "CPUE Fits Plot", current_save_dir = input$plot_export_dir)
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

  register_folder_save_button(
    plot_type = "cpue",
    plot_reactive = cpue_plot_reactive,
    input = input,
    session = session,
    output = output,
    filename_fun = function() {
      format <- input$cpue_format
      paste0("cpue_fits_", Sys.Date(), ".", format)
    }
  )
}
