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
        numericInput("cpue_facet_ncol", "Facet columns:", value = 3, min = 1, max = 12, step = 1),

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
        plotOutput("cpue_plot", height = "650px")
      )
    )
  )
}

mod_cpue_server <- function(input, output, session, rv) {
  # TAB 5: CPUE FITS
  # ===========================================================================

  get_available_cpue_fisheries <- function(scenarios) {
    # Prefer index fisheries from fishery map; fallback to CPUE units in outputs.
    index_ids <- unique(unlist(rv$INDEX_FISHERIES_MAPS[scenarios]))
    index_ids <- index_ids[!is.na(index_ids)]

    if (length(index_ids) > 0) {
      return(as.character(sort(unique(as.numeric(index_ids)))))
    }

    units <- map(scenarios, function(sc) {
      rep_obj <- rv$RepOut_list[[sc]]
      if (is.null(rep_obj)) return(character(0))

      obs <- tryCatch(as.data.frame(cpue_obs(rep_obj)), error = function(e) NULL)
      if (is.null(obs) || nrow(obs) == 0 || !"unit" %in% names(obs)) return(character(0))

      as.character(sort(unique(obs$unit)))
    })

    sort(unique(unlist(units)))
  }

  build_cpue_df <- function(scenarios, fisheries) {
    map_dfr(scenarios, function(scenario) {
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
      cpue <- cpue[cpue$unit %in% as.numeric(fisheries), , drop = FALSE]
      if (nrow(cpue) == 0) return(NULL)

      cpue$Scenario <- scenario
      cpue$fishery_name <- sapply(
        as.character(cpue$unit),
        function(x) get_fishery_name(x, fishery_map)
      )

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

  cpue_plot_reactive <- reactive({
    req(rv$data_loaded, input$cpue_scenarios, input$cpue_fisheries)

    if (length(input$cpue_scenarios) == 0 || length(input$cpue_fisheries) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No models or fisheries selected", size = 6, color = "#999") +
          theme_void()
      )
    }

    tryCatch({
      cpue_all <- build_cpue_df(input$cpue_scenarios, input$cpue_fisheries)

      if (is.null(cpue_all) || nrow(cpue_all) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No CPUE data available", size = 6, color = "#999") +
            theme_void()
        )
      }

      cpue_all <- cpue_all %>%
        mutate(
          year_season = year + (season - 1) / 4,
          obs_log = obs,
          fit_log = fit,
          obs = exp(obs_log),
          fit = exp(fit_log),
          residual = obs - fit
        )

      fishery_levels <- ordered_fishery_label_levels(cpue_all$unit, cpue_all$fishery_name)
      if (length(fishery_levels) > 0) {
        cpue_all <- cpue_all %>% mutate(fishery_name = factor(fishery_name, levels = fishery_levels))
      }

      obs_points <- cpue_all %>%
        group_by(fishery_name, year_season) %>%
        summarise(obs = first(obs), .groups = "drop")

      scenario_colors <- get_scenario_colors(input$cpue_scenarios)
      view_mode <- if (is.null(input$cpue_view_mode)) "overlay" else input$cpue_view_mode
      metric <- if (is.null(input$cpue_metric)) "fits" else input$cpue_metric
      facet_ncol <- suppressWarnings(as.integer(input$cpue_facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
      facet_ncol <- min(max(facet_ncol, 1), 12)

      if (identical(metric, "residuals") && identical(view_mode, "by_scenario")) {
        p <- ggplot(cpue_all, aes(x = year_season, y = residual, color = Scenario)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
          geom_point(size = 1.2, alpha = 0.55) +
          facet_grid(Scenario ~ fishery_name, scales = "free_y") +
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
            title = paste("CPUE Residuals -", paste(input$cpue_scenarios, collapse = ", "))
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
        p <- ggplot(cpue_all, aes(x = year_season)) +
          geom_point(data = obs_points, aes(y = obs), size = 1.8, alpha = 0.65, color = "#E69F00") +
          geom_line(aes(y = fit, color = Scenario), linewidth = 1.1, alpha = 0.9) +
          facet_grid(Scenario ~ fishery_name, scales = "free_y") +
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
        geom_point(data = obs_points, aes(y = obs), size = 2, alpha = 0.6, color = "#E69F00") +
        geom_line(aes(y = fit, color = Scenario), linewidth = 1.2, alpha = 0.9) +
        facet_wrap(~fishery_name, scales = "free_y", ncol = facet_ncol) +
        scale_color_manual(values = scenario_colors) +
        labs(
          x = "Year + Season",
          y = "CPUE",
          title = paste("CPUE Fits -", paste(input$cpue_scenarios, collapse = ", "))
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
    rv$data_loaded,
    input$model_dir,
    input$cpue_scenarios,
    input$cpue_fisheries,
    input$cpue_view_mode,
    input$cpue_metric,
    input$cpue_facet_ncol
  )

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
