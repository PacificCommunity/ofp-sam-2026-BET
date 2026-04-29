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
            "Residuals (obs - fit)" = "residuals",
            "CPUE regional share vs biomass share" = "relative_scale"
          ),
          selected = "fits"
        ),

        conditionalPanel(
          condition = "input.cpue_metric == 'relative_scale'",
          selectInput(
            "cpue_relative_cpue_series",
            "CPUE series:",
            choices = c(
              "Observed CPUE" = "obs",
              "Fitted CPUE" = "fit"
            ),
            selected = "fit"
          ),
          selectInput(
            "cpue_relative_biomass_basis",
            "Biomass basis:",
            choices = c(
              "Adult biomass by region" = "adult",
              "Total biomass by region" = "total",
              "Vulnerable biomass (matching fishery)" = "vuln"
            ),
            selected = "total"
          )
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
      relative_cpue_series = if (is.null(input$cpue_relative_cpue_series)) "fit" else input$cpue_relative_cpue_series,
      relative_biomass_basis = if (is.null(input$cpue_relative_biomass_basis)) "total" else input$cpue_relative_biomass_basis,
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

  get_cpue_region <- function(fishery_num, mapping = NULL) {
    key <- as.character(fishery_num)
    if (is.data.frame(mapping) && all(c("fishery", "region") %in% names(mapping))) {
      idx <- which(as.character(mapping$fishery) == key)
      if (length(idx) > 0) {
        region <- mapping$region[[idx[1]]]
        if (!is.null(region) && length(region) > 0 && !is.na(region)) {
          return(as.character(region))
        }
      }
    }
    NA_character_
  }

  extract_biomass_by_region <- function(rep_obj, fishery_map, fisheries, basis = "vuln") {
    if (is.null(rep_obj)) return(data.frame())

    fisheries_num <- suppressWarnings(as.numeric(fisheries))
    fisheries_num <- fisheries_num[is.finite(fisheries_num)]

    fishery_regions <- data.frame(
      unit = fisheries_num,
      region = vapply(fisheries_num, get_cpue_region, character(1), mapping = fishery_map),
      stringsAsFactors = FALSE
    )
    fishery_regions <- fishery_regions[!is.na(fishery_regions$region) & nzchar(fishery_regions$region), , drop = FALSE]
    region_values <- sort(unique(fishery_regions$region))
    if (length(region_values) == 0) return(data.frame())

    if (identical(basis, "vuln")) {
      biomass <- tryCatch(safe_array_to_df(slot(rep_obj, "vulnBiomass")), error = function(e) NULL)
      if (is.null(biomass) || nrow(biomass) == 0 || !"data" %in% names(biomass)) return(data.frame())

      biomass %>%
        mutate(
          unit_num = suppressWarnings(as.numeric(as.character(unit))),
          year = suppressWarnings(as.numeric(as.character(year))),
          season = suppressWarnings(as.numeric(as.character(season))),
          biomass = suppressWarnings(as.numeric(data))
        ) %>%
        filter(unit_num %in% fisheries_num, is.finite(year), is.finite(season), is.finite(biomass), biomass > 0) %>%
        left_join(fishery_regions, by = c("unit_num" = "unit")) %>%
        filter(!is.na(region), nzchar(region)) %>%
        group_by(year, season, region) %>%
        summarise(biomass = mean(biomass, na.rm = TRUE), .groups = "drop")
    } else {
      slot_name <- if (identical(basis, "total")) "totalBiomass" else "adultBiomass"
      biomass <- tryCatch(safe_array_to_df(slot(rep_obj, slot_name)), error = function(e) NULL)
      if (is.null(biomass) || nrow(biomass) == 0 || !"data" %in% names(biomass)) return(data.frame())

      biomass %>%
        mutate(
          region = as.character(area),
          year = suppressWarnings(as.numeric(as.character(year))),
          season = suppressWarnings(as.numeric(as.character(season))),
          biomass = suppressWarnings(as.numeric(data))
        ) %>%
        filter(region %in% region_values, is.finite(year), is.finite(season), is.finite(biomass), biomass > 0) %>%
        group_by(year, season, region) %>%
        summarise(biomass = sum(biomass, na.rm = TRUE), .groups = "drop")
    }
  }

  build_cpue_biomass_relative_df <- function(cpue_all, filters) {
    if (is.null(cpue_all) || nrow(cpue_all) == 0) return(data.frame())

    cpue_series <- if (identical(filters$relative_cpue_series, "obs")) "obs" else "fit"
    biomass_basis <- if (filters$relative_biomass_basis %in% c("vuln", "adult", "total")) filters$relative_biomass_basis else "total"

    map_dfr(filters$scenarios, function(scenario) {
      fishery_map <- rv$FISHERY_MAPS[[scenario]]
      rep_obj <- rv$RepOut_list[[scenario]]
      scenario_cpue <- cpue_all %>%
        filter(Scenario == scenario) %>%
        mutate(
          region = vapply(unit, get_cpue_region, character(1), mapping = fishery_map),
          cpue_value = if (identical(cpue_series, "fit")) fit else obs
        ) %>%
        filter(!is.na(region), nzchar(region), is.finite(cpue_value), cpue_value > 0) %>%
        group_by(Scenario, year, season, year_season, region) %>%
        summarise(
          cpue_value = mean(cpue_value, na.rm = TRUE),
          fishery_name = paste(sort(unique(as.character(fishery_name))), collapse = ", "),
          .groups = "drop"
        )
      if (nrow(scenario_cpue) == 0) return(NULL)

      biomass <- extract_biomass_by_region(
        rep_obj = rep_obj,
        fishery_map = fishery_map,
        fisheries = filters$fisheries,
        basis = biomass_basis
      )
      if (is.null(biomass) || nrow(biomass) == 0) return(NULL)

      scenario_cpue %>%
        inner_join(biomass, by = c("year", "season", "region")) %>%
        group_by(Scenario, year, season) %>%
        mutate(
          n_regions = n_distinct(region[is.finite(cpue_value) & is.finite(biomass)]),
          cpue_total = sum(cpue_value, na.rm = TRUE),
          biomass_total = sum(biomass, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        filter(n_regions >= 2, is.finite(cpue_total), cpue_total > 0, is.finite(biomass_total), biomass_total > 0) %>%
        mutate(
          cpue_scale = cpue_value / cpue_total,
          biomass_scale = biomass / biomass_total,
          region_label = paste("Region", region),
          cpue_series = if (identical(cpue_series, "fit")) "Fitted CPUE" else "Observed CPUE",
          biomass_basis = dplyr::case_when(
            biomass_basis == "vuln" ~ "Vulnerable biomass",
            biomass_basis == "total" ~ "Total biomass",
            TRUE ~ "Adult biomass"
          )
        ) %>%
        filter(is.finite(cpue_scale), is.finite(biomass_scale))
    })
  }

  build_relative_scale_plot <- function(rel_df, filters) {
    if (is.null(rel_df) || nrow(rel_df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No matched CPUE/biomass relative-scale data", size = 6, color = "#999") +
          theme_void()
      )
    }

    rel_df <- rel_df %>%
      mutate(
        region_label = factor(region_label, levels = paste("Region", sort(unique(suppressWarnings(as.numeric(region)))))),
        Scenario = factor(Scenario, levels = filters$scenarios)
      )

    scenario_count <- dplyr::n_distinct(rel_df$Scenario)
    facet_ncol <- suppressWarnings(as.integer(filters$facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
    facet_ncol <- min(max(facet_ncol, 1), 12)

    cpue_label <- unique(rel_df$cpue_series)[1]
    biomass_label <- unique(rel_df$biomass_basis)[1]

    long_df <- rel_df %>%
      select(Scenario, region_label, cpue_scale, biomass_scale) %>%
      tidyr::pivot_longer(
        cols = c(cpue_scale, biomass_scale),
        names_to = "source",
        values_to = "regional_share"
      ) %>%
      mutate(
        source = recode(source, cpue_scale = cpue_label, biomass_scale = biomass_label)
      )

    avg_df <- long_df %>%
      group_by(Scenario, region_label, source) %>%
      summarise(
        mean_share = mean(regional_share, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        source = factor(source, levels = c(cpue_label, biomass_label)),
        share_label = sprintf("%.1f%%", 100 * mean_share)
      )

    p <- ggplot(avg_df, aes(x = region_label, y = mean_share, fill = source)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64, color = "grey25", linewidth = 0.2) +
      geom_text(
        aes(label = share_label),
        position = position_dodge(width = 0.72),
        vjust = -0.35,
        size = 4.4,
        fontface = "bold",
        color = "#374151"
      ) +
      scale_fill_manual(values = setNames(c("#0072B2", "#D55E00"), c(cpue_label, biomass_label))) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.18)),
        labels = function(x) sprintf("%.0f%%", 100 * x)
      ) +
      labs(
        x = NULL,
        y = "Mean regional share over t",
        fill = NULL,
        title = paste(cpue_label, "mean regional share vs", biomass_label, "share"),
        subtitle = "For each t, region shares sum to 1; bars show the average share by region."
      ) +
      theme_bw(base_size = 15) +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#4b5563"),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 13)
      )

    if (scenario_count > 1) {
      p + facet_wrap(~Scenario, ncol = min(facet_ncol, scenario_count), scales = "free_x")
    } else {
      p
    }
  }

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
                    input$cpue_relative_cpue_series, input$cpue_relative_biomass_basis,
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

      if (identical(metric, "relative_scale")) {
        rel_df <- build_cpue_biomass_relative_df(cpue_all, filters)
        return(build_relative_scale_plot(rel_df, filters))
      }

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
            scenario_fishery = factor(scenario_fishery, levels = unique(as.character(scenario_fishery)))
          )
      }

      finite_range <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        x <- x[is.finite(x)]
        if (length(x) == 0) return(NULL)
        range(x)
      }

      if (identical(metric, "residuals") && identical(view_mode, "by_scenario")) {
        plot_df <- cpue_all
        panel_spec <- NULL
        facet_formula <- Scenario ~ fishery_name
        panel_spec <- by_model_panel_spec(plot_df)
        plot_df <- format_by_model_panel_data(plot_df, panel_spec)

        if (requireNamespace("patchwork", quietly = TRUE)) {
          y_limits <- if (isTRUE(free_y_panel)) NULL else finite_range(plot_df$residual)
          scenario_plots <- lapply(seq_along(panel_spec$scenario_vals), function(i) {
            scenario <- panel_spec$scenario_vals[[i]]
            row_df <- plot_df %>%
              filter(as.character(Scenario) == scenario) %>%
              droplevels()
            if (nrow(row_df) == 0) return(NULL)
            row_ncol <- max(1, dplyr::n_distinct(row_df$fishery_name))

            row_plot <- ggplot(row_df, aes(x = year_season, y = residual, color = Scenario)) +
              geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
              geom_point(size = 1.2, alpha = 0.55) +
              facet_wrap(~fishery_name, scales = if (isTRUE(free_y_panel)) "free_y" else "fixed", ncol = row_ncol) +
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

            if (!isTRUE(free_y_panel) && !is.null(y_limits)) {
              row_plot <- row_plot + coord_cartesian(ylim = y_limits)
            }

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
          scenario_plots <- Filter(Negate(is.null), scenario_plots)

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
          p <- p + facet_wrap(
            facet_formula,
            scales = "fixed",
            ncol = facet_ncol,
            labeller = as_labeller(panel_spec$panel_labels)
          )
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
        panel_spec <- by_model_panel_spec(plot_df)
        plot_df <- format_by_model_panel_data(plot_df, panel_spec)
        obs_df <- format_by_model_panel_data(obs_df, panel_spec)

        if (requireNamespace("patchwork", quietly = TRUE)) {
          y_limits <- if (isTRUE(free_y_panel)) NULL else finite_range(c(plot_df$fit, obs_df$obs))
          scenario_plots <- lapply(seq_along(panel_spec$scenario_vals), function(i) {
            scenario <- panel_spec$scenario_vals[[i]]
            row_df <- plot_df %>%
              filter(as.character(Scenario) == scenario) %>%
              droplevels()
            row_obs <- obs_df %>%
              filter(as.character(Scenario) == scenario) %>%
              droplevels()
            if (nrow(row_df) == 0) return(NULL)
            row_ncol <- max(1, dplyr::n_distinct(row_df$fishery_name))

            row_plot <- ggplot(row_df, aes(x = year_season)) +
              geom_point(data = row_obs, aes(y = obs), size = 1.8, alpha = 0.5, color = "#6b7280") +
              geom_line(aes(y = fit, color = Scenario), linewidth = 1.1, alpha = 0.9) +
              facet_wrap(~fishery_name, scales = if (isTRUE(free_y_panel)) "free_y" else "fixed", ncol = row_ncol) +
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

            if (!isTRUE(free_y_panel) && !is.null(y_limits)) {
              row_plot <- row_plot + coord_cartesian(ylim = y_limits)
            }

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
          scenario_plots <- Filter(Negate(is.null), scenario_plots)

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
          p <- p + facet_wrap(
            facet_formula,
            scales = "fixed",
            ncol = facet_ncol,
            labeller = as_labeller(panel_spec$panel_labels)
          )
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
    rv$data_loaded,
    input$model_dir,
    sort(names(rv$RepOut_list)),
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
