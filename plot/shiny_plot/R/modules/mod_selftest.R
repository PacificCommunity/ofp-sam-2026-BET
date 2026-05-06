mod_selftest_ui <- function() {
  tabItem(
    tabName = "selftest",
    h2("Self-Test Recovery", style = "color: #3c8dbc;"),
    fluidRow(
      box(
        title = "Settings",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        pickerInput(
          "selftest_scenarios",
          "Replicates:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} reps selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search reps...",
            size = 10
          )
        ),
        selectInput(
          "selftest_metric",
          "Quantity:",
          choices = c(
            "Depletion" = "depletion",
            "Spawning potential" = "spawning_potential",
            "Recruitment" = "recruitment",
            "Fishing mortality" = "fishing_mortality"
          ),
          selected = "depletion"
        ),
        checkboxInput("selftest_show_replicates", "Show replicate traces", value = TRUE),
        checkboxInput("selftest_show_interval", "Show 80% refit interval", value = TRUE),
        sliderInput(
          "selftest_recovery_height",
          "Recovery plot height (px)",
          min = 450,
          max = 1400,
          value = 650,
          step = 50
        ),
        sliderInput(
          "selftest_sim_height",
          "Simulation-check height (px)",
          min = 350,
          max = 1000,
          value = 520,
          step = 50
        ),
        actionButton(
          "selftest_apply_filters",
          "Apply",
          class = "btn-primary",
          style = "width: 100%;"
        ),
        tags$small(
          "Load the self-test refit folder, e.g. selftest/<run>/refit.",
          style = "display:block; margin-top:8px; color:#666;"
        )
      ),
      box(
        title = "Truth vs Refit",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        uiOutput("selftest_recovery_plot_ui")
      )
    ),
    fluidRow(
      box(
        title = "Pseudo-Data Simulation Checks",
        width = 8,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        uiOutput("selftest_sim_plot_ui")
      ),
      box(
        title = "Replicate Checks",
        width = 4,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        DT::DTOutput("selftest_sim_table")
      )
    )
  )
}

stp_first_or_null <- function(x) {
  if (length(x) > 0) x[[1]] else NULL
}

stp_rep_id <- function(x) {
  out <- suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(as.character(x)))))
  ifelse(is.finite(out), out, seq_along(x))
}

stp_model_folder <- function(model_dir, scenario) {
  normalizePath(file.path(model_dir, scenario), mustWork = FALSE)
}

stp_selftest_sibling_dir <- function(model_dir, scenario, sibling) {
  folder <- stp_model_folder(model_dir, scenario)
  parent <- dirname(folder)
  root <- dirname(parent)
  if (!basename(parent) %in% c("refit", "inputs", "sim", "recovery")) {
    return(file.path(parent, sibling, basename(folder)))
  }
  file.path(root, sibling, basename(folder))
}

stp_read_model_rep <- function(folder) {
  payload_file <- file.path(folder, "model_payload.rds")
  if (file.exists(payload_file)) {
    payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
    rep_obj <- tryCatch(payload$data$RepOut, error = function(e) NULL)
    if (!is.null(rep_obj)) return(rep_obj)
  }

  rep_file <- tryCatch(mp_final_rep(folder), error = function(e) NULL)
  if (is.null(rep_file) || !file.exists(rep_file)) return(NULL)
  tryCatch(read.MFCLRep(rep_file), error = function(e) NULL)
}

stp_extract_metric <- function(rep_obj, metric) {
  if (is.null(rep_obj)) return(NULL)

  if (identical(metric, "depletion")) {
    bio_fish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass")), error = function(e) NULL)
    bio_nofish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass_nofish")), error = function(e) NULL)
    if (is.null(bio_fish) || is.null(bio_nofish)) return(NULL)
    return(
      bio_fish %>%
        rename(bio_fish = value) %>%
        inner_join(rename(bio_nofish, bio_nofish = value), by = "year") %>%
        mutate(value = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
        select(year, value)
    )
  }

  if (identical(metric, "spawning_potential")) {
    bio_fish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass")), error = function(e) NULL)
    if (is.null(bio_fish)) return(NULL)
    return(rename(bio_fish, value = value))
  }

  if (identical(metric, "recruitment")) {
    rec <- tryCatch(safe_array_to_df(slot(rep_obj, "rec_region")), error = function(e) NULL)
    if (is.null(rec) || !"year" %in% names(rec) || !"data" %in% names(rec)) return(NULL)
    return(
      rec %>%
        mutate(year = suppressWarnings(as.numeric(as.character(year))),
               data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(data)) %>%
        group_by(year) %>%
        summarise(value = sum(data, na.rm = TRUE) / 1e6, .groups = "drop")
    )
  }

  if (identical(metric, "fishing_mortality")) {
    fm <- tryCatch(safe_array_to_df(slot(rep_obj, "fm")), error = function(e) NULL)
    if (is.null(fm) || !"year" %in% names(fm) || !"data" %in% names(fm)) return(NULL)
    return(
      fm %>%
        mutate(year = suppressWarnings(as.numeric(as.character(year))),
               data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(data)) %>%
        group_by(year) %>%
        summarise(value = mean(data, na.rm = TRUE), .groups = "drop")
    )
  }

  NULL
}

stp_metric_label <- function(metric) {
  switch(
    metric,
    depletion = "SB / SB(F=0)",
    spawning_potential = "Spawning potential ('000 t)",
    recruitment = "Recruitment (millions)",
    fishing_mortality = "Mean fishing mortality",
    metric
  )
}

stp_summary_band <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  df %>%
    group_by(year) %>%
    summarise(
      median = stats::median(value, na.rm = TRUE),
      lower = stats::quantile(value, probs = 0.10, na.rm = TRUE, names = FALSE),
      upper = stats::quantile(value, probs = 0.90, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

stp_read_selftest_input_info <- function(model_dir, scenario) {
  model_folder <- stp_model_folder(model_dir, scenario)
  input_folder <- stp_selftest_sibling_dir(model_dir, scenario, "inputs")
  candidates <- c(
    file.path(model_folder, "selftest_input_info.rds"),
    file.path(input_folder, "selftest_input_info.rds")
  )
  path <- stp_first_or_null(candidates[file.exists(candidates)])
  if (is.null(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(out)) return(NULL)
  out
}

stp_input_info_row <- function(info, scenario) {
  get_num <- function(name) {
    x <- suppressWarnings(as.numeric(info[[name]]))
    if (length(x) == 0 || !is.finite(x[1])) NA_real_ else x[1]
  }
  get_chr <- function(name) {
    x <- info[[name]]
    if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  }
  get_lgl <- function(name) {
    x <- info[[name]]
    if (length(x) == 0 || is.na(x[1])) NA else isTRUE(x[1])
  }

  data.frame(
    scenario = scenario,
    replicate = stp_rep_id(scenario),
    catch_replaced_rows = get_num("catch_replaced_rows"),
    cpue_replaced_rows = get_num("cpue_replaced_rows"),
    length_replaced_rows = get_num("length_replaced_rows"),
    weight_replaced_rows = get_num("weight_replaced_rows"),
    tag_recaptures_total = get_num("tag_recaptures_total"),
    tag_recapture_rows = get_num("tag_recapture_rows"),
    age_length_total_obs = get_num("age_length_total_obs"),
    age_length_total_sim = get_num("age_length_total_sim"),
    length_sample_size_mismatch = get_num("length_sample_size_mismatch"),
    weight_sample_size_mismatch = get_num("weight_sample_size_mismatch"),
    age_length_zero_prediction_bins = get_num("age_length_zero_prediction_bins"),
    age_length_sample_sizes_matched = get_lgl("age_length_sample_sizes_matched"),
    base_minyear = get_num("base_minyear"),
    base_maxyear = get_num("base_maxyear"),
    pseudo_minyear = get_num("pseudo_minyear"),
    pseudo_maxyear = get_num("pseudo_maxyear"),
    estimation_period_matched = get_lgl("estimation_period_matched"),
    tag_source = get_chr("tag_source"),
    age_length_source = get_chr("age_length_source"),
    stringsAsFactors = FALSE
  )
}

mod_selftest_server <- function(input, output, session, rv) {
  selftest_filters_current <- reactive({
    list(
      scenarios = input$selftest_scenarios,
      metric = if (is.null(input$selftest_metric)) "depletion" else input$selftest_metric,
      show_replicates = isTRUE(input$selftest_show_replicates),
      show_interval = isTRUE(input$selftest_show_interval),
      recovery_height = if (is.null(input$selftest_recovery_height)) 650 else suppressWarnings(as.integer(input$selftest_recovery_height)),
      sim_height = if (is.null(input$selftest_sim_height)) 520 else suppressWarnings(as.integer(input$selftest_sim_height))
    )
  })
  selftest_filters_applied <- reactiveVal(NULL)

  observeEvent(rv$data_loaded, {
    req(rv$data_loaded)
    models <- names(rv$RepOut_list)
    choices <- if (!is.null(rv$model_choice_labels)) {
      rv$model_choice_labels[unname(rv$model_choice_labels) %in% models]
    } else {
      stats::setNames(models, models)
    }
    updatePickerInput(session, "selftest_scenarios", choices = choices, selected = models)
  }, ignoreInit = FALSE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(selftest_filters_current(), selftest_filters_applied())
    set_apply_pending(session, "selftest_apply_filters", pending)
  })

  observeEvent(input$selftest_apply_filters, {
    selftest_filters_applied(isolate(selftest_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$live_update_plots, input$selftest_scenarios, input$selftest_metric,
         input$selftest_show_replicates, input$selftest_show_interval,
         input$selftest_recovery_height, input$selftest_sim_height),
    {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      if (length(input$selftest_scenarios) == 0) return()
      selftest_filters_applied(isolate(selftest_filters_current()))
    },
    ignoreInit = TRUE
  )

  observeEvent(list(rv$initial_render_nonce, input$selftest_scenarios), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (length(input$selftest_scenarios) == 0) return()
    selftest_filters_applied(isolate(selftest_filters_current()))
  }, ignoreInit = TRUE)

  selftest_recovery_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters, filters$scenarios)
    scenarios <- intersect(filters$scenarios, names(rv$RepOut_list))
    if (length(scenarios) == 0) return(data.frame())

    bind_rows(lapply(scenarios, function(scenario) {
      rep_id <- stp_rep_id(scenario)
      refit_ts <- stp_extract_metric(rv$RepOut_list[[scenario]], filters$metric)
      truth_dir <- stp_selftest_sibling_dir(input$model_dir, scenario, "sim")
      truth_ts <- stp_extract_metric(stp_read_model_rep(truth_dir), filters$metric)

      bind_rows(
        if (!is.null(truth_ts)) {
          truth_ts %>%
            mutate(scenario = scenario, replicate = rep_id, source = "truth")
        },
        if (!is.null(refit_ts)) {
          refit_ts %>%
            mutate(scenario = scenario, replicate = rep_id, source = "refit")
        }
      )
    })) %>%
      mutate(
        year = suppressWarnings(as.numeric(year)),
        value = suppressWarnings(as.numeric(value))
      ) %>%
      filter(is.finite(year), is.finite(value))
  })

  selftest_sim_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters, filters$scenarios)
    scenarios <- intersect(filters$scenarios, names(rv$RepOut_list))
    rows <- lapply(scenarios, function(scenario) {
      info <- stp_read_selftest_input_info(input$model_dir, scenario)
      if (is.null(info)) return(NULL)
      stp_input_info_row(info, scenario)
    })
    bind_rows(rows)
  })

  selftest_recovery_plot <- reactive({
    filters <- selftest_filters_applied()
    req(filters)
    df <- selftest_recovery_data()
    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No self-test truth/refit data found", size = 5, color = "#777") +
          theme_void()
      )
    }

    truth <- df %>% filter(source == "truth")
    refit <- df %>% filter(source == "refit")
    truth_med <- stp_summary_band(truth)
    refit_band <- stp_summary_band(refit)
    y_label <- stp_metric_label(filters$metric)

    p <- ggplot()
    if (isTRUE(filters$show_interval) && nrow(refit_band) > 0) {
      p <- p +
        geom_ribbon(
          data = refit_band,
          aes(x = year, ymin = lower, ymax = upper),
          fill = "#7f8fa6",
          alpha = 0.22
        )
    }
    if (isTRUE(filters$show_replicates) && nrow(refit) > 0) {
      p <- p +
        geom_line(
          data = refit,
          aes(x = year, y = value, group = scenario),
          color = "#718093",
          alpha = 0.35,
          linewidth = 0.45
        )
    }
    if (nrow(refit_band) > 0) {
      p <- p +
        geom_line(data = refit_band, aes(x = year, y = median), color = "#111111", linewidth = 1.15)
    }
    if (nrow(truth_med) > 0) {
      p <- p +
        geom_line(data = truth_med, aes(x = year, y = median), color = "#d62728", linewidth = 1.25)
    }

    p +
      labs(
        x = "Year",
        y = y_label,
        subtitle = "Red = truth, black = refit median, grey = refit replicate traces"
      ) +
      theme_bw(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = "#555")
      )
  })

  selftest_sim_plot <- reactive({
    info <- selftest_sim_data()
    if (nrow(info) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No pseudo-data build diagnostics found", size = 5, color = "#777") +
          theme_void()
      )
    }

    counts <- info %>%
      select(
        replicate,
        catch_replaced_rows,
        cpue_replaced_rows,
        length_replaced_rows,
        weight_replaced_rows,
        tag_recaptures_total,
        age_length_total_sim
      ) %>%
      pivot_longer(-replicate, names_to = "component", values_to = "value") %>%
      mutate(component = recode(
        component,
        catch_replaced_rows = "catch",
        cpue_replaced_rows = "CPUE",
        length_replaced_rows = "length",
        weight_replaced_rows = "weight",
        tag_recaptures_total = "tag recaptures",
        age_length_total_sim = "age-length"
      )) %>%
      filter(is.finite(value))

    checks <- info %>%
      mutate(age_length_total_delta = age_length_total_sim - age_length_total_obs) %>%
      select(
        replicate,
        length_sample_size_mismatch,
        weight_sample_size_mismatch,
        age_length_total_delta,
        age_length_zero_prediction_bins
      ) %>%
      pivot_longer(-replicate, names_to = "check", values_to = "value") %>%
      mutate(check = recode(
        check,
        length_sample_size_mismatch = "length sample mismatch",
        weight_sample_size_mismatch = "weight sample mismatch",
        age_length_total_delta = "age-length total delta",
        age_length_zero_prediction_bins = "age-length zero-pred bins"
      )) %>%
      filter(is.finite(value))

    p_counts <- ggplot(counts, aes(x = replicate, y = value, color = component)) +
      geom_line(alpha = 0.45) +
      geom_point(size = 1.8, alpha = 0.85) +
      scale_y_continuous(trans = "log10") +
      labs(x = NULL, y = "Pseudo-data count (log10)", color = NULL) +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    p_checks <- ggplot(checks, aes(x = replicate, y = value, color = check)) +
      geom_hline(yintercept = 0, color = "#555", linewidth = 0.4) +
      geom_line(alpha = 0.45) +
      geom_point(size = 1.8, alpha = 0.85) +
      labs(x = "Replicate", y = "Mismatch / diagnostic count", color = NULL) +
      theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    cowplot::plot_grid(p_counts, p_checks, ncol = 1, align = "v", rel_heights = c(1.05, 1))
  })

  output$selftest_recovery_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_recovery_height)) 650 else input$selftest_recovery_height
    plotOutput("selftest_recovery_plot", height = paste0(h, "px"))
  })

  output$selftest_sim_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_sim_height)) 520 else input$selftest_sim_height
    plotOutput("selftest_sim_plot", height = paste0(h, "px"))
  })

  output$selftest_recovery_plot <- renderPlot({
    selftest_recovery_plot()
  })

  output$selftest_sim_plot <- renderPlot({
    selftest_sim_plot()
  })

  output$selftest_sim_table <- DT::renderDT({
    info <- selftest_sim_data()
    if (nrow(info) == 0) {
      return(DT::datatable(data.frame(message = "No self-test input diagnostics found"), rownames = FALSE))
    }
    out <- info %>%
      transmute(
        replicate,
        tag_source,
        tag_recaptures_total,
        age_length_source,
        age_length_sample_sizes_matched,
        length_sample_size_mismatch,
        weight_sample_size_mismatch,
        age_length_zero_prediction_bins
      ) %>%
      arrange(replicate)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}
