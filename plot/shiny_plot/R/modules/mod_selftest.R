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
          min = 500,
          max = 1800,
          value = 1200,
          step = 50
        ),
        actionButton(
          "selftest_apply_filters",
          "Apply",
          class = "btn-primary",
          style = "width: 100%;"
        ),
        tags$small(
          "Load the self-test refit folder, e.g. model/<run>/selftest/refit.",
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

stp_selftest_index <- function(model_dir, loaded_models = character()) {
  model_dir <- normalizePath(model_dir, winslash = "/", mustWork = FALSE)
  loaded_models <- as.character(loaded_models)
  loaded_models <- loaded_models[nzchar(loaded_models)]

  make_rows <- function(model_name, selftest_root) {
    refit_root <- file.path(selftest_root, "refit")
    sim_root <- file.path(selftest_root, "sim")
    input_root <- file.path(selftest_root, "inputs")
    if (!dir.exists(refit_root)) return(NULL)
    rep_dirs <- list.dirs(refit_root, recursive = FALSE, full.names = FALSE)
    rep_dirs <- rep_dirs[grepl("^rep_\\d+$", rep_dirs)]
    if (length(rep_dirs) == 0) return(NULL)
    rep_dirs <- sort(rep_dirs)
    data.frame(
      key = paste(model_name, rep_dirs, sep = "::"),
      label = paste(model_name, rep_dirs),
      model = model_name,
      replicate_name = rep_dirs,
      replicate = stp_rep_id(rep_dirs),
      refit_dir = file.path(refit_root, rep_dirs),
      truth_dir = file.path(sim_root, rep_dirs),
      input_dir = file.path(input_root, rep_dirs),
      stringsAsFactors = FALSE
    )
  }

  parent <- basename(model_dir)
  grandparent <- basename(dirname(model_dir))
  if (parent %in% c("refit", "sim", "inputs") && identical(grandparent, "selftest")) {
    model_name <- basename(dirname(dirname(model_dir)))
    return(make_rows(model_name, dirname(model_dir)))
  }

  if (identical(parent, "selftest")) {
    model_name <- basename(dirname(model_dir))
    return(make_rows(model_name, model_dir))
  }

  rows <- lapply(loaded_models, function(model_name) {
    make_rows(model_name, file.path(model_dir, model_name, "selftest"))
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0 && dir.exists(file.path(model_dir, "selftest"))) {
    out <- make_rows(basename(model_dir), file.path(model_dir, "selftest"))
  }
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(
      key = character(), label = character(), model = character(),
      replicate_name = character(), replicate = integer(),
      refit_dir = character(), truth_dir = character(), input_dir = character(),
      stringsAsFactors = FALSE
    ))
  }
  out
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

stp_scalar_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || !is.finite(out[1])) NA_real_ else out[1]
}

stp_read_run_diagnostics <- function(folder, source, scenario, replicate) {
  payload_file <- file.path(folder, "model_payload.rds")
  payload <- if (file.exists(payload_file)) tryCatch(readRDS(payload_file), error = function(e) NULL) else NULL
  info <- tryCatch(payload$data$info, error = function(e) NULL)
  if (is.null(info)) {
    info_file <- file.path(folder, "model_info.rds")
    if (file.exists(info_file)) info <- tryCatch(readRDS(info_file), error = function(e) NULL)
  }
  par_obj <- tryCatch(payload$data$ParOut, error = function(e) NULL)
  obj_fun <- stp_scalar_num(payload$obj_fun)
  if (!is.finite(obj_fun)) obj_fun <- stp_scalar_num(info$obj_fun)
  if (!is.finite(obj_fun) && !is.null(par_obj)) obj_fun <- stp_scalar_num(tryCatch(par_obj@obj_fun, error = function(e) NA_real_))
  max_grad <- stp_scalar_num(payload$max_grad)
  if (!is.finite(max_grad)) max_grad <- stp_scalar_num(info$max_grad)
  if (!is.finite(max_grad) && !is.null(par_obj)) max_grad <- stp_scalar_num(tryCatch(par_obj@max_grad, error = function(e) NA_real_))
  exit_status <- suppressWarnings(as.integer(tryCatch(info$exit_status, error = function(e) NA_integer_)))
  if (length(exit_status) == 0 || is.na(exit_status[1])) exit_status <- NA_integer_ else exit_status <- exit_status[1]
  run_completed <- dir.exists(folder) && is.finite(obj_fun) && is.finite(max_grad) &&
    (is.na(exit_status) || identical(exit_status, 0L))
  data.frame(
    scenario = scenario,
    replicate = replicate,
    source = source,
    obj_fun = obj_fun,
    max_grad = max_grad,
    abs_max_grad = abs(max_grad),
    exit_status = exit_status,
    run_completed = run_completed,
    converged = isTRUE(run_completed) && isTRUE(abs(max_grad) <= 0.01),
    stringsAsFactors = FALSE
  )
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

stp_read_selftest_input_info <- function(model_dir, scenario, input_dir = NULL) {
  model_folder <- stp_model_folder(model_dir, scenario)
  input_folder <- if (!is.null(input_dir) && nzchar(input_dir)) {
    input_dir
  } else {
    stp_selftest_sibling_dir(model_dir, scenario, "inputs")
  }
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

stp_read_data_simulation_summary <- function(input_dir, scenario) {
  if (is.null(input_dir) || !nzchar(input_dir)) return(NULL)
  path <- file.path(input_dir, "data_simulation_summary.rds")
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(out) || nrow(out) == 0) return(NULL)
  out$scenario <- scenario
  out$replicate <- stp_rep_id(scenario)
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
  selftest_index_current <- reactive({
    req(rv$data_loaded)
    stp_selftest_index(input$model_dir, names(rv$RepOut_list))
  })

  selftest_filters_current <- reactive({
    list(
      scenarios = input$selftest_scenarios,
      metric = if (is.null(input$selftest_metric)) "depletion" else input$selftest_metric,
      show_replicates = isTRUE(input$selftest_show_replicates),
      show_interval = isTRUE(input$selftest_show_interval),
      recovery_height = if (is.null(input$selftest_recovery_height)) 650 else suppressWarnings(as.integer(input$selftest_recovery_height)),
      sim_height = if (is.null(input$selftest_sim_height)) 1200 else suppressWarnings(as.integer(input$selftest_sim_height))
    )
  })
  selftest_filters_applied <- reactiveVal(NULL)

  observeEvent(rv$data_loaded, {
    req(rv$data_loaded)
    idx <- selftest_index_current()
    choices <- if (nrow(idx) > 0) stats::setNames(idx$key, idx$label) else character()
    selected <- unname(choices)
    updatePickerInput(session, "selftest_scenarios", choices = choices, selected = selected)
    if (length(selected) > 0) {
      current <- isolate(selftest_filters_current())
      current$scenarios <- selected
      selftest_filters_applied(current)
    }
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
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    selected <- intersect(as.character(filters$scenarios), idx$key)
    if (length(selected) == 0 && nrow(idx) > 0) selected <- idx$key
    idx <- idx[idx$key %in% selected, , drop = FALSE]
    if (nrow(idx) == 0) return(data.frame())

    bind_rows(lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      scenario <- row$key[[1]]
      rep_id <- row$replicate[[1]]
      refit_ts <- stp_extract_metric(stp_read_model_rep(row$refit_dir[[1]]), filters$metric)
      truth_ts <- stp_extract_metric(stp_read_model_rep(row$truth_dir[[1]]), filters$metric)

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
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    selected <- intersect(as.character(filters$scenarios), idx$key)
    if (length(selected) == 0 && nrow(idx) > 0) selected <- idx$key
    idx <- idx[idx$key %in% selected, , drop = FALSE]
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      info <- stp_read_selftest_input_info(input$model_dir, row$key[[1]], row$input_dir[[1]])
      if (is.null(info)) return(NULL)
      stp_input_info_row(info, row$key[[1]])
    })
    bind_rows(rows)
  })

  selftest_data_simulation_series <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    selected <- intersect(as.character(filters$scenarios), idx$key)
    if (length(selected) == 0 && nrow(idx) > 0) selected <- idx$key
    idx <- idx[idx$key %in% selected, , drop = FALSE]
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      stp_read_data_simulation_summary(row$input_dir[[1]], row$key[[1]])
    })
    bind_rows(rows)
  })

  selftest_convergence_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    selected <- intersect(as.character(filters$scenarios), idx$key)
    if (length(selected) == 0 && nrow(idx) > 0) selected <- idx$key
    idx <- idx[idx$key %in% selected, , drop = FALSE]
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      bind_rows(
        stp_read_run_diagnostics(row$truth_dir[[1]], "truth", row$key[[1]], row$replicate[[1]]),
        stp_read_run_diagnostics(row$refit_dir[[1]], "refit", row$key[[1]], row$replicate[[1]])
      )
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

    data_series <- selftest_data_simulation_series()
    conv <- selftest_convergence_data()

    p_conv <- NULL
    if (nrow(conv) > 0) {
      refit_conv <- conv %>% filter(source == "refit")
      p_obj <- ggplot(refit_conv, aes(x = replicate, y = obj_fun)) +
        geom_line(color = "#34495e", alpha = 0.55) +
        geom_point(aes(color = converged), size = 1.9, alpha = 0.9) +
        scale_color_manual(values = c(`TRUE` = "#1f9d55", `FALSE` = "#c0392b"), na.value = "#777") +
        labs(x = NULL, y = "Objective function", color = "MGC <= 0.01") +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      p_mgc <- ggplot(refit_conv, aes(x = replicate, y = abs_max_grad)) +
        geom_hline(yintercept = 0.01, color = "#c0392b", linetype = "dashed", linewidth = 0.4) +
        geom_line(color = "#34495e", alpha = 0.55) +
        geom_point(aes(color = converged), size = 1.9, alpha = 0.9) +
        scale_y_log10() +
        scale_color_manual(values = c(`TRUE` = "#1f9d55", `FALSE` = "#c0392b"), na.value = "#777") +
        labs(x = "Replicate", y = "|MGC| / max gradient", color = "MGC <= 0.01") +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
      p_conv <- cowplot::plot_grid(p_obj, p_mgc, ncol = 1, align = "v")
    }

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

    if (nrow(data_series) > 0) {
      annual <- data_series %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          base_value = suppressWarnings(as.numeric(base_value)),
          pseudo_value = suppressWarnings(as.numeric(pseudo_value))
        ) %>%
        filter(is.finite(year), is.finite(base_value), is.finite(pseudo_value))
      pseudo_band <- annual %>%
        group_by(component, year) %>%
        summarise(
          median = stats::median(pseudo_value, na.rm = TRUE),
          lower = stats::quantile(pseudo_value, 0.10, na.rm = TRUE, names = FALSE),
          upper = stats::quantile(pseudo_value, 0.90, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
      base_line <- annual %>%
        group_by(component, year) %>%
        summarise(base_value = stats::median(base_value, na.rm = TRUE), .groups = "drop")
      p_data <- ggplot() +
        geom_ribbon(data = pseudo_band, aes(x = year, ymin = lower, ymax = upper), fill = "#95a5a6", alpha = 0.18) +
        geom_line(data = annual, aes(x = year, y = pseudo_value, group = scenario), color = "#718093", alpha = 0.22, linewidth = 0.35) +
        geom_line(data = pseudo_band, aes(x = year, y = median), color = "#111111", linewidth = 0.85) +
        geom_line(data = base_line, aes(x = year, y = base_value), color = "#d62728", linewidth = 0.75) +
        facet_wrap(~ component, scales = "free_y", ncol = 2) +
        labs(
          x = "Year",
          y = "Annual value",
          subtitle = "Data simulation check: red = fitted input, grey = pseudo reps, black = pseudo median"
        ) +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "#eef3f7", color = NA))
      pieces <- list(p_data, p_conv, p_counts, p_checks)
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      heights <- if (length(pieces) == 4) c(1.6, 1.0, 0.9, 0.9) else c(1.6, 0.9, 0.9)
      cowplot::plot_grid(plotlist = pieces, ncol = 1, align = "v", rel_heights = heights)
    } else {
      p_missing <- ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = "No catch/CPUE value summary found. Re-run or retrieve self-test outputs created after this update.",
          size = 4.2, color = "#777"
        ) +
        theme_void()
      pieces <- list(p_missing, p_conv, p_counts, p_checks)
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      heights <- if (length(pieces) == 4) c(0.8, 1.0, 1.05, 1) else c(0.8, 1.05, 1)
      cowplot::plot_grid(plotlist = pieces, ncol = 1, align = "v", rel_heights = heights)
    }
  })

  output$selftest_recovery_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_recovery_height)) 650 else input$selftest_recovery_height
    plotOutput("selftest_recovery_plot", height = paste0(h, "px"))
  })

  output$selftest_sim_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_sim_height)) 1200 else input$selftest_sim_height
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
        scenario,
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
    conv <- selftest_convergence_data()
    if (nrow(conv) > 0) {
      conv_wide <- conv %>%
        select(scenario, source, obj_fun, max_grad, exit_status, run_completed, converged) %>%
        tidyr::pivot_wider(
          names_from = source,
          values_from = c(obj_fun, max_grad, exit_status, run_completed, converged),
          names_glue = "{source}_{.value}"
        )
      out <- out %>%
        left_join(conv_wide, by = "scenario") %>%
        mutate(delta_obj_refit_minus_truth = refit_obj_fun - truth_obj_fun) %>%
        select(
          scenario, replicate,
          refit_obj_fun, refit_max_grad, refit_converged, refit_exit_status,
          truth_obj_fun, truth_max_grad, delta_obj_refit_minus_truth,
          everything()
        )
    }
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}
