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
        pickerInput(
          "selftest_sim_components",
          "Simulation data series:",
          choices = c(
            "Catch total" = "catch total",
            "CPUE" = "CPUE",
            "CPUE mean (old summaries)" = "CPUE mean",
            "Effort" = "effort",
            "Length mean" = "length mean",
            "Weight mean" = "weight mean",
            "Tag recaptures" = "tag recaptures",
            "Age-length mean age" = "age-length mean age"
          ),
          selected = c(
            "catch total",
            "CPUE",
            "effort",
            "length mean",
            "weight mean",
            "tag recaptures",
            "age-length mean age"
          ),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} series selected",
            size = 6
          )
        ),
        checkboxInput("selftest_show_replicates", "Show replicate traces", value = FALSE),
        checkboxInput("selftest_show_interval", "Show refit interval", value = TRUE),
        selectInput(
          "selftest_interval_level",
          "Interval:",
          choices = c("50%" = 0.50, "80%" = 0.80, "90%" = 0.90, "95%" = 0.95),
          selected = 0.95
        ),
        sliderInput(
          "selftest_recovery_height",
          "Recovery plot height (px)",
          min = 450,
          max = 1400,
          value = 900,
          step = 50
        ),
        sliderInput(
          "selftest_sim_height",
          "Simulation-check height (px)",
          min = 350,
          max = 1800,
          value = 850,
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
        width = 12,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        uiOutput("selftest_sim_plot_ui")
      )
    ),
    fluidRow(
      box(
        title = "Replicate Checks",
        width = 12,
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
    truth_eval_root <- file.path(selftest_root, "truth_eval")
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
      truth_eval_dir = file.path(truth_eval_root, rep_dirs),
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
      refit_dir = character(), truth_dir = character(), truth_eval_dir = character(), input_dir = character(),
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

stp_par_slot_num <- function(par_obj, slot_name) {
  if (is.null(par_obj) || !slot_name %in% slotNames(par_obj)) return(NA_real_)
  stp_scalar_num(tryCatch(slot(par_obj, slot_name), error = function(e) NA_real_))
}

stp_read_run_diagnostics <- function(folder, source, scenario, replicate) {
  payload_file <- file.path(folder, "model_payload.rds")
  payload <- if (file.exists(payload_file)) tryCatch(readRDS(payload_file), error = function(e) NULL) else NULL
  info_file <- file.path(folder, "model_info.rds")
  info <- if (file.exists(info_file)) tryCatch(readRDS(info_file), error = function(e) NULL) else NULL
  if (is.null(info)) info <- tryCatch(payload$data$info, error = function(e) NULL)
  par_obj <- tryCatch(payload$data$ParOut, error = function(e) NULL)
  info_first <- identical(source, "truth_on_pseudo")
  obj_fun <- if (isTRUE(info_first)) stp_scalar_num(info$obj_fun) else stp_scalar_num(payload$obj_fun)
  if (!is.finite(obj_fun)) obj_fun <- if (isTRUE(info_first)) stp_scalar_num(payload$obj_fun) else stp_scalar_num(info$obj_fun)
  if (!is.finite(obj_fun) && !is.null(par_obj)) obj_fun <- stp_scalar_num(tryCatch(par_obj@obj_fun, error = function(e) NA_real_))
  max_grad <- if (isTRUE(info_first)) stp_scalar_num(info$max_grad) else stp_scalar_num(payload$max_grad)
  if (!is.finite(max_grad)) max_grad <- if (isTRUE(info_first)) stp_scalar_num(payload$max_grad) else stp_scalar_num(info$max_grad)
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
    tag_lik = stp_par_slot_num(par_obj, "tag_lik"),
    mn_len_pen = stp_par_slot_num(par_obj, "mn_len_pen"),
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

stp_recovery_metrics <- function() {
  c("depletion", "spawning_potential", "recruitment", "fishing_mortality")
}

stp_extract_all_metrics <- function(rep_obj) {
  bind_rows(lapply(stp_recovery_metrics(), function(metric) {
    x <- stp_extract_metric(rep_obj, metric)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x$metric <- metric
    x$quantity <- stp_metric_label(metric)
    x
  }))
}

stp_summary_band <- function(df, interval_level = 0.95) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  interval_level <- suppressWarnings(as.numeric(interval_level[[1]]))
  if (!is.finite(interval_level)) interval_level <- 0.95
  interval_level <- max(0.01, min(0.99, interval_level))
  lower_prob <- (1 - interval_level) / 2
  upper_prob <- 1 - lower_prob
  group_cols <- intersect(c("metric", "quantity"), names(df))
  df %>%
    group_by(across(all_of(c(group_cols, "year")))) %>%
    summarise(
      median = stats::median(value, na.rm = TRUE),
      lower = stats::quantile(value, probs = lower_prob, na.rm = TRUE, names = FALSE),
      upper = stats::quantile(value, probs = upper_prob, na.rm = TRUE, names = FALSE),
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

stp_read_data_simulation_summary <- function(input_dir, scenario, model = "") {
  if (is.null(input_dir) || !nzchar(input_dir)) return(NULL)
  path <- file.path(input_dir, "data_simulation_summary.rds")
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(out) || nrow(out) == 0) return(NULL)
  if (!"series" %in% names(out)) out$series <- "all"
  out$scenario <- scenario
  out$model <- model
  out$replicate <- stp_rep_id(scenario)
  out
}

stp_series_fishery <- function(series) {
  out <- suppressWarnings(as.integer(sub("^fishery_", "", as.character(series))))
  ifelse(is.finite(out), out, NA_integer_)
}

stp_fishery_region <- function(fishery_num, mapping = NULL) {
  key <- as.character(fishery_num)
  if (is.data.frame(mapping) && all(c("fishery", "region") %in% names(mapping))) {
    idx <- which(as.character(mapping$fishery) == key)
    if (length(idx) > 0) {
      region <- mapping$region[[idx[[1]]]]
      if (!is.null(region) && length(region) > 0 && !is.na(region) && nzchar(as.character(region))) {
        return(as.character(region))
      }
    }
  }
  if (is.finite(suppressWarnings(as.numeric(fishery_num)))) paste0("fishery ", fishery_num) else "unknown"
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
    catch_conditioned = get_lgl("catch_conditioned"),
    effort_conditioned = get_lgl("effort_conditioned"),
    update_catch = get_lgl("update_catch"),
    update_effort = get_lgl("update_effort"),
    catch_replaced_rows = get_num("catch_replaced_rows"),
    effort_replaced_rows = get_num("effort_replaced_rows"),
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
      show_replicates = isTRUE(input$selftest_show_replicates),
      show_interval = isTRUE(input$selftest_show_interval),
      interval_level = {
        lvl <- suppressWarnings(as.numeric(input$selftest_interval_level))
        if (length(lvl) == 0 || !is.finite(lvl[1])) 0.95 else max(0.01, min(0.99, lvl[1]))
      },
      recovery_height = if (is.null(input$selftest_recovery_height)) 900 else suppressWarnings(as.integer(input$selftest_recovery_height)),
      sim_height = if (is.null(input$selftest_sim_height)) 850 else suppressWarnings(as.integer(input$selftest_sim_height)),
      sim_components = if (is.null(input$selftest_sim_components) || length(input$selftest_sim_components) == 0) {
        c("catch total", "CPUE", "effort", "length mean", "weight mean", "tag recaptures", "age-length mean age")
      } else {
        as.character(input$selftest_sim_components)
      }
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
    list(input$live_update_plots, input$selftest_scenarios,
         input$selftest_show_replicates, input$selftest_show_interval,
         input$selftest_interval_level,
         input$selftest_recovery_height, input$selftest_sim_height,
         input$selftest_sim_components),
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
      refit_ts <- stp_extract_all_metrics(stp_read_model_rep(row$refit_dir[[1]]))
      truth_ts <- stp_extract_all_metrics(stp_read_model_rep(row$truth_dir[[1]]))

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
      stp_read_data_simulation_summary(row$input_dir[[1]], row$key[[1]], row$model[[1]])
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
        stp_read_run_diagnostics(row$truth_dir[[1]], "source_truth", row$key[[1]], row$replicate[[1]]),
        stp_read_run_diagnostics(row$truth_eval_dir[[1]], "truth_on_pseudo", row$key[[1]], row$replicate[[1]]),
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
    interval_level <- if (!is.null(filters$interval_level)) filters$interval_level else 0.95
    truth_med <- stp_summary_band(truth, interval_level)
    refit_band <- stp_summary_band(refit, interval_level)
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
      facet_wrap(~ quantity, scales = "free_y", ncol = 2) +
      labs(
        x = "Year",
        y = NULL,
        subtitle = paste0("Red = truth, black = refit median, grey = refit replicate traces; shaded = ", round(interval_level * 100), "% interval")
      ) +
      theme_bw(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = "#555")
      )
  })

  selftest_sim_plot <- reactive({
    filters <- selftest_filters_applied()
    req(filters)
    info <- selftest_sim_data()
    if (nrow(info) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No pseudo-data build diagnostics found", size = 5, color = "#777") +
          theme_void()
      )
    }

    data_series <- selftest_data_simulation_series()

    if (nrow(data_series) > 0) {
      sim_components <- if (is.null(filters$sim_components) || length(filters$sim_components) == 0) {
        unique(as.character(data_series$component))
      } else {
        as.character(filters$sim_components)
      }
      annual <- data_series %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          series = if ("series" %in% names(.)) as.character(series) else "all",
          model = if ("model" %in% names(.)) as.character(model) else "",
          base_value = suppressWarnings(as.numeric(base_value)),
          pseudo_value = suppressWarnings(as.numeric(pseudo_value))
        ) %>%
        filter(
          component %in% sim_components,
          is.finite(year), is.finite(base_value), is.finite(pseudo_value)
        )
      if (nrow(annual) == 0) {
        p_data <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No selected simulation data series found", size = 4.2, color = "#777") +
          theme_void()
        return(p_data)
      }
      annual <- bind_rows(lapply(split(annual, seq_len(nrow(annual))), function(row) {
        fishery_components <- c("catch total", "CPUE", "effort", "length mean", "weight mean")
        if (!row$component[[1]] %in% fishery_components) {
          row$display_component <- row$component[[1]]
          return(row)
        }
        fishery <- stp_series_fishery(row$series[[1]])
        map <- tryCatch(rv$FISHERY_MAPS[[row$model[[1]]]], error = function(e) NULL)
        region <- stp_fishery_region(fishery, map)
        row$display_component <- paste0(row$component[[1]], " - ", region)
        row
      }))
      annual <- annual %>%
        group_by(scenario, replicate, display_component, year) %>%
        summarise(
          base_value = mean(base_value, na.rm = TRUE),
          pseudo_value = mean(pseudo_value, na.rm = TRUE),
          .groups = "drop"
        )
      interval_level <- if (!is.null(filters$interval_level)) filters$interval_level else 0.95
      lower_prob <- (1 - interval_level) / 2
      upper_prob <- 1 - lower_prob
      pseudo_band <- annual %>%
        group_by(display_component, year) %>%
        summarise(
          median = stats::median(pseudo_value, na.rm = TRUE),
          lower = stats::quantile(pseudo_value, lower_prob, na.rm = TRUE, names = FALSE),
          upper = stats::quantile(pseudo_value, upper_prob, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
      base_line <- annual %>%
        group_by(display_component, year) %>%
        summarise(base_value = stats::median(base_value, na.rm = TRUE), .groups = "drop")
      p_data <- ggplot() +
        geom_ribbon(data = pseudo_band, aes(x = year, ymin = lower, ymax = upper), fill = "#95a5a6", alpha = 0.18) +
        geom_line(data = annual, aes(x = year, y = pseudo_value, group = scenario), color = "#718093", alpha = 0.20, linewidth = 0.32) +
        geom_line(data = pseudo_band, aes(x = year, y = median), color = "#111111", linewidth = 0.85) +
        geom_line(data = base_line, aes(x = year, y = base_value), color = "#d62728", alpha = 0.78, linewidth = 0.65) +
        facet_wrap(~ display_component, scales = "free_y", ncol = 2) +
        labs(
          x = "Year",
          y = "Annual value",
          subtitle = paste0("Data simulation check: red = fitted expectation, grey = pseudo reps, black = pseudo median; shaded = ", round(interval_level * 100), "% interval")
        ) +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "#eef3f7", color = NA))
      p_data
    } else {
      p_missing <- ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = "No pseudo-data value summary found. Re-run or retrieve self-test outputs created after this update.",
          size = 4.2, color = "#777"
        ) +
        theme_void()
      p_missing
    }
  })

  output$selftest_recovery_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_recovery_height)) 900 else input$selftest_recovery_height
    plotOutput("selftest_recovery_plot", height = paste0(h, "px"))
  })

  output$selftest_sim_plot_ui <- renderUI({
    h <- if (is.null(input$selftest_sim_height)) 850 else input$selftest_sim_height
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
        catch_conditioned,
        effort_conditioned,
        update_catch,
        update_effort,
        catch_replaced_rows,
        effort_replaced_rows,
        cpue_replaced_rows,
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
        select(scenario, source, obj_fun, tag_lik, mn_len_pen, max_grad, exit_status, run_completed, converged) %>%
        tidyr::pivot_wider(
          names_from = source,
          values_from = c(obj_fun, tag_lik, mn_len_pen, max_grad, exit_status, run_completed, converged),
          names_glue = "{source}_{.value}"
        )
      for (nm in c(
        "refit_obj_fun", "refit_tag_lik", "refit_mn_len_pen", "refit_max_grad", "refit_converged", "refit_exit_status",
        "truth_on_pseudo_obj_fun", "truth_on_pseudo_tag_lik", "truth_on_pseudo_mn_len_pen", "truth_on_pseudo_max_grad",
        "source_truth_obj_fun", "source_truth_tag_lik", "source_truth_mn_len_pen", "source_truth_max_grad"
      )) {
        if (!nm %in% names(conv_wide)) conv_wide[[nm]] <- NA
      }
      out <- out %>%
        left_join(conv_wide, by = "scenario") %>%
        select(
          scenario, replicate,
          refit_obj_fun, refit_tag_lik, refit_mn_len_pen, refit_max_grad, refit_converged, refit_exit_status,
          truth_on_pseudo_obj_fun, truth_on_pseudo_tag_lik, truth_on_pseudo_mn_len_pen, truth_on_pseudo_max_grad,
          source_truth_obj_fun, source_truth_tag_lik, source_truth_mn_len_pen, source_truth_max_grad,
          everything()
        )
    }
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}
