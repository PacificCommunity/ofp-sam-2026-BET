mod_sections_download <- function(prefix, title, plot_reactive, input, session, output) {
  observeEvent(input[[paste0("show_", prefix, "_download_modal")]], {
    show_download_modal(prefix, title, current_save_dir = input$plot_export_dir)
  })

  observeEvent(input[[paste0(prefix, "_preset_wide")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 16)
    updateNumericInput(session, paste0(prefix, "_height"), value = 9)
  })
  observeEvent(input[[paste0(prefix, "_preset_standard")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 12)
    updateNumericInput(session, paste0(prefix, "_height"), value = 9)
  })
  observeEvent(input[[paste0(prefix, "_preset_square")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 10)
    updateNumericInput(session, paste0(prefix, "_height"), value = 10)
  })

  output[[paste0(prefix, "_download_confirm")]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_", Sys.Date(), ".", input[[paste0(prefix, "_format")]])
    },
    content = function(file) {
      p <- plot_reactive()
      width <- input[[paste0(prefix, "_width")]]
      height <- input[[paste0(prefix, "_height")]]
      dpi <- as.numeric(input[[paste0(prefix, "_dpi")]])
      format <- input[[paste0(prefix, "_format")]]
      save_plot_with_format(p, file, width = width, height = height, dpi = dpi, format = format)
      removeModal()
    }
  )

  register_folder_save_button(
    plot_type = prefix,
    plot_reactive = plot_reactive,
    input = input,
    session = session,
    output = output,
    filename_fun = function() paste0(prefix, "_", Sys.Date(), ".", input[[paste0(prefix, "_format")]])
  )
}

subset_named <- function(x, keys) {
  if (is.null(x)) return(list())
  x[keys]
}

selected_window <- function(par_list) {
  minYear <- max(sapply(par_list, function(p) as.numeric(p@range["minyear"])), na.rm = TRUE)
  maxYear <- min(sapply(par_list, function(p) as.numeric(p@range["maxyear"])), na.rm = TRUE)
  if (!is.finite(minYear) || !is.finite(maxYear) || minYear > maxYear) {
    minYear <- min(sapply(par_list, function(p) as.numeric(p@range["minyear"])), na.rm = TRUE)
    maxYear <- max(sapply(par_list, function(p) as.numeric(p@range["maxyear"])), na.rm = TRUE)
  }
  list(minYear = minYear, maxYear = maxYear)
}

weighted_quantile <- function(x, w = NULL, probs = 0.5) {
  keep <- is.finite(x)
  if (!is.null(w)) keep <- keep & is.finite(w)
  x <- x[keep]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))

  if (is.null(w)) {
    return(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)))
  }

  w <- w[keep]
  positive <- w > 0
  x <- x[positive]
  w <- w[positive]

  if (length(x) == 0 || sum(w, na.rm = TRUE) <= 0) {
    w <- rep(1, length(x))
  }

  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  if (length(x) == 1) return(rep(x, length(probs)))

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  total_w <- sum(w)
  if (!is.finite(total_w) || total_w <= 0) {
    return(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)))
  }
  cum_w <- cumsum(w)
  mid_p <- (cum_w - 0.5 * w) / total_w
  stats::approx(x = mid_p, y = x, xout = probs, method = "linear", rule = 2, ties = "ordered")$y
}

weighted_mean_safe <- function(x, w = NULL) {
  keep <- is.finite(x)
  if (!is.null(w)) keep <- keep & is.finite(w)
  x <- x[keep]
  if (length(x) == 0) return(NA_real_)

  if (is.null(w)) return(mean(x, na.rm = TRUE))

  w <- w[keep]
  positive <- w > 0
  x <- x[positive]
  w <- w[positive]
  if (length(x) == 0) return(NA_real_)

  total_w <- sum(w, na.rm = TRUE)
  if (!is.finite(total_w) || total_w <= 0) return(mean(x, na.rm = TRUE))
  sum(x * w, na.rm = TRUE) / total_w
}

compute_ensemble_weights <- function(par_list, method = "equal") {
  scenario_names <- names(par_list)
  equal_weights <- rep(1 / length(scenario_names), length(scenario_names))
  names(equal_weights) <- scenario_names
  aic_values <- vapply(par_list, function(par_obj) {
    obj_fun <- suppressWarnings(tryCatch(as.numeric(par_obj@obj_fun), error = function(e) NA_real_))
    n_pars <- suppressWarnings(tryCatch(as.numeric(par_obj@n_pars), error = function(e) NA_real_))
    if (!is.finite(obj_fun) || !is.finite(n_pars)) return(NA_real_)
    2 * n_pars + 2 * obj_fun
  }, numeric(1))
  delta_aic <- aic_values - min(aic_values, na.rm = TRUE)
  rel_likelihood <- exp(-0.5 * delta_aic)

  if (!identical(method, "aic")) {
    return(tibble(
      scenario = scenario_names,
      weight = equal_weights,
      aic = as.numeric(aic_values),
      delta_aic = as.numeric(delta_aic),
      rel_likelihood = as.numeric(rel_likelihood),
      method_used = "equal"
    ))
  }

  if (!all(is.finite(aic_values))) {
    return(tibble(
      scenario = scenario_names,
      weight = equal_weights,
      aic = as.numeric(aic_values),
      delta_aic = as.numeric(delta_aic),
      rel_likelihood = as.numeric(rel_likelihood),
      method_used = "equal"
    ))
  }

  aic_weights <- rel_likelihood
  if (sum(aic_weights, na.rm = TRUE) <= 0) {
    aic_weights <- equal_weights
    method_used <- "equal"
  } else {
    aic_weights <- aic_weights / sum(aic_weights)
    method_used <- "aic"
  }

  tibble(
    scenario = scenario_names,
    weight = as.numeric(aic_weights),
    aic = as.numeric(aic_values),
    delta_aic = as.numeric(delta_aic),
    rel_likelihood = as.numeric(rel_likelihood),
    method_used = method_used
  )
}

compute_manual_weights <- function(scenarios, input_weights = NULL) {
  n <- length(scenarios)
  base_weights <- rep(1, n)
  names(base_weights) <- scenarios

  if (!is.null(input_weights)) {
    matched <- input_weights[scenarios]
    keep <- is.finite(matched)
    base_weights[keep] <- matched[keep]
  }

  base_weights[!is.finite(base_weights) | base_weights < 0] <- 0
  total_weight <- sum(base_weights, na.rm = TRUE)
  if (!is.finite(total_weight) || total_weight <= 0) {
    base_weights <- rep(1, n)
    names(base_weights) <- scenarios
    total_weight <- sum(base_weights)
  }

  tibble(
    scenario = scenarios,
    weight_input = as.numeric(base_weights),
    weight = as.numeric(base_weights / total_weight),
    aic = NA_real_,
    delta_aic = NA_real_,
    rel_likelihood = NA_real_,
    method_used = "manual"
  )
}

summarise_ensemble <- function(df, group_cols, value_col, interval = 0.95, weight_col = NULL, center_method = "quantile") {
  tail_prob <- (1 - interval) / 2
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      median = if (identical(center_method, "mean")) {
        weighted_mean_safe(.data[[value_col]], if (!is.null(weight_col)) .data[[weight_col]] else NULL)
      } else {
        weighted_quantile(.data[[value_col]], if (!is.null(weight_col)) .data[[weight_col]] else NULL, probs = 0.5)
      },
      lower = weighted_quantile(.data[[value_col]], if (!is.null(weight_col)) .data[[weight_col]] else NULL, probs = tail_prob),
      upper = weighted_quantile(.data[[value_col]], if (!is.null(weight_col)) .data[[weight_col]] else NULL, probs = 1 - tail_prob),
      .groups = "drop"
    )
}

build_ensemble_bands <- function(df, group_cols, value_col, intervals = c(0.95, 0.8, 0.5), weight_col = NULL, center_method = "quantile") {
  bind_rows(lapply(intervals, function(interval) {
    summarise_ensemble(df, group_cols = group_cols, value_col = value_col, interval = interval, weight_col = weight_col, center_method = center_method) %>%
      mutate(interval = interval)
  }))
}

standardize_year_df <- function(df) {
  if (is.null(df)) return(NULL)
  year_name <- names(df)[tolower(names(df)) == "year"][1]
  if (is.na(year_name) || !nzchar(year_name)) return(NULL)
  df %>%
    rename(year = all_of(year_name)) %>%
    mutate(year = suppressWarnings(as.numeric(as.character(year))))
}

extract_reference_slice <- function(mfcl_array_obj) {
  full_array <- tryCatch(mfcl_array_obj@.Data, error = function(e) NULL)
  if (is.null(full_array) || length(dim(full_array)) < 6) {
    return(NULL)
  }

  years <- tryCatch(dimnames(full_array)[[2]], error = function(e) NULL)
  years <- suppressWarnings(as.numeric(years))
  if (length(years) != dim(full_array)[2]) {
    years <- seq_len(dim(full_array)[2])
  }

  sliced <- tryCatch(full_array[1, , 1, , , 1, drop = FALSE], error = function(e) NULL)
  if (is.null(sliced)) {
    return(NULL)
  }

  list(values = sliced, years = years)
}

collapse_reference_biomass <- function(mfcl_array_obj, scale = 1000) {
  sliced <- extract_reference_slice(mfcl_array_obj)
  if (is.null(sliced)) {
    return(NULL)
  }

  vals <- vapply(seq_along(sliced$years), function(i) {
    year_slice <- sliced$values[1, i, 1, , , 1, drop = FALSE]
    season_n <- dim(year_slice)[4]
    area_n <- dim(year_slice)[5]
    year_matrix <- matrix(as.numeric(year_slice), nrow = season_n, ncol = area_n)
    sum(colMeans(year_matrix, na.rm = TRUE), na.rm = TRUE) / scale
  }, numeric(1))

  tibble(year = sliced$years, value = as.numeric(vals))
}

collapse_reference_mean <- function(mfcl_array_obj) {
  sliced <- extract_reference_slice(mfcl_array_obj)
  if (is.null(sliced)) {
    return(NULL)
  }

  dims <- dim(sliced$values)
  vals <- vapply(seq_along(sliced$years), function(i) {
    year_slice <- sliced$values[1, i, 1, , , 1, drop = FALSE]
    mean(as.numeric(year_slice), na.rm = TRUE)
  }, numeric(1))

  tibble(year = sliced$years, value = as.numeric(vals))
}

collapse_annual_f <- function(rep_obj) {
  agg_f_year <- tryCatch(collapse_reference_mean(AggregateF(rep_obj)), error = function(e) NULL)
  if (is.null(agg_f_year)) {
    return(NULL)
  }

  agg_f_year %>%
    rename(annual_f = value) %>%
    filter(is.finite(year), is.finite(annual_f)) %>%
    arrange(year)
}

extract_kobe_series <- function(rep_obj, scenario_name) {
  ffmsy_year <- tryCatch(collapse_reference_mean(FFMSY_ts(rep_obj)), error = function(e) NULL)
  abbmsy_year <- tryCatch(collapse_reference_mean(ABBMSY_ts(rep_obj)), error = function(e) NULL)
  bio_fish_year <- tryCatch(collapse_reference_biomass(adultBiomass(rep_obj)), error = function(e) NULL)
  bio_nofish_year <- tryCatch(collapse_reference_biomass(adultBiomass_nofish(rep_obj)), error = function(e) NULL)
  annual_f_year <- tryCatch(collapse_annual_f(rep_obj), error = function(e) NULL)

  if (is.null(ffmsy_year) || is.null(abbmsy_year) || is.null(bio_fish_year) || is.null(bio_nofish_year)) {
    return(NULL)
  }

  ffmsy_year %>%
    rename(f_fmsy = value) %>%
    inner_join(abbmsy_year, by = "year") %>%
    rename(sb_sbmsy = value) %>%
    inner_join(bio_fish_year, by = "year") %>%
    rename(bio_fish = value) %>%
    inner_join(bio_nofish_year, by = "year") %>%
    rename(bio_nofish = value) %>%
    mutate(dep = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
    left_join(annual_f_year, by = "year") %>%
    mutate(
      f_msy = ifelse(is.finite(annual_f) & is.finite(f_fmsy) & f_fmsy > 0, annual_f / f_fmsy, NA_real_),
      sb_msy = ifelse(is.finite(bio_fish) & is.finite(sb_sbmsy) & sb_sbmsy > 0, bio_fish / sb_sbmsy, NA_real_)
    ) %>%
    filter(is.finite(f_fmsy), is.finite(sb_sbmsy)) %>%
    arrange(year) %>%
    mutate(scenario = scenario_name)
}

extract_majuro_series <- function(rep_obj, scenario_name) {
  ffmsy_year <- tryCatch(collapse_reference_mean(FFMSY_ts(rep_obj)), error = function(e) NULL)
  bio_fish_year <- tryCatch(collapse_reference_biomass(adultBiomass(rep_obj)), error = function(e) NULL)
  bio_nofish_year <- tryCatch(collapse_reference_biomass(adultBiomass_nofish(rep_obj)), error = function(e) NULL)
  annual_f_year <- tryCatch(collapse_annual_f(rep_obj), error = function(e) NULL)

  if (is.null(ffmsy_year) || is.null(bio_fish_year) || is.null(bio_nofish_year)) {
    return(NULL)
  }

  ffmsy_year %>%
    rename(f_fmsy = value) %>%
    inner_join(bio_fish_year, by = "year") %>%
    rename(bio_fish = value) %>%
    inner_join(bio_nofish_year, by = "year") %>%
    rename(bio_nofish = value) %>%
    mutate(dep = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
    left_join(annual_f_year, by = "year") %>%
    mutate(
      f_msy = ifelse(is.finite(annual_f) & is.finite(f_fmsy) & f_fmsy > 0, annual_f / f_fmsy, NA_real_)
    ) %>%
    filter(is.finite(f_fmsy), is.finite(dep)) %>%
    arrange(year) %>%
    mutate(scenario = scenario_name)
}

default_recent_year_window <- function(years, n_years = 4) {
  years <- sort(unique(as.integer(years[is.finite(years)])))
  if (length(years) == 0) {
    return(c(0L, 0L))
  }

  n_years <- max(1L, min(as.integer(n_years), length(years)))
  c(years[length(years) - n_years + 1], years[length(years)])
}

summarise_recent_reference <- function(data, x_col, y_col, year_range, diagnostic_model = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(points = data.frame(), median_point = data.frame(), mean_point = data.frame(), years = integer(0)))
  }

  year_min <- min(year_range, na.rm = TRUE)
  year_max <- max(year_range, na.rm = TRUE)

  recent_data <- data %>%
    filter(is.finite(year), year >= year_min, year <= year_max)

  if (nrow(recent_data) == 0) {
    return(list(points = data.frame(), median_point = data.frame(), mean_point = data.frame(), years = integer(0)))
  }

  points <- recent_data %>%
    group_by(scenario) %>%
    summarise(
      x = mean(.data[[x_col]], na.rm = TRUE),
      y = mean(.data[[y_col]], na.rm = TRUE),
      year_start = min(year, na.rm = TRUE),
      year_end = max(year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(x), is.finite(y))

  if (nrow(points) == 0) {
    return(list(points = data.frame(), median_point = data.frame(), mean_point = data.frame(), years = integer(0)))
  }

  if (!is.null(diagnostic_model) && nzchar(diagnostic_model)) {
    diag_idx <- points$scenario == diagnostic_model
  } else {
    diag_idx <- grepl("^2023diag$", points$scenario, ignore.case = TRUE) |
      grepl("diag", points$scenario, ignore.case = TRUE)
  }
  points <- points %>%
    mutate(
      point_role = case_when(
        diag_idx ~ "Diagnostic model",
        TRUE ~ "Model"
      )
    )

  median_point <- tibble(
    x = median(points$x, na.rm = TRUE),
    y = median(points$y, na.rm = TRUE),
    point_role = "Median"
  )

  mean_point <- tibble(
    x = mean(points$x, na.rm = TRUE),
    y = mean(points$y, na.rm = TRUE),
    point_role = "Mean"
  )

  list(
    points = points,
    median_point = median_point,
    mean_point = mean_point,
    years = sort(unique(recent_data$year))
  )
}

reference_axis_limits <- function(values, threshold, hard_min = 0, hard_max = NULL,
                                  lower_pad_frac = 0.08, upper_pad_frac = 0.08,
                                  min_span = 0.6, min_below_threshold = 0.2,
                                  min_above_threshold = 0.2) {
  vals <- values[is.finite(values)]
  if (length(vals) == 0) {
    upper_default <- if (is.null(hard_max)) threshold + min_span else hard_max
    return(c(hard_min, upper_default))
  }

  lower_base <- min(c(vals, threshold), na.rm = TRUE)
  upper_base <- max(c(vals, threshold), na.rm = TRUE)
  span <- max(upper_base - lower_base, min_span)

  lower <- lower_base - span * lower_pad_frac
  upper <- upper_base + span * upper_pad_frac

  lower <- min(lower, threshold - min_below_threshold)
  upper <- max(upper, threshold + min_above_threshold)

  if (!is.null(hard_min)) {
    lower <- max(lower, hard_min)
  }
  if (!is.null(hard_max)) {
    upper <- min(upper, hard_max)
  }

  if (upper <= lower) {
    upper <- lower + min_span
    if (!is.null(hard_max)) {
      upper <- min(upper, hard_max)
      lower <- min(lower, upper - min_span / 2)
    }
  }

  c(lower, upper)
}

mod_harvest_ui <- function() {
  tabItem(
    tabName = "harvest",
    h2("Key Quantities", style = "color: #00a65a;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "success",
        pickerInput("harvest_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("harvest_plot", "Plot:", choices = c(
          "Depletion/Recruitment/SP/F (combined)" = "spawning",
          "Depletion by Area" = "depletion_area",
          "Recruitment by Area" = "rec_area",
          "Kobe Plot" = "kobe",
          "Majuro Plot" = "majuro",
          "Juvenile & Adult F by Area" = "fm_juv_adult",
          "Area Contribution to Total F" = "fm_area_contrib",
          "Spawning Potential (with/without fishing)" = "sp_combined",
          "Total Biomass (with/without fishing)" = "tb_combined"
        )),
        conditionalPanel(
          condition = "input.harvest_plot == 'spawning' || input.harvest_plot == 'depletion_area' || input.harvest_plot == 'rec_area'",
          checkboxInput("harvest_ensemble", "Ensemble", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.harvest_ensemble && (input.harvest_plot == 'spawning' || input.harvest_plot == 'depletion_area' || input.harvest_plot == 'rec_area')",
          selectInput(
            "harvest_ensemble_weighting",
              "Weighting:",
            choices = c("Equal" = "equal", "AIC" = "aic", "Manual" = "manual"),
            selected = "equal"
          ),
          selectInput(
            "harvest_ensemble_center",
            "Center line:",
            choices = c("Weighted quantile" = "quantile", "Weighted mean" = "mean"),
            selected = "quantile"
          ),
          checkboxGroupInput(
            "harvest_ensemble_levels",
            "Ensemble bands:",
            choices = c("95%" = "0.95", "80%" = "0.80", "50%" = "0.50"),
            selected = c("0.95", "0.80", "0.50")
          )
        ),
        conditionalPanel(
          condition = "input.harvest_plot == 'kobe' || input.harvest_plot == 'majuro'",
          selectInput(
            "harvest_reference_view",
            "Reference view:",
            choices = c("Trajectory" = "trajectory", "Period-average summary" = "recent"),
            selected = "trajectory"
          ),
          uiOutput("harvest_recent_years_ui"),
          uiOutput("harvest_recent_diag_model_ui"),
          uiOutput("harvest_recent_summary_options_ui")
        ),
        selectInput("harvest_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "2"),
        sliderInput(
          "harvest_plot_height",
          "Plot height (px)",
          min = 450,
          max = 1800,
          value = 900,
          step = 50
        ),
        sliderInput(
          "harvest_plot_width",
          "Plot width (px)",
          min = 700,
          max = 2200,
          value = 1200,
          step = 50
        ),
        actionButton("harvest_apply_filters", "Apply", class = "btn-success", style = "width: 100%;"),
        tags$small("Selections update the plot when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_harvest_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(
        title = "Plot",
        width = 9,
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "harvest_plot_output",
          uiOutput("harvest_plot_output_ui"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering key quantities plot...")
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 9, offset = 3,
        conditionalPanel(
          condition = "input.harvest_ensemble && (input.harvest_ensemble_weighting == 'aic' || input.harvest_ensemble_weighting == 'manual')",
          box(
            title = "Ensemble Weights",
            width = NULL,
            solidHeader = TRUE,
            status = "success",
            collapsible = TRUE,
            collapsed = TRUE,
            uiOutput("harvest_weighting_note"),
            conditionalPanel(
              condition = "input.harvest_ensemble_weighting == 'manual'",
              actionButton("harvest_manual_weights_apply", "Apply", class = "btn-success", style = "margin-bottom: 12px;")
            ),
            DTOutput("harvest_weight_table")
          )
        ),
        conditionalPanel(
          condition = "input.harvest_ensemble",
          box(
            title = "Ensemble Method Guide",
            width = NULL,
            solidHeader = TRUE,
            status = "success",
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("harvest_method_table")
          )
        ),
        conditionalPanel(
          condition = "input.harvest_plot == 'kobe' || input.harvest_plot == 'majuro'",
          box(
            title = "Reference Values",
            width = NULL,
            solidHeader = TRUE,
            status = "success",
            collapsible = TRUE,
            collapsed = TRUE,
            uiOutput("harvest_reference_note"),
            uiOutput("harvest_reference_model_ui"),
            DTOutput("harvest_reference_table")
          )
        )
      )
    )
  )
}

mod_harvest_server <- function(input, output, session, rv) {
  harvest_live_update_nonce <- reactiveVal(0)
  harvest_filters_current <- reactive({
    list(
      scenarios = input$harvest_scenarios,
      plot = if (is.null(input$harvest_plot)) "depletion_rec" else input$harvest_plot,
      reference_view = if (is.null(input$harvest_reference_view)) "trajectory" else input$harvest_reference_view,
      recent_years = input$harvest_recent_years,
      recent_diag_model = input$harvest_recent_diag_model,
      recent_summary_options = input$harvest_recent_summary_options,
      facet_ncol = input$harvest_facet_ncol,
      ensemble = isTRUE(input$harvest_ensemble),
      ensemble_levels = input$harvest_ensemble_levels,
      ensemble_weighting = if (is.null(input$harvest_ensemble_weighting)) "equal" else input$harvest_ensemble_weighting,
      ensemble_center = if (is.null(input$harvest_ensemble_center)) "quantile" else input$harvest_ensemble_center,
      plot_height = if (is.null(input$harvest_plot_height)) 900 else suppressWarnings(as.integer(input$harvest_plot_height)),
      plot_width = if (is.null(input$harvest_plot_width)) 1200 else suppressWarnings(as.integer(input$harvest_plot_width))
    )
  })
  harvest_filters_applied <- reactiveVal(NULL)
  harvest_last_initialized_nonce <- reactiveVal(0)
  observeEvent(rv$data_loaded, {
    sc <- names(rv$ParOut_list)
    updatePickerInput(session, "harvest_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(harvest_filters_current(), harvest_filters_applied())
    set_apply_pending(session, "harvest_apply_filters", pending)
  })

  manual_weight_tbl <- reactiveVal(NULL)
  applied_manual_weight_tbl <- reactiveVal(NULL)

  observeEvent(list(rv$data_loaded, input$harvest_scenarios), {
    req(rv$data_loaded)
    scenarios <- input$harvest_scenarios
    if (is.null(scenarios) || length(scenarios) == 0) {
      manual_weight_tbl(NULL)
      applied_manual_weight_tbl(NULL)
      return()
    }

    existing <- manual_weight_tbl()
    existing_weights <- NULL
    if (!is.null(existing) && nrow(existing) > 0) {
      existing_weights <- existing$weight_input
      names(existing_weights) <- existing$scenario
    }
    updated_tbl <- compute_manual_weights(scenarios, existing_weights)
    manual_weight_tbl(updated_tbl)

    existing_applied <- applied_manual_weight_tbl()
    applied_weights <- NULL
    if (!is.null(existing_applied) && nrow(existing_applied) > 0) {
      applied_weights <- existing_applied$weight_input
      names(applied_weights) <- existing_applied$scenario
    }
    applied_manual_weight_tbl(compute_manual_weights(scenarios, applied_weights))
  }, ignoreInit = FALSE)

  observeEvent(input$harvest_weight_table_cell_edit, {
    req(identical(input$harvest_ensemble_weighting, "manual"))
    info <- input$harvest_weight_table_cell_edit
    tbl <- manual_weight_tbl()
    req(!is.null(tbl), info$col == 1)

    new_value <- suppressWarnings(as.numeric(info$value))
    if (!is.finite(new_value)) new_value <- tbl$weight_input[info$row]
    tbl$weight_input[info$row] <- max(new_value, 0)
    manual_weight_tbl(compute_manual_weights(tbl$scenario, setNames(tbl$weight_input, tbl$scenario)))
  })

  observeEvent(input$harvest_manual_weights_apply, {
    req(identical(input$harvest_ensemble_weighting, "manual"))
    tbl <- manual_weight_tbl()
    req(!is.null(tbl), nrow(tbl) > 0)
    applied_manual_weight_tbl(tbl)
  })

  ensemble_weights_reactive <- reactive({
    req(rv$data_loaded, input$harvest_scenarios)
    scenarios <- input$harvest_scenarios
    validate(need(length(scenarios) > 0, "No models selected"))
    ParOut_list <- subset_named(rv$ParOut_list, scenarios)
    method <- if (is.null(input$harvest_ensemble_weighting)) "equal" else input$harvest_ensemble_weighting

    if (identical(method, "manual")) {
      tbl <- applied_manual_weight_tbl()
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(compute_manual_weights(scenarios))
      }
      return(tbl %>% filter(scenario %in% scenarios))
    }

    compute_ensemble_weights(ParOut_list, method = method)
  })

  applied_manual_weight_key <- reactive({
    tbl <- applied_manual_weight_tbl()
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(NULL)
    }
    tbl %>%
      transmute(scenario, weight_input, weight)
  })

  harvest_plot_reactive <- reactive({
    req(rv$data_loaded, input$harvest_scenarios)
    scenarios_name <- input$harvest_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    yw <- selected_window(ParOut_list)
    minYear <- yw$minYear
    maxYear <- yw$maxYear

    config <- list(linewidth = 1.2, alpha = 0.9)
    ensemble_style <- list(
      line = "#003049",
      fill = c("0.95" = "#005f73", "0.80" = "#005f73", "0.50" = "#005f73"),
      alpha = c("0.95" = 0.22, "0.80" = 0.26, "0.50" = 0.34)
    )
    facet_ncol <- suppressWarnings(as.integer(input$harvest_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
    facet_ncol <- min(max(facet_ncol, 1), 12)

    plot_type <- if (is.null(input$harvest_plot)) "spawning" else input$harvest_plot
    show_ensemble <- isTRUE(input$harvest_ensemble) && plot_type %in% c("spawning", "depletion_area", "rec_area")
    selected_intervals <- suppressWarnings(as.numeric(input$harvest_ensemble_levels))
    selected_intervals <- selected_intervals[is.finite(selected_intervals)]
    if (length(selected_intervals) == 0) selected_intervals <- c(0.95, 0.8, 0.5)
    selected_intervals <- sort(unique(selected_intervals), decreasing = TRUE)
    ensemble_weighting <- if (is.null(input$harvest_ensemble_weighting)) "equal" else input$harvest_ensemble_weighting
    ensemble_center <- if (is.null(input$harvest_ensemble_center)) "quantile" else input$harvest_ensemble_center
    ensemble_weights <- ensemble_weights_reactive()

    if (plot_type == "spawning") {
      SBdep <- tryCatch(GetQuantSimple(SBSBF0, RepOut_list, minYear, maxYear), error = function(e) {
        map_dfr(names(RepOut_list), function(sc) {
          rep_obj <- RepOut_list[[sc]]
          bio_fish <- safe_array_to_df(rep_obj@adultBiomass) %>%
            group_by(year, season) %>%
            summarise(bio_fish = sum(data, na.rm = TRUE), .groups = "drop")
          
          bio_nofish <- safe_array_to_df(rep_obj@adultBiomass_nofish) %>%
            group_by(year, season) %>%
            summarise(bio_nofish = sum(data, na.rm = TRUE), .groups = "drop")
          
          bio_fish %>%
            inner_join(bio_nofish, by = c("year", "season")) %>%
            mutate(
              Scenario = sc,
              Year = as.numeric(year),
              Quant = bio_fish / pmax(bio_nofish, .Machine$double.eps)
            ) %>%
            group_by(Scenario, Year) %>%
            summarise(Quant = mean(Quant, na.rm = TRUE), .groups = "drop")
        })
      })

      rec_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        rec_temp <- safe_array_to_df(RepOut_list[[i]]@rec_region) %>% mutate(scenario = scenarios_name[i])
        rec_all <- bind_rows(rec_all, rec_temp)
      }
      rec_total <- rec_all %>% group_by(year, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop")
      Rec <- rec_total %>% rename(Year = year, Quant = data, Scenario = scenario)

      bioFish_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }
      SpawnPot <- bioFish_all %>%
        group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>%
        group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>%
        rename(Year = year, Quant = data, Scenario = scenario)

      fm_all <- data.frame(); popn_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i])
        fm_all <- bind_rows(fm_all, fm_temp)
        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data)
        popn_all <- bind_rows(popn_all, popn_temp)
      }

      FM <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(catch = data * N) %>%
        group_by(year, season, scenario) %>%
        summarise(total_catch = sum(catch, na.rm = TRUE), total_N = sum(N, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate = total_catch / total_N, inst_F = -log(pmax(1 - harvest_rate, 0.001))) %>%
        group_by(year, scenario) %>% summarise(Quant = sum(inst_F, na.rm = TRUE), .groups = "drop") %>%
        rename(Year = year, Scenario = scenario)

      scenario_levels <- scenarios_name[scenarios_name %in% unique(as.character(SBdep$Scenario))]
      scenario_colors <- get_scenario_colors(scenario_levels)
      SBdep$Scenario <- factor(SBdep$Scenario, levels = scenario_levels)
      Rec$Scenario <- factor(Rec$Scenario, levels = scenario_levels)
      SpawnPot$Scenario <- factor(SpawnPot$Scenario, levels = scenario_levels)
      FM$Scenario <- factor(FM$Scenario, levels = scenario_levels)

      common_theme <- theme_bw() + theme(legend.position = "none")
      if (show_ensemble) {
        SBdep_ens <- SBdep %>%
          left_join(ensemble_weights, by = c("Scenario" = "scenario")) %>%
          build_ensemble_bands("Year", "Quant", selected_intervals, weight_col = "weight", center_method = ensemble_center)
        Rec_ens <- Rec %>%
          left_join(ensemble_weights, by = c("Scenario" = "scenario")) %>%
          build_ensemble_bands("Year", "Quant", selected_intervals, weight_col = "weight", center_method = ensemble_center)
        SpawnPot_ens <- SpawnPot %>%
          left_join(ensemble_weights, by = c("Scenario" = "scenario")) %>%
          build_ensemble_bands("Year", "Quant", selected_intervals, weight_col = "weight", center_method = ensemble_center)
        FM_ens <- FM %>%
          left_join(ensemble_weights, by = c("Scenario" = "scenario")) %>%
          build_ensemble_bands("Year", "Quant", selected_intervals, weight_col = "weight", center_method = ensemble_center)

        SBdepPlot <- ggplot(SBdep_ens, aes(x = Year, y = median)) +
          geom_ribbon(data = filter(SBdep_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(SBdep_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(SBdep_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(SBdep_ens, Year, median), linewidth = 1.2, color = ensemble_style$line) +
          labs(x = "Year", y = bquote(SB/SB["F=0"])) + coord_cartesian(ylim = c(0, 1)) +
          geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") + common_theme

        RecPlot <- ggplot(Rec_ens, aes(x = Year, y = median)) +
          geom_ribbon(data = filter(Rec_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(Rec_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(Rec_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(Rec_ens, Year, median), linewidth = 1.2, color = ensemble_style$line) +
          coord_cartesian(ylim = c(0, NA)) +
          labs(x = "Year", y = "Recruitment (Millions)") + common_theme

        SpawnPotPlot <- ggplot(SpawnPot_ens, aes(x = Year, y = median)) +
          geom_ribbon(data = filter(SpawnPot_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(SpawnPot_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(SpawnPot_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(SpawnPot_ens, Year, median), linewidth = 1.2, color = ensemble_style$line) +
          labs(x = "Year", y = bquote("Spawning Potential (" * 10^3 * " MT)")) + coord_cartesian(ylim = c(0, NA)) + common_theme

        FMPlot <- ggplot(FM_ens, aes(x = Year, y = median)) +
          geom_ribbon(data = filter(FM_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(FM_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(FM_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(FM_ens, Year, median), linewidth = 1.2, color = ensemble_style$line) +
          labs(x = "Year", y = "Annual Instantaneous F") + coord_cartesian(ylim = c(0, NA)) + common_theme
      } else {
        SBdepPlot <- ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
          labs(x = "Year", y = bquote(SB/SB["F=0"])) + coord_cartesian(ylim = c(0, 1)) +
          geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") + common_theme

        RecPlot <- ggplot(Rec, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) + coord_cartesian(ylim = c(0, NA)) +
          scale_color_manual(values = scenario_colors) + labs(x = "Year", y = "Recruitment (Millions)") + common_theme

        SpawnPotPlot <- ggplot(SpawnPot, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
          labs(x = "Year", y = bquote("Spawning Potential (" * 10^3 * " MT)")) + coord_cartesian(ylim = c(0, NA)) + common_theme

        FMPlot <- ggplot(FM, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
          labs(x = "Year", y = "Annual Instantaneous F") + coord_cartesian(ylim = c(0, NA)) + common_theme
      }

      combined_ncol <- min(max(as.integer(facet_ncol), 1), 4)
      combined_plot <- cowplot::plot_grid(
        SBdepPlot, RecPlot, SpawnPotPlot, FMPlot,
        ncol = combined_ncol,
        align = "v"
      )
      if (show_ensemble) {
        return(combined_plot)
      }

      scenario_legend <- cowplot::get_legend(
        ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )
      return(cowplot::plot_grid(combined_plot, scenario_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    if (plot_type == "depletion_area") {
      bioNoFish_all <- data.frame(); bioFish_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        bioNoFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass_nofish) %>% mutate(scenario = scenarios_name[i])
        bioNoFish_all <- bind_rows(bioNoFish_all, bioNoFish_temp)
      }
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }

      bioNoFish_yearly <- bioNoFish_all %>% group_by(year, area, scenario) %>% summarise(biomass_nofish = sum(data) / n_distinct(season), .groups = "drop")
      bioFish_yearly <- bioFish_all %>% group_by(year, area, scenario) %>% summarise(biomass_fish = sum(data) / n_distinct(season), .groups = "drop")

      depletion_area <- bioFish_yearly %>% left_join(bioNoFish_yearly, by = c("year", "area", "scenario")) %>% mutate(depletion = biomass_fish / biomass_nofish)
      bioNoFish_all_areas <- bioNoFish_all %>% group_by(year, season, scenario) %>% summarise(biomass_nofish = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(biomass_nofish = mean(biomass_nofish), .groups = "drop")
      bioFish_all_areas <- bioFish_all %>% group_by(year, season, scenario) %>% summarise(biomass_fish = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(biomass_fish = mean(biomass_fish), .groups = "drop")
      depletion_all <- bioFish_all_areas %>% left_join(bioNoFish_all_areas, by = c("year", "scenario")) %>% mutate(depletion = biomass_fish / biomass_nofish, area = "All")
      depletion_combined <- bind_rows(depletion_area, depletion_all)

      scenario_levels <- scenarios_name[scenarios_name %in% unique(as.character(depletion_combined$scenario))]
      scenario_colors <- get_scenario_colors(scenario_levels)
      depletion_combined$scenario <- factor(depletion_combined$scenario, levels = scenario_levels)

      if (show_ensemble) {
        depletion_ens <- depletion_combined %>%
          left_join(ensemble_weights, by = "scenario") %>%
          build_ensemble_bands(c("year", "area"), "depletion", selected_intervals, weight_col = "weight", center_method = ensemble_center)
        depletion_plot <- ggplot(depletion_ens, aes(x = year, y = median)) +
          geom_ribbon(data = filter(depletion_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(depletion_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(depletion_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(depletion_ens, year, area, median), linewidth = config$linewidth, color = ensemble_style$line) +
          geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
          coord_cartesian(ylim = c(0, 1)) +
          facet_wrap(~ area, ncol = facet_ncol) + labs(x = "Year", y = bquote(SB/SB["F=0"])) +
          theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))
        return(depletion_plot)
      }

      depletion_plot <- ggplot(depletion_combined, aes(x = year, y = depletion, color = scenario)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
        scale_color_manual(values = scenario_colors) + coord_cartesian(ylim = c(0, 1)) +
        facet_wrap(~ area, ncol = facet_ncol) + labs(x = "Year", y = bquote(SB/SB["F=0"])) +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(depletion_combined, aes(x = year, y = depletion, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )
      return(cowplot::plot_grid(depletion_plot, scenario_legend, ncol = 2, rel_widths = c(4, 0.8)))
    }

    if (plot_type == "rec_area") {
      rec_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        rec_temp <- safe_array_to_df(RepOut_list[[i]]@rec_region) %>% mutate(scenario = scenarios_name[i])
        rec_all <- bind_rows(rec_all, rec_temp)
      }

      rec_yearly <- rec_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop")
      rec_total <- rec_all %>% group_by(year, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop") %>% mutate(area = "All")
      rec_combined <- bind_rows(rec_yearly, rec_total)

      scenario_levels <- scenarios_name[scenarios_name %in% unique(as.character(rec_combined$scenario))]
      scenario_colors <- get_scenario_colors(scenario_levels)
      rec_combined$scenario <- factor(rec_combined$scenario, levels = scenario_levels)

      if (show_ensemble) {
        rec_ens <- rec_combined %>%
          left_join(ensemble_weights, by = "scenario") %>%
          build_ensemble_bands(c("year", "area"), "data", selected_intervals, weight_col = "weight", center_method = ensemble_center)
        rec_plot <- ggplot(rec_ens, aes(x = year, y = median)) +
          geom_ribbon(data = filter(rec_ens, interval == 0.95), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.95"]], alpha = ensemble_style$alpha[["0.95"]]) +
          geom_ribbon(data = filter(rec_ens, interval == 0.80), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.80"]], alpha = ensemble_style$alpha[["0.80"]]) +
          geom_ribbon(data = filter(rec_ens, interval == 0.50), aes(ymin = lower, ymax = upper), fill = ensemble_style$fill[["0.50"]], alpha = ensemble_style$alpha[["0.50"]]) +
          geom_line(data = distinct(rec_ens, year, area, median), linewidth = config$linewidth, color = ensemble_style$line) +
          coord_cartesian(ylim = c(0, NA)) + facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
          labs(x = "Year", y = "Recruitment (Millions)") + theme_bw() +
          theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))
        return(rec_plot)
      }

      rec_plot <- ggplot(rec_combined, aes(x = year, y = data, color = scenario)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) + scale_color_manual(values = scenario_colors) +
        coord_cartesian(ylim = c(0, NA)) + facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = "Recruitment (Millions)") + theme_bw() +
        theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(rec_combined, aes(x = year, y = data, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )
      return(cowplot::plot_grid(rec_plot, scenario_legend, ncol = 2, rel_widths = c(4, 0.8)))
    }

    if (plot_type == "kobe") {
      kobe_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_kobe_series(RepOut_list[[i]], scenarios_name[i])
      }))

      validate(need(nrow(kobe_data) > 0, "Kobe plot inputs are not available for the selected models"))

      kobe_data <- kobe_data %>%
        mutate(
          scenario = factor(scenario, levels = scenarios_name),
          year = as.numeric(year)
        ) %>%
        filter(is.finite(year), is.finite(sb_sbmsy), is.finite(f_fmsy))

      validate(need(nrow(kobe_data) > 0, "Kobe plot inputs are not available for the selected models"))

      x_limits <- reference_axis_limits(kobe_data$sb_sbmsy, threshold = 1, hard_min = 0, min_span = 0.75)
      y_limits <- reference_axis_limits(kobe_data$f_fmsy, threshold = 1, hard_min = 0, min_span = 0.75)
      x_min <- x_limits[1]
      x_max <- x_limits[2]
      y_min <- y_limits[1]
      y_max <- y_limits[2]

      if (identical(input$harvest_reference_view, "recent")) {
        recent_year_range <- input$harvest_recent_years
        if (is.null(recent_year_range) || length(recent_year_range) != 2) {
          recent_year_range <- default_recent_year_window(kobe_data$year, 4)
        }

        recent_summary <- summarise_recent_reference(
          kobe_data,
          "sb_sbmsy",
          "f_fmsy",
          recent_year_range,
          diagnostic_model = input$harvest_recent_diag_model
        )
        recent_points <- recent_summary$points
        median_point <- recent_summary$median_point
        mean_point <- recent_summary$mean_point

        validate(need(nrow(recent_points) > 0, "No Kobe summary values available for the selected years"))

        years_label <- paste0(min(recent_summary$years), "-", max(recent_summary$years))
        summary_options <- input$harvest_recent_summary_options
        if (is.null(summary_options)) {
          summary_options <- c("points", "median", "mean", "diagnostic")
        }
        model_points <- recent_points %>% filter(point_role == "Model")
        diagnostic_points <- recent_points %>% filter(point_role == "Diagnostic model")

        p <- ggplot() +
          geom_polygon(data = data.frame(x = c(x_max, 1, 1, x_max), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#f4a261", alpha = 0.7) +
          geom_polygon(data = data.frame(x = c(1, x_min, x_min, 1), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e76f51", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(x_max, 1, 1, x_max), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#2a9d8f", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(1, x_min, x_min, 1), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e9c46a", alpha = 0.72) +
          geom_hline(yintercept = 1, color = "#1f2937", linewidth = 1) +
          geom_vline(xintercept = 1, color = "#1f2937", linewidth = 1) +
          geom_blank(data = recent_points, aes(x = x, y = y))

        if ("points" %in% summary_options && nrow(model_points) > 0) {
          p <- p + geom_point(data = model_points, aes(x = x, y = y), size = 3.8, shape = 21, stroke = 0.55, fill = "#64748b", color = "#334155", alpha = 0.72)
        }
        if ("diagnostic" %in% summary_options && nrow(diagnostic_points) > 0) {
          p <- p + geom_point(data = diagnostic_points, aes(x = x, y = y), size = 4.8, shape = 21, stroke = 0.8, fill = "#facc15", color = "#111827", alpha = 0.9)
        }
        if ("median" %in% summary_options) {
          p <- p + geom_point(data = median_point, aes(x = x, y = y), size = 5.2, shape = 21, stroke = 0.9, fill = "#dc2626", color = "#7f1d1d", alpha = 0.9)
        }
        if ("mean" %in% summary_options) {
          p <- p + geom_point(data = mean_point, aes(x = x, y = y), size = 5, shape = 23, stroke = 0.9, fill = "#2563eb", color = "#1e3a8a", alpha = 0.9)
        }

        return(
          p +
            scale_x_continuous(expression("SB"["t"] / "SB"["MSY,t"]), expand = c(0, 0), limits = c(x_min, x_max)) +
            scale_y_continuous(expression("F"["t"] / "F"["MSY,t"]), expand = c(0, 0), limits = c(y_min, y_max)) +
            labs(title = paste0("Kobe Period-average Summary (", years_label, ")")) +
            theme_bw() +
            theme(
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(linewidth = 0.25, color = "#d1d5db"),
              panel.background = element_rect(fill = "#fcfcf8", color = NA),
              plot.title = element_text(face = "bold")
            )
        )
      }

      kobe_end <- kobe_data %>% group_by(scenario) %>% slice_max(order_by = year, n = 1, with_ties = FALSE) %>% ungroup()

      return(
        ggplot(kobe_data, aes(x = sb_sbmsy, y = f_fmsy)) +
          geom_polygon(data = data.frame(x = c(x_max, 1, 1, x_max), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#f4a261", alpha = 0.7) +
          geom_polygon(data = data.frame(x = c(1, x_min, x_min, 1), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e76f51", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(x_max, 1, 1, x_max), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#2a9d8f", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(1, x_min, x_min, 1), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e9c46a", alpha = 0.72) +
          geom_hline(yintercept = 1, color = "#1f2937", linewidth = 1) +
          geom_vline(xintercept = 1, color = "#1f2937", linewidth = 1) +
          geom_path(linewidth = 0.7, color = "#264653", alpha = 0.45) +
          geom_point(aes(fill = year), size = 3.8, shape = 21, stroke = 0.25, color = "#f8fafc") +
          geom_point(data = kobe_end, aes(fill = year), size = 4.8, shape = 21, stroke = 0.7, color = "#0f172a") +
          scale_fill_viridis_c("Year", option = "C", begin = 0.12, end = 0.95, direction = -1) +
          scale_x_continuous(expression("SB"["t"] / "SB"["MSY,t"]), expand = c(0, 0), limits = c(x_min, x_max)) +
          scale_y_continuous(expression("F"["t"] / "F"["MSY,t"]), expand = c(0, 0), limits = c(y_min, y_max)) +
          facet_wrap(~ scenario, ncol = facet_ncol) +
          labs(title = "Kobe (time-dynamic)") +
          theme_bw() +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25, color = "#d1d5db"),
            panel.background = element_rect(fill = "#fcfcf8", color = NA),
            strip.text = element_text(size = 10, face = "bold"),
            strip.background = element_rect(fill = "#f3f4f6", color = "#d1d5db"),
            legend.position = "right",
            legend.title = element_text(face = "bold"),
            plot.title = element_text(face = "bold")
          )
      )
    }

    if (plot_type == "majuro") {
      majuro_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_majuro_series(RepOut_list[[i]], scenarios_name[i])
      }))

      validate(need(nrow(majuro_data) > 0, "Majuro plot inputs are not available for the selected models"))

      majuro_data <- majuro_data %>%
        mutate(
          scenario = factor(scenario, levels = scenarios_name),
          year = as.numeric(year)
        ) %>%
        filter(is.finite(year), is.finite(dep), is.finite(f_fmsy))

      validate(need(nrow(majuro_data) > 0, "Majuro plot inputs are not available for the selected models"))

      x_limits <- reference_axis_limits(majuro_data$dep, threshold = 0.2, hard_min = 0, hard_max = 1, min_span = 0.35)
      y_limits <- reference_axis_limits(majuro_data$f_fmsy, threshold = 1, hard_min = 0, min_span = 0.75)
      x_min <- x_limits[1]
      x_max <- x_limits[2]
      y_min <- y_limits[1]
      y_max <- y_limits[2]

      if (identical(input$harvest_reference_view, "recent")) {
        recent_year_range <- input$harvest_recent_years
        if (is.null(recent_year_range) || length(recent_year_range) != 2) {
          recent_year_range <- default_recent_year_window(majuro_data$year, 4)
        }

        recent_summary <- summarise_recent_reference(
          majuro_data,
          "dep",
          "f_fmsy",
          recent_year_range,
          diagnostic_model = input$harvest_recent_diag_model
        )
        recent_points <- recent_summary$points
        median_point <- recent_summary$median_point
        mean_point <- recent_summary$mean_point

        validate(need(nrow(recent_points) > 0, "No Majuro summary values available for the selected years"))

        years_label <- paste0(min(recent_summary$years), "-", max(recent_summary$years))
        summary_options <- input$harvest_recent_summary_options
        if (is.null(summary_options)) {
          summary_options <- c("points", "median", "mean", "diagnostic")
        }
        model_points <- recent_points %>% filter(point_role == "Model")
        diagnostic_points <- recent_points %>% filter(point_role == "Diagnostic model")

        p <- ggplot() +
          geom_polygon(data = data.frame(x = c(x_max, 0.2, 0.2, x_max), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#f4a261", alpha = 0.7) +
          geom_polygon(data = data.frame(x = c(0.2, x_min, x_min, 0.2), y = c(y_max, y_max, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e76f51", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(x_max, 0.2, 0.2, x_max), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#2a9d8f", alpha = 0.72) +
          geom_segment(
            data = data.frame(x = 0.2, xend = 1, y = 1, yend = 1),
            aes(x = x, xend = xend, y = y, yend = yend),
            inherit.aes = FALSE,
            color = "#1f2937",
            linewidth = 1
          ) +
          geom_vline(xintercept = 0.2, color = "#1f2937", linewidth = 1) +
          geom_blank(data = recent_points, aes(x = x, y = y))

        if ("points" %in% summary_options && nrow(model_points) > 0) {
          p <- p + geom_point(data = model_points, aes(x = x, y = y), size = 3.8, shape = 21, stroke = 0.55, fill = "#64748b", color = "#334155", alpha = 0.72)
        }
        if ("diagnostic" %in% summary_options && nrow(diagnostic_points) > 0) {
          p <- p + geom_point(data = diagnostic_points, aes(x = x, y = y), size = 4.8, shape = 21, stroke = 0.8, fill = "#facc15", color = "#111827", alpha = 0.9)
        }
        if ("median" %in% summary_options) {
          p <- p + geom_point(data = median_point, aes(x = x, y = y), size = 5.2, shape = 21, stroke = 0.9, fill = "#dc2626", color = "#7f1d1d", alpha = 0.9)
        }
        if ("mean" %in% summary_options) {
          p <- p + geom_point(data = mean_point, aes(x = x, y = y), size = 5, shape = 23, stroke = 0.9, fill = "#2563eb", color = "#1e3a8a", alpha = 0.9)
        }

        return(
          p +
            scale_x_continuous(expression("SB"["t"] / "SB"["F=0,t"]), expand = c(0, 0), limits = c(x_min, x_max)) +
            scale_y_continuous(expression("F"["t"] / "F"["MSY,t"]), expand = c(0, 0), limits = c(y_min, y_max)) +
            labs(title = paste0("Majuro Period-average Summary (", years_label, ")")) +
            theme_bw() +
            theme(
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(linewidth = 0.25, color = "#d1d5db"),
              panel.background = element_rect(fill = "#fcfcf8", color = NA),
              plot.title = element_text(face = "bold")
            )
        )
      }

      majuro_end <- majuro_data %>% group_by(scenario) %>% slice_max(order_by = year, n = 1, with_ties = FALSE) %>% ungroup()

      return(
        ggplot(majuro_data, aes(x = dep, y = f_fmsy)) +
          geom_polygon(data = data.frame(x = c(x_max, 0.2, 0.2, x_max), y = c(y_max, y_max, 1, 1)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#f4a261", alpha = 0.7) +
          geom_polygon(data = data.frame(x = c(0.2, x_min, x_min, 0.2), y = c(y_max, y_max, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#e76f51", alpha = 0.72) +
          geom_polygon(data = data.frame(x = c(x_max, 0.2, 0.2, x_max), y = c(1, 1, y_min, y_min)), aes(x = x, y = y), inherit.aes = FALSE, fill = "#2a9d8f", alpha = 0.72) +
          geom_segment(
            data = data.frame(x = 0.2, xend = 1, y = 1, yend = 1),
            aes(x = x, xend = xend, y = y, yend = yend),
            inherit.aes = FALSE,
            color = "#1f2937",
            linewidth = 1
          ) +
          geom_vline(xintercept = 0.2, color = "#1f2937", linewidth = 1) +
          geom_path(linewidth = 0.7, color = "#264653", alpha = 0.45) +
          geom_point(aes(fill = year), size = 3.8, shape = 21, stroke = 0.25, color = "#f8fafc") +
          geom_point(data = majuro_end, aes(fill = year), size = 4.8, shape = 21, stroke = 0.7, color = "#0f172a") +
          scale_fill_viridis_c("Year", option = "C", begin = 0.12, end = 0.95, direction = -1) +
          scale_x_continuous(expression("SB"["t"] / "SB"["F=0,t"]), expand = c(0, 0), limits = c(x_min, x_max)) +
          scale_y_continuous(expression("F"["t"] / "F"["MSY,t"]), expand = c(0, 0), limits = c(y_min, y_max)) +
          facet_wrap(~ scenario, ncol = facet_ncol) +
          labs(title = "Majuro (time-dynamic)") +
          theme_bw() +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25, color = "#d1d5db"),
            panel.background = element_rect(fill = "#fcfcf8", color = NA),
            strip.text = element_text(size = 10, face = "bold"),
            strip.background = element_rect(fill = "#f3f4f6", color = "#d1d5db"),
            legend.position = "right",
            legend.title = element_text(face = "bold"),
            plot.title = element_text(face = "bold")
          )
      )
    }

    if (plot_type == "fm_juv_adult") {
      fm_all <- data.frame(); mat_all <- data.frame(); popn_all <- data.frame()

      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i])
        fm_all <- bind_rows(fm_all, fm_temp)

        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data)
        popn_all <- bind_rows(popn_all, popn_temp)

        mat_temp <- safe_array_to_df(ParOut_list[[i]]@mat) %>% arrange(age, season) %>%
          mutate(scenario = scenarios_name[i], age_new = row_number()) %>%
          select(age = age_new, maturity = data, scenario)
        mat_all <- bind_rows(mat_all, mat_temp)
      }

      fm_juv_adult_area <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(juv = (1 - maturity) * N, adult = maturity * N, catch.juv = data * juv, catch.adult = data * adult) %>%
        group_by(year, season, area, scenario) %>%
        summarise(catch.juv = sum(catch.juv, na.rm = TRUE), juv = sum(juv, na.rm = TRUE), catch.adult = sum(catch.adult, na.rm = TRUE), adult = sum(adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate.juv = catch.juv / juv, harvest_rate.adult = catch.adult / adult,
               F.juv = -log(pmax(1 - harvest_rate.juv, 0.001)), F.adult = -log(pmax(1 - harvest_rate.adult, 0.001))) %>%
        group_by(year, area, scenario) %>% summarise(F.juv = sum(F.juv, na.rm = TRUE), F.adult = sum(F.adult, na.rm = TRUE), .groups = "drop")

      fm_juv_adult_all <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(juv = (1 - maturity) * N, adult = maturity * N, catch.juv = data * juv, catch.adult = data * adult) %>%
        group_by(year, season, scenario) %>%
        summarise(catch.juv = sum(catch.juv, na.rm = TRUE), juv = sum(juv, na.rm = TRUE), catch.adult = sum(catch.adult, na.rm = TRUE), adult = sum(adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate.juv = catch.juv / juv, harvest_rate.adult = catch.adult / adult,
               F.juv = -log(pmax(1 - harvest_rate.juv, 0.001)), F.adult = -log(pmax(1 - harvest_rate.adult, 0.001))) %>%
        group_by(year, scenario) %>% summarise(F.juv = sum(F.juv, na.rm = TRUE), F.adult = sum(F.adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(area = "All")

      fm_juv_adult_combined <- bind_rows(fm_juv_adult_area, fm_juv_adult_all)
      fm_plot_data <- fm_juv_adult_combined %>%
        pivot_longer(cols = c(F.juv, F.adult), names_to = "type", values_to = "F") %>%
        mutate(type = factor(type, levels = c("F.juv", "F.adult"), labels = c("Juvenile F", "Adult F")))

      scenario_levels <- scenarios_name[scenarios_name %in% unique(as.character(fm_plot_data$scenario))]
      scenario_colors <- get_scenario_colors(scenario_levels)
      fm_plot_data$scenario <- factor(fm_plot_data$scenario, levels = scenario_levels)

      fm_area_plot <- ggplot(fm_plot_data, aes(x = year, y = F, color = scenario, linetype = type)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        scale_color_manual(values = scenario_colors) +
        scale_linetype_manual(name = "Life Stage", values = c("Juvenile F" = "dashed", "Adult F" = "solid")) +
        coord_cartesian(ylim = c(0, NA)) + facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = "Annual Instantaneous F") +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(fm_plot_data, aes(x = year, y = F, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )

      lifestage_legend <- cowplot::get_legend(
        ggplot(fm_plot_data, aes(x = year, y = F, linetype = type)) + geom_line(linewidth = 2) +
          scale_linetype_manual(name = "Life Stage", values = c("Juvenile F" = "dashed", "Adult F" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(linewidth = 1.5))) +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(2.5, "cm"))
      )

      combined_legend <- cowplot::plot_grid(scenario_legend, lifestage_legend, ncol = 1)
      return(cowplot::plot_grid(fm_area_plot, combined_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    if (plot_type == "fm_area_contrib") {
      fm_all <- data.frame(); mat_all <- data.frame(); popn_all <- data.frame(); m_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i]); fm_all <- bind_rows(fm_all, fm_temp)
        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data); popn_all <- bind_rows(popn_all, popn_temp)
        mat_temp <- safe_array_to_df(ParOut_list[[i]]@mat) %>% arrange(age, season) %>% mutate(scenario = scenarios_name[i], age_new = row_number()) %>% select(age = age_new, maturity = data, scenario); mat_all <- bind_rows(mat_all, mat_temp)
        m_temp <- safe_array_to_df(RepOut_list[[i]]@m_at_age) %>% arrange(age, season) %>% mutate(scenario = scenarios_name[i], age_new = row_number()) %>% select(age = age_new, M = data, scenario); m_all <- bind_rows(m_all, m_temp)
      }

      area_data <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        left_join(m_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(catch = data * N) %>%
        group_by(year, season, area, scenario) %>%
        summarise(catch = sum(catch, na.rm = TRUE), N = sum(N, na.rm = TRUE), .groups = "drop") %>%
        mutate(F_season = -log(1 - pmin(catch / N, 0.99)))

      area_contribution <- area_data %>%
        group_by(year, area, scenario) %>%
        summarise(weighted_F = sum(F_season * N, na.rm = TRUE), .groups = "drop") %>%
        group_by(year, scenario) %>%
        mutate(total_weighted_F = sum(weighted_F), contribution = (weighted_F / total_weighted_F) * 100) %>%
        ungroup()

      area_contribution$scenario <- factor(area_contribution$scenario, levels = unique(area_contribution$scenario))
      area_contribution$area <- factor(area_contribution$area)

      return(
        ggplot(area_contribution, aes(x = year, y = contribution, fill = area)) +
          geom_area(position = "stack", alpha = 0.8) +
          scale_fill_viridis_d(name = "Area") +
          facet_wrap(~ scenario, ncol = facet_ncol) +
          labs(x = "Year", y = "Contribution to Total F (%)") +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), strip.text = element_text(size = 10, face = "bold"))
      )
    }

    if (plot_type %in% c("sp_combined", "tb_combined")) {
      use_with <- if (plot_type == "sp_combined") "adultBiomass" else "totalBiomass"
      use_no <- if (plot_type == "sp_combined") "adultBiomass_nofish" else "totalBiomass_nofish"
      y_label <- if (plot_type == "sp_combined") bquote("Spawning Potential (" * 10^3 * " MT)") else bquote("Total Biomass (" * 10^3 * " MT)")

      scenarios_name <- names(RepOut_list)
      bioNoFish_all <- data.frame(); bioFish_all <- data.frame()

      for (i in seq_along(RepOut_list)) {
        bioNoFish_temp <- safe_array_to_df(slot(RepOut_list[[i]], use_no)) %>% mutate(scenario = scenarios_name[i])
        bioNoFish_all <- bind_rows(bioNoFish_all, bioNoFish_temp)
      }
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(slot(RepOut_list[[i]], use_with)) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }

      bioNoFish_yearly <- bioNoFish_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / n_distinct(season) / 1e3, .groups = "drop")
      bioFish_yearly <- bioFish_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / n_distinct(season) / 1e3, .groups = "drop")

      bioNoFish_total <- bioNoFish_all %>% group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>% mutate(area = "All")
      bioFish_total <- bioFish_all %>% group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>% mutate(area = "All")

      bioNoFish_combined <- bind_rows(bioNoFish_yearly, bioNoFish_total) %>% mutate(type = "No fishing")
      bioFish_combined <- bind_rows(bioFish_yearly, bioFish_total) %>% mutate(type = "Fished")
      bio_combined <- bind_rows(bioNoFish_combined, bioFish_combined)

      scenario_levels <- scenarios_name[scenarios_name %in% unique(as.character(bio_combined$scenario))]
      scenario_colors <- get_scenario_colors(scenario_levels)
      bio_combined$scenario <- factor(bio_combined$scenario, levels = scenario_levels)
      bio_combined$type <- factor(bio_combined$type, levels = c("No fishing", "Fished"))

      bio_plot <- ggplot(bio_combined, aes(x = year, y = data, color = scenario, linetype = type)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        scale_color_manual(values = scenario_colors, breaks = scenario_levels, limits = scenario_levels, drop = FALSE) +
        scale_linetype_manual(name = "Status", values = c("No fishing" = "dashed", "Fished" = "solid")) +
        coord_cartesian(ylim = c(0, NA)) +
        facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = y_label) +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(bio_combined, aes(x = year, y = data, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors, breaks = scenario_levels, limits = scenario_levels, drop = FALSE) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )

      status_legend <- cowplot::get_legend(
        ggplot(bio_combined, aes(x = year, y = data, linetype = type)) + geom_line(linewidth = 2) +
          scale_linetype_manual(name = "Status", values = c("No fishing" = "dashed", "Fished" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(linewidth = 1.5))) +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(2.5, "cm"))
      )

      combined_legend <- cowplot::plot_grid(scenario_legend, status_legend, ncol = 1)
      return(cowplot::plot_grid(bio_plot, combined_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    ggplot() + theme_void()
  })
  harvest_plot_reactive <- bindCache(
    harvest_plot_reactive,
    rv$data_loaded,
    input$model_dir,
    input$harvest_scenarios,
    input$harvest_plot,
    input$harvest_reference_view,
    input$harvest_recent_years,
    input$harvest_recent_diag_model,
    input$harvest_recent_summary_options,
    input$harvest_facet_ncol,
    input$harvest_ensemble,
    input$harvest_ensemble_levels,
    input$harvest_ensemble_weighting,
    input$harvest_ensemble_center,
    applied_manual_weight_key()
  )
  observeEvent(list(input$live_update_plots, input$harvest_scenarios, input$harvest_plot,
                    input$harvest_reference_view, input$harvest_recent_years,
                    input$harvest_recent_diag_model, input$harvest_recent_summary_options,
                    input$harvest_facet_ncol, input$harvest_ensemble,
                    input$harvest_ensemble_levels, input$harvest_ensemble_weighting,
                    input$harvest_ensemble_center,
                    input$harvest_plot_height, input$harvest_plot_width), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$harvest_scenarios) == 0) return()
    harvest_filters_applied(isolate(harvest_filters_current()))
    harvest_live_update_nonce(isolate(harvest_live_update_nonce()) + 1)
  }, ignoreInit = TRUE)
  observeEvent(input$harvest_apply_filters, {
    harvest_filters_applied(isolate(harvest_filters_current()))
  }, ignoreInit = TRUE)
  observeEvent(list(rv$initial_render_nonce, input$harvest_scenarios), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= harvest_last_initialized_nonce()) return()
    if (length(input$harvest_scenarios) == 0) return()
    harvest_last_initialized_nonce(rv$initial_render_nonce)
    harvest_filters_applied(isolate(harvest_filters_current()))
  }, ignoreInit = TRUE)
  harvest_plot_reactive <- bindEvent(harvest_plot_reactive, rv$initial_render_nonce, input$harvest_apply_filters, harvest_live_update_nonce(), ignoreInit = FALSE)


  output$harvest_plot_output_ui <- renderUI({
    filters <- harvest_filters_applied()
    h <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_height)) else suppressWarnings(as.integer(input$harvest_plot_height))
    w <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_width)) else suppressWarnings(as.integer(input$harvest_plot_width))
    if (!is.finite(h)) h <- 900
    if (!is.finite(w)) w <- 1200
    h <- min(max(h, 450), 1800)
    w <- min(max(w, 700), 2200)

    plotOutput("harvest_plot_output", height = paste0(h, "px"), width = paste0(w, "px"))
  })

  output$harvest_plot_output <- renderPlot({
    harvest_plot_reactive()
  })

  output$harvest_weight_table <- renderDT({
    req(rv$data_loaded, input$harvest_scenarios)
    req(isTRUE(input$harvest_ensemble))
    req(input$harvest_ensemble_weighting %in% c("aic", "manual"))

    scenarios_name <- input$harvest_scenarios
    validate(need(length(scenarios_name) > 0, "No models selected"))

    if (identical(input$harvest_ensemble_weighting, "manual")) {
      draft_tbl <- manual_weight_tbl()
      if (is.null(draft_tbl) || nrow(draft_tbl) == 0) {
        draft_tbl <- compute_manual_weights(scenarios_name)
      }

      weight_tbl <- draft_tbl %>%
        mutate(
          `Input Weight` = round(weight_input, 6),
          Weight = sprintf("%.4f", weight),
          Percent = sprintf("%.2f%%", 100 * weight)
        ) %>%
        transmute(Model = scenario, `Input Weight`, Weight, Percent)

      return(datatable(
        weight_tbl,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0, 2, 3))),
        options = list(dom = "t", paging = FALSE, ordering = FALSE, deferRender = TRUE)
      ))
    }

    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    weight_tbl <- compute_ensemble_weights(ParOut_list, method = "aic") %>%
      mutate(
        Weight = sprintf("%.4f", weight),
        Percent = sprintf("%.2f%%", 100 * weight),
        AIC = ifelse(is.finite(aic), sprintf("%.2f", aic), NA_character_),
        `Delta AIC` = ifelse(is.finite(delta_aic), sprintf("%.2f", delta_aic), NA_character_),
        `Relative Likelihood` = ifelse(is.finite(rel_likelihood), sprintf("%.6f", rel_likelihood), NA_character_)
      ) %>%
      transmute(Model = scenario, Weight, Percent, AIC, `Delta AIC`, `Relative Likelihood`, `Method Used` = method_used)

    datatable(
      weight_tbl,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, deferRender = TRUE)
    )
  })

  output$harvest_weighting_note <- renderUI({
    req(isTRUE(input$harvest_ensemble))

    if (identical(input$harvest_ensemble_weighting, "aic")) {
      return(
        tags$p(
          HTML("Official AIC weighting is used here: <code>AIC_i = 2k_i - 2 log L_i</code>, <code>&Delta;AIC_i = AIC_i - min(AIC)</code>, <code>RL_i = exp(-0.5 * &Delta;AIC_i)</code>, and <code>w_i = RL_i / sum(RL)</code>."),
          style = "margin-bottom: 12px;"
        )
      )
    }

    if (identical(input$harvest_ensemble_weighting, "manual")) {
      return(
        tags$p(
          "Edit Input Weight directly. Weight and Percent update immediately; the plot updates when you click Apply.",
          style = "margin-bottom: 12px;"
        )
      )
    }

    NULL
  })

  output$harvest_method_table <- renderDT({
    req(isTRUE(input$harvest_ensemble))

    method_tbl <- data.frame(
      Method = c("Weighted quantile", "Weighted mean"),
      `Center line` = c("Weighted median (50% quantile)", "Weighted mean"),
      Bands = c("Weighted 50/80/95% quantile intervals", "Weighted 50/80/95% quantile intervals"),
      Interpretation = c(
        "Robust to outliers and follows the weighted distribution directly.",
        "Averages model values, but uncertainty bands still come from weighted quantiles."
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    datatable(
      method_tbl,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE, deferRender = TRUE)
    )
  })

  output$harvest_reference_table <- renderDT({
    req(rv$data_loaded, input$harvest_scenarios)
    req(input$harvest_plot %in% c("kobe", "majuro"))

    scenarios_name <- input$harvest_scenarios
    validate(need(length(scenarios_name) > 0, "No models selected"))
    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)

    tbl <- if (identical(input$harvest_plot, "kobe")) {
      bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_kobe_series(RepOut_list[[i]], scenarios_name[i])
      })) %>%
        transmute(
          Model = scenario,
          Year = year,
          `SB/SBMSY` = round(sb_sbmsy, 4),
          `F/FMSY` = round(f_fmsy, 4)
        )
    } else {
      bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_majuro_series(RepOut_list[[i]], scenarios_name[i])
      })) %>%
        transmute(
          Model = scenario,
          Year = year,
          `SB/SBF0` = round(dep, 4),
          `F/FMSY` = round(f_fmsy, 4)
        )
    }

    if (!is.null(input$harvest_reference_model) &&
        nzchar(input$harvest_reference_model) &&
        input$harvest_reference_model %in% tbl$Model) {
      tbl <- tbl %>% filter(Model == input$harvest_reference_model)
    }

    validate(need(nrow(tbl) > 0, "No Kobe/Majuro values available for the selected models"))

    datatable(
      tbl,
      rownames = FALSE,
      class = "compact nowrap",
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE, order = list(list(0, "asc"), list(1, "asc")), deferRender = TRUE))
  })

  output$harvest_reference_note <- renderUI({
    req(input$harvest_plot %in% c("kobe", "majuro"))
    if (identical(input$harvest_plot, "kobe")) {
      return(tags$p("Kobe Values", style = "font-weight: 600; margin-bottom: 10px;"))
    }
    tags$p("Majuro Values", style = "font-weight: 600; margin-bottom: 10px;")
  })

  output$harvest_recent_years_ui <- renderUI({
    req(input$harvest_plot %in% c("kobe", "majuro"))
    req(identical(input$harvest_reference_view, "recent"))
    req(rv$data_loaded, input$harvest_scenarios)

    scenarios_name <- input$harvest_scenarios
    validate(need(length(scenarios_name) > 0, "No models selected"))
    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)

    ref_data <- if (identical(input$harvest_plot, "kobe")) {
      bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_kobe_series(RepOut_list[[i]], scenarios_name[i])
      }))
    } else {
      bind_rows(lapply(seq_along(RepOut_list), function(i) {
        extract_majuro_series(RepOut_list[[i]], scenarios_name[i])
      }))
    }

    validate(need(nrow(ref_data) > 0, "No reference years available"))

    years <- sort(unique(as.integer(ref_data$year[is.finite(ref_data$year)])))
    validate(need(length(years) > 0, "No reference years available"))

    default_years <- default_recent_year_window(years, 4)
    selected_years <- isolate(input$harvest_recent_years)
    if (is.null(selected_years) || length(selected_years) != 2) {
      selected_years <- default_years
    } else {
      selected_years[1] <- max(min(selected_years[1], max(years)), min(years))
      selected_years[2] <- max(min(selected_years[2], max(years)), min(years))
      if (selected_years[1] > selected_years[2]) {
        selected_years <- sort(selected_years)
      }
    }

    sliderInput(
      "harvest_recent_years",
      "Averaged over years:",
      min = min(years),
      max = max(years),
      value = selected_years,
      step = 1,
      sep = ""
    )
  })

  output$harvest_recent_diag_model_ui <- renderUI({
    req(input$harvest_plot %in% c("kobe", "majuro"))
    req(identical(input$harvest_reference_view, "recent"))
    scenarios_name <- input$harvest_scenarios
    req(length(scenarios_name) > 1)

    default_diag <- if (!is.null(input$harvest_recent_diag_model) &&
      input$harvest_recent_diag_model %in% scenarios_name) {
      input$harvest_recent_diag_model
    } else {
      diag_match <- scenarios_name[grepl("diag|diagnostic", scenarios_name, ignore.case = TRUE)]
      if (length(diag_match) > 0) diag_match[[1]] else scenarios_name[[1]]
    }

    selectInput(
      "harvest_recent_diag_model",
      "Diagnostic model:",
      choices = scenarios_name,
      selected = default_diag
    )
  })

  output$harvest_recent_summary_options_ui <- renderUI({
    req(input$harvest_plot %in% c("kobe", "majuro"))
    req(identical(input$harvest_reference_view, "recent"))

    checkboxGroupInput(
      "harvest_recent_summary_options",
      "Show:",
      choices = c(
        "Model points" = "points",
        "Median" = "median",
        "Mean" = "mean",
        "Diagnostic model" = "diagnostic"
      ),
      selected = c("points", "median", "mean", "diagnostic")
    )
  })

  output$harvest_reference_model_ui <- renderUI({
    req(input$harvest_plot %in% c("kobe", "majuro"))
    scenarios_name <- input$harvest_scenarios
    req(length(scenarios_name) > 1)

    selectInput(
      "harvest_reference_model",
      "Model:",
      choices = scenarios_name,
      selected = if (!is.null(input$harvest_reference_model) &&
        input$harvest_reference_model %in% scenarios_name) {
        input$harvest_reference_model
      } else {
        scenarios_name[[1]]
      }
    )
  })

  mod_sections_download("harvest", "Key Quantities Plot", harvest_plot_reactive, input, session, output)
}

mod_tagging_ui <- function() {
  tabItem(
    tabName = "tagging",
    h2("Tagging Dynamics", style = "color: #39cccc;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "info",
        pickerInput("tag_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        conditionalPanel(
          condition = "input.tag_plot != 'report'",
          radioButtons(
            "tag_time_mode",
            "Time axis:",
            choices = c(
              "By year" = "year",
              "By model step" = "step"
            ),
            selected = "year"
          )
        ),
        conditionalPanel(
          condition = "input.tag_plot == 'returns_all' || input.tag_plot == 'returns_group'",
          pickerInput(
            "tag_years",
            "Years:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              selectAllText = "Select All",
              deselectAllText = "Deselect All",
              selectedTextFormat = "count > 3",
              countSelectedText = "{0} years selected",
              liveSearch = TRUE,
              liveSearchPlaceholder = "Search years..."
            )
          )
        ),
        selectInput("tag_facet_ncol", "Facet columns:", choices = as.character(1:6), selected = "4"),
        sliderInput(
          "tag_plot_height",
          "Plot height (px)",
          min = 450,
          max = 1800,
          value = 900,
          step = 50
        ),
        sliderInput(
          "tag_plot_width",
          "Plot width (px)",
          min = 700,
          max = 2200,
          value = 1200,
          step = 50
        ),
        conditionalPanel(
          condition = "input.tag_plot == 'report'",
          checkboxInput("tag_rr_nonneg_only", "Tag RR filter: exclude rr <= 0", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.tag_plot == 'recap_pressure'",
          selectInput(
            "tag_pressure_release_dim",
            "Release grouping:",
            choices = c(
              "Release group" = "rel_group",
              "Release region" = "rel_region"
            ),
            selected = "rel_group"
          ),
          selectInput(
            "tag_pressure_recap_dim",
            "Recapture grouping:",
            choices = c(
              "Recapture fishery/group" = "recap_fishery",
              "Recapture region" = "recap_region"
            ),
            selected = "recap_fishery"
          ),
          numericInput(
            "tag_pressure_correction",
            "Continuity correction:",
            value = 1e-4,
            min = 0,
            step = 1e-4
          ),
          sliderInput(
            "tag_pressure_cap",
            "Colour cap: |log2(O/E)|",
            min = 1,
            max = 6,
            value = 3,
            step = 0.5
          )
        ),
        selectInput("tag_plot", "Plot:", choices = c(
          "Tag Reporting Rates by Group" = "report",
          "Tag Returns Over Time (All Combined)" = "returns_all",
          "Tag Returns by Recapture Group" = "returns_group",
          "Tag Attrition (All Fisheries Combined)" = "attr_all",
          "Tag Attrition (By Program)" = "attr_program",
          "Tag Attrition (By Region)" = "attr_region",
          "Observed / Expected Recapture Pressure" = "recap_pressure"
        )),
        actionButton("tag_apply_filters", "Apply", class = "btn-info", style = "width: 100%;"),
        tags$small("Selections update the plot when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_tagging_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(
        title = "Plot",
        width = 9,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "tagging_plot_output",
          uiOutput("tagging_plot_output_ui"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering tagging plot...")
            )
          )
        )
      )
    )
  )
}

mod_tagging_server <- function(input, output, session, rv) {
  tag_live_update_nonce <- reactiveVal(0)
  tag_filters_current <- reactive({
    list(
      scenarios = input$tag_scenarios,
      time_mode = if (is.null(input$tag_time_mode)) "year" else input$tag_time_mode,
      years = input$tag_years,
      facet_ncol = input$tag_facet_ncol,
      rr_nonneg_only = isTRUE(input$tag_rr_nonneg_only),
      pressure_release_dim = if (is.null(input$tag_pressure_release_dim)) "rel_group" else input$tag_pressure_release_dim,
      pressure_recap_dim = if (is.null(input$tag_pressure_recap_dim)) "recap_fishery" else input$tag_pressure_recap_dim,
      pressure_correction = if (is.null(input$tag_pressure_correction)) 1e-4 else suppressWarnings(as.numeric(input$tag_pressure_correction)),
      pressure_cap = if (is.null(input$tag_pressure_cap)) 3 else suppressWarnings(as.numeric(input$tag_pressure_cap)),
      plot = if (is.null(input$tag_plot)) "report" else input$tag_plot,
      plot_height = if (is.null(input$tag_plot_height)) 900 else suppressWarnings(as.integer(input$tag_plot_height)),
      plot_width = if (is.null(input$tag_plot_width)) 1200 else suppressWarnings(as.integer(input$tag_plot_width))
    )
  })
  tag_filters_applied <- reactiveVal(NULL)
  tag_last_initialized_nonce <- reactiveVal(0)
  observeEvent(rv$data_loaded, {
    sc <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    updatePickerInput(session, "tag_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(tag_filters_current(), tag_filters_applied())
    set_apply_pending(session, "tag_apply_filters", pending)
  })

  observe({
    req(rv$data_loaded)
    sc <- input$tag_scenarios
    if (is.null(sc) || length(sc) == 0) return()
    yrs <- unlist(lapply(sc, function(m) {
      yr <- rv$YearRanges[[m]]
      c(as.numeric(yr$minYear), as.numeric(yr$maxYear))
    }))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) == 0) return()
    y_min <- floor(min(yrs))
    y_max <- ceiling(max(yrs))
    choices <- as.character(seq(y_min, y_max))
    cur <- isolate(input$tag_years)
    if (is.null(cur) || length(cur) == 0) cur <- choices
    cur <- intersect(cur, choices)
    if (length(cur) == 0) cur <- choices
    updatePickerInput(session, "tag_years", choices = choices, selected = cur)
  })

  tagging_plot_reactive <- reactive({
    req(rv$data_loaded, input$tag_scenarios)
    scenarios_name <- input$tag_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    time_mode <- if (is.null(input$tag_time_mode)) "year" else input$tag_time_mode
    mode <- if (is.null(input$tag_plot)) "report" else input$tag_plot
    rr_nonneg_only <- isTRUE(input$tag_rr_nonneg_only)

    # Tagging plots now use the Scenarios picker directly:
    # 1 selected = single-model display, 2+ selected = overlay.
    selected_models <- intersect(scenarios_name, names(rv$ParOut_list))
    if (length(selected_models) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No valid models selected"))

    has_tagtemp <- names(rv$TagTempOut_list)[!sapply(rv$TagTempOut_list, is.null)]
    has_tagout <- names(rv$TagOut_list)[!sapply(rv$TagOut_list, is.null)]
    in_scope <- intersect(scenarios_name, names(rv$ParOut_list))

    if (mode %in% c("returns_all", "returns_group")) {
      candidate <- intersect(in_scope, has_tagtemp)
      selected_models <- intersect(selected_models, candidate)
    }

    if (mode %in% c("attr_all", "attr_program", "attr_region", "recap_pressure")) {
      candidate <- intersect(in_scope, intersect(has_tagtemp, has_tagout))
      selected_models <- intersect(selected_models, candidate)
    }

    if (length(selected_models) == 0 || is.na(selected_models[1])) {
      return(
        ggplot() + theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "Required tag data files are missing for selected model(s)")
      )
    }

    overlay <- length(selected_models) > 1
    facet_ncol <- suppressWarnings(as.integer(input$tag_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
    facet_ncol <- min(max(facet_ncol, 1), 6)

    ParOut_list <- subset_named(rv$ParOut_list, selected_models)
    TagRepOut_list <- subset_named(rv$TagRepOut_list, selected_models)
    TagOut_list <- subset_named(rv$TagOut_list, selected_models)
    TagTempOut_list <- subset_named(rv$TagTempOut_list, selected_models)
    seasons_per_model <- pm_get_seasons_per_model(subset_named(rv$ParOut_list, selected_models), fallback = 4)
    fishery_map <- rv$FISHERY_MAPS[[selected_models[1]]]
    scenario_colors <- get_scenario_colors(selected_models)

    is_compatible_by <- function(df, key_col) {
      if (!overlay) return(TRUE)
      sets <- split(df[[key_col]], df$Model)
      sets <- lapply(sets, function(x) sort(unique(as.character(x[!is.na(x)]))))
      if (length(sets) <= 1) return(TRUE)
      all(vapply(sets[-1], function(s) identical(s, sets[[1]]), logical(1)))
    }

    add_time_x <- function(df, x_col = "recap_ts") {
      if (nrow(df) == 0) return(df)
      if (identical(time_mode, "step")) {
        df$x <- df[[x_col]]
        df$x_label <- if (identical(x_col, "recap_ts")) "Time (model steps)" else "Time at liberty (model steps; usually quarters)"
        return(df)
      }

      if (identical(x_col, "recap_ts")) {
        df$x <- floor(as.numeric(df$recap_ts))
        out <- df %>% group_by(Model, x, across(any_of(c("tag_recapture_name")))) %>%
          summarise(recap_obs = sum(recap_obs, na.rm = TRUE), recap_pred = sum(recap_pred, na.rm = TRUE), .groups = "drop")
        out$x_label <- "Time (year)"
        return(out)
      }

      spm <- seasons_per_model[as.character(df$Model)]
      spm[!is.finite(spm) | spm <= 0] <- 4
      df$x <- floor(as.numeric(df$period_at_liberty) / as.numeric(spm))
      out <- df %>% group_by(Model, x, across(any_of(c("program", "recap.region")))) %>%
        summarise(recap_obs = sum(recap_obs, na.rm = TRUE), recap_pred = sum(recap_pred, na.rm = TRUE), .groups = "drop")
      out$x_label <- "Time at liberty (years)"
      out
    }

    apply_year_filter <- function(df) {
      if (!identical(time_mode, "year")) return(df)
      # Apply year filter only for calendar-time series (not years-at-liberty).
      if (!("x_label" %in% names(df)) || !all(df$x_label == "Time (year)")) return(df)
      yrs <- suppressWarnings(as.numeric(input$tag_years))
      if (is.null(yrs) || length(yrs) == 0 || all(!is.finite(yrs))) return(df)
      df %>% filter(x %in% yrs)
    }

    if (mode == "report") {
      get_reporting_group_labels <- function(model_name) {
        tag_map_df <- NULL
        if (!is.null(rv$tag_rep_map_dfs) && model_name %in% names(rv$tag_rep_map_dfs)) {
          tag_map_df <- rv$tag_rep_map_dfs[[model_name]]
        }
        if (!is.data.frame(tag_map_df) || nrow(tag_map_df) == 0) {
          scenario_dir <- tryCatch(file.path(input$model_dir, model_name), error = function(e) NULL)
          tag_map_r <- find_tag_rep_map_script(scenario_dir)
          tag_map_df <- load_tag_rep_map_from_r(tag_map_r)
        }
        if (is.data.frame(tag_map_df) && nrow(tag_map_df) > 0) {
          return(
            tag_map_df %>%
              transmute(
                group = as.numeric(tag_recapture_group),
                group_name = as.character(tag_recapture_name)
              ) %>%
              filter(is.finite(group), !is.na(group_name), nzchar(group_name)) %>%
              arrange(group)
          )
        }

        fmap <- rv$FISHERY_MAPS[[model_name]]
        if (is.null(fmap) || !is.data.frame(fmap)) {
          return(data.frame(group = numeric(0), group_name = character(0), stringsAsFactors = FALSE))
        }

        by_recapture <- fmap %>%
          filter(!is.na(tag_recapture_group)) %>%
          mutate(
            group = as.numeric(tag_recapture_group),
            group_name = as.character(tag_recapture_name),
            fallback_name = as.character(fishery_name)
          ) %>%
          mutate(group_name = if_else(is.na(group_name) | !nzchar(group_name), fallback_name, group_name)) %>%
          select(group, group_name)

        by_fishery <- fmap %>%
          filter(!is.na(fishery), !is.na(fishery_name), nzchar(fishery_name)) %>%
          transmute(group = as.numeric(fishery), group_name = as.character(fishery_name))

        bind_rows(by_recapture, by_fishery) %>%
          filter(is.finite(group)) %>%
          filter(!is.na(group_name), nzchar(group_name)) %>%
          arrange(group) %>%
          group_by(group) %>%
          summarise(group_name = first(group_name), .groups = "drop") %>%
          group_by(group_name) %>%
          mutate(
            name_idx = row_number(),
            name_n = dplyr::n(),
            group_name = if_else(name_n > 1, paste0(group_name, "-", name_idx), group_name)
          ) %>%
          ungroup() %>%
          select(group, group_name)
      }

      tag_rr_list <- list()
      for (i in seq_along(ParOut_list)) {
        model_name <- selected_models[i]
        group_labels <- get_reporting_group_labels(model_name)
        upper.bound <- tryCatch(subset(flags(ParOut_list[[i]]), flagtype == 1 & flag == 33)$value[1] / 100, error = function(e) 1)
        tag_dt <- data.frame(
          group = c(tag_fish_rep_grp(ParOut_list[[i]])),
          rr = c(tag_fish_rep_rate(ParOut_list[[i]])),
          prior_mean = c(tag_fish_rep_target(ParOut_list[[i]]) / 100),
          prior_sd = c(sqrt(1 / (2 * tag_fish_rep_pen(ParOut_list[[i]]))))
        ) %>% unique() %>% arrange(group) %>%
          left_join(group_labels, by = "group") %>%
          mutate(
            scenario = model_name,
            upper_bound = upper.bound,
            names = if_else(
              is.na(group_name) | !nzchar(group_name),
              paste0("Group ", group),
              group_name
            )
          ) %>%
          select(-group_name)
        tag_rr_list[[model_name]] <- tag_dt
      }

      tag_rr_all <- bind_rows(tag_rr_list, .id = "Model")
      if (rr_nonneg_only) {
        tag_rr_all <- tag_rr_all %>% filter(is.finite(rr), rr > 0)
      }
      if (nrow(tag_rr_all) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No tag reporting-rate data"))

      x_seq <- seq(0, 1, length.out = 500)
      prior_curve <- bind_rows(lapply(seq_len(nrow(tag_rr_all)), function(i) {
        data.frame(Model = tag_rr_all$Model[i], group = tag_rr_all$group[i], names = tag_rr_all$names[i], x = x_seq,
                   density = dnorm(x_seq, mean = tag_rr_all$prior_mean[i], sd = tag_rr_all$prior_sd[i]))
      }))
      # Keep numeric panel IDs for ordering, and keep model-specific names for labels.
      tag_rr_all <- tag_rr_all %>%
        mutate(
          panel_id = paste0("G", group),
          panel_order = as.numeric(group),
          panel = panel_id
        )
      prior_curve <- prior_curve %>%
        mutate(
          panel_id = paste0("G", group),
          panel_order = as.numeric(group),
          panel = panel_id
        )
      panel_labels <- tag_rr_all %>%
        distinct(panel, names) %>%
        { stats::setNames(.$names, .$panel) }
      panel_levels <- tag_rr_all %>%
        distinct(panel, panel_order) %>%
        arrange(panel_order, panel) %>%
        pull(panel)
      if (length(panel_levels) > 0) {
        tag_rr_all <- tag_rr_all %>% mutate(panel = factor(panel, levels = panel_levels))
        prior_curve <- prior_curve %>% mutate(panel = factor(panel, levels = panel_levels))
      }

      compatible <- is_compatible_by(tag_rr_all, "names")
      if (!overlay || compatible) {
        p <- ggplot() +
          geom_line(data = prior_curve, aes(x = x, y = density), color = "black", linewidth = 1) +
          geom_vline(data = tag_rr_all, aes(xintercept = rr, color = Model), linewidth = 1.1) +
          geom_vline(data = tag_rr_all, aes(xintercept = upper_bound), color = "blue", linewidth = 0.9, linetype = "dashed") +
          coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol, labeller = as_labeller(panel_labels)) +
          scale_color_manual(values = scenario_colors, drop = FALSE) +
          labs(x = "Reporting rate", y = "Density",
               title = if (overlay) "Tag Reporting Rates (Overlay)" else paste("Tag Reporting Rates -", selected_models[1]),
               color = "Model") +
          theme_bw() +
          theme(strip.text = element_text(size = 9, face = "bold"), strip.background = element_rect(fill = "gray90"), panel.grid.minor = element_blank(),
                legend.position = if (overlay) "bottom" else "none")
        return(p)
      }

      tag_rr_all <- tag_rr_all %>%
        mutate(panel = paste(Model, names, sep = " | "))
      prior_curve <- prior_curve %>%
        mutate(panel = paste(Model, names, sep = " | "))
      panel_levels_incompat <- tag_rr_all %>%
        distinct(Model, group, panel) %>%
        arrange(factor(Model, levels = selected_models), as.numeric(group), panel) %>%
        pull(panel)
      if (length(panel_levels_incompat) > 0) {
        panel_levels_incompat <- unique(panel_levels_incompat)
        tag_rr_all <- tag_rr_all %>% mutate(panel = factor(panel, levels = panel_levels_incompat))
        prior_curve <- prior_curve %>% mutate(panel = factor(panel, levels = panel_levels_incompat))
      }
      return(
        ggplot() +
          geom_line(data = prior_curve, aes(x = x, y = density), color = "black", linewidth = 1) +
          geom_vline(data = tag_rr_all, aes(xintercept = rr), linewidth = 1.1, color = "#2c7fb8") +
          geom_vline(data = tag_rr_all, aes(xintercept = upper_bound), color = "blue", linewidth = 0.9, linetype = "dashed") +
          coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
          labs(x = "Reporting rate", y = "Density", title = "Tag Reporting Rates (Model-specific panels; incompatible groups)") +
          theme_bw() +
          theme(strip.text = element_text(size = 8, face = "bold"), strip.background = element_rect(fill = "gray90"), panel.grid.minor = element_blank(), legend.position = "none")
      )
    }

    tagtemp_nonnull <- TagTempOut_list[!sapply(TagTempOut_list, is.null)]
    if (length(tagtemp_nonnull) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No temporary_tag_report loaded"))
    scenarios_name <- names(tagtemp_nonnull)
    seasons_per_model <- seasons_per_model[scenarios_name]

    if (mode == "returns_all") {
      mixing_periods <- pm_get_mixing_periods(
        scenarios_name,
        info_list = subset_named(rv$Info_list, scenarios_name),
        config_path = c(file.path("config", "mixing_periods.csv"), file.path("..", "config", "mixing_periods.csv"))
      )
      tag_all <- pm_apply_mixing_filter(tag_temp_out_list = tagtemp_nonnull, mixing_periods = mixing_periods, seasons_per_model = seasons_per_model)

      tag_summary <- tag_all %>%
        group_by(Model, recap_ts) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), mixing_period = first(mixing_period), .groups = "drop")

      tag_summary <- pm_pad_model_time(
        data = tag_summary, models = scenarios_name, seasons_per_model = seasons_per_model,
        time_col = "recap_ts", value_cols = c("recap_obs", "recap_pred")
      ) %>% left_join(data.frame(Model = names(mixing_periods), mixing_period = mixing_periods), by = "Model")
      tag_summary <- add_time_x(tag_summary, "recap_ts")
      tag_summary <- apply_year_filter(tag_summary)

      if (overlay) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(aes(y = recap_obs), na.rm = TRUE, size = 1.2, alpha = 0.6) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.9) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = "Tag Returns Over Time (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.9, color = "#2c7fb8") +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = paste("Tag Returns Over Time -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none")
      )
    }

    if (mode == "returns_group") {
      mixing_periods <- pm_get_mixing_periods(
        scenarios_name,
        info_list = subset_named(rv$Info_list, scenarios_name),
        config_path = c(file.path("config", "mixing_periods.csv"), file.path("..", "config", "mixing_periods.csv"))
      )
      tag_recapture_map <- bind_rows(lapply(scenarios_name, function(m) {
        pm_build_tag_recapture_map(rv$FISHERY_MAPS[[m]], include_index = FALSE) %>% mutate(Model = m)
      }))
      tag_all <- pm_apply_mixing_filter(tag_temp_out_list = tagtemp_nonnull, mixing_periods = mixing_periods, seasons_per_model = seasons_per_model)
      tag_all <- tag_all %>% left_join(tag_recapture_map, by = c("Model", "recap.fishery" = "fishery"))

      tag_summary <- tag_all %>%
        filter(!is.na(tag_recapture_group)) %>%
        group_by(Model, tag_recapture_group, tag_recapture_name, recap_ts) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      time_grid <- pm_pad_model_time(
        data = tag_summary, models = scenarios_name, seasons_per_model = seasons_per_model,
        time_col = "recap_ts", value_cols = c("recap_obs", "recap_pred")
      )

      all_groups <- unique(tag_summary$tag_recapture_group)
      all_names <- tag_recapture_map %>% select(Model, tag_recapture_group, tag_recapture_name) %>% distinct()

      tag_summary <- time_grid %>%
        select(Model, recap_ts) %>%
        tidyr::crossing(tag_recapture_group = all_groups) %>%
        left_join(all_names, by = c("Model", "tag_recapture_group")) %>%
        left_join(tag_summary, by = c("Model", "tag_recapture_group", "tag_recapture_name", "recap_ts")) %>%
        mutate(recap_obs = replace_na(recap_obs, 0), recap_pred = replace_na(recap_pred, 0), Model = factor(Model, levels = scenarios_name))
      tag_summary <- tag_summary %>% filter(!is.na(tag_recapture_name))
      tag_summary <- add_time_x(tag_summary, "recap_ts")
      tag_summary <- apply_year_filter(tag_summary)

      compatible <- is_compatible_by(tag_summary, "tag_recapture_name")
      if (overlay && compatible) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(aes(y = recap_obs), na.rm = TRUE, size = 0.9, alpha = 0.6) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7) +
            facet_wrap(~ tag_recapture_name, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = "Tag Returns by Recapture Group (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"),
                  strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "bottom")
        )
      }

      if (overlay && !compatible) {
        tag_summary <- tag_summary %>% mutate(panel = paste(Model, tag_recapture_name, sep = " | "))
        return(
          ggplot(tag_summary, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 0.9, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = "Tag Returns by Recapture Group (Model-specific panels; incompatible groups)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"),
                  strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "none")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7, color = "#2c7fb8") +
          facet_wrap(~ tag_recapture_name, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = paste("Tag Returns by Recapture Group -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "none")
      )
    }

    tag_releases <- pm_get_tag_releases(subset_named(TagOut_list, scenarios_name))

    if (mode == "attr_all") {
      tag_all <- pm_add_period_at_liberty(
        tag_data = bind_rows(tagtemp_nonnull, .id = "Model"),
        tag_releases = tag_releases,
        seasons_per_model = seasons_per_model
      )

      tag_summary <- tag_all %>% group_by(Model, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary <- pm_pad_period_series(
        data = tag_summary, models = scenarios_name, group_cols = character(0),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary <- add_time_x(tag_summary, "period_at_liberty")
      tag_summary <- apply_year_filter(tag_summary)

      tag_obs_only <- pm_build_obs_reference(tag_summary, keys = c("x"), obs_col = "recap_obs", fun = median)

      if (overlay) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 1) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = "Tag Attrition (Overlay)", color = "Model") +
            theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 2, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 1, color = "#2c7fb8") +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = paste("Tag Attrition -", selected_models[1])) +
          theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none")
      )
    }

    if (mode == "attr_program") {
      tag_all <- pm_add_period_at_liberty(
        tag_data = bind_rows(tagtemp_nonnull, .id = "Model"),
        tag_releases = tag_releases,
        seasons_per_model = seasons_per_model
      )

      tag_summary_program <- tag_all %>%
        group_by(Model, program, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary_program <- pm_pad_period_series(
        data = tag_summary_program, models = scenarios_name, group_cols = c("program"),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary_program <- add_time_x(tag_summary_program, "period_at_liberty")
      tag_summary_program <- apply_year_filter(tag_summary_program)

      tag_obs_only <- pm_build_obs_reference(tag_summary_program, keys = c("program", "x"), obs_col = "recap_obs", fun = median)

      compatible <- is_compatible_by(tag_summary_program, "program")
      if (overlay && compatible) {
        return(
          ggplot(tag_summary_program, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8) +
            facet_wrap(~ program, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Program (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      if (overlay && !compatible) {
        tag_summary_program <- tag_summary_program %>% mutate(panel = paste(Model, program, sep = " | "))
        return(
          ggplot(tag_summary_program, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Program (Model-specific panels; incompatible programs)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(tag_summary_program, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
          facet_wrap(~ program, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = paste("Tag Attrition by Program -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    if (mode == "attr_region") {
      tag_all <- bind_rows(tagtemp_nonnull, .id = "Model")
      region_map <- bind_rows(lapply(scenarios_name, function(m) {
        rv$FISHERY_MAPS[[m]][, c("fishery", "region")] %>% mutate(Model = m)
      }))
      tag_all <- tag_all %>% left_join(region_map, by = c("Model", "recap.fishery" = "fishery")) %>% rename(recap.region = region)
      tag_all <- pm_add_period_at_liberty(tag_data = tag_all, tag_releases = tag_releases, seasons_per_model = seasons_per_model)

      tag_summary_region <- tag_all %>%
        group_by(Model, recap.region, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary_region <- pm_pad_period_series(
        data = tag_summary_region, models = scenarios_name, group_cols = c("recap.region"),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary_region <- add_time_x(tag_summary_region, "period_at_liberty")
      tag_summary_region <- apply_year_filter(tag_summary_region)

      tag_obs_only <- pm_build_obs_reference(tag_summary_region, keys = c("recap.region", "x"), obs_col = "recap_obs", fun = median)
      compatible <- is_compatible_by(tag_summary_region, "recap.region")

      if (overlay && compatible) {
        return(
          ggplot(tag_summary_region, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8) +
            facet_wrap(~ recap.region, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Recapture Region (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      if (overlay && !compatible) {
        tag_summary_region <- tag_summary_region %>% mutate(panel = paste(Model, recap.region, sep = " | "))
        return(
          ggplot(tag_summary_region, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Recapture Region (Model-specific panels; incompatible regions)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(tag_summary_region, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
          facet_wrap(~ recap.region, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = paste("Tag Attrition by Recapture Region -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    if (mode == "recap_pressure") {
      release_dim <- if (is.null(input$tag_pressure_release_dim)) "rel_group" else input$tag_pressure_release_dim
      recap_dim <- if (is.null(input$tag_pressure_recap_dim)) "recap_fishery" else input$tag_pressure_recap_dim
      correction <- suppressWarnings(as.numeric(input$tag_pressure_correction))
      if (!is.finite(correction) || correction < 0) correction <- 1e-4
      cap <- suppressWarnings(as.numeric(input$tag_pressure_cap))
      if (!is.finite(cap) || cap <= 0) cap <- 3

      tag_all <- bind_rows(tagtemp_nonnull, .id = "Model")
      region_map <- bind_rows(lapply(scenarios_name, function(m) {
        rv$FISHERY_MAPS[[m]][, c("fishery", "region")] %>% mutate(Model = m)
      }))
      tag_recapture_map <- bind_rows(lapply(scenarios_name, function(m) {
        pm_build_tag_recapture_map(rv$FISHERY_MAPS[[m]], include_index = FALSE) %>% mutate(Model = m)
      }))

      tag_all <- tag_all %>%
        left_join(region_map, by = c("Model", "recap.fishery" = "fishery")) %>%
        rename(recap.region = region) %>%
        left_join(tag_recapture_map, by = c("Model", "recap.fishery" = "fishery")) %>%
        pm_add_period_at_liberty(tag_releases = tag_releases, seasons_per_model = seasons_per_model) %>%
        mutate(
          release_panel = if (identical(release_dim, "rel_region")) {
            paste0("Release region ", rel.region)
          } else {
            paste0("Release group ", rel.group, " | R", rel.region)
          },
          recap_panel = if (identical(recap_dim, "recap_region")) {
            paste0("Recap region ", recap.region)
          } else {
            dplyr::if_else(
              is.na(tag_recapture_name) | !nzchar(tag_recapture_name),
              paste0("Fishery ", recap.fishery),
              as.character(tag_recapture_name)
            )
          },
          recap_order = if (identical(recap_dim, "recap_region")) as.numeric(recap.region) else as.numeric(tag_recapture_group),
          period_x = as.numeric(period_at_liberty)
        )

      if (identical(time_mode, "year")) {
        spm <- seasons_per_model[as.character(tag_all$Model)]
        spm[!is.finite(spm) | spm <= 0] <- 4
        tag_all <- tag_all %>%
          mutate(period_x = floor(as.numeric(period_at_liberty) / as.numeric(spm)))
        x_label <- "Time at liberty (years)"
      } else {
        x_label <- "Time at liberty (model steps; usually quarters)"
      }

      pressure <- tag_all %>%
        filter(!is.na(release_panel), !is.na(recap_panel), is.finite(period_x)) %>%
        group_by(Model, release_panel, recap_panel, recap_order, period_x) %>%
        summarise(
          recap_obs = sum(recap.obs, na.rm = TRUE),
          recap_pred = sum(recap.pred, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(recap_obs > 0 | recap_pred > 0) %>%
        mutate(
          ratio = (recap_obs + correction) / (recap_pred + correction),
          log2_ratio = log2(ratio),
          log2_ratio_capped = pmax(pmin(log2_ratio, cap), -cap),
          tooltip_label = sprintf("O %.1f / E %.1f", recap_obs, recap_pred)
        )

      if (nrow(pressure) == 0) {
        return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No recapture pressure data after filtering"))
      }

      release_levels <- pressure %>%
        distinct(Model, release_panel) %>%
        arrange(factor(Model, levels = scenarios_name), release_panel) %>%
        pull(release_panel) %>%
        unique()
      recap_levels <- pressure %>%
        distinct(recap_panel, recap_order) %>%
        arrange(recap_order, recap_panel) %>%
        pull(recap_panel) %>%
        unique()

      pressure <- pressure %>%
        mutate(
          Model = factor(Model, levels = scenarios_name),
          release_panel = factor(release_panel, levels = release_levels),
          recap_panel = factor(recap_panel, levels = rev(recap_levels))
        )

      legend_breaks <- unique(c(-cap, -1, 0, 1, cap))
      legend_labels <- vapply(legend_breaks, function(b) {
        if (identical(b, -cap)) return(paste0("<= ", round(2^-cap, 2), "x"))
        if (identical(b, cap)) return(paste0(">= ", round(2^cap, 1), "x"))
        paste0(format(round(2^b, 2), trim = TRUE), "x")
      }, character(1))

      facet_formula <- if (overlay) {
        Model ~ release_panel
      } else {
        ~ release_panel
      }

      return(
        ggplot(pressure, aes(x = period_x, y = recap_panel, fill = log2_ratio_capped)) +
          geom_tile(color = "white", linewidth = 0.15) +
          geom_text(
            data = pressure %>% filter(abs(log2_ratio) >= 1, recap_obs >= 1),
            aes(label = round(ratio, 1)),
            size = 2.4,
            color = "black",
            na.rm = TRUE
          ) +
          facet_wrap(facet_formula, ncol = if (overlay) NULL else facet_ncol, scales = "free_y") +
          scale_fill_gradient2(
            "Observed /\nexpected",
            low = "#2166ac",
            mid = "white",
            high = "#b2182b",
            midpoint = 0,
            limits = c(-cap, cap),
            breaks = legend_breaks,
            labels = legend_labels
          ) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 8), expand = expansion(add = 0.1)) +
          labs(
            x = x_label,
            y = if (identical(recap_dim, "recap_region")) "Recapture region" else "Recapture fishery/group",
            title = if (overlay) "Observed / Expected Recapture Pressure (Overlay)" else paste("Observed / Expected Recapture Pressure -", selected_models[1]),
            subtitle = paste0("R = (observed + ", correction, ") / (expected + ", correction, "); red = under-predicted recaptures, blue = over-predicted")
          ) +
          theme_bw() +
          theme(
            panel.grid = element_blank(),
            strip.background = element_rect(fill = "gray90"),
            strip.text = element_text(face = "bold", size = 8),
            axis.text.y = element_text(size = 7),
            legend.position = "bottom"
          )
      )
    }

    ggplot() + theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = paste("Unknown tag plot mode:", mode))
  })
  tagging_plot_reactive <- bindCache(
    tagging_plot_reactive,
    rv$data_loaded,
    input$model_dir,
    input$tag_scenarios,
    input$tag_plot,
    input$tag_time_mode,
    input$tag_years,
    input$tag_facet_ncol,
    input$tag_rr_nonneg_only,
    input$tag_pressure_release_dim,
    input$tag_pressure_recap_dim,
    input$tag_pressure_correction,
    input$tag_pressure_cap
  )
  observeEvent(list(input$live_update_plots, input$tag_scenarios, input$tag_plot,
                    input$tag_time_mode, input$tag_years, input$tag_facet_ncol,
                    input$tag_rr_nonneg_only,
                    input$tag_pressure_release_dim, input$tag_pressure_recap_dim,
                    input$tag_pressure_correction, input$tag_pressure_cap,
                    input$tag_plot_height, input$tag_plot_width), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$tag_scenarios) == 0) return()
    tag_filters_applied(isolate(tag_filters_current()))
    tag_live_update_nonce(isolate(tag_live_update_nonce()) + 1)
  }, ignoreInit = TRUE)
  observeEvent(input$tag_apply_filters, {
    tag_filters_applied(isolate(tag_filters_current()))
  }, ignoreInit = TRUE)
  observeEvent(list(rv$initial_render_nonce, input$tag_scenarios, input$tag_years), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= tag_last_initialized_nonce()) return()
    if (length(input$tag_scenarios) == 0 || length(input$tag_years) == 0) return()
    tag_last_initialized_nonce(rv$initial_render_nonce)
    tag_filters_applied(isolate(tag_filters_current()))
  }, ignoreInit = TRUE)
  tagging_plot_reactive <- bindEvent(tagging_plot_reactive, rv$initial_render_nonce, input$tag_apply_filters, tag_live_update_nonce(), ignoreInit = FALSE)


  output$tagging_plot_output_ui <- renderUI({
    filters <- tag_filters_applied()
    h <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_height)) else suppressWarnings(as.integer(input$tag_plot_height))
    w <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_width)) else suppressWarnings(as.integer(input$tag_plot_width))
    if (!is.finite(h)) h <- 900
    if (!is.finite(w)) w <- 1200
    h <- min(max(h, 450), 1800)
    w <- min(max(w, 700), 2200)

    plotOutput("tagging_plot_output", height = paste0(h, "px"), width = paste0(w, "px"))
  })

  output$tagging_plot_output <- renderPlot({
    tagging_plot_reactive()
  })
  mod_sections_download("tagging", "Tagging Dynamics Plot", tagging_plot_reactive, input, session, output)
}

mod_fishery_process_ui <- function() {
  tabItem(
    tabName = "fishery_process",
    h2("Fishery Process Dynamics", style = "color: #f39c12;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "warning",
        pickerInput("fishery_process_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("fishery_process_plot", "Plot:", choices = c(
          "Fishery Selectivity (Age)" = "selectivity_age",
          "Fishery Selectivity (Length)" = "selectivity_length",
          "Fishery Selectivity (Weight)" = "selectivity_weight",
          "Regional Movement Matrix" = "movement"
        )),
        pickerInput(
          "fishery_process_fisheries",
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
            liveSearchPlaceholder = "Search fisheries..."
          )
        ),
        selectInput("fishery_process_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "4"),
        actionButton("fishery_process_apply_filters", "Apply", class = "btn-warning", style = "width: 100%;"),
        tags$small("Selections update the plot when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_fishery_process_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(
        title = "Plot",
        width = 9,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "fishery_process_plot_output",
          plotOutput("fishery_process_plot_output", height = "730px"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering fishery process plot...")
            )
          )
        )
      )
    )
  )
}

mod_fishery_process_server <- function(input, output, session, rv) {
  fishery_process_live_update_nonce <- reactiveVal(0)
  fishery_process_filters_current <- reactive({
    list(
      scenarios = input$fishery_process_scenarios,
      plot = if (is.null(input$fishery_process_plot)) "selectivity_age" else input$fishery_process_plot,
      fisheries = input$fishery_process_fisheries,
      facet_ncol = input$fishery_process_facet_ncol
    )
  })
  fishery_process_filters_applied <- reactiveVal(NULL)
  fishery_process_last_initialized_nonce <- reactiveVal(0)
  observeEvent(rv$data_loaded, {
    sc <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    updatePickerInput(session, "fishery_process_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(fishery_process_filters_current(), fishery_process_filters_applied())
    set_apply_pending(session, "fishery_process_apply_filters", pending)
  })

  observeEvent(list(input$fishery_process_scenarios, input$fishery_process_plot, rv$data_loaded), {
    req(rv$data_loaded, input$fishery_process_scenarios)
    sc <- input$fishery_process_scenarios
    if (length(sc) == 0) {
      updatePickerInput(session, "fishery_process_fisheries", choices = character(0), selected = character(0))
      return()
    }

    mode <- if (is.null(input$fishery_process_plot)) "selectivity_age" else input$fishery_process_plot
    if (!(mode %in% c("selectivity_age", "selectivity_length", "selectivity_weight"))) {
      updatePickerInput(session, "fishery_process_fisheries", choices = character(0), selected = character(0))
      return()
    }

    fish_ids <- unique(unlist(lapply(sc, function(m) {
      rep_obj <- rv$RepOut_list[[m]]
      if (is.null(rep_obj)) return(numeric(0))
      suppressWarnings(as.numeric(as.character(unique(as.data.frame(sel(rep_obj), drop = TRUE)$unit))))
    })))
    fish_ids <- sort(fish_ids[is.finite(fish_ids)])

    choices <- build_fishery_picker_choices(
      fish_ids = fish_ids,
      model_names = sc,
      fishery_maps = rv$FISHERY_MAPS
    )

    cur <- isolate(input$fishery_process_fisheries)
    if (is.null(cur) || length(cur) == 0) cur <- unname(choices)
    cur <- intersect(cur, unname(choices))
    if (length(cur) == 0) cur <- unname(choices)

    updatePickerInput(session, "fishery_process_fisheries", choices = choices, selected = cur)
  }, ignoreInit = FALSE)

  fishery_process_plot_reactive <- reactive({
    req(rv$data_loaded, input$fishery_process_scenarios)
    scenarios_name <- input$fishery_process_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    scenario_colors <- get_scenario_colors(scenarios_name)
    mode <- if (is.null(input$fishery_process_plot)) "selectivity_age" else input$fishery_process_plot
    facet_ncol <- suppressWarnings(as.integer(input$fishery_process_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 4
    facet_ncol <- min(max(facet_ncol, 1), 12)

    if (mode %in% c("selectivity_age", "selectivity_length", "selectivity_weight")) {
      selected_specs <- parse_fishery_picker_values(input$fishery_process_fisheries)

      sel_list <- lapply(names(RepOut_list), function(model_name) {
        tmp_rep <- RepOut_list[[model_name]]
        tmp_map <- rv$FISHERY_MAPS[[model_name]]
        tmp_sel <- sel(tmp_rep)
        tmp_df <- as.data.frame(tmp_sel, drop = TRUE) %>%
          mutate(age = as.numeric(as.character(age)), fishery = as.numeric(as.character(unit)), selectivity = data, Model = model_name) %>%
          select(Model, fishery, age, selectivity)

        if (mode == "selectivity_length") {
          tmp_laa <- c(aperm(mean_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
          tmp_df$length <- tmp_laa[tmp_df$age]
        }
        if (mode == "selectivity_weight") {
          tmp_waa <- c(aperm(mean_waa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
          tmp_df$weight <- tmp_waa[tmp_df$age]
        }

        tmp_df <- tmp_df %>%
          mutate(
            fishery_name = sapply(fishery, function(f) get_fishery_name(f, tmp_map)),
            fishery_label = ifelse(
              as.character(fishery_name) == as.character(fishery),
              as.character(fishery),
              as.character(fishery_name)
            ),
            fishery_panel = fishery_label
          )

        tmp_df
      })

      sel_data <- bind_rows(sel_list) %>%
        mutate(Model = factor(Model, levels = scenarios_name))
      if (nrow(selected_specs) > 0) {
        any_model_specific <- any(!is.na(selected_specs$Model) & nzchar(selected_specs$Model))
        if (any_model_specific) {
          plain_ids <- selected_specs$fishery[is.na(selected_specs$Model) | !nzchar(selected_specs$Model)]
          model_specs <- selected_specs[!is.na(selected_specs$Model) & nzchar(selected_specs$Model), , drop = FALSE]
          sel_data <- sel_data %>%
            mutate(.keep_selected = fishery %in% plain_ids)
          if (nrow(model_specs) > 0) {
            key_df <- unique(model_specs[, c("Model", "fishery"), drop = FALSE])
            key_df$.keep_pair <- TRUE
            sel_data <- sel_data %>%
              left_join(key_df, by = c("Model", "fishery")) %>%
              mutate(.keep_selected = .keep_selected | (!is.na(.keep_pair) & .keep_pair))
          }
          sel_data <- sel_data %>%
            filter(.keep_selected) %>%
            select(-any_of(c(".keep_selected", ".keep_pair")))
        } else {
          sel_data <- sel_data %>% filter(fishery %in% selected_specs$fishery)
        }
      }
      if (nrow(sel_data) == 0) {
        return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No selectivity data for selected fisheries"))
      }

      sel_data <- build_overlay_fishery_panel_labels(
        sel_data,
        scenario_col = "Model",
        id_col = "fishery",
        label_col = "fishery_label",
        out_col = "fishery_panel"
      )

      panel_levels_df <- sel_data %>%
        distinct(Model, fishery, fishery_panel) %>%
        mutate(
          Model = factor(Model, levels = scenarios_name),
          fishery_num = suppressWarnings(as.numeric(fishery))
        ) %>%
        arrange(fishery_num, Model, fishery_panel)
      panel_levels <- unique(as.character(panel_levels_df$fishery_panel))
      if (length(panel_levels) > 0) {
        sel_data <- sel_data %>% mutate(fishery_panel = factor(fishery_panel, levels = panel_levels))
      }

      if (mode == "selectivity_age") {
        return(
          ggplot(sel_data, aes(x = age, y = selectivity, color = Model)) +
            geom_line(linewidth = 1) +
            facet_wrap(~ fishery_panel, ncol = facet_ncol, scales = "free_y") +
            scale_color_manual("Model", values = scenario_colors) +
            labs(x = "Age class", y = "Selectivity", title = "Estimated Selectivity by Fishery (Age-based)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }
      if (mode == "selectivity_length") {
        return(
          ggplot(sel_data, aes(x = length, y = selectivity, color = Model)) +
            geom_line(linewidth = 1) +
            facet_wrap(~ fishery_panel, ncol = facet_ncol, scales = "free_y") +
            scale_color_manual("Model", values = scenario_colors) +
            labs(x = "Length (cm)", y = "Selectivity", title = "Estimated Selectivity by Fishery (Length-based)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(sel_data, aes(x = weight, y = selectivity, color = Model)) +
          geom_line(linewidth = 1) +
          facet_wrap(~ fishery_panel, ncol = facet_ncol, scales = "free_y") +
          scale_color_manual("Model", values = scenario_colors) +
          labs(x = "Weight (kg)", y = "Selectivity", title = "Estimated Selectivity by Fishery (Weight-based)") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    if (mode == "regional_sharing") {
      selected_specs <- parse_fishery_picker_values(input$fishery_process_fisheries)

      sharing_data <- bind_rows(lapply(names(ParOut_list), function(model_name) {
        par_obj <- ParOut_list[[model_name]]
        rep_obj <- RepOut_list[[model_name]]
        fmap <- rv$FISHERY_MAPS[[model_name]]
        if (is.null(par_obj) || is.null(rep_obj) || is.null(fmap)) return(NULL)

        fish_ids <- sort(unique(suppressWarnings(as.numeric(fmap$fishery))))
        fish_ids <- fish_ids[is.finite(fish_ids)]
        if (length(fish_ids) == 0) return(NULL)

        sel_obj <- tryCatch(sel(rep_obj), error = function(e) tryCatch(sel(par_obj), error = function(e2) NULL))
        sel_df <- if (!is.null(sel_obj)) {
          as.data.frame(sel_obj, drop = TRUE) %>%
            mutate(
              fishery = suppressWarnings(as.numeric(as.character(unit))),
              age = suppressWarnings(as.numeric(as.character(age))),
              selectivity = as.numeric(data)
            ) %>%
            filter(is.finite(fishery), is.finite(age), is.finite(selectivity)) %>%
            group_by(fishery) %>%
            arrange(age, .by_group = TRUE) %>%
            summarise(sel_signature = paste(round(selectivity, 4), collapse = "|"), .groups = "drop") %>%
            mutate(selectivity_group = as.integer(factor(sel_signature, levels = unique(sel_signature))))
        } else {
          tibble(fishery = fish_ids, selectivity_group = NA_integer_)
        }

        q_vals <- tryCatch(as.numeric(av_q_coffs(par_obj)), error = function(e) rep(NA_real_, length(fish_ids)))
        if (length(q_vals) < max(fish_ids, na.rm = TRUE)) {
          q_lookup <- rep(NA_real_, max(fish_ids, na.rm = TRUE))
          q_lookup[seq_along(q_vals)] <- q_vals
        } else {
          q_lookup <- q_vals
        }

        tag_grp_mat <- tryCatch(tag_fish_rep_grp(par_obj), error = function(e) NULL)
        tag_rate_mat <- tryCatch(tag_fish_rep_rate(par_obj), error = function(e) NULL)
        tag_summary <- lapply(fish_ids, function(fid) {
          grp_vals <- if (!is.null(tag_grp_mat) && ncol(tag_grp_mat) >= fid) {
            suppressWarnings(as.numeric(tag_grp_mat[, fid]))
          } else {
            NA_real_
          }
          grp_vals <- sort(unique(grp_vals[is.finite(grp_vals) & grp_vals > 0]))
          grp_label <- if (length(grp_vals) == 0) {
            NA_character_
          } else if (length(grp_vals) <= 3) {
            paste(grp_vals, collapse = ",")
          } else {
            paste0("multi:", length(grp_vals))
          }

          rr_vals <- if (!is.null(tag_rate_mat) && ncol(tag_rate_mat) >= fid) {
            suppressWarnings(as.numeric(tag_rate_mat[, fid]))
          } else {
            NA_real_
          }
          rr_vals <- rr_vals[is.finite(rr_vals) & rr_vals > 0]
          tibble(
            fishery = fid,
            tag_reporting_group = grp_label,
            tag_reporting_rate = if (length(rr_vals) > 0) median(rr_vals, na.rm = TRUE) else NA_real_
          )
        }) %>% bind_rows()

        fmap %>%
          transmute(
            Model = model_name,
            fishery = suppressWarnings(as.numeric(fishery)),
            region = suppressWarnings(as.numeric(region)),
            fishery_name = as.character(fishery_name)
          ) %>%
          filter(fishery %in% fish_ids, is.finite(region)) %>%
          left_join(sel_df %>% select(fishery, selectivity_group), by = "fishery") %>%
          left_join(tag_summary, by = "fishery") %>%
          mutate(
            avg_q = q_lookup[fishery],
            log10_q = log10(avg_q),
            fishery_label = if_else(is.na(fishery_name) | !nzchar(fishery_name), paste0("F", fishery), fishery_name)
          )
      }))

      if (nrow(sharing_data) == 0) {
        return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No sharing-footprint data available"))
      }

      if (nrow(selected_specs) > 0) {
        any_model_specific <- any(!is.na(selected_specs$Model) & nzchar(selected_specs$Model))
        if (any_model_specific) {
          plain_ids <- selected_specs$fishery[is.na(selected_specs$Model) | !nzchar(selected_specs$Model)]
          model_specs <- selected_specs[!is.na(selected_specs$Model) & nzchar(selected_specs$Model), , drop = FALSE]
          sharing_data <- sharing_data %>% mutate(.keep_selected = fishery %in% plain_ids)
          if (nrow(model_specs) > 0) {
            key_df <- unique(model_specs[, c("Model", "fishery"), drop = FALSE])
            key_df$.keep_pair <- TRUE
            sharing_data <- sharing_data %>%
              left_join(key_df, by = c("Model", "fishery")) %>%
              mutate(.keep_selected = .keep_selected | (!is.na(.keep_pair) & .keep_pair))
          }
          sharing_data <- sharing_data %>%
            filter(.keep_selected) %>%
            select(-any_of(c(".keep_selected", ".keep_pair")))
        } else {
          sharing_data <- sharing_data %>% filter(fishery %in% selected_specs$fishery)
        }
      }

      if (nrow(sharing_data) == 0) {
        return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No selected fisheries for sharing footprint"))
      }

      sharing_data <- build_overlay_fishery_panel_labels(
        sharing_data,
        scenario_col = "Model",
        id_col = "fishery",
        label_col = "fishery_label",
        out_col = "fishery_panel"
      ) %>%
        mutate(
          Model = factor(Model, levels = scenarios_name),
          region_label = paste0("R", region),
          fishery_panel = paste0(region_label, " | ", fishery_panel),
          fishery_panel = factor(fishery_panel, levels = rev(unique(fishery_panel[order(region, fishery)]))),
          tag_reporting_group = factor(if_else(is.na(tag_reporting_group), "NA", tag_reporting_group)),
          selectivity_group = factor(if_else(is.na(selectivity_group), "NA", paste0("S", selectivity_group)))
        )

      p_tag <- ggplot(sharing_data, aes(x = region_label, y = fishery_panel, fill = tag_reporting_group)) +
        geom_tile(color = "white", linewidth = 0.25) +
        geom_text(aes(label = tag_reporting_group), size = 2.4) +
        facet_wrap(~ Model, ncol = min(length(scenarios_name), facet_ncol)) +
        scale_fill_viridis_d("Tag reporting\ngroup", option = "D", na.value = "gray90") +
        labs(x = NULL, y = NULL, title = "Tag Reporting Group Footprint") +
        theme_bw() +
        theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 0), axis.text.y = element_text(size = 7),
              strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8),
              legend.position = "right")

      p_sel <- ggplot(sharing_data, aes(x = region_label, y = fishery_panel, fill = selectivity_group)) +
        geom_tile(color = "white", linewidth = 0.25) +
        geom_text(aes(label = selectivity_group), size = 2.4) +
        facet_wrap(~ Model, ncol = min(length(scenarios_name), facet_ncol)) +
        scale_fill_viridis_d("Selectivity\nshape group", option = "C", na.value = "gray90") +
        labs(x = NULL, y = NULL, title = "Selectivity Shape Footprint") +
        theme_bw() +
        theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 0), axis.text.y = element_text(size = 7),
              strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8),
              legend.position = "right")

      p_q <- ggplot(sharing_data, aes(x = region_label, y = fishery_panel, fill = log10_q)) +
        geom_tile(color = "white", linewidth = 0.25) +
        geom_text(aes(label = if_else(is.finite(log10_q), sprintf("%.2f", log10_q), "")), size = 2.3) +
        facet_wrap(~ Model, ncol = min(length(scenarios_name), facet_ncol)) +
        scale_fill_viridis_c("log10(avg q)", option = "B", na.value = "gray90") +
        labs(x = "Fishery region", y = NULL, title = "Catchability Level Footprint") +
        theme_bw() +
        theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 0), axis.text.y = element_text(size = 7),
              strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8),
              legend.position = "right")

      title <- ggdraw() +
        draw_label(
          "Regional Sharing Footprint: repeated colours across regions indicate shared or indistinguishable fishery-process structure",
          fontface = "bold",
          x = 0,
          hjust = 0,
          size = 12
        )

      return(
        plot_grid(
          title,
          p_tag,
          p_sel,
          p_q,
          ncol = 1,
          rel_heights = c(0.08, 1, 1, 1)
        )
      )
    }

    move_all <- bind_rows(lapply(names(ParOut_list), function(model_name) {
      move_array <- diff_coffs_age_period(ParOut_list[[model_name]])
      move_df <- as.data.frame.table(move_array, stringsAsFactors = FALSE)
      colnames(move_df) <- c("from", "to", "age", "period", "value")
      move_df <- move_df %>%
        mutate(from = as.numeric(from), to = as.numeric(to), age = as.numeric(age), period = as.numeric(period), value = as.numeric(value), Model = model_name)
      move_df %>% group_by(Model, period, from, to) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% mutate(Quarter = paste0("Quarter ", period), panel = paste(Model, "-", Quarter))
    }))

    ggplot(move_all, aes(x = to, y = from, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      facet_wrap(~ panel, ncol = facet_ncol) +
      scale_fill_gradientn("Diffusion\nCoefficient", colors = c("royalblue3", "deepskyblue1", "gold", "orange1", "indianred1", "firebrick2", "#AC2020"), limits = c(0, NA)) +
      scale_x_continuous(breaks = sort(unique(move_all$to)), labels = paste0("R", sort(unique(move_all$to)))) +
      scale_y_continuous(breaks = sort(unique(move_all$from)), labels = paste0("R", sort(unique(move_all$from))), trans = "reverse") +
      labs(x = "From Region", y = "To Region", title = "Estimated Regional Movement by Quarter") +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_text(face = "bold", size = 10), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9), axis.text = element_text(size = 8))
  })
  fishery_process_plot_reactive <- bindCache(
    fishery_process_plot_reactive,
    rv$data_loaded,
    input$model_dir,
    input$fishery_process_scenarios,
    input$fishery_process_plot,
    input$fishery_process_fisheries,
    input$fishery_process_facet_ncol
  )
  observeEvent(list(input$live_update_plots, input$fishery_process_scenarios,
                    input$fishery_process_plot, input$fishery_process_fisheries,
                    input$fishery_process_facet_ncol), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$fishery_process_scenarios) == 0) return()
    fishery_process_filters_applied(isolate(fishery_process_filters_current()))
    fishery_process_live_update_nonce(isolate(fishery_process_live_update_nonce()) + 1)
  }, ignoreInit = TRUE)
  observeEvent(input$fishery_process_apply_filters, {
    fishery_process_filters_applied(isolate(fishery_process_filters_current()))
  }, ignoreInit = TRUE)
  observeEvent(list(rv$initial_render_nonce, input$fishery_process_scenarios, input$fishery_process_fisheries), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= fishery_process_last_initialized_nonce()) return()
    if (length(input$fishery_process_scenarios) == 0) return()
    fishery_process_last_initialized_nonce(rv$initial_render_nonce)
    fishery_process_filters_applied(isolate(fishery_process_filters_current()))
  }, ignoreInit = TRUE)
  fishery_process_plot_reactive <- bindEvent(fishery_process_plot_reactive, rv$initial_render_nonce, input$fishery_process_apply_filters, fishery_process_live_update_nonce(), ignoreInit = FALSE)


  output$fishery_process_plot_output <- renderPlot({
    fishery_process_plot_reactive()
  })
  mod_sections_download("fishery_process", "Fishery Process Plot", fishery_process_plot_reactive, input, session, output)
}

mod_population_biology_ui <- function() {
  tabItem(
    tabName = "population_biology",
    h2("Population Biology", style = "color: #605ca8;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "primary",
        pickerInput("population_biology_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("population_biology_plot", "Plot:", choices = c(
          "Stock-Recruitment Relationship" = "srr",
          "Maturity at Age" = "mat",
          "Natural Mortality at Age" = "natm",
          "Growth Curve" = "growth"
        )),
        conditionalPanel(
          condition = "input.population_biology_plot == 'growth'",
          checkboxInput("population_biology_show_growth_band", "Show growth band (LAA +/- 1.96 SD)", value = TRUE)
        ),
        selectInput("population_biology_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "2"),
        actionButton("population_biology_apply_filters", "Apply", class = "btn-primary", style = "width: 100%;"),
        tags$small("Selections update the plot when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_population_biology_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(
        title = "Plot",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "population_biology_plot_output",
          plotOutput("population_biology_plot_output", height = "730px"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering population biology plot...")
            )
          )
        )
      )
    )
  )
}

mod_population_biology_server <- function(input, output, session, rv) {
  population_biology_live_update_nonce <- reactiveVal(0)
  population_biology_filters_current <- reactive({
    list(
      scenarios = input$population_biology_scenarios,
      plot = if (is.null(input$population_biology_plot)) "srr" else input$population_biology_plot,
      facet_ncol = input$population_biology_facet_ncol,
      show_growth_band = isTRUE(input$population_biology_show_growth_band)
    )
  })
  population_biology_filters_applied <- reactiveVal(NULL)
  population_biology_last_initialized_nonce <- reactiveVal(0)
  observeEvent(rv$data_loaded, {
    sc <- names(rv$ParOut_list)
    updatePickerInput(session, "population_biology_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(population_biology_filters_current(), population_biology_filters_applied())
    set_apply_pending(session, "population_biology_apply_filters", pending)
  })

  population_biology_plot_reactive <- reactive({
    req(rv$data_loaded, input$population_biology_scenarios)
    scenarios_name <- input$population_biology_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    scenario_colors <- get_scenario_colors(scenarios_name)
    mode <- if (is.null(input$population_biology_plot)) "srr" else input$population_biology_plot
    facet_ncol <- suppressWarnings(as.integer(input$population_biology_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
    facet_ncol <- min(max(facet_ncol, 1), 12)

    if (mode == "srr") {
      adult_biomass <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        tmp_ab <- areaSums(adultBiomass(RepOut_list[[model_name]]))
        as.data.frame(tmp_ab, drop = TRUE) %>%
          mutate(year = as.numeric(as.character(year)), season = as.numeric(as.character(season)), sb = data, Model = model_name) %>%
          select(Model, year, season, sb)
      }))

      recruitment <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        tmp_rec <- areaSums(popN(RepOut_list[[model_name]])[1, ])
        as.data.frame(tmp_rec, drop = TRUE) %>%
          mutate(year = as.numeric(as.character(year)), season = as.numeric(as.character(season)), rec = data, Model = model_name) %>%
          select(Model, year, season, rec)
      }))

      srr_data <- adult_biomass %>%
        left_join(recruitment, by = c("Model", "year", "season")) %>%
        group_by(Model, year) %>%
        summarise(sb = mean(sb, na.rm = TRUE), rec = sum(rec, na.rm = TRUE), .groups = "drop") %>%
        mutate(Model = factor(Model, levels = scenarios_name))

      bh_data <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        bh_params <- srr(RepOut_list[[model_name]])
        max_sb_model <- max((srr_data %>% filter(Model == model_name))$sb, na.rm = TRUE) * 1.2
        sb_seq <- seq(0, max_sb_model, length = 100)
        a_val <- c(bh_params$a); b_val <- c(bh_params$b)
        data.frame(sb = sb_seq, rec = (sb_seq * a_val) / (b_val + sb_seq), Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      sb_units <- 1000; rec_units <- 1000000

      return(
        ggplot() +
          geom_line(data = bh_data, aes(x = sb / sb_units, y = rec / rec_units), color = "black", linewidth = 1.2) +
          geom_point(data = srr_data, aes(x = sb / sb_units, y = rec / rec_units, fill = year), shape = 21, color = "black", size = 2.5) +
          facet_wrap(~ Model, scales = "free", ncol = facet_ncol) +
          scale_fill_viridis_c("Year", option = "viridis") +
          ylim(c(0, NA)) +
          labs(x = paste0("Adult biomass (mt; ", format(sb_units, big.mark = ",", trim = TRUE, scientific = FALSE), "s)"),
               y = paste0("Recruitment (N; ", format(rec_units, big.mark = ",", trim = TRUE, scientific = FALSE), "s)"),
               title = "Stock-Recruitment Relationship by Model (Annual)") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 10))
      )
    }

    if (mode == "mat") {
      mat_age_data <- bind_rows(lapply(seq_along(ParOut_list), function(i) {
        tmp_par <- ParOut_list[[i]]
        model_name <- names(ParOut_list)[i]
        mat_vals <- mat(tmp_par)
        data.frame(age = 1:length(mat_vals), maturity = mat_vals, Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      return(
        ggplot(mat_age_data, aes(x = age, y = maturity, color = Model)) +
          geom_line(linewidth = 1.5) +
          scale_color_manual("Model", values = scenario_colors) +
          coord_cartesian(ylim = c(0, 1.05)) +
          labs(x = "Age class", y = "Maturity", title = "Maturity at Age") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
      )
    }

    if (mode == "natm") {
      m_age_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
        tmp_rep <- RepOut_list[[i]]
        model_name <- names(RepOut_list)[i]
        m_vals <- m_at_age(tmp_rep)
        data.frame(age = 1:length(m_vals), m = m_vals, Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      return(
        ggplot(m_age_data, aes(x = age, y = m, color = Model)) +
          geom_line(linewidth = 1.5) +
          scale_color_manual("Model", values = scenario_colors) +
          ylim(c(0, NA)) +
          labs(x = "Age class", y = "Natural mortality", title = "Natural Mortality at Age") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
      )
    }

    growth_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
      tmp_rep <- RepOut_list[[i]]
      model_name <- names(RepOut_list)[i]
      tmp_laa <- c(aperm(mean_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
      tmp_sd_laa <- c(aperm(sd_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
      tmp_lower <- tmp_laa - 1.96 * tmp_sd_laa
      tmp_upper <- tmp_laa + 1.96 * tmp_sd_laa
      data.frame(Model = model_name, age = 1:length(tmp_laa), length = tmp_laa, lower = tmp_lower, upper = tmp_upper)
    })) %>% mutate(Model = factor(Model, levels = scenarios_name))

    p_growth <- ggplot(growth_data, aes(x = age, y = length, color = Model, fill = Model))
    if (isTRUE(input$population_biology_show_growth_band)) {
      p_growth <- p_growth + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA)
    }
    p_growth +
      geom_line(linewidth = 1.5) +
      scale_color_manual("Model", values = scenario_colors) +
      scale_fill_manual("Model", values = scenario_colors) +
      labs(x = "Age class", y = "Length (cm)", title = "Model Growth Curve") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
  })
  population_biology_plot_reactive <- bindCache(
    population_biology_plot_reactive,
    rv$data_loaded,
    input$model_dir,
    input$population_biology_scenarios,
    input$population_biology_plot,
    input$population_biology_facet_ncol,
    input$population_biology_show_growth_band
  )
  observeEvent(list(input$live_update_plots, input$population_biology_scenarios,
                    input$population_biology_plot, input$population_biology_facet_ncol,
                    input$population_biology_show_growth_band), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$population_biology_scenarios) == 0) return()
    population_biology_filters_applied(isolate(population_biology_filters_current()))
    population_biology_live_update_nonce(isolate(population_biology_live_update_nonce()) + 1)
  }, ignoreInit = TRUE)
  observeEvent(input$population_biology_apply_filters, {
    population_biology_filters_applied(isolate(population_biology_filters_current()))
  }, ignoreInit = TRUE)
  observeEvent(list(rv$initial_render_nonce, input$population_biology_scenarios), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= population_biology_last_initialized_nonce()) return()
    if (length(input$population_biology_scenarios) == 0) return()
    population_biology_last_initialized_nonce(rv$initial_render_nonce)
    population_biology_filters_applied(isolate(population_biology_filters_current()))
  }, ignoreInit = TRUE)
  population_biology_plot_reactive <- bindEvent(population_biology_plot_reactive, rv$initial_render_nonce, input$population_biology_apply_filters, population_biology_live_update_nonce(), ignoreInit = FALSE)


  output$population_biology_plot_output <- renderPlot({
    population_biology_plot_reactive()
  })
  mod_sections_download("population_biology", "Population Biology Plot", population_biology_plot_reactive, input, session, output)
}
