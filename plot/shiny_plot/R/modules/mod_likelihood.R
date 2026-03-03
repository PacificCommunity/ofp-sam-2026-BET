mod_likelihood_ui <- function() {
  tabItem(
    tabName = "diagnostics",
    h2("Diagnostics", style = "color: #8e44ad;"),

    fluidRow(
      box(
        title = "Settings",
        width = 3,
        solidHeader = TRUE,
        status = "warning",

        pickerInput(
          "lik_scenarios",
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

        selectInput(
          "lik_profile_type",
          "Diagnostics Type:",
          choices = c(
            "Likelihood: Components" = "components",
            "Likelihood: CPUE by Fishery" = "cpues",
            "Likelihood: Length Frequencies" = "lfs",
            "Likelihood: Weight Frequencies" = "wfs",
            "Likelihood: Tagging" = "tagging",
            "Likelihood: CAL by Fishery" = "cal_fishery",
            "Likelihood: CAL by Year" = "cal_year",
            "Jitter Diagnostics" = "jitter",
            "Jitter Parameters" = "jitter_params",
            "Jitter Derived Quantities" = "jitter_derived",
            "Retrospective: Depletion & Spawning Potential" = "retro",
            "Hessian Diagnostics (PDH / SPD)" = "hessian"
          ),
          selected = "components"
        ),

        conditionalPanel(
          condition = "!['jitter', 'jitter_params', 'jitter_derived', 'retro', 'hessian'].includes(input.lik_profile_type)",
          pickerInput(
            "lik_groups",
            "Lines:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              selectAllText = "Select All",
              deselectAllText = "Deselect All",
              selectedTextFormat = "count > 3",
              countSelectedText = "{0} lines selected",
              liveSearch = TRUE,
              liveSearchPlaceholder = "Search lines...",
              size = 10
            )
          )
        ),
        conditionalPanel(
          condition = "['cpues', 'lfs', 'wfs', 'cal_fishery'].includes(input.lik_profile_type)",
          tagList(
            checkboxInput(
              "lik_split_by_region",
              "Split by region",
              value = FALSE
            ),
            pickerInput(
              "lik_regions",
              "Regions:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 3",
                countSelectedText = "{0} regions selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search regions...",
                size = 8
              )
            )
          )
        ),
        selectInput("lik_facet_ncol", "Facet columns:", choices = as.character(1:12), selected = "2"),
        conditionalPanel(
          condition = "input.lik_profile_type == 'jitter_params'",
          tagList(
            selectInput(
              "lik_jitter_param_view",
              "Jitter Param View:",
              choices = c(
                "Original vs jittered pars distribution" = "input",
                "Original vs final fitted pars distribution" = "final"
              ),
              selected = "input"
            ),
            conditionalPanel(
              condition = "input.lik_jitter_param_view == 'final'",
              checkboxInput(
                "lik_jitter_final_converged_only",
                "Converged only (max_grad <= 0.01)",
                value = FALSE
              )
            ),
            selectInput(
              "lik_jitter_param_metric",
              "Jitter Param Scale:",
              choices = c(
                "Parameter value" = "value",
                "Change from original" = "delta",
                "% change from original" = "pct_change"
              ),
              selected = "pct_change"
            )
          )
        ),
        actionButton("lik_apply_filters", "Apply", class = "btn-primary", style = "width: 100%;"),
        tags$small("Selections update the plot and tables when you click Apply.",
                   style = "display:block; margin-top:6px; color:#666;"),

        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton(
          "show_lik_download_modal",
          "Download Plot...",
          class = "btn-info",
          style = "width: 100%;",
          icon = icon("download")
        ),

        helpText(
          "Requires prof/scaler_* outputs (test_plot_output) in each scenario.",
          style = "margin-top: 10px; font-size: 11px; color: #666;"
        )
      ),

      box(
        title = "Diagnostics Plot",
        width = 9,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        div(
          class = "plot-loading-container",
          `data-output-id` = "likelihood_plot",
          plotOutput("likelihood_plot", height = "650px"),
          div(
            class = "plot-loading-overlay",
            div(
              class = "plot-loading-card",
              HTML("<span class='render-spinner'></span>Rendering diagnostics plot...")
            )
          )
        )
      )
    ),

    fluidRow(
      uiOutput("profile_target_info_ui"),
      uiOutput("likelihood_table_ui"),
      uiOutput("profile_gradient_table_ui")
    )
  )
}

mod_likelihood_server <- function(input, output, session, rv) {
  heavy_cache <- reactiveValues(
    retro = list(),
    hessian = list()
  )
  last_group_key <- reactiveVal(NULL)

  scenario_cache_key <- function(model_dir, scenario) {
    paste(
      normalizePath(model_dir, winslash = "/", mustWork = FALSE),
      as.character(scenario),
      sep = "::"
    )
  }

  get_cached_heavy <- function(bucket, key, builder) {
    store <- heavy_cache[[bucket]]
    if (!is.null(store[[key]])) return(store[[key]])
    value <- builder()
    store[[key]] <- value
    heavy_cache[[bucket]] <- store
    value
  }

  clear_heavy_cache <- function() {
    heavy_cache$retro <- list()
    heavy_cache$hessian <- list()
  }
  lik_live_update_nonce <- reactiveVal(0)
  fishery_region_types <- c("cpues", "lfs", "wfs", "cal_fishery")
  lik_filters_current <- reactive({
    list(
      scenarios = input$lik_scenarios,
      profile_type = if (is.null(input$lik_profile_type)) "components" else input$lik_profile_type,
      groups = input$lik_groups,
      regions = input$lik_regions,
      split_by_region = isTRUE(input$lik_split_by_region),
      facet_ncol = input$lik_facet_ncol,
      jitter_param_view = if (is.null(input$lik_jitter_param_view)) "input" else input$lik_jitter_param_view,
      jitter_final_converged_only = isTRUE(input$lik_jitter_final_converged_only),
      jitter_param_metric = if (is.null(input$lik_jitter_param_metric)) "pct_change" else input$lik_jitter_param_metric
    )
  })
  lik_filters_applied <- reactiveVal(NULL)
  lik_last_initialized_nonce <- reactiveVal(0)
  lik_filters <- reactive({
    lik_filters_applied()
  })

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(lik_filters_current(), lik_filters_applied())
    set_apply_pending(session, "lik_apply_filters", pending)
  })

  observeEvent(input$model_dir, {
    clear_heavy_cache()
  }, ignoreInit = TRUE)

  observeEvent(input$load_data, {
    clear_heavy_cache()
  }, ignoreInit = TRUE)

  observeEvent(input$lik_apply_filters, {
    lik_filters_applied(isolate(lik_filters_current()))
  }, ignoreInit = TRUE)

  # Update scenario choices when data is loaded
  observeEvent(rv$data_loaded, {
    req(rv$ParOut_list)
    map_models <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    current_selection <- isolate(input$lik_scenarios)
    if (is.null(current_selection) || length(current_selection) == 0) current_selection <- map_models
    current_selection <- intersect(current_selection, map_models)
    if (length(current_selection) == 0) current_selection <- map_models
    updatePickerInput(
      session,
      "lik_scenarios",
      choices = map_models,
      selected = current_selection
    )
  }, ignoreInit = TRUE)

  observeEvent(list(rv$data_loaded, input$lik_scenarios, input$lik_profile_type), {
    req(rv$data_loaded)

    type <- isolate(input$lik_profile_type)
    if (!(type %in% fishery_region_types)) {
      updatePickerInput(session, "lik_regions", choices = character(0), selected = character(0))
      return()
    }

    scenarios <- isolate(input$lik_scenarios)
    if (is.null(scenarios) || length(scenarios) == 0) {
      updatePickerInput(session, "lik_regions", choices = character(0), selected = character(0))
      return()
    }

    region_values <- sort(unique(unlist(lapply(scenarios, function(sc) {
      fish_map <- rv$FISHERY_MAPS[[sc]]
      if (is.null(fish_map) || !"region" %in% names(fish_map)) return(character(0))
      vals <- fish_map$region
      vals <- vals[!is.na(vals) & nzchar(trimws(as.character(vals)))]
      as.character(vals)
    }), use.names = FALSE)))

    current <- isolate(input$lik_regions)
    if (is.null(current) || length(current) == 0) {
      selected <- region_values
    } else {
      selected <- intersect(as.character(current), region_values)
      if (length(selected) == 0) selected <- region_values
    }

    updatePickerInput(session, "lik_regions", choices = region_values, selected = selected)
  }, ignoreInit = TRUE)

  # Safe read helper for scalar files
  safe_read_scalar <- function(path) {
    if (file.exists(path)) suppressWarnings(as.numeric(read.table(path))) else NA_real_
  }

  quantity_axis_label <- function(profile_data) {
    quantity_labels <- unlist(lapply(profile_data, function(x) x$quantity_label), use.names = FALSE)
    quantity_labels <- quantity_labels[nzchar(quantity_labels)]
    af172_vals <- unlist(lapply(profile_data, function(x) x$af172), use.names = FALSE)
    af172_vals <- af172_vals[is.finite(af172_vals)]
    is_depletion <- length(quantity_labels) > 0 && all(quantity_labels == "relative_depletion")

    if (is_depletion) {
      if (length(af172_vals) == 0) return("Biomass depletion")
      if (all(af172_vals > 0)) return("Adult biomass depletion")
      if (all(af172_vals == 0)) return("Total biomass depletion")
      return("Biomass depletion")
    }

    if (length(af172_vals) == 0) return(bquote("Average biomass (" * 10^3 * " MT)"))
    if (all(af172_vals > 0)) return(bquote("Average adult biomass (" * 10^3 * " MT)"))
    if (all(af172_vals == 0)) return(bquote("Average total biomass (" * 10^3 * " MT)"))
    bquote("Average biomass (" * 10^3 * " MT)")
  }

  scaler_quantity <- function(profile_entry, scl) {
    key <- as.character(scl)
    suppressWarnings(as.numeric(profile_entry$actual_quantity[[key]]))
  }

  safe_payload_numeric <- function(x, field) {
    value <- x[[field]]
    if (is.null(value) || length(value) == 0) return(NA_real_)
    suppressWarnings(as.numeric(value[[1]]))
  }

  profile_target_label <- function(quantity_label) {
    if (identical(quantity_label, "relative_depletion")) return("Target biomass depletion")
    if (identical(quantity_label, "avg_bio")) return("Average biomass")
    "Unknown"
  }

  profile_biomass_label <- function(af172) {
    if (!is.finite(af172)) return("Unknown")
    if (af172 > 0) return("Adult")
    "Total"
  }

  profile_period_window <- function(max_year, seasons, back_index) {
    if (!is.finite(max_year) || !is.finite(seasons) || seasons <= 0 || !is.finite(back_index)) {
      return(NA_character_)
    }

    end_step <- as.integer(round(max_year * seasons))
    period_step <- end_step - as.integer(round(back_index))
    if (!is.finite(period_step)) return(NA_character_)

    year_val <- ((period_step - 1) %/% seasons) + 1
    season_val <- ((period_step - 1) %% seasons) + 1
    paste0(year_val, " S", season_val)
  }

  profile_period_range_label <- function(af173, af174, max_year, seasons) {
    if (!is.finite(af173) || !is.finite(af174) || !is.finite(max_year) || !is.finite(seasons)) {
      return("Not available")
    }

    older_back <- max(af173, af174)
    recent_back <- min(af173, af174)
    older_label <- profile_period_window(max_year, seasons, older_back)
    recent_label <- profile_period_window(max_year, seasons, recent_back)

    if (!nzchar(older_label) || !nzchar(recent_label) || is.na(older_label) || is.na(recent_label)) {
      return("Not available")
    }

    if (older_label == recent_label) return(older_label)
    paste0(older_label, " to ", recent_label)
  }

  profile_period_label <- function(quantity_label, af173, af174, max_year, seasons) {
    if (!is.finite(af173) || !is.finite(af174)) return("Not available")

    if (af173 == 0 && af174 == 0) {
      if (identical(quantity_label, "relative_depletion")) {
        return("Terminal biomass window: whole time series")
      }
      if (identical(quantity_label, "avg_bio")) {
        return("Average biomass window: whole time series")
      }
      return("Whole time series")
    }

    period_core <- profile_period_range_label(af173, af174, max_year, seasons)

    if (identical(quantity_label, "relative_depletion")) {
      return(paste("Terminal biomass window:", period_core))
    }
    if (identical(quantity_label, "avg_bio")) {
      return(paste("Average biomass window:", period_core))
    }

    period_core
  }

  # Load profile outputs for a scenario
  load_profile_outputs <- function(model_dir, scenario) {
    folder <- file.path(model_dir, scenario)
    prof_dir <- file.path(folder, "prof")
    scaler_dirs <- list.dirs(prof_dir, full.names = TRUE, recursive = FALSE)
    scaler_dirs <- grep("scaler_\\d+$", scaler_dirs, value = TRUE)

    if (length(scaler_dirs) > 0) {
      payload_files <- file.path(scaler_dirs, "profile_payload.rds")
      has_payload <- file.exists(payload_files)

      if (any(has_payload)) {
        payloads <- map(payload_files[has_payload], ~ tryCatch(readRDS(.x), error = function(e) NULL))
        payloads <- payloads[!vapply(payloads, is.null, logical(1))]
        info_files <- file.path(scaler_dirs[has_payload], "info.rds")
        info_payloads <- map(info_files[file.exists(info_files)], ~ tryCatch(readRDS(.x), error = function(e) NULL))
        info_payloads <- info_payloads[!vapply(info_payloads, is.null, logical(1))]

        if (length(payloads) > 0) {
          existing_scales <- as.character(vapply(payloads, function(x) as.numeric(x$scaler), numeric(1)))
          lik_out <- setNames(map(payloads, "lik_out"), existing_scales)
          lik_raw <- setNames(map(payloads, "lik_raw"), existing_scales)
          max_grad <- setNames(vapply(payloads, function(x) suppressWarnings(as.numeric(x$max_grad)), numeric(1)), existing_scales)
          obj_fun <- setNames(vapply(payloads, function(x) suppressWarnings(as.numeric(x$obj_fun)), numeric(1)), existing_scales)
          actual_quantity <- setNames(vapply(payloads, function(x) suppressWarnings(as.numeric(x$actual_quantity)), numeric(1)), existing_scales)
          target_quantity <- setNames(vapply(payloads, function(x) suppressWarnings(as.numeric(x$target_quantity)), numeric(1)), existing_scales)
          target_rel_err <- setNames(vapply(payloads, function(x) suppressWarnings(as.numeric(x$target_rel_err)), numeric(1)), existing_scales)
          af172_vals <- suppressWarnings(vapply(payloads, function(x) safe_payload_numeric(x, "af172"), numeric(1)))
          af172 <- if (length(af172_vals) > 0) af172_vals[1] else NA_real_
          af173_vals <- suppressWarnings(vapply(payloads, function(x) safe_payload_numeric(x, "af173"), numeric(1)))
          af174_vals <- suppressWarnings(vapply(payloads, function(x) safe_payload_numeric(x, "af174"), numeric(1)))
          if (length(af173_vals) == 0 && length(info_payloads) > 0) {
            af173_vals <- suppressWarnings(vapply(info_payloads, function(x) as.numeric(x$AgeFlags["Af173"]), numeric(1)))
          }
          if (length(af174_vals) == 0 && length(info_payloads) > 0) {
            af174_vals <- suppressWarnings(vapply(info_payloads, function(x) as.numeric(x$AgeFlags["Af174"]), numeric(1)))
          }
          if (!is.finite(af172) && length(info_payloads) > 0) {
            af172_fallback <- suppressWarnings(vapply(info_payloads, function(x) as.numeric(x$AgeFlags["Af172"]), numeric(1)))
            af172 <- if (length(af172_fallback) > 0) af172_fallback[1] else NA_real_
          }
          af173 <- if (length(af173_vals) > 0) af173_vals[1] else NA_real_
          af174 <- if (length(af174_vals) > 0) af174_vals[1] else NA_real_
          quantity_label_vals <- unlist(map(payloads, "quantity_label"), use.names = FALSE)
          quantity_label_vals <- quantity_label_vals[nzchar(quantity_label_vals)]
          if (length(quantity_label_vals) == 0 && length(info_payloads) > 0) {
            quantity_label_vals <- unlist(map(info_payloads, "quantity_label"), use.names = FALSE)
            quantity_label_vals <- quantity_label_vals[nzchar(quantity_label_vals)]
          }
          quantity_label <- if (length(quantity_label_vals) > 0) quantity_label_vals[1] else NA_character_
          par_obj <- rv$ParOut_list[[scenario]]
          max_year <- suppressWarnings(as.numeric(tryCatch(par_obj@range["maxyear"], error = function(e) NA_real_)))
          seasons <- suppressWarnings(as.numeric(tryCatch(par_obj@dimensions["seasons"], error = function(e) NA_real_)))
        } else {
          lik_out <- list()
          lik_raw <- list()
          max_grad <- numeric(0)
          obj_fun <- numeric(0)
          actual_quantity <- numeric(0)
          target_quantity <- numeric(0)
          target_rel_err <- numeric(0)
          existing_scales <- character(0)
          af172 <- NA_real_
          af173 <- NA_real_
          af174 <- NA_real_
          quantity_label <- NA_character_
          max_year <- NA_real_
          seasons <- NA_real_
        }
      } else {
        scales <- basename(scaler_dirs) %>% str_extract("\\d+$")
        output_files <- file.path(scaler_dirs, "test_plot_output")
        existing_files <- output_files[file.exists(output_files)]
        existing_scales <- scales[file.exists(output_files)]

        if (length(existing_files) > 0) {
          lik_out <- setNames(map(existing_files, read.MFCLLikelihood), existing_scales)
          lik_raw <- setNames(map(existing_files, readLines), existing_scales)
        } else {
          lik_out <- list()
          lik_raw <- list()
          existing_scales <- character(0)
        }
        max_grad <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
        obj_fun <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
        actual_quantity <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
        target_quantity <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
        target_rel_err <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
        af172 <- NA_real_
        af173 <- NA_real_
        af174 <- NA_real_
        quantity_label <- NA_character_
        max_year <- NA_real_
        seasons <- NA_real_
      }
    } else {
      output_files <- list.files(folder, pattern = "^test_plot_output_\\d+$", full.names = TRUE)

      if (length(output_files) > 0) {
        scales <- basename(output_files) %>% str_extract("\\d+$")
        lik_out <- setNames(map(output_files, read.MFCLLikelihood), scales)
        lik_raw <- setNames(map(output_files, readLines), scales)
        existing_scales <- scales
      } else {
        lik_out <- list()
        lik_raw <- list()
        existing_scales <- character(0)
      }
      max_grad <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
      obj_fun <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
      actual_quantity <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
      target_quantity <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
      target_rel_err <- setNames(rep(NA_real_, length(existing_scales)), existing_scales)
      af172 <- NA_real_
      af173 <- NA_real_
      af174 <- NA_real_
      quantity_label <- NA_character_
      max_year <- NA_real_
      seasons <- NA_real_
    }

    list(
      scales = existing_scales,
      lik_out = lik_out,
      lik_raw = lik_raw,
      max_grad = max_grad,
      obj_fun = obj_fun,
      actual_quantity = actual_quantity,
      target_quantity = target_quantity,
      target_rel_err = target_rel_err,
      af172 = af172,
      af173 = af173,
      af174 = af174,
      quantity_label = quantity_label,
      max_year = max_year,
      seasons = seasons
    )
  }

  profile_outputs_reactive <- reactive({
    filters <- lik_filters()
    req(rv$data_loaded, input$model_dir, filters, filters$scenarios)

    selected <- filters$scenarios
    if (length(selected) == 0) return(list())

    profile_data <- setNames(
      lapply(selected, function(sc) load_profile_outputs(input$model_dir, sc)),
      selected
    )

    has_data <- vapply(profile_data, function(x) length(x$scales) > 0, logical(1))
    profile_data[has_data]
  })
  profile_outputs_reactive <- bindCache(
    profile_outputs_reactive,
    rv$data_loaded,
    input$model_dir,
    lik_filters()
  )

  profile_target_info_reactive <- reactive({
    filters <- lik_filters()
    req(filters)

    type <- filters$profile_type
    if (type %in% c("jitter", "jitter_params", "jitter_derived", "retro", "hessian")) return(NULL)

    profile_data <- profile_outputs_reactive()
    if (length(profile_data) == 0) return(NULL)

    rows <- lapply(names(profile_data), function(sc) {
      pd <- profile_data[[sc]]
      quantity_label <- pd$quantity_label
      af172 <- suppressWarnings(as.numeric(pd$af172))
      af173 <- suppressWarnings(as.numeric(pd$af173))
      af174 <- suppressWarnings(as.numeric(pd$af174))
      max_year <- suppressWarnings(as.numeric(pd$max_year))
      seasons <- suppressWarnings(as.numeric(pd$seasons))

      data.frame(
        Model = sc,
        `Profile target` = profile_target_label(quantity_label),
        `Biomass type` = profile_biomass_label(af172),
        Af172 = if (is.finite(af172)) as.integer(af172) else NA_integer_,
        Af173 = if (is.finite(af173)) as.integer(af173) else NA_integer_,
        Af174 = if (is.finite(af174)) as.integer(af174) else NA_integer_,
        `Time-period window` = if (is.finite(af173) && is.finite(af174)) {
          if (af173 == 0 && af174 == 0) {
            "Whole time series"
          } else {
            profile_period_range_label(af173, af174, max_year, seasons)
          }
        } else {
          "Not available"
        },
        `Period definition` = profile_period_label(quantity_label, af173, af174, max_year, seasons),
        stringsAsFactors = FALSE
      )
    })

    bind_rows(rows)
  })
  profile_target_info_reactive <- bindCache(
    profile_target_info_reactive,
    input$model_dir,
    lik_filters()
  )

  profile_gradient_table_reactive <- reactive({
    filters <- lik_filters()
    req(filters)

    type <- filters$profile_type
    if (type %in% c("jitter", "jitter_params", "jitter_derived", "retro", "hessian")) return(NULL)

    profile_data <- profile_outputs_reactive()
    if (length(profile_data) == 0) return(NULL)

    rows <- list()
    for (sc in names(profile_data)) {
      pd <- profile_data[[sc]]
      if (length(pd$scales) == 0) next

      for (scl in pd$scales) {
        grad_val <- pd$max_grad[[as.character(scl)]]
        obj_fun <- pd$obj_fun[[as.character(scl)]]
        actual_quantity <- pd$actual_quantity[[as.character(scl)]]
        target_quantity <- pd$target_quantity[[as.character(scl)]]
        target_rel_err <- pd$target_rel_err[[as.character(scl)]]
        rows[[length(rows) + 1]] <- data.frame(
          scenario = sc,
          scaler = suppressWarnings(as.numeric(scl)),
          actual_quantity = suppressWarnings(as.numeric(actual_quantity)),
          target_quantity = suppressWarnings(as.numeric(target_quantity)),
          target_rel_err = suppressWarnings(as.numeric(target_rel_err)),
          obj_fun = suppressWarnings(as.numeric(obj_fun)),
          max_grad = suppressWarnings(as.numeric(grad_val)),
          stringsAsFactors = FALSE
        )
      }
    }

    out <- bind_rows(rows)
    if (nrow(out) == 0) return(NULL)

    out %>%
      mutate(
        actual_quantity_kmt = actual_quantity / 1000,
        target_quantity_kmt = target_quantity / 1000,
        target_gap_pct = target_rel_err * 100,
        obj_fun = ifelse(is.finite(obj_fun), obj_fun, NA_real_),
        max_grad = ifelse(is.finite(max_grad), max_grad, NA_real_)
      ) %>%
      select(
        Model = scenario,
        Scaler = scaler,
        `Actual Quantity (k MT)` = actual_quantity_kmt,
        `Target Quantity (k MT)` = target_quantity_kmt,
        `Gap (%)` = target_gap_pct,
        `Objective Function` = obj_fun,
        `Max Gradient` = max_grad
      ) %>%
      arrange(Model, Scaler)
  })
  profile_gradient_table_reactive <- bindCache(
    profile_gradient_table_reactive,
    input$model_dir,
    lik_filters()
  )

  # Calculate likelihood change from minimum
  calc_lik_change <- function(df, group_col) {
    grouping_vars <- c(group_col, "scenario")
    if ("region" %in% names(df)) grouping_vars <- c(grouping_vars, "region")

    df %>%
      group_by(across(all_of(grouping_vars))) %>%
      mutate(
        min_value = min(value, na.rm = TRUE),
        change = value - min_value
      ) %>%
      ungroup()
  }

  # Create a standard likelihood profile plot
  create_piner_plot <- function(data, group_var, x_label, label = NULL, facet_ncol = 2, split_by_region = FALSE) {
    if (nrow(data) == 0) return(NULL)

    if ("region" %in% names(data)) {
      data <- data %>%
        mutate(
          region = ifelse(
            is.na(region) | !nzchar(trimws(as.character(region))),
            "Unknown",
            paste("Region", as.character(region))
          )
      )
    }

    unique_groups <- unique(data[[group_var]])
    non_total_groups <- setdiff(unique_groups, "Total")
    legend_ncol <- max(3, min(6, ceiling(sqrt(length(unique_groups)))))
    legend_breaks <- c(setdiff(unique_groups, "Total"), intersect("Total", unique_groups))

    color_values <- c(
      "Total" = "black",
      setNames(viridis::viridis(length(non_total_groups)), non_total_groups)
    )

    p <- ggplot(
      data,
      aes(x = scaler, y = change, colour = .data[[group_var]], shape = .data[[group_var]])
    ) +
      geom_line(aes(linewidth = .data[[group_var]] == "Total"), alpha = 0.7) +
      geom_point(aes(size = .data[[group_var]] == "Total"), alpha = 0.8) +
      scale_color_manual(values = color_values, breaks = legend_breaks) +
      scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = 0.7), guide = "none") +
      scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2), guide = "none") +
      scale_shape_manual(values = rep(0:24, length.out = length(unique_groups))) +
      scale_x_continuous(
        labels = function(x) x / 1000,
        name = x_label
      ) +
      labs(y = "Changes in Likelihood", colour = NULL, shape = group_var) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8.5, face = "bold", colour = "#222222"),
        legend.key.width = unit(11, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.spacing.x = unit(5, "pt"),
        legend.spacing.y = unit(3, "pt"),
        legend.box.spacing = unit(3, "pt"),
        legend.box.margin = margin(t = 2, r = 2, b = 0, l = 2),
        legend.margin = margin(0, 0, 0, 0),
        legend.box = "vertical",
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      guides(
        colour = guide_legend(
          ncol = legend_ncol,
          byrow = TRUE,
          override.aes = list(linewidth = 1.4, size = 3.2, alpha = 1),
          order = 1
        ),
        shape = "none"
      )

    if (isTRUE(split_by_region) && "region" %in% names(data)) {
      n_scenarios <- dplyr::n_distinct(data$scenario)
      if (n_scenarios == 1) {
        region_labels <- sort(unique(as.character(data$region)))
        region_labeller <- setNames(region_labels, region_labels)

        p <- p +
          facet_wrap(
            ~region,
            scales = "free_y",
            ncol = facet_ncol,
            labeller = as_labeller(region_labeller)
          ) +
          labs(title = unique(data$scenario)[1]) +
          theme(
            strip.text = element_text(size = 8.5, lineheight = 0.95, face = "bold"),
            plot.margin = margin(8, 8, 10, 8)
          )
      } else {
        p <- p + facet_grid(rows = vars(scenario), cols = vars(region), scales = "free_y")
      }
    } else {
      p <- p + facet_wrap(~scenario, scales = "free", ncol = facet_ncol)
    }

    if (!is.null(label) && !(isTRUE(split_by_region) && "region" %in% names(data))) {
      p <- p + annotate("text", x = Inf, y = Inf, label = label,
                        hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold")
    }

    p
  }

  build_components_data <- function(profile_data, scenarios, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        raw <- profile_data[[sc]]$lik_raw[[scl]]
        if (is.null(lik)) next

        penalties <- NA_real_
        if (!is.null(raw) && length(raw) >= 6) {
          penalties <- suppressWarnings(as.numeric(raw[6]))
        }

        values <- c(
          Indices = sum(lik@survey_index),
          LFs = sum(lik@total_length_fish),
          Penalties = penalties,
          WFs = sum(lik@total_weight_fish),
          Age = sum(lik@age_length),
          Tags = sum(unlist(lik@tag_rel_fish, recursive = TRUE))
        )
        values <- c(values, Total = sum(values, na.rm = TRUE))

        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next
        rows[[length(rows) + 1]] <- data.frame(
          scenario = sc,
          scaler = scaler_bio,
          Likelihood = names(values),
          value = as.numeric(values),
          stringsAsFactors = FALSE
        )
      }
    }

    bind_rows(rows)
  }

  build_fishery_data <- function(profile_data, scenarios, fishery_maps, slot_name, label, scales,
                                 allowed_fisheries = NULL, fallback_nonzero_only = FALSE) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        vec <- slot(lik, slot_name)
        fish_ids <- as.character(seq_along(vec))
        keep_idx <- rep(TRUE, length(fish_ids))
        if (!is.null(allowed_fisheries)) {
          allowed_ids <- as.character(allowed_fisheries[[sc]])
          allowed_ids <- allowed_ids[!is.na(allowed_ids)]
          if (length(allowed_ids) > 0) {
            keep_idx <- fish_ids %in% allowed_ids
          } else if (isTRUE(fallback_nonzero_only)) {
            keep_idx <- is.finite(vec) & abs(vec) > 0
          }
        } else if (isTRUE(fallback_nonzero_only)) {
          keep_idx <- is.finite(vec) & abs(vec) > 0
        }

        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_map <- fishery_maps[[sc]]
        fish_names <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))
        fish_regions <- sapply(fish_ids, function(fid) get_fishery_region(fid, fish_map))

        df <- data.frame(
          scenario = sc,
          scaler = scaler_bio,
          group = fish_names,
          region = fish_regions,
          value = as.numeric(vec),
          stringsAsFactors = FALSE
        )

        total_row <- df %>%
          group_by(scenario, scaler, region) %>%
          summarise(value = sum(value), n_groups = dplyr::n_distinct(group), .groups = "drop") %>%
          filter(n_groups > 1) %>%
          select(-n_groups) %>%
          mutate(group = "Total")

        rows[[length(rows) + 1]] <- bind_rows(df, total_row)
      }
    }

    data <- bind_rows(rows)
    if (nrow(data) == 0) return(data)

    names(data)[names(data) == "group"] <- label
    data
  }

  allowed_fisheries_for_regions <- function(scenarios, fishery_maps, regions) {
    if (is.null(regions) || length(regions) == 0) return(NULL)
    region_vals <- as.character(regions)
    region_vals <- region_vals[nzchar(region_vals)]
    if (length(region_vals) == 0) return(NULL)

    out <- lapply(scenarios, function(sc) {
      fish_map <- fishery_maps[[sc]]
      if (is.null(fish_map) || !"fishery" %in% names(fish_map) || !"region" %in% names(fish_map)) {
        return(character(0))
      }

      keep <- as.character(fish_map$region) %in% region_vals
      as.character(fish_map$fishery[keep])
    })
    names(out) <- scenarios
    out
  }

  get_fishery_region <- function(fid, fish_map) {
    if (is.null(fish_map) || !"fishery" %in% names(fish_map) || !"region" %in% names(fish_map)) {
      return("Unknown")
    }

    idx <- match(as.character(fid), as.character(fish_map$fishery))
    if (is.na(idx)) return("Unknown")

    region_val <- fish_map$region[[idx]]
    if (is.null(region_val) || length(region_val) == 0 || is.na(region_val) || !nzchar(trimws(as.character(region_val)))) {
      return("Unknown")
    }

    as.character(region_val)
  }

  extract_survey_index_like_from_raw <- function(raw_lines) {
    if (is.null(raw_lines) || length(raw_lines) == 0) return(numeric(0))
    header_idx <- which(grepl("^\\s*#\\s*Survey_index_like_by_fishery\\s*$", raw_lines))
    if (length(header_idx) == 0) return(numeric(0))

    i <- header_idx[1] + 1
    if (i > length(raw_lines)) return(numeric(0))

    block <- character(0)
    while (i <= length(raw_lines)) {
      line <- trimws(raw_lines[i])
      if (!nzchar(line) || grepl("^\\s*#", line)) break
      block <- c(block, line)
      i <- i + 1
    }
    if (length(block) == 0) return(numeric(0))

    vals <- suppressWarnings(as.numeric(strsplit(paste(block, collapse = " "), "\\s+")[[1]]))
    vals[is.finite(vals)]
  }

  build_cpue_fishery_data <- function(profile_data, scenarios, fishery_maps, scales, allowed_fisheries = NULL) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      for (scl in scales) {
        raw <- profile_data[[sc]]$lik_raw[[scl]]
        vec <- extract_survey_index_like_from_raw(raw)
        if (length(vec) == 0) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        fish_ids <- as.character(seq_along(vec))
        keep_idx <- is.finite(vec) & abs(vec) > 0
        if (!is.null(allowed_fisheries)) {
          allowed_ids <- as.character(allowed_fisheries[[sc]])
          allowed_ids <- allowed_ids[!is.na(allowed_ids)]
          if (length(allowed_ids) > 0) {
            keep_idx <- keep_idx & (fish_ids %in% allowed_ids)
          }
        }
        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_map <- fishery_maps[[sc]]
        fish_names <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))
        fish_regions <- sapply(fish_ids, function(fid) get_fishery_region(fid, fish_map))

        df <- data.frame(
          scenario = sc,
          scaler = scaler_bio,
          Fishery = fish_names,
          region = fish_regions,
          value = as.numeric(vec),
          stringsAsFactors = FALSE
        )

        total_row <- df %>%
          group_by(scenario, scaler, region) %>%
          summarise(value = sum(value), n_groups = dplyr::n_distinct(Fishery), .groups = "drop") %>%
          filter(n_groups > 1) %>%
          select(-n_groups) %>%
          mutate(Fishery = "Total")

        rows[[length(rows) + 1]] <- bind_rows(df, total_row)
      }
    }

    bind_rows(rows)
  }

  build_tagging_data <- function(profile_data, scenarios, tag_out_list, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      tag_out <- tag_out_list[[sc]]
      if (is.null(tag_out)) next

      rel_df <- tryCatch(safe_array_to_df(tag_out@releases), error = function(e) NULL)
      if (is.null(rel_df) || nrow(rel_df) == 0) next

      program_map <- rel_df %>%
        distinct(rel.group, program) %>%
        arrange(rel.group) %>%
        rename(program_name = program)

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        tag_rel <- lik@tag_rel_fish
        sums_vec <- sapply(tag_rel, function(g) sum(unlist(g)))
        program_names <- program_map$program_name[seq_along(sums_vec)]
        program_names[is.na(program_names)] <- paste("Program", seq_along(sums_vec))

        df <- data.frame(
          scenario = sc,
          scaler = scaler_bio,
          program = program_names,
          value = as.numeric(sums_vec),
          stringsAsFactors = FALSE
        )

        rows[[length(rows) + 1]] <- df
      }
    }

    data <- bind_rows(rows)
    if (nrow(data) == 0) return(data)

    data %>%
      group_by(program, scaler, scenario) %>%
      summarise(value = sum(value), .groups = "drop")
  }

  get_alk_summary <- function(age_out) {
    if (is.null(age_out)) return(NULL)
    alk_df <- tryCatch(safe_array_to_df(age_out@ALK), error = function(e) NULL)
    if (is.null(alk_df) || nrow(alk_df) == 0) return(NULL)

    alk_df %>%
      mutate(order = row_number()) %>%
      group_by(year, month, fishery) %>%
      summarise(first_order = min(order), .groups = "drop") %>%
      arrange(first_order) %>%
      select(-first_order)
  }

  build_cal_data <- function(profile_data, scenarios, age_out_list, fishery_maps, by = "fishery", scales,
                             allowed_fisheries = NULL) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      alk_summary <- get_alk_summary(age_out_list[[sc]])
      if (is.null(alk_summary) || nrow(alk_summary) == 0) next

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        lik_vec <- lik@age_length
        n_use <- min(length(lik_vec), nrow(alk_summary))
        if (n_use == 0) next

        df <- alk_summary[seq_len(n_use), , drop = FALSE]
        df$Lik <- lik_vec[seq_len(n_use)]
        df$scenario <- sc
        df$scaler <- scaler_bio

        if (by == "fishery") {
          if (!is.null(allowed_fisheries)) {
            allowed_ids <- as.character(allowed_fisheries[[sc]])
            allowed_ids <- allowed_ids[!is.na(allowed_ids)]
            if (length(allowed_ids) > 0) {
              df <- df[as.character(df$fishery) %in% allowed_ids, , drop = FALSE]
            }
          }
          if (nrow(df) == 0) next

          fish_ids <- as.character(df$fishery)
          fish_map <- fishery_maps[[sc]]
          df$fishery <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))
          df$region <- sapply(fish_ids, function(fid) get_fishery_region(fid, fish_map))

          by_group <- df %>%
            group_by(fishery, scaler, scenario, region) %>%
            summarise(value = sum(Lik, na.rm = TRUE), .groups = "drop")

          total_row <- by_group %>%
            group_by(scaler, scenario, region) %>%
            summarise(value = sum(value), n_groups = dplyr::n_distinct(fishery), .groups = "drop") %>%
            filter(n_groups > 1) %>%
            select(-n_groups) %>%
            mutate(fishery = "Total")

          rows[[length(rows) + 1]] <- bind_rows(by_group, total_row)
        } else {
          by_group <- df %>%
            group_by(year, scaler, scenario) %>%
            summarise(value = sum(Lik, na.rm = TRUE), .groups = "drop")

          by_group$year <- as.character(by_group$year)
          total_row <- by_group %>%
            group_by(scaler, scenario) %>%
            summarise(value = sum(value), .groups = "drop") %>%
            mutate(year = "Total")

          rows[[length(rows) + 1]] <- bind_rows(by_group, total_row)
        }
      }
    }

    bind_rows(rows)
  }

  build_jitter_data <- function(scenarios, par_out_list, jitter_pars_list) {
    rows <- list()
    for (sc in scenarios) {
      ref_par <- par_out_list[[sc]]
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(ref_par) || is.null(jit_list) || length(jit_list) == 0) next

      ref_obj <- suppressWarnings(as.numeric(ref_par@obj_fun))
      ref_grad <- suppressWarnings(as.numeric(ref_par@max_grad))
      if (!is.finite(ref_obj)) next

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      extract_jitter_obj <- function(p) {
        if (is.list(p) && !is.null(p$obj_fun)) return(suppressWarnings(as.numeric(p$obj_fun)))
        suppressWarnings(tryCatch(as.numeric(p@obj_fun), error = function(e) NA_real_))
      }
      extract_jitter_grad <- function(p) {
        if (is.list(p) && !is.null(p$max_grad)) return(suppressWarnings(as.numeric(p$max_grad)))
        suppressWarnings(tryCatch(as.numeric(p@max_grad), error = function(e) NA_real_))
      }
      obj_vals <- sapply(jit_list, extract_jitter_obj)
      grad_vals <- sapply(jit_list, extract_jitter_grad)
      keep <- is.finite(obj_vals) & is.finite(grad_vals)
      if (!any(keep)) next

      obj_vals <- obj_vals[keep]
      grad_vals <- grad_vals[keep]
      seeds <- seeds[keep]
      pct_diff <- ((obj_vals - ref_obj) / abs(ref_obj)) * 100

      rows[[length(rows) + 1]] <- data.frame(
        scenario = sc,
        seed = as.character(seeds),
        ref_obj = as.numeric(ref_obj),
        ref_grad = as.numeric(ref_grad),
        obj_fun = as.numeric(obj_vals),
        max_grad = as.numeric(grad_vals),
        pct_diff = as.numeric(pct_diff),
        jitter_id = seq_along(obj_vals),
        stringsAsFactors = FALSE
      )
    }

    bind_rows(rows)
  }

  build_jitter_parameter_data <- function(scenarios, jitter_pars_list, view = "input", converged_only = FALSE) {
    rows <- list()

    for (sc in scenarios) {
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(jit_list) || length(jit_list) == 0) next

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      for (i in seq_along(jit_list)) {
        jit <- jit_list[[i]]
        if (identical(view, "final")) {
          if (!isTRUE(jit$run_completed)) next
          if (isTRUE(converged_only) && !isTRUE(jit$converged)) next
        }
        param_changes <- if (identical(view, "final")) {
          if (is.list(jit) && !is.null(jit$fitted_parameter_changes)) jit$fitted_parameter_changes else NULL
        } else {
          if (is.list(jit) && !is.null(jit$parameter_changes)) jit$parameter_changes else NULL
        }
        labels_df <- if (is.list(param_changes) && !is.null(param_changes$labels)) param_changes$labels else NULL
        if (is.null(labels_df) || nrow(labels_df) == 0) next

        df <- labels_df %>%
          mutate(
            scenario = sc,
            seed = as.character(seeds[[i]]),
            Index = suppressWarnings(as.integer(Index)),
            before = suppressWarnings(as.numeric(before)),
            after = suppressWarnings(as.numeric(after)),
            delta = suppressWarnings(as.numeric(delta)),
            pct_change = suppressWarnings(as.numeric(pct_change)),
            abs_pct_change = suppressWarnings(as.numeric(abs_pct_change)),
            family = ifelse(is.na(family) | !nzchar(family), "unclassified", family)
          ) %>%
          filter(is.finite(before), is.finite(after))

        if (nrow(df) == 0) next
        rows[[length(rows) + 1]] <- df
      }
    }

    all_params <- bind_rows(rows)
    if (nrow(all_params) == 0) return(data.frame())

    ranked <- all_params %>%
      group_by(scenario, Index, Var_name, family) %>%
      summarise(
        before = first(before),
        median_abs_pct_change = median(abs_pct_change, na.rm = TRUE),
        mean_abs_pct_change = mean(abs_pct_change, na.rm = TRUE),
        n_seed = n_distinct(seed),
        .groups = "drop"
      ) %>%
      arrange(scenario, desc(median_abs_pct_change), desc(mean_abs_pct_change), Var_name) %>%
      mutate(param_key = paste(scenario, Index, sep = "::"))

    plot_df <- all_params %>%
      inner_join(
        ranked %>% select(scenario, Index, param_key, median_abs_pct_change, mean_abs_pct_change, n_seed),
        by = c("scenario", "Index")
      )

    duplicate_counts <- plot_df %>%
      distinct(scenario, Index, Var_name, family) %>%
      group_by(scenario, Var_name, family) %>%
      summarise(label_count = dplyr::n(), .groups = "drop")

    plot_df <- plot_df %>%
      left_join(duplicate_counts, by = c("scenario", "Var_name", "family")) %>%
      mutate(
        param_label = ifelse(
          label_count > 1,
          paste0(Var_name, " {", Index, "} [", family, "]"),
          paste0(Var_name, " [", family, "]")
        )
      ) %>%
      select(-label_count)

    param_levels <- ranked %>%
      arrange(scenario, median_abs_pct_change, mean_abs_pct_change, Var_name) %>%
      pull(param_key)
    plot_df$param_key <- factor(plot_df$param_key, levels = unique(param_levels))

    plot_df
  }

  extract_reference_metrics_timeseries <- function(rep_obj, scenario) {
    if (is.null(rep_obj)) return(NULL)

    bio_fish <- safe_array_to_df(rep_obj@adultBiomass) %>%
      mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
      filter(is.finite(year), is.finite(season), is.finite(data)) %>%
      group_by(year, season) %>%
      summarise(bio_fish = sum(data), .groups = "drop")

    bio_nofish <- safe_array_to_df(rep_obj@adultBiomass_nofish) %>%
      mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
      filter(is.finite(year), is.finite(season), is.finite(data)) %>%
      group_by(year, season) %>%
      summarise(bio_nofish = sum(data), .groups = "drop")

    merged <- bio_fish %>%
      inner_join(bio_nofish, by = c("year", "season")) %>%
      mutate(depletion = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
      group_by(year) %>%
      summarise(
        depletion = mean(depletion, na.rm = TRUE),
        spawning_potential = mean(bio_fish, na.rm = TRUE) / 1e3,
        .groups = "drop"
      )

    if (nrow(merged) == 0) return(NULL)
    merged %>%
      mutate(scenario = scenario)
  }

  build_jitter_derived_data <- function(scenarios, rep_out_list, jitter_pars_list) {
    rows <- list()
    ref_rows <- list()

    for (sc in scenarios) {
      ref_metrics <- extract_reference_metrics_timeseries(rep_out_list[[sc]], sc)
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(ref_metrics) || is.null(jit_list) || length(jit_list) == 0) next

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      for (i in seq_along(jit_list)) {
        jit <- jit_list[[i]]
        derived <- if (is.list(jit) && !is.null(jit$derived_quantities)) jit$derived_quantities else NULL
        if (is.null(derived) || !is.data.frame(derived) || nrow(derived) == 0) next

        seed_df <- derived %>%
          mutate(
            scenario = sc,
            seed = as.character(seeds[[i]])
          ) %>%
          transmute(
            scenario = scenario,
            seed = seed,
            year = suppressWarnings(as.numeric(year)),
            depletion = suppressWarnings(as.numeric(depletion)),
            spawning_potential = suppressWarnings(as.numeric(spawning_potential))
          )

        rows[[length(rows) + 1]] <- seed_df
      }

      ref_rows[[length(ref_rows) + 1]] <- bind_rows(
        data.frame(
          scenario = sc,
          metric = "Depletion",
          reference_value = suppressWarnings(as.numeric(ref_metrics$depletion)),
          year = suppressWarnings(as.numeric(ref_metrics$year)),
          stringsAsFactors = FALSE
        ),
        data.frame(
          scenario = sc,
          metric = "Spawning Potential (1e3 MT)",
          reference_value = suppressWarnings(as.numeric(ref_metrics$spawning_potential)),
          year = suppressWarnings(as.numeric(ref_metrics$year)),
          stringsAsFactors = FALSE
        )
      )
    }

    value_rows <- bind_rows(rows)
    if (nrow(value_rows) == 0) return(data.frame())

    ref_rows <- bind_rows(ref_rows)
    if (is.null(ref_rows) || nrow(ref_rows) == 0) return(data.frame())

    value_long <- bind_rows(
      value_rows %>%
        transmute(scenario, seed, year, metric = "Depletion", value = depletion),
      value_rows %>%
        transmute(scenario, seed, year, metric = "Spawning Potential (1e3 MT)", value = spawning_potential)
    ) %>%
      filter(is.finite(value))

    if (nrow(value_long) == 0) return(data.frame())

    value_long %>%
      left_join(ref_rows, by = c("scenario", "metric", "year")) %>%
      mutate(
        pct_change = 100 * (value - reference_value) / pmax(abs(reference_value), .Machine$double.eps)
      )
  }
  
  build_retro_data_for_scenario <- function(scenario, model_dir, rep_obj) {
    extract_retro_metrics <- function(rep_obj, scenario, peel) {
      bio_fish <- safe_array_to_df(rep_obj@adultBiomass) %>%
        mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(season), is.finite(data)) %>%
        group_by(year, season) %>%
        summarise(bio_fish = sum(data), .groups = "drop")
      
      bio_nofish <- safe_array_to_df(rep_obj@adultBiomass_nofish) %>%
        mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(season), is.finite(data)) %>%
        group_by(year, season) %>%
        summarise(bio_nofish = sum(data), .groups = "drop")
      
      dep <- bio_fish %>%
        inner_join(bio_nofish, by = c("year", "season")) %>%
        mutate(
          bio_fish = suppressWarnings(as.numeric(bio_fish)),
          bio_nofish = suppressWarnings(as.numeric(bio_nofish))
        ) %>%
        mutate(depletion = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
        group_by(year) %>%
        summarise(depletion = mean(depletion, na.rm = TRUE), .groups = "drop")
      
      sp <- bio_fish %>%
        group_by(year) %>%
        summarise(spawning_potential = mean(bio_fish, na.rm = TRUE) / 1e3, .groups = "drop")
      
      dep %>%
        inner_join(sp, by = "year") %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          scenario = scenario,
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))
    }

    retro_rows <- list()
    if (!is.null(rep_obj)) {
      retro_rows[[paste0(scenario, "_peel_0")]] <- extract_retro_metrics(rep_obj, scenario, 0)
    }

    retro_dir <- file.path(model_dir, scenario, "retro")
    peel_dirs <- list.dirs(retro_dir, recursive = FALSE, full.names = TRUE)
    peel_dirs <- peel_dirs[grepl("peel_\\d+$", peel_dirs)]

    for (pd in peel_dirs) {
      peel_num <- suppressWarnings(as.integer(stringr::str_extract(basename(pd), "\\d+$")))
      if (!is.finite(peel_num)) next

      metrics_file <- file.path(pd, "retro_metrics.rds")
      if (file.exists(metrics_file)) {
        m <- tryCatch(readRDS(metrics_file), error = function(e) NULL)
        if (!is.null(m) && nrow(m) > 0) {
          retro_rows[[paste0(scenario, "_peel_", peel_num)]] <- m
          next
        }
      }

      rep_path <- tryCatch(finalRep(pd), error = function(e) NULL)
      if (is.null(rep_path) || !file.exists(rep_path)) next

      peel_rep_obj <- tryCatch(read.MFCLRep(rep_path), error = function(e) NULL)
      if (is.null(peel_rep_obj)) next

      retro_rows[[paste0(scenario, "_peel_", peel_num)]] <- extract_retro_metrics(peel_rep_obj, scenario, peel_num)
    }

    bind_rows(retro_rows)
  }

  build_hessian_data_for_scenario <- function(scenario, model_dir) {
      hfile <- file.path(model_dir, scenario, "hessian", "hessian_info.rds")
      part_files <- list.files(
        file.path(model_dir, scenario, "hessian"),
        pattern = "^part_\\d+/hessian_info\\.rds$",
        full.names = TRUE,
        recursive = TRUE
      )
      
      if (!file.exists(hfile)) {
        return(data.frame(
          Scenario = scenario,
          Hessian_File = "Missing",
          PDH = NA_character_,
          `SPD (positivised cov)` = NA_character_,
          `Neg. Eigen` = NA_character_,
          `Hessian Status` = NA_character_,
          Reliability = NA_character_,
          `Stitch Complete` = NA_character_,
          `Parts (found/expected)` = ifelse(length(part_files) > 0, as.character(length(part_files)), NA_character_),
          stringsAsFactors = FALSE
        ))
      }
      
      info <- tryCatch(readRDS(hfile), error = function(e) NULL)
      if (is.null(info)) {
        return(data.frame(
          Scenario = scenario,
          Hessian_File = "Read error",
          PDH = NA_character_,
          `SPD (positivised cov)` = NA_character_,
          `Neg. Eigen` = NA_character_,
          `Hessian Status` = NA_character_,
          Reliability = NA_character_,
          `Stitch Complete` = NA_character_,
          `Parts (found/expected)` = ifelse(length(part_files) > 0, as.character(length(part_files)), NA_character_),
          stringsAsFactors = FALSE
        ))
      }
      
      is_pdh <- tryCatch(info$diagnostics$summary$pdh$is_pdh, error = function(e) NA)
      spd_pos_cov <- tryCatch(info$diagnostics$summary$positivised_cov_is_spd, error = function(e) NA)
      n_neg <- tryCatch(info$eigen$n_negative_eigenvalues, error = function(e) NA)
      n_tot <- tryCatch(info$eigen$n_total_eigenvalues, error = function(e) NA)
      h_status <- tryCatch(info$eigen$hessian_status, error = function(e) NA_character_)
      reliability <- tryCatch(info$eigen$reliability, error = function(e) NA_character_)
      stitch_complete <- tryCatch(info$stitch$is_complete, error = function(e) NA)
      n_parts_expected <- tryCatch(info$stitch$n_parts, error = function(e) NA)
      found_parts <- if (length(part_files) > 0) length(part_files) else as.integer(n_parts_expected)
      
      data.frame(
        Scenario = scenario,
        Hessian_File = "OK",
        PDH = dplyr::case_when(
          isTRUE(is_pdh) ~ "PDH",
          identical(is_pdh, FALSE) ~ "Not PDH",
          TRUE ~ "NA"
        ),
        `SPD (positivised cov)` = dplyr::case_when(
          isTRUE(spd_pos_cov) ~ "SPD",
          identical(spd_pos_cov, FALSE) ~ "Not SPD",
          TRUE ~ "NA"
        ),
        `Neg. Eigen` = ifelse(
          is.na(n_neg) || is.na(n_tot),
          NA_character_,
          sprintf("%d / %d", as.integer(n_neg), as.integer(n_tot))
        ),
        `Hessian Status` = as.character(h_status),
        Reliability = as.character(reliability),
        `Stitch Complete` = dplyr::case_when(
          isTRUE(stitch_complete) ~ "Yes",
          identical(stitch_complete, FALSE) ~ "No",
          TRUE ~ "NA"
        ),
        `Parts (found/expected)` = ifelse(
          is.na(n_parts_expected),
          as.character(found_parts),
          sprintf("%d / %d", as.integer(found_parts), as.integer(n_parts_expected))
        ),
        stringsAsFactors = FALSE
      )
  }

  profile_data_reactive <- reactive({
    filters <- lik_filters()
    req(rv$data_loaded, filters, filters$scenarios)

    if (length(filters$scenarios) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No models selected"))
    }

    type <- filters$profile_type
    selected <- filters$scenarios

    if (type == "jitter") {
      data <- build_jitter_data(selected, rv$ParOut_list, rv$JitterPars_list)
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No jitter analysis results found", plot_kind = "jitter"))
      }
      return(list(data = data, group_col = NULL, label = "Jitter", message = NULL, plot_kind = "jitter"))
    }

    if (type == "jitter_params") {
      jitter_param_view <- if (is.null(filters$jitter_param_view)) "input" else filters$jitter_param_view
      converged_only <- isTRUE(filters$jitter_final_converged_only)
      data <- build_jitter_parameter_data(selected, rv$JitterPars_list, view = jitter_param_view, converged_only = converged_only)
      if (nrow(data) == 0) {
        msg <- if (identical(jitter_param_view, "final")) {
          if (isTRUE(converged_only)) {
            "No converged jitter runs with final parameter distributions found"
          } else {
            "No completed jitter runs with final parameter distributions found"
          }
        } else {
          "No jitter parameter distributions found"
        }
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = msg, plot_kind = "jitter_params"))
      }
      return(list(data = data, group_col = NULL, label = "Jitter Parameters", message = NULL, plot_kind = "jitter_params"))
    }

    if (type == "jitter_derived") {
      data <- build_jitter_derived_data(selected, rv$RepOut_list, rv$JitterPars_list)
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No jitter derived quantity distributions found", plot_kind = "jitter_derived"))
      }
      return(list(data = data, group_col = NULL, label = "Jitter Derived Quantities", message = NULL, plot_kind = "jitter_derived"))
    }
    
    if (type == "retro") {
      req(input$model_dir)
      data <- bind_rows(lapply(selected, function(sc) {
        k <- scenario_cache_key(input$model_dir, sc)
        get_cached_heavy(
          "retro",
          k,
          function() build_retro_data_for_scenario(sc, input$model_dir, rv$RepOut_list[[sc]])
        )
      }))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No retrospective outputs found", plot_kind = "retro"))
      }
      data <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "Retrospective outputs were found but numeric series could not be parsed", plot_kind = "retro"))
      }
      
      eps <- .Machine$double.eps
      base_terminal <- data %>%
        filter(peel == 0) %>%
        select(scenario, year, depletion_base = depletion, spawning_potential_base = spawning_potential)
      
      peel_terminal <- data %>%
        filter(peel > 0) %>%
        group_by(scenario, peel) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        summarise(
          year = max(year, na.rm = TRUE),
          depletion_peel = dplyr::last(depletion),
          spawning_potential_peel = dplyr::last(spawning_potential),
          .groups = "drop"
        )
      
      mohn_summary <- peel_terminal %>%
        left_join(base_terminal, by = c("scenario", "year")) %>%
        mutate(
          rho_dep_component = (depletion_peel - depletion_base) / pmax(abs(depletion_base), eps),
          rho_sp_component = (spawning_potential_peel - spawning_potential_base) / pmax(abs(spawning_potential_base), eps)
        ) %>%
        group_by(scenario) %>%
        summarise(
          mohn_rho_depletion = ifelse(sum(is.finite(rho_dep_component)) > 0, mean(rho_dep_component, na.rm = TRUE), NA_real_),
          mohn_rho_spawning_potential = ifelse(sum(is.finite(rho_sp_component)) > 0, mean(rho_sp_component, na.rm = TRUE), NA_real_),
          .groups = "drop"
        )
      
      return(list(
        data = data,
        group_col = NULL,
        label = "Retrospective",
        message = NULL,
        plot_kind = "retro",
        rho = mohn_summary
      ))
    }
    
    if (type == "hessian") {
      req(input$model_dir)
      data <- bind_rows(lapply(selected, function(sc) {
        k <- scenario_cache_key(input$model_dir, sc)
        get_cached_heavy(
          "hessian",
          k,
          function() build_hessian_data_for_scenario(sc, input$model_dir)
        )
      }))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No Hessian diagnostics found", plot_kind = "hessian"))
      }
      return(list(data = data, group_col = NULL, label = "Hessian", message = NULL, plot_kind = "hessian"))
    }

    profile_data <- profile_outputs_reactive()

    if (length(profile_data) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No likelihood profile data found"))
    }

    all_scales <- sort(unique(unlist(lapply(profile_data, function(x) x$scales))))
    if (length(all_scales) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No scaler values found for the selected scenarios"))
    }

    allowed_region_fisheries <- NULL
    if (type %in% fishery_region_types) {
      allowed_region_fisheries <- allowed_fisheries_for_regions(
        names(profile_data),
        rv$FISHERY_MAPS,
        filters$regions
      )
    }

    if (type == "components") {
      data <- build_components_data(profile_data, names(profile_data), all_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No component data available"))
      }
      data <- calc_lik_change(data, "Likelihood")
      return(list(data = data, group_col = "Likelihood", label = "Components", message = NULL, profile_data = profile_data))
    }

    if (type == "cpues") {
      data <- build_cpue_fishery_data(
        profile_data,
        names(profile_data),
        rv$FISHERY_MAPS,
        all_scales,
        allowed_fisheries = allowed_region_fisheries
      )
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CPUE profile data available"))
      }
      if (!isTRUE(filters$split_by_region) && "region" %in% names(data)) {
        data <- data %>%
          filter(Fishery != "Total") %>%
          group_by(scenario, scaler, Fishery) %>%
          summarise(value = sum(value), .groups = "drop")
        total_rows <- data %>%
          group_by(scenario, scaler) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(Fishery = "Total")
        data <- bind_rows(data, total_rows)
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "CPUEs", message = NULL, profile_data = profile_data))
    }

    if (type == "lfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_length_fish", "Fishery", all_scales,
                                 allowed_fisheries = allowed_region_fisheries)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No LF profile data available"))
      }
      if (!isTRUE(filters$split_by_region) && "region" %in% names(data)) {
        data <- data %>%
          filter(Fishery != "Total") %>%
          group_by(scenario, scaler, Fishery) %>%
          summarise(value = sum(value), .groups = "drop")
        total_rows <- data %>%
          group_by(scenario, scaler) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(Fishery = "Total")
        data <- bind_rows(data, total_rows)
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "LFs", message = NULL, profile_data = profile_data))
    }

    if (type == "wfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_weight_fish", "Fishery", all_scales,
                                 allowed_fisheries = allowed_region_fisheries)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No WF profile data available"))
      }
      if (!isTRUE(filters$split_by_region) && "region" %in% names(data)) {
        data <- data %>%
          filter(Fishery != "Total") %>%
          group_by(scenario, scaler, Fishery) %>%
          summarise(value = sum(value), .groups = "drop")
        total_rows <- data %>%
          group_by(scenario, scaler) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(Fishery = "Total")
        data <- bind_rows(data, total_rows)
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "WFs", message = NULL, profile_data = profile_data))
    }

    if (type == "tagging") {
      data <- build_tagging_data(profile_data, names(profile_data), rv$TagOut_list, all_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No tagging profile data available"))
      }
      total_row <- data %>%
        group_by(scaler, scenario) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        mutate(program = "Total")
      data <- bind_rows(data, total_row)
      data <- calc_lik_change(data, "program")
      return(list(data = data, group_col = "program", label = "Tagging", message = NULL, profile_data = profile_data))
    }

    if (type == "cal_fishery") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "fishery", all_scales,
                             allowed_fisheries = allowed_region_fisheries)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CAL by fishery data available"))
      }
      if (!isTRUE(filters$split_by_region) && "region" %in% names(data)) {
        data <- data %>%
          filter(fishery != "Total") %>%
          group_by(scenario, scaler, fishery) %>%
          summarise(value = sum(value), .groups = "drop")
        total_rows <- data %>%
          group_by(scenario, scaler) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(fishery = "Total")
        data <- bind_rows(data, total_rows)
      }
      data <- calc_lik_change(data, "fishery")
      return(list(data = data, group_col = "fishery", label = "CAL", message = NULL, profile_data = profile_data))
    }

    if (type == "cal_year") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "year", all_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CAL by year data available"))
      }
      data <- calc_lik_change(data, "year")
      return(list(data = data, group_col = "year", label = "CAL", message = NULL, profile_data = profile_data))
    }

    list(data = data.frame(), group_col = NULL, label = NULL, message = "Unsupported profile type")
  })
  profile_data_reactive <- bindCache(
    profile_data_reactive,
    rv$data_loaded,
    input$model_dir,
    lik_filters()
  )

  observeEvent(profile_data_reactive(), {
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (is.null(info$group_col) || nrow(info$data) == 0 || plot_kind %in% c("jitter", "jitter_params", "jitter_derived", "retro", "hessian")) {
      last_group_key(NULL)
      updatePickerInput(session, "lik_groups", choices = character(0), selected = character(0))
      return()
    }

    groups <- sort(unique(info$data[[info$group_col]]))
    current_regions <- isolate(lik_filters_current()$regions)
    if (is.null(current_regions)) current_regions <- character(0)

    group_key <- paste(
      isolate(lik_filters_current()$profile_type),
      paste(sort(current_regions), collapse = "|"),
      as.character(isTRUE(isolate(lik_filters_current()$split_by_region))),
      info$group_col,
      paste(groups, collapse = "||"),
      sep = "::"
    )

    current <- isolate(input$lik_groups)
    if (!identical(last_group_key(), group_key)) {
      selected <- groups
    } else {
      if (is.null(current) || length(current) == 0) {
        selected <- groups
      } else {
        selected <- intersect(current, groups)
        if ("Total" %in% groups && !("Total" %in% selected)) {
          selected <- c("Total", selected)
        }
        if (length(selected) == 0) selected <- groups
      }
    }

    last_group_key(group_key)
    updatePickerInput(session, "lik_groups", choices = groups, selected = selected)
  }, ignoreInit = TRUE)

  likelihood_plot_reactive <- reactive({
    info <- profile_data_reactive()
    filters <- lik_filters()
    req(filters)
    if (!is.null(info$message)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = info$message, size = 6, color = "#999") +
          theme_void()
      )
    }

    data <- info$data
    group_col <- info$group_col
    label <- info$label
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    facet_ncol <- suppressWarnings(as.integer(filters$facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
    facet_ncol <- min(max(facet_ncol, 1), 12)

    if (!(plot_kind %in% c("jitter", "jitter_params", "jitter_derived")) && !is.null(filters$groups) && length(filters$groups) > 0) {
      data <- data[data[[group_col]] %in% filters$groups, , drop = FALSE]
    }

    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data after filtering", size = 6, color = "#999") +
          theme_void()
      )
    }

    if (identical(plot_kind, "jitter")) {
      plot_df <- data %>%
        mutate(
          is_outlier = pct_diff < -5 | pct_diff > 20,
          outlier_direction = case_when(
            pct_diff < -5 ~ "below",
            pct_diff > 20 ~ "above",
            TRUE ~ "none"
          )
        )

      # Log-scale x requires positive gradients.
      plot_df <- plot_df %>% mutate(max_grad = ifelse(max_grad > 0, max_grad, NA_real_))
      ref_df <- plot_df %>%
        group_by(scenario) %>%
        summarise(ref_grad = first(ref_grad), .groups = "drop") %>%
        mutate(ref_grad = ifelse(ref_grad > 0, ref_grad, NA_real_))

      outliers_df <- plot_df %>% filter(is_outlier)
      non_outlier <- plot_df %>% filter(!is_outlier)

      return(
        ggplot(non_outlier, aes(x = max_grad, y = pct_diff)) +
          geom_point(aes(color = jitter_id), size = 3, alpha = 0.7, na.rm = TRUE) +
          geom_point(
            data = outliers_df %>% filter(outlier_direction == "above"),
            aes(x = max_grad, y = 19.5),
            inherit.aes = FALSE,
            color = "orange", size = 3, shape = 24, fill = "orange", na.rm = TRUE
          ) +
          geom_point(
            data = outliers_df %>% filter(outlier_direction == "below"),
            aes(x = max_grad, y = -4.5),
            inherit.aes = FALSE,
            color = "orange", size = 3, shape = 25, fill = "orange", na.rm = TRUE
          ) +
          geom_point(
            data = ref_df,
            aes(x = ref_grad, y = 0),
            inherit.aes = FALSE,
            color = "red", size = 5, shape = 18, na.rm = TRUE
          ) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.5) +
          geom_vline(xintercept = 0.001, linetype = "dotted", color = "gray50", linewidth = 0.6) +
          scale_x_log10() +
          coord_cartesian(ylim = c(-5, 20)) +
          facet_wrap(~ scenario, scales = "free_x", ncol = facet_ncol) +
          scale_color_viridis_c(option = "D", name = "Jitter ID") +
          labs(
            x = "Maximum Gradient (log scale)",
            y = "% Difference in Objective Function",
            title = "Jitter Analysis: Convergence Diagnostics",
            subtitle = "Red diamond = Reference model | Gray line = 0.001 | Orange triangles = Outliers"
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "right",
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "lightblue"),
            panel.grid.minor = element_blank()
          )
      )
    }

    if (identical(plot_kind, "jitter_params")) {
      param_view <- if (is.null(filters$jitter_param_view)) "input" else filters$jitter_param_view
      converged_only <- isTRUE(filters$jitter_final_converged_only)
      metric <- if (is.null(filters$jitter_param_metric)) "pct_change" else filters$jitter_param_metric

      selected_params <- data %>%
        distinct(scenario, param_key, Index, Var_name, family, median_abs_pct_change, mean_abs_pct_change) %>%
        filter(is.finite(median_abs_pct_change)) %>%
        group_by(scenario, family) %>%
        arrange(dplyr::desc(median_abs_pct_change), dplyr::desc(mean_abs_pct_change), Var_name, Index, .by_group = TRUE) %>%
        slice_head(n = 1) %>%
        ungroup()

      scenario_slots <- selected_params %>%
        group_by(scenario) %>%
        summarise(family_count = dplyr::n(), .groups = "drop") %>%
        mutate(extra_slots = pmax(20 - family_count, 0L))

      remaining_slots <- data %>%
        distinct(scenario, param_key, Index, Var_name, family, median_abs_pct_change, mean_abs_pct_change) %>%
        filter(is.finite(median_abs_pct_change)) %>%
        anti_join(selected_params %>% select(scenario, param_key), by = c("scenario", "param_key")) %>%
        left_join(scenario_slots, by = "scenario") %>%
        group_by(scenario) %>%
        arrange(dplyr::desc(median_abs_pct_change), dplyr::desc(mean_abs_pct_change), Var_name, Index, .by_group = TRUE) %>%
        mutate(extra_rank = row_number()) %>%
        filter(extra_rank <= first(extra_slots)) %>%
        ungroup() %>%
        select(-family_count, -extra_slots, -extra_rank)

      selected_params <- bind_rows(selected_params, remaining_slots) %>%
        group_by(scenario) %>%
        arrange(dplyr::desc(median_abs_pct_change), dplyr::desc(mean_abs_pct_change), Var_name, Index, .by_group = TRUE) %>%
        mutate(plot_rank = row_number()) %>%
        ungroup()

      plot_df <- data %>%
        inner_join(selected_params %>% select(scenario, param_key, plot_rank), by = c("scenario", "param_key")) %>%
        mutate(
          param_label = paste0(Var_name, "\n[", family, "]"),
          plot_value = case_when(
            metric == "value" ~ after,
            metric == "delta" ~ delta,
            TRUE ~ pct_change
          ),
          original_value = case_when(
            metric == "value" ~ before,
            TRUE ~ 0
          )
        ) %>%
        filter(is.finite(plot_value))

      original_df <- plot_df %>%
        group_by(scenario, param_key, param_label) %>%
        summarise(original_value = first(original_value), .groups = "drop")

      param_levels <- selected_params %>%
        arrange(scenario, plot_rank, Var_name, Index) %>%
        pull(param_key)
      plot_df$param_key <- factor(plot_df$param_key, levels = unique(param_levels))
      original_df$param_key <- factor(original_df$param_key, levels = unique(param_levels))

      y_label <- case_when(
        metric == "value" ~ "Parameter value",
        metric == "delta" ~ "Change from original",
        TRUE ~ "% change from original"
      )

      plot_title <- case_when(
        metric == "value" && identical(param_view, "final") ~ "Final Fitted Parameter Distributions",
        metric == "delta" && identical(param_view, "final") ~ "Final Fitted Parameter Changes",
        metric == "pct_change" && identical(param_view, "final") ~ "Final Fitted Parameter % Changes",
        metric == "value" ~ "Jittered Input Parameter Distributions",
        metric == "delta" ~ "Jittered Input Parameter Changes",
        TRUE ~ "Jittered Input Parameter % Changes"
      )

      plot_subtitle <- case_when(
        metric == "value" && identical(param_view, "final") && converged_only ~ "About 20 parameters per scenario from converged final runs (max_grad <= 0.01). Red diamond = original value",
        metric == "delta" && identical(param_view, "final") && converged_only ~ "About 20 parameters per scenario from converged final runs (max_grad <= 0.01). Red diamond = no change",
        metric == "pct_change" && identical(param_view, "final") && converged_only ~ "About 20 parameters per scenario from converged final runs (max_grad <= 0.01). Red diamond = 0% change",
        metric == "value" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = original value",
        metric == "delta" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = no change",
        metric == "pct_change" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = 0% change",
        metric == "value" ~ "About 20 parameters per scenario from jittered input pars. Red diamond = original value",
        metric == "delta" ~ "About 20 parameters per scenario from jittered input pars. Red diamond = no change",
        TRUE ~ "About 20 parameters per scenario from jittered input pars. Red diamond = 0% change"
      )

      return(
        ggplot(plot_df, aes(x = param_key, y = plot_value)) +
          geom_boxplot(
            fill = "#9ecae1",
            color = "#2b6c8a",
            outlier.shape = NA,
            na.rm = TRUE
          ) +
          geom_point(
            aes(group = seed),
            position = position_jitter(width = 0.18, height = 0),
            alpha = 0.18,
            size = 0.9,
            color = "#1f4e79",
            na.rm = TRUE
          ) +
          geom_point(
            data = original_df,
            aes(x = param_key, y = original_value),
            inherit.aes = FALSE,
            color = "#d62728",
            fill = "#d62728",
            shape = 23,
            size = 2.8,
            stroke = 0.4,
            na.rm = TRUE
          ) +
          facet_wrap(~ scenario, scales = "free", ncol = facet_ncol) +
          scale_x_discrete(labels = function(x) {
            lab_df <- unique(plot_df[, c("param_key", "param_label")])
            lab_map <- setNames(as.character(lab_df$param_label), as.character(lab_df$param_key))
            unname(lab_map[as.character(x)])
          }) +
          labs(
            x = NULL,
            y = y_label,
            title = plot_title,
            subtitle = plot_subtitle
          ) +
          theme_bw(base_size = 11) +
          theme(
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "#d9edf7"),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 9, face = "bold", colour = "#222222", lineheight = 0.95),
            axis.title.y = element_text(margin = margin(r = 10)),
            plot.margin = margin(5.5, 10, 5.5, 14),
            panel.grid.minor = element_blank()
          ) +
          coord_flip()
      )
    }

    if (identical(plot_kind, "jitter_derived")) {
      ref_df <- data %>%
        distinct(scenario, metric, reference_value, year)

      plot_df <- data %>%
        mutate(seed_value = suppressWarnings(as.numeric(seed)))

      return(
        ggplot(plot_df, aes(x = year, y = value, group = seed, color = seed_value)) +
          geom_line(alpha = 0.6, linewidth = 0.6, na.rm = TRUE) +
          geom_line(
            data = ref_df,
            aes(x = year, y = reference_value, group = 1),
            inherit.aes = FALSE,
            color = "#d62728",
            linewidth = 0.9,
            na.rm = TRUE
          ) +
          facet_grid(metric ~ scenario, scales = "free_y") +
          scale_color_viridis_c(option = "D", name = "Jitter seed", na.value = "#2e6b3f") +
          labs(
            x = "Year",
            y = "Derived quantity value",
            title = "Jitter Derived Quantity Time Series",
            subtitle = "Green lines = jitter runs. Red line = original model time series"
          ) +
          theme_bw(base_size = 12) +
          theme(
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "#d9edf7"),
            panel.grid.minor = element_blank()
          )
      )
    }
    
    if (identical(plot_kind, "retro")) {
      retro_df <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          scenario = factor(scenario, levels = unique(scenario)),
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential), !is.na(peel))
      if (nrow(retro_df) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No retrospective numeric data after parsing", size = 6, color = "#999") +
            theme_void()
        )
      }
      
      peel_levels <- sort(unique(retro_df$peel))
      peel_levels_chr <- as.character(peel_levels)
      
      terminal_by_peel <- retro_df %>%
        group_by(scenario, peel) %>%
        summarise(terminal_year = max(year, na.rm = TRUE), .groups = "drop") %>%
        group_by(peel) %>%
        summarise(
          terminal_year = ifelse(n_distinct(terminal_year) == 1,
                                 dplyr::first(terminal_year),
                                 max(terminal_year, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        arrange(peel)
      
      peel_labels <- setNames(as.character(terminal_by_peel$terminal_year), as.character(terminal_by_peel$peel))
      peel_colors <- if (length(peel_levels) == 1) {
        c("0" = "black")
      } else {
        c(
          "0" = "black",
          setNames(
            viridis(length(peel_levels[peel_levels > 0]), option = "C", direction = -1),
            as.character(peel_levels[peel_levels > 0])
          )
        )
      }
      
      rho_df <- info$rho
      dep_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, label = sprintf("Mohn's rho: %.3f", mohn_rho_depletion))
      } else {
        data.frame(scenario = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      sp_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, label = sprintf("Mohn's rho: %.3f", mohn_rho_spawning_potential))
      } else {
        data.frame(scenario = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      
      dep_plot <- ggplot(
        retro_df,
        aes(
          x = year,
          y = depletion,
          color = factor(peel, levels = peel_levels_chr),
          group = interaction(scenario, peel)
        )
      ) +
        geom_line(linewidth = 1.1, alpha = 0.9) +
        geom_text(
          data = dep_anno,
          aes(x = Inf, y = Inf, label = label),
          inherit.aes = FALSE,
          hjust = 1.05, vjust = 1.2, size = 3.4, fontface = "bold", color = "black"
        ) +
        facet_wrap(~scenario, scales = "free_x", ncol = facet_ncol) +
        scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
        labs(
          x = "Year",
          y = bquote(SB/SB["F=0"]),
          color = "Terminal year",
          title = "Retrospective Depletion"
        ) +
        coord_cartesian(ylim = c(0, 1.1)) +
        theme_bw(base_size = 12)
      
      sp_plot <- ggplot(
        retro_df,
        aes(
          x = year,
          y = spawning_potential,
          color = factor(peel, levels = peel_levels_chr),
          group = interaction(scenario, peel)
        )
      ) +
        geom_line(linewidth = 1.1, alpha = 0.9) +
        geom_text(
          data = sp_anno,
          aes(x = Inf, y = Inf, label = label),
          inherit.aes = FALSE,
          hjust = 1.05, vjust = 1.2, size = 3.4, fontface = "bold", color = "black"
        ) +
        facet_wrap(~scenario, scales = "free_x", ncol = facet_ncol) +
        scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
        labs(
          x = "Year",
          y = bquote("Spawning Potential (" * 10^3 * " MT)"),
          color = "Terminal year",
          title = "Retrospective Spawning Potential"
        ) +
        theme_bw(base_size = 12)
      
      return(cowplot::plot_grid(dep_plot, sp_plot, ncol = 1, align = "v"))
    }
    
    if (identical(plot_kind, "hessian")) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Hessian diagnostics table is shown below", size = 6, color = "#777") +
          theme_void()
      )
    }

    x_label <- quantity_axis_label(info$profile_data)
    create_piner_plot(
      data,
      group_col,
      x_label,
      label,
      facet_ncol = facet_ncol,
      split_by_region = isTRUE(filters$split_by_region)
    )
  })
  observeEvent(list(input$live_update_plots, input$lik_scenarios, input$lik_profile_type,
                    input$lik_groups, input$lik_regions, input$lik_split_by_region, input$lik_facet_ncol,
                    input$lik_jitter_param_view, input$lik_jitter_final_converged_only,
                    input$lik_jitter_param_metric), {
    req(rv$data_loaded)
    if (!isTRUE(input$live_update_plots)) return()
    if (length(input$lik_scenarios) == 0) return()
    lik_filters_applied(isolate(lik_filters_current()))
    lik_live_update_nonce(isolate(lik_live_update_nonce()) + 1)
  }, ignoreInit = TRUE)
  observeEvent(list(rv$initial_render_nonce, input$lik_scenarios), {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (rv$initial_render_nonce <= lik_last_initialized_nonce()) return()
    if (length(input$lik_scenarios) == 0) return()
    lik_last_initialized_nonce(rv$initial_render_nonce)
    lik_filters_applied(isolate(lik_filters_current()))
  }, ignoreInit = TRUE)
  likelihood_plot_reactive <- bindCache(
    likelihood_plot_reactive,
    rv$data_loaded,
    input$model_dir,
    lik_filters()
  )

  output$likelihood_plot <- renderPlot({
    likelihood_plot_reactive()
  })

  output$profile_target_info_ui <- renderUI({
    target_tbl <- profile_target_info_reactive()
    if (is.null(target_tbl) || nrow(target_tbl) == 0) return(NULL)

    box(
      title = "Profile Target Information",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      collapsed = TRUE,
      div(
        style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
        tags$div("Shown from the profile setup flags saved with each model.", style = "font-weight: bold; margin-bottom: 4px;"),
        tags$div("Af172 distinguishes adult vs total biomass. Af173 and Af174 define the time-period window, counting backwards from the end of the time series.", style = "font-size: 12px; color: #333;")
      ),
      DTOutput("profile_target_info_table")
    )
  })
  
  output$likelihood_table_ui <- renderUI({
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (!identical(plot_kind, "hessian")) return(NULL)
    box(
      title = "Hessian Diagnostics Table",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      collapsed = TRUE,
      div(
        style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
        tags$div("Hessian Diagnostics (PDH / SPD)", style = "font-weight: bold; margin-bottom: 4px;"),
        tags$div("PDH: Positive Definite Hessian (target condition for a well-behaved optimum).", style = "font-size: 12px; color: #333;"),
        tags$div("SPD (positivised cov): Positive definite covariance after covariance positivisation step.", style = "font-size: 12px; color: #333;"),
        tags$div("Neg. Eigen: Number of negative Hessian eigenvalues / total eigenvalues (shown as n_negative / n_total).", style = "font-size: 12px; color: #333;"),
        tags$div("Hessian Status: Read from hessian/hessian_info.rds (generated by tools/collate_hessian_mfcl.R). The pipeline reads the first line of neigenvalues and uses n_negative and n_total to classify: PDH if n_negative = 0, Near-PDH if n_negative < 1% of n_total, otherwise Non-PDH; Unknown if unavailable.", style = "font-size: 12px; color: #333;"),
        tags$div("Reliability: Also read from hessian/hessian_info.rds and assigned by the same pipeline from the same eigenvalue rule: HIGH for PDH (0 negative eigenvalues), MODERATE for Near-PDH (<1% negative eigenvalues), LOW for Non-PDH, and UNKNOWN when eigenvalue summary is unavailable. This Shiny table does not recompute the label.", style = "font-size: 12px; color: #333;")
      ),
      DTOutput("likelihood_table")
    )
  })

  output$profile_gradient_table_ui <- renderUI({
    grad_tbl <- profile_gradient_table_reactive()
    filters <- lik_filters()
    if (is.null(filters) || !identical(filters$profile_type, "components")) return(NULL)
    if (is.null(grad_tbl) || nrow(grad_tbl) == 0) return(NULL)

    box(
      title = "Total Profile Scaler Diagnostics",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      collapsed = TRUE,
      uiOutput("profile_gradient_model_ui"),
      DTOutput("profile_gradient_table")
    )
  })
  
  output$likelihood_table <- renderDT({
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (!identical(plot_kind, "hessian") || nrow(info$data) == 0) return(NULL)
    
    datatable(
      info$data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$profile_gradient_table <- renderDT({
    grad_tbl <- profile_gradient_table_reactive()
    if (is.null(grad_tbl) || nrow(grad_tbl) == 0) return(NULL)

    if (!is.null(input$profile_gradient_model) &&
        nzchar(input$profile_gradient_model) &&
        input$profile_gradient_model %in% grad_tbl$Model) {
      grad_tbl <- grad_tbl %>% filter(Model == input$profile_gradient_model)
    }

    datatable(
      grad_tbl,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$profile_gradient_model_ui <- renderUI({
    grad_tbl <- profile_gradient_table_reactive()
    filters <- lik_filters()
    if (is.null(filters) || !identical(filters$profile_type, "components")) return(NULL)
    if (is.null(grad_tbl) || nrow(grad_tbl) == 0) return(NULL)

    model_choices <- unique(grad_tbl$Model)
    if (length(model_choices) <= 1) return(NULL)

    selectInput(
      "profile_gradient_model",
      "Model:",
      choices = model_choices,
      selected = if (!is.null(input$profile_gradient_model) &&
        input$profile_gradient_model %in% model_choices) {
        input$profile_gradient_model
      } else {
        model_choices[[1]]
      }
    )
  })

  output$profile_target_info_table <- renderDT({
    target_tbl <- profile_target_info_reactive()
    if (is.null(target_tbl) || nrow(target_tbl) == 0) return(NULL)

    datatable(
      target_tbl,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  observeEvent(input$show_lik_download_modal, {
    show_download_modal("lik", "Likelihood Profile Plot", current_save_dir = input$plot_export_dir)
  })

  observeEvent(input$lik_preset_wide, {
    updateNumericInput(session, "lik_width", value = 16)
    updateNumericInput(session, "lik_height", value = 9)
  })

  observeEvent(input$lik_preset_standard, {
    updateNumericInput(session, "lik_width", value = 12)
    updateNumericInput(session, "lik_height", value = 9)
  })

  observeEvent(input$lik_preset_square, {
    updateNumericInput(session, "lik_width", value = 10)
    updateNumericInput(session, "lik_height", value = 10)
  })

  output$lik_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$lik_format
      paste0("likelihood_profile_", Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- likelihood_plot_reactive()
      width <- input$lik_width
      height <- input$lik_height
      dpi <- as.numeric(input$lik_dpi)
      format <- input$lik_format

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
    plot_type = "lik",
    plot_reactive = likelihood_plot_reactive,
    input = input,
    session = session,
    output = output,
    filename_fun = function() {
      format <- input$lik_format
      paste0("likelihood_profile_", Sys.Date(), ".", format)
    }
  )
}
