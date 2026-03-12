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
        tabsetPanel(
          id = "lik_main_tab",
          type = "tabs",
          selected = "likelihood",
          tabPanel(
            "Likelihood",
            value = "likelihood",
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
                "Likelihood: CAL by Year" = "cal_year"
              ),
              selected = "components"
            )
          ),
          tabPanel(
            "Jitter",
            value = "jitter",
            selectInput(
              "lik_jitter_type",
              "Jitter Type:",
              choices = c(
                "Jitter Diagnostics" = "jitter",
                "Jitter Parameters" = "jitter_params",
                "Jitter Derived Quantities" = "jitter_derived"
              ),
              selected = "jitter"
            )
          ),
          tabPanel(
            "Retro",
            value = "retro",
            tags$p(
              "Retrospective depletion and spawning potential diagnostics.",
              style = "margin: 8px 2px 0 2px; color: #555;"
            )
          ),
          tabPanel(
            "Hessian",
            value = "hessian",
            tags$p(
              "Hessian PDH / SPD diagnostics and summary table.",
              style = "margin: 8px 2px 0 2px; color: #555;"
            )
          )
        ),

        conditionalPanel(
          condition = "input.lik_main_tab == 'likelihood'",
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
          condition = "input.lik_main_tab == 'likelihood' && ['cpues', 'lfs', 'wfs', 'cal_fishery'].includes(input.lik_profile_type)",
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
          condition = "input.lik_main_tab == 'jitter' && input.lik_jitter_type == 'jitter_params'",
          tagList(
            selectInput(
              "lik_jitter_param_view",
              "Jitter Param View:",
              choices = c(
                "Jittered input pars distribution" = "input",
                "Original vs final fitted pars distribution" = "final"
              ),
              selected = "input"
            ),
            selectInput(
              "lik_jitter_param_display",
              "Jitter Param Display:",
              choices = c(
                "Family summary (recommended for many parameters)" = "family",
                "Parameter detail" = "detail"
              ),
              selected = "family"
            ),
            conditionalPanel(
              condition = "input.lik_jitter_param_display == 'detail'",
              tagList(
                selectInput(
                  "lik_jitter_param_scope",
                  "Jitter Param Coverage:",
                  choices = c(
                    "Top ~20 parameters" = "top",
                    "All parameters window" = "all"
                  ),
                  selected = "top"
                ),
                conditionalPanel(
                  condition = "input.lik_jitter_param_scope == 'all'",
                  sliderInput(
                    "lik_jitter_param_window",
                    "Parameter percentile window:",
                    min = 1,
                    max = 100,
                    value = c(1, 100),
                    step = 1,
                    round = TRUE
                  )
                )
              )
            ),
                  conditionalPanel(
                    condition = "input.lik_jitter_param_view == 'input'",
                    tagList(
	                      selectInput(
	                        "lik_jitter_param_input_scale",
	                  "Input Param Scale:",
	                  choices = c(
	                    "Bound position (0-1)" = "bound_position",
	                    "Raw value" = "value",
	                    "Baseline - fitted" = "baseline_minus",
	                    "Relative difference (%) (Baseline - fitted)/Baseline" = "rel_baseline_minus"
	                  ),
	                  selected = "bound_position"
	                      ),
                      tags$small(
                        "Bound position is computed as (value - L_bound) / (U_bound - L_bound). 0 = lower bound, 1 = upper bound, 0.5 = midpoint.",
                        style = "display:block; margin-top:-6px; margin-bottom:6px; color:#666;"
                      ),
                      tags$small(
                        "Jitter uses interior bounds (lower/upper each trimmed by 2% of span), samples by CV, and resamples if a proposal hits bounds.",
                        style = "display:block; margin-top:-4px; margin-bottom:6px; color:#666;"
                      )
                    )
                  ),
            conditionalPanel(
              condition = "input.lik_jitter_param_view == 'final'",
              selectInput(
                "lik_jitter_param_metric",
                "Jitter Param Scale:",
                choices = c(
                  "Bound position (0-1)" = "bound_position",
                  "Parameter value" = "value",
                  "Change from original" = "delta",
                  "% change from original" = "pct_change",
                  "Baseline - fitted" = "baseline_minus",
                  "Relative difference (%) (Baseline - fitted)/Baseline" = "rel_baseline_minus"
                ),
                selected = "bound_position"
              )
            ),
	            conditionalPanel(
	              condition = "(input.lik_jitter_param_view == 'final' && ['delta', 'pct_change', 'baseline_minus', 'rel_baseline_minus'].includes(input.lik_jitter_param_metric)) || (input.lik_jitter_param_view == 'input' && ['baseline_minus', 'rel_baseline_minus'].includes(input.lik_jitter_param_input_scale))",
	              tagList(
	                sliderInput(
	                  "lik_jitter_param_range_pct",
                  "Jitter Param Range (abs percentile):",
                  min = 50,
                  max = 100,
                  value = 95,
                  step = 1,
                  round = TRUE
                ),
                tags$small(
                  "100 keeps full range. Lower values reduce extreme outlier influence on y-axis limits.",
                  style = "display:block; margin-top:-6px; color:#666;"
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.lik_main_tab == 'jitter' && input.lik_jitter_type == 'jitter'",
          tagList(
            checkboxInput(
              "lik_jitter_converged_only_diagnostics",
              "Converged only",
              value = FALSE
            ),
            selectInput(
              "lik_jitter_grad_reference",
              "Jitter max_grad reference line:",
              choices = c(
                "0.1" = "0.1",
                "0.01" = "0.01",
                "0.001" = "0.001",
                "0.0001" = "0.0001",
                "0.00001" = "0.00001"
              ),
              selected = "0.001"
            )
          )
        ),
        conditionalPanel(
          condition = "input.lik_main_tab == 'jitter' && ['jitter_params', 'jitter_derived'].includes(input.lik_jitter_type)",
          tagList(
            checkboxInput(
              "lik_jitter_converged_only",
              "Converged only",
              value = FALSE
            ),
            selectInput(
              "lik_jitter_converged_max_grad",
              "Converged max_grad cutoff:",
              choices = c(
                "0.1" = "0.1",
                "0.01" = "0.01",
                "0.001" = "0.001",
                "0.0001" = "0.0001",
                "0.00001" = "0.00001"
              ),
              selected = "0.001"
            )
          )
        ),
        conditionalPanel(
          condition = "input.lik_main_tab == 'jitter' && input.lik_jitter_type == 'jitter_derived'",
          selectInput(
            "lik_jitter_derived_view",
            "Jitter Derived View:",
            choices = c(
              "Summary bands" = "summary",
              "Individual lines" = "lines"
            ),
            selected = "summary"
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
      conditionalPanel(
        condition = "input.lik_main_tab == 'likelihood'",
        uiOutput("likelihood_info_ui"),
        uiOutput("profile_gradient_table_ui")
      ),
      conditionalPanel(
        condition = "input.lik_main_tab == 'jitter'",
        uiOutput("jitter_info_ui")
      ),
      conditionalPanel(
        condition = "input.lik_main_tab == 'retro'",
        uiOutput("retro_info_ui")
      ),
      conditionalPanel(
        condition = "input.lik_main_tab == 'hessian'",
        uiOutput("hessian_info_ui"),
        uiOutput("likelihood_table_ui")
      )
    )
  )
}

mod_likelihood_server <- function(input, output, session, rv) {
  heavy_cache <- reactiveValues(
    retro = list(),
    hessian = list(),
    retro_plot = list()
  )
  fishery_diag_cache <- reactiveValues(
    cpue = list(),
    fishery_slot = list(),
    cal = list(),
    alk_summary = list(),
    fishery_lookup = list()
  )
  jitter_data_cache <- reactiveValues(
    diagnostics = list(),
    derived = list()
  )
  jitter_param_cache <- reactiveValues(
    rows = list()
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
    heavy_cache$retro_plot <- list()
  }
  clear_fishery_diag_cache <- function() {
    fishery_diag_cache$cpue <- list()
    fishery_diag_cache$fishery_slot <- list()
    fishery_diag_cache$cal <- list()
    fishery_diag_cache$alk_summary <- list()
    fishery_diag_cache$fishery_lookup <- list()
  }
  clear_jitter_data_cache <- function() {
    jitter_data_cache$diagnostics <- list()
    jitter_data_cache$derived <- list()
  }
  clear_jitter_param_cache <- function() {
    jitter_param_cache$rows <- list()
  }
  lik_live_update_nonce <- reactiveVal(0)
  fishery_region_types <- c("cpues", "lfs", "wfs", "cal_fishery")
  current_profile_type <- reactive({
    if (identical(input$lik_main_tab, "jitter")) {
      if (is.null(input$lik_jitter_type)) "jitter" else input$lik_jitter_type
    } else if (identical(input$lik_main_tab, "retro")) {
      "retro"
    } else if (identical(input$lik_main_tab, "hessian")) {
      "hessian"
    } else {
      if (is.null(input$lik_profile_type)) "components" else input$lik_profile_type
    }
  })
  lik_filters_current <- reactive({
    list(
      scenarios = sort(input$lik_scenarios),
      profile_type = current_profile_type(),
      groups = input$lik_groups,
      regions = input$lik_regions,
      split_by_region = isTRUE(input$lik_split_by_region),
      facet_ncol = input$lik_facet_ncol,
      jitter_grad_reference = if (is.null(input$lik_jitter_grad_reference)) 0.001 else suppressWarnings(as.numeric(input$lik_jitter_grad_reference)),
      jitter_converged_only_diagnostics = isTRUE(input$lik_jitter_converged_only_diagnostics),
      jitter_param_view = if (is.null(input$lik_jitter_param_view)) "input" else input$lik_jitter_param_view,
      jitter_param_display = if (is.null(input$lik_jitter_param_display)) "family" else input$lik_jitter_param_display,
      jitter_param_scope = if (is.null(input$lik_jitter_param_scope)) "top" else input$lik_jitter_param_scope,
      jitter_param_window = if (is.null(input$lik_jitter_param_window)) c(1L, 100L) else pmax(1L, pmin(100L, as.integer(input$lik_jitter_param_window))),
      jitter_converged_only = isTRUE(input$lik_jitter_converged_only),
      jitter_converged_max_grad = if (is.null(input$lik_jitter_converged_max_grad)) 0.001 else suppressWarnings(as.numeric(input$lik_jitter_converged_max_grad)),
      jitter_derived_view = if (is.null(input$lik_jitter_derived_view)) "summary" else input$lik_jitter_derived_view,
      jitter_param_input_scale = if (is.null(input$lik_jitter_param_input_scale)) "bound_position" else input$lik_jitter_param_input_scale,
      jitter_param_metric = if (is.null(input$lik_jitter_param_metric)) "pct_change" else input$lik_jitter_param_metric,
      jitter_param_range_pct = if (is.null(input$lik_jitter_param_range_pct)) 95 else pmax(50, pmin(100, suppressWarnings(as.integer(input$lik_jitter_param_range_pct))))
    )
  })
  lik_filters_applied <- reactiveVal(NULL)
  lik_last_initialized_nonce <- reactiveVal(0)
  lik_filters <- reactive({
    lik_filters_applied()
  })
  lik_data_filters <- reactive({
    filters <- lik_filters()
    if (is.null(filters)) return(NULL)
    list(
      scenarios = filters$scenarios,
      profile_type = filters$profile_type,
      regions = filters$regions,
      split_by_region = filters$split_by_region,
      jitter_grad_reference = filters$jitter_grad_reference,
      jitter_converged_only_diagnostics = filters$jitter_converged_only_diagnostics,
      jitter_param_view = filters$jitter_param_view,
      jitter_param_display = filters$jitter_param_display,
      jitter_param_scope = filters$jitter_param_scope,
      jitter_param_window = filters$jitter_param_window,
      jitter_converged_only = filters$jitter_converged_only,
      jitter_converged_max_grad = filters$jitter_converged_max_grad,
      jitter_derived_view = filters$jitter_derived_view,
      jitter_param_input_scale = filters$jitter_param_input_scale,
      jitter_param_metric = filters$jitter_param_metric,
      jitter_param_range_pct = filters$jitter_param_range_pct
    )
  })
  lik_profile_output_filters <- reactive({
    filters <- lik_data_filters()
    if (is.null(filters)) return(NULL)
    list(
      scenarios = filters$scenarios
    )
  })
  lik_data_cache_key <- reactive({
    filters <- lik_data_filters()
    if (is.null(filters)) return(NULL)

    key <- list(
      profile_type = filters$profile_type,
      scenarios = sort(filters$scenarios)
    )

    if (identical(filters$profile_type, "jitter")) {
      key$jitter_converged_only_diagnostics <- isTRUE(filters$jitter_converged_only_diagnostics)
      key$jitter_grad_reference <- if (is.finite(filters$jitter_grad_reference)) filters$jitter_grad_reference else 0.001
      return(key)
    }

    if (identical(filters$profile_type, "jitter_params")) {
      key$jitter_param_view <- filters$jitter_param_view
      key$jitter_param_display <- filters$jitter_param_display
      key$jitter_param_scope <- filters$jitter_param_scope
      key$jitter_param_window <- filters$jitter_param_window
      key$jitter_converged_only <- isTRUE(filters$jitter_converged_only)
      key$jitter_converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      key$jitter_param_input_scale <- filters$jitter_param_input_scale
      key$jitter_param_metric <- filters$jitter_param_metric
      key$jitter_param_range_pct <- filters$jitter_param_range_pct
      return(key)
    }

    if (identical(filters$profile_type, "jitter_derived")) {
      key$jitter_converged_only <- isTRUE(filters$jitter_converged_only)
      key$jitter_converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      key$jitter_derived_view <- filters$jitter_derived_view
      return(key)
    }

    if (filters$profile_type %in% fishery_region_types) {
      key$regions <- sort(filters$regions)
      key$split_by_region <- isTRUE(filters$split_by_region)
      return(key)
    }

    key
  })
  lik_plot_cache_key <- reactive({
    filters <- lik_filters()
    if (is.null(filters)) return(NULL)

    key <- list(
      profile_type = filters$profile_type,
      scenarios = sort(filters$scenarios),
      facet_ncol = filters$facet_ncol
    )

    if (identical(filters$profile_type, "jitter")) {
      key$jitter_converged_only_diagnostics <- isTRUE(filters$jitter_converged_only_diagnostics)
      key$jitter_grad_reference <- if (is.finite(filters$jitter_grad_reference)) filters$jitter_grad_reference else 0.001
      return(key)
    }

    if (identical(filters$profile_type, "jitter_params")) {
      key$jitter_param_view <- filters$jitter_param_view
      key$jitter_param_display <- filters$jitter_param_display
      key$jitter_param_scope <- filters$jitter_param_scope
      key$jitter_param_window <- filters$jitter_param_window
      key$jitter_converged_only <- isTRUE(filters$jitter_converged_only)
      key$jitter_converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      key$jitter_param_input_scale <- filters$jitter_param_input_scale
      key$jitter_param_metric <- filters$jitter_param_metric
      key$jitter_param_range_pct <- filters$jitter_param_range_pct
      return(key)
    }

    if (identical(filters$profile_type, "jitter_derived")) {
      key$jitter_converged_only <- isTRUE(filters$jitter_converged_only)
      key$jitter_converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      key$jitter_derived_view <- filters$jitter_derived_view
      return(key)
    }

    if (filters$profile_type %in% fishery_region_types) {
      key$regions <- sort(filters$regions)
      key$split_by_region <- isTRUE(filters$split_by_region)
      key$groups <- sort(filters$groups)
      return(key)
    }

    if (filters$profile_type %in% c("components", "tagging", "cal_year")) {
      key$groups <- sort(filters$groups)
      return(key)
    }

    key
  })

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(lik_filters_current(), lik_filters_applied())
    set_apply_pending(session, "lik_apply_filters", pending)
  })

  observeEvent(input$model_dir, {
    clear_heavy_cache()
    clear_fishery_diag_cache()
    clear_jitter_data_cache()
    clear_jitter_param_cache()
  }, ignoreInit = FALSE)

  observeEvent(input$load_data, {
    clear_heavy_cache()
    clear_fishery_diag_cache()
    clear_jitter_data_cache()
    clear_jitter_param_cache()
  }, ignoreInit = FALSE)

  observeEvent(input$lik_apply_filters, {
    lik_filters_applied(isolate(lik_filters_current()))
  }, ignoreInit = FALSE)

  observeEvent(
    list(input$lik_profile_type, input$lik_jitter_type),
    {
      req(rv$data_loaded)
      lik_filters_applied(isolate(lik_filters_current()))
    },
    ignoreInit = TRUE
  )
  observeEvent(input$lik_main_tab, {
    req(rv$data_loaded)
    lik_filters_applied(isolate(lik_filters_current()))
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(rv$data_loaded, {
    req(rv$data_loaded)
    if (is.null(input$lik_main_tab) || !nzchar(input$lik_main_tab)) {
      updateTabsetPanel(session, "lik_main_tab", selected = "likelihood")
    }
    if (is.null(lik_filters_applied())) {
      lik_filters_applied(isolate(lik_filters_current()))
    }
  }, ignoreInit = FALSE)

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

  observeEvent(list(rv$data_loaded, input$lik_scenarios, input$lik_main_tab, input$lik_profile_type, input$lik_jitter_type), {
    req(rv$data_loaded)

    type <- isolate(current_profile_type())
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
  simple_html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "color:#777;", "No rows to display."))
    }
    cols <- names(df)
    tags$table(
      class = "table table-striped table-bordered table-condensed",
      style = "width:100%; margin-bottom:0;",
      tags$thead(
        tags$tr(lapply(cols, function(cn) tags$th(cn)))
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(lapply(cols, function(cn) {
            val <- df[[cn]][i]
            if (length(val) == 0 || is.na(val)) val <- "NA"
            tags$td(as.character(val))
          }))
        })
      )
    )
  }
  format_hessian_display_cols <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    logical_cols <- intersect(c("Hessian.Requested", "Hessian.Attempted", "Post.Hessian.Requested", "Post.Hessian.Attempted"), names(df))
    for (cn in logical_cols) {
      vals <- df[[cn]]
      df[[cn]] <- ifelse(
        is.na(vals),
        "NA",
        ifelse(as.logical(vals), "TRUE", "FALSE")
      )
    }

    status_cols <- intersect(c("Hessian.Status", "Neg..Eigen", "Post.Hessian.Status", "Post.Neg..Eigen"), names(df))
    for (cn in status_cols) {
      vals <- as.character(df[[cn]])
      vals[is.na(vals) | !nzchar(trimws(vals))] <- "NA"
      df[[cn]] <- vals
    }

    df
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

  extract_param_family <- function(var_name) {
    v <- tolower(trimws(as.character(var_name)))
    prefix <- sub("\\(.*$", "", v)
    prefix <- sub(":.*$", "", prefix)
    prefix <- trimws(prefix)

    if (prefix %in% c("bs_selcoff_gp", "selcoff", "ageselcoff", "sel_dev_coffs")) return("Selectivity parameters")
    if (prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs", "region_rec_diff_coffs")) return("Movement parameters")
    if (prefix %in% c("effort_dev_coffs", "grouped_catchability_coffs", "catch_dev_coffs", "q0", "q0_miss", "fm_level_devs")) return("Catchability / effort-deviation parameters")
    if (prefix %in% c("recr", "totpop", "totpop_coff", "region_pars", "region_rec_diffs", "region_rec_diff_coffs", "rec_init_diff")) return("Recruitment")
    if (prefix %in% c("tag_fish_rep", "rep_dev_coffs")) return("Tagging")
    if (prefix %in% c("vb_coff", "var_coff")) return("Growth / size-distribution parameters")
    if (prefix %in% c("sv")) return("Structural / process parameters")

    if (grepl("^age_pars", v)) return("Age parameters")
    if (grepl("^fish_pars", v)) return("Fishery parameters")
    if (grepl("^region_pars", v)) return("Region parameters")
    if (grepl("^species_pars", v)) return("Species parameters")
    if (grepl("^bs_selcoff_gp|^selcoff|selectiv", v)) return("Selectivity parameters")
    if (grepl("^diff_coffs|movement", v)) return("Movement parameters")
    if (grepl("^effort_dev|^q_dev|catchability", v)) return("Catchability / effort-deviation parameters")
    if (grepl("^recr|^rec", v)) return("Recruitment")
    if (grepl("^q|catchability", v)) return("Catchability")
    if (grepl("^af\\d+|^age_flags\\(", v)) return("Age flags / AF controls")
    "Other / mixed"
  }

  parse_param_index <- function(var_name, family_prefix) {
    pat <- paste0("^", family_prefix, "\\((\\d+)")
    m <- regexec(pat, tolower(trimws(as.character(var_name))))
    g <- regmatches(tolower(trimws(as.character(var_name))), m)[[1]]
    if (length(g) < 2) return(NA_integer_)
    suppressWarnings(as.integer(g[[2]]))
  }

  parse_prefix <- function(var_name) {
    v <- tolower(trimws(as.character(var_name)))
    p <- sub("\\(.*$", "", v)
    p <- sub(":.*$", "", p)
    trimws(p)
  }

  parse_indices_from_names <- function(var_names, prefix) {
    if (is.null(var_names) || length(var_names) == 0) return(integer(0))
    pat <- paste0("^", prefix, ".*?\\((\\d+)")
    idx <- vapply(var_names, function(x) {
      m <- regexec(pat, tolower(as.character(x)))
      g <- regmatches(tolower(as.character(x)), m)[[1]]
      if (length(g) < 2) return(NA_integer_)
      suppressWarnings(as.integer(g[[2]]))
    }, integer(1))
    idx <- idx[is.finite(idx)]
    sort(unique(as.integer(idx)))
  }

  lookup_param_reference <- function(var_name, var_names = NULL) {
    v <- tolower(trimws(as.character(var_name)))
    prefix <- parse_prefix(v)

    if (grepl("af172|age_flags\\(172\\)", v)) {
      return(list(
        Description = "Profile biomass type selector used by this app (adult biomass vs total biomass context).",
        Source = "MFCL User's Guide profile flag context + shiny_plot profile logic"
      ))
    }
    if (grepl("af173|age_flags\\(173\\)", v)) {
      return(list(
        Description = "Profile period window control: older bound of backward-looking period index.",
        Source = "MFCL User's Guide profile period flags + shiny_plot period labeling"
      ))
    }
    if (grepl("af174|age_flags\\(174\\)", v)) {
      return(list(
        Description = "Profile period window control: recent bound of backward-looking period index.",
        Source = "MFCL User's Guide profile period flags + shiny_plot period labeling"
      ))
    }

    if (prefix %in% c("bs_selcoff_gp", "bs_selcoff", "selcoff", "ageselcoff", "sel_dev_coffs")) {
      return(list(
        Description = "Selectivity coefficient/deviate parameter used for fishery selectivity-at-age (including time-block/season/grouped selectivity variants).",
        Source = "MFCL User's Guide §3.4.2 (selectivity, time-variant selectivity); src/variable.hpp (selcoff, bs_selcoff), src/newmau5a.cpp (bs_selcoff_gp)"
      ))
    }

    if (prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs", "region_rec_diff_coffs")) {
      return(list(
        Description = "Movement diffusion coefficient parameter (including alternative age-dependent and orthogonal-polynomial/robust parameterizations).",
        Source = "MFCL User's Guide §4.5.12 movement + movement notes; src/variable.hpp (diff_coffs*) and src/newmau5a.cpp (diff_coffs, diff_coffs2, diff_coffs3)"
      ))
    }

    if (prefix %in% c("effort_dev_coffs", "grouped_catchability_coffs", "catch_dev_coffs", "q0", "q0_miss", "fm_level_devs")) {
      return(list(
        Description = "Catchability/effort-deviation parameter (baseline q, grouped catchability, catchability deviations, or effective-effort deviate term).",
        Source = "MFCL User's Guide §3.4.1 and §3.4.3; src/variable.hpp (effort_dev_coffs, grouped_catchability_coffs) and src/newmau5a.cpp"
      ))
    }

    if (prefix %in% c("tag_fish_rep", "rep_dev_coffs")) {
      return(list(
        Description = "Tag reporting-rate parameter or its deviation term for tagging likelihood formulations.",
        Source = "MFCL User's Guide §4.5.10 tagging; src/newmau5a.cpp (tag_fish_rep, rep_dev_coffs)"
      ))
    }

    if (prefix %in% c("recr", "totpop", "totpop_coff", "rec_init_diff")) {
      return(list(
        Description = "Recruitment/initial-population scaling parameter in the core population dynamics equations.",
        Source = "MFCL User's Guide §3.1-§3.2 recruitment and initial population; src/newmau5a.cpp"
      ))
    }

    if (prefix %in% c("region_rec_diffs", "region_rec_diff_coffs")) {
      return(list(
        Description = "Regional recruitment-deviation coefficients controlling time-varying recruitment distribution among regions.",
        Source = "MFCL User's Guide §3.2.1 regional recruitment variation; src/newmau5a.cpp (region_rec_diffs) and src/variable.hpp (region_rec_diff_coffs)"
      ))
    }

    if (prefix %in% c("region_pars")) {
      idx <- parse_indices_from_names(var_names, "region_pars")
      if (length(idx) == 1 && idx[[1]] == 1L) {
        return(list(
          Description = "Region-level recruitment distribution proportions (region_pars row 1).",
          Source = "MFCL User's Guide section 'region parameters' and recruitment distribution; src/newmau5a.cpp region_pars(1)"
        ))
      }
      return(list(
        Description = "Region-level recruitment distribution parameters (including priors/constraints on regional recruitment proportions).",
        Source = "MFCL User's Guide §3.2.1 and region flag appendix; src/newmau5a.cpp region_pars(1), src/newmult.cpp region_pars checks"
      ))
    }

    if (prefix %in% c("age_pars")) {
      idx <- parse_indices_from_names(var_names, "age_pars")
      if (length(idx) == 1 && idx[[1]] == 5L) {
        return(list(
          Description = "age_pars(5): natural-mortality-at-age functional-form parameters (including Lorenzen option depending on age flag settings).",
          Source = "MFCL User's Guide age_pars row definitions and Lorenzen notes; src/newmaux5.cpp age_pars(5)"
        ))
      }
      return(list(
        Description = "Age-structured biological parameter block (growth, mortality, recruitment-shape related rows by model flag settings).",
        Source = "MFCL User's Guide biology/recruitment sections; src/newmaux5.cpp age_pars(1..5)"
      ))
    }

    if (prefix %in% c("vb_coff")) {
      return(list(
        Description = "Von Bertalanffy growth coefficients used in age-length conversions and size-based likelihood components.",
        Source = "MFCL User's Guide growth parameterization; src/newmaux5.cpp vb_coff(1..4), src/lbselclc.cpp"
      ))
    }

    if (prefix %in% c("var_coff")) {
      return(list(
        Description = "Size-at-age variability coefficients used in length/weight distribution and selectivity-at-length calculations.",
        Source = "MFCL User's Guide growth/variance settings; src/newmaux5.cpp var_coff(1..2), src/onevar.cpp"
      ))
    }

    if (prefix %in% c("sv")) {
      idx <- parse_indices_from_names(var_names, "sv")
      if (length(idx) == 1 && idx[[1]] == 21L) {
        return(list(
          Description = "Beverton-Holt stock-recruitment beta scaling parameter (density dependence strength). In code: beta = B0 * (sv(21)+0.001).",
          Source = "MFCL source: vrbioclc.cpp, tx.cpp, do_all_for_empirical_autocorrelated_bh.cpp (sv(21))"
        ))
      }
      if (length(idx) == 1 && idx[[1]] == 29L) {
        return(list(
          Description = "Beverton-Holt stock-recruitment steepness (h) parameter.",
          Source = "MFCL source/manual: vrbioclc.cpp (sv(29)=steepness), MULTIFAN-CL-Users-Guide sv(29)"
        ))
      }
      if (all(c(21L, 29L) %in% idx)) {
        return(list(
          Description = "Core Beverton-Holt SRR parameter set: sv(21)=beta scaling (density dependence strength), sv(29)=steepness (h).",
          Source = "MFCL source: vrbioclc.cpp, tx.cpp; manual sv(29)"
        ))
      }
      idx_label <- if (length(idx) == 0) {
        "unknown"
      } else if (length(idx) <= 8) {
        paste(idx, collapse = ", ")
      } else {
        paste0(paste(head(idx, 8), collapse = ", "), ", ...")
      }
      return(list(
        Description = paste0(
          "MFCL structural/process scalar vector parameter (index-specific meaning). ",
          "Observed sv indices in this model: ", idx_label,
          "."
        ),
        Source = "MFCL User's Guide seasonal growth parameters sv(1,30); src/newmaux5.cpp sv(...) assignments + src/callpen.cpp penalties"
      ))
    }

    if (grepl("^fish_pars\\(", v)) {
      idx <- parse_param_index(v, "fish_pars")
      if (isTRUE(idx %in% c(1L, 2L))) {
        return(list(
          Description = "Catchability-related fishery coefficients (fishery-level q structure components activated via fish_flags).",
          Source = "MFCL User's Guide §3.4.1 catchability; src/newmau5a.cpp fish_pars(1:2)"
        ))
      }
      if (isTRUE(idx == 3L)) {
        return(list(
          Description = "Tag reporting-rate parameter (or tag-fish-group reporting-rate term when release-group reporting rates are enabled).",
          Source = "MFCL User's Guide §4.5.10 tagging flags; src/newmau5a.cpp fish_pars(3), tag_fish_rep"
        ))
      }
      if (isTRUE(idx == 4L)) {
        return(list(
          Description = "Tag-likelihood shape/dispersion-type coefficient used in selected tagging likelihood formulations.",
          Source = "MFCL User's Guide §4.5.10 tagging likelihood options; src/newmau5a.cpp fish_pars(4)"
        ))
      }
      if (isTRUE(idx %in% c(5L, 6L))) {
        return(list(
          Description = "Additional fishery likelihood coefficients used with tagging/catch-conditioned likelihood settings.",
          Source = "MFCL User's Guide tagging/catch-conditioned options; src/newmau5a.cpp fish_pars(5:6)"
        ))
      }
      if (isTRUE(idx %in% c(9L, 10L, 11L))) {
        return(list(
          Description = "Fishery selectivity function coefficients (logistic/double-normal/cubic-spline forms depending on fish_flags).",
          Source = "MFCL User's Guide §3.4.2 selectivity; src/newmau5a.cpp fish_pars(9:11) and selectivity routines"
        ))
      }
      if (isTRUE(idx == 7L)) {
        return(list(
          Description = "Catchability effort-effect parameter (effort-dependent catchability hypothesis).",
          Source = "MFCL User's Guide §3.4.1 catchability; src/newmau5a.cpp fish_pars(7)"
        ))
      }
      if (isTRUE(idx == 8L)) {
        return(list(
          Description = "Catchability abundance-effect parameter (biomass-dependent catchability / hyper-stability-hyper-depletion term).",
          Source = "MFCL User's Guide §3.4.1 catchability; src/newmau5a.cpp fish_pars(8)"
        ))
      }
      if (isTRUE(idx %in% c(14L, 15L))) {
        return(list(
          Description = "Self-scaling multinomial log-variance level parameter for length (14) or weight (15) compositions.",
          Source = "MFCL User's Guide composition likelihood options; src/newmau5a.cpp comments: logvN length/weight for self-scaling multinomial"
        ))
      }
      if (isTRUE(idx %in% c(16L, 17L))) {
        return(list(
          Description = "Self-scaling multinomial rho parameter for length (16) or weight (17) compositions.",
          Source = "MFCL User's Guide composition likelihood options; src/newmau5a.cpp comments: rho length/weight for self-scaling multinomial"
        ))
      }
      if (isTRUE(idx %in% c(18L, 19L))) {
        return(list(
          Description = "Self-scaling multinomial log-variance parameter for length (18) or weight (19) compositions.",
          Source = "MFCL User's Guide composition likelihood options; src/newmau5a.cpp comments: log var length/weight for self-scaling multinomial"
        ))
      }
      if (isTRUE(idx %in% c(20L, 21L))) {
        return(list(
          Description = "Sample-size covariate coefficient for length (20) or weight (21) composition likelihood.",
          Source = "MFCL User's Guide composition likelihood options; src/newmau5a.cpp fish_pars(20:21)"
        ))
      }
      if (isTRUE(idx %in% c(22L, 23L))) {
        return(list(
          Description = "Length Dirichlet-multinomial variance multiplier/covariate coefficients.",
          Source = "MFCL User's Guide Dirichlet-multinomial options; src/newmau5a.cpp comments: fish_pars(22:23)"
        ))
      }
      if (isTRUE(idx %in% c(24L, 25L))) {
        return(list(
          Description = "Weight Dirichlet-multinomial variance multiplier/covariate coefficients.",
          Source = "MFCL User's Guide Dirichlet-multinomial options; src/newmau5a.cpp comments: fish_pars(24:25)"
        ))
      }
      if (isTRUE(idx %in% c(26L, 27L))) {
        return(list(
          Description = "Random-effects heterogeneity variance-multiplier coefficient for length (26) or weight (27) compositions.",
          Source = "MFCL source composition parameterization; src/newmau5a.cpp comments: RE length/weight heterogeneity"
        ))
      }
      if (isTRUE(idx == 30L)) {
        return(list(
          Description = "Tag likelihood auxiliary parameter (gamma/censored-gamma related option).",
          Source = "MFCL User's Guide §4.5.10 tagging likelihood variants; src/newmau5a.cpp fish_pars(30)"
        ))
      }
      if (isTRUE(idx %in% c(31L, 32L, 33L))) {
        return(list(
          Description = "Auxiliary fishery likelihood coefficient controlled by advanced fish_flags (used in selected composition/catchability likelihood extensions).",
          Source = "MFCL source code option blocks; src/newmau5a.cpp fish_pars(31:33)"
        ))
      }
      if (isTRUE(idx %in% c(31L, 32L))) {
        return(list(
          Description = "Catch-conditioned variance / overdispersion-related fishery parameters.",
          Source = "MFCL User's Guide catch-conditioned section; src references to fish_pars(31,32)"
        ))
      }
      if (isTRUE(idx %in% c(3L, 4L, 5L, 6L, 30L))) {
        return(list(
          Description = "Tagging/reporting and recapture-related fishery parameters used in tag-likelihood components.",
          Source = "MFCL source tagging routines (e.g., ptagfit.cpp, threaded_tag3.cpp)"
        ))
      }
      return(list(
        Description = "Fishery-level estimated parameter (catchability/selectivity/variance/tagging component depending on fish_flags and likelihood setup).",
        Source = "MFCL User's Guide Ch. 4 fishery parameterization + source code"
      ))
    }

    if (grepl("^bs_selcoff_gp", v)) {
      return(list(
        Description = "Grouped/block selectivity coefficient used by time-varying selectivity parameterization (e.g., block/season/group-specific selectivity terms).",
        Source = "MFCL User's Guide time-variant selectivity sections; source selectivity coefficient structures"
      ))
    }

    if (grepl("^diff_coffs\\(", v) || grepl("^diff_coffs[23]?\\(", v)) {
      return(list(
        Description = "Movement diffusion coefficient parameter (orthogonal-polynomial/alternative movement parameterization term).",
        Source = "MFCL User's Guide movement parameterization; source references to diff_coffs/diff_coffs2/diff_coffs3"
      ))
    }

    if (grepl("^effort_dev_coffs\\(", v) || grepl("^grouped_catchability_coffs\\(", v)) {
      return(list(
        Description = "Effort-deviation / implicit catchability coefficient controlling time-varying fishery catchability dynamics.",
        Source = "MFCL User's Guide catchability deviations; source catchability routines"
      ))
    }

    if (grepl("^selcoff|selectiv", v)) {
      return(list(
        Description = "Selectivity coefficient parameter (age- or length-based selectivity function term depending on fishery settings).",
        Source = "MFCL User's Guide selectivity parameterization; source selectivity routines"
      ))
    }

    if (grepl("^age_pars\\(", v)) {
      idx <- parse_param_index(v, "age_pars")
      if (isTRUE(idx %in% c(1L, 2L))) {
        return(list(
          Description = "Recruitment-related age parameter row (including recruitment mean/deviate structures depending on chosen recruitment parameterization).",
          Source = "MFCL User's Guide recruitment parameterization sections"
        ))
      }
      if (isTRUE(idx == 5L)) {
        return(list(
          Description = "Lorenzen natural-mortality function parameters (size/age-dependent M relationship).",
          Source = "MFCL User's Guide Lorenzen mortality section; src/newmau5a.cpp + natmort routines"
        ))
      }
      return(list(
        Description = "Age-structured biological parameter row (growth, mortality, maturity, or related age process; exact role depends on active flags).",
        Source = "MFCL User's Guide Ch. 4 biology parameterization + source code"
      ))
    }

    if (grepl("^region_pars\\(", v)) {
      idx <- parse_param_index(v, "region_pars")
      if (isTRUE(idx == 1L)) {
        return(list(
          Description = "Regional recruitment composition/proportion-related parameter (used with region-level recruitment structures and priors).",
          Source = "MFCL User's Guide region parameterization (region_pars)"
        ))
      }
      return(list(
        Description = "Region-level parameter (typically regional recruitment/movement structure component).",
        Source = "MFCL User's Guide region and movement parameterization"
      ))
    }

    if (grepl("^species_pars\\(", v)) {
      idx <- parse_param_index(v, "species_pars")
      if (isTRUE(idx == 2L)) {
        return(list(
          Description = "Recruitment autocorrelation-related species parameter (rho) when AR structure is estimated.",
          Source = "MFCL User's Guide recruitment autocorrelation section"
        ))
      }
      return(list(
        Description = "Species-level dynamic parameter (stock-recruitment / species process setting depending on model mode).",
        Source = "MFCL User's Guide species parameterization"
      ))
    }

    if (grepl("^recr|^rec", v)) {
      return(list(
        Description = "Recruitment parameter (mean/deviate or orthogonal-polynomial recruitment form depending on flags).",
        Source = "MFCL User's Guide recruitment parameterization"
      ))
    }

    if (grepl("^q|catchability", v)) {
      return(list(
        Description = "Catchability parameter (average level, temporal deviation, or regression component).",
        Source = "MFCL User's Guide catchability section"
      ))
    }

    if (grepl("^age_flags\\(", v)) {
      return(list(
        Description = "Age-level control flag index (switch/weight controlling biology, penalties, or process options depending on index).",
        Source = "MFCL User's Guide flag settings + source flag logic"
      ))
    }

    if (grepl("^parest_flags\\(", v)) {
      return(list(
        Description = "Parameter-estimation control flag index (minimizer/report/penalty/feature activation controls depending on index).",
        Source = "MFCL User's Guide flag settings + source flag logic"
      ))
    }

    if (grepl("^fish_flags\\(", v)) {
      return(list(
        Description = "Fishery control flag index (catchability/selectivity/grouping/time-variation settings depending on index).",
        Source = "MFCL User's Guide Appendix B fish flags + source flag logic"
      ))
    }

    if (grepl("^region_flags\\(", v)) {
      return(list(
        Description = "Regional control flag index (regional process/penalty settings depending on index).",
        Source = "MFCL User's Guide Appendix B region flags + source flag logic"
      ))
    }

    list(
      Description = paste0(
        "Estimated MFCL parameter from indepvar.rpt (group='", prefix,
        "'). This parameter group is active in the objective-function parameter vector for the selected model."
      ),
      Source = "MFCL indepvar.rpt design + source code parameter vector"
    )
  }

  param_guide_table_reactive <- reactive({
    req(rv$data_loaded)
    filters <- lik_data_filters()
    scenarios <- if (!is.null(filters) && length(filters$scenarios) > 0) filters$scenarios else input$lik_scenarios
    if (is.null(scenarios) || length(scenarios) == 0) return(NULL)

    rows <- lapply(scenarios, function(sc) {
      indep <- rv$IndepOut_list[[sc]]
      df <- parse_indepvar(indep)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$scenario <- sc
      df
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)

    all_params <- bind_rows(rows)
    if (nrow(all_params) == 0) return(NULL)
    all_params$Param_Group <- vapply(all_params$Var_name, parse_prefix, character(1))

    summary_tbl <- all_params %>%
      group_by(Param_Group) %>%
      summarise(
        Family = extract_param_family(first(Param_Group)),
        Models = n_distinct(scenario),
        Entries = dplyr::n(),
        Bound_Hits = sum(Hit_Bound %in% TRUE, na.rm = TRUE),
        Var_Names = list(sort(unique(Var_name))),
        Example_Names = paste(utils::head(sort(unique(Var_name)), 3), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(desc(Bound_Hits), desc(Entries), Param_Group)

    ref_info <- mapply(
      FUN = function(group_name, name_list) lookup_param_reference(group_name, name_list),
      group_name = summary_tbl$Param_Group,
      name_list = summary_tbl$Var_Names,
      SIMPLIFY = FALSE
    )
    summary_tbl$Description <- vapply(ref_info, function(x) x$Description, character(1))
    summary_tbl$Source <- vapply(ref_info, function(x) x$Source, character(1))
    summary_tbl$Var_Names <- NULL

    summary_tbl %>%
      rename(
        `Parameter Group` = Param_Group,
        `Bound Hits` = Bound_Hits,
        `Example Names` = Example_Names
      )
  })

  build_family_catalog <- function(prefix, idx_range, family_label) {
    data.frame(
      Parameter = paste0(prefix, "(", idx_range, ")"),
      Family = family_label,
      stringsAsFactors = FALSE
    )
  }

  master_param_catalog <- local({
    tbl <- bind_rows(
      build_family_catalog("age_pars", 1:220, "Age parameters"),
      build_family_catalog("fish_pars", 1:80, "Fishery parameters"),
      build_family_catalog("region_pars", 1:80, "Region parameters"),
      build_family_catalog("species_pars", 1:80, "Species parameters"),
      build_family_catalog("age_flags", 1:220, "Age flags"),
      build_family_catalog("fish_flags", 1:120, "Fish flags"),
      build_family_catalog("region_flags", 1:80, "Region flags"),
      build_family_catalog("parest_flags", 1:400, "Parest flags")
    )
    ref_info <- lapply(tbl$Parameter, lookup_param_reference)
    tbl$Description <- vapply(ref_info, function(x) x$Description, character(1))
    tbl$Source <- vapply(ref_info, function(x) x$Source, character(1))
    tbl
  })

  master_param_catalog_reactive <- reactive({
    model_tbl <- param_guide_table_reactive()
    out <- master_param_catalog
    out$Param_Group <- vapply(out$Parameter, parse_prefix, character(1))

    if (is.null(model_tbl) || nrow(model_tbl) == 0) {
      out$`In Selected indepvar` <- "No"
      out$`Selected Entries` <- 0L
      out$`Selected Bound Hits` <- 0L
      out$Param_Group <- NULL
      return(out)
    }

    model_map <- model_tbl %>%
      select(`Parameter Group`, Entries, `Bound Hits`) %>%
      distinct(`Parameter Group`, .keep_all = TRUE)

    out <- out %>%
      left_join(model_map, by = c("Param_Group" = "Parameter Group"))

    out$Entries[is.na(out$Entries)] <- 0L
    out$`Bound Hits`[is.na(out$`Bound Hits`)] <- 0L
    out$`In Selected indepvar` <- ifelse(out$Entries > 0, "Yes", "No")
    out$`Selected Entries` <- as.integer(out$Entries)
    out$`Selected Bound Hits` <- as.integer(out$`Bound Hits`)
    out$Entries <- NULL
    out$`Bound Hits` <- NULL
    out$Param_Group <- NULL
    out
  })

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
        scaler_keys <- basename(scaler_dirs[has_payload]) %>% str_extract("\\d+$")
        info_files <- file.path(scaler_dirs[has_payload], "info.rds")
        info_payloads <- setNames(
          lapply(info_files, function(x) if (file.exists(x)) tryCatch(readRDS(x), error = function(e) NULL) else NULL),
          scaler_keys
        )

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
          valid_infos <- info_payloads[!vapply(info_payloads, is.null, logical(1))]
          if (length(af173_vals) == 0 && length(valid_infos) > 0) {
            af173_vals <- suppressWarnings(vapply(valid_infos, function(x) as.numeric(x$AgeFlags["Af173"]), numeric(1)))
          }
          if (length(af174_vals) == 0 && length(valid_infos) > 0) {
            af174_vals <- suppressWarnings(vapply(valid_infos, function(x) as.numeric(x$AgeFlags["Af174"]), numeric(1)))
          }
          if (!is.finite(af172) && length(valid_infos) > 0) {
            af172_fallback <- suppressWarnings(vapply(valid_infos, function(x) as.numeric(x$AgeFlags["Af172"]), numeric(1)))
            af172 <- if (length(af172_fallback) > 0) af172_fallback[1] else NA_real_
          }
          af173 <- if (length(af173_vals) > 0) af173_vals[1] else NA_real_
          af174 <- if (length(af174_vals) > 0) af174_vals[1] else NA_real_
          quantity_label_vals <- unlist(map(payloads, "quantity_label"), use.names = FALSE)
          quantity_label_vals <- quantity_label_vals[nzchar(quantity_label_vals)]
          if (length(quantity_label_vals) == 0 && length(valid_infos) > 0) {
            quantity_label_vals <- unlist(map(valid_infos, "quantity_label"), use.names = FALSE)
            quantity_label_vals <- quantity_label_vals[nzchar(quantity_label_vals)]
          }
          quantity_label <- if (length(quantity_label_vals) > 0) quantity_label_vals[1] else NA_character_
          par_obj <- rv$ParOut_list[[scenario]]
          max_year <- suppressWarnings(as.numeric(tryCatch(par_obj@range["maxyear"], error = function(e) NA_real_)))
          seasons <- suppressWarnings(as.numeric(tryCatch(par_obj@dimensions["seasons"], error = function(e) NA_real_)))

          hessian_rows <- lapply(existing_scales, function(sc) {
            info_sc <- valid_infos[[as.character(sc)]]
            hs <- if (!is.null(info_sc) && is.list(info_sc$hessian)) info_sc$hessian else NULL
            attempted <- isTRUE(hs$attempted)
            requested <- isTRUE(hs$requested)
            ok <- if (!is.null(hs$hessian_ok) && !is.na(hs$hessian_ok)) isTRUE(hs$hessian_ok) else if (!is.null(hs$is_pdh) && !is.na(hs$is_pdh)) isTRUE(hs$is_pdh) else NA
            status <- if (!is.null(hs$hessian_status) && nzchar(as.character(hs$hessian_status))) as.character(hs$hessian_status) else NA_character_
            neg_eigen <- if (!is.null(hs$n_negative_eigenvalues) && !is.null(hs$n_total_eigenvalues) &&
              is.finite(suppressWarnings(as.numeric(hs$n_negative_eigenvalues))) &&
              is.finite(suppressWarnings(as.numeric(hs$n_total_eigenvalues)))) {
              sprintf("%d / %d", as.integer(hs$n_negative_eigenvalues), as.integer(hs$n_total_eigenvalues))
            } else {
              NA_character_
            }
            data.frame(
              scaler = as.character(sc),
              requested = requested,
              attempted = attempted,
              ok = ok,
              status = status,
              neg_eigen = neg_eigen,
              stringsAsFactors = FALSE
            )
          })
          hessian_df <- bind_rows(hessian_rows)
          if (nrow(hessian_df) > 0) {
            hessian_ok_by_scaler <- setNames(as.list(hessian_df$ok), hessian_df$scaler)
            hessian_status_by_scaler <- setNames(as.list(hessian_df$status), hessian_df$scaler)
            hessian_requested_by_scaler <- setNames(as.list(hessian_df$requested), hessian_df$scaler)
            hessian_attempted_by_scaler <- setNames(as.list(hessian_df$attempted), hessian_df$scaler)
            hessian_neg_by_scaler <- setNames(as.list(hessian_df$neg_eigen), hessian_df$scaler)
          } else {
            hessian_ok_by_scaler <- list()
            hessian_status_by_scaler <- list()
            hessian_requested_by_scaler <- list()
            hessian_attempted_by_scaler <- list()
            hessian_neg_by_scaler <- list()
          }
          hessian_df <- hessian_df[!is.na(hessian_df$requested) | !is.na(hessian_df$attempted) | !is.na(hessian_df$ok), , drop = FALSE]
          profile_hessian_attempted <- if (nrow(hessian_df) > 0) sum(hessian_df$attempted, na.rm = TRUE) else 0L
          profile_hessian_ok <- if (nrow(hessian_df) > 0) sum(hessian_df$attempted & hessian_df$ok %in% TRUE, na.rm = TRUE) else 0L
          profile_hessian_requested <- if (nrow(hessian_df) > 0) sum(hessian_df$requested, na.rm = TRUE) else 0L
          profile_hessian_status <- if (nrow(hessian_df) > 0) {
            vals <- unique(na.omit(as.character(hessian_df$status)))
            if (length(vals) > 0) paste(vals, collapse = ", ") else NA_character_
          } else {
            NA_character_
          }
          profile_hessian_summary <- if (profile_hessian_attempted > 0) {
            paste0(profile_hessian_ok, "/", profile_hessian_attempted, " OK")
          } else if (profile_hessian_requested > 0) {
            "Requested (not completed)"
          } else {
            "Not requested"
          }
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
          profile_hessian_attempted <- 0L
          profile_hessian_ok <- 0L
          profile_hessian_requested <- 0L
          profile_hessian_status <- NA_character_
          profile_hessian_summary <- "Not requested"
          hessian_ok_by_scaler <- list()
          hessian_status_by_scaler <- list()
          hessian_requested_by_scaler <- list()
          hessian_attempted_by_scaler <- list()
          hessian_neg_by_scaler <- list()
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
        profile_hessian_attempted <- 0L
        profile_hessian_ok <- 0L
        profile_hessian_requested <- 0L
        profile_hessian_status <- NA_character_
        profile_hessian_summary <- "Not requested"
        hessian_ok_by_scaler <- list()
        hessian_status_by_scaler <- list()
        hessian_requested_by_scaler <- list()
        hessian_attempted_by_scaler <- list()
        hessian_neg_by_scaler <- list()
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
      profile_hessian_attempted <- 0L
      profile_hessian_ok <- 0L
      profile_hessian_requested <- 0L
      profile_hessian_status <- NA_character_
      profile_hessian_summary <- "Not requested"
      hessian_ok_by_scaler <- list()
      hessian_status_by_scaler <- list()
      hessian_requested_by_scaler <- list()
      hessian_attempted_by_scaler <- list()
      hessian_neg_by_scaler <- list()
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
      seasons = seasons,
      profile_hessian_attempted = profile_hessian_attempted,
      profile_hessian_ok = profile_hessian_ok,
      profile_hessian_requested = profile_hessian_requested,
      profile_hessian_status = profile_hessian_status,
      profile_hessian_summary = profile_hessian_summary,
      profile_hessian_ok_by_scaler = hessian_ok_by_scaler,
      profile_hessian_status_by_scaler = hessian_status_by_scaler,
      profile_hessian_requested_by_scaler = hessian_requested_by_scaler,
      profile_hessian_attempted_by_scaler = hessian_attempted_by_scaler,
      profile_hessian_neg_by_scaler = hessian_neg_by_scaler
    )
  }

  profile_outputs_reactive <- reactive({
    filters <- lik_profile_output_filters()
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
    lik_profile_output_filters()
  )

  profile_target_info_reactive <- reactive({
    filters <- lik_data_filters()
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
    lik_data_filters()
  )

  build_jitter_seed_status_tables <- function(scenarios, cutoff = 0.001) {
    if (length(scenarios) == 0) return(list(summary = NULL, seeds = NULL))

    scalar_chr <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      out <- as.character(x[[1]])
      if (is.na(out) || !nzchar(out)) return(NA_character_)
      out
    }

    scalar_num <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_real_)
      out <- suppressWarnings(as.numeric(x[[1]]))
      if (length(out) == 0 || !is.finite(out)) return(NA_real_)
      out
    }
    scalar_lgl <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA)
      out <- suppressWarnings(as.logical(x[[1]]))
      if (length(out) == 0 || is.na(out)) return(NA)
      isTRUE(out)
    }

    extract_hessian_fields <- function(jit, info) {
      hs_jit <- if (is.list(jit) && is.list(jit$hessian_info)) jit$hessian_info else NULL
      hs_info <- if (is.list(info) && is.list(info$hessian)) info$hessian else NULL
      hs_raw <- if (is.list(jit) && is.list(jit$hessian)) jit$hessian else NULL

      requested <- NA
      attempted <- NA
      status <- NA_character_
      neg_eigen <- NA_character_
      neg_from <- function(x) {
        if (!is.null(x) && is.list(x) &&
          !is.null(x$n_negative_eigenvalues) && !is.null(x$n_total_eigenvalues) &&
          is.finite(suppressWarnings(as.numeric(x$n_negative_eigenvalues))) &&
          is.finite(suppressWarnings(as.numeric(x$n_total_eigenvalues)))) {
          return(sprintf("%d / %d", as.integer(x$n_negative_eigenvalues), as.integer(x$n_total_eigenvalues)))
        }
        NA_character_
      }

      if (!is.null(hs_jit)) {
        requested <- scalar_lgl(hs_jit$requested)
        attempted <- scalar_lgl(hs_jit$attempted)
        status <- scalar_chr(hs_jit$hessian_status)
        neg_eigen <- neg_from(hs_jit)
      }
      if (!is.null(hs_info)) {
        if (is.na(requested)) requested <- scalar_lgl(hs_info$requested)
        if (is.na(attempted)) attempted <- scalar_lgl(hs_info$attempted)
        if (is.na(status)) status <- scalar_chr(hs_info$hessian_status)
        if (is.na(neg_eigen)) neg_eigen <- neg_from(hs_info)
      }
      if (!is.null(hs_raw)) {
        if (is.na(requested)) requested <- scalar_lgl(hs_raw$requested)
        if (is.na(attempted)) attempted <- scalar_lgl(hs_raw$attempted)
        if (is.na(status)) status <- scalar_chr(hs_raw$hessian_status)
        if (is.na(neg_eigen)) neg_eigen <- neg_from(hs_raw)
      }

      if (!is.list(jit)) return(NA)
      hessian_ok <- if (!is.null(jit$hessian_ok)) {
        scalar_lgl(jit$hessian_ok)
      } else if (!is.null(hs_jit) && !is.null(hs_jit$pdh)) {
        scalar_lgl(hs_jit$pdh)
      } else if (!is.null(hs_info) && !is.null(hs_info$hessian_ok)) {
        scalar_lgl(hs_info$hessian_ok)
      } else if (!is.null(hs_info) && !is.null(hs_info$is_pdh)) {
        scalar_lgl(hs_info$is_pdh)
      } else if (!is.null(hs_raw) && !is.null(hs_raw$ok)) {
        scalar_lgl(hs_raw$ok)
      } else {
        NA
      }

      list(
        requested = requested,
        attempted = attempted,
        ok = hessian_ok,
        status = status,
        neg_eigen = neg_eigen
      )
    }

    seed_rows <- lapply(scenarios, function(sc) {
      jitter_payloads <- rv$JitterPars_list[[sc]]
      jitter_infos <- rv$JitterInfos_list[[sc]]

      seed_ids <- sort(unique(c(names(jitter_payloads), names(jitter_infos))))
      if (length(seed_ids) == 0) return(NULL)

      bind_rows(lapply(seed_ids, function(seed_id) {
        jit <- jitter_payloads[[seed_id]]
        info <- jitter_infos[[seed_id]]

        run_completed <- isTRUE(jit$run_completed)
        run_status_jit <- scalar_chr(jit$run_status)
        run_status_info <- scalar_chr(info$mfcl_run$run_status)
        run_status <- if (!is.na(run_status_jit)) {
          run_status_jit
        } else if (!is.na(run_status_info)) {
          run_status_info
        } else {
          "unknown"
        }

        obj_fun <- scalar_num(jit$obj_fun)
        max_grad <- scalar_num(jit$max_grad)
        jitter_cv <- scalar_num(info$jitter_cv)

        hessian <- extract_hessian_fields(jit, info)
        converged_for_cutoff <- isTRUE(run_completed) &&
          isTRUE(is.finite(max_grad)) &&
          isTRUE(abs(max_grad) <= cutoff)

        data.frame(
          Model = sc,
          Seed = suppressWarnings(as.integer(seed_id)),
          `Jitter CV` = jitter_cv,
          `Run status` = run_status,
          Completed = run_completed,
          `Objective Function` = obj_fun,
          `Max Gradient` = max_grad,
          `Hessian.Requested` = if (is.na(hessian$requested)) NA else isTRUE(hessian$requested),
          `Hessian.Attempted` = if (is.na(hessian$attempted)) NA else isTRUE(hessian$attempted),
          `Hessian.Status` = if (is.na(hessian$status)) NA_character_ else hessian$status,
          `Neg..Eigen` = if (is.na(hessian$neg_eigen)) NA_character_ else hessian$neg_eigen,
          Converged = converged_for_cutoff,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }))
    })

    seeds_df <- bind_rows(seed_rows)
    if (nrow(seeds_df) == 0) return(list(summary = NULL, seeds = NULL))

    summary_df <- seeds_df %>%
      group_by(Model) %>%
      summarise(
        `Jitter CV` = {
          cv_vals <- sort(unique(.data[["Jitter CV"]][is.finite(.data[["Jitter CV"]])]))
          if (length(cv_vals) == 0) {
            NA_character_
          } else if (length(cv_vals) == 1) {
            format(cv_vals[[1]], trim = TRUE)
          } else {
            paste0(format(min(cv_vals), trim = TRUE), " to ", format(max(cv_vals), trim = TRUE))
          }
        },
        `Seeds loaded` = dplyr::n(),
        Completed = sum(Completed, na.rm = TRUE),
        Failed = sum(.data[["Run status"]] == "failed", na.rm = TRUE),
        Incomplete = sum(.data[["Run status"]] == "incomplete", na.rm = TRUE),
        Unknown = sum(.data[["Run status"]] == "unknown", na.rm = TRUE),
        `Converged seeds` = sum(Converged, na.rm = TRUE),
        `Converged / Total` = paste0(sum(Converged, na.rm = TRUE), " / ", dplyr::n()),
        `Hessian.Requested` = {
          vals <- .data[["Hessian.Requested"]]
          if (sum(!is.na(vals)) == 0) {
            NA_character_
          } else {
            paste0(sum(vals %in% TRUE, na.rm = TRUE), " / ", sum(!is.na(vals)))
          }
        },
        `Hessian.Attempted` = {
          vals <- .data[["Hessian.Attempted"]]
          if (sum(!is.na(vals)) == 0) {
            NA_character_
          } else {
            paste0(sum(vals %in% TRUE, na.rm = TRUE), " / ", sum(!is.na(vals)))
          }
        },
        `Hessian.Status` = {
          vals <- unique(na.omit(as.character(.data[["Hessian.Status"]])))
          if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
        },
        `Neg..Eigen` = {
          vals <- unique(na.omit(as.character(.data[["Neg..Eigen"]])))
          if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
        },
        `Median |max_grad| (completed)` = {
          completed_grads <- abs(.data[["Max Gradient"]][Completed & is.finite(.data[["Max Gradient"]])])
          if (length(completed_grads) > 0) stats::median(completed_grads, na.rm = TRUE) else NA_real_
        },
        `Best |max_grad| (completed)` = {
          completed_grads <- abs(.data[["Max Gradient"]][Completed & is.finite(.data[["Max Gradient"]])])
          if (length(completed_grads) > 0) min(completed_grads, na.rm = TRUE) else NA_real_
        },
        .groups = "drop"
      )

    seeds_df <- seeds_df %>%
      arrange(Model, Seed)

    list(summary = summary_df, seeds = seeds_df)
  }

  format_jitter_convergence_counts <- function(seed_summary_df) {
    if (is.null(seed_summary_df) || nrow(seed_summary_df) == 0) return(NULL)
    paste(
      paste0(seed_summary_df$Model, " ", seed_summary_df$`Converged / Total`),
      collapse = " | "
    )
  }

  jitter_info_reactive <- reactive({
    filters <- lik_filters_current()
    req(filters)

    type <- filters$profile_type
    if (!(type %in% c("jitter", "jitter_params", "jitter_derived"))) return(NULL)

    scenarios <- filters$scenarios
    if (length(scenarios) == 0) return(NULL)

    cutoff <- if (identical(type, "jitter")) {
      if (is.finite(filters$jitter_grad_reference)) filters$jitter_grad_reference else 0.001
    } else {
      if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.001
    }

    build_jitter_seed_status_tables(scenarios = scenarios, cutoff = cutoff)
  })
  jitter_info_reactive <- bindCache(
    jitter_info_reactive,
    input$model_dir,
    list(
      scenarios = sort(input$lik_scenarios),
      profile_type = current_profile_type(),
      grad_reference = input$lik_jitter_grad_reference,
      converged_max_grad = input$lik_jitter_converged_max_grad
    )
  )

  profile_gradient_table_reactive <- reactive({
    filters <- lik_data_filters()
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
        h_status <- if (!is.null(pd$profile_hessian_status_by_scaler)) pd$profile_hessian_status_by_scaler[[as.character(scl)]] else NA
        h_neg <- if (!is.null(pd$profile_hessian_neg_by_scaler)) pd$profile_hessian_neg_by_scaler[[as.character(scl)]] else NA
        h_requested <- if (!is.null(pd$profile_hessian_requested_by_scaler)) pd$profile_hessian_requested_by_scaler[[as.character(scl)]] else FALSE
        h_attempted <- if (!is.null(pd$profile_hessian_attempted_by_scaler)) pd$profile_hessian_attempted_by_scaler[[as.character(scl)]] else FALSE
        rows[[length(rows) + 1]] <- data.frame(
          scenario = sc,
          scaler = suppressWarnings(as.numeric(scl)),
          actual_quantity = suppressWarnings(as.numeric(actual_quantity)),
          target_quantity = suppressWarnings(as.numeric(target_quantity)),
          target_rel_err = suppressWarnings(as.numeric(target_rel_err)),
          obj_fun = suppressWarnings(as.numeric(obj_fun)),
          max_grad = suppressWarnings(as.numeric(grad_val)),
          hessian_requested = isTRUE(h_requested),
          hessian_attempted = isTRUE(h_attempted),
          hessian_status = if (is.null(h_status) || !nzchar(as.character(h_status))) NA_character_ else as.character(h_status),
          hessian_neg = if (is.null(h_neg) || !nzchar(as.character(h_neg))) NA_character_ else as.character(h_neg),
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
        `Max Gradient` = max_grad,
        `Hessian.Requested` = hessian_requested,
        `Hessian.Attempted` = hessian_attempted,
        `Hessian.Status` = hessian_status,
        `Neg..Eigen` = hessian_neg
      ) %>%
      arrange(Model, Scaler)
  })
  profile_gradient_table_reactive <- bindCache(
    profile_gradient_table_reactive,
    input$model_dir,
    lik_data_filters()
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

    if (isTRUE(split_by_region) && "region" %in% names(data)) {
      valid_regions <- data %>%
        group_by(scenario, region) %>%
        summarise(
          has_non_total = any(.data[[group_var]] != "Total"),
          has_signal = any(.data[[group_var]] != "Total" & is.finite(change) & abs(change) > 1e-10),
          .groups = "drop"
        ) %>%
        filter(has_non_total, has_signal) %>%
        select(scenario, region)

      data <- data %>%
        inner_join(valid_regions, by = c("scenario", "region"))

      if (nrow(data) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No region panels with fishery lines after filtering", size = 6, color = "#999") +
            theme_void()
        )
      }
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
      aes(x = scaler, y = change, colour = .data[[group_var]])
    ) +
      geom_line(aes(linewidth = .data[[group_var]] == "Total"), alpha = 0.7) +
      geom_point(aes(size = .data[[group_var]] == "Total"), alpha = 0.8, shape = 16) +
      scale_color_manual(values = color_values, breaks = legend_breaks) +
      scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = 0.7), guide = "none") +
      scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2), guide = "none") +
      scale_x_continuous(
        labels = function(x) x / 1000,
        name = x_label
      ) +
      labs(y = "Changes in Likelihood", colour = NULL) +
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
        )
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

    get_fishery_rows_for_scenario <- function(sc) {
      allowed_ids <- if (!is.null(allowed_fisheries)) as.character(allowed_fisheries[[sc]]) else character(0)
      allowed_ids <- allowed_ids[!is.na(allowed_ids)]
      cache_key <- paste(sc, slot_name, as.character(isTRUE(fallback_nonzero_only)), paste(sort(unique(allowed_ids)), collapse = "|"), sep = "::")
      cached <- fishery_diag_cache$fishery_slot[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      rows <- list()
      fish_lookup <- get_fishery_lookup(sc, fishery_maps[[sc]])
      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        vec <- slot(lik, slot_name)
        fish_ids <- as.character(seq_along(vec))
        keep_idx <- rep(TRUE, length(fish_ids))
        if (length(allowed_ids) > 0) {
          keep_idx <- fish_ids %in% allowed_ids
        } else if (isTRUE(fallback_nonzero_only)) {
          keep_idx <- is.finite(vec) & abs(vec) > 0
        }

        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_names <- unname(fish_lookup$names[fish_ids])
        fish_regions <- unname(fish_lookup$regions[fish_ids])
        fish_names[is.na(fish_names) | !nzchar(fish_names)] <- fish_ids[is.na(fish_names) | !nzchar(fish_names)]
        fish_regions[is.na(fish_regions) | !nzchar(fish_regions)] <- "Unknown"

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
      fishery_diag_cache$fishery_slot[[cache_key]] <- bind_rows(rows)
      fishery_diag_cache$fishery_slot[[cache_key]]
    }

    data <- bind_rows(lapply(scenarios, get_fishery_rows_for_scenario))
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

  get_fishery_lookup <- function(scenario, fish_map) {
    cached <- fishery_diag_cache$fishery_lookup[[scenario]]
    if (!is.null(cached)) {
      return(cached)
    }

    if (is.null(fish_map) || !"fishery" %in% names(fish_map)) {
      lookup <- list(
        names = setNames(character(0), character(0)),
        regions = setNames(character(0), character(0))
      )
      fishery_diag_cache$fishery_lookup[[scenario]] <- lookup
      return(lookup)
    }

    fish_ids <- as.character(fish_map$fishery)
    fish_names <- setNames(
      vapply(fish_ids, function(fid) get_fishery_name(fid, fish_map), character(1)),
      fish_ids
    )
    fish_regions <- setNames(
      vapply(fish_ids, function(fid) get_fishery_region(fid, fish_map), character(1)),
      fish_ids
    )

    lookup <- list(names = fish_names, regions = fish_regions)
    fishery_diag_cache$fishery_lookup[[scenario]] <- lookup
    lookup
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

    get_cpue_rows_for_scenario <- function(sc) {
      allowed_ids <- if (!is.null(allowed_fisheries)) as.character(allowed_fisheries[[sc]]) else character(0)
      allowed_ids <- allowed_ids[!is.na(allowed_ids)]
      cache_key <- paste(sc, paste(sort(unique(allowed_ids)), collapse = "|"), sep = "::")
      cached <- fishery_diag_cache$cpue[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      rows <- list()
      fish_map <- fishery_maps[[sc]]
      fish_lookup <- get_fishery_lookup(sc, fish_map)

      for (scl in scales) {
        raw <- profile_data[[sc]]$lik_raw[[scl]]
        vec <- extract_survey_index_like_from_raw(raw)
        if (length(vec) == 0) next
        scaler_bio <- scaler_quantity(profile_data[[sc]], scl)
        if (!is.finite(scaler_bio)) next

        fish_ids <- as.character(seq_along(vec))
        keep_idx <- is.finite(vec) & abs(vec) > 0
        if (length(allowed_ids) > 0) {
          keep_idx <- keep_idx & (fish_ids %in% allowed_ids)
        }
        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_names <- unname(fish_lookup$names[fish_ids])
        fish_regions <- unname(fish_lookup$regions[fish_ids])
        fish_names[is.na(fish_names) | !nzchar(fish_names)] <- fish_ids[is.na(fish_names) | !nzchar(fish_names)]
        fish_regions[is.na(fish_regions) | !nzchar(fish_regions)] <- "Unknown"

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

      fishery_diag_cache$cpue[[cache_key]] <- bind_rows(rows)
      fishery_diag_cache$cpue[[cache_key]]
    }

    bind_rows(lapply(scenarios, get_cpue_rows_for_scenario))
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

    get_cal_rows_for_scenario <- function(sc) {
      allowed_ids <- if (!is.null(allowed_fisheries)) as.character(allowed_fisheries[[sc]]) else character(0)
      allowed_ids <- allowed_ids[!is.na(allowed_ids)]
      cache_key <- paste(sc, by, paste(sort(unique(allowed_ids)), collapse = "|"), sep = "::")
      cached <- fishery_diag_cache$cal[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      alk_summary <- fishery_diag_cache$alk_summary[[sc]]
      if (is.null(alk_summary)) {
        alk_summary <- get_alk_summary(age_out_list[[sc]])
        fishery_diag_cache$alk_summary[[sc]] <- alk_summary
      }
      if (is.null(alk_summary) || nrow(alk_summary) == 0) {
        fishery_diag_cache$cal[[cache_key]] <- data.frame()
        return(fishery_diag_cache$cal[[cache_key]])
      }

      rows <- list()
      fish_lookup <- get_fishery_lookup(sc, fishery_maps[[sc]])

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
          if (length(allowed_ids) > 0) {
            df <- df[as.character(df$fishery) %in% allowed_ids, , drop = FALSE]
          }
          if (nrow(df) == 0) next

          fish_ids <- as.character(df$fishery)
          df$fishery <- unname(fish_lookup$names[fish_ids])
          df$region <- unname(fish_lookup$regions[fish_ids])
          df$fishery[is.na(df$fishery) | !nzchar(df$fishery)] <- fish_ids[is.na(df$fishery) | !nzchar(df$fishery)]
          df$region[is.na(df$region) | !nzchar(df$region)] <- "Unknown"

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

      fishery_diag_cache$cal[[cache_key]] <- bind_rows(rows)
      fishery_diag_cache$cal[[cache_key]]
    }

    bind_rows(lapply(scenarios, get_cal_rows_for_scenario))
  }

  build_jitter_data <- function(scenarios, par_out_list, jitter_pars_list,
                                converged_only = FALSE, converged_max_grad = 0.001) {
    get_jitter_diag_rows <- function(sc) {
      cache_key <- paste(sc, as.character(isTRUE(converged_only)), format(converged_max_grad, trim = TRUE), sep = "::")
      cached <- jitter_data_cache$diagnostics[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      ref_par <- par_out_list[[sc]]
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(ref_par) || is.null(jit_list) || length(jit_list) == 0) {
        jitter_data_cache$diagnostics[[cache_key]] <- data.frame()
        return(jitter_data_cache$diagnostics[[cache_key]])
      }

      ref_obj <- suppressWarnings(as.numeric(ref_par@obj_fun))
      ref_grad <- suppressWarnings(as.numeric(ref_par@max_grad))
      if (!is.finite(ref_obj)) {
        jitter_data_cache$diagnostics[[cache_key]] <- data.frame()
        return(jitter_data_cache$diagnostics[[cache_key]])
      }

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
      if (isTRUE(converged_only)) {
        keep <- keep & abs(grad_vals) <= converged_max_grad
      }
      if (!any(keep)) {
        jitter_data_cache$diagnostics[[cache_key]] <- data.frame()
        return(jitter_data_cache$diagnostics[[cache_key]])
      }

      obj_vals <- obj_vals[keep]
      grad_vals <- grad_vals[keep]
      seeds <- seeds[keep]
      pct_diff <- ((obj_vals - ref_obj) / abs(ref_obj)) * 100

      jitter_data_cache$diagnostics[[cache_key]] <- data.frame(
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
      jitter_data_cache$diagnostics[[cache_key]]
    }

    bind_rows(lapply(scenarios, get_jitter_diag_rows))
  }

  build_jitter_parameter_data <- function(scenarios, jitter_pars_list, view = "input", converged_only = FALSE,
                                          converged_max_grad = 0.01) {
    get_jitter_parameter_rows <- function(sc) {
      cache_key <- paste(
        "v3",
        sc,
        view,
        as.character(isTRUE(converged_only)),
        format(converged_max_grad, trim = TRUE),
        sep = "::"
      )
      cached <- jitter_param_cache$rows[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      rows <- list()
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(jit_list) || length(jit_list) == 0) {
        jitter_param_cache$rows[[cache_key]] <- data.frame()
        return(jitter_param_cache$rows[[cache_key]])
      }

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      for (i in seq_along(jit_list)) {
        jit <- jit_list[[i]]
        if (identical(view, "final")) {
          if (!isTRUE(jit$run_completed)) next
          if (isTRUE(converged_only)) {
            jit_max_grad <- suppressWarnings(as.numeric(jit$max_grad))
            if (!is.finite(jit_max_grad) || abs(jit_max_grad) > converged_max_grad) next
          }
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
            family = ifelse(is.na(family) | !nzchar(family), "unclassified", family)
          ) %>%
          mutate(
            pct_change = dplyr::case_when(
              is.finite(pct_change) ~ pct_change,
              is.finite(before) & is.finite(after) & before != 0 ~
                100 * (after - before) / before,
              TRUE ~ NA_real_
            ),
            abs_pct_change = abs(pct_change)
          ) %>%
          filter(is.finite(before), is.finite(after))

        if (nrow(df) == 0) next
        rows[[length(rows) + 1]] <- df
      }

      jitter_param_cache$rows[[cache_key]] <- bind_rows(rows)
      jitter_param_cache$rows[[cache_key]]
    }

    all_params <- bind_rows(lapply(scenarios, get_jitter_parameter_rows))
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

    extract_yearly_sum <- function(slot_obj, scale = 1) {
      slot_df <- tryCatch(safe_array_to_df(slot_obj), error = function(e) NULL)
      if (is.null(slot_df) || nrow(slot_df) == 0) return(NULL)
      slot_df$year <- suppressWarnings(as.numeric(slot_df$year))
      slot_df$data <- suppressWarnings(as.numeric(slot_df$data))
      slot_df <- slot_df[is.finite(slot_df$year) & is.finite(slot_df$data), , drop = FALSE]
      if (nrow(slot_df) == 0) return(NULL)
      out <- stats::aggregate(data ~ year, data = slot_df, FUN = sum)
      out$data <- out$data / scale
      out
    }

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

    rec_df <- extract_yearly_sum(tryCatch(rep_obj@rec_region, error = function(e) NULL), scale = 1e6)
    if (is.null(rec_df) || nrow(rec_df) == 0) {
      rec_df <- extract_yearly_sum(tryCatch(rep_obj@eq_rec, error = function(e) NULL), scale = 1e6)
    }
    if (is.null(rec_df) || nrow(rec_df) == 0) {
      rec_df <- extract_yearly_sum(tryCatch(rep_obj@rec, error = function(e) NULL), scale = 1)
    }
    if (!is.null(rec_df) && nrow(rec_df) > 0) {
      names(rec_df)[names(rec_df) == "data"] <- "recruitment"
      merged <- merge(merged, rec_df, by = "year", all = TRUE)
    }

    fm_df <- tryCatch(safe_array_to_df(rep_obj@fm), error = function(e) NULL)
    popn_df <- tryCatch(safe_array_to_df(rep_obj@popN), error = function(e) NULL)
    used_fm <- FALSE
    if (!is.null(fm_df) && !is.null(popn_df) && nrow(fm_df) > 0 && nrow(popn_df) > 0) {
      fm_df$data <- suppressWarnings(as.numeric(fm_df$data))
      popn_df$data <- suppressWarnings(as.numeric(popn_df$data))
      popn_df$N <- popn_df$data
      popn_df$data <- NULL
      numeric_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), union(names(fm_df), names(popn_df)))
      for (col in numeric_cols) {
        if (col %in% names(fm_df)) fm_df[[col]] <- suppressWarnings(as.numeric(fm_df[[col]]))
        if (col %in% names(popn_df)) popn_df[[col]] <- suppressWarnings(as.numeric(popn_df[[col]]))
      }
      join_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), intersect(names(fm_df), names(popn_df)))
      if (all(c("year", "season") %in% join_cols)) {
        fm_popn <- merge(fm_df, popn_df, by = join_cols, all = FALSE)
        fm_popn <- fm_popn[is.finite(fm_popn$year) & is.finite(fm_popn$season) & is.finite(fm_popn$data) & is.finite(fm_popn$N), , drop = FALSE]
        if (nrow(fm_popn) > 0) {
          fm_popn$catch <- fm_popn$data * fm_popn$N
          yearly <- stats::aggregate(cbind(total_catch = catch, total_N = N) ~ year + season, data = fm_popn, FUN = sum)
          if (nrow(yearly) > 0) {
            yearly$harvest_rate <- yearly$total_catch / pmax(yearly$total_N, .Machine$double.eps)
            yearly$inst_F <- -log(pmax(1 - yearly$harvest_rate, 0.001))
            fm_year <- stats::aggregate(inst_F ~ year, data = yearly, FUN = sum)
            names(fm_year)[names(fm_year) == "inst_F"] <- "fishing_mortality"
            merged <- merge(merged, fm_year, by = "year", all = TRUE)
            used_fm <- TRUE
          }
        }
      }
    }
    if (!used_fm) {
      fmlevel_df <- extract_yearly_sum(tryCatch(rep_obj@fmlevel, error = function(e) NULL), scale = 1)
      if (!is.null(fmlevel_df) && nrow(fmlevel_df) > 0) {
        fmlevel_df <- stats::aggregate(data ~ year, data = fmlevel_df, FUN = mean)
        names(fmlevel_df)[names(fmlevel_df) == "data"] <- "fishing_mortality"
        merged <- merge(merged, fmlevel_df, by = "year", all = TRUE)
      }
    }

    if (nrow(merged) == 0) return(NULL)
    merged %>%
      mutate(scenario = scenario)
  }

  build_jitter_derived_data <- function(scenarios, rep_out_list, jitter_pars_list, converged_only = FALSE,
                                        converged_max_grad = 0.01) {
    extract_rep_age_curves <- function(rep_obj) {
      if (is.null(rep_obj)) return(NULL)
      m_vals <- tryCatch(suppressWarnings(as.numeric(m_at_age(rep_obj))), error = function(e) numeric(0))
      laa_vals <- tryCatch(suppressWarnings(as.numeric(c(aperm(mean_laa(rep_obj), c(4, 1, 2, 3, 5, 6))))), error = function(e) numeric(0))
      if (length(m_vals) == 0 && length(laa_vals) == 0) return(NULL)
      n_age <- max(length(m_vals), length(laa_vals))
      data.frame(
        age = seq_len(n_age),
        natural_mortality = if (length(m_vals) > 0) m_vals[seq_len(n_age)] else rep(NA_real_, n_age),
        growth = if (length(laa_vals) > 0) laa_vals[seq_len(n_age)] else rep(NA_real_, n_age),
        stringsAsFactors = FALSE
      )
    }

    extract_jitter_age_curves <- function(jit, age_cache) {
      # Prefer payload-embedded age curves so jitter_result.rds-only folders render correctly.
      if (is.list(jit) && !is.null(jit$age_curves) && is.data.frame(jit$age_curves) && nrow(jit$age_curves) > 0) {
        ac <- jit$age_curves
        if (!"age" %in% names(ac) && "year" %in% names(ac)) ac$age <- ac$year
        if (!"natural_mortality" %in% names(ac)) ac$natural_mortality <- NA_real_
        if (!"growth" %in% names(ac)) ac$growth <- NA_real_
        return(ac %>%
                 transmute(
                   age = suppressWarnings(as.numeric(age)),
                   natural_mortality = suppressWarnings(as.numeric(natural_mortality)),
                   growth = suppressWarnings(as.numeric(growth))
                 ) %>%
                 filter(is.finite(age)))
      }

      seed_dir <- if (is.list(jit) && !is.null(jit$seed_dir)) as.character(jit$seed_dir[[1]]) else ""
      if (!nzchar(seed_dir)) return(NULL)
      if (exists(seed_dir, envir = age_cache, inherits = FALSE)) {
        return(get(seed_dir, envir = age_cache, inherits = FALSE))
      }

      rep_path <- tryCatch(finalRep(seed_dir), error = function(e) NULL)
      if (is.null(rep_path) || !file.exists(rep_path)) {
        rep_path <- tryCatch({
          cands <- list.files(seed_dir, pattern = "\\.rep$", full.names = TRUE)
          if (length(cands) > 0) cands[[1]] else NULL
        }, error = function(e) NULL)
      }
      rep_obj <- if (!is.null(rep_path) && file.exists(rep_path)) {
        tryCatch(read.MFCLRep(rep_path), error = function(e) NULL)
      } else {
        NULL
      }
      out <- extract_rep_age_curves(rep_obj)
      assign(seed_dir, out, envir = age_cache)
      out
    }

    get_jitter_derived_rows <- function(sc) {
      cache_key <- paste(
        "v2",
        sc,
        as.character(isTRUE(converged_only)),
        format(converged_max_grad, trim = TRUE),
        sep = "::"
      )
      cached <- jitter_data_cache$derived[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }

      ref_metrics <- extract_reference_metrics_timeseries(rep_out_list[[sc]], sc)
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(ref_metrics) || is.null(jit_list) || length(jit_list) == 0) {
        jitter_data_cache$derived[[cache_key]] <- data.frame()
        return(jitter_data_cache$derived[[cache_key]])
      }

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      rows <- list()
      age_rows <- list()
      age_cache <- new.env(parent = emptyenv())

      for (i in seq_along(jit_list)) {
        jit <- jit_list[[i]]
        if (isTRUE(converged_only)) {
          jit_max_grad <- suppressWarnings(as.numeric(jit$max_grad))
          if (!isTRUE(jit$run_completed) || !is.finite(jit_max_grad) || abs(jit_max_grad) > converged_max_grad) next
        }
        derived <- if (is.list(jit) && !is.null(jit$derived_quantities)) jit$derived_quantities else NULL
        if (is.null(derived) || !is.data.frame(derived) || nrow(derived) == 0) next
        if (!"recruitment" %in% names(derived)) derived$recruitment <- NA_real_
        if (!"fishing_mortality" %in% names(derived)) derived$fishing_mortality <- NA_real_

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
            spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
            recruitment = suppressWarnings(as.numeric(recruitment)),
            fishing_mortality = suppressWarnings(as.numeric(fishing_mortality))
          )

        rows[[length(rows) + 1]] <- seed_df

        age_curve <- extract_jitter_age_curves(jit, age_cache = age_cache)
        if (!is.null(age_curve) && nrow(age_curve) > 0) {
          age_rows[[length(age_rows) + 1]] <- age_curve %>%
            transmute(
              scenario = sc,
              seed = as.character(seeds[[i]]),
              year = suppressWarnings(as.numeric(age)),
              natural_mortality = suppressWarnings(as.numeric(natural_mortality)),
              growth = suppressWarnings(as.numeric(growth))
            )
        }
      }

      value_rows <- bind_rows(rows)
      value_age_rows <- bind_rows(age_rows)
      if (is.null(value_age_rows) || nrow(value_age_rows) == 0) {
        value_age_rows <- data.frame(
          scenario = character(0),
          seed = character(0),
          year = numeric(0),
          natural_mortality = numeric(0),
          growth = numeric(0),
          stringsAsFactors = FALSE
        )
      }
      if (nrow(value_rows) == 0 && nrow(value_age_rows) == 0) {
        jitter_data_cache$derived[[cache_key]] <- data.frame()
        return(jitter_data_cache$derived[[cache_key]])
      }

      ref_recruitment <- if ("recruitment" %in% names(ref_metrics)) {
        suppressWarnings(as.numeric(ref_metrics$recruitment))
      } else {
        rep(NA_real_, nrow(ref_metrics))
      }
      ref_fishing_mortality <- if ("fishing_mortality" %in% names(ref_metrics)) {
        suppressWarnings(as.numeric(ref_metrics$fishing_mortality))
      } else {
        rep(NA_real_, nrow(ref_metrics))
      }
      ref_age_curves <- extract_rep_age_curves(rep_out_list[[sc]])

      ref_rows <- bind_rows(
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
        ),
        data.frame(
          scenario = sc,
          metric = "Recruitment",
          reference_value = ref_recruitment,
          year = suppressWarnings(as.numeric(ref_metrics$year)),
          stringsAsFactors = FALSE
        ),
        data.frame(
          scenario = sc,
          metric = "Fishing Mortality",
          reference_value = ref_fishing_mortality,
          year = suppressWarnings(as.numeric(ref_metrics$year)),
          stringsAsFactors = FALSE
        ),
        data.frame(
          scenario = sc,
          metric = "Natural Mortality at Age",
          reference_value = if (!is.null(ref_age_curves)) suppressWarnings(as.numeric(ref_age_curves$natural_mortality)) else numeric(0),
          year = if (!is.null(ref_age_curves)) suppressWarnings(as.numeric(ref_age_curves$age)) else numeric(0),
          stringsAsFactors = FALSE
        ),
        data.frame(
          scenario = sc,
          metric = "Growth Curve",
          reference_value = if (!is.null(ref_age_curves)) suppressWarnings(as.numeric(ref_age_curves$growth)) else numeric(0),
          year = if (!is.null(ref_age_curves)) suppressWarnings(as.numeric(ref_age_curves$age)) else numeric(0),
          stringsAsFactors = FALSE
        )
      )
      if (is.null(ref_rows) || nrow(ref_rows) == 0) {
        jitter_data_cache$derived[[cache_key]] <- data.frame()
        return(jitter_data_cache$derived[[cache_key]])
      }

      value_long <- bind_rows(
        value_rows %>%
          transmute(scenario, seed, year, metric = "Depletion", value = depletion),
        value_rows %>%
          transmute(scenario, seed, year, metric = "Spawning Potential (1e3 MT)", value = spawning_potential),
        value_rows %>%
          transmute(scenario, seed, year, metric = "Recruitment", value = recruitment),
        value_rows %>%
          transmute(scenario, seed, year, metric = "Fishing Mortality", value = fishing_mortality),
        value_age_rows %>%
          transmute(scenario, seed, year, metric = "Natural Mortality at Age", value = natural_mortality),
        value_age_rows %>%
          transmute(scenario, seed, year, metric = "Growth Curve", value = growth)
      ) %>%
        filter(is.finite(value))

      if (nrow(value_long) == 0) {
        jitter_data_cache$derived[[cache_key]] <- data.frame()
        return(jitter_data_cache$derived[[cache_key]])
      }

      jitter_data_cache$derived[[cache_key]] <- value_long %>%
        left_join(ref_rows, by = c("scenario", "metric", "year")) %>%
        mutate(
          pct_change = 100 * (value - reference_value) / pmax(abs(reference_value), .Machine$double.eps)
        )
      jitter_data_cache$derived[[cache_key]]
    }

    bind_rows(lapply(scenarios, get_jitter_derived_rows))
  }
  
  build_retro_data_for_scenario <- function(scenario, model_dir, rep_obj) {
    extract_yearly_sum <- function(slot_obj, scale = 1) {
      slot_df <- tryCatch(safe_array_to_df(slot_obj), error = function(e) NULL)
      if (is.null(slot_df) || nrow(slot_df) == 0) return(NULL)
      slot_df$year <- suppressWarnings(as.numeric(slot_df$year))
      slot_df$data <- suppressWarnings(as.numeric(slot_df$data))
      slot_df <- slot_df[is.finite(slot_df$year) & is.finite(slot_df$data), , drop = FALSE]
      if (nrow(slot_df) == 0) return(NULL)
      out <- stats::aggregate(data ~ year, data = slot_df, FUN = sum)
      out$data <- out$data / scale
      out
    }

    extract_retro_recruitment <- function(rep_obj) {
      rec_region_df <- extract_yearly_sum(tryCatch(rep_obj@rec_region, error = function(e) NULL), scale = 1e6)
      if (!is.null(rec_region_df) && nrow(rec_region_df) > 0) {
        names(rec_region_df)[names(rec_region_df) == "data"] <- "recruitment"
        return(rec_region_df)
      }

      eq_rec_df <- extract_yearly_sum(tryCatch(rep_obj@eq_rec, error = function(e) NULL), scale = 1e6)
      if (!is.null(eq_rec_df) && nrow(eq_rec_df) > 0) {
        names(eq_rec_df)[names(eq_rec_df) == "data"] <- "recruitment"
        return(eq_rec_df)
      }

      rec_df <- extract_yearly_sum(tryCatch(rep_obj@rec, error = function(e) NULL), scale = 1)
      if (!is.null(rec_df) && nrow(rec_df) > 0) {
        names(rec_df)[names(rec_df) == "data"] <- "recruitment"
        return(rec_df)
      }

      NULL
    }

    extract_retro_fm <- function(rep_obj) {
      fm_df <- tryCatch(safe_array_to_df(rep_obj@fm), error = function(e) NULL)
      popn_df <- tryCatch(safe_array_to_df(rep_obj@popN), error = function(e) NULL)

      if (!is.null(fm_df) && !is.null(popn_df) && nrow(fm_df) > 0 && nrow(popn_df) > 0) {
        fm_df$data <- suppressWarnings(as.numeric(fm_df$data))
        popn_df$data <- suppressWarnings(as.numeric(popn_df$data))
        popn_df$N <- popn_df$data
        popn_df$data <- NULL

        numeric_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), union(names(fm_df), names(popn_df)))
        for (col in numeric_cols) {
          if (col %in% names(fm_df)) fm_df[[col]] <- suppressWarnings(as.numeric(fm_df[[col]]))
          if (col %in% names(popn_df)) popn_df[[col]] <- suppressWarnings(as.numeric(popn_df[[col]]))
        }

        join_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), intersect(names(fm_df), names(popn_df)))
        if (all(c("year", "season") %in% join_cols)) {
          fm_popn <- merge(fm_df, popn_df, by = join_cols, all = FALSE)
          fm_popn <- fm_popn[is.finite(fm_popn$year) & is.finite(fm_popn$season) & is.finite(fm_popn$data) & is.finite(fm_popn$N), , drop = FALSE]
          if (nrow(fm_popn) > 0) {
            fm_popn$catch <- fm_popn$data * fm_popn$N
            yearly <- stats::aggregate(
              cbind(total_catch = catch, total_N = N) ~ year + season,
              data = fm_popn,
              FUN = sum
            )
            if (nrow(yearly) > 0) {
              yearly$harvest_rate <- yearly$total_catch / pmax(yearly$total_N, .Machine$double.eps)
              yearly$inst_F <- -log(pmax(1 - yearly$harvest_rate, 0.001))
              out <- stats::aggregate(inst_F ~ year, data = yearly, FUN = sum)
              names(out)[names(out) == "inst_F"] <- "fishing_mortality"
              return(out)
            }
          }
        }
      }

      fmlevel_df <- extract_yearly_sum(tryCatch(rep_obj@fmlevel, error = function(e) NULL), scale = 1)
      if (!is.null(fmlevel_df) && nrow(fmlevel_df) > 0) {
        fmlevel_df <- stats::aggregate(data ~ year, data = fmlevel_df, FUN = function(x) mean(x, na.rm = TRUE))
        names(fmlevel_df)[names(fmlevel_df) == "data"] <- "fishing_mortality"
        return(fmlevel_df)
      }

      NULL
    }

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

      out <- dep %>%
        inner_join(sp, by = "year") %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          scenario = scenario,
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))

      rec_df <- extract_retro_recruitment(rep_obj)
      if (!is.null(rec_df) && nrow(rec_df) > 0) {
        out <- merge(out, rec_df, by = "year", all = TRUE)
      }

      fm_df <- extract_retro_fm(rep_obj)
      if (!is.null(fm_df) && nrow(fm_df) > 0) {
        out <- merge(out, fm_df, by = "year", all = TRUE)
      }

      out
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

  post_hessian_cols_from_info <- function(model_info) {
      hs <- if (!is.null(model_info) && is.list(model_info) && is.list(model_info$hessian)) model_info$hessian else NULL
      requested <- if (!is.null(hs$requested) && !is.na(suppressWarnings(as.logical(hs$requested)))) {
        if (isTRUE(as.logical(hs$requested))) "TRUE" else "FALSE"
      } else {
        "NA"
      }
      attempted <- if (!is.null(hs$attempted) && !is.na(suppressWarnings(as.logical(hs$attempted)))) {
        if (isTRUE(as.logical(hs$attempted))) "TRUE" else "FALSE"
      } else {
        "NA"
      }
      status <- if (!is.null(hs$hessian_status) && nzchar(as.character(hs$hessian_status))) {
        as.character(hs$hessian_status)
      } else {
        "NA"
      }
      neg_eigen <- if (!is.null(hs$n_negative_eigenvalues) && !is.null(hs$n_total_eigenvalues) &&
        is.finite(suppressWarnings(as.numeric(hs$n_negative_eigenvalues))) &&
        is.finite(suppressWarnings(as.numeric(hs$n_total_eigenvalues)))) {
        sprintf("%d / %d", as.integer(hs$n_negative_eigenvalues), as.integer(hs$n_total_eigenvalues))
      } else {
        "NA"
      }
      list(
        `Post.Hessian.Requested` = requested,
        `Post.Hessian.Attempted` = attempted,
        `Post.Hessian.Status` = status,
        `Post.Neg..Eigen` = neg_eigen
      )
  }

  build_hessian_data_for_scenario <- function(scenario, model_dir, model_info = NULL) {
      hfile <- file.path(model_dir, scenario, "hessian", "hessian_info.rds")
      part_files <- list.files(
        file.path(model_dir, scenario, "hessian"),
        pattern = "^part_\\d+/hessian_info\\.rds$",
        full.names = TRUE,
        recursive = TRUE
      )
      post_cols <- post_hessian_cols_from_info(model_info)
      
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
          `Post.Hessian.Requested` = post_cols$`Post.Hessian.Requested`,
          `Post.Hessian.Attempted` = post_cols$`Post.Hessian.Attempted`,
          `Post.Hessian.Status` = post_cols$`Post.Hessian.Status`,
          `Post.Neg..Eigen` = post_cols$`Post.Neg..Eigen`,
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
          `Post.Hessian.Requested` = post_cols$`Post.Hessian.Requested`,
          `Post.Hessian.Attempted` = post_cols$`Post.Hessian.Attempted`,
          `Post.Hessian.Status` = post_cols$`Post.Hessian.Status`,
          `Post.Neg..Eigen` = post_cols$`Post.Neg..Eigen`,
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
        `Post.Hessian.Requested` = post_cols$`Post.Hessian.Requested`,
        `Post.Hessian.Attempted` = post_cols$`Post.Hessian.Attempted`,
        `Post.Hessian.Status` = post_cols$`Post.Hessian.Status`,
        `Post.Neg..Eigen` = post_cols$`Post.Neg..Eigen`,
        stringsAsFactors = FALSE
      )
  }

  profile_data_reactive <- reactive({
    filters <- lik_data_filters()
    req(rv$data_loaded, filters, filters$scenarios)

    if (length(filters$scenarios) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No models selected"))
    }

    type <- filters$profile_type
    selected <- filters$scenarios

    if (type == "jitter") {
      converged_only <- isTRUE(filters$jitter_converged_only_diagnostics)
      converged_max_grad <- if (is.finite(filters$jitter_grad_reference)) filters$jitter_grad_reference else 0.001
      data <- build_jitter_data(
        selected,
        rv$ParOut_list,
        rv$JitterPars_list,
        converged_only = converged_only,
        converged_max_grad = converged_max_grad
      )
      if (nrow(data) == 0) {
        msg <- if (isTRUE(converged_only)) {
          paste0("No converged jitter analysis results found (max_grad <= ", format(converged_max_grad, trim = TRUE), ")")
        } else {
          "No jitter analysis results found"
        }
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = msg, plot_kind = "jitter"))
      }
      return(list(data = data, group_col = NULL, label = "Jitter", message = NULL, plot_kind = "jitter"))
    }

    if (type == "jitter_params") {
      jitter_param_view <- if (is.null(filters$jitter_param_view)) "input" else filters$jitter_param_view
      converged_only <- isTRUE(filters$jitter_converged_only) && identical(jitter_param_view, "final")
      converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      data <- build_jitter_parameter_data(
        selected,
        rv$JitterPars_list,
        view = jitter_param_view,
        converged_only = converged_only,
        converged_max_grad = converged_max_grad
      )
      if (nrow(data) == 0) {
        msg <- if (identical(jitter_param_view, "final")) {
          if (isTRUE(converged_only)) {
            paste0("No converged jitter runs with final parameter distributions found (max_grad <= ", format(converged_max_grad, trim = TRUE), ")")
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
      converged_only <- isTRUE(filters$jitter_converged_only)
      converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      data <- build_jitter_derived_data(
        selected,
        rv$RepOut_list,
        rv$JitterPars_list,
        converged_only = converged_only,
        converged_max_grad = converged_max_grad
      )
      if (nrow(data) == 0) {
        msg <- if (isTRUE(converged_only)) {
          paste0("No converged jitter derived quantity distributions found (max_grad <= ", format(converged_max_grad, trim = TRUE), ")")
        } else {
          "No jitter derived quantity distributions found"
        }
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = msg, plot_kind = "jitter_derived"))
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
      if (!"recruitment" %in% names(data)) data$recruitment <- NA_real_
      if (!"fishing_mortality" %in% names(data)) data$fishing_mortality <- NA_real_
      data <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          recruitment = suppressWarnings(as.numeric(recruitment)),
          fishing_mortality = suppressWarnings(as.numeric(fishing_mortality)),
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "Retrospective outputs were found but numeric series could not be parsed", plot_kind = "retro"))
      }
      
      eps <- .Machine$double.eps
      base_terminal <- data %>%
        filter(peel == 0) %>%
        select(
          scenario,
          year,
          depletion_base = depletion,
          spawning_potential_base = spawning_potential,
          recruitment_base = recruitment,
          fishing_mortality_base = fishing_mortality
        )
      
      peel_terminal <- data %>%
        filter(peel > 0) %>%
        group_by(scenario, peel) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        summarise(
          year = max(year, na.rm = TRUE),
          depletion_peel = dplyr::last(depletion),
          spawning_potential_peel = dplyr::last(spawning_potential),
          recruitment_peel = dplyr::last(recruitment),
          fishing_mortality_peel = dplyr::last(fishing_mortality),
          .groups = "drop"
        )
      
      mohn_summary <- peel_terminal %>%
        left_join(base_terminal, by = c("scenario", "year")) %>%
        mutate(
          rho_dep_component = (depletion_peel - depletion_base) / pmax(abs(depletion_base), eps),
          rho_sp_component = (spawning_potential_peel - spawning_potential_base) / pmax(abs(spawning_potential_base), eps),
          rho_rec_component = (recruitment_peel - recruitment_base) / pmax(abs(recruitment_base), eps),
          rho_f_component = (fishing_mortality_peel - fishing_mortality_base) / pmax(abs(fishing_mortality_base), eps)
        ) %>%
        group_by(scenario) %>%
        summarise(
          mohn_rho_depletion = ifelse(sum(is.finite(rho_dep_component)) > 0, mean(rho_dep_component, na.rm = TRUE), NA_real_),
          mohn_rho_spawning_potential = ifelse(sum(is.finite(rho_sp_component)) > 0, mean(rho_sp_component, na.rm = TRUE), NA_real_),
          mohn_rho_recruitment = ifelse(sum(is.finite(rho_rec_component)) > 0, mean(rho_rec_component, na.rm = TRUE), NA_real_),
          mohn_rho_fishing_mortality = ifelse(sum(is.finite(rho_f_component)) > 0, mean(rho_f_component, na.rm = TRUE), NA_real_),
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
          function() build_hessian_data_for_scenario(sc, input$model_dir, rv$Info_list[[sc]])
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
    lik_data_cache_key()
  )

  observeEvent(profile_data_reactive(), {
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (identical(plot_kind, "jitter_params") && nrow(info$data) > 0) {
      current_window <- isolate(input$lik_jitter_param_window)
      if (is.null(current_window) || length(current_window) != 2) current_window <- c(1, 100)
      current_window <- c(max(1, min(current_window[1], 100)), max(1, min(current_window[2], 100)))
      if (current_window[1] > current_window[2]) current_window <- sort(current_window)
      if (!identical(input$lik_jitter_param_window, current_window)) {
        updateSliderInput(session, "lik_jitter_param_window", min = 1, max = 100, value = current_window)
      }
    }
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

    if (!(plot_kind %in% c("jitter", "jitter_params", "jitter_derived")) &&
        !is.null(group_col) &&
        length(group_col) == 1 &&
        nzchar(group_col) &&
        group_col %in% names(data) &&
        !is.null(filters$groups) &&
        length(filters$groups) > 0) {
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
      grad_reference <- if (is.finite(filters$jitter_grad_reference) && filters$jitter_grad_reference > 0) filters$jitter_grad_reference else 0.001
      converged_only <- isTRUE(filters$jitter_converged_only_diagnostics)
      jitter_counts <- format_jitter_convergence_counts(
        build_jitter_seed_status_tables(filters$scenarios, cutoff = grad_reference)$summary
      )
      plot_df <- data %>%
        mutate(
          max_grad = ifelse(max_grad > 0, max_grad, NA_real_),
          is_outlier = pct_diff < -5 | pct_diff > 20,
          outlier_direction = case_when(
            pct_diff < -5 ~ "below",
            pct_diff > 20 ~ "above",
            TRUE ~ "none"
          ),
          plot_pct_diff = case_when(
            pct_diff < -5 ~ -4.5,
            pct_diff > 20 ~ 19.5,
            TRUE ~ pct_diff
          )
        )

      ref_df <- plot_df %>%
        group_by(scenario) %>%
        summarise(ref_grad = first(ref_grad), .groups = "drop") %>%
        mutate(ref_grad = ifelse(ref_grad > 0, ref_grad, NA_real_))

      point_df <- plot_df %>% filter(is.finite(max_grad), max_grad > 0, is.finite(plot_pct_diff))
      if (nrow(point_df) == 0) {
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "No positive finite max_grad values available for jitter diagnostics.",
              size = 6,
              color = "#999999"
            ) +
            theme_void()
        )
      }
      outlier_df <- point_df %>% filter(is_outlier)

      return(
        ggplot(point_df, aes(x = max_grad, y = plot_pct_diff, color = jitter_id)) +
          geom_point(size = 2.3, alpha = 0.7, na.rm = TRUE) +
          geom_point(
            data = outlier_df %>% filter(outlier_direction == "above"),
            aes(x = max_grad, y = plot_pct_diff),
            inherit.aes = FALSE,
            color = "#d97904", size = 2.8, shape = 24, fill = "#d97904", na.rm = TRUE
          ) +
          geom_point(
            data = outlier_df %>% filter(outlier_direction == "below"),
            aes(x = max_grad, y = plot_pct_diff),
            inherit.aes = FALSE,
            color = "#d97904", size = 2.8, shape = 25, fill = "#d97904", na.rm = TRUE
          ) +
          geom_point(
            data = ref_df,
            aes(x = ref_grad, y = 0),
            inherit.aes = FALSE,
            color = "red", size = 5, shape = 18, na.rm = TRUE
          ) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.5) +
          geom_vline(xintercept = grad_reference, linetype = "dotted", color = "gray50", linewidth = 0.6) +
          scale_x_log10() +
          scale_color_viridis_c(option = "D", guide = "none") +
          coord_cartesian(ylim = c(-5, 20)) +
          facet_wrap(~ scenario, scales = "free_x", ncol = facet_ncol) +
          labs(
            x = "Maximum Gradient (log scale)",
            y = "% Difference in Objective Function",
            title = "Jitter Analysis: Convergence Diagnostics",
            subtitle = if (isTRUE(converged_only)) {
              paste0(
                "Converged only: max_grad <= ", format(grad_reference, trim = TRUE),
                " | Red diamond = Reference model | Orange triangles = Outliers",
                if (!is.null(jitter_counts)) paste0(" | Converged/total: ", jitter_counts) else ""
              )
            } else {
              paste0(
                "Red diamond = Reference model | Gray line = ", format(grad_reference, trim = TRUE),
                " | Orange triangles = Outliers",
                if (!is.null(jitter_counts)) paste0(" | Converged/total: ", jitter_counts) else ""
              )
            }
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "none",
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "lightblue"),
            panel.grid.minor = element_blank()
          )
      )
    }

    if (identical(plot_kind, "jitter_params")) {
      param_view <- if (is.null(filters$jitter_param_view)) "input" else filters$jitter_param_view
      param_display <- if (is.null(filters$jitter_param_display)) "family" else filters$jitter_param_display
      param_scope <- if (is.null(filters$jitter_param_scope)) "top" else filters$jitter_param_scope
      param_window <- if (is.null(filters$jitter_param_window)) c(1L, 100L) else as.integer(filters$jitter_param_window)
      converged_only <- isTRUE(filters$jitter_converged_only) && identical(param_view, "final")
      converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      metric <- if (identical(param_view, "input")) {
        if (is.null(filters$jitter_param_input_scale)) "bound_position" else filters$jitter_param_input_scale
      } else if (is.null(filters$jitter_param_metric)) "pct_change" else filters$jitter_param_metric
      range_pct <- if (identical(param_view, "input")) 100 else if (is.null(filters$jitter_param_range_pct)) 95 else pmax(50, pmin(100, suppressWarnings(as.integer(filters$jitter_param_range_pct))))
      jitter_counts <- format_jitter_convergence_counts(
        build_jitter_seed_status_tables(filters$scenarios, cutoff = converged_max_grad)$summary
      )

      jitter_interior_clip <- function(x, lower, upper, eps = 1e-12) {
        x <- suppressWarnings(as.numeric(x))
        lower <- suppressWarnings(as.numeric(lower))
        upper <- suppressWarnings(as.numeric(upper))
        out <- x
        ok <- is.finite(x) & is.finite(lower) & is.finite(upper) & (upper > lower)
        if (any(ok)) {
          span <- upper[ok] - lower[ok]
          margin <- pmax(abs(span) * 2e-2, eps)
          lo <- lower[ok] + margin
          hi <- upper[ok] - margin
          out[ok] <- pmin(hi, pmax(lo, x[ok]))
        }
        out
      }

      if (identical(param_display, "family")) {
        signed_max_abs <- function(x) {
          x <- as.numeric(x)
          x <- x[is.finite(x)]
          if (length(x) == 0) return(NA_real_)
          x[[which.max(abs(x))]]
        }

        family_plot_df <- data %>%
          mutate(
            family = ifelse(is.na(family) | !nzchar(family), "unclassified", family),
            plot_value = case_when(
              metric == "bound_position" & is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) ~
                (after - L_bound) / (U_bound - L_bound),
              metric == "value" ~ after,
              metric == "delta" ~ delta,
              metric == "baseline_minus" ~ (before - after),
              metric == "rel_baseline_minus" ~ dplyr::case_when(
                is.finite(before) & before != 0 ~ 100 * (before - after) / before,
                TRUE ~ NA_real_
              ),
              TRUE ~ dplyr::case_when(
                is.finite(pct_change) ~ pct_change,
                is.finite(before) & is.finite(after) & before != 0 ~
                  100 * (after - before) / before,
                TRUE ~ NA_real_
              )
            ),
            original_value = case_when(
              metric == "bound_position" & is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) &
                is.finite(before) ~ {
                  ref_before <- if (identical(param_view, "input")) {
                    jitter_interior_clip(before, L_bound, U_bound)
                  } else {
                    before
                  }
                  (ref_before - L_bound) / (U_bound - L_bound)
                },
              metric == "value" ~ {
                if (identical(param_view, "input")) {
                  dplyr::if_else(
                    is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) &
                      is.finite(before) & (before <= L_bound | before >= U_bound),
                    jitter_interior_clip(before, L_bound, U_bound),
                    before
                  )
                } else {
                  before
                }
              },
              TRUE ~ 0
            )
          ) %>%
          filter(is.finite(plot_value), !is.na(seed), nzchar(as.character(seed)))

        if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus")) {
          family_seed_df <- family_plot_df %>%
            group_by(scenario, family, seed) %>%
            summarise(plot_value = signed_max_abs(plot_value), .groups = "drop") %>%
            filter(is.finite(plot_value))
        } else {
          family_seed_df <- family_plot_df %>%
            group_by(scenario, family, seed) %>%
            summarise(plot_value = median(plot_value, na.rm = TRUE), .groups = "drop") %>%
            filter(is.finite(plot_value))
        }

        if (nrow(family_seed_df) == 0) {
          return(
            ggplot() +
              annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = "No finite family summary values available for the selected settings.",
                size = 6,
                color = "#999"
              ) +
              theme_void()
          )
        }

        if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus")) {
          original_family_df <- family_plot_df %>%
            group_by(scenario, family) %>%
            summarise(original_value = signed_max_abs(original_value), .groups = "drop")
        } else {
          original_family_df <- family_plot_df %>%
            group_by(scenario, family) %>%
            summarise(original_value = median(original_value, na.rm = TRUE), .groups = "drop")
        }

        plot_limit <- NULL
        if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus") && range_pct < 100) {
          finite_vals <- family_seed_df$plot_value[is.finite(family_seed_df$plot_value)]
          if (length(finite_vals) > 0) {
            robust_limit <- suppressWarnings(as.numeric(stats::quantile(abs(finite_vals), probs = range_pct / 100, na.rm = TRUE)))
            if (is.finite(robust_limit) && robust_limit > 0) {
              plot_limit <- c(-1.1 * robust_limit, 1.1 * robust_limit)
              if (metric %in% c("pct_change", "rel_baseline_minus")) {
                plot_limit <- c(max(plot_limit[1], -100), min(plot_limit[2], 100))
              }
            }
          }
        }

        y_label <- case_when(
          metric == "bound_position" ~ "Position within indepvar bounds",
          metric == "value" && identical(param_view, "input") ~ "Jittered input parameter value",
          metric == "value" ~ "Parameter value",
          metric == "delta" ~ "Change from original",
          metric == "baseline_minus" ~ "Baseline - fitted",
          metric == "rel_baseline_minus" ~ "Relative difference (%)",
          TRUE ~ "% change from original"
        )

        plot_title <- case_when(
          metric == "bound_position" ~ "Jitter Parameter Family Summary (Bound Position)",
          metric == "value" && identical(param_view, "final") ~ "Final Fitted Parameter Family Summary",
          metric == "delta" && identical(param_view, "final") ~ "Final Fitted Parameter Family Change Summary",
          metric == "baseline_minus" && identical(param_view, "final") ~ "Final Fitted Parameter Family Summary (Baseline - fitted)",
          metric == "rel_baseline_minus" && identical(param_view, "final") ~ "Final Fitted Parameter Family Summary (Relative difference %)",
          metric == "pct_change" && identical(param_view, "final") ~ "Final Fitted Parameter Family % Change Summary",
          metric == "value" ~ "Jittered Input Parameter Family Summary",
          metric == "delta" ~ "Jittered Input Parameter Family Change Summary",
          metric == "baseline_minus" ~ "Jittered Input Parameter Family Summary (Baseline - fitted)",
          metric == "rel_baseline_minus" ~ "Jittered Input Parameter Family Summary (Relative difference %)",
          TRUE ~ "Jittered Input Parameter Family % Change Summary"
        )

        agg_label <- if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus")) {
          "max |change| parameter by seed (signed)"
        } else {
          "median by seed"
        }
        plot_subtitle <- if (identical(param_view, "final") && converged_only) {
          paste0("Family-level summary across parameters (", agg_label, "), converged runs only (max_grad <= ", format(converged_max_grad, trim = TRUE), ").")
        } else if (identical(param_view, "final")) {
          paste0("Family-level summary across parameters (", agg_label, ") from completed final runs.")
        } else {
          paste0("Family-level summary across parameters (", agg_label, ") from jittered input parameters.")
        }
        if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus")) {
          plot_subtitle <- paste0(
            plot_subtitle,
            if (range_pct < 100) paste0(" Showing ", range_pct, "th percentile focused range.") else " Showing full range."
          )
        }
        if (!is.null(jitter_counts)) {
          plot_subtitle <- paste0(plot_subtitle, " Converged/total: ", jitter_counts, ".")
        }
        if (identical(param_view, "input") && metric %in% c("bound_position", "value")) {
          plot_subtitle <- paste0(plot_subtitle, " Red diamond = jitter reference (baseline after interior-bound adjustment when baseline hits bounds).")
        } else {
          plot_subtitle <- paste0(plot_subtitle, " Red diamond = baseline/original.")
        }

        p <- ggplot(family_seed_df, aes(x = family, y = plot_value)) +
          geom_boxplot(
            fill = "#9ecae1",
            color = "#2b6c8a",
            outlier.shape = NA,
            na.rm = TRUE
          ) +
          geom_point(
            aes(group = seed),
            position = position_jitter(width = 0.18, height = 0),
            alpha = 0.22,
            size = 1.0,
            color = "#1f4e79",
            na.rm = TRUE
          ) +
          {
            if (nrow(original_family_df) > 0) geom_point(
              data = original_family_df,
              aes(x = family, y = original_value),
              inherit.aes = FALSE,
              color = "#d62728",
              fill = "#d62728",
              shape = 23,
              size = 2.8,
              stroke = 0.4,
              na.rm = TRUE
            )
          } +
          facet_wrap(~ scenario, scales = "free", ncol = facet_ncol) +
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
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 9, face = "bold", colour = "#222222"),
            axis.title.y = element_text(margin = margin(r = 10)),
            plot.margin = margin(5.5, 10, 5.5, 14),
            panel.grid.minor = element_blank()
          )

        return(p + coord_flip(ylim = plot_limit))
      }

      ranked_params_all <- data %>%
        distinct(scenario, param_key, Index, Var_name, family, median_abs_pct_change, mean_abs_pct_change) %>%
        mutate(
          median_abs_pct_change = suppressWarnings(as.numeric(median_abs_pct_change)),
          mean_abs_pct_change = suppressWarnings(as.numeric(mean_abs_pct_change))
        )

      ranked_params <- ranked_params_all %>%
        filter(is.finite(median_abs_pct_change))

      selected_params <- ranked_params %>%
        group_by(scenario, family) %>%
        arrange(dplyr::desc(median_abs_pct_change), dplyr::desc(mean_abs_pct_change), Var_name, Index, .by_group = TRUE) %>%
        slice_head(n = 1) %>%
        ungroup()

      scenario_slots <- selected_params %>%
        group_by(scenario) %>%
        summarise(family_count = dplyr::n(), .groups = "drop") %>%
        mutate(extra_slots = pmax(20 - family_count, 0L))

      remaining_slots <- ranked_params %>%
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

      if (identical(param_scope, "all")) {
        window_start <- max(1L, min(param_window[1], 100L))
        window_end <- max(window_start, min(param_window[2], 100L))
        selected_params <- ranked_params_all %>%
          group_by(scenario) %>%
          arrange(
            dplyr::desc(is.finite(median_abs_pct_change)),
            dplyr::desc(median_abs_pct_change),
            dplyr::desc(mean_abs_pct_change),
            Var_name,
            Index,
            .by_group = TRUE
          ) %>%
          mutate(
            plot_rank = row_number(),
            n_params = dplyr::n(),
            window_rank_min = pmax(1L, floor(((window_start - 1) / 100) * n_params) + 1L),
            window_rank_max = pmax(window_rank_min, ceiling((window_end / 100) * n_params))
          ) %>%
          ungroup() %>%
          filter(plot_rank >= window_rank_min, plot_rank <= window_rank_max) %>%
          select(-n_params, -window_rank_min, -window_rank_max)
      }

      global_rank_caption <- NULL
      if (identical(param_scope, "all") && nrow(selected_params) > 0) {
        rank_span <- selected_params %>%
          group_by(scenario) %>%
          summarise(
            rank_min = min(plot_rank, na.rm = TRUE),
            rank_max = max(plot_rank, na.rm = TRUE),
            rank_total = dplyr::n_distinct(plot_rank),
            .groups = "drop"
          )
        rank_totals <- ranked_params_all %>%
          group_by(scenario) %>%
          summarise(rank_total = dplyr::n(), .groups = "drop")
        rank_span <- rank_span %>%
          select(-rank_total) %>%
          left_join(rank_totals, by = "scenario")
        global_rank_caption <- paste(
          paste0(rank_span$scenario, " ranks ", rank_span$rank_min, "-", rank_span$rank_max, " of ", rank_span$rank_total),
          collapse = " | "
        )
      }

      plot_df_all <- data %>%
        inner_join(selected_params %>% select(scenario, param_key, plot_rank), by = c("scenario", "param_key")) %>%
        mutate(
          param_label = paste0(Var_name, "\n[", family, "]"),
          plot_value = case_when(
            metric == "bound_position" & is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) ~
              (after - L_bound) / (U_bound - L_bound),
            metric == "value" ~ after,
            metric == "delta" ~ delta,
            metric == "baseline_minus" ~ (before - after),
            metric == "rel_baseline_minus" ~ dplyr::case_when(
              is.finite(before) & before != 0 ~ 100 * (before - after) / before,
              TRUE ~ NA_real_
            ),
            TRUE ~ dplyr::case_when(
              is.finite(pct_change) ~ pct_change,
              is.finite(before) & is.finite(after) & before != 0 ~
                100 * (after - before) / before,
              TRUE ~ NA_real_
            )
          ),
          original_value = case_when(
            metric == "bound_position" & is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) &
              is.finite(before) ~ {
                ref_before <- if (identical(param_view, "input")) {
                  jitter_interior_clip(before, L_bound, U_bound)
                } else {
                  before
                }
                (ref_before - L_bound) / (U_bound - L_bound)
              },
            metric == "value" ~ {
              if (identical(param_view, "input")) {
                dplyr::if_else(
                  is.finite(L_bound) & is.finite(U_bound) & (U_bound > L_bound) &
                    is.finite(before) & (before <= L_bound | before >= U_bound),
                  jitter_interior_clip(before, L_bound, U_bound),
                  before
                )
              } else {
                before
              }
            },
            TRUE ~ 0
          )
        )

      scaffold_df <- selected_params %>%
        transmute(
          scenario,
          param_key,
          plot_rank,
          param_label = paste0(Var_name, "\n[", family, "]")
        )

      plot_df <- plot_df_all %>%
        filter(is.finite(plot_value))

      if (nrow(plot_df) == 0 && !identical(param_scope, "all")) {
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "No finite parameter values in the selected percentile window for this scale.",
              size = 6,
              color = "#999"
            ) +
            theme_void()
        )
      }

      original_df <- plot_df_all %>%
        group_by(scenario, param_key, param_label) %>%
        summarise(original_value = first(original_value), .groups = "drop")

      plot_limit <- NULL
      if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus") && range_pct < 100) {
        finite_vals <- plot_df$plot_value[is.finite(plot_df$plot_value)]
        if (length(finite_vals) > 0) {
          robust_limit <- suppressWarnings(as.numeric(stats::quantile(abs(finite_vals), probs = range_pct / 100, na.rm = TRUE)))
          if (is.finite(robust_limit) && robust_limit > 0) {
            plot_limit <- c(-1.1 * robust_limit, 1.1 * robust_limit)
            if (metric %in% c("pct_change", "rel_baseline_minus")) {
              plot_limit <- c(max(plot_limit[1], -100), min(plot_limit[2], 100))
            }
          }
        }
      }

      param_levels <- selected_params %>%
        arrange(scenario, plot_rank, Var_name, Index) %>%
        pull(param_key)
      plot_df$param_key <- factor(plot_df$param_key, levels = unique(param_levels))
      original_df$param_key <- factor(original_df$param_key, levels = unique(param_levels))
      summary_df <- plot_df %>%
        group_by(scenario, param_key, param_label) %>%
        summarise(
          q10 = stats::quantile(plot_value, 0.1, na.rm = TRUE),
          q25 = stats::quantile(plot_value, 0.25, na.rm = TRUE),
          q50 = stats::quantile(plot_value, 0.5, na.rm = TRUE),
          q75 = stats::quantile(plot_value, 0.75, na.rm = TRUE),
          q90 = stats::quantile(plot_value, 0.9, na.rm = TRUE),
          .groups = "drop"
        )
      if (identical(param_scope, "all")) {
        summary_df <- scaffold_df %>%
          left_join(summary_df, by = c("scenario", "param_key", "param_label"))
      }
      summary_df$param_key <- factor(summary_df$param_key, levels = unique(param_levels))
      label_map <- plot_df %>%
        distinct(param_key, param_label) %>%
        mutate(param_key = as.character(param_key))
      if (identical(param_scope, "all")) {
        label_map <- selected_params %>%
          distinct(param_key, plot_rank) %>%
          arrange(plot_rank) %>%
          mutate(
            param_key = as.character(param_key),
            param_label = as.character(plot_rank)
          ) %>%
          select(param_key, param_label)
      }

      y_label <- case_when(
        metric == "bound_position" ~ "Position within indepvar bounds",
        metric == "value" && identical(param_view, "input") ~ "Jittered input parameter value",
        metric == "value" ~ "Parameter value",
        metric == "delta" ~ "Change from original",
        metric == "baseline_minus" ~ "Baseline - fitted",
        metric == "rel_baseline_minus" ~ "Relative difference (%)",
        TRUE ~ "% change from original"
      )

      plot_title <- case_when(
        metric == "bound_position" ~ "Jittered Input Parameter Bound Positions",
        metric == "bound_position" && identical(param_view, "final") ~ "Final Fitted Parameter Bound Positions",
        metric == "value" && identical(param_view, "final") ~ "Final Fitted Parameter Distributions",
        metric == "delta" && identical(param_view, "final") ~ "Final Fitted Parameter Changes",
        metric == "baseline_minus" && identical(param_view, "final") ~ "Final Fitted Parameters (Baseline - fitted)",
        metric == "rel_baseline_minus" && identical(param_view, "final") ~ "Final Fitted Parameters Relative Difference (%)",
        metric == "pct_change" && identical(param_view, "final") ~ "Final Fitted Parameter % Changes",
        metric == "value" ~ "Jittered Input Parameter Distributions",
        metric == "delta" ~ "Jittered Input Parameter Changes",
        metric == "baseline_minus" ~ "Jittered Input Parameters (Baseline - fitted)",
        metric == "rel_baseline_minus" ~ "Jittered Input Parameters Relative Difference (%)",
        TRUE ~ "Jittered Input Parameter % Changes"
      )

      plot_subtitle <- case_when(
        metric == "bound_position" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "), shown as position within indepvar bounds. Red diamond = original position."),
        metric == "bound_position" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs, shown as position within indepvar bounds. Red diamond = original position.",
        metric == "bound_position" ~ "About 20 parameters per scenario from jittered input pars, shown as position within indepvar bounds.",
        metric == "value" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "). Red diamond = original value"),
        metric == "delta" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "). Red diamond = no change"),
        metric == "baseline_minus" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "). Red diamond = no difference"),
        metric == "rel_baseline_minus" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "). Red diamond = 0% difference"),
        metric == "pct_change" && identical(param_view, "final") && converged_only ~ paste0("About 20 parameters per scenario from converged final runs (max_grad <= ", format(converged_max_grad, trim = TRUE), "). Red diamond = 0% change"),
        metric == "value" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = original value",
        metric == "delta" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = no change",
        metric == "baseline_minus" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = no difference",
        metric == "rel_baseline_minus" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = 0% difference",
        metric == "pct_change" && identical(param_view, "final") ~ "About 20 parameters per scenario from completed final runs. Red diamond = 0% change",
        metric == "value" ~ "About 20 parameters per scenario from jittered input pars.",
        metric == "delta" ~ "About 20 parameters per scenario from jittered input pars.",
        metric == "baseline_minus" ~ "About 20 parameters per scenario from jittered input pars.",
        metric == "rel_baseline_minus" ~ "About 20 parameters per scenario from jittered input pars.",
        TRUE ~ "About 20 parameters per scenario from jittered input pars."
      )

      if (identical(param_scope, "all")) {
        plot_subtitle <- gsub(
          "About 20 parameters per scenario",
          paste0("Showing parameter percentile window ", window_start, "-", window_end, "% per scenario"),
          plot_subtitle,
          fixed = TRUE
        )
        if (!is.null(global_rank_caption)) {
          plot_subtitle <- paste0(plot_subtitle, " Global ranks: ", global_rank_caption, ".")
        }
      }

        if (metric %in% c("delta", "pct_change", "baseline_minus", "rel_baseline_minus")) {
          plot_subtitle <- paste0(
            plot_subtitle,
            if (range_pct < 100) paste0(" Showing ", range_pct, "th percentile focused range.") else " Showing full range."
          )
        }
        if (!is.null(jitter_counts)) {
          plot_subtitle <- paste0(plot_subtitle, " Converged/total: ", jitter_counts, ".")
        }
      if (identical(param_view, "input") && metric %in% c("bound_position", "value")) {
        plot_subtitle <- paste0(plot_subtitle, " Red diamond = jitter reference (baseline after interior-bound adjustment when baseline hits bounds).")
      } else {
        plot_subtitle <- paste0(plot_subtitle, " Red diamond = baseline/original.")
      }

      if (identical(param_scope, "all")) {
        p <- ggplot(summary_df, aes(x = param_key, y = q50)) +
          geom_linerange(
            aes(ymin = q10, ymax = q90),
            colour = "#9fb9c9",
            linewidth = 0.8,
            na.rm = TRUE
          ) +
          geom_linerange(
            aes(ymin = q25, ymax = q75),
            colour = "#2b6c8a",
            linewidth = 2.2,
            na.rm = TRUE
          ) +
          geom_point(
            colour = "#1f4e79",
            size = 1.4,
            na.rm = TRUE
          ) +
          {
            if (nrow(original_df) > 0) geom_point(
              data = original_df,
              aes(x = param_key, y = original_value),
              inherit.aes = FALSE,
              color = "#d62728",
              fill = "#d62728",
              shape = 23,
              size = 2.6,
              stroke = 0.4,
              na.rm = TRUE
            )
          }
      } else {
        p <- ggplot(plot_df, aes(x = param_key, y = plot_value)) +
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
          {
            if (nrow(original_df) > 0) geom_point(
              data = original_df,
              aes(x = param_key, y = original_value),
              inherit.aes = FALSE,
              color = "#d62728",
              fill = "#d62728",
              shape = 23,
              size = 2.8,
              stroke = 0.4,
              na.rm = TRUE
            )
          }
      }

      p <- p +
        facet_wrap(~ scenario, scales = "free", ncol = facet_ncol) +
        scale_x_discrete(drop = FALSE, labels = function(x) {
          lab_map <- setNames(as.character(label_map$param_label), as.character(label_map$param_key))
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
          axis.text.y = element_text(
            size = if (identical(param_scope, "all")) 8 else 9,
            face = "bold",
            colour = "#222222",
            lineheight = 0.95
          ),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.margin = margin(5.5, 10, 5.5, 14),
          panel.grid.minor = element_blank()
        )

      return(p + coord_flip(ylim = plot_limit))
    }

    if (identical(plot_kind, "jitter_derived")) {
      converged_only <- isTRUE(filters$jitter_converged_only)
      converged_max_grad <- if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.01
      derived_view <- if (is.null(filters$jitter_derived_view)) "summary" else filters$jitter_derived_view
      jitter_counts <- format_jitter_convergence_counts(
        build_jitter_seed_status_tables(filters$scenarios, cutoff = converged_max_grad)$summary
      )
      scenario_levels <- unique(as.character(data$scenario))
      facet_ncol <- suppressWarnings(as.integer(input$lik_facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
      facet_ncol <- min(max(facet_ncol, 1), 12)

      ref_df <- data %>%
        distinct(scenario, metric, reference_value, year)

      summary_df <- data %>%
        group_by(scenario, metric, year) %>%
        summarise(
          q025 = stats::quantile(value, 0.025, na.rm = TRUE),
          q10 = stats::quantile(value, 0.10, na.rm = TRUE),
          q25 = stats::quantile(value, 0.25, na.rm = TRUE),
          q50 = stats::quantile(value, 0.50, na.rm = TRUE),
          q75 = stats::quantile(value, 0.75, na.rm = TRUE),
          q90 = stats::quantile(value, 0.90, na.rm = TRUE),
          q975 = stats::quantile(value, 0.975, na.rm = TRUE),
          .groups = "drop"
        )

      make_jitter_derived_plot <- function(metric_name, y_lab, add_dep_lines = FALSE, ylim_zero = FALSE) {
        x_lab <- if (metric_name %in% c("Natural Mortality at Age", "Growth Curve")) "Age class" else "Year"
        metric_ref <- ref_df %>%
          filter(metric == metric_name) %>%
          mutate(scenario = factor(as.character(scenario), levels = scenario_levels))

        if (identical(derived_view, "lines")) {
          metric_plot_df <- data %>%
            filter(metric == metric_name) %>%
            mutate(
              scenario = factor(as.character(scenario), levels = scenario_levels),
              seed_id = as.factor(seed)
            )

          if (nrow(metric_plot_df) == 0) {
            return(
              ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = paste("No", metric_name, "data"), size = 5, color = "#999") +
                theme_void()
            )
          }

          p <- ggplot(metric_plot_df, aes(x = year, y = value, group = seed, color = seed_id)) +
            geom_line(alpha = 0.55, linewidth = 0.6, na.rm = TRUE) +
            geom_line(
              data = metric_ref,
              aes(x = year, y = reference_value, group = 1),
              inherit.aes = FALSE,
              color = "#d62728",
              linewidth = 0.9,
              na.rm = TRUE
            ) +
            facet_wrap(~ scenario, ncol = facet_ncol, scales = "free_y") +
            scale_color_viridis_d(option = "D", guide = "none") +
            labs(x = x_lab, y = y_lab) +
            theme_bw(base_size = 11) +
            theme(
              strip.text = element_text(face = "bold"),
              strip.background = element_rect(fill = "#d9edf7"),
              panel.grid.minor = element_blank()
            )
        } else {
          metric_summary_df <- summary_df %>%
            filter(metric == metric_name) %>%
            mutate(scenario = factor(as.character(scenario), levels = scenario_levels))

          if (nrow(metric_summary_df) == 0) {
            return(
              ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = paste("No", metric_name, "data"), size = 5, color = "#999") +
                theme_void()
            )
          }

          p <- ggplot(metric_summary_df, aes(x = year)) +
            geom_ribbon(aes(ymin = q025, ymax = q975), fill = "#d9ecf5", alpha = 0.8, na.rm = TRUE) +
            geom_ribbon(aes(ymin = q10, ymax = q90), fill = "#9ecae1", alpha = 0.85, na.rm = TRUE) +
            geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#4f90b5", alpha = 0.9, na.rm = TRUE) +
            geom_line(aes(y = q50), color = "#123b5d", linewidth = 0.9, na.rm = TRUE) +
            geom_line(
              data = metric_ref,
              aes(x = year, y = reference_value, group = 1),
              inherit.aes = FALSE,
              color = "#d62728",
              linewidth = 0.9,
              na.rm = TRUE
            ) +
            facet_wrap(~ scenario, ncol = facet_ncol, scales = "free_y") +
            labs(x = x_lab, y = y_lab) +
            theme_bw(base_size = 11) +
            theme(
              strip.text = element_text(face = "bold"),
              strip.background = element_rect(fill = "#d9edf7"),
              panel.grid.minor = element_blank()
            )
        }

        if (isTRUE(add_dep_lines)) {
          p <- p +
            geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen")
        } else if (isTRUE(ylim_zero)) {
          p <- p + coord_cartesian(ylim = c(0, NA))
        }

        p
      }

      dep_plot <- make_jitter_derived_plot("Depletion", bquote(SB/SB["F=0"]), add_dep_lines = TRUE)
      rec_plot <- make_jitter_derived_plot("Recruitment", "Recruitment (Millions)", ylim_zero = TRUE)
      sp_plot <- make_jitter_derived_plot("Spawning Potential (1e3 MT)", bquote("Spawning Potential (" * 10^3 * " MT)"), ylim_zero = TRUE)
      fm_plot <- make_jitter_derived_plot("Fishing Mortality", "Annual Instantaneous F", ylim_zero = TRUE)
      nm_plot <- make_jitter_derived_plot("Natural Mortality at Age", "Natural Mortality (M)", ylim_zero = TRUE)
      gr_plot <- make_jitter_derived_plot("Growth Curve", "Length (cm)", ylim_zero = TRUE)

      metric_grid_ncol <- min(max(facet_ncol, 1), 6)
      combined_plot <- cowplot::plot_grid(
        dep_plot, rec_plot, sp_plot, fm_plot, nm_plot, gr_plot,
        ncol = metric_grid_ncol,
        align = "hv"
      )

      plot_title <- "Jitter Derived Quantity Time Series"
      plot_subtitle <- if (identical(derived_view, "lines")) {
        if (isTRUE(converged_only)) {
          paste0(
            "Individual jitter runs shown as lines. Red line = original model. Converged only: max_grad <= ",
            format(converged_max_grad, trim = TRUE),
            if (!is.null(jitter_counts)) paste0(" | Converged/total: ", jitter_counts) else ""
          )
        } else {
          paste0(
            "Individual jitter runs shown as lines. Red line = original model.",
            if (!is.null(jitter_counts)) paste0(" Converged/total: ", jitter_counts, ".") else ""
          )
        }
      } else {
        if (isTRUE(converged_only)) {
          paste0(
            "Bands show 95%, 80%, and 50% ranges with median line. Red line = original model. Converged only: max_grad <= ",
            format(converged_max_grad, trim = TRUE),
            if (!is.null(jitter_counts)) paste0(" | Converged/total: ", jitter_counts) else ""
          )
        } else {
          paste0(
            "Bands show 95%, 80%, and 50% ranges with median line. Red line = original model.",
            if (!is.null(jitter_counts)) paste0(" Converged/total: ", jitter_counts, ".") else ""
          )
        }
      }

      return(
        cowplot::ggdraw() +
          cowplot::draw_label(plot_title, x = 0.5, y = 0.995, hjust = 0.5, vjust = 1, fontface = "bold", size = 15) +
          cowplot::draw_label(plot_subtitle, x = 0.5, y = 0.965, hjust = 0.5, vjust = 1, size = 10) +
          cowplot::draw_plot(combined_plot, x = 0, y = 0, width = 1, height = 0.93)
      )
    }
    
    if (identical(plot_kind, "retro")) {
      if (!"recruitment" %in% names(data)) data$recruitment <- NA_real_
      if (!"fishing_mortality" %in% names(data)) data$fishing_mortality <- NA_real_
      retro_df <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          recruitment = suppressWarnings(as.numeric(recruitment)),
          fishing_mortality = suppressWarnings(as.numeric(fishing_mortality)),
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

      retro_plot_cache_key <- paste(
        normalizePath(input$model_dir, winslash = "/", mustWork = FALSE),
        paste(sort(unique(as.character(retro_df$scenario))), collapse = "|"),
        nrow(retro_df),
        suppressWarnings(min(retro_df$year, na.rm = TRUE)),
        suppressWarnings(max(retro_df$year, na.rm = TRUE)),
        paste(sort(unique(as.integer(retro_df$peel))), collapse = "|"),
        as.character(input$lik_facet_ncol),
        if (!is.null(info$rho) && nrow(info$rho) > 0) {
          paste(
            apply(info$rho, 1, function(x) paste(as.character(x), collapse = "|")),
            collapse = "||"
          )
        } else {
          "no_rho"
        },
        sep = "::"
      )

      return(
        get_cached_heavy("retro_plot", retro_plot_cache_key, function() {
      
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
        rho_df %>% transmute(scenario, metric = "Depletion", label = sprintf("Mohn's rho: %.3f", mohn_rho_depletion))
      } else {
        data.frame(scenario = character(0), metric = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      sp_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, metric = "Spawning potential", label = sprintf("Mohn's rho: %.3f", mohn_rho_spawning_potential))
      } else {
        data.frame(scenario = character(0), metric = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      rec_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, metric = "Recruitment", label = sprintf("Mohn's rho: %.3f", mohn_rho_recruitment))
      } else {
        data.frame(scenario = character(0), metric = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      fm_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, metric = "Fishing mortality", label = sprintf("Mohn's rho: %.3f", mohn_rho_fishing_mortality))
      } else {
        data.frame(scenario = character(0), metric = character(0), label = character(0), stringsAsFactors = FALSE)
      }

      scenario_levels <- unique(as.character(retro_df$scenario))
      facet_ncol <- suppressWarnings(as.integer(input$lik_facet_ncol))
      if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
      facet_ncol <- min(max(facet_ncol, 1), 12)

      make_retro_plot <- function(value_col, y_lab, anno_tbl = NULL, add_dep_lines = FALSE, ylim_zero = FALSE) {
        plot_df <- retro_df %>%
          transmute(
            scenario = factor(as.character(scenario), levels = scenario_levels),
            year = year,
            peel = factor(as.character(peel), levels = peel_levels_chr),
            value = .data[[value_col]]
          ) %>%
          filter(is.finite(value))

        if (nrow(plot_df) == 0) {
          return(
            ggplot() +
              annotate("text", x = 0.5, y = 0.5, label = "No numeric data", size = 5, color = "#999") +
              theme_void()
          )
        }

        p <- ggplot(
          plot_df,
          aes(x = year, y = value, color = peel, group = interaction(scenario, peel))
        ) +
          geom_line(linewidth = 1.0, alpha = 0.9) +
          scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
          facet_wrap(~ scenario, ncol = facet_ncol, scales = "free_y") +
          labs(x = "Year", y = y_lab) +
          theme_bw(base_size = 11) +
          theme(
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "#d9edf7"),
            panel.grid.minor = element_blank(),
            legend.position = "none"
          )

        if (!is.null(anno_tbl) && nrow(anno_tbl) > 0) {
          anno_tbl$scenario <- factor(as.character(anno_tbl$scenario), levels = scenario_levels)
          p <- p + geom_text(
            data = anno_tbl,
            aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.2, size = 3.1, fontface = "bold", color = "black"
          )
        }

        if (isTRUE(add_dep_lines)) {
          p <- p +
            geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
            coord_cartesian(ylim = c(0, 1))
        } else if (isTRUE(ylim_zero)) {
          p <- p + coord_cartesian(ylim = c(0, NA))
        }

        p
      }

      dep_plot <- make_retro_plot("depletion", bquote(SB/SB["F=0"]), dep_anno, add_dep_lines = TRUE)
      rec_plot <- make_retro_plot("recruitment", "Recruitment (Millions)", rec_anno, ylim_zero = TRUE)
      sp_plot <- make_retro_plot("spawning_potential", bquote("Spawning Potential (" * 10^3 * " MT)"), sp_anno, ylim_zero = TRUE)
      fm_plot <- make_retro_plot("fishing_mortality", "Annual Instantaneous F", fm_anno, ylim_zero = TRUE)

      metric_grid_ncol <- min(max(facet_ncol, 1), 4)
      combined_plot <- cowplot::plot_grid(
        dep_plot, rec_plot, sp_plot, fm_plot,
        ncol = metric_grid_ncol,
        align = "hv"
      )

      retro_legend <- cowplot::get_legend(
        ggplot(
          retro_df %>%
            transmute(
              scenario = factor(as.character(scenario), levels = scenario_levels),
              year = year,
              peel = factor(as.character(peel), levels = peel_levels_chr),
              value = depletion
            ) %>%
            filter(is.finite(value)),
          aes(x = year, y = value, color = peel, group = interaction(scenario, peel))
        ) +
          geom_line(linewidth = 1.1) +
          scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
          theme_bw() +
          theme(
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            legend.key.width = unit(1.3, "cm")
          ) +
          labs(color = "Terminal year")
      )

      cowplot::plot_grid(combined_plot, retro_legend, ncol = 1, rel_heights = c(1, 0.08))
      })
      )
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
  observeEvent(list(input$live_update_plots, input$lik_main_tab, input$lik_scenarios, input$lik_profile_type, input$lik_jitter_type,
                    input$lik_groups, input$lik_regions, input$lik_split_by_region, input$lik_facet_ncol,
                    input$lik_jitter_grad_reference, input$lik_jitter_converged_only_diagnostics,
                    input$lik_jitter_param_view, input$lik_jitter_param_display, input$lik_jitter_param_scope, input$lik_jitter_param_window,
                    input$lik_jitter_converged_only, input$lik_jitter_converged_max_grad,
                    input$lik_jitter_derived_view, input$lik_jitter_param_input_scale,
                    input$lik_jitter_param_metric, input$lik_jitter_param_range_pct), {
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
    lik_plot_cache_key()
  )

  output$likelihood_plot <- renderPlot({
    likelihood_plot_reactive()
  })

  output$likelihood_info_ui <- renderUI({
    if (!identical(input$lik_main_tab, "likelihood")) return(NULL)
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

  output$jitter_info_ui <- renderUI({
    if (!identical(input$lik_main_tab, "jitter")) return(NULL)

    filters <- lik_filters_current()
    jitter_info <- jitter_info_reactive()
    if (is.null(jitter_info) || is.null(jitter_info$summary) || nrow(jitter_info$summary) == 0) return(NULL)

    cutoff <- if (identical(filters$profile_type, "jitter")) {
      if (is.finite(filters$jitter_grad_reference)) filters$jitter_grad_reference else 0.001
    } else {
      if (is.finite(filters$jitter_converged_max_grad)) filters$jitter_converged_max_grad else 0.001
    }

    box(
      title = "Jitter Information",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      collapsed = TRUE,
      div(
        style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
        tags$div("Summary of jitter seeds loaded for each selected model.", style = "font-weight: bold; margin-bottom: 4px;"),
        tags$div(
          paste0(
            "Converged means completed runs with |max_grad| <= ",
            format(cutoff, trim = TRUE),
            "."
          ),
          style = "font-size: 12px; color: #333;"
        )
      ),
      tags$div(style = "font-weight: bold; margin-bottom: 6px;", "Model summary"),
      DTOutput("jitter_info_table"),
      tags$hr(style = "margin: 12px 0 10px 0;"),
      tags$div(style = "font-weight: bold; margin-bottom: 6px;", "Seed details"),
      uiOutput("jitter_seed_model_ui"),
      DTOutput("jitter_seed_table")
    )
  })

  output$param_guide_ui <- renderUI({
    if (!identical(input$lik_main_tab, "param_guide")) return(NULL)
    tbl <- param_guide_table_reactive()

    tagList(
      box(
        title = "MFCL Master Parameter Catalog",
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        collapsed = TRUE,
        div(
          style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
          tags$div("Comprehensive family/index catalog from MFCL parameter structures (age/fish/region/species pars + core flags).", style = "font-weight: bold; margin-bottom: 4px;"),
          tags$div("Includes whether each parameter appears in selected models' indepvar.rpt.", style = "font-size: 12px; color: #333;")
        ),
        DTOutput("param_guide_master_table")
      ),
      box(
        title = "Selected Models: indepvar.rpt Parameters",
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        collapsed = FALSE,
        div(
          style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
          tags$div("Descriptions are derived from MFCL source/manual rules and then matched to parameters that appear in indepvar.rpt.", style = "font-weight: bold; margin-bottom: 4px;"),
          tags$div("Source reference: PacificCommunity/multifan-cl source + MULTIFAN-CL-Users-Guide.pdf.", style = "font-size: 12px; color: #333;")
        ),
        if (is.null(tbl) || nrow(tbl) == 0) {
          tags$div(style = "color:#777;", "No indepvar.rpt parameter rows found for the selected models.")
        } else {
          DTOutput("param_guide_table")
        }
      )
    )
  })

  output$retro_info_ui <- renderUI({
    if (!identical(input$lik_main_tab, "retro")) return(NULL)

    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else NULL
    retro_df <- if (!is.null(info$data)) info$data else NULL
    rho_df <- if (!is.null(info$rho)) info$rho else NULL
    has_retro_data <- identical(plot_kind, "retro") && !is.null(retro_df) && nrow(retro_df) > 0
    has_rho <- !is.null(rho_df) && nrow(rho_df) > 0

    box(
      title = "Retro Information",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      collapsed = TRUE,
      div(
        style = "margin-bottom: 10px; padding: 10px 12px; background: #fff8e1; border: 1px solid #f0d98c; border-left: 4px solid #f39c12; border-radius: 4px;",
        tags$div("Retrospective diagnostics compare the base run against successive terminal-year peels.", style = "font-weight: bold; margin-bottom: 4px;"),
        tags$div("The plot shows depletion, recruitment, spawning potential, and F trajectories by peel. Tables below summarize Mohn's rho and terminal-year peel information.", style = "font-size: 12px; color: #333;")
      ),
      tags$div(style = "font-weight: bold; margin-bottom: 6px;", "Mohn's rho summary"),
      if (!has_rho) tags$div(
        style = "margin-bottom: 8px; color: #777;",
        "Mohn's rho summary is not available for the current selection."
      ),
      uiOutput("retro_rho_table"),
      tags$hr(style = "margin: 12px 0 10px 0;"),
      tags$div(style = "font-weight: bold; margin-bottom: 6px;", "Peel details"),
      if (!has_retro_data) tags$div(
        style = "margin-bottom: 8px; color: #777;",
        "Retro peel details are not available for the current selection."
      ),
      uiOutput("retro_peel_model_ui"),
      uiOutput("retro_peel_table")
    )
  })

  output$hessian_info_ui <- renderUI({
    if (!identical(input$lik_main_tab, "hessian")) return(NULL)

    box(
      title = "Hessian Information",
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
      )
    )
  })
  
  output$likelihood_table_ui <- renderUI({
    if (!identical(input$lik_main_tab, "hessian")) return(NULL)
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
      DTOutput("likelihood_table")
    )
  })

  output$profile_gradient_table_ui <- renderUI({
    if (!identical(input$lik_main_tab, "likelihood")) return(NULL)
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    grad_tbl <- profile_gradient_table_reactive()
    filters <- lik_filters()
    if (is.null(filters) || !identical(filters$profile_type, "components")) return(NULL)
    if (!identical(plot_kind, "piner")) return(NULL)
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
    if (!identical(input$lik_main_tab, "hessian")) return(NULL)
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (!identical(plot_kind, "hessian") || nrow(info$data) == 0) return(NULL)

    htbl <- format_hessian_display_cols(info$data)
    htbl[] <- lapply(htbl, function(col) {
      out <- as.character(col)
      out[is.na(col) | !nzchar(trimws(out))] <- "NA"
      out
    })

    datatable(
      htbl,
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
      format_hessian_display_cols(grad_tbl),
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
    if (!identical(input$lik_main_tab, "likelihood")) return(NULL)
    target_tbl <- profile_target_info_reactive()
    if (is.null(target_tbl) || nrow(target_tbl) == 0) return(NULL)

    datatable(
      target_tbl,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$jitter_info_table <- renderDT({
    if (!identical(input$lik_main_tab, "jitter")) return(NULL)
    jitter_info <- jitter_info_reactive()
    jitter_tbl <- if (!is.null(jitter_info)) jitter_info$summary else NULL
    if (is.null(jitter_tbl) || nrow(jitter_tbl) == 0) return(NULL)

    datatable(
      format_hessian_display_cols(jitter_tbl),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$jitter_seed_table <- renderDT({
    if (!identical(input$lik_main_tab, "jitter")) return(NULL)
    jitter_info <- jitter_info_reactive()
    seed_tbl <- if (!is.null(jitter_info)) jitter_info$seeds else NULL
    if (is.null(seed_tbl) || nrow(seed_tbl) == 0) return(NULL)

    if (!is.null(input$jitter_seed_model) &&
        nzchar(input$jitter_seed_model) &&
        input$jitter_seed_model %in% seed_tbl$Model) {
      seed_tbl <- seed_tbl %>% filter(Model == input$jitter_seed_model)
    }

    datatable(
      format_hessian_display_cols(seed_tbl),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$jitter_seed_model_ui <- renderUI({
    if (!identical(input$lik_main_tab, "jitter")) return(NULL)
    jitter_info <- jitter_info_reactive()
    seed_tbl <- if (!is.null(jitter_info)) jitter_info$seeds else NULL
    if (is.null(seed_tbl) || nrow(seed_tbl) == 0) return(NULL)

    model_choices <- unique(seed_tbl$Model)
    if (length(model_choices) <= 1) return(NULL)

    selectInput(
      "jitter_seed_model",
      "Model:",
      choices = model_choices,
      selected = if (!is.null(input$jitter_seed_model) && input$jitter_seed_model %in% model_choices) {
        input$jitter_seed_model
      } else {
        model_choices[[1]]
      }
    )
  })

  output$param_guide_table <- renderDT({
    if (!identical(input$lik_main_tab, "param_guide")) return(NULL)
    tbl <- param_guide_table_reactive()
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

    datatable(
      tbl,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$param_guide_master_table <- renderDT({
    if (!identical(input$lik_main_tab, "param_guide")) return(NULL)
    tbl <- master_param_catalog_reactive()
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

    datatable(
      tbl,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  retro_hessian_info_reactive <- reactive({
    filters <- lik_data_filters()
    scenarios <- if (!is.null(filters) && length(filters$scenarios) > 0) {
      filters$scenarios
    } else {
      input$lik_scenarios
    }
    if (length(scenarios) == 0) return(NULL)

    rows <- list()
    for (sc in scenarios) {
      base_hs <- rv$Info_list[[sc]]$hessian
      base_requested <- if (!is.null(base_hs$requested) && !is.na(suppressWarnings(as.logical(base_hs$requested)))) isTRUE(as.logical(base_hs$requested)) else NA
      base_attempted <- if (!is.null(base_hs$attempted) && !is.na(suppressWarnings(as.logical(base_hs$attempted)))) isTRUE(as.logical(base_hs$attempted)) else NA
      rows[[length(rows) + 1]] <- data.frame(
        Model = sc,
        Peel = 0L,
        `Hessian.Requested` = if (is.na(base_requested)) NA else isTRUE(base_requested),
        `Hessian.Attempted` = if (is.na(base_attempted)) NA else isTRUE(base_attempted),
        `Hessian.Status` = if (!is.null(base_hs$hessian_status) && nzchar(as.character(base_hs$hessian_status))) as.character(base_hs$hessian_status) else NA_character_,
        `Neg..Eigen` = if (!is.null(base_hs$n_negative_eigenvalues) && !is.null(base_hs$n_total_eigenvalues) &&
          is.finite(suppressWarnings(as.numeric(base_hs$n_negative_eigenvalues))) &&
          is.finite(suppressWarnings(as.numeric(base_hs$n_total_eigenvalues)))) {
          sprintf("%d / %d", as.integer(base_hs$n_negative_eigenvalues), as.integer(base_hs$n_total_eigenvalues))
        } else {
          NA_character_
        },
        stringsAsFactors = FALSE
      )

      retro_dir <- file.path(input$model_dir, sc, "retro")
      peel_dirs <- list.dirs(retro_dir, recursive = FALSE, full.names = TRUE)
      peel_dirs <- peel_dirs[grepl("peel_\\d+$", peel_dirs)]

      for (pd in peel_dirs) {
        peel_num <- suppressWarnings(as.integer(stringr::str_extract(basename(pd), "\\d+$")))
        if (!is.finite(peel_num)) next
        info_file <- file.path(pd, "retro_info.rds")
        if (!file.exists(info_file)) next
        rinfo <- tryCatch(readRDS(info_file), error = function(e) NULL)
        hs <- if (!is.null(rinfo) && is.list(rinfo$hessian)) rinfo$hessian else NULL
        hs_requested <- if (!is.null(hs$requested) && !is.na(suppressWarnings(as.logical(hs$requested)))) isTRUE(as.logical(hs$requested)) else NA
        hs_attempted <- if (!is.null(hs$attempted) && !is.na(suppressWarnings(as.logical(hs$attempted)))) isTRUE(as.logical(hs$attempted)) else NA
        rows[[length(rows) + 1]] <- data.frame(
          Model = sc,
          Peel = as.integer(peel_num),
          `Hessian.Requested` = if (is.na(hs_requested)) NA else isTRUE(hs_requested),
          `Hessian.Attempted` = if (is.na(hs_attempted)) NA else isTRUE(hs_attempted),
          `Hessian.Status` = if (!is.null(hs$hessian_status) && nzchar(as.character(hs$hessian_status))) as.character(hs$hessian_status) else NA_character_,
          `Neg..Eigen` = if (!is.null(hs$n_negative_eigenvalues) && !is.null(hs$n_total_eigenvalues) &&
            is.finite(suppressWarnings(as.numeric(hs$n_negative_eigenvalues))) &&
            is.finite(suppressWarnings(as.numeric(hs$n_total_eigenvalues)))) {
            sprintf("%d / %d", as.integer(hs$n_negative_eigenvalues), as.integer(hs$n_total_eigenvalues))
          } else {
            NA_character_
          },
          stringsAsFactors = FALSE
        )
      }
    }

    out <- bind_rows(rows)
    if (nrow(out) == 0) return(NULL)
    out %>% arrange(Model, Peel)
  })
  retro_hessian_info_reactive <- bindCache(
    retro_hessian_info_reactive,
    input$model_dir,
    list(scenarios = sort(input$lik_scenarios), profile_type = current_profile_type())
  )

  output$retro_rho_table <- renderUI({
    tryCatch({
      if (!identical(input$lik_main_tab, "retro")) return(NULL)
      info <- profile_data_reactive()
      plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else NULL
      rho_df <- if (!is.null(info$rho)) info$rho else NULL
      if (!identical(plot_kind, "retro") || is.null(rho_df) || nrow(rho_df) == 0) {
        return(simple_html_table(data.frame(
          Message = "Mohn's rho summary is not available for the current selection.",
          stringsAsFactors = FALSE
        )))
      }

      required_rho_cols <- c(
        "scenario",
        "mohn_rho_depletion",
        "mohn_rho_recruitment",
        "mohn_rho_spawning_potential",
        "mohn_rho_fishing_mortality"
      )
      if (!all(required_rho_cols %in% names(rho_df))) {
        return(simple_html_table(data.frame(
          Message = "Mohn's rho columns are missing in retro data.",
          stringsAsFactors = FALSE
        )))
      }

      rho_tbl <- rho_df %>%
        transmute(
          Model = scenario,
          `Mohn's rho: SB/SB[F=0]` = mohn_rho_depletion,
          `Mohn's rho: Recruitment` = mohn_rho_recruitment,
          `Mohn's rho: SB (1e3 MT)` = mohn_rho_spawning_potential,
          `Mohn's rho: F` = mohn_rho_fishing_mortality
        )

      retro_hs <- retro_hessian_info_reactive()
      required_hs_cols <- c("Model", "Hessian.Requested", "Hessian.Attempted", "Hessian.Status", "Neg..Eigen")
      if (!is.null(retro_hs) && nrow(retro_hs) > 0 && all(required_hs_cols %in% names(retro_hs))) {
        hs_summary <- retro_hs %>%
          group_by(Model) %>%
          summarise(
            `Hessian.Requested` = {
              vals <- .data[["Hessian.Requested"]]
              if (sum(!is.na(vals)) == 0) NA_character_ else paste0(sum(vals %in% TRUE, na.rm = TRUE), " / ", sum(!is.na(vals)))
            },
            `Hessian.Attempted` = {
              vals <- .data[["Hessian.Attempted"]]
              if (sum(!is.na(vals)) == 0) NA_character_ else paste0(sum(vals %in% TRUE, na.rm = TRUE), " / ", sum(!is.na(vals)))
            },
            `Hessian.Status` = {
              vals <- unique(na.omit(as.character(.data[["Hessian.Status"]])))
              if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
            },
            `Neg..Eigen` = {
              vals <- unique(na.omit(as.character(.data[["Neg..Eigen"]])))
              if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
            },
            .groups = "drop"
          )
        rho_tbl <- rho_tbl %>% left_join(hs_summary, by = "Model")
      }
      simple_html_table(format_hessian_display_cols(rho_tbl))
    }, error = function(e) {
      simple_html_table(data.frame(
        Message = paste("Retro rho summary rendering error:", conditionMessage(e)),
        stringsAsFactors = FALSE
      ))
    })
  })

  output$retro_peel_model_ui <- renderUI({
    if (!identical(input$lik_main_tab, "retro")) return(NULL)
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else NULL
    retro_df <- if (!is.null(info$data)) info$data else NULL
    if (!identical(plot_kind, "retro") || is.null(retro_df) || nrow(retro_df) == 0) return(NULL)

    model_choices <- unique(as.character(retro_df$scenario))
    if (length(model_choices) <= 1) return(NULL)

    selectInput(
      "retro_peel_model",
      "Model:",
      choices = model_choices,
      selected = if (!is.null(input$retro_peel_model) && input$retro_peel_model %in% model_choices) {
        input$retro_peel_model
      } else {
        model_choices[[1]]
      }
    )
  })

  output$retro_peel_table <- renderUI({
    if (!identical(input$lik_main_tab, "retro")) return(NULL)
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else NULL
    retro_df <- if (!is.null(info$data)) info$data else NULL
    if (!identical(plot_kind, "retro") || is.null(retro_df) || nrow(retro_df) == 0) {
      return(simple_html_table(data.frame(
        Message = "Retro peel details are not available for the current selection.",
        stringsAsFactors = FALSE
      )))
    }

    peel_tbl <- retro_df %>%
      mutate(
        scenario = as.character(scenario),
        year = suppressWarnings(as.numeric(year)),
        peel = suppressWarnings(as.integer(peel)),
        depletion = suppressWarnings(as.numeric(depletion)),
        spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
        recruitment = suppressWarnings(as.numeric(recruitment)),
        fishing_mortality = suppressWarnings(as.numeric(fishing_mortality))
      ) %>%
      filter(is.finite(peel), peel > 0, is.finite(year)) %>%
      group_by(scenario, peel) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      summarise(
        `Terminal year` = max(year, na.rm = TRUE),
        `SB/SB[F=0]` = dplyr::last(depletion),
        Recruitment = dplyr::last(recruitment),
        `SB (1e3 MT)` = dplyr::last(spawning_potential),
        F = dplyr::last(fishing_mortality),
        .groups = "drop"
      ) %>%
      rename(Model = scenario, Peel = peel) %>%
      arrange(Model, Peel)

    retro_hs <- retro_hessian_info_reactive()
    if (!is.null(retro_hs) && nrow(retro_hs) > 0) {
      peel_tbl <- peel_tbl %>% left_join(retro_hs, by = c("Model", "Peel"))
    }

    if (!is.null(input$retro_peel_model) &&
        nzchar(input$retro_peel_model) &&
        input$retro_peel_model %in% peel_tbl$Model) {
      peel_tbl <- peel_tbl %>% filter(Model == input$retro_peel_model)
    }

    if (nrow(peel_tbl) == 0) {
      return(simple_html_table(data.frame(
        Message = "No peeled retrospective runs were found (peel > 0).",
        stringsAsFactors = FALSE
      )))
    }
    simple_html_table(format_hessian_display_cols(peel_tbl))
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
