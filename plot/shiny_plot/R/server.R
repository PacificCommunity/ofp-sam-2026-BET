source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")
source("R/modules/mod_likelihood.R")
source("R/modules/mod_sections.R")
source("R/modules/mod_selftest.R")
source("R/server/download_helpers.R")
source("R/server/server_dir_detection.R")
source("R/server/server_data_load.R")
source("R/server/server_nav.R")

reset_loaded_data_state <- function(rv) {
  rv$data_loaded <- FALSE
  rv$initial_render_pending <- FALSE
  rv$initial_render_nonce <- 0
  rv$ParOut_list <- NULL
  rv$RepOut_list <- NULL
  rv$LengOut_list <- NULL
  rv$WeightOut_list <- NULL
  rv$TagOut_list <- NULL
  rv$TagRepOut_list <- NULL
  rv$TagTempOut_list <- NULL
  rv$AgeOut_list <- NULL
  rv$IndepOut_list <- NULL
  rv$Info_list <- NULL
  rv$JitterPars_list <- NULL
  rv$JitterInfos_list <- NULL
  rv$model_choice_labels <- NULL
  rv$FISHERY_MAPS <- NULL
  rv$INDEX_FISHERIES_MAPS <- NULL
  rv$YearRanges <- NULL
  rv$fishery_names_dfs <- NULL
  rv$tag_rep_map_dfs <- NULL
  rv$fishery_map_missing_models <- NULL
}

filters_equal <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  isTRUE(all.equal(normalize_filter_state(a), normalize_filter_state(b), check.attributes = FALSE))
}

normalize_filter_state <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.list(x)) {
    out <- lapply(x, normalize_filter_state)
    nms <- names(out)
    if (!is.null(nms) && length(nms) > 0) {
      out <- out[order(nms)]
    }
    return(out)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.atomic(x) && length(x) > 1 && is.null(names(x))) {
    return(sort(x, na.last = TRUE))
  }

  x
}

set_apply_pending <- function(session, id, pending) {
  session$sendCustomMessage(
    "setApplyPending",
    list(id = id, pending = isTRUE(pending))
  )
}

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Reactive Values Storage
  # ---------------------------------------------------------------------------
  rv <- reactiveValues(
    data_loaded = FALSE,              # Flag: data successfully loaded
    initial_render_pending = FALSE,   # Flag: run one initial full render after load
    initial_render_nonce = 0,         # Counter: triggers one full render after inputs are ready
    scenarios_detected = FALSE,       # Flag: scenarios found in directory
    detected_scenario_names = NULL,   # List of detected scenario names
    ParOut_list = NULL,               # List of MFCLPar objects
    RepOut_list = NULL,               # List of MFCLRep objects
    LengOut_list = NULL,              # List of MFCLLenFit objects
    WeightOut_list = NULL,            # List of MFCLWgtFit objects
    TagOut_list = NULL,               # List of MFCLTag objects
    TagRepOut_list = NULL,            # List of MFCLTagRep objects
    TagTempOut_list = NULL,           # List of temporary tag report dataframes
    AgeOut_list = NULL,               # List of MFCLALK objects
    IndepOut_list = NULL,             # List of indepvar.rpt contents
    JitterPars_list = NULL,           # List of jitter .par objects by seed
    JitterInfos_list = NULL,          # List of jitter_info.rds by seed
    FISHERY_MAPS = NULL,              # Fishery name mappings
    INDEX_FISHERIES_MAPS = NULL,      # Index fishery identifiers
    YearRanges = NULL,                # Year ranges for each scenario
    fishery_names_dfs = NULL,         # List of fishery names dataframes (one per model)
    tag_rep_map_dfs = NULL,           # List of tag reporting maps (one per model)
    fishery_map_required = TRUE,      # Require fishery_map.R to enable map-dependent tabs
    fishery_map_missing_models = NULL # Models missing fishery_map.R
  )

  # ---------------------------------------------------------------------------
  # Quick Figure Popup (header dropdown)
  # ---------------------------------------------------------------------------
  quick_fig_dir <- file.path("www", "quick_reference")

  quick_fig_list <- reactive({
    if (!dir.exists(quick_fig_dir)) dir.create(quick_fig_dir, recursive = TRUE)
    list.files(quick_fig_dir, pattern = "\\.(png|jpg|jpeg|pdf)$", ignore.case = TRUE)
  })

  quick_fig_ordered <- reactiveVal(character(0))
  quick_fig_index <- reactiveVal(1L)

  observe({
    fig_files <- quick_fig_list()
    fig_labels <- tools::file_path_sans_ext(fig_files)
    choices <- setNames(fig_files, fig_labels)
    updatePickerInput(session, "quick_fig_select", choices = choices, selected = intersect(isolate(input$quick_fig_select), fig_files))
  })

  observeEvent(input$quick_fig_view_btn, {
    file_sel <- input$quick_fig_select
    if (is.null(file_sel) || length(file_sel) == 0) return()
    fig_files <- quick_fig_list()
    file_sel <- intersect(file_sel, fig_files)
    if (length(file_sel) == 0) return()
    quick_fig_ordered(file_sel)
    quick_fig_index(1L)

    showModal(
      modalDialog(
        title = "Quick Figures",
        uiOutput("quick_fig_viewer"),
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          actionButton("quick_fig_prev", "Previous"),
          actionButton("quick_fig_next", "Next"),
          modalButton("Close")
        )
      )
    )
  }, ignoreInit = TRUE)

  output$quick_fig_viewer <- renderUI({
    file_list <- quick_fig_ordered()
    if (length(file_list) == 0) return(NULL)
    idx <- quick_fig_index()
    if (!is.finite(idx) || idx < 1) idx <- 1L
    if (idx > length(file_list)) idx <- length(file_list)
    file_sel <- file_list[[idx]]
    ext <- tolower(tools::file_ext(file_sel))
    src_path <- file.path("quick_reference", file_sel)

    if (ext == "pdf") {
      tagList(
        tags$div(style = "font-weight: 600; margin-bottom: 6px;",
                 paste0(tools::file_path_sans_ext(basename(file_sel)), " (", idx, "/", length(file_list), ")")),
        tags$iframe(
          src = src_path,
          style = "width:100%; height:80vh; border: none;"
        )
      )
    } else {
      tagList(
        tags$div(style = "font-weight: 600; margin-bottom: 6px;",
                 paste0(tools::file_path_sans_ext(basename(file_sel)), " (", idx, "/", length(file_list), ")")),
        tags$img(src = src_path, style = "width:100%; height:auto;")
      )
    }
  })

  observeEvent(input$quick_fig_prev, {
    idx <- quick_fig_index()
    file_list <- quick_fig_ordered()
    if (length(file_list) == 0) return()
    idx <- idx - 1L
    if (idx < 1L) idx <- length(file_list)
    quick_fig_index(idx)
  }, ignoreInit = TRUE)

  observeEvent(input$quick_fig_next, {
    idx <- quick_fig_index()
    file_list <- quick_fig_ordered()
    if (length(file_list) == 0) return()
    idx <- idx + 1L
    if (idx > length(file_list)) idx <- 1L
    quick_fig_index(idx)
  }, ignoreInit = TRUE)

  observeEvent(input$quick_fig_refresh, {
    fig_files <- quick_fig_list()
    fig_labels <- tools::file_path_sans_ext(fig_files)
    choices <- setNames(fig_files, fig_labels)
    updatePickerInput(session, "quick_fig_select", choices = choices, selected = intersect(isolate(input$quick_fig_select), fig_files))
  }, ignoreInit = TRUE)

  session$allowReconnect(FALSE)
  session$onSessionEnded(function() {
    reset_loaded_data_state(rv)
  })

  server_dir_detection(input, output, session, rv)
  server_data_load(input, output, session, rv)
  mod_summary_server(input, output, session, rv)
  mod_fishery_names_server(input, output, session, rv)
  mod_bounds_server(input, output, session, rv)
  mod_cpue_server(input, output, session, rv)
  mod_lf_server(input, output, session, rv)
  mod_wf_server(input, output, session, rv)
  mod_likelihood_server(input, output, session, rv)
  mod_harvest_server(input, output, session, rv)
  mod_selftest_server(input, output, session, rv)
  mod_tagging_server(input, output, session, rv)
  mod_fishery_process_server(input, output, session, rv)
  mod_population_biology_server(input, output, session, rv)
  server_nav(input, output, session, rv)

  # Keep per-tab model pickers aligned with the global "Filter Models" selection.
  observeEvent(list(rv$data_loaded, input$scenarios), {
    req(rv$data_loaded)
    selected_models <- input$scenarios
    if (is.null(selected_models)) selected_models <- character(0)

    loaded_models <- names(rv$ParOut_list)
    selected_models <- intersect(selected_models, loaded_models)

    cpue_models <- intersect(selected_models, names(rv$RepOut_list))
    lf_models <- intersect(selected_models, names(rv$LengOut_list)[!vapply(rv$LengOut_list, is.null, logical(1))])
    wf_models <- intersect(selected_models, names(rv$WeightOut_list)[!vapply(rv$WeightOut_list, is.null, logical(1))])

    picker_ids_all <- c(
      "lik_scenarios",
      "harvest_scenarios",
      "selftest_scenarios",
      "tag_scenarios",
      "fishery_process_scenarios",
      "population_biology_scenarios"
    )
    for (id in picker_ids_all) {
      updatePickerInput(session, id, choices = selected_models, selected = selected_models)
    }

    updatePickerInput(session, "cpue_scenarios", choices = cpue_models, selected = cpue_models)
    updatePickerInput(session, "lf_scenarios", choices = lf_models, selected = lf_models)
    updatePickerInput(session, "wf_scenarios", choices = wf_models, selected = wf_models)

    current_bound <- isolate(input$bound_model)
    bound_selected <- if (!is.null(current_bound) && current_bound %in% selected_models) current_bound else selected_models[1]
    updateSelectInput(session, "bound_model", choices = selected_models, selected = bound_selected)

    current_fishery_names_model <- isolate(input$fishery_names_model)
    fishery_names_selected <- if (!is.null(current_fishery_names_model) && current_fishery_names_model %in% selected_models) current_fishery_names_model else selected_models[1]
    updateSelectInput(session, "fishery_names_model", choices = selected_models, selected = fishery_names_selected)

  }, ignoreInit = TRUE)

  observeEvent(
    list(
      rv$data_loaded,
      input$scenarios
    ),
    {
      req(rv$data_loaded)
      if (!isTRUE(rv$initial_render_pending)) return()

      # Only require that global scenarios are set; hidden tabs stay suspended
      # and render when first opened.
      ready <- length(input$scenarios) > 0

      if (!ready) return()

      rv$initial_render_pending <- FALSE
      rv$initial_render_nonce <- isolate(rv$initial_render_nonce) + 1
      session$onFlushed(function() {
        session$sendCustomMessage("toggleInitialRenderOverlay", FALSE)
      }, once = TRUE)
    },
    ignoreInit = TRUE
  )
}
