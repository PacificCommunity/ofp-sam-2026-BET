source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")
source("R/modules/mod_likelihood.R")
source("R/modules/mod_sections.R")
source("R/server/download_helpers.R")
source("R/server/server_dir_detection.R")
source("R/server/server_data_load.R")
source("R/server/server_nav.R")

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Reactive Values Storage
  # ---------------------------------------------------------------------------
  rv <- reactiveValues(
    data_loaded = FALSE,              # Flag: data successfully loaded
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
      "stock_scenarios",
      "lik_scenarios",
      "harvest_scenarios",
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
}
