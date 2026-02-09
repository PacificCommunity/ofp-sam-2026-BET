source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_stock.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")
source("R/modules/mod_likelihood.R")
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
    AgeOut_list = NULL,               # List of MFCLALK objects
    IndepOut_list = NULL,             # List of indepvar.rpt contents
    FISHERY_MAPS = NULL,              # Fishery name mappings
    INDEX_FISHERIES_MAPS = NULL,      # Index fishery identifiers
    YearRanges = NULL,                # Year ranges for each scenario
    fishery_names_dfs = NULL          # List of fishery names dataframes (one per model)
  )

  server_dir_detection(input, output, session, rv)
  server_data_load(input, output, session, rv)
  mod_summary_server(input, output, session, rv)
  mod_fishery_names_server(input, output, session, rv)
  mod_bounds_server(input, output, session, rv)
  mod_stock_server(input, output, session, rv)
  mod_cpue_server(input, output, session, rv)
  mod_lf_server(input, output, session, rv)
  mod_wf_server(input, output, session, rv)
  mod_likelihood_server(input, output, session, rv)
  server_nav(input, output, session, rv)
}
