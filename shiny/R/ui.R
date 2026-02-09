source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_stock.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")

ui <- dashboardPage(
  skin = "blue",
  
  # ---------------------------------------------------------------------------
  # Header
  # ---------------------------------------------------------------------------
  dashboardHeader(title = "MFCL Output Viewer"),
  
  # ---------------------------------------------------------------------------
  # Sidebar
  # ---------------------------------------------------------------------------
  dashboardSidebar(
    
    # Navigation menu
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Model Summary", tabName = "summary", icon = icon("table")),
      menuItem("⚙️ Fishery Names", tabName = "fishery_names", icon = icon("fish")),
      menuItem("⚠️ Bound Hits", tabName = "bounds", icon = icon("exclamation-triangle")),
      menuItem("🐟 Stock Status", tabName = "stock", icon = icon("chart-line")),
      menuItem("📈 CPUE Fits", tabName = "cpue", icon = icon("chart-area")),
      menuItem("📏 Length Frequency", tabName = "lf", icon = icon("ruler-horizontal")),
      menuItem("⚖️ Weight Frequency", tabName = "wf", icon = icon("weight-hanging"))
    ),
    
    shiny::hr(),
    
    # Data loading section
    h4("📁 Load Model Data", style = "padding-left: 15px; color: #3c8dbc;"),
    
    # Model directory path input
    div(
      style = "margin: 0 15px;",
      textInput("model_dir", "Model Directory:",
                value = normalizePath(file.path("..", "model"), mustWork = FALSE),
                placeholder = "/path/to/model")
    ),
    
    # Browse button (Windows only)
    div(
      style = "margin: 0 15px 10px 15px;",
      actionButton("browse_dir", "Browse...", 
                   icon = icon("folder-open"),
                   class = "btn-info btn-sm")
    ),
    
    helpText("Folder containing scenario subfolders", 
             style = "margin: 0 15px 10px 15px; font-size: 11px; color: #777;"),
    
    # Display detected models before loading with dropdown selection
    conditionalPanel(
      condition = "output.scenarios_detected == true",
      wellPanel(
        style = "background-color: #f5f5f5; margin: 10px 15px; padding: 12px;",
        h5("📦 Detected Models:", style = "margin-top: 0; color: #3c8dbc; font-weight: bold;"),
        
        # Summary info
        div(
          style = "background: white; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
          textOutput("detected_models_summary")
        ),
        
        # Searchable dropdown for model selection
        pickerInput(
          "models_to_load",
          label = "Select models to load:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} models selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search models...",
            size = 10,
            dropupAuto = FALSE,
            style = "btn-default"
          )
        ),
        
        tags$small(
          "💡 Tip: Use search box to find specific models",
          style = "color: #666; font-style: italic; display: block; margin-top: 5px;"
        )
      )
    ),
    
    # Load data button
    div(
      style = "margin: 0 50px 10px 15px;",
      actionButton("load_data", "Load Data", 
                   icon = icon("upload"),
                   class = "btn-primary",
                   style = "width: 100%;")
    ),
    
    shiny::hr(),
    
    # Scenario filter (shown after data is loaded)
    conditionalPanel(
      condition = "output.data_loaded == true",
      h5("🎯 Filter Scenarios", style = "padding-left: 15px; font-weight: bold;"),
      pickerInput("scenarios", NULL,
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectAllText = "All",
                    deselectAllText = "None",
                    selectedTextFormat = "count > 2",
                    liveSearch = TRUE
                  ))
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Body
  # ---------------------------------------------------------------------------
  dashboardBody(
    
    # Custom CSS styling
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; font-size: 16px; }
        .small-box { cursor: default; }
        .content-wrapper { background-color: #ecf0f5; }
        .btn-primary { background-color: #3c8dbc; border-color: #367fa9; }
        .btn-primary:hover { background-color: #367fa9; }
        .well { padding: 10px; margin-bottom: 10px; }
        
        /* Fix detected models summary text color */
        #detected_models_summary {
          color: #333 !important;
          font-size: 13px;
          line-height: 1.5;
        }
        
        /* Editable table styling */
        .editable-cell {
          cursor: pointer;
          background-color: #ffffcc;
        }
        .editable-cell:hover {
          background-color: #ffff99;
        }
      "))
    ),
    
    tabItems(
      
      # -----------------------------------------------------------------------
      # TAB 1: MODEL SUMMARY
      # -----------------------------------------------------------------------
      mod_summary_ui(),
      # -----------------------------------------------------------------------
      # TAB 2: FISHERY NAMES EDITOR
      # -----------------------------------------------------------------------
      mod_fishery_names_ui(),
      # -----------------------------------------------------------------------
      # TAB 3: BOUND HITS
      # -----------------------------------------------------------------------
      mod_bounds_ui(),
      # -----------------------------------------------------------------------
      # TAB 4: STOCK STATUS
      # -----------------------------------------------------------------------
      mod_stock_ui(),
      # -----------------------------------------------------------------------
      # TAB 5: CPUE FITS
      # -----------------------------------------------------------------------
      mod_cpue_ui(),
      # -----------------------------------------------------------------------
      # TAB 6: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_lf_ui(),
      # -----------------------------------------------------------------------
      # TAB 7: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_wf_ui()
    )
  )
)

# =============================================================================
