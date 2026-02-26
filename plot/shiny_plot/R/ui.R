source("R/modules/mod_summary.R")
source("R/modules/mod_fishery_names.R")
source("R/modules/mod_bounds.R")
source("R/modules/mod_cpue.R")
source("R/modules/mod_lf.R")
source("R/modules/mod_wf.R")
source("R/modules/mod_likelihood.R")
source("R/modules/mod_sections.R")

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
    div(
      style = "margin: 10px 12px 8px 12px; padding: 10px; background: #f7fbff; border: 1px solid #cfe3f2; border-left: 4px solid #3c8dbc; border-radius: 4px; color: #1f2d3d; line-height: 1.35;",
      tags$div(
        style = "margin-bottom: 6px;",
        tags$img(src = "spc-logo.svg", alt = "Pacific Community (SPC) logo", style = "max-width: 100%; height: 34px; display: block;")
      ),
      tags$div("Created by Kyuhan Kim", style = "font-weight: 700; font-size: 12px;"),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "Contact info: ",
        tags$a("kyuhank@spc.int", href = "mailto:kyuhank@spc.int", style = "color: #3c8dbc;")
      ),
      tags$div(
        style = "font-size: 11px; margin-top: 2px;",
        "GitHub: ",
        tags$a("github.com/kyuhank", href = "https://github.com/kyuhank", target = "_blank", style = "color: #3c8dbc;")
      )
    ),
    
    # Global model filter (shown after data is loaded)
    conditionalPanel(
      condition = "output.data_loaded == true",
      h5("🎯 Filter Models", style = "padding-left: 15px; font-weight: bold;"),
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
    ),

    div(style = "display:none;",
        textInput("plot_export_dir", NULL, value = "")
    ),

    shiny::hr(),

    # Navigation menu
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Model Summary", tabName = "summary", icon = NULL),
      menuItem("🏷️ Fishery Names", tabName = "fishery_names", icon = NULL),
      menuItem("⚠️ Bound Hits", tabName = "bounds", icon = NULL),
      menuItem("📈 CPUE Fits", tabName = "cpue", icon = NULL),
      menuItem("📏 Length Frequency", tabName = "lf", icon = NULL),
      menuItem("⚖️ Weight Frequency", tabName = "wf", icon = NULL),
      menuItem("📉 Diagnostics", tabName = "diagnostics", icon = NULL),
      menuItem("⭐ Key Quantities", tabName = "harvest", icon = NULL),
      menuItem("🎣 Tagging Dynamics", tabName = "tagging", icon = NULL),
      menuItem("🚢 Fishery Process", tabName = "fishery_process", icon = NULL),
      menuItem("🧬 Population Biology", tabName = "population_biology", icon = NULL)
    ),
    
    shiny::hr(),
    
    # Data loading section
    h4("📁 Load Model Data", style = "padding-left: 15px; color: #3c8dbc;"),
    
    # Model directory path input
    div(
      style = "margin: 0 15px;",
      textInput("model_dir", "Model Directory:",
                value = normalizePath("..", mustWork = FALSE),
                placeholder = "/path/to/model")
    ),
    
    # Browse + Refresh buttons
    div(
      style = "margin: 0 15px 10px 15px;",
      shinyFiles::shinyDirButton("browse_dir", "Browse...", 
                                 title = "Select Model Directory",
                                 icon = icon("folder-open"),
                                 class = "btn-info btn-sm"),
      actionButton("refresh_dir", "Refresh", 
                   icon = icon("sync"),
                   class = "btn-default btn-sm",
                   style = "margin-left: 8px;")
    ),
    
    helpText("Folder containing model subfolders", 
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
        tags$style(HTML("
          label[for='models_to_load'] {
            color: #000 !important;
          }
        ")),
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
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Body
  # ---------------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Pretty modal styling (used by shinyFiles browse dialog) */
        .modal-header {
          background: #3c8dbc;
          color: #fff;
          border-bottom: 0;
        }
        .modal-header .modal-title {
          font-weight: 600;
          letter-spacing: 0.2px;
        }
        .modal-content {
          border-radius: 8px;
          border: 1px solid #e6e9ef;
          box-shadow: 0 10px 30px rgba(22, 41, 70, 0.15);
        }
        .modal-body {
          background: #f7f9fb;
        }
        .modal-footer {
          border-top: 0;
          background: #f7f9fb;
        }
        .modal-footer .btn {
          border-radius: 6px;
        }
        /* shinyFiles modal prettify */
        .shinyFiles .form-group,
        .shinyfiles .form-group {
          margin-bottom: 10px;
        }
        .shinyFiles .form-control,
        .shinyfiles .form-control {
          border-radius: 6px;
          border: 1px solid #d7dce3;
          box-shadow: none;
        }
        .shinyFiles .btn,
        .shinyfiles .btn {
          border-radius: 6px;
        }
        .shinyFiles select.form-control,
        .shinyfiles select.form-control {
          background-color: #fff;
        }
        .modal-dialog {
          max-width: 860px;
          width: 80%;
        }
        .shinyFiles .well,
        .shinyfiles .well {
          background: #ffffff;
          border: 1px solid #e6e9ef;
          box-shadow: none;
        }
        .shinyFiles label,
        .shinyfiles label {
          font-weight: 600;
          color: #2c3e50;
        }
        .shinyFiles .help-block,
        .shinyfiles .help-block {
          color: #6c7a89;
        }
        .shinyFiles .btn-default,
        .shinyfiles .btn-default {
          background: #ffffff;
          border: 1px solid #d7dce3;
        }
        .shinyFiles .btn-default:hover,
        .shinyfiles .btn-default:hover {
          background: #f2f4f7;
        }
        .shinyFiles .btn-primary,
        .shinyfiles .btn-primary {
          background: #3c8dbc;
          border-color: #367fa9;
        }
        .shinyFiles .btn-primary:hover,
        .shinyfiles .btn-primary:hover {
          background: #367fa9;
        }
        .shinyFiles select,
        .shinyfiles select {
          font-size: 13px;
          padding: 6px 10px;
        }
        .shinyFiles .well .form-group:last-child,
        .shinyfiles .well .form-group:last-child {
          margin-bottom: 0;
        }
      "))
    ),
    
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

        /* Make selected picker items easier to identify (models and other multi-selects) */
        .bootstrap-select .dropdown-menu li.selected > a,
        .bootstrap-select .dropdown-menu li.selected > a:hover,
        .bootstrap-select .dropdown-menu li.selected > a:focus {
          background-color: #d9ecfa !important;
          color: #123a56 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-menu li.selected .text {
          color: #123a56 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-menu li.selected .check-mark {
          color: #1f78b4 !important;
          opacity: 1 !important;
          font-weight: 700;
        }

        .bootstrap-select .dropdown-toggle .filter-option {
          font-weight: 600;
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
      # TAB 4: CPUE FITS
      # -----------------------------------------------------------------------
      mod_cpue_ui(),
      # -----------------------------------------------------------------------
      # TAB 5: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_lf_ui(),
      # -----------------------------------------------------------------------
      # TAB 6: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      mod_wf_ui(),
      # -----------------------------------------------------------------------
      # TAB 7: DIAGNOSTICS
      # -----------------------------------------------------------------------
      mod_likelihood_ui(),
      # -----------------------------------------------------------------------
      # TAB 8: KEY QUANTITIES
      # -----------------------------------------------------------------------
      mod_harvest_ui(),
      # -----------------------------------------------------------------------
      # TAB 9: TAGGING DYNAMICS
      # -----------------------------------------------------------------------
      mod_tagging_ui(),
      # -----------------------------------------------------------------------
      # TAB 10: FISHERY PROCESS DYNAMICS
      # -----------------------------------------------------------------------
      mod_fishery_process_ui(),
      # -----------------------------------------------------------------------
      # TAB 11: POPULATION BIOLOGY
      # -----------------------------------------------------------------------
      mod_population_biology_ui()
    )
  )
)

# =============================================================================
