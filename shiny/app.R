# =============================================================================
# MFCL OUTPUT VISUALIZATION SHINY APP
# Author: Kyuhan Kim
# Description: Interactive dashboard for visualizing MFCL stock assessment outputs
# =============================================================================

# Load required packages
library(shiny)              # Web application framework
library(shinydashboard)     # Dashboard layout
library(shinyWidgets)       # Enhanced UI widgets
library(ggplot2)            # Data visualization
library(cowplot)            # Plot arrangement
library(dplyr)              # Data manipulation
library(FLR4MFCL)           # MFCL file reading
library(tidyr)              # Data tidying
library(viridis)            # Color palettes
library(stringr)            # String manipulation
library(purrr)              # Functional programming
library(DT)                 # Interactive tables
library(parallel)           # Parallel processing

# Fix namespace conflicts
hr <- shiny::hr              # Restore shiny's hr() function
renderDataTable <- DT::renderDataTable  # Use DT's renderDataTable
dataTableOutput <- DT::dataTableOutput  # Use DT's dataTableOutput

# Load helper functions
source("helpers.R")

# =============================================================================
# USER INTERFACE
# =============================================================================

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
    
    hr(),
    
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
    
    hr(),
    
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
      tabItem(
        tabName = "summary",
        h2("Model Summary", style = "color: #3c8dbc;"),
        
        # Overall summary value boxes
        fluidRow(
          valueBoxOutput("n_models", width = 4),
          valueBoxOutput("total_scenarios", width = 4),
          valueBoxOutput("overall_year_range", width = 4)
        ),
        
        # Model-specific information boxes
        fluidRow(
          box(
            title = "Model-Specific Information",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            uiOutput("model_info_boxes")
          )
        ),
        
        # Model configuration table
        fluidRow(
          box(
            title = "Detailed Model Configuration",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            DTOutput("summary_table")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 2: FISHERY NAMES EDITOR
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "fishery_names",
        h2("Fishery Names Manager", style = "color: #17a2b8;"),
        
        fluidRow(
          box(
            title = "Instructions",
            width = 12,
            status = "info",
            collapsible = TRUE,
            collapsed = FALSE,
            HTML("<ul>
              <li>🎯 <strong>Select Model:</strong> Choose which model's fishery names to edit</li>
              <li>📝 <strong>Edit names:</strong> Click on any cell in the 'Fishery_Name' column to edit</li>
              <li>💾 <strong>Save changes:</strong> Click 'Apply Changes' to update the selected model</li>
              <li>🔄 <strong>Apply to All:</strong> Copy current model's names to all other models</li>
              <li>📥 <strong>Export/Import:</strong> Download as CSV, edit in Excel, then upload back</li>
              <li>↩️ <strong>Reset:</strong> Restore default names from helpers.R</li>
            </ul>")
          )
        ),
        
        fluidRow(
          box(
            title = "Fishery Names Table",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            
            # Model selector
            fluidRow(
              column(4,
                     selectInput("fishery_names_model", "Select Model:",
                                 choices = NULL,
                                 selected = NULL)
              ),
              column(8,
                     div(
                       style = "padding-top: 25px;",
                       tags$span(
                         id = "fishery_count_display",
                         style = "font-size: 14px; color: #666; font-weight: bold;",
                         textOutput("fishery_count_text", inline = TRUE)
                       )
                     )
              )
            ),
            
            hr(),
            
            # Action buttons
            div(
              style = "margin-bottom: 15px;",
              actionButton("apply_fishery_names", "💾 Apply Changes to This Model", 
                           class = "btn-success", icon = icon("check")),
              actionButton("apply_to_all_models", "🔄 Apply to All Models", 
                           class = "btn-warning", icon = icon("copy")),
              actionButton("reset_fishery_names", "↩️ Reset to Default", 
                           class = "btn-danger", icon = icon("undo")),
              downloadButton("download_fishery_names", "📥 Download CSV", 
                             class = "btn-info"),
              tags$div(
                style = "display: inline-block; margin-left: 10px;",
                fileInput("upload_fishery_names", "📤 Upload CSV", 
                          accept = ".csv",
                          buttonLabel = "Browse...",
                          width = "250px")
              )
            ),
            
            # Editable table
            DTOutput("fishery_names_table")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 3: BOUND HITS
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "bounds",
        h2("Parameter Bound Hit Analysis", style = "color: #f39c12;"),
        
        # Overview table
        fluidRow(
          box(
            title = "Overview",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            DTOutput("bounds_overview")
          )
        ),
        
        # Detailed bound hits table
        fluidRow(
          box(
            title = "Detailed Bound Hits",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            collapsible = TRUE,
            selectInput("bound_model", "Select Model:", choices = NULL),
            DTOutput("bounds_detail"),
            downloadButton("download_bounds", "Download CSV", class = "btn-info")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 4: STOCK STATUS
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "stock",
        h2("Stock Status", style = "color: #00a65a;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "success",
            
            # Scenarios selector with dropdown
            pickerInput(
              "stock_scenarios",
              "Scenarios:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} scenarios selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search scenarios...",
                size = 10
              )
            ),
            
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_stock_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download")),
            helpText("Select scenarios to display", style = "margin-top: 10px;")
          ),
          
          # Stock status plots
          box(
            title = "Spawning Biomass Depletion & Recruitment",
            width = 9,
            solidHeader = TRUE,
            status = "success",
            collapsible = TRUE,
            plotOutput("stock_plot", height = "600px")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 5: CPUE FITS
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "cpue",
        h2("CPUE Fits", style = "color: #00c0ef;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "info",
            
            # Scenarios selector with dropdown
            pickerInput(
              "cpue_scenarios",
              "Scenarios:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} scenarios selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search scenarios...",
                size = 10
              )
            ),
            
            # Fisheries selector with dropdown
            pickerInput(
              "cpue_fisheries",
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
                liveSearchPlaceholder = "Search fisheries...",
                size = 10
              )
            ),
            
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_cpue_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download")),
            helpText("Select scenarios and fisheries to display", style = "margin-top: 10px;")
          ),
          
          # CPUE plot panel
          box(
            title = "CPUE Observed vs Predicted",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput("cpue_plot", height = "600px")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 6: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "lf",
        h2("Length Frequency Fits", style = "color: #605ca8;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            
            # Model selector (single selection)
            selectInput(
              "lf_model",
              "Model:",
              choices = NULL,
              selected = NULL
            ),
            
            # Fishery selector with navigation buttons
            div(
              style = "margin-bottom: 15px;",
              tags$label("Fishery:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
              div(
                style = "display: flex; align-items: center; gap: 5px;",
                actionButton("lf_prev", "", icon = icon("chevron-left"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;"),
                div(
                  style = "flex: 1;",
                  selectInput("lf_fishery", NULL, choices = NULL)
                ),
                actionButton("lf_next", "", icon = icon("chevron-right"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;")
              )
            ),
            
            # Year selector with Select All / Deselect All
            pickerInput(
              "lf_years",
              "Years:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "All Years",
                deselectAllText = "None",
                selectedTextFormat = "count > 5",
                countSelectedText = "{0} years selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search years...",
                size = 10
              )
            ),
            
            hr(),
            
            # Scenarios selector for overlay (only compatible models)
            pickerInput(
              "lf_scenarios",
              "Overlay Scenarios:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} scenarios selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search scenarios...",
                size = 10
              )
            ),
            
            helpText("💡 Only models with identical fishery structure and names can be overlaid", 
                     style = "font-size: 11px; color: #666; font-style: italic;"),
            
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_lf_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download"))
          ),
          
          # Length frequency plot panel (DYNAMIC HEIGHT)
          uiOutput("lf_plot_box")
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 7: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "wf",
        h2("Weight Frequency Fits", style = "color: #dd4b39;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            
            # Model selector (single selection)
            selectInput(
              "wf_model",
              "Model:",
              choices = NULL,
              selected = NULL
            ),
            
            # Fishery selector with navigation buttons
            div(
              style = "margin-bottom: 15px;",
              tags$label("Fishery:", style = "font-weight: bold; margin-bottom: 5px; display: block;"),
              div(
                style = "display: flex; align-items: center; gap: 5px;",
                actionButton("wf_prev", "", icon = icon("chevron-left"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;"),
                div(
                  style = "flex: 1;",
                  selectInput("wf_fishery", NULL, choices = NULL)
                ),
                actionButton("wf_next", "", icon = icon("chevron-right"), 
                             class = "btn-sm btn-default",
                             style = "padding: 5px 10px;")
              )
            ),
            
            # Year selector with Select All / Deselect All
            pickerInput(
              "wf_years",
              "Years:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "All Years",
                deselectAllText = "None",
                selectedTextFormat = "count > 5",
                countSelectedText = "{0} years selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search years...",
                size = 10
              )
            ),
            
            hr(),
            
            # Scenarios selector for overlay (only compatible models)
            pickerInput(
              "wf_scenarios",
              "Overlay Scenarios:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} scenarios selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search scenarios...",
                size = 10
              )
            ),
            
            helpText("💡 Only models with identical fishery structure and names can be overlaid", 
                     style = "font-size: 11px; color: #666; font-style: italic;"),
            
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_wf_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download"))
          ),
          
          # Weight frequency plot panel (DYNAMIC HEIGHT)
          uiOutput("wf_plot_box")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

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
    IndepOut_list = NULL,             # List of indepvar.rpt contents
    FISHERY_MAPS = NULL,              # Fishery name mappings
    INDEX_FISHERIES_MAPS = NULL,      # Index fishery identifiers
    YearRanges = NULL,                # Year ranges for each scenario
    fishery_names_dfs = NULL          # List of fishery names dataframes (one per model)
  )
  
  # ---------------------------------------------------------------------------
  # DIRECTORY DETECTION
  # ---------------------------------------------------------------------------
  
  # Automatically detect scenarios when directory path changes
  observe({
    model_dir <- input$model_dir
    
    # Check if directory exists
    if (nchar(model_dir) > 0 && dir.exists(model_dir)) {
      
      # Get all subdirectories (potential scenarios)
      scenario_folders <- list.dirs(model_dir, full.names = FALSE, recursive = FALSE)
      
      # Filter out hidden folders and common non-model folders
      scenario_folders <- scenario_folders[
        !grepl("^\\.|^__", scenario_folders) & 
          !scenario_folders %in% c("archive", "old", "backup", "test")
      ]
      
      # Update reactive values
      if (length(scenario_folders) > 0) {
        rv$scenarios_detected <- TRUE
        rv$detected_scenario_names <- scenario_folders
        
        # Update picker choices
        updatePickerInput(
          session, 
          "models_to_load",
          choices = scenario_folders,
          selected = scenario_folders  # All selected by default
        )
      } else {
        rv$scenarios_detected <- FALSE
        rv$detected_scenario_names <- NULL
        updatePickerInput(session, "models_to_load", choices = NULL)
      }
    } else {
      rv$scenarios_detected <- FALSE
      rv$detected_scenario_names <- NULL
      updatePickerInput(session, "models_to_load", choices = NULL)
    }
  })
  
  # Output: scenarios detected flag
  output$scenarios_detected <- reactive({ rv$scenarios_detected })
  outputOptions(output, "scenarios_detected", suspendWhenHidden = FALSE)
  
  # Display detected models summary
  output$detected_models_summary <- renderText({
    req(rv$detected_scenario_names)
    n_detected <- length(rv$detected_scenario_names)
    
    # Show first few model names as preview
    if (n_detected <= 5) {
      preview <- paste(rv$detected_scenario_names, collapse = ", ")
    } else {
      preview <- paste(
        paste(head(rv$detected_scenario_names, 3), collapse = ", "),
        "... and",
        n_detected - 3,
        "more"
      )
    }
    
    paste0("✓ Found ", n_detected, " model(s): ", preview)
  })
  
  # ---------------------------------------------------------------------------
  # BROWSE DIRECTORY BUTTON
  # ---------------------------------------------------------------------------
  
  observeEvent(input$browse_dir, {
    
    # Windows: use choose.dir() for directory dialog
    if (.Platform$OS.type == "windows") {
      tryCatch({
        selected <- choose.dir(default = input$model_dir, 
                               caption = "Select Model Directory")
        if (!is.na(selected) && !is.null(selected)) {
          updateTextInput(session, "model_dir", value = selected)
        }
      }, error = function(e) {
        showNotification("Please enter path manually", type = "message")
      })
    } else {
      # Mac/Linux: show instruction modal
      showModal(modalDialog(
        title = "📁 Select Directory",
        HTML(paste0(
          "<p>Please enter the path manually in the text box above.</p>",
          "<p><strong>Current path:</strong><br/>",
          "<code>", input$model_dir, "</code></p>",
          "<hr/>",
          "<p><strong>Examples:</strong></p>",
          "<ul>",
          "<li>Mac: <code>/Users/username/Documents/model</code></li>",
          "<li>Linux: <code>/home/username/model</code></li>",
          "<li>Windows: <code>C:/Users/username/model</code></li>",
          "</ul>"
        )),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  # ---------------------------------------------------------------------------
  # DATA LOADING (PARALLELIZED)
  # ---------------------------------------------------------------------------
  
  observeEvent(input$load_data, {
    
    req(input$model_dir, input$models_to_load)
    
    # Check if any models selected
    if (length(input$models_to_load) == 0) {
      showNotification("Please select at least one model to load!", 
                       type = "warning", duration = 5)
      return(NULL)
    }
    
    MODEL_DIR <- input$model_dir
    
    # Validate directory exists
    if (!dir.exists(MODEL_DIR)) {
      showNotification("Model directory not found!", type = "error", duration = 5)
      return(NULL)
    }
    
    # Show progress bar
    withProgress(message = "Loading model data...", value = 0, {
      
      # Get only selected scenario folders
      model_names <- input$models_to_load
      model_folders <- file.path(MODEL_DIR, model_names)
      
      # Validate all selected folders exist
      existing_idx <- dir.exists(model_folders)
      if (!all(existing_idx)) {
        missing <- model_names[!existing_idx]
        showNotification(
          paste("Some selected models not found:", paste(missing, collapse = ", ")),
          type = "warning", duration = 5
        )
      }
      
      # Keep only existing folders
      model_folders <- model_folders[existing_idx]
      model_names <- model_names[existing_idx]
      
      # Check if any scenarios found
      if (length(model_folders) == 0) {
        showNotification("No valid scenario folders found!", type = "error", duration = 5)
        return(NULL)
      }
      
      incProgress(0.1, detail = paste("Loading", length(model_folders), "scenarios"))
      
      # =======================================================================
      # PARALLEL LOADING
      # =======================================================================
      
      # Determine optimal number of cores (total cores - 2, minimum 1)
      n_cores <- parallel::detectCores()
      n_cores <- max(1, min(n_cores - 2, length(model_folders)))
      
      # Show parallel info
      showNotification(
        paste0("🚀 Using ", n_cores, " parallel worker", 
               if(n_cores > 1) "s" else "", " for faster loading"),
        type = "message",
        duration = 3
      )
      
      incProgress(0.15, detail = paste("Initializing", n_cores, "parallel workers..."))
      
      # Create cluster
      cl <- makeCluster(n_cores)
      
      # Ensure cluster cleanup on exit
      on.exit({
        tryCatch(stopCluster(cl), error = function(e) NULL)
      }, add = TRUE)
      
      # Export necessary packages to workers
      clusterEvalQ(cl, {
        library(FLR4MFCL)
        library(purrr)
      })
      
      # Export helper functions and variables
      clusterExport(cl, 
                    c("finalPar", "finalRep", "safe_read", 
                      "model_folders", "model_names"), 
                    envir = environment())
      
      # Load scenarios in parallel
      results <- tryCatch({
        parLapply(cl, seq_along(model_folders), function(i) {
          folder <- model_folders[i]
          scenario_name <- model_names[i]
          
          tryCatch({
            # Check if required files exist
            par_file <- finalPar(folder)
            rep_file <- finalRep(folder)
            
            # Validate .par file
            if (!file.exists(par_file)) {
              return(list(
                name = scenario_name,
                error = paste("Missing .par file in", scenario_name),
                data = NULL
              ))
            }
            
            # Validate plot.rep file
            if (!file.exists(rep_file)) {
              return(list(
                name = scenario_name,
                error = paste("Missing plot.rep file in", scenario_name),
                data = NULL
              ))
            }
            
            # Read model output files
            data <- list(
              ParOut = read.MFCLPar(par_file),
              RepOut = read.MFCLRep(rep_file),
              LengOut = tryCatch({
                lf_file <- file.path(folder, "length.fit")
                if (file.exists(lf_file)) read.MFCLLenFit(lf_file) else NULL
              }, error = function(e) NULL),
              WeightOut = tryCatch({
                wf_file <- file.path(folder, "weight.fit")
                if (file.exists(wf_file)) read.MFCLWgtFit(wf_file) else NULL
              }, error = function(e) NULL),
              IndepOut = safe_read(file.path(folder, "indepvar.rpt"))
            )
            
            list(name = scenario_name, error = NULL, data = data)
            
          }, error = function(e) {
            list(
              name = scenario_name,
              error = paste("Error loading", scenario_name, ":", e$message),
              data = NULL
            )
          })
        })
      }, error = function(e) {
        showNotification(
          paste("Parallel loading error:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
      
      # Stop cluster
      stopCluster(cl)
      
      # Check if loading succeeded
      if (is.null(results)) {
        return(NULL)
      }
      
      incProgress(0.7, detail = "Processing loaded data...")
      
      # Process results
      errors <- Filter(function(x) !is.null(x$error), results)
      successes <- Filter(function(x) is.null(x$error), results)
      
      # Show errors if any
      if (length(errors) > 0) {
        for (err in errors) {
          showNotification(err$error, type = "warning", duration = 3)
        }
      }
      
      # Check if any scenarios loaded successfully
      if (length(successes) == 0) {
        showNotification("Failed to load any scenarios!", type = "error", duration = 5)
        return(NULL)
      }
      
      # Convert to named list
      results_named <- setNames(
        lapply(successes, function(x) x$data),
        sapply(successes, function(x) x$name)
      )
      
      incProgress(0.8, detail = "Creating fishery mappings...")
      
      # Extract data into separate lists
      rv$ParOut_list <- map(results_named, "ParOut")
      rv$RepOut_list <- map(results_named, "RepOut")
      rv$LengOut_list <- map(results_named, "LengOut")
      rv$WeightOut_list <- map(results_named, "WeightOut")
      rv$IndepOut_list <- map(results_named, "IndepOut")
      
      # Create fishery name mappings for each scenario
      rv$FISHERY_MAPS <- lapply(names(results_named), function(sc) {
        create_fishery_map(rv$ParOut_list[[sc]], GLOBAL_FISHERY_NAMES)
      })
      names(rv$FISHERY_MAPS) <- names(results_named)
      
      # Detect index fisheries (fisheries ending with 'i' or containing 'index')
      rv$INDEX_FISHERIES_MAPS <- lapply(rv$FISHERY_MAPS, detect_index_fisheries)
      
      # Extract year ranges for each scenario
      rv$YearRanges <- map(rv$ParOut_list, function(par) {
        list(minYear = par@range["minyear"], maxYear = par@range["maxyear"])
      })
      
      # Initialize fishery names dataframes (one per model)
      rv$fishery_names_dfs <- lapply(names(rv$FISHERY_MAPS), function(model_name) {
        fishery_map <- rv$FISHERY_MAPS[[model_name]]
        data.frame(
          Fishery_ID = names(fishery_map),
          Fishery_Name = as.character(fishery_map),
          stringsAsFactors = FALSE
        )
      })
      names(rv$fishery_names_dfs) <- names(rv$FISHERY_MAPS)
      
      incProgress(0.95, detail = "Finalizing...")
      
      # Set data loaded flag
      rv$data_loaded <- TRUE
      
      # Update UI components with loaded data
      updatePickerInput(session, "scenarios", 
                        choices = names(results_named),
                        selected = names(results_named))
      
      updateSelectInput(session, "bound_model", choices = names(results_named))
      
      # Update scenario pickers for all tabs (select all by default)
      updatePickerInput(session, "stock_scenarios", 
                        choices = names(results_named), 
                        selected = names(results_named))
      updatePickerInput(session, "cpue_scenarios", 
                        choices = names(results_named), 
                        selected = names(results_named))
      
      # Update model selectors for LF/WF tabs (single selection)
      updateSelectInput(session, "lf_model", 
                        choices = names(results_named),
                        selected = names(results_named)[1])
      updateSelectInput(session, "wf_model", 
                        choices = names(results_named),
                        selected = names(results_named)[1])
      
      # Update fishery names model selector
      updateSelectInput(session, "fishery_names_model",
                        choices = names(results_named),
                        selected = names(results_named)[1])
      
      incProgress(1)
      
      # Display success message with timing info
      showNotification(
        HTML(paste0(
          "<strong>✓ Successfully loaded (parallel mode)!</strong><br/>",
          "Directory: ", basename(MODEL_DIR), "<br/>",
          "Scenarios: ", length(results_named), "<br/>",
          "Workers: ", n_cores, " parallel core", if(n_cores > 1) "s" else "", "<br/>",
          "Names: ", paste(names(results_named), collapse = ", ")
        )), 
        type = "message", 
        duration = 10
      )
    })
  })
  
  # Output: data loaded flag
  output$data_loaded <- reactive({ rv$data_loaded })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # ===========================================================================
  # TAB 1: MODEL SUMMARY
  # ===========================================================================
  
  # Render model summary table
  output$summary_table <- renderDT({
    req(rv$data_loaded, input$scenarios)
    
    # Extract parameters from each selected scenario
    params_df <- imap_dfr(rv$ParOut_list[input$scenarios], function(par, model_name) {
      dims <- as.list(par@dimensions)
      year_range <- rv$YearRanges[[model_name]]
      data.frame(
        Model = model_name,
        Max_Grad = sprintf("%.6f", as.numeric(par@max_grad)),
        Obj_Fun = sprintf("%.2f", as.numeric(par@obj_fun)),
        N_Pars = as.numeric(par@n_pars),
        Fisheries = dims$fisheries,
        Years = paste(year_range$minYear, "-", year_range$maxYear),
        Regions = dims$regions,
        Seasons = dims$seasons
      )
    })
    
    # Display as interactive table
    datatable(params_df, 
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
              rownames = FALSE)
  })
  
  # Value box: number of models selected
  output$n_models <- renderValueBox({
    req(rv$data_loaded)
    valueBox(
      length(input$scenarios), "Models Selected", 
      icon = icon("check-square"),
      color = "blue"
    )
  })
  
  # Value box: total scenarios loaded
  output$total_scenarios <- renderValueBox({
    req(rv$data_loaded)
    valueBox(
      length(rv$ParOut_list), "Total Models Loaded", 
      icon = icon("cube"),
      color = "green"
    )
  })
  
  # Value box: overall year range
  output$overall_year_range <- renderValueBox({
    req(rv$data_loaded)
    all_years <- range(unlist(lapply(rv$YearRanges[input$scenarios], 
                                     function(x) c(x$minYear, x$maxYear))))
    valueBox(
      paste(all_years[1], "-", all_years[2]), "Overall Year Range", 
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # Render model-specific info boxes
  output$model_info_boxes <- renderUI({
    req(rv$data_loaded, input$scenarios)
    
    # Create a box for each selected model
    boxes <- lapply(input$scenarios, function(model_name) {
      par <- rv$ParOut_list[[model_name]]
      dims <- as.list(par@dimensions)
      year_range <- rv$YearRanges[[model_name]]
      
      # Count index fisheries
      n_index <- length(rv$INDEX_FISHERIES_MAPS[[model_name]])
      
      # Create info box
      column(
        width = 4,
        box(
          title = model_name,
          width = NULL,
          status = "primary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          tags$div(
            style = "padding: 5px;",
            tags$table(
              style = "width: 100%; font-size: 13px;",
              tags$tr(
                tags$td(tags$strong("🎣 Fisheries:"), style = "width: 60%;"),
                tags$td(dims$fisheries, style = "text-align: right;")
              ),
              tags$tr(
                tags$td(tags$strong("📊 Index Fisheries:"), style = "padding-top: 5px;"),
                tags$td(n_index, style = "text-align: right; padding-top: 5px;")
              ),
              tags$tr(
                tags$td(tags$strong("📅 Years:"), style = "padding-top: 5px;"),
                tags$td(
                  paste(year_range$minYear, "-", year_range$maxYear),
                  style = "text-align: right; padding-top: 5px;"
                )
              ),
              tags$tr(
                tags$td(tags$strong("🗺️ Regions:"), style = "padding-top: 5px;"),
                tags$td(dims$regions, style = "text-align: right; padding-top: 5px;")
              ),
              tags$tr(
                tags$td(tags$strong("📆 Seasons:"), style = "padding-top: 5px;"),
                tags$td(dims$seasons, style = "text-align: right; padding-top: 5px;")
              ),
              tags$tr(
                tags$td(tags$strong("📈 Parameters:"), style = "padding-top: 5px;"),
                tags$td(par@n_pars, style = "text-align: right; padding-top: 5px;")
              ),
              tags$tr(
                tags$td(tags$strong("🎯 Max Gradient:"), style = "padding-top: 5px;"),
                tags$td(
                  sprintf("%.2e", as.numeric(par@max_grad)),
                  style = "text-align: right; padding-top: 5px;"
                )
              ),
              tags$tr(
                tags$td(tags$strong("💰 Obj Function:"), style = "padding-top: 5px;"),
                tags$td(
                  sprintf("%.2f", as.numeric(par@obj_fun)),
                  style = "text-align: right; padding-top: 5px;"
                )
              )
            )
          )
        )
      )
    })
    
    # Arrange boxes in rows of 3
    do.call(fluidRow, boxes)
  })
  
  # ===========================================================================
  # TAB 2: FISHERY NAMES EDITOR
  # ===========================================================================
  
  # Display fishery count for selected model
  output$fishery_count_text <- renderText({
    req(input$fishery_names_model, rv$fishery_names_dfs)
    n_fisheries <- nrow(rv$fishery_names_dfs[[input$fishery_names_model]])
    paste0("📊 Total fisheries in this model: ", n_fisheries)
  })
  
  # Render editable fishery names table for selected model
  output$fishery_names_table <- renderDT({
    req(rv$data_loaded, input$fishery_names_model, rv$fishery_names_dfs)
    
    df <- rv$fishery_names_dfs[[input$fishery_names_model]]
    
    datatable(
      df,
      editable = list(target = "cell", disable = list(columns = 0)),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(className = 'dt-center', targets = 0),
          list(className = 'editable-cell', targets = 1)
        )
      ),
      rownames = FALSE
    )
  })
  
  # Handle cell edits
  observeEvent(input$fishery_names_table_cell_edit, {
    req(input$fishery_names_model)
    
    info <- input$fishery_names_table_cell_edit
    rv$fishery_names_dfs[[input$fishery_names_model]][info$row, info$col + 1] <- info$value
  })
  
  # Apply fishery name changes to selected model only
  observeEvent(input$apply_fishery_names, {
    req(input$fishery_names_model, rv$fishery_names_dfs)
    
    model_name <- input$fishery_names_model
    df <- rv$fishery_names_dfs[[model_name]]
    
    # Create custom mapping for this model
    custom_mapping <- setNames(df$Fishery_Name, df$Fishery_ID)
    
    # Update FISHERY_MAPS for this model only
    for (fid in names(custom_mapping)) {
      if (fid %in% names(rv$FISHERY_MAPS[[model_name]])) {
        rv$FISHERY_MAPS[[model_name]][fid] <- custom_mapping[fid]
      }
    }
    
    # Update UI elements in other tabs
    
    # 1. Update CPUE fisheries dropdown (if this model is selected)
    if (model_name %in% input$cpue_scenarios) {
      all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
      
      if (length(all_index_fish) > 0) {
        fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
        choices <- setNames(all_index_fish, 
                            sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
        
        current_selection <- isolate(input$cpue_fisheries)
        updatePickerInput(session, "cpue_fisheries", 
                          choices = choices,
                          selected = current_selection)
      }
    }
    
    # 2. Update Length Frequency fishery dropdown (if this model is selected)
    if (!is.null(input$lf_model) && input$lf_model == model_name) {
      if (!is.null(rv$LengOut_list[[model_name]])) {
        fisheries <- unique(rv$LengOut_list[[model_name]]@lenfits$fishery)
        
        if (length(fisheries) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[model_name]]
          choices <- setNames(fisheries, 
                              sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
          
          current_selection <- isolate(input$lf_fishery)
          if (!is.null(current_selection) && current_selection %in% fisheries) {
            selected <- current_selection
          } else {
            selected <- fisheries[1]
          }
          
          updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
        }
      }
      
      # Re-check compatibility after name change
      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      compatible_models <- check_lf_compatibility(input$lf_model, all_models)
      
      updatePickerInput(session, "lf_scenarios",
                        choices = compatible_models,
                        selected = intersect(isolate(input$lf_scenarios), compatible_models))
    }
    
    # 3. Update Weight Frequency fishery dropdown (if this model is selected)
    if (!is.null(input$wf_model) && input$wf_model == model_name) {
      if (!is.null(rv$WeightOut_list[[model_name]])) {
        fisheries <- unique(rv$WeightOut_list[[model_name]]@wgtfits$fishery)
        
        if (length(fisheries) > 0) {
          fishery_map <- rv$FISHERY_MAPS[[model_name]]
          choices <- setNames(fisheries, 
                              sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
          
          current_selection <- isolate(input$wf_fishery)
          if (!is.null(current_selection) && current_selection %in% fisheries) {
            selected <- current_selection
          } else {
            selected <- fisheries[1]
          }
          
          updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
        }
      }
      
      # Re-check compatibility after name change
      all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
      compatible_models <- check_wf_compatibility(input$wf_model, all_models)
      
      updatePickerInput(session, "wf_scenarios",
                        choices = compatible_models,
                        selected = intersect(isolate(input$wf_scenarios), compatible_models))
    }
    
    showNotification(
      HTML(paste0(
        "✓ Fishery names updated for model: <strong>", model_name, "</strong><br/>",
        "📊 UI elements refreshed in all tabs"
      )),
      type = "message",
      duration = 4
    )
  })
  
  # Apply current model's fishery names to all models
  observeEvent(input$apply_to_all_models, {
    req(input$fishery_names_model, rv$fishery_names_dfs)
    
    source_model <- input$fishery_names_model
    source_df <- rv$fishery_names_dfs[[source_model]]
    source_mapping <- setNames(source_df$Fishery_Name, source_df$Fishery_ID)
    
    # Count how many models will be updated
    n_updated <- 0
    
    # Apply to all models
    for (model_name in names(rv$FISHERY_MAPS)) {
      updated <- FALSE
      for (fid in names(source_mapping)) {
        if (fid %in% names(rv$FISHERY_MAPS[[model_name]])) {
          rv$FISHERY_MAPS[[model_name]][fid] <- source_mapping[fid]
          
          # Also update the dataframe
          idx <- which(rv$fishery_names_dfs[[model_name]]$Fishery_ID == fid)
          if (length(idx) > 0) {
            rv$fishery_names_dfs[[model_name]][idx, "Fishery_Name"] <- source_mapping[fid]
          }
          updated <- TRUE
        }
      }
      if (updated) n_updated <- n_updated + 1
    }
    
    # Update all UI elements
    
    # 1. Update CPUE fisheries dropdown
    if (length(input$cpue_scenarios) > 0) {
      all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
      
      if (length(all_index_fish) > 0) {
        fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
        choices <- setNames(all_index_fish, 
                            sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
        
        current_selection <- isolate(input$cpue_fisheries)
        updatePickerInput(session, "cpue_fisheries", 
                          choices = choices,
                          selected = current_selection)
      }
    }
    
    # 2. Update Length Frequency fishery dropdown
    if (!is.null(input$lf_model) && !is.null(rv$LengOut_list[[input$lf_model]])) {
      fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
      
      if (length(fisheries) > 0) {
        fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
        choices <- setNames(fisheries, 
                            sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
        
        current_selection <- isolate(input$lf_fishery)
        if (!is.null(current_selection) && current_selection %in% fisheries) {
          selected <- current_selection
        } else {
          selected <- fisheries[1]
        }
        
        updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
      }
      
      # Re-check compatibility
      all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
      compatible_models <- check_lf_compatibility(input$lf_model, all_models)
      
      updatePickerInput(session, "lf_scenarios",
                        choices = compatible_models,
                        selected = intersect(isolate(input$lf_scenarios), compatible_models))
    }
    
    # 3. Update Weight Frequency fishery dropdown
    if (!is.null(input$wf_model) && !is.null(rv$WeightOut_list[[input$wf_model]])) {
      fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
      
      if (length(fisheries) > 0) {
        fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
        choices <- setNames(fisheries, 
                            sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
        
        current_selection <- isolate(input$wf_fishery)
        if (!is.null(current_selection) && current_selection %in% fisheries) {
          selected <- current_selection
        } else {
          selected <- fisheries[1]
        }
        
        updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
      }
      
      # Re-check compatibility
      all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
      compatible_models <- check_wf_compatibility(input$wf_model, all_models)
      
      updatePickerInput(session, "wf_scenarios",
                        choices = compatible_models,
                        selected = intersect(isolate(input$wf_scenarios), compatible_models))
    }
    
    showNotification(
      HTML(paste0(
        "<strong>✓ Applied fishery names to all models!</strong><br/>",
        "Source: ", source_model, "<br/>",
        "Updated: ", n_updated, " model(s)<br/>",
        "📊 UI elements refreshed in all tabs"
      )),
      type = "message",
      duration = 5
    )
  })
  
  # Reset fishery names to default for selected model
  observeEvent(input$reset_fishery_names, {
    req(rv$data_loaded, input$fishery_names_model)
    
    model_name <- input$fishery_names_model
    
    # Recreate default mapping for this model
    rv$FISHERY_MAPS[[model_name]] <- create_fishery_map(
      rv$ParOut_list[[model_name]], 
      GLOBAL_FISHERY_NAMES
    )
    
    # Reset dataframe
    fishery_map <- rv$FISHERY_MAPS[[model_name]]
    rv$fishery_names_dfs[[model_name]] <- data.frame(
      Fishery_ID = names(fishery_map),
      Fishery_Name = as.character(fishery_map),
      stringsAsFactors = FALSE
    )
    
    showNotification(
      paste0("✓ Fishery names reset to default for: ", model_name),
      type = "warning",
      duration = 3
    )
  })
  
  # Download fishery names CSV for selected model
  output$download_fishery_names <- downloadHandler(
    filename = function() {
      req(input$fishery_names_model)
      paste0("fishery_names_", input$fishery_names_model, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$fishery_names_model, rv$fishery_names_dfs)
      df <- rv$fishery_names_dfs[[input$fishery_names_model]]
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Upload fishery names CSV for selected model
  observeEvent(input$upload_fishery_names, {
    req(input$upload_fishery_names, input$fishery_names_model)
    
    tryCatch({
      uploaded <- read.csv(input$upload_fishery_names$datapath, stringsAsFactors = FALSE)
      
      # Validate columns
      if (!all(c("Fishery_ID", "Fishery_Name") %in% names(uploaded))) {
        showNotification("❌ CSV must have columns: Fishery_ID, Fishery_Name", 
                         type = "error", duration = 5)
        return()
      }
      
      model_name <- input$fishery_names_model
      
      # Check if Fishery_IDs match the current model
      current_ids <- rv$fishery_names_dfs[[model_name]]$Fishery_ID
      uploaded_ids <- uploaded$Fishery_ID
      
      if (!all(uploaded_ids %in% current_ids)) {
        missing <- setdiff(uploaded_ids, current_ids)
        showNotification(
          HTML(paste0(
            "⚠️ Warning: Some Fishery_IDs not found in current model:<br/>",
            paste(missing, collapse = ", ")
          )),
          type = "warning",
          duration = 5
        )
      }
      
      # Update dataframe (only matching IDs)
      for (i in 1:nrow(uploaded)) {
        fid <- uploaded$Fishery_ID[i]
        fname <- uploaded$Fishery_Name[i]
        
        idx <- which(rv$fishery_names_dfs[[model_name]]$Fishery_ID == fid)
        if (length(idx) > 0) {
          rv$fishery_names_dfs[[model_name]][idx, "Fishery_Name"] <- fname
        }
      }
      
      showNotification(
        paste0("✓ Fishery names uploaded for: ", model_name, 
               ". Click 'Apply Changes' to save."),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("❌ Error uploading CSV:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # ===========================================================================
  # TAB 3: BOUND HITS
  # ===========================================================================
  
  # Reactive: process bound hits data
  bounds_data <- reactive({
    req(rv$data_loaded, input$scenarios)
    
    # Process indepvar.rpt for each scenario
    results <- map(input$scenarios, function(model_name) {
      df <- parse_indepvar(rv$IndepOut_list[[model_name]])
      if (is.null(df)) return(NULL)
      
      # Calculate distances to bounds and identify hit type
      df <- df %>%
        mutate(
          Distance_to_lower = abs(Estimate - L_bound),
          Distance_to_upper = abs(Estimate - U_bound),
          Hit_Type = case_when(
            !Hit_Bound ~ "None",
            Distance_to_lower <= Distance_to_upper ~ "Lower",
            TRUE ~ "Upper"
          )
        )
      
      # Filter to only parameters that hit bounds
      bound_hits <- df %>% filter(Hit_Bound)
      list(total_params = nrow(df), bound_hits = bound_hits)
    })
    names(results) <- input$scenarios
    Filter(Negate(is.null), results)
  })
  
  # Render bound hits overview table
  output$bounds_overview <- renderDT({
    req(bounds_data())
    
    # Create summary table
    overview <- data.frame(
      Model = names(bounds_data()),
      Total_Params = sapply(bounds_data(), function(x) x$total_params),
      Bound_Hits = sapply(bounds_data(), function(x) nrow(x$bound_hits)),
      Hit_Rate = sapply(bounds_data(), function(x) 
        sprintf("%.2f%%", nrow(x$bound_hits) / x$total_params * 100))
    )
    
    datatable(overview, 
              options = list(pageLength = 10, dom = 'tip'), 
              rownames = FALSE)
  })
  
  # Render detailed bound hits table
  output$bounds_detail <- renderDT({
    req(input$bound_model, bounds_data())
    
    # Check if data exists for selected model
    if (!input$bound_model %in% names(bounds_data())) {
      return(data.frame(Message = "No data available for this model"))
    }
    
    bounds <- bounds_data()[[input$bound_model]]$bound_hits
    
    # Display message if no bound hits
    if (nrow(bounds) == 0) {
      data.frame(Message = "✓ No bound hits detected")
    } else {
      # Display detailed bound hits
      bounds %>%
        select(Index, Var_name, Estimate, Hit_Type, L_bound, U_bound) %>%
        datatable(options = list(pageLength = 20, scrollX = TRUE), 
                  rownames = FALSE)
    }
  })
  
  # Download handler for bound hits CSV
  output$download_bounds <- downloadHandler(
    filename = function() {
      paste0("bound_hits_", input$bound_model, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$bound_model, bounds_data())
      bounds <- bounds_data()[[input$bound_model]]$bound_hits
      write.csv(bounds, file, row.names = FALSE)
    }
  )
  
  # ===========================================================================
  # TAB 4: STOCK STATUS
  # ===========================================================================
  
  # Reactive: generate stock status plot
  stock_plot_reactive <- reactive({
    req(rv$data_loaded, input$stock_scenarios)
    
    # Check if any scenarios selected
    if (length(input$stock_scenarios) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    tryCatch({
      # Extract spawning biomass depletion (SB/SBF0)
      SBdep <- map_dfr(input$stock_scenarios, function(scenario) {
        rep_obj <- rv$RepOut_list[[scenario]]
        year_range <- rv$YearRanges[[scenario]]
        
        # Calculate SB/SBF0 using adultBiomass slots
        sb <- slot(rep_obj, "adultBiomass")
        sbf0 <- slot(rep_obj, "adultBiomass_nofish")
        
        # Sum across all dimensions except time (dimension 2)
        sb_vec <- apply(sb, 2, sum, na.rm = TRUE)
        sbf0_vec <- apply(sbf0, 2, sum, na.rm = TRUE)
        
        # Calculate depletion ratio
        sb_ratio <- as.numeric(sb_vec / sbf0_vec)
        
        # Generate years
        n_years <- length(sb_ratio)
        years <- year_range$minYear + seq(0, n_years - 1)
        
        data.frame(
          Scenario = scenario,
          Year = years,
          Quant = sb_ratio,
          stringsAsFactors = FALSE
        )
      })
      
      # Extract recruitment
      Rec <- map_dfr(input$stock_scenarios, function(scenario) {
        rep_obj <- rv$RepOut_list[[scenario]]
        year_range <- rv$YearRanges[[scenario]]
        
        # Get recruitment data
        rec_data <- slot(rep_obj, "eq_rec")
        
        # Sum across all dimensions except time (dimension 2)
        rec_vec <- apply(rec_data, 2, sum, na.rm = TRUE)
        rec_vec <- as.numeric(rec_vec)
        
        # Generate years
        n_years <- length(rec_vec)
        years <- year_range$minYear + seq(0, n_years - 1)
        
        data.frame(
          Scenario = scenario,
          Year = years,
          Quant = rec_vec / 1e6,  # Convert to millions
          stringsAsFactors = FALSE
        )
      })
      
      # Remove NA and Inf values
      SBdep <- SBdep[!is.na(SBdep$Quant) & is.finite(SBdep$Quant), ]
      Rec <- Rec[!is.na(Rec$Quant) & is.finite(Rec$Quant), ]
      
      # Check if data exists
      if (nrow(SBdep) < 2 || nrow(Rec) < 2) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Insufficient data points\nSB data:", nrow(SBdep), 
                                 "points\nRec data:", nrow(Rec), "points"), 
                   size = 6, color = "#999") +
          theme_void()
        return(p)
      }
      
      # Generate color palette for scenarios
      scenario_colors <- get_scenario_colors(input$stock_scenarios)
      
      # Plot 1: Spawning Biomass Depletion
      p1 <- ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = scenario_colors) +
        scale_y_continuous(limits = c(0, max(1, max(SBdep$Quant, na.rm = TRUE) * 1.05)), 
                           expand = c(0, 0.02)) +
        labs(x = NULL, y = "SB / SB(F=0)") +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "#d9534f", linewidth = 0.8) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "#5cb85c", linewidth = 0.8) +
        annotate("text", x = min(SBdep$Year), y = 0.2, label = "0.2", 
                 vjust = -0.5, hjust = -0.2, size = 3.5, color = "#d9534f") +
        annotate("text", x = min(SBdep$Year), y = 0.5, label = "0.5", 
                 vjust = -0.5, hjust = -0.2, size = 3.5, color = "#5cb85c") +
        theme_bw(base_size = 14) + 
        theme(
          legend.position = "none",
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
      # Plot 2: Recruitment
      p2 <- ggplot(Rec, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = scenario_colors) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(x = "Year", y = "Recruitment (millions)", color = NULL) +
        theme_bw(base_size = 14) + 
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          panel.grid.minor = element_blank()
        ) +
        guides(color = guide_legend(nrow = 1))
      
      # Combine both plots vertically
      plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1.3))
      
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error loading stock status:\n", e$message), 
                 size = 5, color = "red") +
        theme_void()
      return(p)
    })
  })
  
  # Render stock status plot
  output$stock_plot <- renderPlot({
    stock_plot_reactive()
  })
  
  # ===========================================================================
  # TAB 5: CPUE FITS
  # ===========================================================================
  
  # Update fishery choices when scenarios change (preserve selection)
  observeEvent(input$cpue_scenarios, {
    req(rv$data_loaded)
    
    if (length(input$cpue_scenarios) == 0) {
      updatePickerInput(session, "cpue_fisheries", choices = character(0))
      return()
    }
    
    # Get index fisheries from all selected scenarios
    all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
    
    # Check if index fisheries exist
    if (length(all_index_fish) == 0) {
      updatePickerInput(session, "cpue_fisheries", choices = character(0))
      showNotification("No index fisheries detected in selected scenarios", 
                       type = "warning", duration = 3)
      return()
    }
    
    # Create named choices with fishery labels
    fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
    choices <- setNames(all_index_fish, 
                        sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
    
    # Preserve current selection if it exists in new choices
    current_selection <- isolate(input$cpue_fisheries)
    
    # Determine new selection
    if (is.null(current_selection) || length(current_selection) == 0) {
      new_selection <- all_index_fish
    } else {
      new_selection <- intersect(current_selection, all_index_fish)
      if (length(new_selection) == 0) {
        new_selection <- all_index_fish
      }
    }
    
    updatePickerInput(session, "cpue_fisheries", 
                      choices = choices,
                      selected = new_selection)
  }, ignoreInit = FALSE)
  
  # Reactive: generate CPUE plot
  cpue_plot_reactive <- reactive({
    req(rv$data_loaded, input$cpue_scenarios, input$cpue_fisheries)
    
    # Check if any selections made
    if (length(input$cpue_scenarios) == 0 || length(input$cpue_fisheries) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No scenarios or fisheries selected", 
                 size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    tryCatch({
      # Combine CPUE data from all selected scenarios
      cpue_all <- map_dfr(input$cpue_scenarios, function(scenario) {
        rep_obj <- rv$RepOut_list[[scenario]]
        fishery_map <- rv$FISHERY_MAPS[[scenario]]
        
        # Extract observed and predicted CPUE
        obs <- as.data.frame(cpue_obs(rep_obj))
        fit <- as.data.frame(cpue_pred(rep_obj))
        
        # Standardize column names
        names(obs)[names(obs) == "data"] <- "obs"
        names(fit)[names(fit) == "data"] <- "fit"
        
        # Merge observed and predicted
        cpue <- merge(obs, fit)
        cpue <- type.convert(cpue, as.is = TRUE)
        
        # Filter to selected fisheries
        cpue <- cpue[cpue$unit %in% as.numeric(input$cpue_fisheries), ]
        
        if (nrow(cpue) > 0) {
          cpue$Scenario <- scenario
          cpue$fishery_name <- sapply(as.character(cpue$unit), 
                                      function(x) get_fishery_name(x, fishery_map))
          cpue
        } else {
          NULL
        }
      })
      
      # Check if data exists
      if (is.null(cpue_all) || nrow(cpue_all) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No CPUE data available", 
                   size = 6, color = "#999") +
          theme_void()
        return(p)
      }
      
      # Transform data (convert from log-scale)
      cpue_all <- cpue_all %>%
        mutate(
          year_season = year + (season - 1) / 4,
          obs = exp(obs),
          fit = exp(fit)
        )
      
      # Generate colors
      scenario_colors <- get_scenario_colors(input$cpue_scenarios)
      
      # Create CPUE plot
      p <- ggplot(cpue_all, aes(x = year_season)) +
        geom_point(aes(y = obs), size = 2, alpha = 0.6, color = "#E69F00") +
        geom_line(aes(y = fit, color = Scenario), linewidth = 1.2, alpha = 0.9) +
        facet_wrap(~fishery_name, scales = "free_y", ncol = 3) +
        scale_color_manual(values = scenario_colors) +
        labs(x = "Year + Season", y = "CPUE", 
             title = paste("CPUE Fits -", 
                           paste(input$cpue_scenarios, collapse = ", "))) +
        theme_bw(base_size = 13) +
        theme(
          legend.position = "top", 
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          strip.background = element_rect(fill = "grey90"),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      return(p)
      
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error:", e$message), 
                 size = 5, color = "red") +
        theme_void()
      return(p)
    })
  })
  
  # Render CPUE plot
  output$cpue_plot <- renderPlot({
    cpue_plot_reactive()
  })
  
  # ===========================================================================
  # TAB 6: LENGTH FREQUENCY (DYNAMIC BOX HEIGHT)
  # ===========================================================================
  
  # Helper function to check if models are compatible for LF overlay
  check_lf_compatibility <- function(base_model, compare_models) {
    if (is.null(rv$LengOut_list[[base_model]])) return(character(0))
    
    base_fisheries <- unique(rv$LengOut_list[[base_model]]@lenfits$fishery)
    base_years <- unique(rv$LengOut_list[[base_model]]@lenfits$year)
    
    # Get base model fishery names
    base_fishery_names <- sapply(base_fisheries, function(f) {
      rv$FISHERY_MAPS[[base_model]][[as.character(f)]]
    })
    
    compatible <- sapply(compare_models, function(m) {
      if (is.null(rv$LengOut_list[[m]])) return(FALSE)
      
      m_fisheries <- unique(rv$LengOut_list[[m]]@lenfits$fishery)
      m_years <- unique(rv$LengOut_list[[m]]@lenfits$year)
      
      # Get comparison model fishery names
      m_fishery_names <- sapply(m_fisheries, function(f) {
        rv$FISHERY_MAPS[[m]][[as.character(f)]]
      })
      
      # Check: same fishery IDs, same fishery names, same years
      same_ids <- setequal(base_fisheries, m_fisheries)
      same_names <- identical(sort(base_fishery_names), sort(m_fishery_names))
      same_years <- setequal(base_years, m_years)
      
      same_ids && same_names && same_years
    })
    
    names(compatible)[compatible]
  }
  
  # Update fishery choices when base model changes
  observeEvent(input$lf_model, {
    req(rv$data_loaded, input$lf_model, rv$LengOut_list[[input$lf_model]])
    
    # Get fisheries from selected model
    fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
    if (length(fisheries) == 0) {
      updateSelectInput(session, "lf_fishery", choices = character(0))
      return()
    }
    
    # Create named choices
    fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
    choices <- setNames(fisheries, 
                        sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    
    # Preserve current selection if valid
    current_selection <- isolate(input$lf_fishery)
    if (!is.null(current_selection) && current_selection %in% fisheries) {
      selected <- current_selection
    } else {
      selected <- fisheries[1]
    }
    
    updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)
    
    # Update compatible scenarios for overlay
    all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
    compatible_models <- check_lf_compatibility(input$lf_model, all_models)
    
    # Show notification if some models are incompatible
    n_incompatible <- length(all_models) - length(compatible_models)
    if (n_incompatible > 0) {
      showNotification(
        HTML(paste0(
          "⚠️ <strong>", n_incompatible, " model(s) excluded from overlay</strong><br/>",
          "Reason: Different fishery structure or names"
        )),
        type = "warning",
        duration = 4
      )
    }
    
    updatePickerInput(session, "lf_scenarios",
                      choices = compatible_models,
                      selected = input$lf_model)
  })
  
  # Update year choices when fishery or scenarios change
  observeEvent(list(input$lf_fishery, input$lf_model), {
    req(rv$data_loaded, input$lf_fishery, input$lf_model)
    
    # Extract years for selected fishery from base model
    if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
    df <- rv$LengOut_list[[input$lf_model]]@lenfits
    years <- df %>% 
      filter(fishery == as.numeric(input$lf_fishery)) %>% 
      pull(year) %>% 
      unique() %>%
      sort()
    
    if (length(years) == 0) {
      updatePickerInput(session, "lf_years", choices = NULL, selected = NULL)
      return()
    }
    
    # Preserve current selection if valid
    current_selection <- isolate(input$lf_years)
    if (!is.null(current_selection) && all(current_selection %in% years)) {
      selected <- current_selection
    } else {
      selected <- years
    }
    
    updatePickerInput(session, "lf_years", 
                      choices = years,
                      selected = selected)
  }, ignoreInit = TRUE)
  
  # Reactive: calculate dynamic plot height for LF
  lf_plot_height <- reactive({
    req(rv$data_loaded, input$lf_years)
    
    n_years <- length(input$lf_years)
    
    if (n_years == 0) return(400)
    
    # Determine number of columns based on year count
    ncol_facet <- case_when(
      n_years <= 6 ~ 2,
      n_years <= 12 ~ 3,
      n_years <= 20 ~ 4,
      n_years <= 30 ~ 5,
      TRUE ~ 6
    )
    
    # Calculate rows needed
    n_rows <- ceiling(n_years / ncol_facet)
    
    # Height formula: base + height per row
    base_height <- 150
    height_per_row <- 200
    total_height <- base_height + (n_rows * height_per_row)
    
    # Constrain between 400 and 3000 pixels
    min(max(total_height, 400), 3000)
  })
  
  # Reactive: generate length frequency plot
  lf_plot_reactive <- reactive({
    req(rv$data_loaded, input$lf_model, input$lf_fishery, input$lf_scenarios, input$lf_years)
    
    # Check if any scenarios selected
    if (length(input$lf_scenarios) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Check if any years selected
    if (length(input$lf_years) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No years selected", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Combine data from selected scenarios
    combined_data <- map_dfr(input$lf_scenarios, function(sc) {
      if (is.null(rv$LengOut_list[[sc]])) return(NULL)
      df <- rv$LengOut_list[[sc]]@lenfits
      if (as.numeric(input$lf_fishery) %in% unique(df$fishery)) {
        df %>% 
          filter(fishery == as.numeric(input$lf_fishery)) %>% 
          mutate(Scenario = sc)
      } else {
        NULL
      }
    })
    
    # Check if data exists
    if (is.null(combined_data) || nrow(combined_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Aggregate by scenario, year, length
    plot_data <- combined_data %>%
      group_by(Scenario, fishery, year, length) %>%
      summarise(obs = sum(obs, na.rm = TRUE), 
                pred = sum(pred, na.rm = TRUE), 
                .groups = "drop") %>%
      filter(obs > 0 | pred > 0)
    
    # Apply year filter
    plot_data <- plot_data %>%
      filter(year %in% input$lf_years)
    
    # Check if data exists after filtering
    if (nrow(plot_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data for selected years", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Separate observed data (same across scenarios)
    obs_data <- plot_data %>%
      group_by(year, length) %>%
      summarise(obs = first(obs), .groups = "drop")
    
    fishery_name <- get_fishery_name(input$lf_fishery, rv$FISHERY_MAPS[[input$lf_model]])
    
    # Determine optimal layout
    n_years <- length(unique(plot_data$year))
    ncol_facet <- case_when(
      n_years <= 6 ~ 2,
      n_years <= 12 ~ 3,
      n_years <= 20 ~ 4,
      n_years <= 30 ~ 5,
      TRUE ~ 6
    )
    
    strip_size <- case_when(
      n_years <= 12 ~ 10,
      n_years <= 20 ~ 9,
      n_years <= 30 ~ 8,
      TRUE ~ 7
    )
    
    # Create overlay plot
    p <- ggplot() +
      geom_col(data = obs_data, 
               aes(x = length, y = obs, fill = "Observed"), 
               alpha = 0.7, width = 2, position = "identity") +
      geom_line(data = plot_data,
                aes(x = length, y = pred, color = Scenario), 
                linewidth = 1.2) +
      facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
      scale_fill_manual(values = c("Observed" = "#E69F00")) +
      scale_color_viridis_d() +
      labs(title = paste(fishery_name, "- Base:", input$lf_model,
                         paste0("(", n_years, " years)")),
           x = "Length (cm)", y = "Frequency") +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = strip_size, face = "bold"),
        panel.spacing = unit(0.3, "lines")
      )
    
    return(p)
  })
  
  # Render length frequency plot
  output$lf_plot <- renderPlot({
    lf_plot_reactive()
  })
  
  # Render dynamic box for LF with calculated height
  output$lf_plot_box <- renderUI({
    height <- lf_plot_height()
    
    box(
      title = "Length Frequency",
      width = 9,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      plotOutput("lf_plot", height = paste0(height, "px"))
    )
  })
  
  # ===========================================================================
  # TAB 7: WEIGHT FREQUENCY (DYNAMIC BOX HEIGHT)
  # ===========================================================================
  
  # Helper function to check if models are compatible for WF overlay
  check_wf_compatibility <- function(base_model, compare_models) {
    if (is.null(rv$WeightOut_list[[base_model]])) return(character(0))
    
    base_fisheries <- unique(rv$WeightOut_list[[base_model]]@wgtfits$fishery)
    base_years <- unique(rv$WeightOut_list[[base_model]]@wgtfits$year)
    
    # Get base model fishery names
    base_fishery_names <- sapply(base_fisheries, function(f) {
      rv$FISHERY_MAPS[[base_model]][[as.character(f)]]
    })
    
    compatible <- sapply(compare_models, function(m) {
      if (is.null(rv$WeightOut_list[[m]])) return(FALSE)
      
      m_fisheries <- unique(rv$WeightOut_list[[m]]@wgtfits$fishery)
      m_years <- unique(rv$WeightOut_list[[m]]@wgtfits$year)
      
      # Get comparison model fishery names
      m_fishery_names <- sapply(m_fisheries, function(f) {
        rv$FISHERY_MAPS[[m]][[as.character(f)]]
      })
      
      # Check: same fishery IDs, same fishery names, same years
      same_ids <- setequal(base_fisheries, m_fisheries)
      same_names <- identical(sort(base_fishery_names), sort(m_fishery_names))
      same_years <- setequal(base_years, m_years)
      
      same_ids && same_names && same_years
    })
    
    names(compatible)[compatible]
  }
  
  # Update fishery choices when base model changes
  observeEvent(input$wf_model, {
    req(rv$data_loaded, input$wf_model, rv$WeightOut_list[[input$wf_model]])
    
    # Get fisheries from selected model
    fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
    if (length(fisheries) == 0) {
      updateSelectInput(session, "wf_fishery", choices = character(0))
      return()
    }
    
    # Create named choices
    fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
    choices <- setNames(fisheries, 
                        sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    
    # Preserve current selection if valid
    current_selection <- isolate(input$wf_fishery)
    if (!is.null(current_selection) && current_selection %in% fisheries) {
      selected <- current_selection
    } else {
      selected <- fisheries[1]
    }
    
    updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)
    
    # Update compatible scenarios for overlay
    all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
    compatible_models <- check_wf_compatibility(input$wf_model, all_models)
    
    # Show notification if some models are incompatible
    n_incompatible <- length(all_models) - length(compatible_models)
    if (n_incompatible > 0) {
      showNotification(
        HTML(paste0(
          "⚠️ <strong>", n_incompatible, " model(s) excluded from overlay</strong><br/>",
          "Reason: Different fishery structure or names"
        )),
        type = "warning",
        duration = 4
      )
    }
    
    updatePickerInput(session, "wf_scenarios",
                      choices = compatible_models,
                      selected = input$wf_model)
  })
  
  # Update year choices when fishery or model change
  observeEvent(list(input$wf_fishery, input$wf_model), {
    req(rv$data_loaded, input$wf_fishery, input$wf_model)
    
    # Extract years for selected fishery from base model
    if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
    df <- rv$WeightOut_list[[input$wf_model]]@wgtfits
    years <- df %>% 
      filter(fishery == as.numeric(input$wf_fishery)) %>% 
      pull(year) %>% 
      unique() %>%
      sort()
    
    if (length(years) == 0) {
      updatePickerInput(session, "wf_years", choices = NULL, selected = NULL)
      return()
    }
    
    # Preserve current selection if valid
    current_selection <- isolate(input$wf_years)
    if (!is.null(current_selection) && all(current_selection %in% years)) {
      selected <- current_selection
    } else {
      selected <- years
    }
    
    updatePickerInput(session, "wf_years", 
                      choices = years,
                      selected = selected)
  }, ignoreInit = TRUE)
  
  # Reactive: calculate dynamic plot height for WF
  wf_plot_height <- reactive({
    req(rv$data_loaded, input$wf_years)
    
    n_years <- length(input$wf_years)
    
    if (n_years == 0) return(400)
    
    # Determine number of columns based on year count
    ncol_facet <- case_when(
      n_years <= 6 ~ 2,
      n_years <= 12 ~ 3,
      n_years <= 20 ~ 4,
      n_years <= 30 ~ 5,
      TRUE ~ 6
    )
    
    # Calculate rows needed
    n_rows <- ceiling(n_years / ncol_facet)
    
    # Height formula: base + height per row
    base_height <- 150
    height_per_row <- 200
    total_height <- base_height + (n_rows * height_per_row)
    
    # Constrain between 400 and 3000 pixels
    min(max(total_height, 400), 3000)
  })
  
  # Reactive: generate weight frequency plot
  wf_plot_reactive <- reactive({
    req(rv$data_loaded, input$wf_model, input$wf_fishery, input$wf_scenarios, input$wf_years)
    
    # Check if any scenarios selected
    if (length(input$wf_scenarios) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Check if any years selected
    if (length(input$wf_years) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No years selected", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Combine data from selected scenarios
    combined_data <- map_dfr(input$wf_scenarios, function(sc) {
      if (is.null(rv$WeightOut_list[[sc]])) return(NULL)
      df <- rv$WeightOut_list[[sc]]@wgtfits
      if (as.numeric(input$wf_fishery) %in% unique(df$fishery)) {
        df %>% 
          filter(fishery == as.numeric(input$wf_fishery)) %>% 
          mutate(Scenario = sc)
      } else {
        NULL
      }
    })
    
    # Check if data exists
    if (is.null(combined_data) || nrow(combined_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Aggregate by scenario, year, weight
    plot_data <- combined_data %>%
      group_by(Scenario, fishery, year, weight) %>%
      summarise(obs = sum(obs, na.rm = TRUE), 
                pred = sum(pred, na.rm = TRUE), 
                .groups = "drop") %>%
      filter(obs > 0 | pred > 0)
    
    # Apply year filter
    plot_data <- plot_data %>%
      filter(year %in% input$wf_years)
    
    # Check if data exists after filtering
    if (nrow(plot_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data for selected years", size = 6, color = "#999") +
        theme_void()
      return(p)
    }
    
    # Separate observed data
    obs_data <- plot_data %>%
      group_by(year, weight) %>%
      summarise(obs = first(obs), .groups = "drop")
    
    fishery_name <- get_fishery_name(input$wf_fishery, rv$FISHERY_MAPS[[input$wf_model]])
    
    # Determine optimal layout
    n_years <- length(unique(plot_data$year))
    ncol_facet <- case_when(
      n_years <= 6 ~ 2,
      n_years <= 12 ~ 3,
      n_years <= 20 ~ 4,
      n_years <= 30 ~ 5,
      TRUE ~ 6
    )
    
    strip_size <- case_when(
      n_years <= 12 ~ 10,
      n_years <= 20 ~ 9,
      n_years <= 30 ~ 8,
      TRUE ~ 7
    )
    
    # Create overlay plot
    p <- ggplot() +
      geom_col(data = obs_data, 
               aes(x = weight, y = obs, fill = "Observed"), 
               alpha = 0.7, width = 2, position = "identity") +
      geom_line(data = plot_data,
                aes(x = weight, y = pred, color = Scenario), 
                linewidth = 1.2) +
      facet_wrap(~year, scales = "free_y", ncol = ncol_facet) +
      scale_fill_manual(values = c("Observed" = "#E69F00")) +
      scale_color_viridis_d() +
      labs(title = paste(fishery_name, "- Base:", input$wf_model,
                         paste0("(", n_years, " years)")),
           x = "Weight (kg)", y = "Frequency") +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = strip_size, face = "bold"),
        panel.spacing = unit(0.3, "lines")
      )
    
    return(p)
  })
  
  # Render weight frequency plot
  output$wf_plot <- renderPlot({
    wf_plot_reactive()
  })
  
  # Render dynamic box for WF with calculated height
  output$wf_plot_box <- renderUI({
    height <- wf_plot_height()
    
    box(
      title = "Weight Frequency",
      width = 9,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      plotOutput("wf_plot", height = paste0(height, "px"))
    )
  })
  
  # ===========================================================================
  # DOWNLOAD MODALS AND HANDLERS
  # ===========================================================================
  
  # Common download modal function
  show_download_modal <- function(plot_type, plot_name) {
    showModal(modalDialog(
      title = paste("📥 Download", plot_name),
      size = "m",
      
      fluidRow(
        column(6,
               h5("📐 Dimensions", style = "font-weight: bold; margin-top: 0;"),
               numericInput(paste0(plot_type, "_width"), "Width (inches):", 
                            value = 12, min = 4, max = 24, step = 1),
               numericInput(paste0(plot_type, "_height"), "Height (inches):", 
                            value = 8, min = 4, max = 20, step = 1),
               
               h5("📊 Presets", style = "font-weight: bold; margin-top: 15px;"),
               actionButton(paste0(plot_type, "_preset_wide"), "Wide (16:9)", 
                            class = "btn-sm btn-default", 
                            style = "width: 100%; margin-bottom: 5px;"),
               actionButton(paste0(plot_type, "_preset_standard"), "Standard (4:3)", 
                            class = "btn-sm btn-default", 
                            style = "width: 100%; margin-bottom: 5px;"),
               actionButton(paste0(plot_type, "_preset_square"), "Square (1:1)", 
                            class = "btn-sm btn-default", 
                            style = "width: 100%;")
        ),
        column(6,
               h5("🎨 Quality", style = "font-weight: bold; margin-top: 0;"),
               selectInput(paste0(plot_type, "_dpi"), "Resolution (DPI):",
                           choices = c("Screen (96)" = 96,
                                       "Print Draft (150)" = 150,
                                       "Print Standard (300)" = 300,
                                       "Print High (600)" = 600),
                           selected = 300),
               
               h5("📄 Format", style = "font-weight: bold; margin-top: 15px;"),
               radioButtons(paste0(plot_type, "_format"), NULL,
                            choices = c("PNG (Raster)" = "png",
                                        "PDF (Vector)" = "pdf",
                                        "SVG (Vector)" = "svg",
                                        "JPEG (Raster)" = "jpeg"),
                            selected = "png"),
               
               helpText("💡 PDF/SVG recommended for reports (scalable)", 
                        style = "font-size: 11px; font-style: italic; color: #666;")
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        downloadButton(paste0(plot_type, "_download_confirm"), "Download", 
                       class = "btn-primary")
      )
    ))
  }
  
  # ---------------------------------------------------------------------------
  # STOCK STATUS DOWNLOAD
  # ---------------------------------------------------------------------------
  
  observeEvent(input$show_stock_download_modal, {
    show_download_modal("stock", "Stock Status Plot")
  })
  
  observeEvent(input$stock_preset_wide, {
    updateNumericInput(session, "stock_width", value = 16)
    updateNumericInput(session, "stock_height", value = 9)
  })
  
  observeEvent(input$stock_preset_standard, {
    updateNumericInput(session, "stock_width", value = 12)
    updateNumericInput(session, "stock_height", value = 9)
  })
  
  observeEvent(input$stock_preset_square, {
    updateNumericInput(session, "stock_width", value = 10)
    updateNumericInput(session, "stock_height", value = 10)
  })
  
  output$stock_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$stock_format
      paste0("stock_status_", Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- stock_plot_reactive()
      width <- input$stock_width
      height <- input$stock_height
      dpi <- as.numeric(input$stock_dpi)
      format <- input$stock_format
      
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
  
  # ---------------------------------------------------------------------------
  # CPUE DOWNLOAD
  # ---------------------------------------------------------------------------
  
  observeEvent(input$show_cpue_download_modal, {
    show_download_modal("cpue", "CPUE Fits Plot")
  })
  
  observeEvent(input$cpue_preset_wide, {
    updateNumericInput(session, "cpue_width", value = 16)
    updateNumericInput(session, "cpue_height", value = 9)
  })
  
  observeEvent(input$cpue_preset_standard, {
    updateNumericInput(session, "cpue_width", value = 12)
    updateNumericInput(session, "cpue_height", value = 9)
  })
  
  observeEvent(input$cpue_preset_square, {
    updateNumericInput(session, "cpue_width", value = 10)
    updateNumericInput(session, "cpue_height", value = 10)
  })
  
  output$cpue_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$cpue_format
      paste0("cpue_fits_", Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- cpue_plot_reactive()
      width <- input$cpue_width
      height <- input$cpue_height
      dpi <- as.numeric(input$cpue_dpi)
      format <- input$cpue_format
      
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
  
  # ---------------------------------------------------------------------------
  # LENGTH FREQUENCY DOWNLOAD
  # ---------------------------------------------------------------------------
  
  observeEvent(input$show_lf_download_modal, {
    show_download_modal("lf", "Length Frequency Plot")
  })
  
  observeEvent(input$lf_preset_wide, {
    updateNumericInput(session, "lf_width", value = 16)
    updateNumericInput(session, "lf_height", value = 10)
  })
  
  observeEvent(input$lf_preset_standard, {
    updateNumericInput(session, "lf_width", value = 12)
    updateNumericInput(session, "lf_height", value = 9)
  })
  
  observeEvent(input$lf_preset_square, {
    updateNumericInput(session, "lf_width", value = 10)
    updateNumericInput(session, "lf_height", value = 10)
  })
  
  output$lf_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$lf_format
      paste0("length_freq_", input$lf_model, "_", input$lf_fishery, "_", 
             Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- lf_plot_reactive()
      width <- input$lf_width
      height <- input$lf_height
      dpi <- as.numeric(input$lf_dpi)
      format <- input$lf_format
      
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
  
  # ---------------------------------------------------------------------------
  # WEIGHT FREQUENCY DOWNLOAD
  # ---------------------------------------------------------------------------
  
  observeEvent(input$show_wf_download_modal, {
    show_download_modal("wf", "Weight Frequency Plot")
  })
  
  observeEvent(input$wf_preset_wide, {
    updateNumericInput(session, "wf_width", value = 16)
    updateNumericInput(session, "wf_height", value = 10)
  })
  
  observeEvent(input$wf_preset_standard, {
    updateNumericInput(session, "wf_width", value = 12)
    updateNumericInput(session, "wf_height", value = 9)
  })
  
  observeEvent(input$wf_preset_square, {
    updateNumericInput(session, "wf_width", value = 10)
    updateNumericInput(session, "wf_height", value = 10)
  })
  
  output$wf_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$wf_format
      paste0("weight_freq_", input$wf_model, "_", input$wf_fishery, "_", 
             Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- wf_plot_reactive()
      width <- input$wf_width
      height <- input$wf_height
      dpi <- as.numeric(input$wf_dpi)
      format <- input$wf_format
      
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
  
  # ===========================================================================
  # FISHERY NAVIGATION BUTTONS
  # ===========================================================================
  
  # Length Frequency: Previous fishery
  observeEvent(input$lf_prev, {
    req(rv$data_loaded, input$lf_model, input$lf_fishery)
    
    if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
    fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
    if (length(fisheries) == 0) return()
    
    current_idx <- which(fisheries == input$lf_fishery)
    
    if (length(current_idx) == 0) {
      new_selection <- fisheries[1]
    } else {
      new_idx <- ifelse(current_idx == 1, length(fisheries), current_idx - 1)
      new_selection <- fisheries[new_idx]
    }
    
    updateSelectInput(session, "lf_fishery", selected = new_selection)
  })
  
  # Length Frequency: Next fishery
  observeEvent(input$lf_next, {
    req(rv$data_loaded, input$lf_model, input$lf_fishery)
    
    if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
    fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
    if (length(fisheries) == 0) return()
    
    current_idx <- which(fisheries == input$lf_fishery)
    
    if (length(current_idx) == 0) {
      new_selection <- fisheries[1]
    } else {
      new_idx <- ifelse(current_idx == length(fisheries), 1, current_idx + 1)
      new_selection <- fisheries[new_idx]
    }
    
    updateSelectInput(session, "lf_fishery", selected = new_selection)
  })
  
  # Weight Frequency: Previous fishery
  observeEvent(input$wf_prev, {
    req(rv$data_loaded, input$wf_model, input$wf_fishery)
    
    if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
    fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
    if (length(fisheries) == 0) return()
    
    current_idx <- which(fisheries == input$wf_fishery)
    
    if (length(current_idx) == 0) {
      new_selection <- fisheries[1]
    } else {
      new_idx <- ifelse(current_idx == 1, length(fisheries), current_idx - 1)
      new_selection <- fisheries[new_idx]
    }
    
    updateSelectInput(session, "wf_fishery", selected = new_selection)
  })
  
  # Weight Frequency: Next fishery
  observeEvent(input$wf_next, {
    req(rv$data_loaded, input$wf_model, input$wf_fishery)
    
    if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
    fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
    if (length(fisheries) == 0) return()
    
    current_idx <- which(fisheries == input$wf_fishery)
    
    if (length(current_idx) == 0) {
      new_selection <- fisheries[1]
    } else {
      new_idx <- ifelse(current_idx == length(fisheries), 1, current_idx + 1)
      new_selection <- fisheries[new_idx]
    }
    
    updateSelectInput(session, "wf_fishery", selected = new_selection)
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui, server)

