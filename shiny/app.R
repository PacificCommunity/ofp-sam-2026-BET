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
    textInput("model_dir", "Model Directory:",
              value = normalizePath(file.path("..", "model"), mustWork = FALSE),
              placeholder = "/path/to/model"),
    
    # Browse button (Windows only)
    actionButton("browse_dir", "Browse...", 
                 icon = icon("folder-open"),
                 class = "btn-info btn-sm",
                 style = "margin-left: 15px; margin-bottom: 10px;"),
    
    helpText("Folder containing scenario subfolders", 
             style = "padding-left: 15px; font-size: 11px; color: #777;"),
    
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
    
    # Load data button (fixed width)
    div(
      style = "padding: 0 15px;",
      actionButton("load_data", "Load Data", 
                   icon = icon("upload"),
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 15px;")
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
        
        /* Fix checkbox text color and styling */
        #stock_scenarios label,
        #cpue_scenarios label,
        #cpue_fisheries label,
        #lf_scenarios label,
        #wf_scenarios label {
          color: #333 !important;
          font-size: 13px !important;
          font-weight: normal !important;
          margin-bottom: 6px;
          padding: 4px 8px;
          border-radius: 3px;
          transition: background-color 0.2s;
        }
        
        /* Hover effect */
        #stock_scenarios label:hover,
        #cpue_scenarios label:hover,
        #cpue_fisheries label:hover,
        #lf_scenarios label:hover,
        #wf_scenarios label:hover {
          background-color: #f5f5f5;
        }
        
        /* Checked state - make text bold */
        #stock_scenarios input[type='checkbox']:checked + span,
        #cpue_scenarios input[type='checkbox']:checked + span,
        #cpue_fisheries input[type='checkbox']:checked + span,
        #lf_scenarios input[type='checkbox']:checked + span,
        #wf_scenarios input[type='checkbox']:checked + span {
          font-weight: 600;
          color: #3c8dbc !important;
        }
        
        /* Checkbox container spacing */
        .checkbox { margin-top: 5px; margin-bottom: 5px; }
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
      # TAB 2: BOUND HITS
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
      # TAB 3: STOCK STATUS
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
            checkboxGroupInput("stock_scenarios", "Scenarios:", choices = NULL),
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            downloadButton("download_stock_png", "PNG", class = "btn-info btn-sm", 
                           style = "width: 48%; margin-right: 2%;"),
            downloadButton("download_stock_pdf", "PDF", class = "btn-info btn-sm", 
                           style = "width: 48%;"),
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
      # TAB 4: CPUE FITS
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
            checkboxGroupInput("cpue_scenarios", "Scenarios:", choices = NULL),
            checkboxGroupInput("cpue_fisheries", "Fisheries:", choices = NULL),
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            downloadButton("download_cpue_png", "PNG", class = "btn-info btn-sm", 
                           style = "width: 48%; margin-right: 2%;"),
            downloadButton("download_cpue_pdf", "PDF", class = "btn-info btn-sm", 
                           style = "width: 48%;"),
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
      # TAB 5: LENGTH FREQUENCY
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
            selectInput("lf_fishery", "Fishery:", choices = NULL),
            checkboxGroupInput("lf_scenarios", "Scenarios:", choices = NULL),
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            downloadButton("download_lf_png", "PNG", class = "btn-info btn-sm", 
                           style = "width: 48%; margin-right: 2%;"),
            downloadButton("download_lf_pdf", "PDF", class = "btn-info btn-sm", 
                           style = "width: 48%;"),
            helpText("Select scenarios to overlay on the plot", style = "margin-top: 10px;")
          ),
          # Length frequency plot panel
          box(
            title = "Length Frequency",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput("lf_plot", height = "700px")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 6: WEIGHT FREQUENCY
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
            selectInput("wf_fishery", "Fishery:", choices = NULL),
            checkboxGroupInput("wf_scenarios", "Scenarios:", choices = NULL),
            hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            downloadButton("download_wf_png", "PNG", class = "btn-info btn-sm", 
                           style = "width: 48%; margin-right: 2%;"),
            downloadButton("download_wf_pdf", "PDF", class = "btn-info btn-sm", 
                           style = "width: 48%;"),
            helpText("Select scenarios to overlay on the plot", style = "margin-top: 10px;")
          ),
          # Weight frequency plot panel
          box(
            title = "Weight Frequency",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput("wf_plot", height = "700px")
          )
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
    YearRanges = NULL                 # Year ranges for each scenario
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
  # DATA LOADING
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
      
      # Load each scenario
      results <- setNames(lapply(seq_along(model_folders), function(i) {
        folder <- model_folders[i]
        scenario_name <- model_names[i]
        
        incProgress(0.6 / length(model_folders), 
                    detail = paste("Loading", scenario_name, "..."))
        
        tryCatch({
          
          # Check if required files exist
          par_file <- finalPar(folder)
          rep_file <- finalRep(folder)
          
          # Validate .par file
          if (!file.exists(par_file)) {
            showNotification(
              paste("Missing .par file in", scenario_name), 
              type = "warning",
              duration = 3
            )
            return(NULL)
          }
          
          # Validate plot.rep file
          if (!file.exists(rep_file)) {
            showNotification(
              paste("Missing plot.rep file in", scenario_name), 
              type = "warning",
              duration = 3
            )
            return(NULL)
          }
          
          # Read model output files
          list(
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
        }, error = function(e) {
          showNotification(
            paste("Error loading", scenario_name, ":", e$message), 
            type = "error",
            duration = 5
          )
          NULL
        })
      }), model_names)
      
      # Remove failed loads
      results <- Filter(Negate(is.null), results)
      
      # Check if any scenarios loaded successfully
      if (length(results) == 0) {
        showNotification("Failed to load any scenarios!", type = "error", duration = 5)
        return(NULL)
      }
      
      incProgress(0.8, detail = "Creating fishery mappings...")
      
      # Extract data into separate lists
      rv$ParOut_list <- map(results, "ParOut")
      rv$RepOut_list <- map(results, "RepOut")
      rv$LengOut_list <- map(results, "LengOut")
      rv$WeightOut_list <- map(results, "WeightOut")
      rv$IndepOut_list <- map(results, "IndepOut")
      
      # Create fishery name mappings for each scenario
      rv$FISHERY_MAPS <- lapply(names(results), function(sc) {
        create_fishery_map(rv$ParOut_list[[sc]], GLOBAL_FISHERY_NAMES)
      })
      names(rv$FISHERY_MAPS) <- names(results)
      
      # Detect index fisheries (fisheries ending with 'i' or containing 'index')
      rv$INDEX_FISHERIES_MAPS <- lapply(rv$FISHERY_MAPS, detect_index_fisheries)
      
      # Extract year ranges for each scenario
      rv$YearRanges <- map(rv$ParOut_list, function(par) {
        list(minYear = par@range["minyear"], maxYear = par@range["maxyear"])
      })
      
      incProgress(0.95, detail = "Finalizing...")
      
      # Set data loaded flag
      rv$data_loaded <- TRUE
      
      # Update UI components with loaded data
      updatePickerInput(session, "scenarios", 
                        choices = names(results),
                        selected = names(results))
      
      updateSelectInput(session, "bound_model", choices = names(results))
      
      # Update scenario checkboxes for all tabs
      updateCheckboxGroupInput(session, "stock_scenarios", 
                               choices = names(results), 
                               selected = names(results)[1])
      updateCheckboxGroupInput(session, "cpue_scenarios", 
                               choices = names(results), 
                               selected = names(results)[1])
      updateCheckboxGroupInput(session, "lf_scenarios", 
                               choices = names(results), 
                               selected = names(results)[1])
      updateCheckboxGroupInput(session, "wf_scenarios", 
                               choices = names(results), 
                               selected = names(results)[1])
      
      incProgress(1)
      
      # Display success message
      showNotification(
        HTML(paste0(
          "<strong>✓ Successfully loaded!</strong><br/>",
          "Directory: ", basename(MODEL_DIR), "<br/>",
          "Scenarios: ", length(results), "<br/>",
          "Names: ", paste(names(results), collapse = ", ")
        )), 
        type = "message", 
        duration = 8
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
  # TAB 2: BOUND HITS
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
  # TAB 3: STOCK STATUS
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
        # Reference lines for management thresholds
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "#d9534f", linewidth = 0.8) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "#5cb85c", linewidth = 0.8) +
        # Annotations for reference lines
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
  
  # Download stock plot as PNG
  output$download_stock_png <- downloadHandler(
    filename = function() {
      paste0("stock_status_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- stock_plot_reactive()
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
    }
  )
  
  # Download stock plot as PDF
  output$download_stock_pdf <- downloadHandler(
    filename = function() {
      paste0("stock_status_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      p <- stock_plot_reactive()
      ggsave(file, plot = p, width = 10, height = 8, device = "pdf")
    }
  )
  
  # ===========================================================================
  # TAB 4: CPUE FITS
  # ===========================================================================
  
  # Update fishery choices when scenarios change
  observe({
    req(rv$data_loaded, input$cpue_scenarios)
    
    if (length(input$cpue_scenarios) == 0) {
      updateCheckboxGroupInput(session, "cpue_fisheries", choices = character(0))
      return()
    }
    
    # Get index fisheries from all selected scenarios
    all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
    
    # Check if index fisheries exist
    if (length(all_index_fish) == 0) {
      updateCheckboxGroupInput(session, "cpue_fisheries", choices = character(0))
      showNotification("No index fisheries detected in selected scenarios", 
                       type = "warning", duration = 3)
      return()
    }
    
    # Create named choices with fishery labels
    fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
    choices <- setNames(all_index_fish, 
                        sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
    
    updateCheckboxGroupInput(session, "cpue_fisheries", 
                             choices = choices,
                             selected = all_index_fish[1])
  })
  
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
  
  # Download CPUE plot as PNG
  output$download_cpue_png <- downloadHandler(
    filename = function() {
      paste0("cpue_fits_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = cpue_plot_reactive(), width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  # Download CPUE plot as PDF
  output$download_cpue_pdf <- downloadHandler(
    filename = function() {
      paste0("cpue_fits_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = cpue_plot_reactive(), width = 12, height = 8, device = "pdf")
    }
  )
  
  # ===========================================================================
  # TAB 5: LENGTH FREQUENCY
  # ===========================================================================
  
  # Update fishery choices when data loaded
  observe({
    req(rv$data_loaded, rv$LengOut_list)
    
    # Get fisheries from all scenarios
    fisheries <- unique(unlist(lapply(rv$LengOut_list[!sapply(rv$LengOut_list, is.null)], 
                                      function(x) unique(x@lenfits$fishery))))
    
    # Check if fisheries found
    if (length(fisheries) == 0) {
      updateSelectInput(session, "lf_fishery", choices = character(0))
      return()
    }
    
    # Create named choices
    fishery_map <- rv$FISHERY_MAPS[[1]]
    choices <- setNames(fisheries, 
                        sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    updateSelectInput(session, "lf_fishery", choices = choices)
  })
  
  # Reactive: generate length frequency plot
  lf_plot_reactive <- reactive({
    req(rv$data_loaded, input$lf_fishery, input$lf_scenarios)
    
    # Check if any scenarios selected
    if (length(input$lf_scenarios) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
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
    
    # Separate observed data (same across scenarios)
    obs_data <- plot_data %>%
      group_by(year, length) %>%
      summarise(obs = first(obs), .groups = "drop")
    
    fishery_name <- get_fishery_name(input$lf_fishery, rv$FISHERY_MAPS[[1]])
    
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
      labs(title = paste(fishery_name, "-", 
                         paste(input$lf_scenarios, collapse = ", "),
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
  
  # Download LF plot as PNG
  output$download_lf_png <- downloadHandler(
    filename = function() {
      paste0("length_freq_", input$lf_fishery, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = lf_plot_reactive(), width = 12, height = 9, dpi = 300, bg = "white")
    }
  )
  
  # Download LF plot as PDF
  output$download_lf_pdf <- downloadHandler(
    filename = function() {
      paste0("length_freq_", input$lf_fishery, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = lf_plot_reactive(), width = 12, height = 9, device = "pdf")
    }
  )
  
  # ===========================================================================
  # TAB 6: WEIGHT FREQUENCY
  # ===========================================================================
  
  # Update fishery choices when data loaded
  observe({
    req(rv$data_loaded, rv$WeightOut_list)
    
    # Get fisheries from all scenarios
    fisheries <- unique(unlist(lapply(rv$WeightOut_list[!sapply(rv$WeightOut_list, is.null)], 
                                      function(x) unique(x@wgtfits$fishery))))
    
    # Check if fisheries found
    if (length(fisheries) == 0) {
      updateSelectInput(session, "wf_fishery", choices = character(0))
      return()
    }
    
    # Create named choices
    fishery_map <- rv$FISHERY_MAPS[[1]]
    choices <- setNames(fisheries, 
                        sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    updateSelectInput(session, "wf_fishery", choices = choices)
  })
  
  # Reactive: generate weight frequency plot
  wf_plot_reactive <- reactive({
    req(rv$data_loaded, input$wf_fishery, input$wf_scenarios)
    
    # Check if any scenarios selected
    if (length(input$wf_scenarios) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
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
    
    # Separate observed data
    obs_data <- plot_data %>%
      group_by(year, weight) %>%
      summarise(obs = first(obs), .groups = "drop")
    
    fishery_name <- get_fishery_name(input$wf_fishery, rv$FISHERY_MAPS[[1]])
    
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
      labs(title = paste(fishery_name, "-",
                         paste(input$wf_scenarios, collapse = ", "),
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
  
  # Download WF plot as PNG
  output$download_wf_png <- downloadHandler(
    filename = function() {
      paste0("weight_freq_", input$wf_fishery, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = wf_plot_reactive(), width = 12, height = 9, dpi = 300, bg = "white")
    }
  )
  
  # Download WF plot as PDF
  output$download_wf_pdf <- downloadHandler(
    filename = function() {
      paste0("weight_freq_", input$wf_fishery, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      ggsave(file, plot = wf_plot_reactive(), width = 12, height = 9, device = "pdf")
    }
  )
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui, server)

