library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(CondorBox)

# ==================== UI ====================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Condor Job Manager"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Launch Jobs", tabName = "launch", icon = icon("rocket")),
      menuItem("Monitor Jobs", tabName = "monitor", icon = icon("chart-line")),
      menuItem("Retrieve Results", tabName = "retrieve", icon = icon("download")),
      menuItem("Edit Models", tabName = "edit", icon = icon("edit")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .box-body { font-size: 14px; }
        .btn-launch { width: 100%; margin-top: 10px; }
        .status-running { color: #3c8dbc; }
        .status-completed { color: #00a65a; }
        .status-failed { color: #dd4b39; }

        /* Model selector styling */
        .model-selector-container {
          background: #f9f9f9;
          border: 2px solid #ddd;
          border-radius: 4px;
          padding: 12px;
          min-height: 100px;
          max-height: 400px;
          overflow-y: auto;
          overflow-x: hidden;
        }

        .model-selector-container::-webkit-scrollbar {
          width: 12px;
        }
        .model-selector-container::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        .model-selector-container::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 10px;
        }
        .model-selector-container::-webkit-scrollbar-thumb:hover {
          background: #555;
        }

        .model-checkbox-item {
          padding: 5px 8px;
          margin: 3px 0;
          border-bottom: 1px solid #eee;
          transition: background 0.2s;
        }
        .model-checkbox-item:hover {
          background: #e8f4f8;
          border-radius: 3px;
        }
        .model-checkbox-item:last-child {
          border-bottom: none;
        }
        
        /* Fix Shiny checkbox styling */
        .model-checkbox-item .shiny-input-container {
          margin-bottom: 0;
        }
        .model-checkbox-item .checkbox {
          margin-top: 0;
          margin-bottom: 0;
        }

        .model-details-container {
          max-height: 400px;
          overflow-y: auto;
          overflow-x: hidden;
          padding-right: 10px;
        }

        .model-details-container::-webkit-scrollbar {
          width: 12px;
        }
        .model-details-container::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        .model-details-container::-webkit-scrollbar-thumb {
          background: #3c8dbc;
          border-radius: 10px;
        }
        .model-details-container::-webkit-scrollbar-thumb:hover {
          background: #2c6d8c;
        }

        .model-details-card {
          background: #ffffff;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 12px;
          margin: 8px 0;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .model-name-header {
          font-size: 15px;
          font-weight: bold;
          color: #3c8dbc;
          margin-bottom: 8px;
        }
        .model-param {
          font-size: 12px;
          margin: 3px 0;
          color: #555;
        }
        .model-desc {
          background: #e8f4f8;
          padding: 8px;
          margin: 8px 0;
          border-left: 3px solid #3c8dbc;
          font-style: italic;
          font-size: 12px;
        }
        .param-label { 
          font-weight: bold; 
          margin-top: 10px;
          margin-bottom: 5px;
        }
        .description-box {
          background: #e8f4f8;
          padding: 10px;
          border-left: 4px solid #3c8dbc;
          margin: 10px 0;
          font-style: italic;
        }
        .config-card {
          background: #f9f9f9;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 12px;
          margin-bottom: 12px;
        }
        .config-card:hover {
          background: #f0f0f0;
          cursor: pointer;
        }
        .job-history {
          background: #fff9e6;
          border-left: 4px solid #f39c12;
          padding: 8px;
          margin: 5px 0;
          font-size: 11px;
        }
        .search-box {
          margin-bottom: 10px;
        }
        .path-input-group {
          margin-bottom: 15px;
        }
        
        /* Fix Browse button alignment */
        .download-settings-content {
          display: flex;
          flex-direction: column;
          gap: 10px;
        }
        
        .download-path-row {
          display: flex;
          gap: 10px;
          align-items: flex-start;
        }
        
        .download-path-input {
          flex: 1;
        }
        
        .download-path-button {
          flex-shrink: 0;
          padding-top: 0;
        }
        
        .commands-preview {
          background: #f5f5f5;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          margin-top: 10px;
          font-family: monospace;
          font-size: 12px;
          color: #333;
        }
        .download-location-display {
          background: #e8f4f8;
          padding: 10px;
          border-radius: 4px;
          margin: 10px 0;
          font-family: monospace;
          font-size: 12px;
        }
        
        /* Folders selector container with scrolling */
        .folders-selector-container {
          background: #f9f9f9;
          border: 2px solid #ddd;
          border-radius: 4px;
          padding: 12px;
          max-height: 400px;
          overflow-y: auto;
          overflow-x: hidden;
        }
        
        .folders-selector-container::-webkit-scrollbar {
          width: 12px;
        }
        .folders-selector-container::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        .folders-selector-container::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 10px;
        }
        .folders-selector-container::-webkit-scrollbar-thumb:hover {
          background: #555;
        }
        
        .folder-checkbox-item {
          padding: 5px 8px;
          margin: 3px 0;
          border-bottom: 1px solid #eee;
          transition: background 0.2s;
          display: flex;
          align-items: center;
          justify-content: space-between;
        }
        .folder-checkbox-item:hover {
          background: #e8f4f8;
          border-radius: 3px;
        }
        .folder-checkbox-item:last-child {
          border-bottom: none;
        }
        
        .folder-left-content {
          display: flex;
          align-items: center;
          flex: 1;
          gap: 8px;
        }
        
        /* Fix folder checkbox styling */
        .folder-left-content .shiny-input-container {
          margin-bottom: 0;
          width: auto;
        }
        .folder-left-content .checkbox {
          margin-top: 0;
          margin-bottom: 0;
          padding-left: 0;
        }
        .folder-left-content .checkbox label {
          padding-left: 0;
          margin-bottom: 0;
        }
        
        .folder-name {
          font-weight: 500;
          color: #333;
        }
        
        .folder-files-count {
          color: #666;
          font-size: 11px;
          background: #e8f4f8;
          padding: 3px 10px;
          border-radius: 12px;
          white-space: nowrap;
        }
        
        /* Retrieval log scrollable */
        .retrieval-log-container {
          max-height: 300px;
          overflow-y: auto;
          background: #f5f5f5;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          font-family: monospace;
          font-size: 12px;
        }
        
        .retrieval-log-container::-webkit-scrollbar {
          width: 10px;
        }
        .retrieval-log-container::-webkit-scrollbar-track {
          background: #f1f1f1;
        }
        .retrieval-log-container::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 5px;
        }
        
        /* Fix Show Log checkbox in box title */
        .log-title-container {
          display: flex;
          justify-content: space-between;
          align-items: center;
          width: 100%;
        }
        
        .log-checkbox-wrapper {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .log-checkbox-wrapper .shiny-input-container {
          margin-bottom: 0;
        }
        
        .log-checkbox-wrapper .checkbox {
          margin: 0;
          padding: 0;
        }
        
        .log-checkbox-wrapper .checkbox label {
          margin: 0;
          padding-left: 20px;
          font-weight: normal;
        }
        
        /* Archive contents tree view */
        .archive-tree {
          background: #f9f9f9;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          margin: 10px 0;
          max-height: 400px;
          overflow-y: auto;
          font-family: monospace;
          font-size: 12px;
        }
        
        .tree-item {
          padding: 2px 0;
          color: #333;
        }
        
        .tree-folder {
          color: #3c8dbc;
          font-weight: bold;
        }
        
        .tree-file {
          color: #666;
        }
        
        .extract-path-input {
          background: #fff3cd;
          border: 2px solid #ffc107;
          border-radius: 4px;
          padding: 15px;
          margin: 10px 0;
        }
        
        .path-preview-box {
          background: #e8f4f8;
          padding: 10px;
          border-radius: 4px;
          margin-top: 10px;
          font-family: monospace;
          font-size: 13px;
        }
        
        /* Button loading state and spinner */
        .btn-launch.loading {
          background-color: #f39c12 !important;
          border-color: #f39c12 !important;
          cursor: wait !important;
          opacity: 0.8;
        }

        .spinner {
          border: 4px solid #f3f3f3;
          border-top: 4px solid #3c8dbc;
          border-radius: 50%;
          width: 50px;
          height: 50px;
          animation: spin-anim 1s linear infinite;
          margin: 0 auto 20px;
        }

        @keyframes spin-anim {
          from { transform: rotate(0deg); }
          to { transform: rotate(360deg); }
        }
      "))
    ),
    
    tabItems(
      # ========== LAUNCH JOBS TAB ==========
      tabItem(
        tabName = "launch",
        
        # ---- Config File Loader (inline) ----
        fluidRow(
          box(
            title = "Load Model Configuration", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(4,
                fileInput("launch_config_upload", "Upload R Script / RDS:",
                          accept = c(".R", ".r", ".rds", ".RDS"),
                          width = "100%")
              ),
              column(5,
                textInput("launch_config_path", "Or specify path:",
                          value = "../configs/set_model.R",
                          placeholder = "Path to set_model.R or saved .rds",
                          width = "100%")
              ),
              column(3,
                div(style = "margin-top: 25px;",
                  actionButton("launch_load_config", "Load Models",
                               icon = icon("folder-open"),
                               class = "btn-info btn-block")
                )
              )
            ),
            uiOutput("launch_config_status_ui")
          )
        ),
        
        fluidRow(
          box(
            title = "Job Configuration", status = "primary", solidHeader = TRUE, width = 6,
            
            selectInput("job_type", "Job Type:",
                        choices = c("Model" = "model", 
                                    "Jitter" = "jitter",
                                    "Hessian" = "hessian",
                                    "Retrospective" = "retro",
                                    "Profile" = "prof"),
                        selected = "model"),
            
            textInput("output_dir", "Output Directory:", 
                      value = "quick/test_run"),
            
            selectInput("branch", "Git Branch:",
                        choices = c("main", "develop", "develop_lik"),
                        selected = "develop_lik"),
            
            hr(),
            
            h4("Model Selection"),
            
            div(class = "search-box",
                textInput("model_search", NULL,
                          placeholder = "🔍 Search models...",
                          width = "100%")
            ),
            
            fluidRow(
              column(6,
                     actionButton("select_all_models", "Select All", 
                                  class = "btn-sm btn-success btn-block",
                                  icon = icon("check-square"))
              ),
              column(6,
                     actionButton("deselect_all_models", "Deselect All", 
                                  class = "btn-sm btn-warning btn-block",
                                  icon = icon("square"))
              )
            ),
            
            br(),
            
            uiOutput("model_selection_ui"),
            
            actionButton("launch_btn", "Launch Job(s)", 
                         class = "btn-primary btn-launch", 
                         icon = icon("rocket"))
          ),
          
          box(
            title = "Resource Configuration", status = "info", solidHeader = TRUE, width = 6,
            
            textInput("remote_user", "Remote User:", 
                      value = Sys.getenv("USER", "kyuhank")),
            
            textInput("remote_host", "Remote Host:", 
                      value = Sys.getenv("NOU_CONDOR", "")),
            
            textInput("github_username", "GitHub Username:", 
                      value = "kyuhank"),
            
            textInput("github_org", "GitHub Organization:", 
                      value = "PacificCommunity"),
            
            textInput("github_repo", "GitHub Repository:", 
                      value = "ofp-sam-2026-bet"),
            
            textInput("docker_image", "Docker Image:", 
                      value = "ghcr.io/pacificcommunity/bet-2026:v1.2"),
            
            hr(),
            
            sliderInput("condor_cpus", "CPUs:", 
                        min = 1, max = 16, value = 2, step = 1),
            
            selectInput("condor_memory", "Memory:",
                        choices = c("4GB", "6GB", "8GB", "10GB", "12GB", "16GB", "24GB"),
                        selected = "12GB"),
            
            selectInput("condor_disk", "Disk:",
                        choices = c("5GB", "10GB", "15GB", "20GB", "30GB"),
                        selected = "10GB")
          )
        ),
        
        fluidRow(
          box(
            title = "Selected Model Details", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            uiOutput("model_details_display")
          )
        ),
        
        fluidRow(
          box(
            title = "Launch Log", status = "success", solidHeader = TRUE, width = 12,
            verbatimTextOutput("launch_log")
          )
        )
      ),
      
      # ========== EDIT MODELS TAB ==========
      tabItem(
        tabName = "edit",
        fluidRow(
          box(
            title = "Manage Models", status = "warning", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(6,
                     selectInput("edit_model_select", "Select Model to Edit:",
                                 choices = NULL)
              ),
              column(3,
                     br(),
                     actionButton("add_new_model", "Add New Model", 
                                  class = "btn-success btn-block", icon = icon("plus"))
              ),
              column(3,
                     br(),
                     actionButton("save_config_btn", "Save Run Config", 
                                  class = "btn-info btn-block", icon = icon("file-export"))
              )
            ),
            
            hr(),
            
            div(class = "param-label", "Model Description:"),
            textAreaInput("edit_description", NULL,
                          placeholder = "Describe what this model does, changes from base model, etc.",
                          rows = 3, width = "100%"),
            
            uiOutput("model_editor_ui"),
            
            hr(),
            
            fluidRow(
              column(4,
                     actionButton("save_model", "Save Changes", 
                                  class = "btn-success btn-block", icon = icon("save"))
              ),
              column(4,
                     actionButton("reset_model", "Reset to Original", 
                                  class = "btn-warning btn-block", icon = icon("undo"))
              ),
              column(4,
                     actionButton("delete_model", "Delete Model", 
                                  class = "btn-danger btn-block", icon = icon("trash"))
              )
            )
          )
        )
      ),
      
      # ========== MONITOR JOBS TAB ==========
      tabItem(
        tabName = "monitor",
        fluidRow(
          box(
            title = "Job Status", status = "primary", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(2,
                     actionButton("refresh_jobs", "Refresh", 
                                  icon = icon("sync"),
                                  class = "btn-default btn-block")
              ),
              column(2,
                     actionButton("remove_selected_jobs", "Remove Selected", 
                                  icon = icon("trash"),
                                  class = "btn-danger btn-block")
              ),
              column(8,
                     div(id = "job_selection_info",
                         style = "padding: 8px; color: #666; font-size: 13px;",
                         textOutput("selected_jobs_info", inline = TRUE))
              )
            ),
            
            hr(),
            DTOutput("jobs_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Job Details", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("job_details")
          )
        )
      ),
      
      # ========== RETRIEVE RESULTS TAB ==========
      tabItem(
        tabName = "retrieve",
        fluidRow(
          box(
            title = "Scan Settings", status = "primary", solidHeader = TRUE, width = 6,
            
            div(class = "param-label", "Remote Output Directory:"),
            fluidRow(
              column(9,
                     textInput("scan_output_dir", NULL, 
                               value = "quick/test_run",
                               placeholder = "quick/test_run")
              ),
              column(3,
                     actionButton("browse_remote_output", "Browse", 
                                  class = "btn-default btn-sm btn-block",
                                  icon = icon("folder-open"))
              )
            ),
            
            br(),
            
            fluidRow(
              column(4,
                     actionButton("scan_results", "Scan Folders", 
                                  icon = icon("search"),
                                  class = "btn-info btn-block")
              ),
              column(4,
                     actionButton("download_all", "Download All", 
                                  icon = icon("download"),
                                  class = "btn-success btn-block")
              ),
              column(4,
                     actionButton("delete_remote_dir", "Delete Directory", 
                                  icon = icon("trash"),
                                  class = "btn-danger btn-block")
              )
            )
          ),
          
          box(
            title = "Download Settings", status = "info", solidHeader = TRUE, width = 6,
            
            div(class = "download-settings-content",
                div(
                  div(class = "param-label", "Local Download Location:"),
                  div(class = "download-path-row",
                      div(class = "download-path-input",
                          textInput("download_location", NULL, 
                                    value = "../model",
                                    placeholder = "../model")
                      ),
                      div(class = "download-path-button",
                          actionButton("browse_download_location", "Browse", 
                                       class = "btn-default btn-sm",
                                       icon = icon("folder-open"))
                      )
                  )
                ),
                
                div(class = "download-location-display",
                    icon("info-circle"), " Current download location:",
                    br(),
                    strong(textOutput("download_location_display", inline = TRUE))
                )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Select Specific Folders (Advanced)", status = "warning", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
            
            fluidRow(
              column(3,
                     actionButton("select_all_folders", "Select All", 
                                  class = "btn-sm btn-success",
                                  icon = icon("check-square"))
              ),
              column(3,
                     actionButton("deselect_all_folders", "Deselect All", 
                                  class = "btn-sm btn-warning",
                                  icon = icon("square"))
              ),
              column(3,
                     div(style = "padding: 5px;",
                         textOutput("selected_folders_count"))
              ),
              column(3,
                     actionButton("refresh_folders", "Refresh", 
                                  class = "btn-sm btn-default",
                                  icon = icon("sync"))
              )
            ),
            
            hr(),
            
            uiOutput("folders_selection_ui"),
            
            hr(),
            
            p(strong("Actions for Selected Folders:"), style = "margin-top: 15px; color: #666;"),
            
            fluidRow(
              column(6,
                     actionButton("fetch_selected", "Download Selected", 
                                  class = "btn-primary btn-block", 
                                  icon = icon("download"))
              ),
              column(6,
                     actionButton("delete_selected", "Delete Selected", 
                                  class = "btn-danger btn-block", 
                                  icon = icon("trash"))
              )
            ),
            
            br(),
            
            actionButton("preview_archives", "Preview Archive Contents", 
                         class = "btn-info btn-block", 
                         icon = icon("eye"))
          )
        ),
        
        fluidRow(
          box(
            title = "Extract Configuration", status = "warning", solidHeader = TRUE, width = 12,
            
            div(class = "extract-path-input",
                h4(icon("download"), " Specify Folder Path to Extract"),
                p("Specify the path within the archive to extract. You can modify the repository name if needed.", 
                  style = "color: #666; margin-bottom: 15px;"),
                
                fluidRow(
                  column(4,
                         textInput("extract_repo_name", 
                                   label = strong("Repository name:"),
                                   value = NULL,
                                   placeholder = "Auto-detected from settings")
                  ),
                  column(8,
                         textInput("extract_path_manual", 
                                   label = strong("Path within repository:"),
                                   value = "model",
                                   placeholder = "e.g., model, plots, mfcl/outputs, etc.",
                                   width = "100%")
                  )
                ),
                
                div(class = "path-preview-box",
                    strong(icon("info-circle"), " Full extraction path preview:"), br(),
                    code(textOutput("extract_full_path_preview", inline = TRUE)),
                    br(), br(),
                    strong(icon("arrow-down"), " Will be extracted to:"), br(),
                    code(textOutput("extract_target_preview", inline = TRUE))
                ),
                
                p(style = "color: #666; font-size: 12px; margin-top: 15px;",
                  icon("lightbulb"), " Examples:",
                  tags$ul(
                    tags$li(code("model"), " - extracts contents: archive/repo/model/* → ../model/[folder_name]/*"),
                    tags$li(code("plots"), " - extracts contents: archive/repo/plots/* → ../model/[folder_name]/*"),
                    tags$li(code("mfcl/outputs"), " - extracts contents: archive/repo/mfcl/outputs/* → ../model/[folder_name]/*"),
                    tags$li("Leave path empty to extract everything from repository root")
                  )
                )
            ),
            
          )
        ),
        
        fluidRow(
          box(
            title = div(
              class = "log-title-container",
              span("Retrieval Log"),
              div(
                class = "log-checkbox-wrapper",
                checkboxInput("show_retrieval_log", "Show Log", value = FALSE)
              )
            ),
            status = "info", solidHeader = TRUE, width = 12,
            collapsible = FALSE,
            
            conditionalPanel(
              condition = "input.show_retrieval_log == true",
              div(class = "retrieval-log-container",
                  verbatimTextOutput("retrieval_log")
              )
            )
          )
        )
      ),
      
      # ========== SETTINGS TAB ==========
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Load Pre-specified Models from R Script", status = "primary", solidHeader = TRUE, width = 6,
            
            p(strong("Load base model configurations"), 
              "from an R script (typically", code("set_model.R"), 
              ") that contains a", code("models"), "list object."),
            
            fileInput("config_file_upload", "Upload R script:",
                      accept = c(".R", ".r")),
            
            p("Or specify path:"),
            textInput("config_file_path", "R Script Path:", 
                      value = "../configs/set_model.R",
                      placeholder = "Path to your set_model.R file"),
            
            actionButton("reload_config", "Load Models from Script", 
                         icon = icon("sync"), class = "btn-info btn-block"),
            
            hr(),
            
            verbatimTextOutput("config_status")
          ),
          
          box(
            title = "Load Previous Run Configuration", status = "info", solidHeader = TRUE, width = 6,
            
            p(strong("Browse saved model run history."), 
              "These are configurations you saved after modifying models for specific analyses."),
            
            actionButton("refresh_saved_configs", "Refresh List", 
                         icon = icon("sync"), class = "btn-sm btn-default",
                         style = "margin-bottom: 10px;"),
            
            uiOutput("saved_configs_ui")
          )
        ),
        
        fluidRow(
          box(
            title = "Currently Loaded Models", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("models_summary")
          )
        )
      )
    )
  )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
  
  # ===== SETTINGS SAVE/LOAD =====
  # Settings file path
  settings_file <- "../configs/.last_settings.rds"
  
  # Load saved settings on startup
  observe({
    if (file.exists(settings_file)) {
      tryCatch({
        saved_settings <- readRDS(settings_file)
        
        if (!is.null(saved_settings$scan_output_dir)) {
          updateTextInput(session, "scan_output_dir", value = saved_settings$scan_output_dir)
        }
        
        if (!is.null(saved_settings$download_location)) {
          updateTextInput(session, "download_location", value = saved_settings$download_location)
        }
        
        if (!is.null(saved_settings$extract_repo_name)) {
          updateTextInput(session, "extract_repo_name", value = saved_settings$extract_repo_name)
        }
        
        if (!is.null(saved_settings$extract_path_manual)) {
          updateTextInput(session, "extract_path_manual", value = saved_settings$extract_path_manual)
        }
        
        if (!is.null(saved_settings$output_dir)) {
          updateTextInput(session, "output_dir", value = saved_settings$output_dir)
        }
        
        if (!is.null(saved_settings$branch)) {
          updateSelectInput(session, "branch", selected = saved_settings$branch)
        }
        
        if (!is.null(saved_settings$job_type)) {
          updateSelectInput(session, "job_type", selected = saved_settings$job_type)
        }
        
      }, error = function(e) {
        # If settings file is corrupted, ignore and use defaults
      })
    }
  })
  
  # Function to save settings
  save_settings <- function() {
    settings <- list(
      scan_output_dir = input$scan_output_dir,
      download_location = input$download_location,
      extract_repo_name = input$extract_repo_name,
      extract_path_manual = input$extract_path_manual,
      output_dir = input$output_dir,
      branch = input$branch,
      job_type = input$job_type,
      timestamp = Sys.time()
    )
    
    tryCatch({
      saveRDS(settings, settings_file)
    }, error = function(e) {
      # Silently fail if can't save
    })
  }
  
  # Auto-save settings when they change
  observeEvent(input$scan_output_dir, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$download_location, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$extract_repo_name, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$extract_path_manual, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$output_dir, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$branch, {
    save_settings()
  }, ignoreInit = TRUE)
  
  observeEvent(input$job_type, {
    save_settings()
  }, ignoreInit = TRUE)
  # ===== END SETTINGS SAVE/LOAD =====
  # Reactive values storage
  rv <- reactiveValues(
    models = list(),
    models_original = list(),
    launch_log = "",
    retrieval_log = "",
    folders_data = data.frame(),
    archive_contents = list(),
    selected_folders = c(),
    jobs_status = data.frame(
      Owner = character(),
      BatchName = character(),
      Submitted = character(),
      Done = character(),
      Run = character(),
      Idle = character(),
      Total = character(),
      JobIDs = character(),
      stringsAsFactors = FALSE
    ),
    config_loaded = FALSE,
    config_path = NULL,
    config_status_msg = "",
    base_config_name = "set_model.R",
    run_metadata = list(
      run_name = "",
      description = "",
      date = NULL,
      base_config = ""
    ),
    saved_configs_trigger = 0,
    current_config_file = NULL,
    selected_models = c()
  )
  
  # Initialize directories
  observe({
    if (!dir.exists("../configs")) {
      dir.create("../configs", recursive = TRUE)
    }
    if (!dir.exists("../configs/models_ran")) {
      dir.create("../configs/models_ran", recursive = TRUE)
    }
    
    model_dir <- normalizePath("../model", mustWork = FALSE)
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
  })
  
  # Browse download location
  observeEvent(input$browse_download_location, {
    tryCatch({
      start_path <- normalizePath("..", mustWork = FALSE)
      dirs <- list.dirs(start_path, recursive = FALSE, full.names = TRUE)
      
      showModal(modalDialog(
        title = "Select Download Location",
        size = "m",
        p(strong("Available directories:"), style = "margin-bottom: 10px;"),
        p(paste("Current location:", normalizePath(getwd())), 
          style = "color: #666; font-size: 11px; margin-bottom: 5px;"),
        p(paste("Browsing:", start_path), 
          style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
        
        div(
          style = "max-height: 400px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
          
          tags$div(
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('selected_download_path', '..', {priority: 'event'}); return false;",
              icon("folder", style = "color: #f39c12;"), " ..",
              style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px; font-weight: bold;"
            ),
            style = "padding: 5px 0; margin-bottom: 10px; border-bottom: 2px solid #ddd;"
          ),
          
          if (length(dirs) > 0) {
            lapply(dirs, function(d) {
              dir_name <- basename(d)
              rel_path <- gsub(paste0("^", normalizePath(getwd()), "/?"), "", d)
              
              tags$div(
                tags$a(
                  href = "#",
                  onclick = sprintf("Shiny.setInputValue('selected_download_path', '%s', {priority: 'event'}); return false;", rel_path),
                  icon("folder-open", style = "color: #3c8dbc;"), " ", dir_name,
                  style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
                ),
                style = "padding: 3px 0; padding-left: 20px; transition: background 0.2s;",
                onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px';",
                onmouseout = "this.style.background='';"
              )
            })
          } else {
            p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        hr(),
        textInput("download_manual_path", "Or enter path manually:",
                  value = input$download_location,
                  placeholder = "../model"),
        p(style = "color: #666; font-size: 12px;", 
          "Tip: Path relative to Shiny app location (e.g., ../model, ../output)"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_download_path", "Select", class = "btn-primary")
        )
      ))
      
    }, error = function(e) {
      showNotification(paste("Error browsing directories:", e$message), type = "error")
    })
  })
  
  observeEvent(input$selected_download_path, {
    req(input$selected_download_path)
    updateTextInput(session, "download_location", value = input$selected_download_path)
    removeModal()
    showNotification(paste("Selected:", input$selected_download_path), type = "message", duration = 2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_download_path, {
    if (!is.null(input$download_manual_path) && input$download_manual_path != "") {
      updateTextInput(session, "download_location", value = input$download_manual_path)
    }
    removeModal()
  })
  
  output$download_location_display <- renderText({
    dl_path <- input$download_location
    if (is.null(dl_path) || dl_path == "") {
      return("(not set - please browse or enter path)")
    }
    normalizePath(dl_path, mustWork = FALSE)
  })
  
  output$selected_folders_count <- renderText({
    paste(length(rv$selected_folders), "folders selected")
  })
  
  observe({
    updateTextInput(session, "extract_repo_name", 
                    placeholder = input$github_repo)
  })
  
  output$extract_full_path_preview <- renderText({
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- "[repository]"
    }
    
    extract_path <- trimws(input$extract_path_manual)
    
    if (is.null(extract_path) || extract_path == "") {
      return(paste0(repo_name, "/*  (everything from repository root)"))
    }
    
    paste0(repo_name, "/", extract_path, "/*")
  })
  
  output$extract_target_preview <- renderText({
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      return("(download location not set)")
    }
    paste0(normalizePath(download_dir, mustWork = FALSE), "/[folder_name]/*")
  })
  
  # ========== JOB HISTORY FUNCTIONS ==========
  
  get_job_history_file <- function(config_file) {
    if (is.null(config_file)) return(NULL)
    config_basename <- tools::file_path_sans_ext(basename(config_file))
    job_history_file <- file.path("../configs/models_ran", 
                                  paste0(config_basename, "_job_history.rds"))
    return(job_history_file)
  }
  
  load_job_history <- function(config_file) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file) || !file.exists(job_file)) {
      return(data.frame(
        timestamp = character(),
        job_type = character(),
        model_names = character(),
        output_dir = character(),
        batch_names = character(),
        remote_dirs = character(),
        branch = character(),
        status = character(),
        stringsAsFactors = FALSE
      ))
    }
    readRDS(job_file)
  }
  
  save_job_history <- function(config_file, job_record) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file)) return()
    history <- load_job_history(config_file)
    history <- rbind(history, job_record)
    saveRDS(history, job_file)
  }
  
  # ========== LOAD MODELS FUNCTION ==========
  
  load_models <- function(config_path = NULL, is_saved_run = FALSE) {
    current_wd <- getwd()
    possible_paths <- c()
    
    if (!is.null(config_path)) {
      possible_paths <- c(possible_paths, config_path)
    }
    
    if (!is.null(input$config_file_path) && input$config_file_path != "") {
      possible_paths <- c(possible_paths, input$config_file_path)
    }
    
    if (!is.null(input$launch_config_path) && input$launch_config_path != "") {
      possible_paths <- c(possible_paths, input$launch_config_path)
    }
    
    possible_paths <- c(
      possible_paths,
      "../configs/set_model.R",
      "configs/set_model.R",
      "set_model.R"
    )
    
    possible_paths <- unique(possible_paths[!is.na(possible_paths) & possible_paths != ""])
    
    rv$config_status_msg <- paste0(
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📁 Current Working Directory:\n",
      current_wd, "\n\n",
      "🔍 Searching for R script with models...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
    )
    
    found_path <- NULL
    for (path in possible_paths) {
      exists <- file.exists(path)
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     ifelse(exists, "✓ ", "✗ "),
                                     path, "\n")
      
      if (exists && is.null(found_path)) {
        found_path <- path
      }
    }
    
    if (is.null(found_path)) {
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     "\n❌ No R script with models found!\n",
                                     "Please provide a path to an R script containing a 'models' list object.")
      showNotification("Config file not found", type = "error")
      return(FALSE)
    }
    
    rv$config_status_msg <- paste0(rv$config_status_msg, 
                                   "\n✅ Found R script at:\n", found_path, "\n\n")
    
    tryCatch({
      if (grepl("\\.rds$|\\.RDS$", found_path)) {
        saved_data <- readRDS(found_path)
        rv$models <- saved_data$models
        rv$models_original <- saved_data$models
        rv$run_metadata <- saved_data$metadata
        rv$base_config_name <- saved_data$metadata$base_config
        rv$current_config_file <- found_path
        
        job_history <- load_job_history(found_path)
        
        rv$config_status_msg <- paste0(rv$config_status_msg,
                                       "✅ Loaded saved run configuration:\n",
                                       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                                       "Run Name: ", saved_data$metadata$run_name, "\n",
                                       "Description: ", saved_data$metadata$description, "\n",
                                       "Date: ", format(saved_data$metadata$date, "%Y-%m-%d %H:%M"), "\n",
                                       "Base Config: ", saved_data$metadata$base_config, "\n",
                                       "Created By: ", saved_data$metadata$created_by, "\n",
                                       "Models: ", length(rv$models), "\n")
        
        if (nrow(job_history) > 0) {
          rv$config_status_msg <- paste0(rv$config_status_msg,
                                         "\n📊 Job Run History (", nrow(job_history), " runs):\n")
          
          for (i in 1:min(5, nrow(job_history))) {
            row <- job_history[nrow(job_history) - i + 1, ]
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ", i, ". ", row$timestamp, " - ", row$job_type, 
                                           " (", row$model_names, ")\n",
                                           "     Output: ", row$output_dir, "\n",
                                           "     Branch: ", row$branch, "\n")
          }
          
          if (nrow(job_history) > 5) {
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ... and ", nrow(job_history) - 5, " more runs\n")
          }
        }
        
        rv$config_status_msg <- paste0(rv$config_status_msg,
                                       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      } else {
        env <- new.env()
        source(found_path, local = env)
        
        if (exists("models", envir = env)) {
          rv$models <- env$models
          rv$models_original <- env$models
          rv$base_config_name <- basename(found_path)
          rv$current_config_file <- NULL
          
          rv$config_status_msg <- paste0(rv$config_status_msg,
                                         "✅ Successfully loaded R script\n",
                                         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
                                         "Script: ", basename(found_path), "\n",
                                         "Models found: ", length(rv$models), "\n",
                                         "  - ", paste(names(rv$models), collapse = "\n  - "), "\n",
                                         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        } else {
          stop("No 'models' list object found in the R script.\nMake sure your script contains: models <- list(...)")
        }
      }
      
      rv$config_loaded <- TRUE
      rv$config_path <- found_path
      rv$selected_models <- names(rv$models)
      
      updateSelectInput(session, "edit_model_select", 
                        choices = names(rv$models),
                        selected = names(rv$models)[1])
      
      showNotification(paste("✓ Loaded", length(rv$models), "models"), type = "message")
      return(TRUE)
      
    }, error = function(e) {
      rv$config_status_msg <- paste0(rv$config_status_msg, "❌ Error loading R script:\n", e$message)
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
  }
  
  observe({
    if (!rv$config_loaded) {
      load_models()
    }
  })
  
  observeEvent(input$reload_config, { load_models() })
  
  observeEvent(input$config_file_upload, {
    req(input$config_file_upload)
    load_models(input$config_file_upload$datapath)
  })
  
  # ---- Launch tab config loader handlers ----
  observeEvent(input$launch_load_config, {
    load_models()
  })
  
  observeEvent(input$launch_config_upload, {
    req(input$launch_config_upload)
    load_models(input$launch_config_upload$datapath)
  })
  
  output$launch_config_status_ui <- renderUI({
    req(rv$config_loaded)
    n <- length(rv$models)
    src <- if (!is.null(rv$config_path)) basename(rv$config_path) else "unknown"
    
    descs <- sapply(names(rv$models), function(nm) {
      m <- rv$models[[nm]]
      d <- if (!is.null(m$description) && m$description != "") m$description else "설명 없음"
      paste0(nm, ": ", d)
    })
    
    div(style = "margin-top: 4px; padding: 6px 10px; background: #f0f9f0; border-left: 3px solid #28a745; border-radius: 4px;",
      tags$span(style = "color: #28a745; font-weight: bold;",
        icon("check-circle"),
        paste0(" ", n, " model", if(n != 1) "s", " loaded from ", src)
      ),
      tags$small(style = "display: block; color: #666; margin-top: 2px;",
        HTML(paste(descs, collapse = " &nbsp;|&nbsp; "))
      )
    )
  })
  
  observeEvent(input$refresh_saved_configs, {
    rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
    showNotification("Refreshed saved configurations list", type = "message", duration = 2)
  })
  
  observeEvent(input$select_all_models, {
    rv$selected_models <- names(rv$models)
  })
  
  observeEvent(input$deselect_all_models, {
    rv$selected_models <- c()
  })
  
  # ========== MODEL SELECTION UI ==========
  
  filtered_models <- reactive({
    if (length(rv$models) == 0) return(character(0))
    
    search_term <- input$model_search
    model_names <- names(rv$models)
    
    if (is.null(search_term) || search_term == "") {
      return(model_names)
    }
    
    grep(search_term, model_names, ignore.case = TRUE, value = TRUE)
  })
  
  output$model_selection_ui <- renderUI({
    if (length(rv$models) == 0) {
      return(div(
        p("No models loaded.", style = "color: red; font-weight: bold;"),
        p("Go to Settings tab to load config file.")
      ))
    }
    
    visible_models <- filtered_models()
    
    if (length(visible_models) == 0) {
      return(div(
        class = "model-selector-container",
        p("No models match your search.", style = "color: #999; font-style: italic; text-align: center; padding: 20px;")
      ))
    }
    
    model_checkboxes <- lapply(visible_models, function(model_name) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      
      observeEvent(input[[checkbox_id]], {
        if (input[[checkbox_id]]) {
          if (!(model_name %in% rv$selected_models)) {
            rv$selected_models <- c(rv$selected_models, model_name)
          }
        } else {
          rv$selected_models <- rv$selected_models[rv$selected_models != model_name]
        }
      }, ignoreInit = TRUE)
      
      div(
        class = "model-checkbox-item",
        checkboxInput(
          checkbox_id,
          label = model_name,
          value = model_name %in% rv$selected_models
        )
      )
    })
    
    div(
      class = "model-selector-container",
      model_checkboxes
    )
  })
  
  observe({
    req(length(rv$models) > 0)
    
    visible_models <- filtered_models()
    
    for (model_name in visible_models) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      is_selected <- model_name %in% rv$selected_models
      updateCheckboxInput(session, checkbox_id, value = is_selected)
    }
  })
  
  output$model_details_display <- renderUI({
    if (length(rv$selected_models) == 0) {
      return(p("No models selected. Please select at least one model above.", 
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    model_cards <- lapply(rv$selected_models, function(model_name) {
      m <- rv$models[[model_name]]
      
      div(
        class = "model-details-card",
        div(class = "model-name-header", 
            icon("cube"), " ", model_name),
        
        if (!is.null(m$description) && m$description != "") {
          div(class = "model-desc", m$description)
        } else {
          div(class = "model-desc", style = "color: #999; font-style: italic;", "No description provided")
        },
        
        div(class = "model-param", 
            strong("Program: "), m$program_path),
        div(class = "model-param", 
            strong("Base Dir: "), m$base_dir),
        div(class = "model-param", 
            strong("Commands: "), m$mfcl_commands),
        
        hr(style = "margin: 8px 0;"),
        
        fluidRow(
          column(6,
                 div(class = "model-param", 
                     strong(icon("random"), " Jitter Seeds: "), m$jitter_seeds),
                 div(class = "model-param", 
                     strong(icon("history"), " Retro Peels: "), m$retro_peels)
          ),
          column(6,
                 div(class = "model-param", 
                     strong(icon("calculator"), " Hessian Splits: "), m$nsplit),
                 div(class = "model-param", 
                     strong(icon("chart-line"), " Profile Scalers: "), m$scalers),
                 div(class = "model-param", 
                     strong(icon("repeat"), " Profile Reps: "), m$Reps)
          )
        )
      )
    })
    
    div(
      class = "model-details-container",
      model_cards
    )
  })
  
  # ========== BROWSE REMOTE OUTPUT DIRECTORY ==========
  
  observeEvent(input$browse_remote_output, {
    showNotification("Loading remote directories...", type = "message", duration = 2)
    
    tryCatch({
      base_path <- input$github_repo
      find_cmd <- sprintf("find %s -maxdepth 2 -mindepth 1 -type d | sort", base_path)
      cmd <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      result <- system(cmd, intern = TRUE)
      
      if (length(result) > 0) {
        dirs <- gsub(paste0("^", base_path, "/?"), "", result)
        dirs <- dirs[nzchar(dirs)]
        
        dir_items <- lapply(dirs, function(d) {
          depth <- length(strsplit(d, "/")[[1]])
          indent <- paste(rep("&nbsp;&nbsp;&nbsp;&nbsp;", depth - 1), collapse = "")
          dir_name <- basename(d)
          
          list(
            path = d,
            depth = depth,
            indent = indent,
            name = dir_name
          )
        })
        
        showModal(modalDialog(
          title = "Select Remote Output Directory",
          size = "l",
          p(strong("Available directories on remote:"), style = "margin-bottom: 10px;"),
          p(paste("Base path:", base_path), style = "color: #666; font-size: 12px; margin-bottom: 15px;"),
          div(
            style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
            if (length(dirs) > 0) {
              lapply(dir_items, function(item) {
                icon_type <- if (item$depth == 1) "folder" else "folder-open"
                icon_color <- if (item$depth == 1) "#f39c12" else if (item$depth == 2) "#3c8dbc" else "#95a5a6"
                
                tags$div(
                  HTML(item$indent),
                  tags$a(
                    href = "#", 
                    onclick = sprintf(
                      "Shiny.setInputValue('selected_remote_path', '%s', {priority: 'event'}); return false;", 
                      item$path
                    ),
                    icon(icon_type, style = paste0("color: ", icon_color, ";")), " ", item$name,
                    style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
                  ),
                  style = "padding: 3px 0; transition: background 0.2s;",
                  onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                  onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
                )
              })
            } else {
              p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
            }
          ),
          hr(),
          textInput("remote_output_manual", "Or enter path manually:", 
                    value = input$scan_output_dir,
                    placeholder = "e.g., quick/test_run, output/models"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_remote_output", "Select", class = "btn-primary")
          )
        ))
      } else {
        showNotification("No directories found", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$selected_remote_path, {
    req(input$selected_remote_path)
    updateTextInput(session, "scan_output_dir", value = input$selected_remote_path)
    removeModal()
    showNotification(paste("Selected:", input$selected_remote_path), type = "message", duration = 2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_remote_output, {
    req(input$remote_output_manual)
    if (input$remote_output_manual != "") {
      updateTextInput(session, "scan_output_dir", value = input$remote_output_manual)
    }
    removeModal()
  })
  observe({
    # Get all inputs that start with select_dir_
    dir_inputs <- names(input)
    dir_inputs <- dir_inputs[grepl("^select_dir_", dir_inputs)]
    
    for (input_name in dir_inputs) {
      local({
        local_input <- input_name
        observeEvent(input[[local_input]], {
          selected_path <- input[[local_input]]
          updateTextInput(session, "scan_output_dir", value = selected_path)
          removeModal()
          showNotification(paste("Selected:", selected_path), type = "message", duration = 2)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$confirm_remote_output, {
    if (!is.null(input$remote_output_manual) && input$remote_output_manual != "") {
      updateTextInput(session, "scan_output_dir", value = input$remote_output_manual)
    }
    removeModal()
  })
  
  
  
  # ========== RETRIEVE RESULTS HANDLERS ==========
  
  
  # Download All - downloads entire directory without scanning individual folders
  observeEvent(input$download_all, {
    scan_dir <- input$scan_output_dir
    
    if (is.null(scan_dir) || scan_dir == "") {
      showNotification("Please specify a remote output directory first", type = "warning")
      return()
    }
    
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      showNotification("Please specify a download location first", type = "warning")
      return()
    }
    
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      showNotification("Repository name not specified", type = "error")
      return()
    }
    
    extract_subpath <- trimws(input$extract_path_manual)
    if (extract_subpath == "") {
      extract_path <- repo_name
    } else {
      extract_path <- paste0(repo_name, "/", extract_subpath)
    }
    
    if (!dir.exists(download_dir)) {
      dir.create(download_dir, recursive = TRUE)
    }
    
    remote_path <- paste0(input$github_repo, "/", scan_dir)
    
    rv$retrieval_log <- paste0(Sys.time(), " - Download All Started\n",
                               "   Remote: ", remote_path, "\n",
                               "   Local: ", normalizePath(download_dir, mustWork = FALSE), "\n",
                               "   Extract path: ", extract_path, "\n\n")
    
    showNotification("Downloading all files... This may take a while", 
                     type = "message", duration = NULL, id = "download_all_progress")
    
    tryCatch({
      find_cmd <- sprintf('find %s -name "*.tar.gz" -o -name "*.tgz"', remote_path)
      ssh_find <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      tar_files <- system(ssh_find, intern = TRUE)
      
      if (length(tar_files) == 0) {
        rv$retrieval_log <- paste0(rv$retrieval_log, "✗ No tar.gz files found\n")
        removeNotification("download_all_progress")
        showNotification("No tar.gz files found in directory", type = "warning")
        return()
      }
      
      rv$retrieval_log <- paste0(rv$retrieval_log, "✓ Found ", length(tar_files), " tar.gz files\n\n")
      
      for (i in seq_along(tar_files)) {
        tar_file <- tar_files[i]
        tar_name <- basename(tar_file)
        folder_name <- basename(dirname(tar_file))
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "[", i, "/", length(tar_files), "] ", 
                                   folder_name, "/", tar_name, "\n")
        
        temp_dir <- file.path(tempdir(), 
                              paste0("condor_all_", gsub("[^a-zA-Z0-9]", "_", folder_name), 
                                     "_", format(Sys.time(), "%H%M%S")))
        if (!dir.exists(temp_dir)) {
          dir.create(temp_dir, recursive = TRUE)
        }
        
        remote_tar <- sprintf("%s@%s:%s", input$remote_user, input$remote_host, tar_file)
        local_tar <- file.path(temp_dir, tar_name)
        
        rsync_cmd <- sprintf('rsync -avz --progress "%s" "%s"', remote_tar, local_tar)
        system(rsync_cmd)
        
        extract_cmd <- sprintf('tar -xzf "%s" -C "%s"', local_tar, temp_dir)
        system(extract_cmd)
        
        source_path <- file.path(temp_dir, extract_path)
        
        if (dir.exists(source_path)) {
          items_in_source <- list.files(source_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
          
          if (length(items_in_source) == 1 && file.info(items_in_source[1])$isdir) {
            single_folder_name <- basename(items_in_source[1])
            if (single_folder_name == folder_name) {
              items_in_source <- list.files(items_in_source[1], full.names = TRUE, all.files = TRUE, no.. = TRUE)
            }
          }
          
          if (length(items_in_source) > 0) {
            target_dir <- file.path(download_dir, folder_name)
            if (!dir.exists(target_dir)) {
              dir.create(target_dir, recursive = TRUE)
            }
            
            for (item in items_in_source) {
              item_name <- basename(item)
              target_item <- file.path(target_dir, item_name)
              
              if (file.info(item)$isdir) {
                if (dir.exists(target_item)) {
                  unlink(target_item, recursive = TRUE)
                }
                system(sprintf('cp -r "%s" "%s"', item, target_item))
              } else {
                file.copy(item, target_item, overwrite = TRUE)
              }
            }
            
            rv$retrieval_log <- paste0(rv$retrieval_log, 
                                       "   ✓ ", length(items_in_source), " items → ", target_dir, "\n")
          }
        } else {
          rv$retrieval_log <- paste0(rv$retrieval_log, "   ⚠ Path not found: ", extract_path, "\n")
        }
        
        unlink(temp_dir, recursive = TRUE)
      }
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "\n", strrep("=", 70), "\n",
                                 "✅ Download All Complete!\n",
                                 "   Processed: ", length(tar_files), " archives\n",
                                 "   Location: ", normalizePath(download_dir, mustWork = FALSE), "\n",
                                 strrep("=", 70), "\n")
      
      removeNotification("download_all_progress")
      showNotification("✓ Download complete!", type = "message", duration = 5)
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "\n✗ ERROR: ", e$message, "\n")
      removeNotification("download_all_progress")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$scan_results, {
    rv$retrieval_log <- paste0(Sys.time(), " - Scanning remote directory...\n")
    
    tryCatch({
      scan_path <- paste0(input$github_repo, "/", input$scan_output_dir)
      
      find_cmd <- sprintf("find %s -maxdepth 1 -mindepth 1 -type d", scan_path)
      cmd <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      folders <- system(cmd, intern = TRUE)
      
      if (length(folders) == 0) {
        rv$folders_data <- data.frame()
        rv$selected_folders <- c()
        rv$retrieval_log <- paste0(rv$retrieval_log, "⚠ No folders found\n")
        showNotification("No folders found", type = "warning")
        return()
      }
      
      folder_info <- lapply(folders, function(folder_path) {
        folder_name <- basename(folder_path)
        
        count_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\) | wc -l", 
                             folder_path)
        count_full_cmd <- sprintf('ssh %s@%s "%s"', 
                                  input$remote_user, input$remote_host, count_cmd)
        file_count <- as.numeric(system(count_full_cmd, intern = TRUE))
        
        data.frame(
          Folder = folder_name,
          Path = folder_path,
          TarGzFiles = file_count,
          stringsAsFactors = FALSE
        )
      })
      
      rv$folders_data <- do.call(rbind, folder_info)
      
      # Select all by default
      rv$selected_folders <- rv$folders_data$Folder
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "✓ Found ", nrow(rv$folders_data), " folders with ", 
                                 sum(rv$folders_data$TarGzFiles), " tar.gz files total\n",
                                 "All folders selected by default\n")
      showNotification(paste("Found", nrow(rv$folders_data), "folders (all selected)"), type = "message")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  
  # ========== DELETE REMOTE DIRECTORY ==========
  observeEvent(input$delete_remote_dir, {
    remote_dir <- input$scan_output_dir
    
    if (is.null(remote_dir) || remote_dir == "") {
      showNotification("Please specify a remote directory", type = "warning")
      return()
    }
    
    full_path <- paste0(input$github_repo, "/", remote_dir)
    
    showModal(modalDialog(
      title = "Delete Remote Directory",
      p(strong("Delete entire directory:"), style = "margin-bottom: 10px;"),
      p(code(full_path), style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 13px;"),
      p(strong("This will delete ALL files and folders!"), 
        style = "color: #dc3545; margin-top: 15px;"),
      p("This action cannot be undone.", style = "color: #666;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_remote_dir", "Yes, Delete", 
                     class = "btn-danger", icon = icon("trash"))
      )
    ))
  })
  
  observeEvent(input$confirm_delete_remote_dir, {
    removeModal()
    
    remote_dir <- input$scan_output_dir
    full_path <- paste0(input$github_repo, "/", remote_dir)
    
    rv$retrieval_log <- paste0(
      "\n", strrep("=", 70), "\n",
      Sys.time(), " - DELETING REMOTE DIRECTORY\n",
      strrep("=", 70), "\n",
      "Target: ", full_path, "\n"
    )
    
    showNotification(
      paste("Deleting:", remote_dir),
      type = "warning",
      duration = NULL,
      id = "delete_progress"
    )
    
    tryCatch({
      delete_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", 
                            input$remote_user, 
                            input$remote_host, 
                            full_path)
      
      result <- system(delete_cmd, intern = FALSE)
      
      if (result == 0) {
        verify_cmd <- sprintf("ssh %s@%s '[ ! -d %s ] && echo DELETED || echo EXISTS'", 
                              input$remote_user, 
                              input$remote_host, 
                              full_path)
        
        verify_result <- system(verify_cmd, intern = TRUE)
        
        if (length(verify_result) > 0 && verify_result[1] == "DELETED") {
          rv$retrieval_log <- paste0(rv$retrieval_log,
                                     "✓ Successfully deleted\n",
                                     strrep("=", 70), "\n\n")
          
          rv$folders_data <- data.frame()
          rv$selected_folders <- c()
          
          removeNotification("delete_progress")
          showNotification("✓ Directory deleted", type = "message", duration = 3)
        } else {
          rv$retrieval_log <- paste0(rv$retrieval_log, "⚠ Verification failed\n\n")
          removeNotification("delete_progress")
          showNotification("Deletion may have failed", type = "warning", duration = 5)
        }
      } else {
        rv$retrieval_log <- paste0(rv$retrieval_log, "✗ Deletion failed\n\n")
        removeNotification("delete_progress")
        showNotification("Failed to delete", type = "error", duration = 5)
      }
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n\n")
      removeNotification("delete_progress")
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })
  
  observeEvent(input$refresh_folders, {
    if (nrow(rv$folders_data) > 0) {
      showNotification("Refreshing folder list...", type = "message", duration = 1)
    }
  })
  
  # Render folders checkboxes
  output$folders_selection_ui <- renderUI({
    if (nrow(rv$folders_data) == 0) {
      return(p("No folders available. Click 'Scan Remote Directory' first.", 
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    folder_checkboxes <- lapply(1:nrow(rv$folders_data), function(i) {
      folder_name <- rv$folders_data$Folder[i]
      tar_count <- rv$folders_data$TarGzFiles[i]
      
      checkbox_id <- paste0("folder_check_", gsub("[^a-zA-Z0-9]", "_", folder_name))
      
      observeEvent(input[[checkbox_id]], {
        if (input[[checkbox_id]]) {
          if (!(folder_name %in% rv$selected_folders)) {
            rv$selected_folders <- c(rv$selected_folders, folder_name)
          }
        } else {
          rv$selected_folders <- rv$selected_folders[rv$selected_folders != folder_name]
        }
      }, ignoreInit = TRUE)
      
      div(
        class = "folder-checkbox-item",
        div(class = "folder-left-content",
            checkboxInput(
              checkbox_id,
              label = folder_name,
              value = folder_name %in% rv$selected_folders
            )
        ),
        span(class = "folder-files-count", paste(tar_count, "files"))
      )
    })
    
    div(
      class = "folders-selector-container",
      folder_checkboxes
    )
  })
  
  # Update folder checkboxes when selection changes
  observe({
    req(nrow(rv$folders_data) > 0)
    
    for (i in 1:nrow(rv$folders_data)) {
      folder_name <- rv$folders_data$Folder[i]
      checkbox_id <- paste0("folder_check_", gsub("[^a-zA-Z0-9]", "_", folder_name))
      is_selected <- folder_name %in% rv$selected_folders
      updateCheckboxInput(session, checkbox_id, value = is_selected)
    }
  })
  
  observeEvent(input$select_all_folders, {
    if (nrow(rv$folders_data) > 0) {
      rv$selected_folders <- rv$folders_data$Folder
    }
  })
  
  observeEvent(input$deselect_all_folders, {
    rv$selected_folders <- c()
  })
  
  observeEvent(input$preview_archives, {
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    rv$retrieval_log <- paste0(rv$retrieval_log, Sys.time(), " - Loading archive contents preview...\n")
    
    tryCatch({
      selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
      
      rv$archive_contents <- list()
      
      for (i in 1:nrow(selected_data)) {
        folder_name <- selected_data$Folder[i]
        folder_path <- selected_data$Path[i]
        
        rv$retrieval_log <- paste0(rv$retrieval_log, "\n📁 ", folder_name, ":\n")
        
        find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", 
                                folder_path)
        cmd <- sprintf('ssh %s@%s "%s"', 
                       input$remote_user, input$remote_host, find_tar_cmd)
        tar_files <- system(cmd, intern = TRUE)
        
        if (length(tar_files) == 0) next
        
        folder_contents <- list()
        
        for (tar_file in tar_files) {
          tar_name <- basename(tar_file)
          
          list_cmd <- sprintf('ssh %s@%s "tar -tzf %s"', 
                              input$remote_user, input$remote_host, tar_file)
          contents <- system(list_cmd, intern = TRUE)
          
          folder_contents[[tar_name]] <- contents
          
          rv$retrieval_log <- paste0(rv$retrieval_log, 
                                     "  ", tar_name, ": ", 
                                     length(contents), " items\n")
        }
        
        rv$archive_contents[[folder_name]] <- folder_contents
      }
      
      rv$retrieval_log <- paste0(rv$retrieval_log, "\n✅ Preview loaded\n")
      
      showModal(modalDialog(
        title = HTML(paste(icon("eye"), " Archive Contents Preview")),
        size = "l",
        
        p("Below is the complete folder structure inside the selected archives.", 
          style = "color: #666; font-size: 13px; margin-bottom: 15px;"),
        
        lapply(names(rv$archive_contents), function(folder_name) {
          folder_data <- rv$archive_contents[[folder_name]]
          
          tagList(
            h5(icon("folder"), " ", folder_name, style = "color: #3c8dbc; margin-top: 15px;"),
            
            lapply(names(folder_data), function(tar_name) {
              items <- folder_data[[tar_name]]
              
              div(
                style = "margin-left: 15px; margin-bottom: 10px;",
                strong(icon("file-archive"), " ", tar_name),
                div(
                  class = "archive-tree",
                  pre(paste(items[1:min(100, length(items))], collapse = "\n")),
                  if (length(items) > 100) {
                    p(paste("... and", length(items) - 100, "more items"), 
                      style = "color: #999; font-style: italic; margin: 5px 0;")
                  }
                )
              )
            })
          )
        }),
        
        footer = modalButton("Close")
      ))
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Download and extract
  observeEvent(input$fetch_selected, {
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      showNotification("Repository name not specified", type = "error")
      return()
    }
    
    extract_subpath <- trimws(input$extract_path_manual)
    
    if (extract_subpath == "") {
      extract_path <- repo_name
    } else {
      extract_path <- paste0(repo_name, "/", extract_subpath)
    }
    
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      download_dir <- "../model"
    }
    
    if (!dir.exists(download_dir)) {
      dir.create(download_dir, recursive = TRUE)
    }
    
    rv$retrieval_log <- paste0(Sys.time(), " - Starting extraction from ", extract_path, "/\n",
                               "Target base: ", download_dir, "/\n\n")
    
    tryCatch({
      selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
      
      for (i in 1:nrow(selected_data)) {
        folder_name <- selected_data$Folder[i]
        folder_path <- selected_data$Path[i]
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "📁 Processing: ", folder_name, "\n")
        
        find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", 
                                folder_path)
        cmd <- sprintf('ssh %s@%s "%s"', 
                       input$remote_user, input$remote_host, find_tar_cmd)
        tar_files <- system(cmd, intern = TRUE)
        
        if (length(tar_files) == 0) {
          rv$retrieval_log <- paste0(rv$retrieval_log, "  ⚠ No tar.gz files found\n\n")
          next
        }
        
        for (tar_file in tar_files) {
          remote_path <- sprintf("%s@%s:%s", 
                                 input$remote_user, 
                                 input$remote_host, 
                                 tar_file)
          
          temp_dir <- file.path(tempdir(), paste0("condor_extract_", gsub("[^a-zA-Z0-9]", "_", folder_name), "_", format(Sys.time(), "%H%M%S")))
          if (!dir.exists(temp_dir)) {
            dir.create(temp_dir, recursive = TRUE)
          }
          
          tar_name <- basename(tar_file)
          local_tar_path <- file.path(temp_dir, tar_name)
          
          rsync_cmd <- sprintf("rsync -avz --progress '%s' '%s'", remote_path, local_tar_path)
          system(rsync_cmd)
          
          extract_cmd <- sprintf("tar -xzf '%s' -C '%s'", local_tar_path, temp_dir)
          system(extract_cmd)
          
          source_path <- file.path(temp_dir, extract_path)
          
          if (dir.exists(source_path)) {
            items_in_source <- list.files(source_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
            
            # Check if there's a single folder with same name as target
            if (length(items_in_source) == 1 && file.info(items_in_source[1])$isdir) {
              single_folder_name <- basename(items_in_source[1])
              
              if (single_folder_name == folder_name) {
                items_in_source <- list.files(items_in_source[1], full.names = TRUE, all.files = TRUE, no.. = TRUE)
              }
            }
            
            if (length(items_in_source) > 0) {
              target_dir <- file.path(download_dir, folder_name)
              
              if (!dir.exists(target_dir)) {
                dir.create(target_dir, recursive = TRUE)
              }
              
              for (item in items_in_source) {
                item_name <- basename(item)
                target_item <- file.path(target_dir, item_name)
                
                if (file.info(item)$isdir) {
                  if (dir.exists(target_item)) {
                    unlink(target_item, recursive = TRUE)
                  }
                  system(sprintf("cp -r '%s' '%s'", item, target_item))
                } else {
                  file.copy(item, target_item, overwrite = TRUE)
                }
              }
              
              rv$retrieval_log <- paste0(rv$retrieval_log, 
                                         "  ✓ Extracted ", length(items_in_source), " items → ", target_dir, "/\n\n")
            }
          }
          
          unlink(temp_dir, recursive = TRUE)
        }
      }
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "✅ Extraction complete!\n",
                                 "   📂 Location: ", normalizePath(download_dir, mustWork = FALSE), "/\n",
                                 "   📁 Folders: ", paste(selected_data$Folder, collapse = ", "), "\n")
      showNotification("Extraction complete!", type = "message", duration = 5)
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$delete_selected, {
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
    total_files <- sum(selected_data$TarGzFiles)
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Delete", nrow(selected_data), "folders with", 
            total_files, "tar.gz files from remote server?"),
      tags$ul(
        lapply(selected_data$Folder, function(f) tags$li(f))
      ),
      p(strong("Warning:"), "This action cannot be undone!", style = "color: red;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_results", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_results, {
    removeModal()
    
    selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
    rv$retrieval_log <- paste0(Sys.time(), " - Deleting selected folders...\n")
    
    tryCatch({
      for (i in 1:nrow(selected_data)) {
        folder_name <- selected_data$Folder[i]
        folder_path <- selected_data$Path[i]
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "\n📁 Deleting folder: ", folder_name, "\n")
        
        delete_cmd <- sprintf('ssh %s@%s "rm -rf %s"', 
                              input$remote_user, 
                              input$remote_host, 
                              folder_path)
        
        system(delete_cmd)
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "  ✓ Deleted: ", folder_name, "\n")
      }
      
      rv$folders_data <- rv$folders_data[!(rv$folders_data$Folder %in% rv$selected_folders), ]
      rv$selected_folders <- c()
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "\n✅ Deletion complete!\n")
      showNotification("Deletion complete!", type = "message")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Retrieval log output
  output$retrieval_log <- renderText({
    rv$retrieval_log
  })
  
  # ========== MODEL EDITING HANDLERS ==========
  
  # Browse program path - only show mfcl/exe directory
  observeEvent(input$browse_program, {
    tryCatch({
      # Fixed path to mfcl/exe
      target_path <- normalizePath("../mfcl/exe", mustWork = FALSE)
      
      if (!dir.exists(target_path)) {
        showNotification("Directory ../mfcl/exe not found", type = "warning")
        return()
      }
      
      # Get all files in mfcl/exe
      all_files <- list.files(target_path, full.names = TRUE)
      
      # Filter executable files or .sh files
      exec_files <- all_files[file.access(all_files, 1) == 0 | grepl("\\.sh$", all_files)]
      
      # Convert to relative paths
      current_dir <- normalizePath(getwd())
      exec_files_rel <- sapply(exec_files, function(f) {
        gsub(paste0("^", current_dir, "/?"), "", f)
      })
      
      showModal(modalDialog(
        title = "Select Program File",
        size = "l",
        p(strong("Executable files in mfcl/exe:"), style = "margin-bottom: 10px;"),
        p(paste("Location:", target_path), 
          style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
        
        div(
          style = "max-height: 400px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
          if (length(exec_files_rel) > 0) {
            lapply(exec_files_rel, function(f) {
              file_name <- basename(f)
              tags$div(
                tags$a(
                  href = "#",
                  onclick = sprintf("document.getElementById('edit_program_path').value = '%s'; Shiny.setInputValue('close_program_modal', Math.random()); return false;", f),
                  icon("file-code", style = "color: #e74c3c;"), " ", file_name,
                  style = "color: #333; cursor: pointer; text-decoration: none; font-size: 12px;"
                ),
                style = "padding: 3px 0; transition: background 0.2s;",
                onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
              )
            })
          } else {
            p("No executable files found", style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        hr(),
        textInput("program_manual_path", "Or enter path manually:",
                  value = input$edit_program_path,
                  placeholder = "mfcl/exe/mfclo64 or ./doitall.sh"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_program_path", "Select", class = "btn-primary")
        )
      ))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$close_program_modal, {
    removeModal()
  })
  
  observeEvent(input$confirm_program_path, {
    if (!is.null(input$program_manual_path) && input$program_manual_path != "") {
      updateTextInput(session, "edit_program_path", value = input$program_manual_path)
    }
    removeModal()
  })
  
  # Browse base directory - only show mfcl/inputs subdirectories
  observeEvent(input$browse_basedir, {
    tryCatch({
      # Fixed path to mfcl/inputs
      target_path <- normalizePath("../mfcl/inputs", mustWork = FALSE)
      
      if (!dir.exists(target_path)) {
        showNotification("Directory ../mfcl/inputs not found", type = "warning")
        return()
      }
      
      # Get all subdirectories in mfcl/inputs
      subdirs <- list.dirs(target_path, recursive = FALSE, full.names = TRUE)
      
      # Convert to relative paths
      current_dir <- normalizePath(getwd())
      subdirs_relative <- sapply(subdirs, function(d) {
        gsub(paste0("^", current_dir, "/?"), "", d)
      })
      
      # Sort directories
      subdirs_relative <- sort(subdirs_relative)
      
      showModal(modalDialog(
        title = "Select Base Directory",
        size = "l",
        p(strong("Available directories in mfcl/inputs:"), style = "margin-bottom: 10px;"),
        p(paste("Location:", target_path), 
          style = "color: #666; font-size: 11px; margin-bottom: 15px;"),
        
        div(
          style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
          if (length(subdirs_relative) > 0) {
            lapply(subdirs_relative, function(d) {
              dir_name <- basename(d)
              tags$div(
                tags$a(
                  href = "#",
                  onclick = sprintf("document.getElementById('edit_base_dir').value = '%s'; Shiny.setInputValue('close_basedir_modal', Math.random()); return false;", d),
                  icon("folder", style = "color: #3c8dbc;"), " ", dir_name,
                  style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
                ),
                style = "padding: 3px 0; transition: background 0.2s;",
                onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
              )
            })
          } else {
            p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
          }
        ),
        
        hr(),
        textInput("basedir_manual_path", "Or enter path manually:",
                  value = input$edit_base_dir,
                  placeholder = "mfcl/inputs/2023_rep"),
        p(style = "color: #666; font-size: 12px;", 
          "Tip: Enter the relative path to your model input directory"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_basedir_path", "Select", class = "btn-primary")
        )
      ))
    }, error = function(e) {
      showNotification(paste("Error browsing directories:", e$message), type = "error")
    })
  })
  
  observeEvent(input$close_basedir_modal, {
    removeModal()
  })
  
  observeEvent(input$confirm_basedir_path, {
    if (!is.null(input$basedir_manual_path) && input$basedir_manual_path != "") {
      updateTextInput(session, "edit_base_dir", value = input$basedir_manual_path)
    }
    removeModal()
  })
  
  output$model_editor_ui <- renderUI({
    req(input$edit_model_select)
    model <- rv$models[[input$edit_model_select]]
    if (is.null(model)) return(p("No model selected"))
    
    updateTextAreaInput(session, "edit_description", 
                        value = ifelse(is.null(model$description), "", model$description))
    
    mfcl_args <- if (!is.null(model$mfcl_commands) && !is.null(model$program_path)) {
      trimmed_cmd <- trimws(gsub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", model$program_path), "\\s*"), "", model$mfcl_commands))
      trimmed_cmd
    } else {
      ""
    }
    
    tagList(
      h4(paste("Editing:", input$edit_model_select)),
      
      if (!is.null(model$description) && model$description != "") {
        div(class = "description-box", strong("Description: "), model$description)
      },
      
      div(class = "path-input-group",
          div(class = "param-label", "Program Path:"),
          fluidRow(
            column(10,
                   textInput("edit_program_path", NULL, 
                             value = model$program_path, 
                             width = "100%",
                             placeholder = "mfcl/exe/mfclo64 or ./doitall.sh")
            ),
            column(2,
                   actionButton("browse_program", "Browse", 
                                class = "btn-default browse-btn btn-block",
                                icon = icon("folder-open"))
            )
          )
      ),
      
      div(class = "path-input-group",
          div(class = "param-label", "Base Directory:"),
          fluidRow(
            column(10,
                   textInput("edit_base_dir", NULL, 
                             value = model$base_dir, 
                             width = "100%",
                             placeholder = "mfcl/inputs/2023_rep")
            ),
            column(2,
                   actionButton("browse_basedir", "Browse", 
                                class = "btn-default browse-btn btn-block",
                                icon = icon("folder-open"))
            )
          )
      ),
      
      div(class = "param-label", "MFCL Arguments:"),
      textAreaInput("edit_mfcl_args", NULL, 
                    value = mfcl_args,
                    rows = 2, width = "100%",
                    placeholder = "bet.frq 11.par 12.par -switch 2 1 1 10000 (or leave empty for ./doitall.sh)"),
      p(style = "color: #666; font-size: 11px; margin-top: -10px;",
        "💡 If arguments start with './' (like ./doitall.sh), only arguments will be used as command"),
      
      div(class = "commands-preview",
          strong("Full Command Preview:"), br(),
          textOutput("mfcl_commands_preview")
      ),
      
      hr(),
      
      fluidRow(
        column(6,
               div(class = "param-label", "Jitter Seeds:"),
               textInput("edit_jitter_seeds", NULL, value = model$jitter_seeds)
        ),
        column(6,
               div(class = "param-label", "Jitter Amount:"),
               textInput("edit_jitter_amount", NULL, value = model$jitter_amount)
        )
      ),
      fluidRow(
        column(6,
               div(class = "param-label", "Retrospective Peels:"),
               textInput("edit_retro_peels", NULL, value = model$retro_peels)
        ),
        column(6,
               div(class = "param-label", "Hessian Splits:"),
               numericInput("edit_nsplit", NULL, value = as.numeric(model$nsplit), min = 1, max = 20)
        )
      ),
      div(class = "param-label", "Profile Scalers:"),
      textInput("edit_scalers", NULL, value = model$scalers, width = "100%"),
      div(class = "param-label", "Profile Reps:"),
      textInput("edit_reps", NULL, value = model$Reps, width = "100%")
    )
  })
  
  output$mfcl_commands_preview <- renderText({
    if (is.null(input$edit_program_path)) {
      return("")
    }
    
    if (is.null(input$edit_mfcl_args) || trimws(input$edit_mfcl_args) == "") {
      return(input$edit_program_path)
    }
    
    if (grepl("^\\./", trimws(input$edit_mfcl_args))) {
      return(trimws(input$edit_mfcl_args))
    }
    
    paste(input$edit_program_path, input$edit_mfcl_args)
  })
  
  observeEvent(input$save_model, {
    req(input$edit_model_select)
    model_name <- input$edit_model_select
    
    full_commands <- if (is.null(input$edit_mfcl_args) || trimws(input$edit_mfcl_args) == "") {
      input$edit_program_path
    } else if (grepl("^\\./", trimws(input$edit_mfcl_args))) {
      trimws(input$edit_mfcl_args)
    } else {
      paste(input$edit_program_path, input$edit_mfcl_args)
    }
    
    rv$models[[model_name]]$description <- input$edit_description
    rv$models[[model_name]]$mfcl_commands <- full_commands
    rv$models[[model_name]]$program_path <- input$edit_program_path
    rv$models[[model_name]]$base_dir <- input$edit_base_dir
    rv$models[[model_name]]$jitter_seeds <- input$edit_jitter_seeds
    rv$models[[model_name]]$jitter_amount <- input$edit_jitter_amount
    rv$models[[model_name]]$retro_peels <- input$edit_retro_peels
    rv$models[[model_name]]$nsplit <- as.character(input$edit_nsplit)
    rv$models[[model_name]]$scalers <- input$edit_scalers
    rv$models[[model_name]]$Reps <- input$edit_reps
    
    showNotification(paste("✓ Saved changes to", model_name), type = "message")
  })
  
  output$saved_configs_ui <- renderUI({
    rv$saved_configs_trigger
    saved_dir <- "../configs/models_ran"
    if (!dir.exists(saved_dir)) return(p("No saved run configurations found."))
    
    saved_files <- list.files(saved_dir, pattern = "\\.rds$", full.names = TRUE)
    saved_files <- saved_files[!grepl("_job_history\\.rds$", saved_files)]
    if (length(saved_files) == 0) return(p("No saved run configurations found."))
    
    saved_files <- saved_files[order(file.info(saved_files)$mtime, decreasing = TRUE)]
    
    config_cards <- lapply(saved_files, function(file) {
      config_name <- basename(file)
      tryCatch({
        saved_data <- readRDS(file)
        meta <- saved_data$metadata
        job_history <- load_job_history(file)
        
        div(class = "config-card",
            div(style = "display: flex; justify-content: space-between; align-items: start;",
                div(
                  strong(style = "font-size: 16px; color: #333;", meta$run_name), br(),
                  span(style = "color: #666;", meta$description), br(),
                  small(style = "color: #999;",
                        icon("calendar"), " ", format(meta$date, "%Y-%m-%d %H:%M"), " | ",
                        icon("user"), " ", meta$created_by, " | ",
                        icon("cube"), " ", length(saved_data$models), " models", br(),
                        icon("file"), " Base: ", meta$base_config
                  ),
                  if (nrow(job_history) > 0) {
                    div(class = "job-history",
                        strong(icon("rocket"), " Jobs Run: ", nrow(job_history)), br(),
                        small("Latest: ", job_history[nrow(job_history), "timestamp"],
                              " (", job_history[nrow(job_history), "job_type"], ")", br(),
                              "Output: ", job_history[nrow(job_history), "output_dir"])
                    )
                  }
                ),
                actionButton(paste0("load_saved_", gsub("[^a-zA-Z0-9]", "_", config_name)),
                             "Load", class = "btn-sm btn-primary",
                             onclick = sprintf("Shiny.setInputValue('load_saved_config', '%s', {priority: 'event'})", file))
            )
        )
      }, error = function(e) { 
        div(class = "config-card", p(config_name)) 
      })
    })
    tagList(config_cards)
  })
  
  observeEvent(input$load_saved_config, {
    req(input$load_saved_config)
    load_models(input$load_saved_config, is_saved_run = TRUE)
  })
  
  observeEvent(input$save_config_btn, {
    showModal(modalDialog(
      title = "Save Model Run Configuration", size = "m",
      textInput("modal_run_name", "Run Name:", placeholder = "e.g., Sensitivity Analysis - February 2026"),
      textAreaInput("modal_run_description", "Description:", placeholder = "Why are you running these models?", rows = 4),
      hr(),
      p(strong("Current Models:"), paste(names(rv$models), collapse = ", ")),
      p(strong("Base Config:"), rv$base_config_name),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_save_run", "Save Run Config", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$confirm_save_run, {
    req(input$modal_run_name)
    if (length(rv$models) == 0) { 
      showNotification("No models to save!", type = "error")
      return() 
    }
    
    tryCatch({
      date_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
      safe_name <- gsub("[^a-zA-Z0-9_-]", "_", input$modal_run_name)
      safe_name <- substr(safe_name, 1, 50)
      filename <- paste0(date_str, "_", safe_name, ".rds")
      
      save_data <- list(
        metadata = list(
          run_name = input$modal_run_name, 
          description = input$modal_run_description,
          date = Sys.time(), 
          created_by = Sys.getenv("USER"), 
          base_config = rv$base_config_name,
          n_models = length(rv$models), 
          model_names = names(rv$models)
        ),
        models = rv$models
      )
      
      filepath <- file.path("../configs/models_ran", filename)
      saveRDS(save_data, filepath)
      rv$current_config_file <- filepath
      
      removeModal()
      showNotification(paste0("✓ Saved run configuration:\n", filename), type = "message", duration = 5)
      rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
      
    }, error = function(e) { 
      showNotification(paste("Error saving:", e$message), type = "error") 
    })
  })
  
  # ========== LAUNCH JOB HANDLERS ==========
  
  observeEvent(input$launch_btn, {
    if (length(rv$selected_models) == 0) { 
      showNotification("Please select at least one model", type = "error")
      return() 
    }
    if (length(rv$models) == 0) { 
      showNotification("Please load models first", type = "error")
      return() 
    }
    
    rv$launch_log <- paste0(Sys.time(), " - Starting job submission...\n")
    
    tryCatch({
      batch_names <- c()
      remote_dirs <- c()
      
      for (model_name in rv$selected_models) {
        model_env <- rv$models[[model_name]]
        
        if (input$job_type == "jitter") {
          seeds <- as.numeric(strsplit(model_env$jitter_seeds, "\\s+")[[1]])
          for (seed in seeds) {
            result <- launch_single_job(model_name, model_env, seed = seed)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
          }
        } else if (input$job_type == "hessian") {
          for (part in 1:as.numeric(model_env$nsplit)) {
            result <- launch_single_job(model_name, model_env, part = part)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
          }
        } else if (input$job_type == "retro") {
          peels <- as.numeric(strsplit(model_env$retro_peels, "\\s+")[[1]])
          for (peel in peels) {
            result <- launch_single_job(model_name, model_env, peel = peel)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
          }
        } else if (input$job_type == "prof") {
          scalers <- as.numeric(strsplit(model_env$scalers, "\\s+")[[1]])
          for (sc in scalers) {
            result <- launch_single_job(model_name, model_env, scaler = sc)
            batch_names <- c(batch_names, result$batch_name)
            remote_dirs <- c(remote_dirs, result$remote_dir)
          }
        } else {
          result <- launch_single_job(model_name, model_env)
          batch_names <- c(batch_names, result$batch_name)
          remote_dirs <- c(remote_dirs, result$remote_dir)
        }
      }
      
      if (!is.null(rv$current_config_file)) {
        job_record <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
          job_type = input$job_type,
          model_names = paste(rv$selected_models, collapse = ", "), 
          output_dir = input$output_dir,
          batch_names = paste(batch_names, collapse = "; "), 
          remote_dirs = paste(remote_dirs, collapse = "; "),
          branch = input$branch, 
          status = "launched", 
          stringsAsFactors = FALSE
        )
        save_job_history(rv$current_config_file, job_record)
        rv$launch_log <- paste0(rv$launch_log, "\n✓ Job history saved to config file\n")
      }
      
      rv$launch_log <- paste0(rv$launch_log, "\n", Sys.time(), " - All jobs submitted!\n")
      showNotification("Jobs launched successfully!", type = "message")
      
    }, error = function(e) {
      rv$launch_log <- paste0(rv$launch_log, "\nERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  launch_single_job <- function(model_name, model_env, seed = NULL, part = NULL, peel = NULL, scaler = NULL) {
    job_env <- model_env
    remote_dir_suffix <- model_name
    batch_suffix <- ""
    
    if (!is.null(seed)) {
      job_env$jitter_seed <- as.character(seed)
      remote_dir_suffix <- paste0(model_name, "_seed", seed)
      batch_suffix <- paste0("-jitter", seed)
    } else if (!is.null(part)) {
      job_env$hessian_part <- as.character(part)
      remote_dir_suffix <- paste0(model_name, "_part", part)
      batch_suffix <- paste0("-hess", part)
    } else if (!is.null(peel)) {
      job_env$retro_peel <- as.character(peel)
      remote_dir_suffix <- paste0(model_name, "_peel", peel)
      batch_suffix <- paste0("-retro", peel)
    } else if (!is.null(scaler)) {
      job_env$scaler <- as.character(scaler)
      remote_dir_suffix <- paste0(model_name, "_sc", scaler)
      batch_suffix <- paste0("-sc", scaler)
    }
    
    remote_dir <- paste0(input$github_repo, "/", input$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(model_name, batch_suffix, "-", format(Sys.time(), "%H:%M:%S"))
    
    rv$launch_log <- paste0(rv$launch_log, "  → ", batch_name, "\n")
    
    CondorBox::CondorBox(
      make_options = input$job_type, 
      remote_user = input$remote_user, 
      remote_host = input$remote_host,
      remote_dir = remote_dir, 
      github_pat = Sys.getenv("GIT_PAT"), 
      github_username = input$github_username,
      github_org = input$github_org, 
      github_repo = input$github_repo, 
      docker_image = input$docker_image,
      condor_memory = input$condor_memory, 
      condor_cpus = input$condor_cpus, 
      condor_disk = input$condor_disk,
      stream_error = "TRUE", 
      branch = input$branch, 
      rmclone_script = "no", 
      ghcr_login = TRUE,
      exclude_slots = c("slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
                        "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"),
      custom_batch_name = batch_name, 
      condor_environment = job_env
    )
    
    return(list(batch_name = batch_name, remote_dir = remote_dir))
  }
  
  # ========== ADD/DELETE MODEL HANDLERS ==========
  
  observeEvent(input$add_new_model, {
    showModal(modalDialog(
      title = "Add New Model",
      textInput("new_model_name", "Model Name:", placeholder = "e.g., sensitivity_M_high"),
      selectInput("new_model_base", "Copy from existing model:", 
                  choices = c("Create from scratch" = "", names(rv$models))),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_add_model", "Create", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$confirm_add_model, {
    req(input$new_model_name)
    new_name <- input$new_model_name
    
    if (new_name %in% names(rv$models)) { 
      showNotification("Model name already exists!", type = "error")
      return() 
    }
    
    if (input$new_model_base == "") {
      new_model <- list(
        description = "New model", 
        mfcl_commands = "./doitall.sh",
        program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278", 
        base_dir = "mfcl/inputs/2023_rep",
        jitter_seeds = "1 2 3", 
        jitter_amount = "0.01", 
        retro_peels = "1 2 3 4 5",
        nsplit = "5", 
        scalers = "110 100 90", 
        Reps = "2 2 2 2 2 2"
      )
    } else {
      new_model <- rv$models[[input$new_model_base]]
      new_model$description <- paste("Copied from", input$new_model_base)
    }
    
    rv$models[[new_name]] <- new_model
    updateSelectInput(session, "edit_model_select", choices = names(rv$models), selected = new_name)
    removeModal()
    showNotification(paste("✓ Created model:", new_name), type = "message")
  })
  
  observeEvent(input$delete_model, {
    req(input$edit_model_select)
    showModal(modalDialog(
      title = "Confirm Deletion", 
      paste("Delete model:", input$edit_model_select, "?"),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_delete_model", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_model, {
    model_name <- input$edit_model_select
    rv$models[[model_name]] <- NULL
    updateSelectInput(session, "edit_model_select", choices = names(rv$models), selected = names(rv$models)[1])
    removeModal()
    showNotification(paste("✓ Deleted model:", model_name), type = "message")
  })
  
  observeEvent(input$reset_model, {
    req(input$edit_model_select)
    model_name <- input$edit_model_select
    
    if (model_name %in% names(rv$models_original)) {
      rv$models[[model_name]] <- rv$models_original[[model_name]]
      showNotification(paste("✓ Reset", model_name), type = "message")
    } else {
      showNotification("Cannot reset - this is a new model", type = "warning")
    }
  })
  
  # ========== OUTPUT DISPLAYS ==========
  
  output$config_status <- renderText({ rv$config_status_msg })
  output$launch_log <- renderText({ rv$launch_log })
  
  output$models_summary <- renderText({
    if (length(rv$models) == 0) return("No models loaded.")
    
    summary_lines <- lapply(names(rv$models), function(nm) {
      m <- rv$models[[nm]]
      desc_line <- if (!is.null(m$description) && m$description != "") {
        paste0("  Description: ", m$description, "\n")
      } else {
        ""
      }
      paste0("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", "Model: ", nm, "\n", 
             desc_line, "  Program: ", m$program_path, "\n")
    })
    paste(summary_lines, collapse = "\n")
  })
  
  # ========== MONITOR JOBS ==========
  
  observeEvent(input$refresh_jobs, {
    showNotification("Refreshing jobs...", type = "message", duration = 1)
    
    tryCatch({
      # SSH command execution (batch format)
      cmd <- sprintf("ssh %s@%s 'condor_q'", input$remote_user, input$remote_host)
      
      jobs_output <- system(cmd, intern = TRUE)
      
      # Parse condor_q output
      jobs_list <- list()
      
      # Find header line starting with "OWNER   BATCH_NAME"
      header_idx <- grep("^OWNER\\s+BATCH_NAME", jobs_output)
      
      if (length(header_idx) > 0 && header_idx < length(jobs_output)) {
        # Parse from line after header
        start_idx <- header_idx + 1
        
        # Find line starting with "Total for"
        total_idx <- grep("^Total", jobs_output)
        if (length(total_idx) > 0) {
          end_idx <- total_idx[1] - 1
        } else {
          end_idx <- length(jobs_output)
        }
        
        if (start_idx <= end_idx) {
          data_lines <- jobs_output[start_idx:end_idx]
          # Remove empty lines
          data_lines <- data_lines[nzchar(trimws(data_lines))]
          
          for (line in data_lines) {
            line_trimmed <- trimws(line)
            if (nzchar(line_trimmed)) {
              # Split by whitespace
              parts <- unlist(strsplit(line_trimmed, "\\s+"))
              
              if (length(parts) >= 8) {
                # condor_q batch output format:
                # OWNER BATCH_NAME SUBMITTED DONE RUN IDLE TOTAL JOB_IDS
                jobs_list[[length(jobs_list) + 1]] <- data.frame(
                  Owner = parts[1],
                  BatchName = parts[2],
                  Submitted = paste(parts[3], parts[4]),
                  Done = ifelse(parts[5] == "_", "0", parts[5]),
                  Run = ifelse(parts[6] == "_", "0", parts[6]),
                  Idle = ifelse(parts[7] == "_", "0", parts[7]),
                  Total = parts[8],
                  JobIDs = ifelse(length(parts) >= 9, parts[9], ""),
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
      
      if (length(jobs_list) > 0) {
        rv$jobs_status <- do.call(rbind, jobs_list)
        showNotification(paste("Found", nrow(rv$jobs_status), "jobs"), type = "message", duration = 2)
      } else {
        rv$jobs_status <- data.frame(
          Owner = character(),
          BatchName = character(),
          Submitted = character(),
          Done = character(),
          Run = character(),
          Idle = character(),
          Total = character(),
          JobIDs = character(),
          stringsAsFactors = FALSE
        )
        showNotification("No jobs found", type = "warning", duration = 2)
      }
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      rv$jobs_status <- data.frame(
        Owner = character(),
        BatchName = character(),
        Submitted = character(),
        Done = character(),
        Run = character(),
        Idle = character(),
        Total = character(),
        JobIDs = character(),
        stringsAsFactors = FALSE
      )
    })
  })
  
  output$jobs_table <- renderDT({
    datatable(
      rv$jobs_status, 
      selection = 'multiple',
      options = list(
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      class = 'cell-border stripe'
    )
  })
  
  output$selected_jobs_info <- renderText({
    selected_rows <- input$jobs_table_rows_selected
    if (length(selected_rows) > 0) {
      paste(length(selected_rows), "job(s) selected")
    } else {
      "No jobs selected"
    }
  })
  
  observeEvent(input$remove_selected_jobs, {
    selected_rows <- input$jobs_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("No jobs selected", type = "warning")
      return()
    }
    
    selected_jobs <- rv$jobs_status[selected_rows, ]
    
    # Extract job IDs
    job_ids <- selected_jobs$JobIDs
    
    showModal(modalDialog(
      title = "Confirm Job Removal",
      size = "m",
      p(strong(paste("Remove", length(selected_rows), "job(s)?")), style = "margin-bottom: 15px;"),
      div(
        style = "max-height: 300px; overflow-y: auto; background: #f9f9f9; padding: 10px; border: 1px solid #ddd; border-radius: 4px;",
        tags$table(
          style = "width: 100%; font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("Batch Name", style = "text-align: left; padding: 5px;"),
              tags$th("Job ID", style = "text-align: left; padding: 5px;"),
              tags$th("Status", style = "text-align: center; padding: 5px;")
            )
          ),
          tags$tbody(
            lapply(1:nrow(selected_jobs), function(i) {
              tags$tr(
                tags$td(selected_jobs$BatchName[i], style = "padding: 5px;"),
                tags$td(selected_jobs$JobIDs[i], style = "padding: 5px;"),
                tags$td(paste0("Run: ", selected_jobs$Run[i], " | Idle: ", selected_jobs$Idle[i]), 
                        style = "padding: 5px; text-align: center;")
              )
            })
          )
        )
      ),
      hr(),
      p(strong("Warning:"), "This will remove the jobs from the Condor queue.", 
        style = "color: #d9534f; margin-top: 15px;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_jobs", "Remove Jobs", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_remove_jobs, {
    selected_rows <- input$jobs_table_rows_selected
    
    if (length(selected_rows) == 0) {
      removeModal()
      return()
    }
    
    selected_jobs <- rv$jobs_status[selected_rows, ]
    job_ids <- selected_jobs$JobIDs
    
    removeModal()
    
    showNotification("Removing jobs...", type = "message", duration = 2)
    
    tryCatch({
      removed_count <- 0
      failed_count <- 0
      
      for (job_id in job_ids) {
        if (nzchar(job_id)) {
          # Remove job using condor_rm
          cmd <- sprintf("ssh %s@%s 'condor_rm %s'", 
                         input$remote_user, input$remote_host, job_id)
          
          result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
          
          if (length(result) > 0 && !grepl("ERROR|Error", result[1])) {
            removed_count <- removed_count + 1
          } else {
            failed_count <- failed_count + 1
          }
        }
      }
      
      # Show result
      if (removed_count > 0) {
        showNotification(
          paste("Successfully removed", removed_count, "job(s)"), 
          type = "message", 
          duration = 3
        )
      }
      
      if (failed_count > 0) {
        showNotification(
          paste("Failed to remove", failed_count, "job(s)"), 
          type = "warning", 
          duration = 3
        )
      }
      
      # Refresh job list after a short delay
      Sys.sleep(1)
      shinyjs::click("refresh_jobs")
      
    }, error = function(e) {
      showNotification(paste("Error removing jobs:", e$message), type = "error", duration = 5)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)




