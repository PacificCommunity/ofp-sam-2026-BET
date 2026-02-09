settings_tab_ui <- function() {
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
                      value = "configs/set_model.R",
                      placeholder = "Path to your set_model.R file"),
            
            actionButton("reload_config", "Load Models from Script", 
                         icon = icon("sync"), class = "btn-info btn-block"),
            
            shiny::hr(),
            
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
}
