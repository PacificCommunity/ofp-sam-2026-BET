settings_tab_ui <- function() {
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Load Common Launch Settings from R Script", status = "primary", solidHeader = TRUE, width = 6,
            
            p(strong("Load shared launcher settings"), 
              "from an R script (typically", code("set_model.R"), 
              ") that contains a", code("launch_defaults"), "list object."),
            
            fileInput("config_file_upload", "Upload R script:",
                      accept = c(".R", ".r")),
            
            p("Or specify path:"),
            textInput("config_file_path", "R Script Path:", 
                      value = "configs/2023R4_launch_settings.R",
                      placeholder = "Path to your launch settings file"),
            
            actionButton("reload_config", "Load Settings from Script", 
                         icon = icon("sync"), class = "btn-info btn-block"),
            
            shiny::hr(),
            
            verbatimTextOutput("config_status")
          ),
          
          box(
            title = "Load Previous Run Configuration", status = "info", solidHeader = TRUE, width = 6,
            
            p(strong("Browse saved run history."), 
              "These are configurations saved from previous launcher sessions."),
            
            actionButton("refresh_saved_configs", "Refresh List", 
                         icon = icon("sync"), class = "btn-sm btn-default",
                         style = "margin-bottom: 10px;"),
            
            uiOutput("saved_configs_ui")
          )
        ),
        
        fluidRow(
          box(
            title = "Currently Loaded Launch Settings", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("models_summary")
          )
        )
      )
}
