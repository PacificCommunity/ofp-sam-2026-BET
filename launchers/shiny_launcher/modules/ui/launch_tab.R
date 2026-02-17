launch_tab_ui <- function() {
  tabItem(
    tabName = "launch",
    
    fluidRow(
      box(
        title = "Repository Root", status = "primary", solidHeader = TRUE, width = 12,
        div(class = "param-label", "Repo Root Path:"),
        fluidRow(
          column(9,
                 textInput("repo_root", NULL,
                          value = normalizePath(file.path("..", ".."), mustWork = FALSE),
                          placeholder = "/path/to/repo")
          ),
          column(3,
                 actionButton("browse_repo_root", "Browse",
                              class = "btn-default btn-block",
                              icon = icon("folder-open"))
          )
        ),
        div(style = "margin-top: 6px; color: #666; font-size: 12px;",
            "All config/model paths are resolved relative to this root unless absolute.")
      )
    ),
        
    # ---- Config File Loader (inline) ----
    fluidRow(
          box(
            title = "Load Model Configuration", status = "info", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(6,
                     actionButton("launch_load_config", "Browse & Load Configuration",
                                  icon = icon("folder-open"),
                                  class = "btn-info btn-lg",
                                  style = "width: 100%; margin-top: 10px; height: 60px; font-size: 16px;")
              ),
              column(6,
                     actionButton("edit_script_rstudio", "Modify Current Script",
                                  icon = icon("edit"),
                                  class = "btn-warning btn-lg",
                                  style = "width: 100%; margin-top: 10px; height: 60px; font-size: 16px;")
              )
            ),
            uiOutput("launch_config_status_ui")
          )
        ),
        
        
        fluidRow(
          box(
            title = "Job Configuration", status = "primary", solidHeader = TRUE, width = 6,
            
            checkboxGroupInput("job_types", "Job Types:",
                               choices = c("Model" = "model", 
                                           "Jitter" = "jitter",
                                           "Hessian" = "hessian",
                                           "Retrospective" = "retro",
                                           "Profile" = "prof"),
                               selected = NULL),
            
            textInput("output_dir", "Output Directory:", 
                      value = "quick/test_run"),
            
            selectizeInput(
              "branch",
              "Git Branch:",
              choices = c("main", "develop", "develop_lik"),
              selected = "develop_lik",
              options = list(
                create = TRUE,
                persist = FALSE,
                placeholder = "Select or type a branch"
              )
            ),
            
            shiny::hr(),
            
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
            
            checkboxInput("ghcr_login", "GHCR Login (private images)", value = FALSE),
            
            checkboxInput("parallel_launch", "Parallel launch", value = TRUE),
            
            selectInput(
              "launch_parallel_cores",
              "Launch cores:",
              choices = as.character(seq_len(max(1, parallel::detectCores() - 2))),
              selected = as.character(max(1, parallel::detectCores() - 2))
            ),
            
            shiny::hr(),
            
            numericInput("condor_cpus", "CPUs:", 
                         value = 2, min = 1, max = 32, step = 1),
            
            numericInput("condor_memory", "Memory (GB):",
                         value = 12, min = 1, max = 128, step = 1),
            
            numericInput("condor_disk", "Disk (GB):",
                         value = 10, min = 1, max = 100, step = 1)
            
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
      )
}
