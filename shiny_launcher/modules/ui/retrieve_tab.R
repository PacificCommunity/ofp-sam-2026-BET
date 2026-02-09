retrieve_tab_ui <- function() {
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
                     div(style = "display:flex; align-items:center; gap:8px;",
                         actionButton("scan_results", "Scan Folders", 
                                      icon = icon("search"),
                                      class = "btn-info"),
                         textOutput("scan_results_status", inline = TRUE)
                     )
              ),
              column(4,
                     div(style = "display:flex; align-items:center; gap:8px;",
                         actionButton("download_all", "Download All", 
                                      icon = icon("download"),
                                      class = "btn-success"),
                         textOutput("download_all_status", inline = TRUE)
                     )
              ),
              column(4,
                     div(style = "display:flex; align-items:center; gap:8px;",
                         actionButton("delete_remote_dir", "Delete Directory", 
                                      icon = icon("trash"),
                                      class = "btn-danger"),
                         textOutput("delete_remote_dir_status", inline = TRUE)
                     )
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
                                    value = "model",
                                    placeholder = "/path/to/download")
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
            
            shiny::hr(),
            
            uiOutput("folders_selection_ui"),
            
            shiny::hr(),
            
            p(strong("Actions for Selected Folders:"), style = "margin-top: 15px; color: #666;"),
            
            fluidRow(
              column(6,
                     div(style = "display:flex; align-items:center; gap:8px;",
                         actionButton("fetch_selected", "Download Selected", 
                                      class = "btn-primary", 
                                      icon = icon("download")),
                         textOutput("download_selected_status", inline = TRUE)
                     )
              ),
              column(6,
                     div(style = "display:flex; align-items:center; gap:8px;",
                         actionButton("delete_selected", "Delete Selected", 
                                      class = "btn-danger", 
                                      icon = icon("trash")),
                         textOutput("delete_selected_status", inline = TRUE)
                     )
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
                    tags$li(code("model"), " - extracts contents: archive/repo/model/* → model/[folder_name]/*"),
                    tags$li(code("plots"), " - extracts contents: archive/repo/plots/* → model/[folder_name]/*"),
                    tags$li(code("mfcl/outputs"), " - extracts contents: archive/repo/mfcl/outputs/* → model/[folder_name]/*"),
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
      )
}
