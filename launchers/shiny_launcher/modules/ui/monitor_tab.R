monitor_tab_ui <- function() {
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
              column(2,
                     div(style = "margin-top: 5px;",
                         checkboxInput("show_all_jobs", "Show All Users", 
                                       value = FALSE)
                     )
              ),
              column(2,
                     div(style = "margin-top: 5px;",
                         checkboxInput("auto_refresh_jobs", "Auto Refresh", 
                                       value = FALSE)
                     )
              ),
              column(4,
                     div(id = "job_selection_info",
                         style = "padding: 8px; color: #666; font-size: 13px;",
                         textOutput("selected_jobs_info", inline = TRUE))
              )
            ),
            
            
            shiny::hr(),
            DTOutput("jobs_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Job Details", status = "info", solidHeader = TRUE, width = 12,
            selectInput("job_detail_batch", "Select Job:", choices = character(0)),
            verbatimTextOutput("job_details")
          )
        )
      )
}
