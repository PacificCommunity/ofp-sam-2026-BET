joblog_tab_ui <- function() {
  tabItem(
    tabName = "joblog",
    fluidRow(
      box(
        title = "Job Log",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(
            3,
            actionButton(
              "refresh_launcher_job_log",
              "Refresh Job Log",
              class = "btn-default btn-sm",
              icon = icon("sync")
            )
          ),
          column(
            9,
            div(
              style = "padding-top: 7px; color: #666;",
              textOutput("launcher_job_log_status", inline = TRUE)
            )
          )
        ),
        br(),
        DT::DTOutput("launcher_job_log_table")
      )
    )
  )
}
