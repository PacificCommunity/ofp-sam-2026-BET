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
            3,
            actionButton(
              "delete_selected_launcher_job_log",
              "Delete Selected",
              class = "btn-warning btn-sm",
              icon = icon("trash")
            )
          ),
          column(
            2,
            actionButton(
              "clear_launcher_job_log",
              "Clear All",
              class = "btn-danger btn-sm",
              icon = icon("trash")
            )
          ),
          column(
            2,
            div(
              style = "margin-top: 4px;",
              checkboxInput(
                "joblog_delete_with_remote_dir",
                "Delete remote directory too",
                value = FALSE
              )
            )
          ),
          column(
            2,
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
