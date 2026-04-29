condor_completed_tab_ui <- function() {
  tabItem(
    tabName = "condor_completed",
    fluidRow(
      box(
        title = "Condor Completed Runs",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(
            8,
            div(
              style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
              actionButton("condor_completed_scan", "Scan Completed Remote Dirs", class = "btn-info", icon = icon("search")),
              actionButton("condor_completed_refresh_log", "Reload Job Log", class = "btn-default", icon = icon("sync")),
              div(
                style = "min-width:180px;",
                selectInput(
                  "condor_completed_launch_window",
                  "Show by launch time:",
                  choices = c(
                    "Last 6 hours" = "6h",
                    "Last 12 hours" = "12h",
                    "Last 24 hours" = "1d",
                    "Last 7 days" = "7d",
                    "Last 30 days" = "30d",
                    "All" = "all"
                  ),
                  selected = "7d",
                  width = "100%"
                )
              )
            ),
            div(
              style = "margin-top: 8px; color: #666; font-size: 12px; line-height: 1.35;",
              "Compares remote Condor result archives against the launcher job log. Completion time is taken from the newest ",
              code(".tar.gz/.tgz"),
              " result file in each remote job directory; duration is completion time minus launcher run time. Scans both ",
              code("nouofpsubmit"),
              " and ",
              code("suvofpsubmit"),
              " plus the currently selected remote host."
            )
          ),
          column(
            4,
            div(
              style = "padding-top: 6px; color: #666;",
              textOutput("condor_completed_status", inline = TRUE)
            )
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Completed Status",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        DT::DTOutput("condor_completed_table")
      )
    ),
    fluidRow(
      box(
        title = "Condor Completion Log",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        verbatimTextOutput("condor_completed_log")
      )
    )
  )
}
