promote_tab_ui <- function() {
  tabItem(
    tabName = "promote",
    fluidRow(
      box(
        title = "Promote Model Outputs to MFCL Inputs",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(
            6,
            textInput(
              "promote_source_dir",
              "Output model directory:",
              value = "model",
              placeholder = "model or path containing model_info.rds files"
            )
          ),
          column(
            6,
            checkboxInput("promote_backup_existing", "Backup existing input files", value = FALSE),
            div(
              style = "margin-top: -8px; margin-bottom: 8px; color: #666; font-size: 12px; line-height: 1.35;",
              "When enabled, existing input files are copied next to themselves with a ",
              code(".bak_YYYYMMDD_HHMMSS"),
              " suffix before being overwritten. Usually leave this off because the git commit provides the cleaner backup."
            ),
            div(
              style = "display:flex; gap:8px;",
              actionButton("promote_scan", "Scan / Refresh", class = "btn-info", icon = icon("search")),
              actionButton("promote_apply", "Promote Selected + Commit + Push", class = "btn-success", icon = icon("level-up-alt")),
              actionButton("promote_retry_push", "Retry Git Push", class = "btn-warning", icon = icon("sync"))
            ),
            div(style = "margin-top: 8px; color: #666;", textOutput("promote_selected_status", inline = TRUE)),
            div(style = "margin-top: 4px; color: #666;", textOutput("promote_run_status", inline = TRUE))
          )
        ),
        div(
          style = "margin-top: 10px; color: #666; font-size: 12px;",
          "Scans model_info.rds files, shows whether .par and indepvar.rpt exist in both output and input folders, then promotes matching output files to their recorded input target."
        )
      )
    ),
    fluidRow(
      box(
        title = "Promotion Status",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        div(
          style = "display:flex; gap:8px; margin-bottom:10px;",
          actionButton("promote_select_all", "Select All", class = "btn-sm btn-success", icon = icon("check-square")),
          actionButton("promote_deselect_all", "Deselect All", class = "btn-sm btn-warning", icon = icon("square"))
        ),
        DT::DTOutput("promote_status_table")
      )
    ),
    fluidRow(
      box(
        title = "Promotion Log",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        verbatimTextOutput("promote_log")
      )
    )
  )
}
