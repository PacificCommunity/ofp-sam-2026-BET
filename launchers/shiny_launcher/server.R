server <- function(input, output, session) {
  app_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile)),
    error = function(e) normalizePath(getwd(), mustWork = FALSE)
  )
  server_dir <- file.path(app_dir, "modules", "server")
  source(file.path(server_dir, "00_settings.R"), local = TRUE)
  source(file.path(server_dir, "01_retrieve_ui_helpers.R"), local = TRUE)
  source(file.path(server_dir, "02_helpers.R"), local = TRUE)
  source(file.path(server_dir, "03_launch_config.R"), local = TRUE)
  source(file.path(server_dir, "04_model_selection.R"), local = TRUE)
  source(file.path(server_dir, "05_retrieve.R"), local = TRUE)
  source(file.path(server_dir, "06_edit_models.R"), local = TRUE)
  source(file.path(server_dir, "07_launch_jobs.R"), local = TRUE)
  source(file.path(server_dir, "08_add_delete_model.R"), local = TRUE)
  source(file.path(server_dir, "09_outputs.R"), local = TRUE)
  source(file.path(server_dir, "10_monitor.R"), local = TRUE)
  source(file.path(server_dir, "12_promote_outputs.R"), local = TRUE)
  source(file.path(server_dir, "13_condor_completed.R"), local = TRUE)
  source(file.path(server_dir, "14_launch_preflight.R"), local = TRUE)
}
