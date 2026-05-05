library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(CondorBox)

app_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) normalizePath(getwd(), mustWork = FALSE)
)
if (!file.exists(file.path(app_dir, "modules", "ui", "launch_tab.R")) &&
    file.exists(file.path("launchers", "shiny_launcher", "modules", "ui", "launch_tab.R"))) {
  app_dir <- normalizePath(file.path("launchers", "shiny_launcher"), mustWork = TRUE)
}
repo_root <- normalizePath(file.path(app_dir, "..", ".."), mustWork = FALSE)
source(file.path(repo_root, "tools", "model_defaults.R"))
source(file.path(repo_root, "tools", "input_change_metadata.R"))
source(file.path(repo_root, "tools", "input_sensitivities", "sensitivity_catalog.R"))
source(file.path(repo_root, "tools", "fitted_model_source.R"))
ui_dir <- file.path(app_dir, "modules", "ui")
source(file.path(ui_dir, "launch_tab.R"))
source(file.path(ui_dir, "edit_tab.R"))
source(file.path(ui_dir, "monitor_tab.R"))
source(file.path(ui_dir, "retrieve_tab.R"))
source(file.path(ui_dir, "joblog_tab.R"))
source(file.path(ui_dir, "condor_completed_tab.R"))
source(file.path(ui_dir, "settings_tab.R"))
