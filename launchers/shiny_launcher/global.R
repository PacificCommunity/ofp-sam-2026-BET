library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(CondorBox)

app_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) normalizePath(getwd(), mustWork = FALSE)
)
ui_dir <- file.path(app_dir, "modules", "ui")
source(file.path(ui_dir, "launch_tab.R"))
source(file.path(ui_dir, "edit_tab.R"))
source(file.path(ui_dir, "monitor_tab.R"))
source(file.path(ui_dir, "retrieve_tab.R"))
source(file.path(ui_dir, "joblog_tab.R"))
source(file.path(ui_dir, "promote_tab.R"))
source(file.path(ui_dir, "condor_completed_tab.R"))
source(file.path(ui_dir, "settings_tab.R"))
