app_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) normalizePath(getwd(), mustWork = FALSE)
)
source(file.path(app_dir, "global.R"))
source(file.path(app_dir, "ui.R"))
source(file.path(app_dir, "server.R"))

shinyApp(ui = ui, server = server)
