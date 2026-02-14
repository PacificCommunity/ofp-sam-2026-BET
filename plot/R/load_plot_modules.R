load_plot_modules <- function(base_dir = ".") {
  module_files <- c(
    "R/mod_general.R",
    "R/mod_model_meta.R",
    "R/mod_cpue.R",
    "R/mod_tag.R",
    "R/mod_fishery.R",
    "R/mod_overlay.R",
    "R/mod_model_io.R"
  )
  
  for (f in module_files) {
    source(file.path(base_dir, f), local = FALSE)
  }
  
  invisible(module_files)
}
