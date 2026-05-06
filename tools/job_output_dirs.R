job_canonical_model_dir <- function(model_dir, launcher_model_name = Sys.getenv("launcher_model_name", "")) {
  model_dir <- trimws(as.character(model_dir[[1]]))
  if (!nzchar(model_dir)) model_dir <- "model/base"

  launcher_model_name <- trimws(as.character(launcher_model_name[[1]]))
  if (nzchar(launcher_model_name)) {
    parent <- dirname(model_dir)
    if (!nzchar(parent) || identical(parent, ".") || identical(parent, "")) parent <- "model"
    return(file.path(parent, launcher_model_name))
  }

  base <- basename(model_dir)
  canonical <- base
  canonical <- sub("_[^_]+_profchain_(down|up)$", "", canonical, ignore.case = TRUE, perl = TRUE)
  canonical <- sub("(_profchain_(down|up)|_prof2d|_model)$", "", canonical, ignore.case = TRUE, perl = TRUE)
  canonical <- sub("(_seed[0-9]+|_part[0-9]+|_peel[0-9]+|_selftest_rep[0-9]+)$", "", canonical, ignore.case = TRUE, perl = TRUE)
  canonical <- sub("_sc[-+]?[0-9.]+$", "", canonical, ignore.case = TRUE, perl = TRUE)

  if (!identical(canonical, base) && nzchar(canonical)) {
    return(file.path(dirname(model_dir), canonical))
  }
  model_dir
}
