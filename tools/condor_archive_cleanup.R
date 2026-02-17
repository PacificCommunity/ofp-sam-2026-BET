cb_condor_keep_only_model_cleanup <- function(
    keep_dir = "model"
) {
  is_condor <- Sys.getenv("_CONDOR_JOB_AD", "") != "" ||
    Sys.getenv("_CONDOR_SCRATCH_DIR", "") != "" ||
    Sys.getenv("CONDOR_ID", "") != ""
  
  if (!is_condor) {
    cat("Not running in Condor environment - skipping cleanup\n")
    return(invisible(FALSE))
  }
  
  wd <- getwd()
  keep_path <- file.path(wd, keep_dir)
  
  if (!dir.exists(keep_path)) {
    cat("Condor archive cleanup skipped: keep directory not found:", keep_path, "\n")
    return(invisible(FALSE))
  }
  
  entries <- list.files(wd, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(entries) == 0) {
    cat("Condor archive cleanup: nothing to delete in", wd, "\n")
    return(invisible(TRUE))
  }
  
  keep_norm <- normalizePath(keep_path, winslash = "/", mustWork = FALSE)
  entry_norm <- normalizePath(entries, winslash = "/", mustWork = FALSE)
  to_delete <- entries[entry_norm != keep_norm]
  
  if (length(to_delete) == 0) {
    cat("Condor archive cleanup: only", keep_dir, "exists in", wd, "\n")
    return(invisible(TRUE))
  }
  
  suppressWarnings(unlink(to_delete, recursive = TRUE, force = TRUE))
  failed <- to_delete[file.exists(to_delete)]
  
  if (length(failed) > 0) {
    cat("Condor archive cleanup finished with failures:", length(failed), "item(s)\n")
  } else {
    cat("Condor archive cleanup complete: kept", keep_dir, "and removed", length(to_delete), "item(s)\n")
  }
  
  invisible(length(failed) == 0)
}
