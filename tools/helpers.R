#!/usr/bin/env Rscript
## Helper functions for MFCL assessment
## Separated from config.R for cleaner organization

get_mfcl_exe <- function(version = MFCL_VERSION) {
  if(version == "2026") {
    exe <- "mfclo64_2026_01_22_vsn2278"
  } else if(version == "2023") {
    exe <- "mfclo64_2023"
  } else {
    exe <- paste0("mfclo64_", version)
  }
  
  ## Try multiple paths (for local vs Condor environments)
  possible_paths <- c(
    file.path(MFCL_EXE_DIR, exe),           # Local: /full/path/mfcl/exe/...
    file.path("mfcl/exe", exe),              # Condor: relative from repo root
    file.path("../mfcl/exe", exe),           # From scripts/ directory
    file.path("../../mfcl/exe", exe)         # From model/base/ directory
  )
  
  for(exe_path in possible_paths) {
    if(file.exists(exe_path)) {
      return(normalizePath(exe_path))
    }
  }
  
  ## If not found, return relative path (will be resolved at runtime)
  return(file.path("mfcl/exe", exe))
}

get_input_files <- function() {
  list(
    frq = file.path(INPUTS_DIR, paste0(SPECIES, ".frq")),
    age_length = file.path(INPUTS_DIR, paste0(SPECIES, ".age_length")),
    tag = file.path(INPUTS_DIR, paste0(SPECIES, ".tag")),
    ini = file.path(INPUTS_DIR, paste0(SPECIES, ".ini")),
    cfg = file.path(INPUTS_DIR, "mfcl.cfg")  # Always mfcl.cfg, not species-specific
  )
}

check_input_files <- function() {
  files <- get_input_files()
  missing <- files[!file.exists(unlist(files))]
  if(length(missing) > 0) {
    stop("Missing files:\n  ", paste(missing, collapse = "\n  "))
  }
  invisible(TRUE)
}

create_dirs <- function() {
  dirs <- c(MODEL_DIR, HESSIAN_DIR, PROF_DIR, JITTER_DIR)
  for(d in dirs) {
    if(!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      cat("Created:", d, "\n")
    }
  }
  invisible(TRUE)
}
