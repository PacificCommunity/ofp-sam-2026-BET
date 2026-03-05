mp_hessian_summary_blank <- function(requested = FALSE) {
  list(
    requested = isTRUE(requested),
    attempted = FALSE,
    run_ok = NA,
    error = NA_character_,
    info_file = NA_character_,
    is_pdh = NA,
    is_spd = NA,
    hessian_ok = NA,
    n_negative_eigenvalues = NA_integer_,
    n_total_eigenvalues = NA_integer_,
    hessian_status = NA_character_,
    reliability = NA_character_
  )
}

mp_parse_hessian_summary_from_info <- function(hinfo, summary) {
  pdh_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$pdh$is_pdh), error = function(e) NA))
  spd_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$positivised_cov_is_spd), error = function(e) NA))
  n_neg <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_negative_eigenvalues), error = function(e) NA_integer_))
  n_tot <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_total_eigenvalues), error = function(e) NA_integer_))
  h_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$hessian_status), error = function(e) NA_character_))
  rel_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$reliability), error = function(e) NA_character_))

  pdh_scalar <- if (length(pdh_val) > 0) pdh_val[[1]] else NA
  spd_scalar <- if (length(spd_val) > 0) spd_val[[1]] else NA

  summary$run_ok <- TRUE
  summary$is_pdh <- pdh_scalar
  summary$is_spd <- spd_scalar
  summary$hessian_ok <- if (!is.na(pdh_scalar)) {
    if (!is.na(spd_scalar)) isTRUE(pdh_scalar) && isTRUE(spd_scalar) else isTRUE(pdh_scalar)
  } else {
    NA
  }
  summary$n_negative_eigenvalues <- if (length(n_neg) > 0) n_neg[[1]] else NA_integer_
  summary$n_total_eigenvalues <- if (length(n_tot) > 0) n_tot[[1]] else NA_integer_
  summary$hessian_status <- if (length(h_status) > 0) h_status[[1]] else NA_character_
  summary$reliability <- if (length(rel_status) > 0) rel_status[[1]] else NA_character_
  summary
}

mp_parse_npars_from_par <- function(par_path, fallback_work_dir = NULL) {
  npars <- NA_integer_
  if (!file.exists(par_path)) return(npars)

  par_lines <- readLines(par_path, warn = FALSE)
  npars_line <- grep("# The number of parameters", par_lines)
  if (length(npars_line) > 0) {
    npars <- suppressWarnings(as.integer(scan(par_path, skip = npars_line, nlines = 1, quiet = TRUE)))
  }

  if ((!is.finite(npars) || is.na(npars) || npars <= 0) && !is.null(fallback_work_dir)) {
    indepvar_file <- file.path(fallback_work_dir, "indepvar.rpt")
    if (file.exists(indepvar_file)) {
      indep <- suppressWarnings(tryCatch(read.table(indepvar_file, header = TRUE), error = function(e) NULL))
      if (!is.null(indep) && nrow(indep) > 0) {
        npars <- nrow(indep)
      }
    }
  }

  if (!is.finite(npars) || is.na(npars) || npars <= 0) NA_integer_ else as.integer(npars)
}

mp_run_post_hessian <- function(work_dir,
                                program_path_abs,
                                program_path,
                                frq_file,
                                input_par,
                                project_root,
                                requested = FALSE) {
  summary <- mp_hessian_summary_blank(requested = requested)
  if (!isTRUE(requested)) return(summary)

  if (!dir.exists(work_dir)) {
    summary$run_ok <- FALSE
    summary$error <- paste("Work directory does not exist:", work_dir)
    return(summary)
  }
  if (!is.character(frq_file) || !nzchar(frq_file)) {
    summary$run_ok <- FALSE
    summary$error <- "Missing frq file for post-hessian run."
    return(summary)
  }

  input_par_name <- if (file.exists(input_par)) basename(input_par) else basename(input_par)
  input_par_path <- file.path(work_dir, input_par_name)
  if (!file.exists(input_par_path)) {
    summary$run_ok <- FALSE
    summary$error <- paste("Missing input par for post-hessian run:", input_par_name)
    return(summary)
  }

  summary$attempted <- TRUE
  hessian_dir <- file.path(work_dir, "hessian")
  part_dir <- file.path(hessian_dir, "part_1")
  dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

  seed_files <- list.files(work_dir, full.names = TRUE, recursive = FALSE)
  seed_files <- seed_files[file.info(seed_files)$isdir %in% FALSE]
  if (length(seed_files) > 0) {
    file.copy(seed_files, to = part_dir, overwrite = TRUE, recursive = FALSE)
  }

  npars <- mp_parse_npars_from_par(file.path(part_dir, input_par_name), fallback_work_dir = work_dir)
  if (!is.finite(npars) || is.na(npars) || npars <= 0) {
    summary$run_ok <- FALSE
    summary$error <- "Failed to determine number of parameters for post-hessian run."
    return(summary)
  }

  hessian_switch <- paste("-switch 3", "1 145 1", "1 223 1", "1 224", npars, sep = " ")
  hessian_out_par <- "hessian_1.par"
  hessian_cmd <- paste(
    shQuote(program_path_abs),
    shQuote(frq_file),
    shQuote(input_par_name),
    shQuote(hessian_out_par),
    hessian_switch
  )

  hessian_run_ok <- TRUE
  hessian_error <- NULL
  tryCatch(
    {
      run_commands(
        commands = hessian_cmd,
        work_dirs = part_dir,
        save_log = TRUE,
        parallel = FALSE,
        verbose = TRUE,
        log_file = file.path(part_dir, "mfcl_hessian_log.txt")
      )
    },
    error = function(e) {
      hessian_run_ok <<- FALSE
      hessian_error <<- conditionMessage(e)
    }
  )

  if (!isTRUE(hessian_run_ok)) {
    summary$run_ok <- FALSE
    summary$error <- if (!is.null(hessian_error) && nzchar(hessian_error)) hessian_error else "Post-hessian MFCL run failed."
    return(summary)
  }

  part_info <- list(
    hessian_part = 1L,
    nsplit = 1L,
    start_par = 1L,
    end_par = as.integer(npars),
    npars = as.integer(npars),
    frq_file = frq_file,
    program_path = program_path,
    model_dir = work_dir,
    part_dir = part_dir,
    base_dir = work_dir,
    input_par = input_par_name,
    output_par = hessian_out_par
  )
  saveRDS(part_info, file = file.path(part_dir, "hessian_info.rds"), compress = "xz")

  collate_script <- file.path(project_root, "tools", "collate_hessian_mfcl.R")
  collate_status <- suppressWarnings(system2(
    "Rscript",
    args = c(collate_script, work_dir),
    env = c(
      "hessian_compact_cleanup=true",
      "hessian_keep_hes=false"
    ),
    stdout = FALSE,
    stderr = FALSE
  ))

  hessian_info_file <- file.path(hessian_dir, "hessian_info.rds")
  summary$info_file <- hessian_info_file
  if (!identical(collate_status, 0L) || !file.exists(hessian_info_file)) {
    summary$run_ok <- FALSE
    summary$error <- "Post-hessian collate failed or hessian_info.rds not produced."
    return(summary)
  }

  hinfo <- suppressWarnings(tryCatch(readRDS(hessian_info_file), error = function(e) NULL))
  if (is.null(hinfo)) {
    summary$run_ok <- FALSE
    summary$error <- "Failed to read hessian_info.rds after post-hessian run."
    return(summary)
  }
  summary <- mp_parse_hessian_summary_from_info(hinfo, summary)
  summary$info_file <- NA_character_
  unlink(hessian_dir, recursive = TRUE, force = TRUE)
  summary
}
