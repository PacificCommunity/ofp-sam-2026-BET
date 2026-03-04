mp_safe <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

mp_safe_read_lines <- function(path) {
  if (!file.exists(path)) return(NULL)
  mp_safe(readLines(path))
}

mp_detect_jitter_run_state <- function(seed_dir,
                                       output_par_exists,
                                       obj_fun = NA_real_,
                                       max_grad = NA_real_,
                                       exit_status = NA_integer_,
                                       max_grad_converged_threshold = 0.01) {
  log_file <- file.path(seed_dir, "mfcl_log.txt")
  error_file <- file.path(seed_dir, "error.log")
  log_lines <- c(mp_safe_read_lines(log_file), mp_safe_read_lines(error_file))
  log_lines <- log_lines[!is.na(log_lines)]
  log_text <- if (length(log_lines) > 0) paste(log_lines, collapse = "\n") else ""

  has_fatal_log <- nzchar(log_text) && grepl(
    "Floating point exception|SIGFPE|segmentation fault|core dumped|Error trying to open|had status [1-9][0-9]*",
    log_text,
    ignore.case = TRUE
  )

  exit_status_scalar <- suppressWarnings(as.integer(if (length(exit_status) > 0) exit_status[1] else NA_integer_))
  if (length(exit_status_scalar) == 0 || is.na(exit_status_scalar)) {
    exit_status_scalar <- NA_integer_
  }

  obj_fun_scalar <- suppressWarnings(as.numeric(if (length(obj_fun) > 0) obj_fun[1] else NA_real_))
  if (length(obj_fun_scalar) == 0) obj_fun_scalar <- NA_real_

  max_grad_scalar <- suppressWarnings(as.numeric(if (length(max_grad) > 0) max_grad[1] else NA_real_))
  if (length(max_grad_scalar) == 0) max_grad_scalar <- NA_real_

  has_bad_exit_status <- isTRUE(is.finite(exit_status_scalar) && exit_status_scalar != 0L)
  run_completed <- isTRUE(output_par_exists) &&
    isTRUE(is.finite(obj_fun_scalar)) &&
    isTRUE(is.finite(max_grad_scalar)) &&
    !has_fatal_log &&
    !has_bad_exit_status
  converged <- isTRUE(run_completed) && isTRUE(abs(max_grad_scalar) <= max_grad_converged_threshold)

  run_status <- if (run_completed) {
    "completed"
  } else if (has_fatal_log || has_bad_exit_status) {
    "failed"
  } else if (length(log_lines) > 0) {
    "incomplete"
  } else {
    "unknown"
  }

  convergence_status <- if (!run_completed) {
    "not_completed"
  } else if (converged) {
    "converged"
  } else {
    "not_converged"
  }

  failure_reason <- if (run_completed) {
    NA_character_
  } else if (has_bad_exit_status) {
    sprintf("MFCL exited with status %s", exit_status_scalar)
  } else if (nzchar(log_text)) {
    fatal_hits <- grep(
      "Floating point exception|SIGFPE|segmentation fault|core dumped|Error trying to open|had status [1-9][0-9]*",
      log_lines,
      value = TRUE,
      ignore.case = TRUE
    )
    if (length(fatal_hits) > 0) fatal_hits[[1]] else "MFCL log exists but final output par was not created."
  } else {
    "No final jitter output par was found."
  }

  list(
    run_status = run_status,
    run_completed = run_completed,
    convergence_status = convergence_status,
    converged = converged,
    exit_status = exit_status_scalar,
    max_grad_converged_threshold = max_grad_converged_threshold,
    failure_reason = failure_reason,
    log_file_exists = file.exists(log_file),
    error_log_exists = file.exists(error_file)
  )
}

mp_first_or_null <- function(x) {
  if (length(x) > 0) x[[1]] else NULL
}

mp_extract_yearly_sum <- function(slot_obj, scale = 1) {
  slot_df <- mp_safe(as.data.frame(slot_obj))
  if (is.null(slot_df) || nrow(slot_df) == 0) return(NULL)

  slot_df$year <- suppressWarnings(as.numeric(slot_df$year))
  slot_df$data <- suppressWarnings(as.numeric(slot_df$data))
  slot_df <- slot_df[is.finite(slot_df$year) & is.finite(slot_df$data), , drop = FALSE]
  if (nrow(slot_df) == 0) return(NULL)

  out <- stats::aggregate(data ~ year, data = slot_df, FUN = sum)
  out$data <- out$data / scale
  out
}

mp_extract_recruitment_timeseries <- function(rep_obj) {
  rec_region_df <- mp_extract_yearly_sum(mp_safe(tryCatch(rep_obj@rec_region, error = function(e) NULL)), scale = 1e6)
  if (!is.null(rec_region_df) && nrow(rec_region_df) > 0) {
    names(rec_region_df)[names(rec_region_df) == "data"] <- "recruitment"
    return(rec_region_df)
  }

  eq_rec_df <- mp_extract_yearly_sum(mp_safe(tryCatch(rep_obj@eq_rec, error = function(e) NULL)), scale = 1e6)
  if (!is.null(eq_rec_df) && nrow(eq_rec_df) > 0) {
    names(eq_rec_df)[names(eq_rec_df) == "data"] <- "recruitment"
    return(eq_rec_df)
  }

  rec_df <- mp_extract_yearly_sum(mp_safe(tryCatch(rep_obj@rec, error = function(e) NULL)), scale = 1)
  if (!is.null(rec_df) && nrow(rec_df) > 0) {
    names(rec_df)[names(rec_df) == "data"] <- "recruitment"
    return(rec_df)
  }

  NULL
}

mp_extract_fm_timeseries <- function(rep_obj) {
  fm_df <- mp_safe(as.data.frame(mp_safe(tryCatch(rep_obj@fm, error = function(e) NULL))))
  popn_df <- mp_safe(as.data.frame(mp_safe(tryCatch(rep_obj@popN, error = function(e) NULL))))

  if (!is.null(fm_df) && !is.null(popn_df) && nrow(fm_df) > 0 && nrow(popn_df) > 0) {
    fm_df$data <- suppressWarnings(as.numeric(fm_df$data))
    popn_df$data <- suppressWarnings(as.numeric(popn_df$data))
    popn_df$N <- popn_df$data
    popn_df$data <- NULL

    numeric_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), union(names(fm_df), names(popn_df)))
    for (col in numeric_cols) {
      if (col %in% names(fm_df)) fm_df[[col]] <- suppressWarnings(as.numeric(fm_df[[col]]))
      if (col %in% names(popn_df)) popn_df[[col]] <- suppressWarnings(as.numeric(popn_df[[col]]))
    }

    join_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), intersect(names(fm_df), names(popn_df)))
    if (all(c("year", "season") %in% join_cols)) {
      fm_popn <- merge(fm_df, popn_df, by = join_cols, all = FALSE)
      fm_popn <- fm_popn[is.finite(fm_popn$year) & is.finite(fm_popn$season) & is.finite(fm_popn$data) & is.finite(fm_popn$N), , drop = FALSE]
      if (nrow(fm_popn) > 0) {
        fm_popn$catch <- fm_popn$data * fm_popn$N
        yearly <- stats::aggregate(
          cbind(total_catch = catch, total_N = N) ~ year + season,
          data = fm_popn,
          FUN = sum
        )
        if (nrow(yearly) > 0) {
          yearly$harvest_rate <- yearly$total_catch / pmax(yearly$total_N, .Machine$double.eps)
          yearly$inst_F <- -log(pmax(1 - yearly$harvest_rate, 0.001))
          out <- stats::aggregate(inst_F ~ year, data = yearly, FUN = sum)
          names(out)[names(out) == "inst_F"] <- "fishing_mortality"
          return(out)
        }
      }
    }
  }

  fmlevel_df <- mp_extract_yearly_sum(mp_safe(tryCatch(rep_obj@fmlevel, error = function(e) NULL)), scale = 1)
  if (!is.null(fmlevel_df) && nrow(fmlevel_df) > 0) {
    fmlevel_df <- stats::aggregate(data ~ year, data = fmlevel_df, FUN = function(x) mean(x, na.rm = TRUE))
    names(fmlevel_df)[names(fmlevel_df) == "data"] <- "fishing_mortality"
    return(fmlevel_df)
  }

  NULL
}

mp_extract_par_obj_fun <- function(par_file) {
  par_obj <- mp_safe(read.MFCLPar(par_file))
  obj_fun <- suppressWarnings(tryCatch(as.numeric(par_obj@obj_fun), error = function(e) NA_real_))
  if (is.finite(obj_fun)) return(obj_fun)

  lines <- mp_safe_read_lines(par_file)
  if (is.null(lines)) return(NA_real_)
  idx <- grep("^#\\s*Objective function value\\s*$", lines)
  if (length(idx) == 0 || idx[1] >= length(lines)) return(NA_real_)
  suppressWarnings(as.numeric(trimws(lines[idx[1] + 1])))
}

mp_extract_par_max_grad <- function(par_file) {
  par_obj <- mp_safe(read.MFCLPar(par_file))
  max_grad <- suppressWarnings(tryCatch(as.numeric(par_obj@max_grad), error = function(e) NA_real_))
  if (is.finite(max_grad)) return(max_grad)

  lines <- mp_safe_read_lines(par_file)
  if (is.null(lines)) return(NA_real_)
  idx <- grep("^#\\s*Maximum magnitude gradient value\\s*$", lines)
  if (length(idx) == 0 || idx[1] >= length(lines)) return(NA_real_)
  suppressWarnings(as.numeric(trimws(lines[idx[1] + 1])))
}

mp_final_par <- function(folder) {
  profile_priority <- c(
    "*finalmle.par",
    "*finalz.par",
    "*finalzz.par",
    "*finaly.par",
    "*finalx.par",
    "*final.par"
  )
  for (pat in profile_priority) {
    hits <- list.files(folder, pattern = glob2rx(pat), full.names = TRUE)
    if (length(hits) > 0) {
      file_info <- file.info(hits)
      return(rownames(file_info)[which.max(file_info$mtime)])
    }
  }

  f <- mp_safe(finalPar(folder))
  if (!is.null(f) && file.exists(f)) return(f)

  par_files <- list.files(folder, pattern = "\\.par$", full.names = TRUE)
  if (length(par_files) == 0) return(NULL)
  file_info <- file.info(par_files)
  rownames(file_info)[which.max(file_info$mtime)]
}

mp_final_rep <- function(folder) {
  rep_priority <- c(
    "plot-jittered_out_*.par.rep",
    "plot-*.par.rep",
    "plotq0-jittered_out_*.par.rep",
    "plotq0-*.par.rep"
  )
  for (pat in rep_priority) {
    hits <- list.files(folder, pattern = glob2rx(pat), full.names = TRUE)
    if (length(hits) > 0) {
      file_info <- file.info(hits)
      return(rownames(file_info)[which.max(file_info$mtime)])
    }
  }

  f <- mp_safe(finalRep(folder))
  if (!is.null(f) && file.exists(f)) return(f)

  rep_files <- list.files(folder, pattern = "\\.rep$", full.names = TRUE)
  if (length(rep_files) == 0) return(NULL)
  file_info <- file.info(rep_files)
  rownames(file_info)[which.max(file_info$mtime)]
}

mp_extract_rep_timeseries <- function(rep_obj, scenario = NA_character_, peel = 0L) {
  if (is.null(rep_obj)) return(NULL)

  bio_fish <- mp_safe(as.data.frame(rep_obj@adultBiomass))
  bio_nofish <- mp_safe(as.data.frame(rep_obj@adultBiomass_nofish))
  if (is.null(bio_fish) || is.null(bio_nofish)) return(NULL)

  bio_fish$year <- suppressWarnings(as.numeric(bio_fish$year))
  bio_fish$season <- suppressWarnings(as.numeric(bio_fish$season))
  bio_fish$data <- suppressWarnings(as.numeric(bio_fish$data))
  bio_fish <- bio_fish[is.finite(bio_fish$year) & is.finite(bio_fish$season) & is.finite(bio_fish$data), , drop = FALSE]
  if (nrow(bio_fish) == 0) return(NULL)
  bio_fish <- stats::aggregate(data ~ year + season, data = bio_fish, FUN = sum)
  names(bio_fish)[names(bio_fish) == "data"] <- "bio_fish"

  bio_nofish$year <- suppressWarnings(as.numeric(bio_nofish$year))
  bio_nofish$season <- suppressWarnings(as.numeric(bio_nofish$season))
  bio_nofish$data <- suppressWarnings(as.numeric(bio_nofish$data))
  bio_nofish <- bio_nofish[is.finite(bio_nofish$year) & is.finite(bio_nofish$season) & is.finite(bio_nofish$data), , drop = FALSE]
  if (nrow(bio_nofish) == 0) return(NULL)
  bio_nofish <- stats::aggregate(data ~ year + season, data = bio_nofish, FUN = sum)
  names(bio_nofish)[names(bio_nofish) == "data"] <- "bio_nofish"

  merged <- merge(bio_fish, bio_nofish, by = c("year", "season"), all = FALSE)
  if (nrow(merged) == 0) return(NULL)
  merged$depletion <- merged$bio_fish / pmax(merged$bio_nofish, .Machine$double.eps)

  dep <- stats::aggregate(depletion ~ year, data = merged, FUN = function(x) mean(x, na.rm = TRUE))
  sp <- stats::aggregate(bio_fish ~ year, data = merged, FUN = function(x) mean(x, na.rm = TRUE))
  sp$spawning_potential <- sp$bio_fish / 1e3
  sp$bio_fish <- NULL

  out <- merge(dep, sp, by = "year", all = FALSE)

  rec_df <- mp_extract_recruitment_timeseries(rep_obj)
  if (!is.null(rec_df) && nrow(rec_df) > 0) {
    out <- merge(out, rec_df, by = "year", all = TRUE)
  }

  fm_df <- mp_extract_fm_timeseries(rep_obj)
  if (!is.null(fm_df) && nrow(fm_df) > 0) {
    out <- merge(out, fm_df, by = "year", all = TRUE)
  }

  out$scenario <- scenario
  out$peel <- as.integer(peel)
  out
}

mp_build_model_payload <- function(folder, tag_report_year1 = "auto") {
  par_file <- mp_final_par(folder)
  rep_file <- mp_final_rep(folder)

  par_out <- if (!is.null(par_file) && file.exists(par_file)) mp_safe(read.MFCLPar(par_file)) else NULL
  rep_out <- if (!is.null(rep_file) && file.exists(rep_file)) mp_safe(read.MFCLRep(rep_file)) else NULL

  tagrep_out <- if (!is.null(par_file) && file.exists(par_file)) mp_safe(suppressWarnings(read.MFCLTagRep(par_file))) else NULL

  info_out <- if (file.exists(file.path(folder, "model_info.rds"))) mp_safe(readRDS(file.path(folder, "model_info.rds"))) else NULL
  model_min_year <- suppressWarnings(as.numeric(info_out$min_year))
  if (!is.finite(model_min_year)) model_min_year <- suppressWarnings(as.numeric(tryCatch(par_out@range["minyear"], error = function(e) NA)))

  tag_year1 <- if (is.numeric(tag_report_year1) && length(tag_report_year1) > 0 && is.finite(tag_report_year1[1])) {
    as.integer(tag_report_year1[1])
  } else if (is.null(tag_report_year1) || (length(tag_report_year1) == 1 && is.na(tag_report_year1)) || (is.character(tag_report_year1) && tolower(tag_report_year1) == "auto")) {
    if (is.finite(model_min_year)) as.integer(model_min_year) else NA_integer_
  } else {
    NA_integer_
  }

  tagtemp_file <- file.path(folder, "temporary_tag_report")
  tagtemp_out <- if (file.exists(tagtemp_file) && is.finite(tag_year1)) {
    mp_safe(suppressWarnings(read.temporary_tag_report(tagtemp_file, year1 = tag_year1)))
  } else {
    NULL
  }

  leng_file <- file.path(folder, "length.fit")
  leng_out <- if (file.exists(leng_file)) {
    mp_safe(suppressWarnings(read.MFCLLenFit(leng_file)))
  } else {
    NULL
  }

  weight_file <- file.path(folder, "weight.fit")
  weight_out <- if (file.exists(weight_file)) {
    mp_safe(suppressWarnings(read.MFCLWgtFit(weight_file)))
  } else {
    NULL
  }

  tag_file <- mp_first_or_null(list.files(folder, pattern = "\\.tag$", full.names = TRUE))
  age_file <- mp_first_or_null(list.files(folder, pattern = "\\.age_length$", full.names = TRUE))

  tag_out <- if (!is.null(tag_file) && file.exists(tag_file)) mp_safe(suppressWarnings(read.MFCLTag(tag_file))) else NULL
  age_out <- if (!is.null(age_file) && file.exists(age_file)) mp_safe(suppressWarnings(read.MFCLALK(age_file))) else NULL

  list(
    version = "v1",
    created_at = as.character(Sys.time()),
    folder = folder,
    files = list(par = par_file, rep = rep_file),
    data = list(
      ParOut = par_out,
      RepOut = rep_out,
      TagRepOut = tagrep_out,
      TagTempOut = tagtemp_out,
      LengOut = leng_out,
      WeightOut = weight_out,
      TagOut = tag_out,
      AgeOut = age_out,
      IndepOut = mp_safe_read_lines(file.path(folder, "indepvar.rpt")),
      info = info_out
    )
  )
}

mp_build_profile_payload <- function(scaler_dir) {
  out_file <- file.path(scaler_dir, "test_plot_output")
  if (!file.exists(out_file)) return(NULL)
  info_file <- file.path(scaler_dir, "info.rds")
  info_out <- if (file.exists(info_file)) mp_safe(readRDS(info_file)) else NULL
  par_file <- mp_final_par(scaler_dir)

  list(
    version = "v1",
    created_at = as.character(Sys.time()),
    scaler_dir = scaler_dir,
    scaler = suppressWarnings(as.numeric(sub(".*?(\\d+)$", "\\1", basename(scaler_dir)))),
    quantity_label = mp_safe(info_out$quantity_label),
    reference_quantity = suppressWarnings(as.numeric(info_out$reference_quantity)),
    target_quantity = suppressWarnings(as.numeric(info_out$target_quantity)),
    actual_quantity = suppressWarnings(as.numeric(info_out$actual_quantity)),
    target_rel_err = suppressWarnings(as.numeric(info_out$target_rel_err)),
    af172 = suppressWarnings(as.numeric(mp_safe(info_out$AgeFlags["Af172"]))),
    af173 = suppressWarnings(as.numeric(mp_safe(info_out$AgeFlags["Af173"]))),
    af174 = suppressWarnings(as.numeric(mp_safe(info_out$AgeFlags["Af174"]))),
    avg_bio = suppressWarnings(as.numeric(info_out$actual_quantity)),
    obj_fun = if (!is.null(par_file) && file.exists(par_file)) mp_extract_par_obj_fun(par_file) else NA_real_,
    max_grad = if (!is.null(par_file) && file.exists(par_file)) mp_extract_par_max_grad(par_file) else NA_real_,
    output_par = if (!is.null(par_file)) basename(par_file) else NA_character_,
    lik_out = mp_safe(read.MFCLLikelihood(out_file)),
    lik_raw = mp_safe(readLines(out_file))
  )
}

mp_jitter_parameter_changes_from_labels <- function(labels_df, summary_df = NULL, seed = NA_integer_) {
  seed_val <- seed
  if (is.null(labels_df) || nrow(labels_df) == 0) {
    return(list(
      files = NULL,
      labels = labels_df,
      summary = summary_df,
      family_stats = NULL,
      overall_stats = NULL
    ))
  }

  labels_df$before <- suppressWarnings(as.numeric(labels_df$before))
  labels_df$after <- suppressWarnings(as.numeric(labels_df$after))
  labels_df$delta <- suppressWarnings(as.numeric(labels_df$delta))

  pct_change_floor <- 1e-6
  denom <- ifelse(
    is.finite(labels_df$before) & abs(labels_df$before) >= pct_change_floor,
    abs(labels_df$before),
    NA_real_
  )
  labels_df$rel_change <- labels_df$delta / denom
  labels_df$pct_change <- 100 * labels_df$rel_change
  labels_df$abs_delta <- abs(labels_df$delta)
  labels_df$abs_pct_change <- abs(labels_df$pct_change)
  changed_col <- if ("changed" %in% names(labels_df)) labels_df$changed else rep(FALSE, nrow(labels_df))
  labels_df$changed_flag <- as.integer((!is.na(changed_col) & changed_col) | (is.finite(labels_df$delta) & abs(labels_df$delta) > 0))
  labels_df$seed <- seed_val

  family_stats <- stats::aggregate(rep(1, nrow(labels_df)), by = list(family = labels_df$family), FUN = sum)
  names(family_stats)[names(family_stats) == "x"] <- "n"

  changed_stats <- stats::aggregate(labels_df$changed_flag, by = list(family = labels_df$family), FUN = sum, na.rm = TRUE)
  mean_abs_delta_stats <- stats::aggregate(labels_df$abs_delta, by = list(family = labels_df$family), FUN = mean, na.rm = TRUE)
  median_abs_delta_stats <- stats::aggregate(labels_df$abs_delta, by = list(family = labels_df$family), FUN = stats::median, na.rm = TRUE)
  mean_abs_pct_stats <- stats::aggregate(labels_df$abs_pct_change, by = list(family = labels_df$family), FUN = mean, na.rm = TRUE)
  median_abs_pct_stats <- stats::aggregate(labels_df$abs_pct_change, by = list(family = labels_df$family), FUN = stats::median, na.rm = TRUE)

  names(changed_stats)[2] <- "changed"
  names(mean_abs_delta_stats)[2] <- "mean_abs_delta"
  names(median_abs_delta_stats)[2] <- "median_abs_delta"
  names(mean_abs_pct_stats)[2] <- "mean_abs_pct_change"
  names(median_abs_pct_stats)[2] <- "median_abs_pct_change"

  family_stats <- merge(family_stats, changed_stats, by = "family", all.x = TRUE)
  family_stats <- merge(family_stats, mean_abs_delta_stats, by = "family", all.x = TRUE)
  family_stats <- merge(family_stats, median_abs_delta_stats, by = "family", all.x = TRUE)
  family_stats <- merge(family_stats, mean_abs_pct_stats, by = "family", all.x = TRUE)
  family_stats <- merge(family_stats, median_abs_pct_stats, by = "family", all.x = TRUE)
  family_stats$changed_pct <- ifelse(family_stats$n > 0, 100 * family_stats$changed / family_stats$n, NA_real_)
  family_stats$seed <- seed_val

  overall_stats <- data.frame(
    seed = seed_val,
    n = nrow(labels_df),
    changed = sum(labels_df$changed_flag, na.rm = TRUE),
    changed_pct = 100 * mean(labels_df$changed_flag, na.rm = TRUE),
    mean_abs_delta = mean(labels_df$abs_delta, na.rm = TRUE),
    median_abs_delta = stats::median(labels_df$abs_delta, na.rm = TRUE),
    mean_abs_pct_change = mean(labels_df$abs_pct_change, na.rm = TRUE),
    median_abs_pct_change = stats::median(labels_df$abs_pct_change, na.rm = TRUE)
  )

  list(
    files = NULL,
    labels = labels_df,
    summary = summary_df,
    family_stats = family_stats,
    overall_stats = overall_stats
  )
}

mp_read_jitter_parameter_changes <- function(seed_dir, seed = NA_integer_, field_name = "parameter_changes") {
  seed_val <- ifelse(
    is.na(seed),
    suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(seed_dir)))),
    seed
  )
  label_file <- file.path(seed_dir, sprintf("jitter_seed_%s_label_changes.csv", seed_val))
  summary_file <- file.path(seed_dir, sprintf("jitter_seed_%s_summary.csv", seed_val))
  info_file <- file.path(seed_dir, "jitter_info.rds")

  if (file.exists(info_file)) {
    info_out <- mp_safe(readRDS(info_file))
    change_obj <- mp_safe(info_out[[field_name]])
    labels_df <- mp_safe(change_obj$labels)
    summary_df <- mp_safe(change_obj$summary)
    if (!is.null(labels_df) && nrow(labels_df) > 0) {
      return(mp_jitter_parameter_changes_from_labels(labels_df, summary_df = summary_df, seed = seed_val))
    }
  }

  labels_df <- if (file.exists(label_file)) mp_safe(utils::read.csv(label_file, stringsAsFactors = FALSE)) else NULL
  summary_df <- if (file.exists(summary_file)) mp_safe(utils::read.csv(summary_file, stringsAsFactors = FALSE)) else NULL
  out <- mp_jitter_parameter_changes_from_labels(labels_df, summary_df = summary_df, seed = seed_val)
  out$files <- list(label_changes = label_file, summary = summary_file)
  out
}

mp_extract_jitter_derived_quantities <- function(seed_dir, seed = NA_integer_) {
  rep_file <- mp_final_rep(seed_dir)
  if (is.null(rep_file) || !file.exists(rep_file)) {
    return(NULL)
  }

  rep_obj <- mp_safe(read.MFCLRep(rep_file))
  if (is.null(rep_obj)) {
    return(NULL)
  }

  ts_df <- mp_extract_rep_timeseries(rep_obj, scenario = NA_character_, peel = 0L)
  if (is.null(ts_df) || nrow(ts_df) == 0) {
    return(NULL)
  }

  recruitment_vals <- if ("recruitment" %in% names(ts_df)) {
    suppressWarnings(as.numeric(ts_df$recruitment))
  } else {
    rep(NA_real_, nrow(ts_df))
  }
  fishing_mortality_vals <- if ("fishing_mortality" %in% names(ts_df)) {
    suppressWarnings(as.numeric(ts_df$fishing_mortality))
  } else {
    rep(NA_real_, nrow(ts_df))
  }

  data.frame(
    seed = rep(
      ifelse(is.na(seed), suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(seed_dir)))), seed),
      nrow(ts_df)
    ),
    year = suppressWarnings(as.numeric(ts_df$year)),
    depletion = suppressWarnings(as.numeric(ts_df$depletion)),
    spawning_potential = suppressWarnings(as.numeric(ts_df$spawning_potential)),
    recruitment = recruitment_vals,
    fishing_mortality = fishing_mortality_vals,
    stringsAsFactors = FALSE
  )
}

mp_build_jitter_payload <- function(seed_dir, seed = NA_integer_) {
  info_file <- file.path(seed_dir, "jitter_info.rds")
  info_out <- if (file.exists(info_file)) mp_safe(readRDS(info_file)) else NULL
  seed_val <- ifelse(is.na(seed), suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(seed_dir)))), seed)
  mfcl_run <- mp_safe(info_out$mfcl_run)

  out_par <- mp_first_or_null(list.files(seed_dir, pattern = "jittered_out_\\d+\\.par$", full.names = TRUE))
  if (is.null(out_par) && !is.null(info_out$output_par)) {
    candidate_out <- file.path(seed_dir, info_out$output_par)
    if (file.exists(candidate_out)) out_par <- candidate_out
  }
  if (is.null(out_par) && !is.null(mfcl_run$output_par)) {
    candidate_out <- file.path(seed_dir, mfcl_run$output_par)
    if (file.exists(candidate_out)) out_par <- candidate_out
  }
  if (is.null(out_par)) {
    out_par <- mp_final_par(seed_dir)
  }

  out_exists <- !is.null(out_par) && file.exists(out_par)
  out_obj <- if (out_exists) mp_safe(read.MFCLPar(out_par)) else NULL
  obj_fun <- if (!is.null(out_obj)) suppressWarnings(as.numeric(out_obj@obj_fun)) else NA_real_
  max_grad <- if (!is.null(out_obj)) suppressWarnings(as.numeric(out_obj@max_grad)) else NA_real_

  parameter_changes <- mp_read_jitter_parameter_changes(seed_dir, seed, field_name = "parameter_changes")
  fitted_parameter_changes <- mp_read_jitter_parameter_changes(seed_dir, seed, field_name = "fitted_parameter_changes")
  derived_quantities <- mp_extract_jitter_derived_quantities(seed_dir, seed_val)
  exit_status <- suppressWarnings(as.integer(mp_safe(mfcl_run$exit_status)))
  run_checks <- mp_detect_jitter_run_state(
    seed_dir = seed_dir,
    output_par_exists = out_exists,
    obj_fun = obj_fun,
    max_grad = max_grad,
    exit_status = exit_status
  )
  run_status <- if (!is.null(mfcl_run$run_status)) mfcl_run$run_status else run_checks$run_status

  list(
    version = "v1",
    created_at = as.character(Sys.time()),
    seed_dir = seed_dir,
    seed = seed_val,
    run_status = run_status,
    run_completed = run_checks$run_completed,
    convergence_status = run_checks$convergence_status,
    converged = run_checks$converged,
    success = run_checks$run_completed,
    exit_status = exit_status,
    failure_reason = if (!is.null(mp_safe(mfcl_run$failure_reason))) mp_safe(mfcl_run$failure_reason) else run_checks$failure_reason,
    output_par_exists = out_exists,
    obj_fun = obj_fun,
    max_grad = max_grad,
    output_par = if (out_exists) basename(out_par) else mp_safe(info_out$output_par),
    parameter_changes = parameter_changes,
    fitted_parameter_changes = fitted_parameter_changes,
    derived_quantities = derived_quantities,
    mfcl_run = mfcl_run,
    run_checks = run_checks
  )
}

mp_cleanup_files <- function(dir_path, keep = character(0), recursive = TRUE) {
  if (!dir.exists(dir_path)) return(invisible(0L))
  files <- list.files(dir_path, full.names = TRUE, recursive = recursive, all.files = FALSE, no.. = TRUE)
  if (length(files) == 0) return(invisible(0L))
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0) return(invisible(0L))

  keep <- unique(keep[nzchar(keep)])
  keep_paths <- normalizePath(file.path(dir_path, keep), winslash = "/", mustWork = FALSE)
  target_paths <- normalizePath(files, winslash = "/", mustWork = FALSE)

  to_delete <- files[!(target_paths %in% keep_paths)]
  if (length(to_delete) > 0) {
    unlink(to_delete, recursive = FALSE, force = TRUE)
  }

  invisible(length(to_delete))
}
