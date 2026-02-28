mp_safe <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

mp_safe_read_lines <- function(path) {
  if (!file.exists(path)) return(NULL)
  mp_safe(readLines(path))
}

mp_first_or_null <- function(x) {
  if (length(x) > 0) x[[1]] else NULL
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
    avg_bio = suppressWarnings(as.numeric(info_out$actual_quantity)),
    obj_fun = if (!is.null(par_file) && file.exists(par_file)) mp_extract_par_obj_fun(par_file) else NA_real_,
    max_grad = if (!is.null(par_file) && file.exists(par_file)) mp_extract_par_max_grad(par_file) else NA_real_,
    output_par = if (!is.null(par_file)) basename(par_file) else NA_character_,
    lik_out = mp_safe(read.MFCLLikelihood(out_file)),
    lik_raw = mp_safe(readLines(out_file))
  )
}

mp_build_jitter_payload <- function(seed_dir, seed = NA_integer_) {
  out_par <- mp_first_or_null(list.files(seed_dir, pattern = "jittered_out_\\d+\\.par$", full.names = TRUE))
  if (is.null(out_par) || !file.exists(out_par)) return(NULL)

  out_obj <- mp_safe(read.MFCLPar(out_par))
  if (is.null(out_obj)) return(NULL)

  list(
    version = "v1",
    created_at = as.character(Sys.time()),
    seed_dir = seed_dir,
    seed = ifelse(is.na(seed), suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(seed_dir)))), seed),
    obj_fun = suppressWarnings(as.numeric(out_obj@obj_fun)),
    max_grad = suppressWarnings(as.numeric(out_obj@max_grad)),
    output_par = basename(out_par)
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
