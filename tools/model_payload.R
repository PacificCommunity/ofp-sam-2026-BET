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

mp_final_par <- function(folder) {
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

mp_build_model_payload <- function(folder, tag_report_year1 = 1952) {
  par_file <- mp_final_par(folder)
  rep_file <- mp_final_rep(folder)

  par_out <- if (!is.null(par_file) && file.exists(par_file)) mp_safe(read.MFCLPar(par_file)) else NULL
  rep_out <- if (!is.null(rep_file) && file.exists(rep_file)) mp_safe(read.MFCLRep(rep_file)) else NULL

  tagrep_out <- if (!is.null(par_file) && file.exists(par_file)) mp_safe(suppressWarnings(read.MFCLTagRep(par_file))) else NULL

  tagtemp_file <- file.path(folder, "temporary_tag_report")
  tagtemp_out <- if (file.exists(tagtemp_file)) {
    mp_safe(suppressWarnings(read.temporary_tag_report(tagtemp_file, year1 = tag_report_year1)))
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
      info = if (file.exists(file.path(folder, "model_info.rds"))) mp_safe(readRDS(file.path(folder, "model_info.rds"))) else NULL
    )
  )
}

mp_build_profile_payload <- function(scaler_dir) {
  out_file <- file.path(scaler_dir, "test_plot_output")
  if (!file.exists(out_file)) return(NULL)

  avg_bio <- NA_real_
  avg_bio_file <- file.path(scaler_dir, "avg_bio")
  if (file.exists(avg_bio_file)) {
    avg_bio <- suppressWarnings(as.numeric(read.table(avg_bio_file)))
  }

  list(
    version = "v1",
    created_at = as.character(Sys.time()),
    scaler_dir = scaler_dir,
    scaler = suppressWarnings(as.numeric(sub(".*?(\\d+)$", "\\1", basename(scaler_dir)))),
    avg_bio = avg_bio,
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
