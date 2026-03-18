# Safely evaluate an expression and return NULL on error.
pm_safe_eval <- function(expr, label = "object", debug = FALSE, envir = parent.frame()) {
  tryCatch(
    eval(expr, envir = envir),
    error = function(e) {
      pm_debug_message(sprintf("Failed to read %s: %s", label, e$message), enabled = debug)
      NULL
    }
  )
}

# Discover immediate model folders from a model directory.
pm_discover_model_folders <- function(model_dir) {
  list.dirs(model_dir, full.names = TRUE, recursive = FALSE)
}

# Resolve year1 for temporary_tag_report.
# - numeric: use as-is
# - "auto"/NULL/NA: prefer info$min_year, then ParOut@range["minyear"]
pm_resolve_tag_report_year1 <- function(par_out, tag_report_year1 = "auto", info_obj = NULL) {
  if (is.numeric(tag_report_year1) && length(tag_report_year1) > 0 && is.finite(tag_report_year1[1])) {
    return(as.integer(tag_report_year1[1]))
  }

  use_auto <- is.null(tag_report_year1) ||
    (length(tag_report_year1) == 1 && is.na(tag_report_year1)) ||
    (is.character(tag_report_year1) && length(tag_report_year1) == 1 && tolower(tag_report_year1) == "auto")
  if (!use_auto) return(NA_integer_)

  minyear <- suppressWarnings(as.numeric(info_obj$min_year))
  minyear <- if (length(minyear) > 0) minyear[1] else NA_real_
  if (!is.finite(minyear)) {
    minyear <- suppressWarnings(as.numeric(tryCatch(par_out@range["minyear"], error = function(e) NA_real_)))
    minyear <- if (length(minyear) > 0) minyear[1] else NA_real_
  }
  if (is.finite(minyear)) as.integer(minyear) else NA_integer_
}

pm_read_model_payload <- function(folder, debug = FALSE) {
  payload_file <- file.path(folder, "model_payload.rds")
  if (!file.exists(payload_file)) return(NULL)
  payload <- pm_safe_eval(quote(readRDS(payload_file)), label = "model_payload", debug = debug)
  if (is.null(payload) || is.null(payload$data)) return(NULL)
  payload$data
}

# Read likelihood profile outputs from either new or legacy layout.
pm_read_likelihood_profiles <- function(folder, debug = FALSE) {
  profile_roots <- c(
    file.path(folder, "prof_indepvar"),
    file.path(folder, "prof")
  )
  scalar_dirs <- character(0)
  for (prof_dir in profile_roots) {
    if (!dir.exists(prof_dir)) next
    all_dirs <- list.dirs(prof_dir, full.names = TRUE, recursive = FALSE)
    hits <- grep("(scalar|scaler)_\\d+$", all_dirs, value = TRUE)
    if (length(hits) > 0) {
      scalar_dirs <- hits
      break
    }
  }
  
  if (length(scalar_dirs) > 0) {
    scales <- basename(scalar_dirs) %>% stringr::str_extract("\\d+$")
    profile_payload_files <- file.path(scalar_dirs, "profile_payload.rds")
    has_payload <- file.exists(profile_payload_files)
    
    if (any(has_payload)) {
      payloads <- purrr::map(profile_payload_files[has_payload], ~ pm_safe_eval(quote(readRDS(.x)), label = "profile_payload", debug = debug))
      payloads <- payloads[!vapply(payloads, is.null, logical(1))]
      if (length(payloads) > 0) {
        fallback_scales <- suppressWarnings(as.numeric(scales[has_payload]))
        fallback_scales <- fallback_scales[seq_len(length(payloads))]
        payload_scales <- as.character(vapply(seq_along(payloads), function(i) {
          x <- payloads[[i]]
          val <- NA_real_
          if (!is.null(x$scalar)) val <- suppressWarnings(as.numeric(x$scalar))
          if (!is.finite(val) && !is.null(x$scaler)) val <- suppressWarnings(as.numeric(x$scaler))
          if (!is.finite(val)) val <- fallback_scales[[i]]
          val
        }, numeric(1)))
        lik_out <- setNames(purrr::map(payloads, "lik_out"), payload_scales)
        lik_raw <- setNames(purrr::map(payloads, "lik_raw"), payload_scales)
        existing_scales <- payload_scales
      } else {
        lik_out <- list()
        lik_raw <- list()
        existing_scales <- character(0)
      }
    } else {
      output_files <- file.path(scalar_dirs, "test_plot_output")
      existing_files <- output_files[file.exists(output_files)]
      existing_scales <- scales[file.exists(output_files)]
    
      if (length(existing_files) > 0) {
        lik_out <- setNames(purrr::map(existing_files, ~ pm_safe_eval(quote(read.MFCLLikelihood(.x)), label = "LikOut", debug = debug)), existing_scales)
        lik_raw <- setNames(purrr::map(existing_files, ~ pm_safe_eval(quote(readLines(.x)), label = "LikRawOut", debug = debug)), existing_scales)
      } else {
        lik_out <- list()
        lik_raw <- list()
        existing_scales <- character(0)
      }
    }
  } else {
    output_files <- list.files(folder, pattern = "^test_plot_output_\\d+$", full.names = TRUE)
    if (length(output_files) > 0) {
      scales <- basename(output_files) %>% stringr::str_extract("\\d+$")
      lik_out <- setNames(purrr::map(output_files, ~ pm_safe_eval(quote(read.MFCLLikelihood(.x)), label = "LikOut", debug = debug)), scales)
      lik_raw <- setNames(purrr::map(output_files, ~ pm_safe_eval(quote(readLines(.x)), label = "LikRawOut", debug = debug)), scales)
      existing_scales <- scales
    } else {
      lik_out <- list()
      lik_raw <- list()
      existing_scales <- character(0)
    }
  }
  
  list(LikOut = lik_out, LikRawOut = lik_raw, scales = existing_scales)
}

# Read jitter outputs from jitter_seed_* subfolders.
pm_read_jitter_outputs <- function(folder, debug = FALSE) {
  jitter_dir <- file.path(folder, "jitter")
  jitter_seed_dirs <- list.dirs(jitter_dir, full.names = TRUE, recursive = FALSE)
  jitter_seed_dirs <- grep("jitter_seed_\\d+$", jitter_seed_dirs, value = TRUE)
  
  if (length(jitter_seed_dirs) == 0) {
    return(list(JitterPars = list(), JitterInfos = list()))
  }
  
  seeds <- basename(jitter_seed_dirs) %>% stringr::str_extract("\\d+$")
  
  jitter_pars <- setNames(purrr::map(jitter_seed_dirs, function(d) {
    jitter_payload_file <- file.path(d, "jitter_result.rds")
    if (file.exists(jitter_payload_file)) {
      return(pm_safe_eval(quote(readRDS(jitter_payload_file)), label = "jitter_result", debug = debug))
    }
    par_file <- list.files(d, pattern = "jittered_out_\\d+\\.par$", full.names = TRUE)
    if (length(par_file) == 0) return(NULL)
    par_obj <- pm_safe_eval(quote(read.MFCLPar(par_file[1])), label = "JitterPar", debug = debug)
    if (is.null(par_obj)) return(NULL)
    list(
      seed = suppressWarnings(as.integer(stringr::str_extract(basename(d), "\\d+$"))),
      obj_fun = suppressWarnings(as.numeric(par_obj@obj_fun)),
      max_grad = suppressWarnings(as.numeric(par_obj@max_grad)),
      output_par = basename(par_file[1]),
      source = "legacy_par"
    )
  }), seeds)
  
  jitter_infos <- setNames(purrr::map(jitter_seed_dirs, function(d) {
    info_file <- file.path(d, "jitter_info.rds")
    if (file.exists(info_file)) readRDS(info_file) else NULL
  }), seeds)
  
  list(
    JitterPars = Filter(Negate(is.null), jitter_pars),
    JitterInfos = Filter(Negate(is.null), jitter_infos)
  )
}

# Read one model folder and return a standardized result list.
# Returns NULL when required core files (Par/Rep) are unavailable.
pm_read_single_model <- function(folder, debug = FALSE, tag_report_year1 = "auto") {
  pm_debug_message(sprintf("Processing folder: %s", folder), enabled = debug)

  payload <- pm_read_model_payload(folder, debug = debug)
  if (!is.null(payload)) {
    par_out <- payload$ParOut
    rep_out <- payload$RepOut
    tagrep_out <- payload$TagRepOut
    tagtemp_out <- payload$TagTempOut
    len_out <- payload$LengOut
    wgt_out <- payload$WeightOut
    tag_out <- payload$TagOut
    age_out <- payload$AgeOut
    indep_out <- payload$IndepOut
    info <- payload$info
  } else {
    par_file <- pm_safe_eval(quote(finalPar(folder)), label = "finalPar", debug = debug)
    rep_file <- pm_safe_eval(quote(finalRep(folder)), label = "finalRep", debug = debug)

    par_out <- if (!is.null(par_file) && file.exists(par_file)) {
      pm_safe_eval(quote(read.MFCLPar(par_file)), label = "ParOut", debug = debug)
    } else NULL
    rep_out <- if (!is.null(rep_file) && file.exists(rep_file)) {
      pm_safe_eval(quote(read.MFCLRep(rep_file)), label = "RepOut", debug = debug)
    } else NULL

    if (is.null(par_out) || is.null(rep_out)) {
      pm_debug_message(sprintf("Skipping model due to missing Par/Rep: %s", basename(folder)), enabled = TRUE)
      return(NULL)
    }

    info <- pm_safe_eval(quote(readRDS(file.path(folder, "model_info.rds"))), label = "model_info", debug = debug)
    tagrep_out <- pm_safe_eval(quote(read.MFCLTagRep(par_file)), label = "TagRepOut", debug = debug)
    tag_year1 <- pm_resolve_tag_report_year1(par_out, tag_report_year1, info_obj = info)
    tagtemp_out <- if (is.finite(tag_year1)) {
      pm_safe_eval(
        quote(read.temporary_tag_report(file.path(folder, "temporary_tag_report"), year1 = tag_year1)),
        label = "TagTempOut",
        debug = debug
      )
    } else {
      pm_debug_message(sprintf("Skipping temporary_tag_report (min_year unavailable): %s", basename(folder)), enabled = debug)
      NULL
    }
    len_out <- pm_safe_eval(quote(read.MFCLLenFit(file.path(folder, "length.fit"))), label = "LengOut", debug = debug)
    wgt_out <- pm_safe_eval(quote(read.MFCLWgtFit(file.path(folder, "weight.fit"))), label = "WeightOut", debug = debug)

    tag_file <- list.files(folder, "\\.tag$", full.names = TRUE)
    tag_out <- if (length(tag_file) > 0) {
      pm_safe_eval(quote(read.MFCLTag(tag_file)), label = "TagOut", debug = debug)
    } else NULL

    age_file <- list.files(folder, "\\.age_length$", full.names = TRUE)
    age_out <- if (length(age_file) > 0) {
      pm_safe_eval(quote(read.MFCLALK(age_file)), label = "AgeOut", debug = debug)
    } else NULL

    indep_out <- if (file.exists(file.path(folder, "indepvar.rpt"))) readLines(file.path(folder, "indepvar.rpt")) else NULL
  }

  if (is.null(par_out) || is.null(rep_out)) {
    pm_debug_message(sprintf("Skipping model due to missing Par/Rep: %s", basename(folder)), enabled = TRUE)
    return(NULL)
  }

  lik <- pm_read_likelihood_profiles(folder, debug = debug)
  jit <- pm_read_jitter_outputs(folder, debug = debug)
  
  list(
    ParOut = par_out,
    RepOut = rep_out,
    LengOut = len_out,
    WeightOut = wgt_out,
    TagRepOut = tagrep_out,
    TagtempOut = tagtemp_out,
    TagOut = tag_out,
    AgeOut = age_out,
    LikOut = lik$LikOut,
    LikRawOut = lik$LikRawOut,
    scales = lik$scales,
    IndepOut = indep_out,
    info = info,
    JitterPars = jit$JitterPars,
    JitterInfos = jit$JitterInfos
  )
}

# Load all models from a model directory and return only valid models.
pm_load_models <- function(model_dir, debug = FALSE, tag_report_year1 = "auto") {
  model_folders <- pm_discover_model_folders(model_dir)
  model_names <- basename(model_folders)
  
  results_raw <- setNames(
    lapply(model_folders, pm_read_single_model, debug = debug, tag_report_year1 = tag_report_year1),
    model_names
  )
  
  Filter(Negate(is.null), results_raw)
}

# Split loaded model results into named lists used by plotting sections.
pm_extract_result_lists <- function(results) {
  list(
    ParOut_list = purrr::map(results, "ParOut"),
    RepOut_list = purrr::map(results, "RepOut"),
    LengOut_list = purrr::map(results, "LengOut") %>% purrr::discard(is.null),
    WeightOut_list = purrr::map(results, "WeightOut") %>% purrr::discard(is.null),
    TagOut_list = purrr::map(results, "TagOut") %>% purrr::discard(is.null),
    TagRepOut_list = purrr::map(results, "TagRepOut") %>% purrr::discard(is.null),
    TagTempOut_list = purrr::map(results, "TagtempOut") %>% purrr::discard(is.null),
    AgeOut_list = purrr::map(results, "AgeOut") %>% purrr::discard(is.null),
    IndepOut_list = purrr::map(results, "IndepOut"),
    Info_list = purrr::map(results, "info"),
    ProfLikOut_list = purrr::map(results, "LikOut") %>% purrr::discard(~ length(.x) == 0),
    ProfLikOutRaw_list = purrr::map(results, "LikRawOut") %>% purrr::discard(~ length(.x) == 0),
    JitterPars_list = purrr::map(results, "JitterPars"),
    JitterInfos_list = purrr::map(results, "JitterInfos")
  )
}

# Build per-model tag program map for downstream tag plots.
pm_build_tag_program_maps <- function(tag_out_list) {
  purrr::map(tag_out_list, ~ {
    .x@releases %>%
      dplyr::distinct(rel.group, program) %>%
      dplyr::rename(program_name = program)
  })
}
