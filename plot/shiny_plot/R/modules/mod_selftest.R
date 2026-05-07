mod_selftest_ui <- function() {
  tabItem(
    tabName = "selftest",
    h2("Self-Test Recovery", style = "color: #3c8dbc;"),
    fluidRow(
      box(
        title = "Settings",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        pickerInput(
          "selftest_scenarios",
          "Replicates:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} reps selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search reps...",
            size = 10
          )
        ),
        pickerInput(
          "selftest_models",
          "Models:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} models selected",
            liveSearch = TRUE,
            size = 6
          )
        ),
        pickerInput(
          "selftest_sim_components",
          "Simulation data series:",
          choices = c(
            "Catch total" = "catch total",
            "CPUE" = "CPUE",
            "CPUE mean (old summaries)" = "CPUE mean",
            "Effort carrier" = "effort",
            "Length mean" = "length mean",
            "Length q10" = "length q10",
            "Length median" = "length median",
            "Length q90" = "length q90",
            "Weight mean" = "weight mean",
            "Weight q10" = "weight q10",
            "Weight median" = "weight median",
            "Weight q90" = "weight q90",
            "Tag recaptures" = "tag recaptures",
            "Tag recaptures by fishery" = "tag recaptures by fishery",
            "Age-length mean age" = "age-length mean age",
            "Age-length age q10" = "age-length age q10",
            "Age-length age median" = "age-length age median",
            "Age-length age q90" = "age-length age q90"
          ),
          selected = c(
            "catch total",
            "CPUE",
            "length mean",
            "weight mean",
            "tag recaptures",
            "age-length mean age"
          ),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} series selected",
            size = 6
          )
        ),
        selectInput(
          "selftest_recovery_layout",
          "Recovery layout:",
          choices = c("Overlay models" = "overlay", "Facet by model" = "facet"),
          selected = "overlay"
        ),
        selectInput(
          "selftest_sim_layout",
          "Simulation-check layout:",
          choices = c("Facet by model" = "facet", "Overlay selected models" = "overlay"),
          selected = "facet"
        ),
        pickerInput(
          "selftest_param_metrics",
          "Key parameter boxplot:",
          choices = c(
            "Total population" = "totpop",
            "Log M" = "LorenM",
            "L1" = "L1",
            "L2" = "L2",
            "Kappa" = "kappa",
            "s1" = "s1",
            "s2" = "s2",
            "Recent depletion" = "Recent depletion",
            "Recent SSB" = "Recent SSB",
            "Recent F" = "Recent F",
            "Recent recruitment" = "Recent recruitment",
            "Terminal depletion" = "Terminal depletion",
            "Terminal SSB" = "Terminal SSB"
          ),
          selected = c(
            "totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2",
            "Recent depletion", "Recent SSB", "Recent F", "Recent recruitment",
            "Terminal depletion", "Terminal SSB"
          ),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} quantities selected",
            size = 7
          )
        ),
        checkboxInput("selftest_show_replicates", "Show replicate traces", value = FALSE),
        checkboxInput("selftest_show_interval", "Show refit interval", value = TRUE),
        selectInput(
          "selftest_interval_level",
          "Interval:",
          choices = c("50%" = 0.50, "80%" = 0.80, "90%" = 0.90, "95%" = 0.95),
          selected = 0.95
        ),
        sliderInput(
          "selftest_recovery_height",
          "Recovery plot height (px)",
          min = 450,
          max = 1400,
          value = 900,
          step = 50
        ),
        sliderInput(
          "selftest_sim_height",
          "Simulation-check height (px)",
          min = 350,
          max = 1800,
          value = 850,
          step = 50
        ),
        actionButton(
          "selftest_apply_filters",
          "Apply",
          class = "btn-primary",
          style = "width: 100%;"
        ),
        tags$small(
          "Load the self-test refit folder, e.g. model/<run>/selftest/refit.",
          style = "display:block; margin-top:8px; color:#666;"
        )
      ),
      box(
        title = "Truth vs Refit",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        uiOutput("selftest_recovery_plot_ui")
      )
    ),
    fluidRow(
      box(
        title = "Pseudo-Data Simulation Checks",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        uiOutput("selftest_sim_plot_ui")
      )
    ),
    fluidRow(
      box(
        title = "Key Parameter Relative Difference",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        plotOutput("selftest_param_boxplot", height = "420px")
      )
    ),
    fluidRow(
      box(
        title = "Replicate Checks",
        width = 12,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,
        DT::DTOutput("selftest_sim_table")
      )
    )
  )
}

stp_first_or_null <- function(x) {
  if (length(x) > 0) x[[1]] else NULL
}

stp_rep_id <- function(x) {
  out <- suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", basename(as.character(x)))))
  ifelse(is.finite(out), out, seq_along(x))
}

stp_model_folder <- function(model_dir, scenario) {
  normalizePath(file.path(model_dir, scenario), mustWork = FALSE)
}

stp_selftest_sibling_dir <- function(model_dir, scenario, sibling) {
  folder <- stp_model_folder(model_dir, scenario)
  parent <- dirname(folder)
  root <- dirname(parent)
  if (!basename(parent) %in% c("refit", "inputs", "sim", "truth", "truth_eval", "recovery")) {
    return(file.path(parent, sibling, basename(folder)))
  }
  file.path(root, sibling, basename(folder))
}

stp_selftest_index <- function(model_dir, loaded_models = character()) {
  model_dir <- normalizePath(model_dir, winslash = "/", mustWork = FALSE)
  loaded_models <- as.character(loaded_models)
  loaded_models <- loaded_models[nzchar(loaded_models)]

  make_rows <- function(model_name, selftest_root) {
    refit_root <- file.path(selftest_root, "refit")
    sim_root <- file.path(selftest_root, "sim")
    truth_root <- file.path(selftest_root, "truth")
    truth_eval_root <- file.path(selftest_root, "truth_eval")
    input_root <- file.path(selftest_root, "inputs")
    recovery_root <- file.path(selftest_root, "recovery")
    if (!dir.exists(refit_root)) return(NULL)
    rep_dirs <- list.dirs(refit_root, recursive = FALSE, full.names = FALSE)
    rep_dirs <- rep_dirs[grepl("^rep_\\d+$", rep_dirs)]
    if (length(rep_dirs) == 0) return(NULL)
    rep_dirs <- sort(rep_dirs)
    central_truth <- file.exists(file.path(truth_root, "model_info.rds")) ||
      file.exists(file.path(truth_root, "model_payload.rds"))
    data.frame(
      key = paste(model_name, rep_dirs, sep = "::"),
      label = paste(model_name, rep_dirs),
      model = model_name,
      replicate_name = rep_dirs,
      replicate = stp_rep_id(rep_dirs),
      refit_dir = file.path(refit_root, rep_dirs),
      truth_dir = if (isTRUE(central_truth)) truth_root else file.path(sim_root, rep_dirs),
      truth_eval_dir = file.path(truth_eval_root, rep_dirs),
      input_dir = file.path(input_root, rep_dirs),
      recovery_dir = file.path(recovery_root, rep_dirs),
      stringsAsFactors = FALSE
    )
  }

  parent <- basename(model_dir)
  grandparent <- basename(dirname(model_dir))
  if (parent %in% c("refit", "sim", "truth", "truth_eval", "inputs", "recovery") && identical(grandparent, "selftest")) {
    model_name <- basename(dirname(dirname(model_dir)))
    return(make_rows(model_name, dirname(model_dir)))
  }

  if (identical(parent, "selftest")) {
    model_name <- basename(dirname(model_dir))
    return(make_rows(model_name, model_dir))
  }

  rows <- lapply(loaded_models, function(model_name) {
    make_rows(model_name, file.path(model_dir, model_name, "selftest"))
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0 && dir.exists(file.path(model_dir, "selftest"))) {
    out <- make_rows(basename(model_dir), file.path(model_dir, "selftest"))
  }
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(
      key = character(), label = character(), model = character(),
      replicate_name = character(), replicate = integer(),
      refit_dir = character(), truth_dir = character(), truth_eval_dir = character(), input_dir = character(),
      recovery_dir = character(),
      stringsAsFactors = FALSE
    ))
  }
  out
}

stp_filter_selftest_index <- function(idx, filters) {
  if (is.null(idx) || nrow(idx) == 0) return(idx)
  selected_models <- as.character(filters$models)
  selected_models <- selected_models[nzchar(selected_models)]
  if (length(selected_models) > 0) {
    idx <- idx[idx$model %in% selected_models, , drop = FALSE]
  }
  selected <- intersect(as.character(filters$scenarios), idx$key)
  if (length(selected) == 0 && nrow(idx) > 0) selected <- idx$key
  idx[idx$key %in% selected, , drop = FALSE]
}

stp_read_model_rep <- function(folder) {
  payload_file <- file.path(folder, "model_payload.rds")
  if (file.exists(payload_file)) {
    payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
    rep_obj <- tryCatch(payload$data$RepOut, error = function(e) NULL)
    if (!is.null(rep_obj)) return(rep_obj)
  }

  rep_file <- tryCatch(mp_final_rep(folder), error = function(e) NULL)
  if (is.null(rep_file) || !file.exists(rep_file)) return(NULL)
  tryCatch(read.MFCLRep(rep_file), error = function(e) NULL)
}

stp_read_model_par <- function(folder) {
  payload_file <- file.path(folder, "model_payload.rds")
  if (file.exists(payload_file)) {
    payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
    par_obj <- tryCatch(payload$data$ParOut, error = function(e) NULL)
    if (!is.null(par_obj)) return(par_obj)
  }
  par_files <- list.files(folder, pattern = "\\.par[0-9]*$|^[0-9]+\\.par$", full.names = TRUE)
  if (length(par_files) == 0) return(NULL)
  info <- file.info(par_files)
  par_file <- par_files[order(-as.numeric(info$mtime), basename(par_files))][[1]]
  tryCatch(read.MFCLPar(par_file), error = function(e) NULL)
}

stp_profile_value_vector <- function(par_obj, profile_name) {
  out <- tryCatch({
    if (identical(profile_name, "totpop")) {
      tot_pop(par_obj)
    } else if (identical(profile_name, "LorenM")) {
      as.vector(aperm(log_m(par_obj), c(4, 1, 2, 3, 5, 6)))
    } else if (identical(profile_name, "L1")) {
      growth(par_obj)[1, 1]
    } else if (identical(profile_name, "L2")) {
      growth(par_obj)[2, 1]
    } else if (identical(profile_name, "kappa")) {
      growth(par_obj)[3, 1]
    } else if (identical(profile_name, "s1")) {
      growth_var_pars(par_obj)[1, 1]
    } else if (identical(profile_name, "s2")) {
      growth_var_pars(par_obj)[2, 1]
    } else {
      numeric()
    }
  }, error = function(e) numeric())
  out <- suppressWarnings(as.numeric(out))
  out[is.finite(out)]
}

stp_par_key_values <- function(par_obj) {
  if (is.null(par_obj)) return(data.frame())
  profile_names <- c("totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2")
  rows <- lapply(profile_names, function(group) {
    val <- stp_profile_value_vector(par_obj, group)
    if (length(val) == 0) return(NULL)
    data.frame(
      parameter = group,
      index = seq_along(val),
      value = val,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

stp_par_relative_diff <- function(truth_par, refit_par, scenario, replicate) {
  truth <- stp_par_key_values(truth_par)
  refit <- stp_par_key_values(refit_par)
  if (nrow(truth) == 0 || nrow(refit) == 0) return(data.frame())
  inner_join(
    rename(truth, truth_value = value),
    rename(refit, refit_value = value),
    by = c("parameter", "index")
  ) %>%
    mutate(
      scenario = scenario,
      model = stp_model_from_scenario(scenario),
      replicate = replicate,
      rel_diff = if_else(
        parameter == "LorenM",
        (refit_value - truth_value) / abs(truth_value),
        (refit_value - truth_value) / truth_value
      )
    ) %>%
    filter(is.finite(rel_diff), abs(truth_value) > 1e-8)
}

stp_read_derived_recovery <- function(recovery_dir, scenario, replicate, model = stp_model_from_scenario(scenario)) {
  path <- file.path(recovery_dir, "derived_recovery.csv")
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(x) || nrow(x) == 0 || !"year" %in% names(x)) return(NULL)
  metrics <- stp_recovery_metrics()
  rows <- lapply(metrics, function(metric) {
    truth_col <- paste0(metric, "_truth")
    refit_col <- paste0(metric, "_estimate")
    if (!all(c(truth_col, refit_col) %in% names(x))) return(NULL)
    data.frame(
      year = suppressWarnings(as.numeric(x$year)),
      metric = metric,
      quantity = stp_metric_label(metric),
      truth = suppressWarnings(as.numeric(x[[truth_col]])),
      refit = suppressWarnings(as.numeric(x[[refit_col]])),
      stringsAsFactors = FALSE
    ) %>%
      tidyr::pivot_longer(c("truth", "refit"), names_to = "source", values_to = "value") %>%
      mutate(scenario = scenario, model = model, replicate = replicate)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(NULL)
  out %>%
    mutate(
      year = suppressWarnings(as.numeric(year)),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(is.finite(year), is.finite(value))
}

stp_read_profile_parameter_recovery <- function(recovery_dir, scenario, replicate, model = stp_model_from_scenario(scenario)) {
  path <- file.path(recovery_dir, "profile_parameter_recovery.csv")
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(x) || nrow(x) == 0) return(NULL)
  if (!all(c("parameter", "index", "truth_value", "refit_value") %in% names(x))) return(NULL)
  if (!"rel_diff" %in% names(x)) {
    x$rel_diff <- ifelse(
      x$parameter == "LorenM",
      (x$refit_value - x$truth_value) / abs(x$truth_value),
      (x$refit_value - x$truth_value) / x$truth_value
    )
  }
  x %>%
    transmute(
      parameter = as.character(.data$parameter),
      index = suppressWarnings(as.integer(.data$index)),
      truth_value = suppressWarnings(as.numeric(.data$truth_value)),
      refit_value = suppressWarnings(as.numeric(.data$refit_value)),
      scenario = scenario,
      model = model,
      replicate = replicate,
      rel_diff = suppressWarnings(as.numeric(.data$rel_diff))
    ) %>%
    filter(is.finite(rel_diff), is.finite(truth_value), abs(truth_value) > 1e-8)
}

stp_read_recent_derived_recovery <- function(recovery_dir, scenario, replicate, model = stp_model_from_scenario(scenario), n_years = 4L) {
  path <- file.path(recovery_dir, "derived_recovery.csv")
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(x) || nrow(x) == 0 || !"year" %in% names(x)) return(NULL)
  x$year <- suppressWarnings(as.numeric(x$year))
  recent_metrics <- c(
    depletion = "Recent depletion",
    spawning_potential = "Recent SSB",
    fishing_mortality = "Recent F",
    recruitment = "Recent recruitment"
  )
  terminal_metrics <- c(
    depletion = "Terminal depletion",
    spawning_potential = "Terminal SSB"
  )
  make_row <- function(metric, label, reducer = c("recent", "terminal")) {
    reducer <- match.arg(reducer)
    truth_col <- paste0(metric, "_truth")
    refit_col <- paste0(metric, "_estimate")
    if (!all(c(truth_col, refit_col) %in% names(x))) return(NULL)
    y <- x %>%
      transmute(
        year = .data$year,
        truth_value = suppressWarnings(as.numeric(.data[[truth_col]])),
        refit_value = suppressWarnings(as.numeric(.data[[refit_col]]))
      ) %>%
      filter(is.finite(.data$year), is.finite(.data$truth_value), is.finite(.data$refit_value))
    if (nrow(y) == 0) return(NULL)
    if (identical(reducer, "recent")) {
      keep_years <- tail(sort(unique(y$year)), max(1L, as.integer(n_years)))
      y <- y %>% filter(.data$year %in% keep_years)
    } else {
      y <- y %>% filter(.data$year == max(.data$year, na.rm = TRUE))
    }
    truth_value <- mean(y$truth_value, na.rm = TRUE)
    refit_value <- mean(y$refit_value, na.rm = TRUE)
    if (!is.finite(truth_value) || abs(truth_value) <= 1e-8 || !is.finite(refit_value)) return(NULL)
    data.frame(
      parameter = label,
      index = 1L,
      truth_value = truth_value,
      refit_value = refit_value,
      scenario = scenario,
      model = model,
      replicate = replicate,
      rel_diff = (refit_value - truth_value) / truth_value,
      stringsAsFactors = FALSE
    )
  }
  rows <- c(
    lapply(names(recent_metrics), function(metric) make_row(metric, recent_metrics[[metric]], "recent")),
    lapply(names(terminal_metrics), function(metric) make_row(metric, terminal_metrics[[metric]], "terminal"))
  )
  bind_rows(rows)
}

stp_scalar_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || !is.finite(out[1])) NA_real_ else out[1]
}

stp_par_slot_num <- function(par_obj, slot_name) {
  if (is.null(par_obj) || !slot_name %in% slotNames(par_obj)) return(NA_real_)
  stp_scalar_num(tryCatch(slot(par_obj, slot_name), error = function(e) NA_real_))
}

stp_read_run_diagnostics <- function(folder, source, scenario, replicate) {
  payload_file <- file.path(folder, "model_payload.rds")
  payload <- if (file.exists(payload_file)) tryCatch(readRDS(payload_file), error = function(e) NULL) else NULL
  info_file <- file.path(folder, "model_info.rds")
  info <- if (file.exists(info_file)) tryCatch(readRDS(info_file), error = function(e) NULL) else NULL
  if (is.null(info)) info <- tryCatch(payload$data$info, error = function(e) NULL)
  par_obj <- tryCatch(payload$data$ParOut, error = function(e) NULL)
  info_first <- identical(source, "truth_on_pseudo")
  obj_fun <- if (isTRUE(info_first)) stp_scalar_num(info$obj_fun) else stp_scalar_num(payload$obj_fun)
  if (!is.finite(obj_fun)) obj_fun <- if (isTRUE(info_first)) stp_scalar_num(payload$obj_fun) else stp_scalar_num(info$obj_fun)
  if (!is.finite(obj_fun) && !is.null(par_obj)) obj_fun <- stp_scalar_num(tryCatch(par_obj@obj_fun, error = function(e) NA_real_))
  max_grad <- if (isTRUE(info_first)) stp_scalar_num(info$max_grad) else stp_scalar_num(payload$max_grad)
  if (!is.finite(max_grad)) max_grad <- if (isTRUE(info_first)) stp_scalar_num(payload$max_grad) else stp_scalar_num(info$max_grad)
  if (!is.finite(max_grad) && !is.null(par_obj)) max_grad <- stp_scalar_num(tryCatch(par_obj@max_grad, error = function(e) NA_real_))
  tag_lik <- stp_par_slot_num(par_obj, "tag_lik")
  if (!is.finite(tag_lik)) tag_lik <- stp_scalar_num(info$tag_lik)
  mn_len_pen <- stp_par_slot_num(par_obj, "mn_len_pen")
  if (!is.finite(mn_len_pen)) mn_len_pen <- stp_scalar_num(info$mn_len_pen)
  exit_status <- suppressWarnings(as.integer(tryCatch(info$exit_status, error = function(e) NA_integer_)))
  if (length(exit_status) == 0 || is.na(exit_status[1])) exit_status <- NA_integer_ else exit_status <- exit_status[1]
  run_completed <- dir.exists(folder) && is.finite(obj_fun) && is.finite(max_grad) &&
    (is.na(exit_status) || identical(exit_status, 0L))
  data.frame(
    scenario = scenario,
    replicate = replicate,
    source = source,
    obj_fun = obj_fun,
    tag_lik = tag_lik,
    mn_len_pen = mn_len_pen,
    max_grad = max_grad,
    abs_max_grad = abs(max_grad),
    exit_status = exit_status,
    run_completed = run_completed,
    converged = isTRUE(run_completed) && isTRUE(abs(max_grad) <= 0.01),
    stringsAsFactors = FALSE
  )
}

stp_extract_metric <- function(rep_obj, metric) {
  if (is.null(rep_obj)) return(NULL)

  if (identical(metric, "depletion")) {
    bio_fish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass")), error = function(e) NULL)
    bio_nofish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass_nofish")), error = function(e) NULL)
    if (is.null(bio_fish) || is.null(bio_nofish)) return(NULL)
    return(
      bio_fish %>%
        rename(bio_fish = value) %>%
        inner_join(rename(bio_nofish, bio_nofish = value), by = "year") %>%
        mutate(value = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
        select(year, value)
    )
  }

  if (identical(metric, "spawning_potential")) {
    bio_fish <- tryCatch(collapse_reference_biomass(slot(rep_obj, "adultBiomass")), error = function(e) NULL)
    if (is.null(bio_fish)) return(NULL)
    return(rename(bio_fish, value = value))
  }

  if (identical(metric, "recruitment")) {
    rec <- tryCatch(safe_array_to_df(slot(rep_obj, "rec_region")), error = function(e) NULL)
    if (is.null(rec) || !"year" %in% names(rec) || !"data" %in% names(rec)) return(NULL)
    return(
      rec %>%
        mutate(year = suppressWarnings(as.numeric(as.character(year))),
               data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(data)) %>%
        group_by(year) %>%
        summarise(value = sum(data, na.rm = TRUE) / 1e6, .groups = "drop")
    )
  }

  if (identical(metric, "fishing_mortality")) {
    fm <- tryCatch(safe_array_to_df(slot(rep_obj, "fm")), error = function(e) NULL)
    if (is.null(fm) || !"year" %in% names(fm) || !"data" %in% names(fm)) return(NULL)
    return(
      fm %>%
        mutate(year = suppressWarnings(as.numeric(as.character(year))),
               data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(data)) %>%
        group_by(year) %>%
        summarise(value = mean(data, na.rm = TRUE), .groups = "drop")
    )
  }

  NULL
}

stp_metric_label <- function(metric) {
  switch(
    metric,
    depletion = "SB / SB(F=0)",
    spawning_potential = "Spawning potential ('000 t)",
    recruitment = "Recruitment (millions)",
    fishing_mortality = "Mean fishing mortality",
    metric
  )
}

stp_model_from_scenario <- function(scenario) {
  scenario <- as.character(scenario)
  out <- sub("::.*$", "", scenario)
  bad <- is.na(out) | !nzchar(out)
  out[bad] <- scenario[bad]
  out
}

stp_model_palette <- function(models) {
  models <- as.character(models)
  models <- models[!is.na(models) & nzchar(models)]
  models <- sort(unique(models))
  if (length(models) == 0) return(character())
  base <- c(
    "#4E79A7", "#F28E2B", "#59A14F", "#E15759",
    "#76B7B2", "#B07AA1", "#EDC948", "#9C755F",
    "#BAB0AC", "#1F77B4", "#D62728", "#2CA02C"
  )
  stats::setNames(rep(base, length.out = length(models)), models)
}

stp_recovery_metrics <- function() {
  c("depletion", "spawning_potential", "recruitment", "fishing_mortality")
}

stp_extract_all_metrics <- function(rep_obj) {
  bind_rows(lapply(stp_recovery_metrics(), function(metric) {
    x <- stp_extract_metric(rep_obj, metric)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    x$metric <- metric
    x$quantity <- stp_metric_label(metric)
    x
  }))
}

stp_summary_band <- function(df, interval_level = 0.95) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  interval_level <- suppressWarnings(as.numeric(interval_level[[1]]))
  if (!is.finite(interval_level)) interval_level <- 0.95
  interval_level <- max(0.01, min(0.99, interval_level))
  lower_prob <- (1 - interval_level) / 2
  upper_prob <- 1 - lower_prob
  group_cols <- intersect(c("model", "metric", "quantity"), names(df))
  df %>%
    group_by(across(all_of(c(group_cols, "year")))) %>%
    summarise(
      median = stats::median(value, na.rm = TRUE),
      lower = stats::quantile(value, probs = lower_prob, na.rm = TRUE, names = FALSE),
      upper = stats::quantile(value, probs = upper_prob, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

stp_recovery_axis_label <- function(metric) {
  switch(
    metric,
    depletion = bquote(SB/SB["F=0"]),
    recruitment = "Recruitment (Millions)",
    spawning_potential = bquote("Spawning Potential (" * 10^3 * " MT)"),
    fishing_mortality = "Annual Instantaneous F",
    stp_metric_label(metric)
  )
}

stp_recovery_panel <- function(metric, truth_med, refit, refit_band, filters, model_palette = character(), facet_model = FALSE) {
  metric_filter <- function(x) {
    if (is.null(x) || nrow(x) == 0 || !"metric" %in% names(x)) return(data.frame())
    x %>% filter(.data$metric == .env$metric)
  }
  truth_q <- metric_filter(truth_med)
  refit_q <- metric_filter(refit)
  band_q <- metric_filter(refit_band)
  line_size <- 1.05
  model_count <- length(unique(c(truth_q$model, refit_q$model, band_q$model)))

  p <- ggplot()
  if (identical(metric, "depletion")) {
    p <- p +
      geom_hline(yintercept = 0.5, color = "#2e7d32", linetype = "dashed", linewidth = 0.45) +
      geom_hline(yintercept = 0.2, color = "#c62828", linetype = "dashed", linewidth = 0.45)
  }
  if (isTRUE(filters$show_interval) && nrow(band_q) > 0) {
    p <- p +
      geom_ribbon(
        data = band_q,
        aes(x = .data$year, ymin = .data$lower, ymax = .data$upper, group = .data$model, fill = .data$model),
        alpha = 0.16
      )
  }
  if (isTRUE(filters$show_replicates) && nrow(refit_q) > 0) {
    p <- p +
      geom_line(
        data = refit_q,
        aes(x = .data$year, y = .data$value, group = .data$scenario, color = .data$model),
        alpha = 0.20,
        linewidth = 0.35
      )
  }
  if (nrow(band_q) > 0) {
    p <- p +
      geom_line(
        data = band_q,
        aes(x = .data$year, y = .data$median, group = .data$model, color = .data$model),
        linewidth = line_size
      )
  }
  if (nrow(truth_q) > 0) {
    p <- p +
      geom_line(
        data = truth_q,
        aes(x = .data$year, y = .data$median, group = .data$model, color = .data$model),
        linewidth = line_size,
        linetype = "dashed"
      )
  }

  p <- p +
    scale_color_manual(
      name = "Model",
      values = model_palette,
      breaks = names(model_palette)
    ) +
    scale_fill_manual(values = model_palette, guide = "none") +
    labs(x = "Year", y = stp_recovery_axis_label(metric)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_line(color = "#f1f1f1", linewidth = 0.25),
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.35),
      legend.position = "none",
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      plot.margin = margin(6, 8, 6, 8)
    )

  if (identical(metric, "depletion")) {
    p <- p + coord_cartesian(ylim = c(0, 1.05))
  } else if (metric %in% c("recruitment", "spawning_potential", "fishing_mortality")) {
    p <- p + expand_limits(y = 0)
  }
  if (isTRUE(facet_model) && model_count > 1L) {
    p <- p +
      facet_wrap(~ model, nrow = 1) +
      theme(strip.background = element_rect(fill = "#eef3f7", color = NA))
  }

  p
}

stp_read_selftest_input_info <- function(model_dir, scenario, input_dir = NULL) {
  model_folder <- stp_model_folder(model_dir, scenario)
  input_folder <- if (!is.null(input_dir) && nzchar(input_dir)) {
    input_dir
  } else {
    stp_selftest_sibling_dir(model_dir, scenario, "inputs")
  }
  candidates <- c(
    file.path(model_folder, "selftest_input_info.rds"),
    file.path(input_folder, "selftest_input_info.rds")
  )
  path <- stp_first_or_null(candidates[file.exists(candidates)])
  if (is.null(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(out)) return(NULL)
  out
}

stp_read_data_simulation_summary <- function(input_dir, scenario, model = "") {
  if (is.null(input_dir) || !nzchar(input_dir)) return(NULL)
  path <- file.path(input_dir, "data_simulation_summary.rds")
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(out) || nrow(out) == 0) return(NULL)
  if (!"series" %in% names(out)) out$series <- "all"
  out$scenario <- scenario
  out$model <- model
  out$replicate <- stp_rep_id(scenario)
  out
}

stp_series_fishery <- function(series) {
  out <- suppressWarnings(as.integer(sub("^fishery_", "", as.character(series))))
  ifelse(is.finite(out), out, NA_integer_)
}

stp_fishery_region <- function(fishery_num, mapping = NULL) {
  key <- as.character(fishery_num)
  if (is.data.frame(mapping) && all(c("fishery", "region") %in% names(mapping))) {
    idx <- which(as.character(mapping$fishery) == key)
    if (length(idx) > 0) {
      region <- mapping$region[[idx[[1]]]]
      if (!is.null(region) && length(region) > 0 && !is.na(region) && nzchar(as.character(region))) {
        return(as.character(region))
      }
    }
  }
  if (is.finite(suppressWarnings(as.numeric(fishery_num)))) paste0("fishery ", fishery_num) else "unknown"
}

stp_cpue_fisheries <- function(model_dir, model = "") {
  candidates <- c(
    file.path(model_dir, "model_payload.rds"),
    if (nzchar(model)) file.path(dirname(model_dir), model, "model_payload.rds") else NA_character_,
    if (nzchar(model)) file.path("model", model, "model_payload.rds") else NA_character_
  )
  candidates <- candidates[!is.na(candidates) & file.exists(candidates)]
  for (path in candidates) {
    payload <- tryCatch(readRDS(path), error = function(e) NULL)
    flags <- tryCatch(payload$data$ParOut@flags, error = function(e) NULL)
    if (!is.data.frame(flags) || !all(c("flagtype", "flag", "value") %in% names(flags))) next
    vals <- flags[flags$flagtype < 0 & flags$flag == 92, c("flagtype", "value"), drop = FALSE]
    vals$value <- suppressWarnings(as.numeric(vals$value))
    out <- abs(as.integer(vals$flagtype[is.finite(vals$value) & vals$value != 0]))
    if (length(out) > 0) return(out)
  }
  integer(0)
}

stp_input_info_row <- function(info, scenario, model = stp_model_from_scenario(scenario)) {
  get_num <- function(name) {
    x <- suppressWarnings(as.numeric(info[[name]]))
    if (length(x) == 0 || !is.finite(x[1])) NA_real_ else x[1]
  }
  get_chr <- function(name) {
    x <- info[[name]]
    if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  }
  get_lgl <- function(name) {
    x <- info[[name]]
    if (length(x) == 0 || is.na(x[1])) NA else isTRUE(x[1])
  }

  data.frame(
    scenario = scenario,
    model = model,
    replicate = stp_rep_id(scenario),
    catch_conditioned = get_lgl("catch_conditioned"),
    effort_conditioned = get_lgl("effort_conditioned"),
    update_catch = get_lgl("update_catch"),
    update_effort = get_lgl("update_effort"),
    catch_replaced_rows = get_num("catch_replaced_rows"),
    effort_replaced_rows = get_num("effort_replaced_rows"),
    cpue_replaced_rows = get_num("cpue_replaced_rows"),
    length_replaced_rows = get_num("length_replaced_rows"),
    weight_replaced_rows = get_num("weight_replaced_rows"),
    tag_recaptures_total = get_num("tag_recaptures_total"),
    tag_recapture_rows = get_num("tag_recapture_rows"),
    age_length_total_obs = get_num("age_length_total_obs"),
    age_length_total_sim = get_num("age_length_total_sim"),
    age_length_draw_size_mode = get_chr("age_length_draw_size_mode"),
    age_length_ess_min = get_num("age_length_ess_min"),
    age_length_ess_max = get_num("age_length_ess_max"),
    age_length_draw_n_total = get_num("age_length_draw_n_total"),
    age_length_draw_n_min = get_num("age_length_draw_n_min"),
    age_length_draw_n_max = get_num("age_length_draw_n_max"),
    age_length_sample_size_max_abs_diff = get_num("age_length_sample_size_max_abs_diff"),
    age_length_sample_size_tolerance = get_num("age_length_sample_size_tolerance"),
    length_sample_size_mismatch = get_num("length_sample_size_mismatch"),
    weight_sample_size_mismatch = get_num("weight_sample_size_mismatch"),
    age_length_zero_prediction_bins = get_num("age_length_zero_prediction_bins"),
    age_length_sample_sizes_matched = get_lgl("age_length_sample_sizes_matched"),
    base_minyear = get_num("base_minyear"),
    base_maxyear = get_num("base_maxyear"),
    pseudo_minyear = get_num("pseudo_minyear"),
    pseudo_maxyear = get_num("pseudo_maxyear"),
    estimation_period_matched = get_lgl("estimation_period_matched"),
    tag_source = get_chr("tag_source"),
    age_length_source = get_chr("age_length_source"),
    stringsAsFactors = FALSE
  )
}

mod_selftest_server <- function(input, output, session, rv) {
  selftest_index_current <- reactive({
    req(rv$data_loaded)
    stp_selftest_index(input$model_dir, names(rv$RepOut_list))
  })

  selftest_filters_current <- reactive({
    list(
      scenarios = input$selftest_scenarios,
      models = input$selftest_models,
      show_replicates = isTRUE(input$selftest_show_replicates),
      show_interval = isTRUE(input$selftest_show_interval),
      recovery_layout = if (is.null(input$selftest_recovery_layout)) "overlay" else as.character(input$selftest_recovery_layout),
      sim_layout = if (is.null(input$selftest_sim_layout)) "facet" else as.character(input$selftest_sim_layout),
      param_metrics = if (is.null(input$selftest_param_metrics) || length(input$selftest_param_metrics) == 0) {
        c(
          "totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2",
          "Recent depletion", "Recent SSB", "Recent F", "Recent recruitment",
          "Terminal depletion", "Terminal SSB"
        )
      } else {
        as.character(input$selftest_param_metrics)
      },
      interval_level = {
        lvl <- suppressWarnings(as.numeric(input$selftest_interval_level))
        if (length(lvl) == 0 || !is.finite(lvl[1])) 0.95 else max(0.01, min(0.99, lvl[1]))
      },
      recovery_height = if (is.null(input$selftest_recovery_height)) 900 else suppressWarnings(as.integer(input$selftest_recovery_height)),
      sim_height = if (is.null(input$selftest_sim_height)) 850 else suppressWarnings(as.integer(input$selftest_sim_height)),
      sim_components = if (is.null(input$selftest_sim_components) || length(input$selftest_sim_components) == 0) {
        c("catch total", "CPUE", "length mean", "weight mean", "tag recaptures", "age-length mean age")
      } else {
        as.character(input$selftest_sim_components)
      }
    )
  })
  selftest_filters_applied <- reactiveVal(NULL)

  observeEvent(rv$data_loaded, {
    req(rv$data_loaded)
    idx <- selftest_index_current()
    choices <- if (nrow(idx) > 0) stats::setNames(idx$key, idx$label) else character()
    selected <- unname(choices)
    model_choices <- if (nrow(idx) > 0) sort(unique(idx$model)) else character()
    model_choices <- stats::setNames(model_choices, model_choices)
    updatePickerInput(session, "selftest_models", choices = model_choices, selected = unname(model_choices))
    updatePickerInput(session, "selftest_scenarios", choices = choices, selected = selected)
    if (length(selected) > 0) {
      current <- isolate(selftest_filters_current())
      current$scenarios <- selected
      current$models <- unname(model_choices)
      selftest_filters_applied(current)
    }
  }, ignoreInit = FALSE)

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(selftest_filters_current(), selftest_filters_applied())
    set_apply_pending(session, "selftest_apply_filters", pending)
  })

  observeEvent(input$selftest_apply_filters, {
    selftest_filters_applied(isolate(selftest_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$live_update_plots, input$selftest_scenarios, input$selftest_models,
         input$selftest_recovery_layout, input$selftest_sim_layout,
         input$selftest_show_replicates, input$selftest_show_interval,
         input$selftest_interval_level,
         input$selftest_recovery_height, input$selftest_sim_height,
         input$selftest_sim_components, input$selftest_param_metrics),
    {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      if (length(input$selftest_scenarios) == 0) return()
      selftest_filters_applied(isolate(selftest_filters_current()))
    },
    ignoreInit = TRUE
  )

  observeEvent(rv$initial_render_nonce, {
    req(rv$data_loaded, rv$initial_render_nonce)
    if (length(input$selftest_scenarios) == 0) return()
    selftest_filters_applied(isolate(selftest_filters_current()))
  }, ignoreInit = TRUE)

  selftest_recovery_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    if (nrow(idx) == 0) return(data.frame())

    rows <- bind_rows(lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      scenario <- row$key[[1]]
      model <- row$model[[1]]
      rep_id <- row$replicate[[1]]
      from_recovery <- stp_read_derived_recovery(row$recovery_dir[[1]], scenario, rep_id, model)
      if (!is.null(from_recovery) && nrow(from_recovery) > 0) return(from_recovery)

      refit_ts <- stp_extract_all_metrics(stp_read_model_rep(row$refit_dir[[1]]))
      truth_ts <- stp_extract_all_metrics(stp_read_model_rep(row$truth_dir[[1]]))

      bind_rows(
        if (!is.null(truth_ts)) {
          truth_ts %>%
            mutate(scenario = scenario, model = model, replicate = rep_id, source = "truth")
        },
        if (!is.null(refit_ts)) {
          refit_ts %>%
            mutate(scenario = scenario, model = model, replicate = rep_id, source = "refit")
        }
      )
    }))
    if (nrow(rows) == 0 || !all(c("year", "value") %in% names(rows))) return(data.frame())
    rows %>%
      mutate(
        model = if ("model" %in% names(.)) as.character(.data$model) else stp_model_from_scenario(.data$scenario),
        year = suppressWarnings(as.numeric(year)),
        value = suppressWarnings(as.numeric(value))
      ) %>%
      filter(is.finite(year), is.finite(value))
  })

  selftest_parameter_diff_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    if (nrow(idx) == 0) return(data.frame())
    bind_rows(lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      from_recovery <- stp_read_profile_parameter_recovery(
        row$recovery_dir[[1]],
        row$key[[1]],
        row$replicate[[1]],
        row$model[[1]]
      )
      param_rows <- if (!is.null(from_recovery) && nrow(from_recovery) > 0) {
        from_recovery
      } else {
        stp_par_relative_diff(
          stp_read_model_par(row$truth_dir[[1]]),
          stp_read_model_par(row$refit_dir[[1]]),
          row$key[[1]],
          row$replicate[[1]]
        )
      }
      derived_rows <- stp_read_recent_derived_recovery(
        row$recovery_dir[[1]],
        row$key[[1]],
        row$replicate[[1]],
        row$model[[1]]
      )
      bind_rows(param_rows, derived_rows)
    }))
  })

  selftest_sim_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      info <- stp_read_selftest_input_info(input$model_dir, row$key[[1]], row$input_dir[[1]])
      if (is.null(info)) return(NULL)
      stp_input_info_row(info, row$key[[1]], row$model[[1]])
    })
    bind_rows(rows)
  })

  selftest_data_simulation_series <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      stp_read_data_simulation_summary(row$input_dir[[1]], row$key[[1]], row$model[[1]])
    })
    bind_rows(rows)
  })

  selftest_convergence_data <- reactive({
    filters <- selftest_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      bind_rows(
        stp_read_run_diagnostics(row$truth_dir[[1]], "source_truth", row$key[[1]], row$replicate[[1]]),
        stp_read_run_diagnostics(row$truth_eval_dir[[1]], "truth_on_pseudo", row$key[[1]], row$replicate[[1]]),
        stp_read_run_diagnostics(row$refit_dir[[1]], "refit", row$key[[1]], row$replicate[[1]])
      )
    })
    bind_rows(rows)
  })

  selftest_recovery_plot <- reactive({
    filters <- selftest_filters_applied()
    req(filters)
    df <- selftest_recovery_data()
    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No self-test truth/refit data found", size = 5, color = "#777") +
          theme_void()
      )
    }

    truth <- df %>% filter(source == "truth")
    refit <- df %>% filter(source == "refit")
    interval_level <- if (!is.null(filters$interval_level)) filters$interval_level else 0.95
    model_palette <- stp_model_palette(df$model)
    truth_med <- stp_summary_band(truth, interval_level)
    refit_band <- stp_summary_band(refit, interval_level)
    metrics <- c("depletion", "recruitment", "spawning_potential", "fishing_mortality")
    facet_model <- identical(filters$recovery_layout, "facet") && length(unique(df$model)) > 1L
    panels <- lapply(
      metrics,
      stp_recovery_panel,
      truth_med = truth_med,
      refit = refit,
      refit_band = refit_band,
      filters = filters,
      model_palette = model_palette,
      facet_model = facet_model
    )
    body <- cowplot::plot_grid(plotlist = panels, ncol = 2, align = "hv")
    subtitle <- paste0(
      if (isTRUE(facet_model)) "Facets = model; " else "Colour = model; ",
      "solid = refit median; dashed = truth",
      if (isTRUE(filters$show_replicates)) "; faint lines = refit replicate traces" else "",
      if (isTRUE(filters$show_interval)) paste0("; shaded = ", round(interval_level * 100), "% interval") else ""
    )
    title <- cowplot::ggdraw() +
      cowplot::draw_label(subtitle, x = 0, hjust = 0, size = 10, color = "#555")
    if (isTRUE(facet_model)) {
      return(cowplot::plot_grid(title, body, ncol = 1, rel_heights = c(0.04, 1)))
    }
    legend_panel <- stp_recovery_panel(metrics[[1]], truth_med, refit, refit_band, filters, model_palette = model_palette) +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit(1.2, "cm")
      )
    legend <- cowplot::get_legend(legend_panel)
    cowplot::plot_grid(
      cowplot::plot_grid(title, body, ncol = 1, rel_heights = c(0.04, 1)),
      legend,
      ncol = 2,
      rel_widths = c(1, 0.16)
    )
  })

  selftest_sim_plot <- reactive({
    filters <- selftest_filters_applied()
    req(filters)
    info <- selftest_sim_data()
    if (nrow(info) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No pseudo-data build diagnostics found", size = 5, color = "#777") +
          theme_void()
      )
    }

    data_series <- selftest_data_simulation_series()

    if (nrow(data_series) > 0) {
      sim_components <- if (is.null(filters$sim_components) || length(filters$sim_components) == 0) {
        unique(as.character(data_series$component))
      } else {
        as.character(filters$sim_components)
      }
      annual <- data_series %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          series = if ("series" %in% names(.)) as.character(series) else "all",
          model = if ("model" %in% names(.)) as.character(model) else "",
          fishery_num = stp_series_fishery(series),
          base_value = suppressWarnings(as.numeric(base_value)),
          pseudo_value = suppressWarnings(as.numeric(pseudo_value))
        ) %>%
        filter(
          component %in% sim_components,
          is.finite(year), is.finite(base_value), is.finite(pseudo_value)
        )
      cpue_fisheries <- unique(unlist(lapply(unique(annual$model), function(model_name) {
        stp_cpue_fisheries(input$model_dir, model_name)
      }), use.names = FALSE))
      if (length(cpue_fisheries) > 0) {
        annual <- annual %>%
          filter(component != "CPUE" | fishery_num %in% cpue_fisheries)
      }
      if (nrow(annual) == 0) {
        p_data <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No selected simulation data series found", size = 4.2, color = "#777") +
          theme_void()
        return(p_data)
      }
      annual <- bind_rows(lapply(split(annual, seq_len(nrow(annual))), function(row) {
        fishery_components <- c(
          "catch total", "CPUE", "effort",
          "length mean", "length q10", "length median", "length q90",
          "weight mean", "weight q10", "weight median", "weight q90",
          "tag recaptures by fishery"
        )
        if (!row$component[[1]] %in% fishery_components) {
          row$display_component <- row$component[[1]]
          return(row)
        }
        fishery <- stp_series_fishery(row$series[[1]])
        map <- tryCatch(rv$FISHERY_MAPS[[row$model[[1]]]], error = function(e) NULL)
        region <- stp_fishery_region(fishery, map)
        row$display_component <- paste0(row$component[[1]], " - ", region)
        row
      }))
      annual <- annual %>%
        group_by(model, scenario, replicate, display_component, year) %>%
        summarise(
          base_value = mean(base_value, na.rm = TRUE),
          pseudo_value = mean(pseudo_value, na.rm = TRUE),
          .groups = "drop"
        )
      interval_level <- if (!is.null(filters$interval_level)) filters$interval_level else 0.95
      lower_prob <- (1 - interval_level) / 2
      upper_prob <- 1 - lower_prob
      facet_model <- identical(filters$sim_layout, "facet") && length(unique(annual$model)) > 1L
      annual <- annual %>%
        mutate(display_panel = if (isTRUE(facet_model)) paste(.data$model, .data$display_component, sep = " | ") else .data$display_component)
      pseudo_band <- annual %>%
        group_by(model, display_panel, year) %>%
        summarise(
          median = stats::median(pseudo_value, na.rm = TRUE),
          lower = stats::quantile(pseudo_value, lower_prob, na.rm = TRUE, names = FALSE),
          upper = stats::quantile(pseudo_value, upper_prob, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
      base_line <- annual %>%
        group_by(model, display_panel, year) %>%
        summarise(base_value = stats::median(base_value, na.rm = TRUE), .groups = "drop")
      p_data <- ggplot() +
        geom_ribbon(data = pseudo_band, aes(x = year, ymin = lower, ymax = upper), fill = "#95a5a6", alpha = 0.18) +
        geom_line(data = annual, aes(x = year, y = pseudo_value, group = interaction(model, scenario)), color = "#59636e", alpha = 0.42, linewidth = 0.38) +
        geom_line(data = pseudo_band, aes(x = year, y = median), color = "#111111", linewidth = 0.85) +
        geom_line(data = base_line, aes(x = year, y = base_value), color = "#d62728", alpha = 0.78, linewidth = 0.65) +
        facet_wrap(~ display_panel, scales = "free_y", ncol = 2) +
        labs(
          x = "Year",
          y = "Annual value",
          subtitle = paste0(
            "Data simulation check: red = fitted expectation, grey = pseudo reps, black = pseudo median",
            if (isTRUE(facet_model)) "; facets include model" else "",
            "; shaded = ", round(interval_level * 100), "% interval"
          )
        ) +
        theme_bw(base_size = 12) +
        theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "#eef3f7", color = NA))
      p_data
    } else {
      p_missing <- ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = "No pseudo-data value summary found. Re-run or retrieve self-test outputs created after this update.",
          size = 4.2, color = "#777"
        ) +
        theme_void()
      p_missing
    }
  })

  selftest_param_boxplot <- reactive({
    filters <- selftest_filters_applied()
    req(filters)
    df <- selftest_parameter_diff_data()
    if (nrow(df) == 0) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No key parameter comparison found", size = 4.2, color = "#777") +
        theme_void())
    }
    selected_params <- if (is.null(filters$param_metrics) || length(filters$param_metrics) == 0) {
      c(
        "totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2",
        "Recent depletion", "Recent SSB", "Recent F", "Recent recruitment",
        "Terminal depletion", "Terminal SSB"
      )
    } else {
      as.character(filters$param_metrics)
    }
    df <- df %>%
      mutate(
        model = if ("model" %in% names(.)) as.character(.data$model) else stp_model_from_scenario(.data$scenario),
        parameter = factor(parameter, levels = c(
          "totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2",
          "Recent depletion", "Recent SSB", "Recent F", "Recent recruitment",
          "Terminal depletion", "Terminal SSB"
        )),
        rel_diff_pct = 100 * rel_diff
      ) %>%
      filter(
        !is.na(parameter),
        as.character(parameter) %in% selected_params,
        is.finite(rel_diff_pct),
        abs(rel_diff_pct) < 1000
      )
    if (nrow(df) == 0) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No selected key parameter comparison found", size = 4.2, color = "#777") +
        theme_void())
    }
    model_palette <- stp_model_palette(df$model)
    ggplot(df, aes(x = parameter, y = rel_diff_pct, fill = model)) +
      geom_hline(yintercept = 0, color = "#777", linewidth = 0.35) +
      geom_boxplot(
        color = "#2d3436",
        outlier.alpha = 0.25,
        width = 0.65,
        position = position_dodge2(width = 0.78, preserve = "single")
      ) +
      scale_fill_manual(name = "Model", values = model_palette, breaks = names(model_palette)) +
      labs(x = NULL, y = "Relative difference (%)", subtitle = "Refit vs truth for key parameters plus recent 4-year derived quantities; LorenM uses log M with signed denominator abs(truth)") +
      theme_bw(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "right"
      )
  })

  output$selftest_recovery_plot_ui <- renderUI({
    filters <- selftest_filters_applied()
    h <- if (is.null(filters) || is.null(filters$recovery_height)) 900 else filters$recovery_height
    plotOutput("selftest_recovery_plot", height = paste0(h, "px"))
  })

  output$selftest_sim_plot_ui <- renderUI({
    filters <- selftest_filters_applied()
    h <- if (is.null(filters) || is.null(filters$sim_height)) 850 else filters$sim_height
    plotOutput("selftest_sim_plot", height = paste0(h, "px"))
  })

  output$selftest_recovery_plot <- renderPlot({
    selftest_recovery_plot()
  })

  output$selftest_sim_plot <- renderPlot({
    selftest_sim_plot()
  })

  output$selftest_param_boxplot <- renderPlot({
    selftest_param_boxplot()
  })

  output$selftest_sim_table <- DT::renderDT({
    info <- selftest_sim_data()
    if (nrow(info) == 0) {
      return(DT::datatable(data.frame(message = "No self-test input diagnostics found"), rownames = FALSE))
    }
    out <- info %>%
      transmute(
        scenario,
        model,
        replicate,
        catch_conditioned,
        effort_conditioned,
        update_catch,
        update_effort,
        catch_replaced_rows,
        effort_replaced_rows,
        cpue_replaced_rows,
        tag_source,
        tag_recaptures_total,
        age_length_source,
        age_length_sample_sizes_matched,
        age_length_draw_size_mode,
        age_length_ess_min,
        age_length_ess_max,
        age_length_draw_n_total,
        age_length_sample_size_max_abs_diff,
        length_sample_size_mismatch,
        weight_sample_size_mismatch,
        age_length_zero_prediction_bins
      ) %>%
      arrange(model, replicate)
    conv <- selftest_convergence_data()
    if (nrow(conv) > 0) {
      conv_wide <- conv %>%
        select(scenario, source, obj_fun, tag_lik, mn_len_pen, max_grad, exit_status, run_completed, converged) %>%
        tidyr::pivot_wider(
          names_from = source,
          values_from = c(obj_fun, tag_lik, mn_len_pen, max_grad, exit_status, run_completed, converged),
          names_glue = "{source}_{.value}"
        )
      for (nm in c(
        "refit_obj_fun", "refit_tag_lik", "refit_mn_len_pen", "refit_max_grad", "refit_converged", "refit_exit_status",
        "truth_on_pseudo_obj_fun", "truth_on_pseudo_tag_lik", "truth_on_pseudo_mn_len_pen", "truth_on_pseudo_max_grad",
        "source_truth_obj_fun", "source_truth_tag_lik", "source_truth_mn_len_pen", "source_truth_max_grad"
      )) {
        if (!nm %in% names(conv_wide)) conv_wide[[nm]] <- NA
      }
      out <- out %>%
        left_join(conv_wide, by = "scenario") %>%
        select(
          scenario, model, replicate,
          refit_obj_fun, refit_tag_lik, refit_mn_len_pen, refit_max_grad, refit_converged, refit_exit_status,
          truth_on_pseudo_obj_fun, truth_on_pseudo_tag_lik, truth_on_pseudo_mn_len_pen, truth_on_pseudo_max_grad,
          source_truth_obj_fun, source_truth_tag_lik, source_truth_mn_len_pen, source_truth_max_grad,
          everything()
        )
    }
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}
