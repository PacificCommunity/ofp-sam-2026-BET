mod_selftest_ui <- function() {
  selftest_model_picker <- function(input_id, label = "Models:") {
    pickerInput(
      input_id,
      label,
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectedTextFormat = "count > 0",
        countSelectedText = "{0} models selected",
        liveSearch = TRUE,
        size = 6
      )
    )
  }

  tabItem(
    tabName = "selftest",
    h2("Self-Test Recovery", style = "color: #3c8dbc;"),
    tabsetPanel(
      id = "selftest_main_tab",
      tabPanel(
        "Recovery",
        fluidRow(
          box(
            title = "Recovery Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            selftest_model_picker("selftest_recovery_models"),
            selectInput(
              "selftest_recovery_layout",
              "Recovery layout:",
              choices = c("Overlay models" = "overlay", "Facet by model" = "facet"),
              selected = "overlay"
            ),
            numericInput(
              "selftest_recovery_facet_cols",
              "Facet columns:",
              value = 2,
              min = 1,
              max = 8,
              step = 1
            ),
            checkboxInput("selftest_show_replicates", "Show replicate traces", value = FALSE),
            numericInput(
              "selftest_recovery_trace_limit",
              "Max traces shown:",
              value = 10,
              min = 0,
              max = 500,
              step = 1
            ),
            checkboxInput("selftest_show_interval", "Show refit interval", value = TRUE),
            selectInput(
              "selftest_interval_level",
              "Interval:",
              choices = c("50%" = 0.50, "80%" = 0.80, "90%" = 0.90, "95%" = 0.95),
              selected = 0.95
            ),
            sliderInput(
              "selftest_recovery_width",
              "Recovery plot width (px)",
              min = 900,
              max = 2600,
              value = 1000,
              step = 50
            ),
            sliderInput(
              "selftest_recovery_height",
              "Recovery plot height (px)",
              min = 450,
              max = 1400,
              value = 900,
              step = 50
            ),
            actionButton(
              "selftest_apply_filters",
              "Apply Recovery Settings",
              class = "btn-primary",
              style = "width: 100%;"
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
        )
      ),
      tabPanel(
        "Parameter Checks",
        fluidRow(
          box(
            title = "Parameter Settings",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            selftest_model_picker("selftest_param_models"),
            selectInput(
              "selftest_param_group",
              "Boxplot:",
              choices = c(
                "Core + recent quantities" = "core",
                "Movement coefficients" = "movement",
                "Recruitment deviations" = "recruitment",
                "All named indepvar parameters" = "indepvar"
              ),
              selected = "core"
            ),
            pickerInput(
              "selftest_param_metrics",
              "Key parameter boxplot:",
              choices = stp_key_param_choices(),
              selected = stp_default_key_params(),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = "count > 0",
                countSelectedText = "{0} quantities selected",
                size = 7
              )
            ),
            actionButton(
              "selftest_param_apply_filters",
              "Apply Parameter Settings",
              class = "btn-primary",
              style = "width: 100%;"
            )
          ),
          box(
            title = "Key Parameter Relative Difference",
            width = 9,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput("selftest_param_boxplot", height = "520px")
          )
        )
      ),
      tabPanel(
        "Pseudo-Data Checks",
        fluidRow(
          box(
            title = "Pseudo-Data Settings",
            width = 3,
            solidHeader = TRUE,
            status = "info",
            selectInput(
              "selftest_sim_object",
              "Plot object:",
              choices = stp_sim_object_choices(),
              selected = "cpue"
            ),
            selftest_model_picker(
              "selftest_sim_models",
              "Models in this plot:"
            ),
            selectInput(
              "selftest_sim_layout",
              "Simulation-check layout:",
              choices = c("Facet by model" = "facet", "Overlay selected models" = "overlay"),
              selected = "facet"
            ),
            checkboxInput("selftest_sim_show_traces", "Show pseudo replicate traces", value = FALSE),
            numericInput(
              "selftest_sim_trace_limit",
              "Max traces shown:",
              value = 10,
              min = 0,
              max = 500,
              step = 1
            ),
            selectInput(
              "selftest_sim_interval_level",
              "Interval:",
              choices = c("50%" = 0.50, "80%" = 0.80, "90%" = 0.90, "95%" = 0.95),
              selected = 0.95
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
              "selftest_sim_apply_filters",
              "Apply Pseudo-Data Settings",
              class = "btn-info",
              style = "width: 100%;"
            )
          ),
          box(
            title = "Pseudo-Data Simulation Checks",
            width = 9,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            uiOutput("selftest_sim_plot_ui")
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

stp_sim_object_choices <- function() {
  c(
    "CPUE" = "cpue",
    "Length composition" = "length",
    "Weight composition" = "weight",
    "Age-length" = "agelength",
    "Tag recaptures" = "tag",
    "Catch" = "catch",
    "Effort" = "effort"
  )
}

stp_sim_component_choices <- function() {
  c(
    "Catch total" = "catch total",
    "CPUE" = "CPUE",
    "CPUE mean (old summaries)" = "CPUE mean",
    "Effort carrier" = "effort",
    "Mean length by region" = "length mean",
    "Mean weight by region" = "weight mean",
    "Tag recaptures" = "tag recaptures",
    "Tag recaptures by region" = "tag recaptures by fishery",
    "Age-length mean age" = "age-length mean age",
    "Age-length age q10" = "age-length age q10",
    "Age-length age median" = "age-length age median",
    "Age-length age q90" = "age-length age q90"
  )
}

stp_region_component_label <- function(component) {
  component <- as.character(component)[[1]]
  switch(
    component,
    "catch total" = "catch by region",
    "CPUE" = "CPUE by region",
    "effort" = "effort by region",
    "tag recaptures by fishery" = "tag recaptures by region",
    "length mean" = "mean length by region",
    "weight mean" = "mean weight by region",
    "age-length mean age" = "mean age by region",
    "age-length age q10" = "age q10 by region",
    "age-length age median" = "age median by region",
    "age-length age q90" = "age q90 by region",
    component
  )
}

stp_sim_components_for_object <- function(object) {
  object <- stp_first_or_null(as.character(object))
  if (is.null(object) || !nzchar(object)) object <- "cpue"
  switch(
    object,
    cpue = c("CPUE"),
    length = c("length mean"),
    weight = c("weight mean"),
    agelength = c("age-length mean age", "age-length age q10", "age-length age median", "age-length age q90"),
    tag = c("tag recaptures by fishery"),
    catch = c("catch total"),
    effort = c("effort"),
    c("CPUE")
  )
}

stp_sim_y_label <- function(object) {
  object <- stp_first_or_null(as.character(object))
  if (is.null(object) || !nzchar(object)) object <- "cpue"
  switch(
    object,
    cpue = "CPUE",
    catch = "Catch",
    effort = "Effort",
    length = "Mean length",
    weight = "Mean weight",
    agelength = "Age",
    tag = "Tag recaptures",
    "Value"
  )
}

stp_default_key_params <- function() {
  c(
    "totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2",
    "Recent depletion (4yr avg)", "Recent SSB (4yr avg)",
    "Recent F (4yr avg)", "Recent recruitment (4yr avg)"
  )
}

stp_key_param_order <- function(params = NULL) {
  defaults <- stp_default_key_params()
  if (is.null(params)) return(defaults)
  params <- unique(as.character(params))
  params <- params[nzchar(params)]
  c(defaults[defaults %in% params], sort(setdiff(params, defaults)))
}

stp_key_param_choices <- function(params = NULL) {
  params <- stp_key_param_order(params)
  labels <- c(
    totpop = "Total population",
    LorenM = "Log M",
    L1 = "L1",
    L2 = "L2",
    kappa = "Kappa",
    s1 = "s1",
    s2 = "s2",
    "Recent depletion (4yr avg)" = "Recent depletion (4yr avg)",
    "Recent SSB (4yr avg)" = "Recent SSB (4yr avg)",
    "Recent F (4yr avg)" = "Recent F (4yr avg)",
    "Recent recruitment (4yr avg)" = "Recent recruitment (4yr avg)"
  )
  stats::setNames(params, ifelse(params %in% names(labels), labels[params], params))
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

stp_read_selftest_truth_par <- function(truth_dir, model_name = "", model_root = "") {
  candidates <- c(
    truth_dir,
    if (nzchar(model_root) && nzchar(model_name)) file.path(model_root, model_name) else NA_character_,
    if (nzchar(model_name)) file.path("model", model_name) else NA_character_
  )
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  for (candidate in candidates) {
    par_obj <- stp_read_model_par(candidate)
    if (!is.null(par_obj)) return(par_obj)
  }
  NULL
}

stp_flag_value <- function(par_obj, flagtype, flag_no) {
  if (is.null(par_obj)) return(NA_real_)
  out <- tryCatch(flagval(par_obj, flagtype, flag_no)$value, error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(out))
  if (length(out) == 0 || !is.finite(out[[1]])) NA_real_ else out[[1]]
}

stp_model_fixed_profile_parameters <- function(model_name) {
  x <- tolower(as.character(model_name))
  out <- character()
  if (grepl("fixm", x, fixed = TRUE)) out <- c(out, "LorenM")
  if (grepl("fixvb", x, fixed = TRUE)) out <- c(out, "L1", "L2", "kappa")
  unique(out)
}

stp_profile_parameter_estimated <- function(par_obj, parameter) {
  parameter <- as.character(parameter[[1]])
  flag <- switch(
    parameter,
    totpop = stp_flag_value(par_obj, 2L, 31L),
    LorenM = {
      age_flag <- stp_flag_value(par_obj, 2L, 33L)
      fixed_hint <- stp_flag_value(par_obj, 1L, 121L)
      if (is.finite(age_flag)) {
        age_flag
      } else if (is.finite(fixed_hint) && fixed_hint == 0) {
        0
      } else {
        NA_real_
      }
    },
    L1 = stp_flag_value(par_obj, 1L, 12L),
    L2 = stp_flag_value(par_obj, 1L, 13L),
    kappa = stp_flag_value(par_obj, 1L, 14L),
    NA_real_
  )
  if (!is.finite(flag)) return(NA)
  isTRUE(flag == 1)
}

stp_filter_estimated_profile_parameters <- function(df, truth_par, model_name = "") {
  if (is.null(df) || nrow(df) == 0 || !"parameter" %in% names(df)) return(df)
  retired_params <- c("BetaScale", "Terminal depletion", "Terminal SSB")
  df[!as.character(df$parameter) %in% retired_params, , drop = FALSE]
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
  profile_names <- stp_default_key_params()
  profile_names <- profile_names[!grepl("^Recent ", profile_names)]
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
      rel_diff = (refit_value - truth_value) / abs(truth_value)
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
  x %>%
    transmute(
      parameter = as.character(.data$parameter),
      index = suppressWarnings(as.integer(.data$index)),
      truth_value = suppressWarnings(as.numeric(.data$truth_value)),
      refit_value = suppressWarnings(as.numeric(.data$refit_value)),
      scenario = scenario,
      model = model,
      replicate = replicate
    ) %>%
    mutate(rel_diff = (.data$refit_value - .data$truth_value) / abs(.data$truth_value)) %>%
    filter(is.finite(rel_diff), is.finite(truth_value), abs(truth_value) > 1e-8)
}

stp_read_indepvar_parameter_recovery <- function(recovery_dir, scenario, replicate, model = stp_model_from_scenario(scenario)) {
  path <- file.path(recovery_dir, "parameter_recovery.csv")
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(x) || nrow(x) == 0) return(NULL)
  needed <- c("name", "index", "truth", "estimate", "rel_delta")
  if (!all(needed %in% names(x))) return(NULL)
  out <- x %>%
    transmute(
      parameter = as.character(.data$name),
      index = suppressWarnings(as.integer(.data$index)),
      truth_value = suppressWarnings(as.numeric(.data$truth)),
      refit_value = suppressWarnings(as.numeric(.data$estimate)),
      scenario = scenario,
      model = model,
      replicate = replicate,
      rel_diff = suppressWarnings(as.numeric(.data$rel_delta))
    ) %>%
    filter(
      nzchar(.data$parameter),
      !grepl("^[+-]?[0-9.]+$", .data$parameter),
      is.finite(.data$rel_diff),
      is.finite(.data$truth_value),
      abs(.data$truth_value) > 1e-8
    )
  if (nrow(out) == 0) NULL else out
}

stp_filter_parameter_group <- function(df, group = "core") {
  if (is.null(df) || nrow(df) == 0 || !"parameter" %in% names(df)) return(df)
  group <- stp_first_or_null(as.character(group))
  if (is.null(group) || !nzchar(group)) group <- "core"
  parameter <- as.character(df$parameter)
  keep <- switch(
    group,
    movement = grepl("diff_coffs|diff[._ ]?coeff|move|movement|diffusion|mixing|transfer|migrate|migration|coff|coeff", parameter, ignore.case = TRUE),
    recruitment = grepl("region_rec_diffs|recruit|rec_dev|recdev|region.*rec|rec.*region", parameter, ignore.case = TRUE),
    indepvar = TRUE,
    TRUE
  )
  df[keep, , drop = FALSE]
}

stp_read_recent_derived_recovery <- function(recovery_dir, scenario, replicate, model = stp_model_from_scenario(scenario), n_years = 4L) {
  path <- file.path(recovery_dir, "derived_recovery.csv")
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(x) || nrow(x) == 0 || !"year" %in% names(x)) return(NULL)
  x$year <- suppressWarnings(as.numeric(x$year))
  recent_metrics <- c(
    depletion = "Recent depletion (4yr avg)",
    spawning_potential = "Recent SSB (4yr avg)",
    fishing_mortality = "Recent F (4yr avg)",
    recruitment = "Recent recruitment (4yr avg)"
  )
  make_row <- function(metric, label) {
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
    keep_years <- tail(sort(unique(y$year)), max(1L, as.integer(n_years)))
    y <- y %>% filter(.data$year %in% keep_years)
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
      rel_diff = (refit_value - truth_value) / abs(truth_value),
      stringsAsFactors = FALSE
    )
  }
  rows <- lapply(names(recent_metrics), function(metric) make_row(metric, recent_metrics[[metric]]))
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

stp_trace_limit <- function(x, default = 10L, max_limit = 500L) {
  if (is.null(x) || length(x) == 0) return(as.integer(default))
  out <- suppressWarnings(as.integer(x[[1]]))
  if (!is.finite(out)) out <- default
  max(0L, min(max_limit, out))
}

stp_limit_replicate_traces <- function(df,
                                       group_cols,
                                       scenario_col = "scenario",
                                       limit = 10L) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  limit <- stp_trace_limit(limit)
  if (limit <= 0L || !scenario_col %in% names(df)) return(df[0, , drop = FALSE])
  group_cols <- intersect(group_cols, names(df))
  key_cols <- unique(c(group_cols, scenario_col))
  keys <- df %>%
    distinct(across(all_of(key_cols))) %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(.data[[scenario_col]], .by_group = TRUE) %>%
    slice_head(n = limit) %>%
    ungroup()
  semi_join(df, keys, by = key_cols)
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
  line_size <- 0.54
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
        alpha = 0.38
      )
  }
  trace_q <- if (isTRUE(filters$show_replicates) && nrow(refit_q) > 0) {
    stp_limit_replicate_traces(
      refit_q,
      group_cols = c("model", "metric"),
      scenario_col = "scenario",
      limit = if (!is.null(filters$recovery_trace_limit)) filters$recovery_trace_limit else 10L
    )
  } else {
    refit_q[0, , drop = FALSE]
  }
  if (nrow(trace_q) > 0) {
    p <- p +
      geom_line(
        data = trace_q,
        aes(x = .data$year, y = .data$value, group = .data$scenario, color = .data$model),
        alpha = 0.18,
        linewidth = 0.24
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
    if (isTRUE(facet_model)) {
      p <- p +
        geom_line(
          data = truth_q,
          aes(x = .data$year, y = .data$median, group = .data$model),
          color = "#d62728",
          linewidth = 0.52
        )
    } else {
      p <- p +
        geom_line(
          data = truth_q,
          aes(x = .data$year, y = .data$median, group = .data$model, color = .data$model),
          linewidth = 0.52,
          linetype = "longdash",
          alpha = 0.95
        )
    }
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
    facet_cols <- suppressWarnings(as.integer(if (!is.null(filters$recovery_facet_cols)) filters$recovery_facet_cols[[1]] else 2L))
    if (!is.finite(facet_cols) || facet_cols < 1L) facet_cols <- 2L
    p <- p +
      facet_wrap(~ model, ncol = facet_cols) +
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
  info <- tryCatch(readRDS(file.path(input_dir, "selftest_input_info.rds")), error = function(e) NULL)
  min_year <- suppressWarnings(as.integer(tryCatch(info$base_minyear, error = function(e) NA_integer_)))
  max_year <- suppressWarnings(as.integer(tryCatch(info$base_maxyear, error = function(e) NA_integer_)))
  if ((!is.finite(min_year) || !is.finite(max_year)) && all(c("component", "year") %in% names(out))) {
    yr <- suppressWarnings(as.integer(out$year))
    non_tag <- !grepl("^tag recaptures", as.character(out$component))
    inferred <- yr[non_tag & is.finite(yr)]
    if (length(inferred) > 0) {
      if (!is.finite(min_year)) min_year <- min(inferred, na.rm = TRUE)
      if (!is.finite(max_year)) max_year <- max(inferred, na.rm = TRUE)
    }
  }
  if ("year" %in% names(out) && (is.finite(min_year) || is.finite(max_year))) {
    year <- suppressWarnings(as.integer(out$year))
    keep <- is.finite(year)
    if (is.finite(min_year)) keep <- keep & year >= min_year
    if (is.finite(max_year)) keep <- keep & year <= max_year
    out <- out[keep, , drop = FALSE]
    if (nrow(out) == 0) return(NULL)
  }
  out$scenario <- scenario
  out$model <- model
  out$replicate <- stp_rep_id(scenario)
  out
}

stp_series_fishery <- function(series) {
  out <- suppressWarnings(as.integer(sub("^fishery_", "", as.character(series))))
  ifelse(is.finite(out), out, NA_integer_)
}

stp_series_region <- function(series) {
  out <- suppressWarnings(as.integer(sub("^region_", "", as.character(series))))
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

  selftest_all_scenarios <- reactive({
    idx <- selftest_index_current()
    if (nrow(idx) == 0) character() else as.character(idx$key)
  })

  selftest_filters_current <- reactive({
    list(
      scenarios = selftest_all_scenarios(),
      models = input$selftest_recovery_models,
      show_replicates = isTRUE(input$selftest_show_replicates),
      recovery_trace_limit = stp_trace_limit(input$selftest_recovery_trace_limit),
      show_interval = isTRUE(input$selftest_show_interval),
      recovery_layout = if (is.null(input$selftest_recovery_layout)) "overlay" else as.character(input$selftest_recovery_layout),
      recovery_facet_cols = {
        cols <- suppressWarnings(as.integer(input$selftest_recovery_facet_cols))
        if (length(cols) == 0 || !is.finite(cols[1])) 2L else max(1L, min(8L, cols[1]))
      },
      interval_level = {
        lvl <- suppressWarnings(as.numeric(input$selftest_interval_level))
        if (length(lvl) == 0 || !is.finite(lvl[1])) 0.95 else max(0.01, min(0.99, lvl[1]))
      },
      recovery_width = if (is.null(input$selftest_recovery_width)) 1000 else suppressWarnings(as.integer(input$selftest_recovery_width)),
      recovery_height = if (is.null(input$selftest_recovery_height)) 900 else suppressWarnings(as.integer(input$selftest_recovery_height))
    )
  })
  selftest_sim_filters_current <- reactive({
    list(
      scenarios = selftest_all_scenarios(),
      models = input$selftest_sim_models,
      sim_layout = if (is.null(input$selftest_sim_layout)) "facet" else as.character(input$selftest_sim_layout),
      sim_show_traces = isTRUE(input$selftest_sim_show_traces),
      sim_trace_limit = stp_trace_limit(input$selftest_sim_trace_limit),
      sim_models = {
        x <- as.character(input$selftest_sim_models)
        x[nzchar(x)]
      },
      sim_object = if (is.null(input$selftest_sim_object)) "cpue" else as.character(input$selftest_sim_object),
      interval_level = {
        lvl <- suppressWarnings(as.numeric(input$selftest_sim_interval_level))
        if (length(lvl) == 0 || !is.finite(lvl[1])) 0.95 else max(0.01, min(0.99, lvl[1]))
      },
      sim_height = if (is.null(input$selftest_sim_height)) 850 else suppressWarnings(as.integer(input$selftest_sim_height)),
      sim_components = stp_sim_components_for_object(input$selftest_sim_object)
    )
  })
  selftest_param_filters_current <- reactive({
    list(
      scenarios = selftest_all_scenarios(),
      models = input$selftest_param_models,
      param_group = if (is.null(input$selftest_param_group)) "core" else as.character(input$selftest_param_group),
      param_metrics = if (is.null(input$selftest_param_metrics) || length(input$selftest_param_metrics) == 0) {
        stp_default_key_params()
      } else {
        as.character(input$selftest_param_metrics)
      }
    )
  })
  selftest_filters_applied <- reactiveVal(NULL)
  selftest_sim_filters_applied <- reactiveVal(NULL)
  selftest_param_filters_applied <- reactiveVal(NULL)

  observeEvent(rv$data_loaded, {
    req(rv$data_loaded)
    idx <- selftest_index_current()
    selected <- if (nrow(idx) > 0) as.character(idx$key) else character()
    model_choices <- if (nrow(idx) > 0) sort(unique(idx$model)) else character()
    model_choices <- stats::setNames(model_choices, model_choices)
    updatePickerInput(session, "selftest_recovery_models", choices = model_choices, selected = unname(model_choices))
    updatePickerInput(session, "selftest_param_models", choices = model_choices, selected = unname(model_choices))
    updatePickerInput(session, "selftest_sim_models", choices = model_choices, selected = unname(model_choices))
    if (length(selected) > 0) {
      current <- isolate(selftest_filters_current())
      sim_current <- isolate(selftest_sim_filters_current())
      param_current <- isolate(selftest_param_filters_current())
      current$scenarios <- selected
      current$models <- unname(model_choices)
      sim_current$scenarios <- selected
      sim_current$models <- unname(model_choices)
      sim_current$sim_models <- unname(model_choices)
      param_current$scenarios <- selected
      param_current$models <- unname(model_choices)
      selftest_filters_applied(current)
      selftest_sim_filters_applied(sim_current)
      selftest_param_filters_applied(param_current)
    }
  }, ignoreInit = FALSE)

  observe({
    req(rv$data_loaded)
    idx <- selftest_index_current()
    if (nrow(idx) == 0) {
      updatePickerInput(session, "selftest_sim_models", choices = character(), selected = character())
      return()
    }
    model_choices <- sort(unique(idx$model))
    model_choices <- stats::setNames(model_choices, model_choices)
    current <- isolate(as.character(input$selftest_sim_models))
    selected <- intersect(current, unname(model_choices))
    if (length(selected) == 0) selected <- unname(model_choices)
    updatePickerInput(session, "selftest_sim_models", choices = model_choices, selected = selected)
  })

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(selftest_filters_current(), selftest_filters_applied())
    set_apply_pending(session, "selftest_apply_filters", pending)
  })

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(selftest_sim_filters_current(), selftest_sim_filters_applied())
    set_apply_pending(session, "selftest_sim_apply_filters", pending)
  })

  observe({
    req(rv$data_loaded)
    pending <- !isTRUE(input$live_update_plots) &&
      !filters_equal(selftest_param_filters_current(), selftest_param_filters_applied())
    set_apply_pending(session, "selftest_param_apply_filters", pending)
  })

  observeEvent(input$selftest_apply_filters, {
    selftest_filters_applied(isolate(selftest_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(input$selftest_sim_apply_filters, {
    selftest_sim_filters_applied(isolate(selftest_sim_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(input$selftest_param_apply_filters, {
    selftest_param_filters_applied(isolate(selftest_param_filters_current()))
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$live_update_plots, input$selftest_recovery_models,
         input$selftest_recovery_layout,
         input$selftest_recovery_facet_cols,
         input$selftest_show_replicates, input$selftest_recovery_trace_limit,
         input$selftest_show_interval,
         input$selftest_interval_level,
         input$selftest_recovery_width,
         input$selftest_recovery_height),
    {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      selftest_filters_applied(isolate(selftest_filters_current()))
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(input$live_update_plots, input$selftest_sim_models,
         input$selftest_sim_layout, input$selftest_sim_interval_level,
         input$selftest_sim_show_traces, input$selftest_sim_trace_limit,
         input$selftest_sim_height, input$selftest_sim_object, input$selftest_sim_models),
    {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      selftest_sim_filters_applied(isolate(selftest_sim_filters_current()))
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(input$live_update_plots, input$selftest_param_models,
         input$selftest_param_group, input$selftest_param_metrics),
    {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      selftest_param_filters_applied(isolate(selftest_param_filters_current()))
    },
    ignoreInit = TRUE
  )

  observeEvent(rv$initial_render_nonce, {
    req(rv$data_loaded, rv$initial_render_nonce)
    selftest_filters_applied(isolate(selftest_filters_current()))
    selftest_sim_filters_applied(isolate(selftest_sim_filters_current()))
    selftest_param_filters_applied(isolate(selftest_param_filters_current()))
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
    filters <- selftest_param_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    if (nrow(idx) == 0) return(data.frame())
    bind_rows(lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      truth_par <- stp_read_selftest_truth_par(row$truth_dir[[1]], row$model[[1]], input$model_dir)
      param_group <- if (is.null(filters$param_group)) "core" else as.character(filters$param_group)
      if (!identical(param_group, "core")) {
        indepvar_rows <- stp_read_indepvar_parameter_recovery(
          row$recovery_dir[[1]],
          row$key[[1]],
          row$replicate[[1]],
          row$model[[1]]
        )
        return(stp_filter_parameter_group(indepvar_rows, param_group))
      }

      from_recovery <- stp_read_profile_parameter_recovery(
        row$recovery_dir[[1]],
        row$key[[1]],
        row$replicate[[1]],
        row$model[[1]]
      )
      from_par <- stp_par_relative_diff(
        truth_par,
        stp_read_model_par(row$refit_dir[[1]]),
        row$key[[1]],
        row$replicate[[1]]
      )
      param_rows <- if (!is.null(from_recovery) && nrow(from_recovery) > 0 && nrow(from_par) > 0) {
        bind_rows(
          from_recovery,
          anti_join(
            from_par,
            from_recovery,
            by = c("parameter", "index", "scenario", "model", "replicate")
          )
        )
      } else if (!is.null(from_recovery) && nrow(from_recovery) > 0) {
        from_recovery
      } else {
        from_par
      }
      param_rows <- stp_filter_estimated_profile_parameters(param_rows, truth_par, row$model[[1]])
      derived_rows <- stp_read_recent_derived_recovery(
        row$recovery_dir[[1]],
        row$key[[1]],
        row$replicate[[1]],
        row$model[[1]]
      )
      bind_rows(param_rows, derived_rows)
    }))
  })

  observeEvent(selftest_parameter_diff_data(), {
    df <- selftest_parameter_diff_data()
    param_group <- isolate(if (is.null(input$selftest_param_group)) "core" else as.character(input$selftest_param_group))
    available <- if (nrow(df) > 0 && "parameter" %in% names(df)) {
      unique(as.character(df$parameter))
    } else {
      character()
    }
    if (identical(param_group, "core")) {
      available <- if (length(available) > 0) stp_key_param_order(available) else stp_default_key_params()
      choices <- stp_key_param_choices(available)
    } else {
      available <- sort(available[nzchar(available)])
      choices <- stats::setNames(available, available)
    }
    current <- isolate(as.character(input$selftest_param_metrics))
    selected <- intersect(current, unname(choices))
    if (length(selected) == 0 && identical(param_group, "core")) selected <- intersect(stp_default_key_params(), unname(choices))
    if (length(selected) == 0 && length(choices) > 0) selected <- head(unname(choices), min(12L, length(choices)))
    updatePickerInput(session, "selftest_param_metrics", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  selftest_sim_data <- reactive({
    filters <- selftest_sim_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    sim_models <- as.character(filters$sim_models)
    sim_models <- sim_models[nzchar(sim_models)]
    if (length(sim_models) > 0 && nrow(idx) > 0) idx <- idx[idx$model %in% sim_models, , drop = FALSE]
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      info <- stp_read_selftest_input_info(input$model_dir, row$key[[1]], row$input_dir[[1]])
      if (is.null(info)) return(NULL)
      stp_input_info_row(info, row$key[[1]], row$model[[1]])
    })
    bind_rows(rows)
  })

  selftest_data_simulation_series <- reactive({
    filters <- selftest_sim_filters_applied()
    req(rv$data_loaded, filters)
    idx <- selftest_index_current()
    idx <- stp_filter_selftest_index(idx, filters)
    sim_models <- as.character(filters$sim_models)
    sim_models <- sim_models[nzchar(sim_models)]
    if (length(sim_models) > 0 && nrow(idx) > 0) idx <- idx[idx$model %in% sim_models, , drop = FALSE]
    rows <- lapply(seq_len(nrow(idx)), function(i) {
      row <- idx[i, , drop = FALSE]
      stp_read_data_simulation_summary(row$input_dir[[1]], row$key[[1]], row$model[[1]])
    })
    bind_rows(rows)
  })

  selftest_convergence_data <- reactive({
    filters <- selftest_sim_filters_applied()
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
    trace_limit <- if (!is.null(filters$recovery_trace_limit)) stp_trace_limit(filters$recovery_trace_limit) else 10L
    subtitle <- paste0(
      if (isTRUE(facet_model)) "Facets = model; " else "Colour = model; ",
      if (isTRUE(facet_model)) "coloured solid = refit median; red solid = truth" else "solid = refit median; long-dash = truth",
      if (isTRUE(filters$show_replicates)) paste0("; faint lines = up to ", trace_limit, " refit replicate traces per panel") else "",
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
    filters <- selftest_sim_filters_applied()
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
          region_num = stp_series_region(series),
          base_value = suppressWarnings(as.numeric(base_value)),
          pseudo_value = suppressWarnings(as.numeric(pseudo_value))
        ) %>%
        filter(
          component %in% sim_components,
          is.finite(year), is.finite(base_value), is.finite(pseudo_value)
        )
      if (nrow(annual) > 0 && "scenario" %in% names(info)) {
        effort_info <- info %>%
          distinct(.data$scenario, .data$effort_conditioned)
        annual <- annual %>%
          left_join(effort_info, by = "scenario") %>%
          filter(
            .data$component != "effort" |
              (!is.na(.data$effort_conditioned) & .data$effort_conditioned)
          )
      }
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
        region_series <- stp_series_region(row$series[[1]])
        if (is.finite(region_series)) {
          component_label <- stp_region_component_label(row$component[[1]])
          row$display_component <- paste0(component_label, " - ", region_series)
          row$aggregation <- if (row$component[[1]] %in% c("catch total", "tag recaptures by fishery")) "sum" else "mean"
          return(row)
        }
        if (identical(row$component[[1]], "tag recaptures by fishery")) {
          fishery <- stp_series_fishery(row$series[[1]])
          map <- tryCatch(rv$FISHERY_MAPS[[row$model[[1]]]], error = function(e) NULL)
          region <- stp_fishery_region(fishery, map)
          row$display_component <- paste0("tag recaptures by region - ", region)
          row$aggregation <- "sum"
          return(row)
        }
        fishery_components <- c(
          "catch total", "CPUE", "effort",
          "length mean", "length q10", "length median", "length q90",
          "weight mean", "weight q10", "weight median", "weight q90"
        )
        if (!row$component[[1]] %in% fishery_components) {
          row$display_component <- row$component[[1]]
          row$aggregation <- "mean"
          return(row)
        }
        fishery <- stp_series_fishery(row$series[[1]])
        map <- tryCatch(rv$FISHERY_MAPS[[row$model[[1]]]], error = function(e) NULL)
        region <- stp_fishery_region(fishery, map)
        component_label <- stp_region_component_label(row$component[[1]])
        row$display_component <- paste0(component_label, " - ", region)
        row$aggregation <- if (identical(row$component[[1]], "catch total")) "sum" else "mean"
        row
      }))
      annual <- annual %>%
        group_by(model, scenario, replicate, display_component, year) %>%
        summarise(
          aggregation = dplyr::first(.data$aggregation),
          base_value = if (identical(dplyr::first(.data$aggregation), "sum")) sum(base_value, na.rm = TRUE) else mean(base_value, na.rm = TRUE),
          pseudo_value = if (identical(dplyr::first(.data$aggregation), "sum")) sum(pseudo_value, na.rm = TRUE) else mean(pseudo_value, na.rm = TRUE),
          .groups = "drop"
        )
      interval_level <- if (!is.null(filters$interval_level)) filters$interval_level else 0.95
      lower_prob <- (1 - interval_level) / 2
      upper_prob <- 1 - lower_prob
      facet_model <- identical(filters$sim_layout, "facet") && length(unique(annual$model)) > 1L
      model_palette <- stp_model_palette(annual$model)
      annual <- annual %>%
        mutate(display_panel = .data$display_component)
      pseudo_band <- annual %>%
        group_by(model, display_component, display_panel, year) %>%
        summarise(
          median = stats::median(pseudo_value, na.rm = TRUE),
          lower = stats::quantile(pseudo_value, lower_prob, na.rm = TRUE, names = FALSE),
          upper = stats::quantile(pseudo_value, upper_prob, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
      base_line <- annual %>%
        group_by(model, display_component, display_panel, year) %>%
        summarise(base_value = stats::median(base_value, na.rm = TRUE), .groups = "drop")
      trace_limit <- if (!is.null(filters$sim_trace_limit)) stp_trace_limit(filters$sim_trace_limit) else 10L
      annual_trace <- if (isTRUE(filters$sim_show_traces)) {
        stp_limit_replicate_traces(
          annual,
          group_cols = c("model", "display_component", "display_panel"),
          scenario_col = "scenario",
          limit = trace_limit
        )
      } else {
        annual[0, , drop = FALSE]
      }
      p_data <- ggplot()
      if (isTRUE(facet_model)) {
        p_data <- p_data +
          geom_ribbon(
            data = pseudo_band,
            aes(x = year, ymin = lower, ymax = upper, group = interaction(model, display_component)),
            fill = "#7f8c8d",
            alpha = 0.28
          )
        if (nrow(annual_trace) > 0) {
          p_data <- p_data +
          geom_line(
            data = annual_trace,
            aes(x = year, y = pseudo_value, group = interaction(model, scenario)),
            color = "#59636e",
            alpha = 0.32,
            linewidth = 0.26
          )
        }
        p_data <- p_data +
          geom_line(
            data = pseudo_band,
            aes(x = year, y = median, group = interaction(model, display_component)),
            color = "#111111",
            linewidth = 0.52
          ) +
          geom_line(
            data = base_line,
            aes(x = year, y = base_value, group = interaction(model, display_component)),
            color = "#d62728",
            alpha = 0.88,
            linewidth = 0.48
          )
      } else {
        p_data <- p_data +
          geom_ribbon(
            data = pseudo_band,
            aes(x = year, ymin = lower, ymax = upper, group = interaction(model, display_component), fill = model),
            alpha = 0.24
          )
        if (nrow(annual_trace) > 0) {
          p_data <- p_data +
          geom_line(
            data = annual_trace,
            aes(x = year, y = pseudo_value, group = interaction(model, scenario), color = model),
            alpha = 0.14,
            linewidth = 0.22
          )
        }
        p_data <- p_data +
          geom_line(
            data = pseudo_band,
            aes(x = year, y = median, group = interaction(model, display_component), color = model),
            linewidth = 0.52
          ) +
          geom_line(
            data = base_line,
            aes(x = year, y = base_value, group = interaction(model, display_component), color = model),
            alpha = 0.88,
            linewidth = 0.42,
            linetype = "longdash"
          ) +
          scale_color_manual(name = "Model", values = model_palette, breaks = names(model_palette)) +
          scale_fill_manual(name = "Model", values = model_palette, breaks = names(model_palette))
      }
      p_data <- p_data +
        labs(
          x = "Year",
          y = stp_sim_y_label(filters$sim_object),
          subtitle = paste0(
            if (isTRUE(facet_model)) {
              paste0(
                "Data simulation check: red = fitted expectation, black = pseudo median",
                if (nrow(annual_trace) > 0) paste0(", grey = up to ", trace_limit, " pseudo reps per panel") else "",
                "; facet rows = series/region, columns = model"
              )
            } else {
              paste0(
                "Data simulation check: colour = model; solid = pseudo median; dashed = fitted expectation",
                if (nrow(annual_trace) > 0) paste0("; faint lines = up to ", trace_limit, " pseudo reps per panel") else ""
              )
            },
            "; shaded = ", round(interval_level * 100), "% interval"
          )
        ) +
        theme_bw(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#eef3f7", color = NA),
          legend.position = if (isTRUE(facet_model)) "none" else "right"
        )
      if (isTRUE(facet_model)) {
        p_data <- p_data + facet_grid(display_component ~ model, scales = "free_y")
      } else {
        p_data <- p_data + facet_wrap(~ display_panel, scales = "free_y", ncol = 2)
      }
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
    filters <- selftest_param_filters_applied()
    req(filters)
    df <- selftest_parameter_diff_data()
    empty_message <- function() {
      group <- if (is.null(filters$param_group)) "core" else as.character(filters$param_group)
      if (identical(group, "movement")) {
        "No movement coefficient recovery found. Re-run or retrieve self-test outputs with named indepvar.rpt recovery retained."
      } else if (identical(group, "recruitment")) {
        "No recruitment-deviation recovery found. Re-run or retrieve self-test outputs with named indepvar.rpt recovery retained."
      } else if (identical(group, "indepvar")) {
        "No named indepvar recovery found. Re-run or retrieve self-test outputs with named indepvar.rpt recovery retained."
      } else {
        "No key parameter comparison found"
      }
    }
    if (nrow(df) == 0) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = empty_message(), size = 4.2, color = "#777") +
        theme_void())
    }
    selected_params <- if (is.null(filters$param_metrics) || length(filters$param_metrics) == 0) {
      unique(as.character(df$parameter))
    } else {
      as.character(filters$param_metrics)
    }
    parameter_levels <- if (identical(filters$param_group, "core")) {
      stp_key_param_order(c(selected_params, unique(as.character(df$parameter))))
    } else {
      unique(c(selected_params, sort(unique(as.character(df$parameter)))))
    }
    df <- df %>%
      mutate(
        model = if ("model" %in% names(.)) as.character(.data$model) else stp_model_from_scenario(.data$scenario),
        parameter = factor(parameter, levels = parameter_levels),
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
        annotate("text", x = 0.5, y = 0.5, label = empty_message(), size = 4.2, color = "#777") +
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
      labs(x = NULL, y = "Signed relative difference (%)", subtitle = "Refit vs truth for selected quantities; signed % uses abs(truth) as the denominator") +
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
    w <- if (is.null(filters) || is.null(filters$recovery_width)) 1000 else filters$recovery_width
    div(
      style = "overflow-x:auto;",
      plotOutput("selftest_recovery_plot", height = paste0(h, "px"), width = paste0(w, "px"))
    )
  })

  output$selftest_sim_plot_ui <- renderUI({
    filters <- selftest_sim_filters_applied()
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
