# Provide default mixing-period settings keyed by model name.
pm_default_mixing_periods <- function(models, default_steps = 2) {
  setNames(rep(default_steps, length(models)), models)
}

# Build standardized release table from MFCL tag objects.
pm_get_tag_releases <- function(tag_out_list) {
  release_list <- lapply(names(tag_out_list), function(model_name) {
    tag_obj <- tag_out_list[[model_name]]
    rel_data <- releases(tag_obj)
    rel_data %>%
      dplyr::group_by(rel.group, region, year, month, program) %>%
      dplyr::summarise(rel.obs = sum(lendist, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Model = model_name)
  })
  
  dplyr::bind_rows(release_list) %>%
    dplyr::mutate(rel.ts = year + (month - 1) / 12 + 1 / 24) %>%
    dplyr::rename(rel.year = year, rel.month = month, rel.region = region)
}

# Apply model-aware mixing-period filter in continuous time.
pm_apply_mixing_filter <- function(tag_temp_out_list, mixing_periods, seasons_per_model) {
  dplyr::bind_rows(tag_temp_out_list, .id = "Model") %>%
    dplyr::mutate(recap_ts = recap.year + (recap.month - 1.5) / 12) %>%
    dplyr::group_by(Model, rel.group) %>%
    dplyr::mutate(first_recap_ts = min(recap_ts[recap.obs > 0 | recap.pred > 0], na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(data.frame(Model = names(mixing_periods), mixing_period = mixing_periods), by = "Model") %>%
    dplyr::mutate(seasons = seasons_per_model[Model]) %>%
    dplyr::mutate(min_allowed_ts = first_recap_ts + (mixing_period / seasons)) %>%
    dplyr::filter(recap_ts >= min_allowed_ts | is.infinite(min_allowed_ts))
}

# Join recapture data to release timing and compute period at liberty.
pm_add_period_at_liberty <- function(tag_data, tag_releases, seasons_per_model) {
  tag_data %>%
    dplyr::left_join(tag_releases, by = c("Model", "rel.group")) %>%
    dplyr::mutate(seasons = seasons_per_model[Model]) %>%
    dplyr::mutate(
      recap.ts = recap.year + (recap.month - 1) / 12 + 1 / 24,
      period_at_liberty = round((recap.ts - rel.ts) * seasons)
    )
}

# Pad recap time-series per model using each model's native seasonal step.
pm_pad_model_time <- function(data, models, seasons_per_model, time_col = "recap_ts", value_cols = c("recap_obs", "recap_pred")) {
  min_ts <- min(data[[time_col]], na.rm = TRUE)
  max_ts <- max(data[[time_col]], na.rm = TRUE)
  
  grid <- pm_build_model_time_grid(
    models = models,
    min_ts = min_ts,
    max_ts = max_ts,
    seasons_lookup = seasons_per_model
  )
  
  out <- data %>%
    dplyr::right_join(grid, by = c("Model", time_col))
  
  for (v in value_cols) {
    out[[v]] <- tidyr::replace_na(out[[v]], 0)
  }
  
  out %>%
    dplyr::mutate(Model = factor(Model, levels = models))
}

# Pad discrete period-at-liberty series for all model/group combinations.
pm_pad_period_series <- function(data, models, group_cols = character(0), period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")) {
  min_period <- min(data[[period_col]], na.rm = TRUE)
  max_period <- max(data[[period_col]], na.rm = TRUE)
  all_periods <- seq(min_period, max_period, by = 1)
  
  if (length(group_cols) == 0) {
    grid <- expand.grid(Model = models, period_at_liberty = all_periods, stringsAsFactors = FALSE)
  } else {
    base_grid <- expand.grid(Model = models, period_at_liberty = all_periods, stringsAsFactors = FALSE)
    group_values <- data %>%
      dplyr::select(dplyr::all_of(group_cols)) %>%
      dplyr::distinct()
    
    grid <- tidyr::crossing(base_grid, group_values)
  }
  
  out <- data %>%
    dplyr::right_join(grid, by = c("Model", period_col, group_cols))
  
  for (v in value_cols) {
    out[[v]] <- tidyr::replace_na(out[[v]], 0)
  }
  
  out %>%
    dplyr::mutate(Model = factor(Model, levels = models))
}

# Build tag recapture group map from fishery_map with optional index fisheries.
pm_build_tag_recapture_map <- function(fishery_map, include_index = FALSE) {
  if (include_index) {
    fishery_map %>%
      dplyr::select(fishery, tag_recapture_group, tag_recapture_name) %>%
      dplyr::distinct()
  } else {
    fishery_map %>%
      dplyr::filter(group != "Index") %>%
      dplyr::select(fishery, tag_recapture_group, tag_recapture_name) %>%
      dplyr::distinct()
  }
}
