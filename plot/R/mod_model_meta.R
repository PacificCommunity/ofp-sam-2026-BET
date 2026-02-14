# Extract model metadata safely from MFCL par object.
pm_extract_model_meta <- function(par_obj, model_name = NA_character_) {
  dims <- tryCatch(as.list(par_obj@dimensions), error = function(e) list())
  yr_range <- tryCatch(as.list(par_obj@range), error = function(e) list())
  
  tibble::tibble(
    Model = model_name,
    minyear = suppressWarnings(as.numeric(pm_null_coalesce(yr_range$minyear, NA_real_))),
    maxyear = suppressWarnings(as.numeric(pm_null_coalesce(yr_range$maxyear, NA_real_))),
    seasons = suppressWarnings(as.numeric(pm_null_coalesce(dims$seasons, NA_real_))),
    regions = suppressWarnings(as.numeric(pm_null_coalesce(dims$regions, NA_real_))),
    fisheries = suppressWarnings(as.numeric(pm_null_coalesce(dims$fisheries, NA_real_))),
    taggrps = suppressWarnings(as.numeric(pm_null_coalesce(dims$taggrps, NA_real_)))
  )
}

# Compute a common year window across models.
# intersection: safest for overlay; union: full span across all models.
pm_get_model_year_window <- function(par_list, mode = c("intersection", "union")) {
  mode <- match.arg(mode)
  if (length(par_list) == 0) {
    return(list(minYear = NA_real_, maxYear = NA_real_))
  }
  
  meta <- purrr::imap_dfr(par_list, pm_extract_model_meta)
  if (nrow(meta) == 0 || !all(c("minyear", "maxyear") %in% colnames(meta))) {
    return(list(minYear = NA_real_, maxYear = NA_real_))
  }
  valid_meta <- dplyr::filter(meta, !is.na(minyear), !is.na(maxyear))
  
  if (nrow(valid_meta) == 0) {
    return(list(minYear = NA_real_, maxYear = NA_real_))
  }
  
  if (mode == "intersection") {
    min_year <- max(valid_meta$minyear, na.rm = TRUE)
    max_year <- min(valid_meta$maxyear, na.rm = TRUE)
    if (min_year > max_year) {
      min_year <- min(valid_meta$minyear, na.rm = TRUE)
      max_year <- max(valid_meta$maxyear, na.rm = TRUE)
    }
  } else {
    min_year <- min(valid_meta$minyear, na.rm = TRUE)
    max_year <- max(valid_meta$maxyear, na.rm = TRUE)
  }
  
  list(minYear = as.integer(min_year), maxYear = as.integer(max_year))
}

# Derive seasons-per-year by model with fallback if metadata is missing.
pm_get_seasons_per_model <- function(par_list, fallback = 4) {
  purrr::imap_int(par_list, function(par_obj, model_name) {
    s <- tryCatch(as.numeric(par_obj@dimensions["seasons"]), error = function(e) NA_real_)
    ifelse(is.na(s) || s <= 0, fallback, as.integer(s))
  })
}

# Build a model-specific time grid so models with different seasons still align.
pm_build_model_time_grid <- function(models, min_ts, max_ts, seasons_lookup) {
  purrr::map_dfr(models, function(m) {
    step <- 1 / pm_null_coalesce(seasons_lookup[[m]], 4)
    tibble::tibble(Model = m, recap_ts = seq(min_ts, max_ts, by = step))
  })
}

# Ensure fishery_map includes all fishery IDs that appear in model outputs.
pm_augment_fishery_map <- function(base_map, rep_list, len_list, wgt_list, tagtemp_list) {
  cpue_units <- unique(unlist(lapply(rep_list, function(x) {
    tryCatch(as.numeric(unique(cpue_obs(x)$unit)), error = function(e) numeric(0))
  })))
  len_units <- unique(unlist(lapply(len_list, function(x) {
    tryCatch(as.numeric(unique(x@lenfits$fishery)), error = function(e) numeric(0))
  })))
  wgt_units <- unique(unlist(lapply(wgt_list, function(x) {
    tryCatch(as.numeric(unique(x@wgtfits$fishery)), error = function(e) numeric(0))
  })))
  tag_units <- unique(unlist(lapply(tagtemp_list, function(x) {
    tryCatch(as.numeric(unique(x$recap.fishery)), error = function(e) numeric(0))
  })))
  
  all_units <- sort(unique(c(cpue_units, len_units, wgt_units, tag_units)))
  missing_units <- setdiff(all_units, base_map$fishery)
  
  if (length(missing_units) == 0) {
    return(base_map)
  }
  
  add_df <- tibble::tibble(
    fishery_name = paste("Fishery", missing_units),
    fishery = missing_units,
    region = NA_real_,
    group = "Unknown",
    tag_recapture_group = missing_units,
    tag_recapture_name = paste("Fishery", missing_units)
  )
  
  dplyr::bind_rows(base_map, add_df) %>%
    dplyr::arrange(fishery)
}
