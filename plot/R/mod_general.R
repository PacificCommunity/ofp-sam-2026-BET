pm_null_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

pm_build_obs_reference <- function(data, keys, obs_col = "obs", fun = median) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(!!obs_col := fun(.data[[obs_col]], na.rm = TRUE), .groups = "drop")
}

pm_assert_columns <- function(data, required_cols, data_name = "data") {
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "%s is missing required columns: %s",
      data_name,
      paste(missing_cols, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

pm_debug_message <- function(msg, enabled = FALSE) {
  if (isTRUE(enabled)) {
    message(sprintf("[plot-debug] %s", msg))
  }
  invisible(NULL)
}
