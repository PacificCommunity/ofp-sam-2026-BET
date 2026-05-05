# Sensitivity catalog used by the Shiny launcher and input-recipe builder.
#
# To add a new launcher-visible sensitivity, add one input_sensitivity_row()
# below. If the sensitivity uses a new kind of recipe option, also add the
# corresponding step wiring in recipe_registry.R and update input_recipe_plan()
# in the launcher to pass that option through.

input_sensitivity_row <- function(id,
                                  token,
                                  suffix,
                                  input_suffix,
                                  factor,
                                  label,
                                  nested = FALSE,
                                  movement_pairs = "",
                                  sel_nodes = "",
                                  index_cv_half = "0") {
  data.frame(
    id = id,
    token = token,
    suffix = suffix,
    input_suffix = input_suffix,
    factor = factor,
    nested = isTRUE(nested),
    label = label,
    movement_pairs = movement_pairs,
    sel_nodes = sel_nodes,
    index_cv_half = index_cv_half,
    stringsAsFactors = FALSE
  )
}

validate_input_sensitivity_catalog <- function(x) {
  required <- c(
    "id", "token", "suffix", "input_suffix", "factor", "nested", "label",
    "movement_pairs", "sel_nodes", "index_cv_half"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Sensitivity catalog is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (any(!nzchar(x$id))) stop("Sensitivity catalog has empty id value(s).")
  if (any(duplicated(x$id))) {
    stop("Sensitivity catalog has duplicate id value(s): ", paste(unique(x$id[duplicated(x$id)]), collapse = ", "))
  }
  if (any(!nzchar(x$token))) stop("Sensitivity catalog has empty token value(s).")
  if (any(duplicated(x$token))) {
    stop("Sensitivity catalog has duplicate token value(s): ", paste(unique(x$token[duplicated(x$token)]), collapse = ", "))
  }
  if (any(!nzchar(x$factor))) stop("Sensitivity catalog has empty factor value(s).")
  if (any(!startsWith(x$suffix, "_"))) stop("Every compact suffix should start with '_'.")
  if (any(!startsWith(x$input_suffix, "_"))) stop("Every input suffix should start with '_'.")
  x$nested <- as.logical(x$nested)
  x
}

input_sensitivity_catalog <- function() {
  validate_input_sensitivity_catalog(do.call(rbind, list(
    input_sensitivity_row(
      id = "sel4",
      token = "sel4",
      suffix = "_sel4",
      input_suffix = "_sel_spline4",
      factor = "selectivity",
      label = "Selectivity spline = 4",
      sel_nodes = "4"
    ),
    input_sensitivity_row(
      id = "cvH",
      token = "cvH",
      suffix = "_cvH",
      input_suffix = "_index_cv_half",
      factor = "index_cv",
      label = "Index CV half",
      index_cv_half = "1"
    ),
    input_sensitivity_row(
      id = "move_R2_R3",
      token = "m23",
      suffix = "_m23",
      input_suffix = "_movement_R2_R3",
      factor = "movement",
      label = "Tag movement R2-R3",
      nested = TRUE,
      movement_pairs = "2-3"
    ),
    input_sensitivity_row(
      id = "move_all",
      token = "m123",
      suffix = "_m123",
      input_suffix = "_movement_R1_R2_R1_R3_R2_R3",
      factor = "movement",
      label = "Tag movement all R1/R2/R3 pairs",
      nested = TRUE,
      movement_pairs = "1-2,1-3,2-3"
    )
  )))
}

input_sensitivity_choices <- function() {
  x <- input_sensitivity_catalog()
  stats::setNames(x$id, paste0(x$label, "  [", x$suffix, "]"))
}

input_sensitivity_by_id <- function(ids) {
  x <- input_sensitivity_catalog()
  ids <- unique(as.character(ids))
  x[x$id %in% ids, , drop = FALSE]
}

input_sensitivity_recipe_options <- function(ids) {
  rows <- input_sensitivity_by_id(ids)
  list(
    movement_pairs = paste(rows$movement_pairs[nzchar(rows$movement_pairs)], collapse = ","),
    sel_nodes = {
      vals <- rows$sel_nodes[nzchar(rows$sel_nodes)]
      if (length(vals) > 0) vals[[1]] else ""
    },
    index_cv_half = nrow(rows) > 0 && any(tolower(trimws(rows$index_cv_half)) %in% c("1", "true", "yes", "y", "on")),
    suffix_parts = rows$suffix,
    tokens = unique(rows$token),
    labels = rows$label
  )
}

input_sensitivity_token <- function(id) {
  hit <- input_sensitivity_by_id(id)
  if (nrow(hit) == 0) "" else hit$token[[1]]
}

input_sensitivity_has_nested_levels <- function(ids) {
  rows <- input_sensitivity_by_id(ids)
  nrow(rows) > 0 && any(rows$nested)
}

compact_input_name <- function(name) {
  x <- basename(as.character(name[[1]]))
  replacements <- input_sensitivity_catalog()[, c("input_suffix", "suffix"), drop = FALSE]
  replacements <- replacements[order(nchar(replacements$input_suffix), decreasing = TRUE), , drop = FALSE]
  for (idx in seq_len(nrow(replacements))) {
    x <- gsub(replacements$input_suffix[[idx]], replacements$suffix[[idx]], x, fixed = TRUE)
  }
  x <- gsub("__+", "_", x)
  gsub("^_+|_+$", "", x)
}
