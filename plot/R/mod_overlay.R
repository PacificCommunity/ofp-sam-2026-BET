# Return model count and model names for UI logic (e.g., Shiny mode selectors).
pm_model_inventory <- function(model_list) {
  list(
    n_models = length(model_list),
    model_names = names(model_list)
  )
}

# Evaluate whether overlay is feasible based on common IDs across models.
# extractor should return an ID vector for one model object (e.g., fisheries).
pm_overlay_capability <- function(model_list, extractor) {
  id_list <- lapply(model_list, function(x) {
    ids <- tryCatch(extractor(x), error = function(e) numeric(0))
    sort(unique(ids))
  })
  
  common_ids <- Reduce(intersect, id_list)
  union_ids <- sort(unique(unlist(id_list)))
  
  list(
    can_overlay = length(model_list) > 1 && length(common_ids) > 0,
    common_ids = common_ids,
    union_ids = union_ids,
    ids_by_model = id_list
  )
}
