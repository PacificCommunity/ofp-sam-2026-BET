# Select CPUE fisheries from map first; fall back to model outputs when needed.
pm_select_cpue_fisheries <- function(rep_list, fishery_map) {
  cpue_fisheries <- fishery_map %>%
    dplyr::filter(group == "Index") %>%
    dplyr::pull(fishery)
  
  if (length(cpue_fisheries) == 0) {
    cpue_fisheries <- unique(unlist(lapply(rep_list, function(x) {
      tryCatch(as.numeric(unique(cpue_obs(x)$unit)), error = function(e) numeric(0))
    })))
  }
  
  cpue_fisheries
}

# Build fishery ID -> display name mapping for plotting/faceting labels.
pm_build_fishery_mapping <- function(fishery_map, fisheries) {
  fishery_map %>%
    dplyr::filter(fishery %in% fisheries) %>%
    with(setNames(fishery_name, fishery))
}
