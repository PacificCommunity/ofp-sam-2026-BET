retro.tag <- function(tag.obj, max_year, n_mixing_periods = NA_real_) {
  # Calculate the effective maximum year for tag releases
  # Releases must occur BEFORE (terminal_period - mixing_periods)
  effective_max_year <- max_year - (n_mixing_periods / 4)  # assuming quarterly periods
  
  # Keep only releases up to the effective maximum year
  keep_releases <- tag.obj@releases[tag.obj@releases$year <= effective_max_year, ]
  keep_groups <- unique(keep_releases$rel.group)
  
  # Keep only recaptures from valid groups up to and including max_year
  keep_recaps <- tag.obj@recaptures[
    tag.obj@recaptures$rel.group %in% keep_groups & 
      tag.obj@recaptures$recap.year <= max_year,
  ]
  
  # Remap group numbers sequentially
  new_ids <- setNames(seq_along(keep_groups), keep_groups)
  keep_releases$rel.group <- new_ids[as.character(keep_releases$rel.group)]
  keep_recaps$rel.group <- new_ids[as.character(keep_recaps$rel.group)]
  
  # Update object slots
  tag.obj@release_groups <- length(keep_groups)
  tag.obj@releases <- keep_releases
  tag.obj@recaptures <- keep_recaps
  tag.obj@recoveries <- tabulate(keep_recaps$rel.group, length(keep_groups))
  tag.obj@range["maxyear"] <- max_year
  
  return(tag.obj)
}

retro.frq <- function(frq.obj, max_year, retro.tag.obj = NULL) {
  # Filter frequency data up to and including max_year
  filtered_freq <- frq.obj@freq[frq.obj@freq$year <= max_year, ]
  
  # Count unique datasets (year/month/week/fishery combinations)
  n_datasets <- nrow(unique(filtered_freq[, c("year", "month", "week", "fishery")]))
  
  frq.obj@freq <- filtered_freq
  frq.obj@range["maxyear"] <- max_year
  frq.obj@lf_range["Datasets"] <- n_datasets
  
  # Update tag group count if provided
  if (!is.null(retro.tag.obj)) {
    frq.obj@n_tag_groups <- retro.tag.obj@release_groups
  }
  
  return(frq.obj)
}

retro.age <- function(age.obj, max_year) {
  # Step 1: Extract unique groups in original order (matching ESS order)
  groups_original <- age.obj@ALK[!duplicated(age.obj@ALK[, c("year", "month", "fishery", "species")]), 
                                 c("year", "month", "fishery", "species")]
  groups_original$ess_value <- age.obj@ESS
  
  # Step 2: Filter ALK data to include only years up to max_year
  age.obj@ALK <- age.obj@ALK[age.obj@ALK$year <= max_year, ]
  
  # Step 3: Extract unique groups remaining after filtering
  groups_filtered <- age.obj@ALK[!duplicated(age.obj@ALK[, c("year", "month", "fishery", "species")]), 
                                 c("year", "month", "fishery", "species")]
  
  # Step 4: Sort groups as they will be ordered after write() 
  # (write() sorts by year, month, fishery, species)
  groups_sorted <- groups_filtered[order(groups_filtered$year, 
                                         groups_filtered$month,
                                         groups_filtered$fishery, 
                                         groups_filtered$species), ]
  
  # Step 5: Reorder ESS to match the sorted group order
  library(dplyr)
  ess_reordered <- groups_sorted %>%
    left_join(groups_original, by = c("year", "month", "fishery", "species")) %>%
    pull(ess_value)
  
  age.obj@ESS <- ess_reordered
  
  # Update year range
  age.obj@range["maxyear"] <- max_year
  
  cat("Reordered", length(age.obj@ESS), "ESS values to match write() sort order\n")
  
  return(age.obj)
}


retro.ini <- function(ini.obj, tag.obj, max_year, n_mixing_periods = 2) {
  # Execute retro.tag with mixing period consideration
  new_tag <- retro.tag(tag.obj, max_year, n_mixing_periods)
  new_n_taggrps <- new_tag@release_groups
  
  # Identify original group numbers to keep (before remapping in retro.tag)
  effective_max_year <- max_year - (n_mixing_periods / 4)
  keep_releases <- tag.obj@releases[tag.obj@releases$year <= effective_max_year, ]
  keep_groups <- unique(keep_releases$rel.group)
  
  # Update tag group dimension in ini file
  ini.obj@dimensions["taggrps"] <- new_n_taggrps
  
  # Get the index of the last aggregate row
  n_rows_original <- nrow(ini.obj@tag_fish_rep_rate)
  aggregate_row <- n_rows_original
  
  # Combine valid groups with aggregate row
  keep_rows <- c(keep_groups, aggregate_row)
  
  # Update all tag-related slots (row dimension = tag groups + 1 aggregate)
  ini.obj@tag_fish_rep_rate <- ini.obj@tag_fish_rep_rate[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_grp <- ini.obj@tag_fish_rep_grp[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_flags <- ini.obj@tag_fish_rep_flags[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_target <- ini.obj@tag_fish_rep_target[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_pen <- ini.obj@tag_fish_rep_pen[keep_rows, , drop = FALSE]
  
  # Return both updated ini and tag objects
  return(list(ini = ini.obj, tag = new_tag))
}

