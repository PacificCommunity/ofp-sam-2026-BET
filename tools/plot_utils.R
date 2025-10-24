
########################
## Plotting functions ##
########################

GetQuantSimple <- function(calc_function, RepOut_list, minYear, maxYear, scale_factor = 1) {
  # Ensure the provided function is callable
  if (!is.function(calc_function)) {
    stop("The provided calc_function must be a valid function.")
  }
  
  # Calculate the quantity of interest using the provided function
  QuantInterest <- do.call(rbind, lapply(RepOut_list, calc_function))
  colnames(QuantInterest) <- minYear:maxYear
  
  # Melt to long format
  QuantInterest <- reshape2::melt(QuantInterest)
  colnames(QuantInterest) <- c("Scenario", "Year", "Quant")
  
  # Apply scaling
  QuantInterest$Quant <- QuantInterest$Quant * scale_factor
  
  # Rename scenarios
  QuantInterest[, 1] <- sub("^skj_", "", QuantInterest[, 1])
  
  return(QuantInterest)
}




# Return n distinct colors from a chosen viridis option
get_viridis_colors <- function(n, option = "viridis") {
  # valid options: "viridis", "plasma", "inferno", "magma", "cividis"
  switch(option,
         "viridis" = viridis::viridis(n),
         "plasma"  = viridis::plasma(n),
         "inferno" = viridis::inferno(n),
         "magma"   = viridis::magma(n),
         "cividis" = viridis::cividis(n),
         viridis::viridis(n) # default fallback
  )
}

# Main plotting function: builds a plot per scenario
# - RepOut_list: list with model outputs per scenario
# - fisheries: character vector of fishery units to include
# - fisheries_mapping: optional named vector to rename units
# - viridis_option: which viridis palette to use
# - custom_config: list to override defaults in `config`
create_cpue_plots <- function(RepOut_list,
                              fisheries,
                              fisheries_mapping = NULL,
                              viridis_option = "viridis",
                              custom_config = list()) {
  
  # merge defaults with user config
  cfg <- modifyList(config, custom_config)
  
  # basic validation
  if (length(RepOut_list) == 0) stop("RepOut_list must not be empty")
  if (length(fisheries) == 0)   stop("fisheries must not be empty")
  
  plots <- list()
  scenario_names <- names(RepOut_list)
  
  # build a fixed color mapping across scenarios for consistent legend
  scenario_colors <- get_viridis_colors(length(scenario_names), viridis_option)
  names(scenario_colors) <- scenario_names
  all_colors <- c("Observed" = cfg$observed_color, scenario_colors)
  
  # iterate scenarios
  for (i in seq_along(RepOut_list)) {
    scenario_name <- scenario_names[i]
    
    # 1) extract observed and predicted CPUE
    # Note: cpue_obs/cpue_pred are expected to exist in your environment
    obs <- as.data.frame(cpue_obs(RepOut_list[[scenario_name]]))
    fit <- as.data.frame(cpue_pred(RepOut_list[[scenario_name]]))
    
    # 2) standardize column names
    names(obs)[names(obs) == "data"] <- "obs"
    names(fit)[names(fit) == "data"] <- "fit"
    
    # 3) join and coerce types
    cpue <- merge(obs, fit)
    cpue <- type.convert(cpue, as.is = TRUE)
    
    # 4) keep only selected fisheries
    cpue <- cpue[cpue$unit %in% fisheries, , drop = FALSE]
    if (nrow(cpue) == 0) {
      warning(paste("No data after filtering for scenario:", scenario_name))
      next
    }
    
    # 5) optional unit renaming via mapping
    if (!is.null(fisheries_mapping)) {
      cpue$unit <- as.character(cpue$unit)
      cpue$unit <- ifelse(
        cpue$unit %in% names(fisheries_mapping),
        fisheries_mapping[cpue$unit],
        paste("Unknown", cpue$unit)
      )
    }
    
    # 6) transform for plotting (assuming log-scale inputs)
    cpue <- cpue %>%
      mutate(
        year_season = year + (season - 1) / 4,  # combine year and season (quarterly)
        obs = exp(obs),
        fit = exp(fit)
      )
    
    # 7) build plot
    p <- ggplot(cpue, aes(x = year_season)) +
      geom_point(aes(y = obs, color = "Observed"),
                 size = cfg$point_size, alpha = cfg$alpha, shape = 16) +
      geom_line(aes(y = fit, color = scenario_name),
                alpha = cfg$alpha, linewidth = cfg$linewidth) +
      facet_wrap(~unit, scales = "free_y", ncol = cfg$facet_ncol) +
      scale_color_manual(values = all_colors, name = "Type") +
      labs(
        title = paste("Observed vs Fitted CPUE:", scenario_name),
        x = "Year + Season (Q)",
        y = "CPUE"
      ) +
      theme(
        legend.position = cfg$legend_position,
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    plots[[scenario_name]] <- p
  }
  
  return(plots)
}


