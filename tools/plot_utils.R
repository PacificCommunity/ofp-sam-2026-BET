
########################
## Plotting functions ##
########################

GetQuantSimple <- function(calc_function, RepOut_list, minYear, maxYear, scale_factor = 1) {
  # Ensure the provided function is callable
  if (!is.function(calc_function)) {
    stop("The provided calc_function must be a valid function.")
  }
  if (length(RepOut_list) == 0) {
    stop("RepOut_list must not be empty")
  }
  
  has_year_args <- !missing(minYear) && !missing(maxYear) &&
    is.numeric(minYear) && is.numeric(maxYear)
  
  # Build long-format output per scenario to handle mismatched model year ranges.
  out <- purrr::imap_dfr(RepOut_list, function(rep_obj, scenario_name) {
    vals <- calc_function(rep_obj)
    vals <- as.numeric(vals)
    
    # Prefer year labels from the vector itself. Fall back to a sequential index.
    yr_names <- suppressWarnings(as.numeric(names(vals)))
    if (length(yr_names) != length(vals) || all(is.na(yr_names))) {
      if (has_year_args) {
        fallback_years <- minYear:maxYear
        if (length(fallback_years) >= length(vals)) {
          years <- fallback_years[seq_along(vals)]
        } else {
          years <- seq_len(length(vals))
        }
      } else {
        years <- seq_len(length(vals))
      }
    } else {
      years <- yr_names
    }
    
    data.frame(
      Scenario = sub("^skj_", "", scenario_name),
      Year = years,
      Quant = vals * scale_factor
    )
  })
  
  out
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
  
  # Merge defaults with user config
  cfg <- modifyList(config, custom_config)
  
  # Basic validation
  if (length(RepOut_list) == 0) stop("RepOut_list must not be empty")
  if (length(fisheries) == 0)   stop("fisheries must not be empty")
  
  plots <- list()
  scenario_names <- names(RepOut_list)
  
  # Build a fixed color mapping across scenarios for consistent legend
  if (!is.null(cfg$predicted_color)) {
    # Use single user-specified color for all predicted lines
    scenario_colors <- rep(cfg$predicted_color, length(scenario_names))
    names(scenario_colors) <- scenario_names
  } else {
    # Use viridis colors (default)
    scenario_colors <- get_viridis_colors(length(scenario_names), viridis_option)
    names(scenario_colors) <- scenario_names
  }
  
  all_colors <- c("Observed" = cfg$observed_color, scenario_colors)
  
  # Iterate scenarios
  for (i in seq_along(RepOut_list)) {
    scenario_name <- scenario_names[i]
    
    # Extract observed and predicted CPUE
    obs <- as.data.frame(cpue_obs(RepOut_list[[scenario_name]]))
    fit <- as.data.frame(cpue_pred(RepOut_list[[scenario_name]]))
    
    # Standardize column names
    names(obs)[names(obs) == "data"] <- "obs"
    names(fit)[names(fit) == "data"] <- "fit"
    
    # Join and coerce types
    cpue <- merge(obs, fit)
    cpue <- type.convert(cpue, as.is = TRUE)
    
    # Keep only selected fisheries
    cpue <- cpue[cpue$unit %in% fisheries, , drop = FALSE]
    if (nrow(cpue) == 0) {
      warning(paste("No data after filtering for scenario:", scenario_name))
      next
    }
    
    # Optional unit renaming via mapping
    if (!is.null(fisheries_mapping)) {
      cpue$unit <- as.character(cpue$unit)
      cpue$unit <- ifelse(
        cpue$unit %in% names(fisheries_mapping),
        fisheries_mapping[cpue$unit],
        paste("Unknown", cpue$unit)
      )
    }
    
    # Transform for plotting (assuming log-scale inputs)
    cpue <- cpue %>%
      mutate(
        year_season = year + (season - 1) / 4,
        obs = exp(obs),
        fit = exp(fit),
        Scenario = scenario_name
      )
    
    # Determine plot title
    if (!is.null(cfg$show_title) && cfg$show_title == FALSE) {
      plot_title <- NULL
    } else if (!is.null(cfg$custom_title)) {
      plot_title <- cfg$custom_title
    } else {
      plot_title <- paste("Observed vs Fitted CPUE:", scenario_name)
    }
    
    # Build plot
    p <- ggplot(cpue, aes(x = year_season)) +
      geom_point(aes(y = obs, color = "Observed"),
                 size = cfg$point_size, alpha = cfg$alpha, shape = 16) +
      geom_line(aes(y = fit, color = Scenario),  # ✅ CHANGED: use Scenario column instead
                alpha = cfg$alpha, linewidth = cfg$linewidth) +
      facet_wrap(~unit, scales = "free_y", ncol = cfg$facet_ncol) +
      scale_color_manual(values = all_colors, name = "Type") +
      labs(
        title = plot_title,
        x = "Year + Season (Q)",
        y = "CPUE"
      ) +
      theme_bw() +
      theme(
        legend.position = cfg$legend_position,
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
    
    plots[[scenario_name]] <- p
  }
  
  return(plots)
}





# Plotting function: overlay all scenarios in one plot

create_overlay_cpue_plot <- function(RepOut_list,
                                     fisheries,
                                     fisheries_mapping = NULL,
                                     viridis_option = "viridis",
                                     custom_config = list()) {
  
  # Merge defaults with user config
  cfg <- modifyList(config, custom_config)
  
  # Basic validation
  if (length(RepOut_list) == 0) stop("RepOut_list must not be empty")
  if (length(fisheries) == 0)   stop("fisheries must not be empty")
  
  scenario_names <- names(RepOut_list)
  all_cpue <- list()
  
  # Extract data from all scenarios
  for (scenario_name in scenario_names) {
    
    # Extract observed and predicted CPUE
    obs <- as.data.frame(cpue_obs(RepOut_list[[scenario_name]]))
    fit <- as.data.frame(cpue_pred(RepOut_list[[scenario_name]]))
    
    # Standardize column names
    names(obs)[names(obs) == "data"] <- "obs"
    names(fit)[names(fit) == "data"] <- "fit"
    
    # Join and coerce types
    cpue <- merge(obs, fit)
    cpue <- type.convert(cpue, as.is = TRUE)
    
    # Keep only selected fisheries
    cpue <- cpue[cpue$unit %in% fisheries, , drop = FALSE]
    
    # Optional unit renaming
    if (!is.null(fisheries_mapping)) {
      cpue$unit <- as.character(cpue$unit)
      cpue$unit <- ifelse(
        cpue$unit %in% names(fisheries_mapping),
        fisheries_mapping[cpue$unit],
        paste("Unknown", cpue$unit)
      )
    }
    
    # Transform for plotting
    cpue <- cpue %>%
      mutate(
        year_season = year + (season - 1) / 4,
        obs = exp(obs),
        fit = exp(fit),
        scenario = scenario_name
      )
    
    all_cpue[[scenario_name]] <- cpue
  }
  
  # Combine all scenarios
  combined_cpue <- bind_rows(all_cpue)
  
  # Build a robust observed reference from all scenarios.
  # This avoids assuming the first scenario always has valid/full obs.
  obs_data <- combined_cpue %>%
    group_by(unit, year_season) %>%
    summarise(obs = median(obs, na.rm = TRUE), .groups = "drop")
  
  # Build color palette for scenarios
  if (!is.null(cfg$predicted_colors)) {
    # Use user-specified colors
    if (length(cfg$predicted_colors) < length(scenario_names)) {
      warning("Not enough colors provided. Recycling colors.")
      scenario_colors <- rep(cfg$predicted_colors, length.out = length(scenario_names))
    } else {
      scenario_colors <- cfg$predicted_colors[1:length(scenario_names)]
    }
    names(scenario_colors) <- scenario_names
  } else {
    # Use viridis colors (default)
    scenario_colors <- get_viridis_colors(length(scenario_names), viridis_option)
    names(scenario_colors) <- scenario_names
  }
  
  all_colors <- c("Observed" = cfg$observed_color, scenario_colors)
  
  # Determine title
  if (!is.null(cfg$show_title) && cfg$show_title == FALSE) {
    plot_title <- NULL
  } else if (!is.null(cfg$custom_title)) {
    plot_title <- cfg$custom_title
  } else {
    plot_title <- "Observed vs Fitted CPUE: All Scenarios"
  }
  
  # Create overlay plot
  p <- ggplot() +
    # Observed points
    geom_point(data = obs_data, 
               aes(x = year_season, y = obs, color = "Observed"),
               size = cfg$point_size, alpha = cfg$alpha, shape = 16) +
    # Predicted lines for each scenario
    geom_line(data = combined_cpue,
              aes(x = year_season, y = fit, color = scenario),
              alpha = cfg$alpha, linewidth = cfg$linewidth) +
    facet_wrap(~unit, scales = "free_y", ncol = cfg$facet_ncol) +
    scale_color_manual(values = all_colors, name = "Type") +
    labs(
      title = plot_title,
      x = "Year + Season (Q)",
      y = "CPUE"
    ) +
    theme_bw() +
    theme(
      legend.position = cfg$legend_position,
      strip.text = element_text(size = 10, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  return(p)
}
