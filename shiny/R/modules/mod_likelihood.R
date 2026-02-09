mod_likelihood_ui <- function() {
  tabItem(
    tabName = "likelihood",
    h2("Likelihood Profile", style = "color: #8e44ad;"),

    fluidRow(
      box(
        title = "Settings",
        width = 3,
        solidHeader = TRUE,
        status = "warning",

        pickerInput(
          "lik_scenarios",
          "Scenarios:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} scenarios selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search scenarios...",
            size = 10
          )
        ),

        selectInput(
          "lik_profile_type",
          "Profile Type:",
          choices = c(
            "Components" = "components",
            "Length Frequencies" = "lfs",
            "Weight Frequencies" = "wfs",
            "Tagging" = "tagging",
            "CAL by Fishery" = "cal_fishery",
            "CAL by Year" = "cal_year"
          ),
          selected = "components"
        ),

        helpText(
          "Requires prof/scaler_* outputs (test_plot_output) in each scenario.",
          style = "margin-top: 10px; font-size: 11px; color: #666;"
        )
      ),

      box(
        title = "Likelihood Profile Plot",
        width = 9,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        plotOutput("likelihood_plot", height = "650px")
      )
    )
  )
}

mod_likelihood_server <- function(input, output, session, rv) {
  # Update scenario choices when data is loaded
  observeEvent(rv$data_loaded, {
    req(rv$ParOut_list)
    current_selection <- isolate(input$scenarios)
    if (is.null(current_selection) || length(current_selection) == 0) {
      current_selection <- names(rv$ParOut_list)
    }
    updatePickerInput(
      session,
      "lik_scenarios",
      choices = names(rv$ParOut_list),
      selected = current_selection
    )
  }, ignoreInit = TRUE)

  # Safe read helper for scalar files
  safe_read_scalar <- function(path) {
    if (file.exists(path)) suppressWarnings(as.numeric(read.table(path))) else NA_real_
  }

  # Read average biomass values for a scenario
  read_avg_bio <- function(model_dir, scenario) {
    prof_dir <- file.path(model_dir, scenario, "prof")
    scaler_dirs <- list.dirs(prof_dir, full.names = TRUE, recursive = FALSE)
    scaler_dirs <- grep("scaler_\\d+$", scaler_dirs, value = TRUE)

    if (length(scaler_dirs) > 0) {
      avg_bio_file <- file.path(scaler_dirs[1], "avg_bio")
      if (file.exists(avg_bio_file)) return(safe_read_scalar(avg_bio_file))
    }

    avg_bio_file <- file.path(model_dir, scenario, "avg_bio")
    if (file.exists(avg_bio_file)) return(safe_read_scalar(avg_bio_file))

    NA_real_
  }

  # Convert scaler to biomass units
  scale_to_biomass <- function(scaler, avg_bio) {
    as.numeric(scaler) * 0.01 * avg_bio
  }

  # Load profile outputs for a scenario
  load_profile_outputs <- function(model_dir, scenario) {
    folder <- file.path(model_dir, scenario)
    prof_dir <- file.path(folder, "prof")
    scaler_dirs <- list.dirs(prof_dir, full.names = TRUE, recursive = FALSE)
    scaler_dirs <- grep("scaler_\\d+$", scaler_dirs, value = TRUE)

    if (length(scaler_dirs) > 0) {
      scales <- basename(scaler_dirs) %>% str_extract("\\d+$")
      output_files <- file.path(scaler_dirs, "test_plot_output")
      existing_files <- output_files[file.exists(output_files)]
      existing_scales <- scales[file.exists(output_files)]

      if (length(existing_files) > 0) {
        lik_out <- setNames(map(existing_files, read.MFCLLikelihood), existing_scales)
        lik_raw <- setNames(map(existing_files, readLines), existing_scales)
      } else {
        lik_out <- list()
        lik_raw <- list()
        existing_scales <- character(0)
      }
    } else {
      output_files <- list.files(folder, pattern = "^test_plot_output_\\d+$", full.names = TRUE)

      if (length(output_files) > 0) {
        scales <- basename(output_files) %>% str_extract("\\d+$")
        lik_out <- setNames(map(output_files, read.MFCLLikelihood), scales)
        lik_raw <- setNames(map(output_files, readLines), scales)
        existing_scales <- scales
      } else {
        lik_out <- list()
        lik_raw <- list()
        existing_scales <- character(0)
      }
    }

    list(
      scales = existing_scales,
      lik_out = lik_out,
      lik_raw = lik_raw,
      avg_bio = read_avg_bio(model_dir, scenario)
    )
  }

  # Calculate likelihood change from minimum
  calc_lik_change <- function(df, group_col) {
    df %>%
      group_by(.data[[group_col]], scenario) %>%
      mutate(
        min_value = min(value, na.rm = TRUE),
        change = value - min_value
      ) %>%
      ungroup()
  }

  # Create a standard likelihood profile plot
  create_piner_plot <- function(data, group_var, label = NULL) {
    if (nrow(data) == 0) return(NULL)

    unique_groups <- unique(data[[group_var]])
    non_total_groups <- setdiff(unique_groups, "Total")

    color_values <- c(
      "Total" = "black",
      setNames(viridis::viridis(length(non_total_groups)), non_total_groups)
    )

    p <- ggplot(
      data,
      aes(x = scaler, y = change, colour = .data[[group_var]], shape = .data[[group_var]])
    ) +
      geom_line(aes(linewidth = .data[[group_var]] == "Total"), alpha = 0.7) +
      geom_point(aes(size = .data[[group_var]] == "Total"), alpha = 0.8) +
      scale_color_manual(values = color_values) +
      scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = 0.7), guide = "none") +
      scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2), guide = "none") +
      scale_shape_manual(values = rep(0:24, length.out = length(unique_groups))) +
      facet_wrap(~scenario, scales = "free") +
      scale_x_continuous(
        labels = function(x) x / 1000,
        name = bquote("Average biomass (" * 10^3 * " MT)")
      ) +
      labs(y = "Changes in Likelihood", colour = group_var, shape = group_var) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank()
      )

    if (!is.null(label)) {
      p <- p + annotate("text", x = Inf, y = Inf, label = label,
                        hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold")
    }

    p
  }

  build_components_data <- function(profile_data, scenarios, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        raw <- profile_data[[sc]]$lik_raw[[scl]]
        if (is.null(lik)) next

        penalties <- NA_real_
        if (!is.null(raw) && length(raw) >= 6) {
          penalties <- suppressWarnings(as.numeric(raw[6]))
        }

        values <- c(
          Indices = sum(lik@survey_index),
          LFs = sum(lik@total_length_fish),
          Penalties = penalties,
          WFs = sum(lik@total_weight_fish),
          Age = sum(lik@age_length),
          Tags = sum(unlist(lik@tag_rel_fish, recursive = TRUE))
        )

        scaler_bio <- scale_to_biomass(scl, avg_bio)
        rows[[length(rows) + 1]] <- data.frame(
          scenario = sc,
          scaler = scaler_bio,
          Likelihood = names(values),
          value = as.numeric(values),
          stringsAsFactors = FALSE
        )
      }
    }

    bind_rows(rows)
  }

  build_fishery_data <- function(profile_data, scenarios, fishery_maps, slot_name, label, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next

        vec <- slot(lik, slot_name)
        fish_ids <- as.character(seq_along(vec))
        fish_map <- fishery_maps[[sc]]
        if (is.null(fish_map)) {
          fish_names <- paste("Fishery", fish_ids)
        } else {
          fish_names <- fish_map[fish_ids]
          fish_names[is.na(fish_names)] <- paste("Fishery", fish_ids)
        }

        df <- data.frame(
          scenario = sc,
          scaler = scale_to_biomass(scl, avg_bio),
          group = fish_names,
          value = as.numeric(vec),
          stringsAsFactors = FALSE
        )

        total_row <- data.frame(
          scenario = sc,
          scaler = scale_to_biomass(scl, avg_bio),
          group = "Total",
          value = sum(vec),
          stringsAsFactors = FALSE
        )

        rows[[length(rows) + 1]] <- bind_rows(df, total_row)
      }
    }

    data <- bind_rows(rows)
    if (nrow(data) == 0) return(data)

    names(data)[names(data) == "group"] <- label
    data
  }

  build_tagging_data <- function(profile_data, scenarios, tag_out_list, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      tag_out <- tag_out_list[[sc]]
      if (is.null(tag_out)) next

      rel_df <- tryCatch(as.data.frame(tag_out@releases), error = function(e) NULL)
      if (is.null(rel_df) || nrow(rel_df) == 0) next

      program_map <- rel_df %>%
        distinct(rel.group, program) %>%
        arrange(rel.group) %>%
        rename(program_name = program)

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next

        tag_rel <- lik@tag_rel_fish
        sums_vec <- sapply(tag_rel, function(g) sum(unlist(g)))
        program_names <- program_map$program_name[seq_along(sums_vec)]
        program_names[is.na(program_names)] <- paste("Program", seq_along(sums_vec))

        df <- data.frame(
          scenario = sc,
          scaler = scale_to_biomass(scl, avg_bio),
          program = program_names,
          value = as.numeric(sums_vec),
          stringsAsFactors = FALSE
        )

        rows[[length(rows) + 1]] <- df
      }
    }

    data <- bind_rows(rows)
    if (nrow(data) == 0) return(data)

    data %>%
      group_by(program, scaler, scenario) %>%
      summarise(value = sum(value), .groups = "drop")
  }

  get_alk_summary <- function(age_out) {
    if (is.null(age_out)) return(NULL)
    alk_df <- tryCatch(as.data.frame(age_out@ALK), error = function(e) NULL)
    if (is.null(alk_df) || nrow(alk_df) == 0) return(NULL)

    alk_df %>%
      mutate(order = row_number()) %>%
      group_by(year, month, fishery) %>%
      summarise(first_order = min(order), .groups = "drop") %>%
      arrange(first_order) %>%
      select(-first_order)
  }

  build_cal_data <- function(profile_data, scenarios, age_out_list, fishery_maps, by = "fishery", scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      alk_summary <- get_alk_summary(age_out_list[[sc]])
      if (is.null(alk_summary) || nrow(alk_summary) == 0) next

      for (scl in scales) {
        lik <- profile_data[[sc]]$lik_out[[scl]]
        if (is.null(lik)) next

        lik_vec <- lik@age_length
        n_use <- min(length(lik_vec), nrow(alk_summary))
        if (n_use == 0) next

        df <- alk_summary[seq_len(n_use), , drop = FALSE]
        df$Lik <- lik_vec[seq_len(n_use)]
        df$scenario <- sc
        df$scaler <- scale_to_biomass(scl, avg_bio)

        if (by == "fishery") {
          fish_ids <- as.character(df$fishery)
          fish_map <- fishery_maps[[sc]]
          if (!is.null(fish_map)) {
            df$fishery <- fish_map[fish_ids]
            df$fishery[is.na(df$fishery)] <- paste("Fishery", fish_ids)
          } else {
            df$fishery <- paste("Fishery", fish_ids)
          }

          by_group <- df %>%
            group_by(fishery, scaler, scenario) %>%
            summarise(value = sum(Lik, na.rm = TRUE), .groups = "drop")

          total_row <- by_group %>%
            group_by(scaler, scenario) %>%
            summarise(value = sum(value), .groups = "drop") %>%
            mutate(fishery = "Total")

          rows[[length(rows) + 1]] <- bind_rows(by_group, total_row)
        } else {
          by_group <- df %>%
            group_by(year, scaler, scenario) %>%
            summarise(value = sum(Lik, na.rm = TRUE), .groups = "drop")

          by_group$year <- as.character(by_group$year)
          total_row <- by_group %>%
            group_by(scaler, scenario) %>%
            summarise(value = sum(value), .groups = "drop") %>%
            mutate(year = "Total")

          rows[[length(rows) + 1]] <- bind_rows(by_group, total_row)
        }
      }
    }

    bind_rows(rows)
  }

  likelihood_plot_reactive <- reactive({
    req(rv$data_loaded, input$lik_scenarios, input$model_dir)

    if (length(input$lik_scenarios) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No scenarios selected", size = 6, color = "#999") +
          theme_void()
      )
    }

    selected <- input$lik_scenarios
    profile_data <- setNames(
      lapply(selected, function(sc) load_profile_outputs(input$model_dir, sc)),
      selected
    )

    has_data <- sapply(profile_data, function(x) length(x$scales) > 0)
    profile_data <- profile_data[has_data]

    if (length(profile_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No likelihood profile data found",
                   size = 6, color = "#999") +
          theme_void()
      )
    }

    base_scales <- profile_data[[names(profile_data)[1]]]$scales
    if (length(base_scales) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No likelihood profile data found",
                   size = 6, color = "#999") +
          theme_void()
      )
    }

    type <- input$lik_profile_type

    if (type == "components") {
      data <- build_components_data(profile_data, names(profile_data), base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No component data available",
                                                      size = 6, color = "#999") + theme_void())
      data <- calc_lik_change(data, "Likelihood")
      return(create_piner_plot(data, "Likelihood"))
    }

    if (type == "lfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_length_fish", "Fishery", base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No LF profile data available",
                                                      size = 6, color = "#999") + theme_void())
      data <- calc_lik_change(data, "Fishery")
      return(create_piner_plot(data, "Fishery", "LFs"))
    }

    if (type == "wfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_weight_fish", "Fishery", base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No WF profile data available",
                                                      size = 6, color = "#999") + theme_void())
      data <- calc_lik_change(data, "Fishery")
      return(create_piner_plot(data, "Fishery", "WFs"))
    }

    if (type == "tagging") {
      data <- build_tagging_data(profile_data, names(profile_data), rv$TagOut_list, base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No tagging profile data available",
                                                      size = 6, color = "#999") + theme_void())
      total_row <- data %>%
        group_by(scaler, scenario) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        mutate(program = "Total")
      data <- bind_rows(data, total_row)
      data <- calc_lik_change(data, "program")
      return(create_piner_plot(data, "program", "Tagging"))
    }

    if (type == "cal_fishery") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "fishery", base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No CAL by fishery data available",
                                                      size = 6, color = "#999") + theme_void())
      data <- calc_lik_change(data, "fishery")
      return(create_piner_plot(data, "fishery", "CAL"))
    }

    if (type == "cal_year") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "year", base_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                                      label = "No CAL by year data available",
                                                      size = 6, color = "#999") + theme_void())
      data <- calc_lik_change(data, "year")
      return(create_piner_plot(data, "year", "CAL"))
    }

    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Unsupported profile type", size = 6, color = "#999") +
      theme_void()
  })

  output$likelihood_plot <- renderPlot({
    likelihood_plot_reactive()
  })
}
