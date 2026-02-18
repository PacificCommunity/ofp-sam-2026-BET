mod_likelihood_ui <- function() {
  tabItem(
    tabName = "diagnostics",
    h2("Diagnostics", style = "color: #8e44ad;"),

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
          "Diagnostics Type:",
          choices = c(
            "Likelihood: Components" = "components",
            "Likelihood: CPUE by Fishery" = "cpues",
            "Likelihood: Length Frequencies" = "lfs",
            "Likelihood: Weight Frequencies" = "wfs",
            "Likelihood: Tagging" = "tagging",
            "Likelihood: CAL by Fishery" = "cal_fishery",
            "Likelihood: CAL by Year" = "cal_year",
            "Jitter Diagnostics" = "jitter",
            "Retrospective: Depletion & Spawning Potential" = "retro",
            "Hessian Diagnostics (PDH / SPD)" = "hessian"
          ),
          selected = "components"
        ),

        pickerInput(
          "lik_groups",
          "Lines:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} lines selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search lines...",
            size = 10
          )
        ),

        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton(
          "show_lik_download_modal",
          "Download Plot...",
          class = "btn-info",
          style = "width: 100%;",
          icon = icon("download")
        ),

        helpText(
          "Requires prof/scaler_* outputs (test_plot_output) in each scenario.",
          style = "margin-top: 10px; font-size: 11px; color: #666;"
        )
      ),

      box(
        title = "Diagnostics Plot",
        width = 9,
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        plotOutput("likelihood_plot", height = "650px"),
        uiOutput("likelihood_table_ui")
      )
    )
  )
}

mod_likelihood_server <- function(input, output, session, rv) {
  heavy_cache <- reactiveValues(
    retro = list(),
    hessian = list()
  )
  last_group_key <- reactiveVal(NULL)

  scenario_cache_key <- function(model_dir, scenario) {
    paste(
      normalizePath(model_dir, winslash = "/", mustWork = FALSE),
      as.character(scenario),
      sep = "::"
    )
  }

  get_cached_heavy <- function(bucket, key, builder) {
    store <- heavy_cache[[bucket]]
    if (!is.null(store[[key]])) return(store[[key]])
    value <- builder()
    store[[key]] <- value
    heavy_cache[[bucket]] <- store
    value
  }

  clear_heavy_cache <- function() {
    heavy_cache$retro <- list()
    heavy_cache$hessian <- list()
  }

  observeEvent(input$model_dir, {
    clear_heavy_cache()
  }, ignoreInit = TRUE)

  observeEvent(input$load_data, {
    clear_heavy_cache()
  }, ignoreInit = TRUE)

  # Update scenario choices when data is loaded
  observeEvent(rv$data_loaded, {
    req(rv$ParOut_list)
    map_models <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    current_selection <- isolate(input$lik_scenarios)
    if (is.null(current_selection) || length(current_selection) == 0) current_selection <- map_models
    current_selection <- intersect(current_selection, map_models)
    if (length(current_selection) == 0) current_selection <- map_models
    updatePickerInput(
      session,
      "lik_scenarios",
      choices = map_models,
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
      payload_files <- file.path(scaler_dirs, "profile_payload.rds")
      has_payload <- file.exists(payload_files)

      if (any(has_payload)) {
        payloads <- map(payload_files[has_payload], ~ tryCatch(readRDS(.x), error = function(e) NULL))
        payloads <- payloads[!vapply(payloads, is.null, logical(1))]

        if (length(payloads) > 0) {
          existing_scales <- as.character(vapply(payloads, function(x) as.numeric(x$scaler), numeric(1)))
          lik_out <- setNames(map(payloads, "lik_out"), existing_scales)
          lik_raw <- setNames(map(payloads, "lik_raw"), existing_scales)
          avg_bio_payload <- suppressWarnings(as.numeric(payloads[[1]]$avg_bio))
        } else {
          lik_out <- list()
          lik_raw <- list()
          existing_scales <- character(0)
          avg_bio_payload <- NA_real_
        }
      } else {
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
        avg_bio_payload <- NA_real_
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
      avg_bio_payload <- NA_real_
    }

    list(
      scales = existing_scales,
      lik_out = lik_out,
      lik_raw = lik_raw,
      avg_bio = if (is.finite(avg_bio_payload)) avg_bio_payload else read_avg_bio(model_dir, scenario)
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
        values <- c(values, Total = sum(values, na.rm = TRUE))

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

  build_fishery_data <- function(profile_data, scenarios, fishery_maps, slot_name, label, scales,
                                 allowed_fisheries = NULL, fallback_nonzero_only = FALSE) {
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
        keep_idx <- rep(TRUE, length(fish_ids))
        if (!is.null(allowed_fisheries)) {
          allowed_ids <- as.character(allowed_fisheries[[sc]])
          allowed_ids <- allowed_ids[!is.na(allowed_ids)]
          if (length(allowed_ids) > 0) {
            keep_idx <- fish_ids %in% allowed_ids
          } else if (isTRUE(fallback_nonzero_only)) {
            keep_idx <- is.finite(vec) & abs(vec) > 0
          }
        } else if (isTRUE(fallback_nonzero_only)) {
          keep_idx <- is.finite(vec) & abs(vec) > 0
        }

        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_map <- fishery_maps[[sc]]
        fish_names <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))

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

  extract_survey_index_like_from_raw <- function(raw_lines) {
    if (is.null(raw_lines) || length(raw_lines) == 0) return(numeric(0))
    header_idx <- which(grepl("^\\s*#\\s*Survey_index_like_by_fishery\\s*$", raw_lines))
    if (length(header_idx) == 0) return(numeric(0))

    i <- header_idx[1] + 1
    if (i > length(raw_lines)) return(numeric(0))

    block <- character(0)
    while (i <= length(raw_lines)) {
      line <- trimws(raw_lines[i])
      if (!nzchar(line) || grepl("^\\s*#", line)) break
      block <- c(block, line)
      i <- i + 1
    }
    if (length(block) == 0) return(numeric(0))

    vals <- suppressWarnings(as.numeric(strsplit(paste(block, collapse = " "), "\\s+")[[1]]))
    vals[is.finite(vals)]
  }

  build_cpue_fishery_data <- function(profile_data, scenarios, fishery_maps, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      for (scl in scales) {
        raw <- profile_data[[sc]]$lik_raw[[scl]]
        vec <- extract_survey_index_like_from_raw(raw)
        if (length(vec) == 0) next

        fish_ids <- as.character(seq_along(vec))
        keep_idx <- is.finite(vec) & abs(vec) > 0
        vec <- vec[keep_idx]
        fish_ids <- fish_ids[keep_idx]
        if (length(vec) == 0) next

        fish_map <- fishery_maps[[sc]]
        fish_names <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))

        df <- data.frame(
          scenario = sc,
          scaler = scale_to_biomass(scl, avg_bio),
          Fishery = fish_names,
          value = as.numeric(vec),
          stringsAsFactors = FALSE
        )

        total_row <- data.frame(
          scenario = sc,
          scaler = scale_to_biomass(scl, avg_bio),
          Fishery = "Total",
          value = sum(vec),
          stringsAsFactors = FALSE
        )

        rows[[length(rows) + 1]] <- bind_rows(df, total_row)
      }
    }

    bind_rows(rows)
  }

  build_tagging_data <- function(profile_data, scenarios, tag_out_list, scales) {
    if (length(scales) == 0) return(data.frame())

    rows <- list()
    for (sc in scenarios) {
      avg_bio <- profile_data[[sc]]$avg_bio
      if (!is.finite(avg_bio)) next

      tag_out <- tag_out_list[[sc]]
      if (is.null(tag_out)) next

      rel_df <- tryCatch(safe_array_to_df(tag_out@releases), error = function(e) NULL)
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
    alk_df <- tryCatch(safe_array_to_df(age_out@ALK), error = function(e) NULL)
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
          df$fishery <- sapply(fish_ids, function(fid) get_fishery_name(fid, fish_map))

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

  build_jitter_data <- function(scenarios, par_out_list, jitter_pars_list) {
    rows <- list()
    for (sc in scenarios) {
      ref_par <- par_out_list[[sc]]
      jit_list <- jitter_pars_list[[sc]]
      if (is.null(ref_par) || is.null(jit_list) || length(jit_list) == 0) next

      ref_obj <- suppressWarnings(as.numeric(ref_par@obj_fun))
      ref_grad <- suppressWarnings(as.numeric(ref_par@max_grad))
      if (!is.finite(ref_obj)) next

      seeds <- names(jit_list)
      if (is.null(seeds) || any(is.na(seeds) | seeds == "")) {
        seeds <- as.character(seq_along(jit_list))
      }

      extract_jitter_obj <- function(p) {
        if (is.list(p) && !is.null(p$obj_fun)) return(suppressWarnings(as.numeric(p$obj_fun)))
        suppressWarnings(tryCatch(as.numeric(p@obj_fun), error = function(e) NA_real_))
      }
      extract_jitter_grad <- function(p) {
        if (is.list(p) && !is.null(p$max_grad)) return(suppressWarnings(as.numeric(p$max_grad)))
        suppressWarnings(tryCatch(as.numeric(p@max_grad), error = function(e) NA_real_))
      }
      obj_vals <- sapply(jit_list, extract_jitter_obj)
      grad_vals <- sapply(jit_list, extract_jitter_grad)
      keep <- is.finite(obj_vals) & is.finite(grad_vals)
      if (!any(keep)) next

      obj_vals <- obj_vals[keep]
      grad_vals <- grad_vals[keep]
      seeds <- seeds[keep]
      pct_diff <- ((obj_vals - ref_obj) / abs(ref_obj)) * 100

      rows[[length(rows) + 1]] <- data.frame(
        scenario = sc,
        seed = as.character(seeds),
        ref_obj = as.numeric(ref_obj),
        ref_grad = as.numeric(ref_grad),
        obj_fun = as.numeric(obj_vals),
        max_grad = as.numeric(grad_vals),
        pct_diff = as.numeric(pct_diff),
        jitter_id = seq_along(obj_vals),
        stringsAsFactors = FALSE
      )
    }

    bind_rows(rows)
  }
  
  build_retro_data_for_scenario <- function(scenario, model_dir, rep_obj) {
    extract_retro_metrics <- function(rep_obj, scenario, peel) {
      bio_fish <- safe_array_to_df(rep_obj@adultBiomass) %>%
        mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(season), is.finite(data)) %>%
        group_by(year, season) %>%
        summarise(bio_fish = sum(data), .groups = "drop")
      
      bio_nofish <- safe_array_to_df(rep_obj@adultBiomass_nofish) %>%
        mutate(year = suppressWarnings(as.numeric(year)), season = suppressWarnings(as.numeric(season)), data = suppressWarnings(as.numeric(data))) %>%
        filter(is.finite(year), is.finite(season), is.finite(data)) %>%
        group_by(year, season) %>%
        summarise(bio_nofish = sum(data), .groups = "drop")
      
      dep <- bio_fish %>%
        inner_join(bio_nofish, by = c("year", "season")) %>%
        mutate(
          bio_fish = suppressWarnings(as.numeric(bio_fish)),
          bio_nofish = suppressWarnings(as.numeric(bio_nofish))
        ) %>%
        mutate(depletion = bio_fish / pmax(bio_nofish, .Machine$double.eps)) %>%
        group_by(year) %>%
        summarise(depletion = mean(depletion, na.rm = TRUE), .groups = "drop")
      
      sp <- bio_fish %>%
        group_by(year) %>%
        summarise(spawning_potential = mean(bio_fish, na.rm = TRUE) / 1e3, .groups = "drop")
      
      dep %>%
        inner_join(sp, by = "year") %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          scenario = scenario,
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))
    }

    retro_rows <- list()
    if (!is.null(rep_obj)) {
      retro_rows[[paste0(scenario, "_peel_0")]] <- extract_retro_metrics(rep_obj, scenario, 0)
    }

    retro_dir <- file.path(model_dir, scenario, "retro")
    peel_dirs <- list.dirs(retro_dir, recursive = FALSE, full.names = TRUE)
    peel_dirs <- peel_dirs[grepl("peel_\\d+$", peel_dirs)]

    for (pd in peel_dirs) {
      peel_num <- suppressWarnings(as.integer(stringr::str_extract(basename(pd), "\\d+$")))
      if (!is.finite(peel_num)) next

      metrics_file <- file.path(pd, "retro_metrics.rds")
      if (file.exists(metrics_file)) {
        m <- tryCatch(readRDS(metrics_file), error = function(e) NULL)
        if (!is.null(m) && nrow(m) > 0) {
          retro_rows[[paste0(scenario, "_peel_", peel_num)]] <- m
          next
        }
      }

      rep_path <- tryCatch(finalRep(pd), error = function(e) NULL)
      if (is.null(rep_path) || !file.exists(rep_path)) next

      peel_rep_obj <- tryCatch(read.MFCLRep(rep_path), error = function(e) NULL)
      if (is.null(peel_rep_obj)) next

      retro_rows[[paste0(scenario, "_peel_", peel_num)]] <- extract_retro_metrics(peel_rep_obj, scenario, peel_num)
    }

    bind_rows(retro_rows)
  }

  build_hessian_data_for_scenario <- function(scenario, model_dir) {
      hfile <- file.path(model_dir, scenario, "hessian", "hessian_info.rds")
      part_files <- list.files(
        file.path(model_dir, scenario, "hessian"),
        pattern = "^part_\\d+/hessian_info\\.rds$",
        full.names = TRUE,
        recursive = TRUE
      )
      
      if (!file.exists(hfile)) {
        return(data.frame(
          Scenario = scenario,
          Hessian_File = "Missing",
          PDH = NA_character_,
          `SPD (positivised cov)` = NA_character_,
          `Neg. Eigen` = NA_character_,
          `Hessian Status` = NA_character_,
          Reliability = NA_character_,
          `Stitch Complete` = NA_character_,
          `Parts (found/expected)` = ifelse(length(part_files) > 0, as.character(length(part_files)), NA_character_),
          stringsAsFactors = FALSE
        ))
      }
      
      info <- tryCatch(readRDS(hfile), error = function(e) NULL)
      if (is.null(info)) {
        return(data.frame(
          Scenario = scenario,
          Hessian_File = "Read error",
          PDH = NA_character_,
          `SPD (positivised cov)` = NA_character_,
          `Neg. Eigen` = NA_character_,
          `Hessian Status` = NA_character_,
          Reliability = NA_character_,
          `Stitch Complete` = NA_character_,
          `Parts (found/expected)` = ifelse(length(part_files) > 0, as.character(length(part_files)), NA_character_),
          stringsAsFactors = FALSE
        ))
      }
      
      is_pdh <- tryCatch(info$diagnostics$summary$pdh$is_pdh, error = function(e) NA)
      spd_pos_cov <- tryCatch(info$diagnostics$summary$positivised_cov_is_spd, error = function(e) NA)
      n_neg <- tryCatch(info$eigen$n_negative_eigenvalues, error = function(e) NA)
      n_tot <- tryCatch(info$eigen$n_total_eigenvalues, error = function(e) NA)
      h_status <- tryCatch(info$eigen$hessian_status, error = function(e) NA_character_)
      reliability <- tryCatch(info$eigen$reliability, error = function(e) NA_character_)
      stitch_complete <- tryCatch(info$stitch$is_complete, error = function(e) NA)
      n_parts_expected <- tryCatch(info$stitch$n_parts, error = function(e) NA)
      found_parts <- if (length(part_files) > 0) length(part_files) else as.integer(n_parts_expected)
      
      data.frame(
        Scenario = scenario,
        Hessian_File = "OK",
        PDH = dplyr::case_when(
          isTRUE(is_pdh) ~ "PDH",
          identical(is_pdh, FALSE) ~ "Not PDH",
          TRUE ~ "NA"
        ),
        `SPD (positivised cov)` = dplyr::case_when(
          isTRUE(spd_pos_cov) ~ "SPD",
          identical(spd_pos_cov, FALSE) ~ "Not SPD",
          TRUE ~ "NA"
        ),
        `Neg. Eigen` = ifelse(
          is.na(n_neg) || is.na(n_tot),
          NA_character_,
          sprintf("%d / %d", as.integer(n_neg), as.integer(n_tot))
        ),
        `Hessian Status` = as.character(h_status),
        Reliability = as.character(reliability),
        `Stitch Complete` = dplyr::case_when(
          isTRUE(stitch_complete) ~ "Yes",
          identical(stitch_complete, FALSE) ~ "No",
          TRUE ~ "NA"
        ),
        `Parts (found/expected)` = ifelse(
          is.na(n_parts_expected),
          as.character(found_parts),
          sprintf("%d / %d", as.integer(found_parts), as.integer(n_parts_expected))
        ),
        stringsAsFactors = FALSE
      )
  }

  profile_data_reactive <- reactive({
    req(rv$data_loaded, input$lik_scenarios)

    if (length(input$lik_scenarios) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No scenarios selected"))
    }

    type <- input$lik_profile_type
    selected <- input$lik_scenarios

    if (type == "jitter") {
      data <- build_jitter_data(selected, rv$ParOut_list, rv$JitterPars_list)
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No jitter analysis results found", plot_kind = "jitter"))
      }
      return(list(data = data, group_col = NULL, label = "Jitter", message = NULL, plot_kind = "jitter"))
    }
    
    if (type == "retro") {
      req(input$model_dir)
      data <- bind_rows(lapply(selected, function(sc) {
        k <- scenario_cache_key(input$model_dir, sc)
        get_cached_heavy(
          "retro",
          k,
          function() build_retro_data_for_scenario(sc, input$model_dir, rv$RepOut_list[[sc]])
        )
      }))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No retrospective outputs found", plot_kind = "retro"))
      }
      data <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "Retrospective outputs were found but numeric series could not be parsed", plot_kind = "retro"))
      }
      
      eps <- .Machine$double.eps
      base_terminal <- data %>%
        filter(peel == 0) %>%
        select(scenario, year, depletion_base = depletion, spawning_potential_base = spawning_potential)
      
      peel_terminal <- data %>%
        filter(peel > 0) %>%
        group_by(scenario, peel) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        summarise(
          year = max(year, na.rm = TRUE),
          depletion_peel = dplyr::last(depletion),
          spawning_potential_peel = dplyr::last(spawning_potential),
          .groups = "drop"
        )
      
      mohn_summary <- peel_terminal %>%
        left_join(base_terminal, by = c("scenario", "year")) %>%
        mutate(
          rho_dep_component = (depletion_peel - depletion_base) / pmax(abs(depletion_base), eps),
          rho_sp_component = (spawning_potential_peel - spawning_potential_base) / pmax(abs(spawning_potential_base), eps)
        ) %>%
        group_by(scenario) %>%
        summarise(
          mohn_rho_depletion = ifelse(sum(is.finite(rho_dep_component)) > 0, mean(rho_dep_component, na.rm = TRUE), NA_real_),
          mohn_rho_spawning_potential = ifelse(sum(is.finite(rho_sp_component)) > 0, mean(rho_sp_component, na.rm = TRUE), NA_real_),
          .groups = "drop"
        )
      
      return(list(
        data = data,
        group_col = NULL,
        label = "Retrospective",
        message = NULL,
        plot_kind = "retro",
        rho = mohn_summary
      ))
    }
    
    if (type == "hessian") {
      req(input$model_dir)
      data <- bind_rows(lapply(selected, function(sc) {
        k <- scenario_cache_key(input$model_dir, sc)
        get_cached_heavy(
          "hessian",
          k,
          function() build_hessian_data_for_scenario(sc, input$model_dir)
        )
      }))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No Hessian diagnostics found", plot_kind = "hessian"))
      }
      return(list(data = data, group_col = NULL, label = "Hessian", message = NULL, plot_kind = "hessian"))
    }

    req(input$model_dir)

    selected <- input$lik_scenarios
    profile_data <- setNames(
      lapply(selected, function(sc) load_profile_outputs(input$model_dir, sc)),
      selected
    )

    has_data <- sapply(profile_data, function(x) length(x$scales) > 0)
    profile_data <- profile_data[has_data]

    if (length(profile_data) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No likelihood profile data found"))
    }

    common_scales <- Reduce(intersect, lapply(profile_data, function(x) x$scales))
    if (length(common_scales) == 0) {
      return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No shared scaler values across selected scenarios"))
    }

    if (type == "components") {
      data <- build_components_data(profile_data, names(profile_data), common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No component data available"))
      }
      data <- calc_lik_change(data, "Likelihood")
      return(list(data = data, group_col = "Likelihood", label = "Components", message = NULL))
    }

    if (type == "cpues") {
      data <- build_cpue_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS, common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CPUE profile data available"))
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "CPUEs", message = NULL))
    }

    if (type == "lfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_length_fish", "Fishery", common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No LF profile data available"))
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "LFs", message = NULL))
    }

    if (type == "wfs") {
      data <- build_fishery_data(profile_data, names(profile_data), rv$FISHERY_MAPS,
                                 "total_weight_fish", "Fishery", common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No WF profile data available"))
      }
      data <- calc_lik_change(data, "Fishery")
      return(list(data = data, group_col = "Fishery", label = "WFs", message = NULL))
    }

    if (type == "tagging") {
      data <- build_tagging_data(profile_data, names(profile_data), rv$TagOut_list, common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No tagging profile data available"))
      }
      total_row <- data %>%
        group_by(scaler, scenario) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        mutate(program = "Total")
      data <- bind_rows(data, total_row)
      data <- calc_lik_change(data, "program")
      return(list(data = data, group_col = "program", label = "Tagging", message = NULL))
    }

    if (type == "cal_fishery") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "fishery", common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CAL by fishery data available"))
      }
      data <- calc_lik_change(data, "fishery")
      return(list(data = data, group_col = "fishery", label = "CAL", message = NULL))
    }

    if (type == "cal_year") {
      data <- build_cal_data(profile_data, names(profile_data), rv$AgeOut_list,
                             rv$FISHERY_MAPS, by = "year", common_scales)
      data <- data %>% filter(is.finite(value) & is.finite(scaler))
      if (nrow(data) == 0) {
        return(list(data = data.frame(), group_col = NULL, label = NULL, message = "No CAL by year data available"))
      }
      data <- calc_lik_change(data, "year")
      return(list(data = data, group_col = "year", label = "CAL", message = NULL))
    }

    list(data = data.frame(), group_col = NULL, label = NULL, message = "Unsupported profile type")
  })

  observeEvent(profile_data_reactive(), {
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (is.null(info$group_col) || nrow(info$data) == 0 || plot_kind %in% c("jitter", "retro", "hessian")) {
      last_group_key(NULL)
      updatePickerInput(session, "lik_groups", choices = character(0), selected = character(0))
      return()
    }

    groups <- sort(unique(info$data[[info$group_col]]))
    group_key <- paste(
      input$lik_profile_type,
      info$group_col,
      paste(groups, collapse = "||"),
      sep = "::"
    )

    current <- isolate(input$lik_groups)
    if (!identical(last_group_key(), group_key)) {
      selected <- groups
    } else {
      if (is.null(current) || length(current) == 0) {
        selected <- groups
      } else {
        selected <- intersect(current, groups)
        if (length(selected) == 0) selected <- groups
      }
    }

    last_group_key(group_key)
    updatePickerInput(session, "lik_groups", choices = groups, selected = selected)
  }, ignoreInit = TRUE)

  likelihood_plot_reactive <- reactive({
    info <- profile_data_reactive()
    if (!is.null(info$message)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = info$message, size = 6, color = "#999") +
          theme_void()
      )
    }

    data <- info$data
    group_col <- info$group_col
    label <- info$label
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"

    if (!identical(plot_kind, "jitter") && !is.null(input$lik_groups) && length(input$lik_groups) > 0) {
      data <- data[data[[group_col]] %in% input$lik_groups, , drop = FALSE]
    }

    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data after filtering", size = 6, color = "#999") +
          theme_void()
      )
    }

    if (identical(plot_kind, "jitter")) {
      plot_df <- data %>%
        mutate(
          is_outlier = pct_diff < -5 | pct_diff > 20,
          outlier_direction = case_when(
            pct_diff < -5 ~ "below",
            pct_diff > 20 ~ "above",
            TRUE ~ "none"
          )
        )

      # Log-scale x requires positive gradients.
      plot_df <- plot_df %>% mutate(max_grad = ifelse(max_grad > 0, max_grad, NA_real_))
      ref_df <- plot_df %>%
        group_by(scenario) %>%
        summarise(ref_grad = first(ref_grad), .groups = "drop") %>%
        mutate(ref_grad = ifelse(ref_grad > 0, ref_grad, NA_real_))

      outliers_df <- plot_df %>% filter(is_outlier)
      non_outlier <- plot_df %>% filter(!is_outlier)

      return(
        ggplot(non_outlier, aes(x = max_grad, y = pct_diff)) +
          geom_point(aes(color = jitter_id), size = 3, alpha = 0.7, na.rm = TRUE) +
          geom_point(
            data = outliers_df %>% filter(outlier_direction == "above"),
            aes(x = max_grad, y = 19.5),
            inherit.aes = FALSE,
            color = "orange", size = 3, shape = 24, fill = "orange", na.rm = TRUE
          ) +
          geom_point(
            data = outliers_df %>% filter(outlier_direction == "below"),
            aes(x = max_grad, y = -4.5),
            inherit.aes = FALSE,
            color = "orange", size = 3, shape = 25, fill = "orange", na.rm = TRUE
          ) +
          geom_point(
            data = ref_df,
            aes(x = ref_grad, y = 0),
            inherit.aes = FALSE,
            color = "red", size = 5, shape = 18, na.rm = TRUE
          ) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.5) +
          geom_vline(xintercept = 0.001, linetype = "dotted", color = "gray50", linewidth = 0.6) +
          scale_x_log10() +
          coord_cartesian(ylim = c(-5, 20)) +
          facet_wrap(~ scenario, scales = "free_x", ncol = 2) +
          scale_color_viridis_c(option = "D", name = "Jitter ID") +
          labs(
            x = "Maximum Gradient (log scale)",
            y = "% Difference in Objective Function",
            title = "Jitter Analysis: Convergence Diagnostics",
            subtitle = "Red diamond = Reference model | Gray line = 0.001 | Orange triangles = Outliers"
          ) +
          theme_bw(base_size = 12) +
          theme(
            legend.position = "right",
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "lightblue"),
            panel.grid.minor = element_blank()
          )
      )
    }
    
    if (identical(plot_kind, "retro")) {
      retro_df <- data %>%
        mutate(
          year = suppressWarnings(as.numeric(year)),
          depletion = suppressWarnings(as.numeric(depletion)),
          spawning_potential = suppressWarnings(as.numeric(spawning_potential)),
          scenario = factor(scenario, levels = unique(scenario)),
          peel = as.integer(peel)
        ) %>%
        filter(is.finite(year), is.finite(depletion), is.finite(spawning_potential), !is.na(peel))
      if (nrow(retro_df) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No retrospective numeric data after parsing", size = 6, color = "#999") +
            theme_void()
        )
      }
      
      peel_levels <- sort(unique(retro_df$peel))
      peel_levels_chr <- as.character(peel_levels)
      
      terminal_by_peel <- retro_df %>%
        group_by(scenario, peel) %>%
        summarise(terminal_year = max(year, na.rm = TRUE), .groups = "drop") %>%
        group_by(peel) %>%
        summarise(
          terminal_year = ifelse(n_distinct(terminal_year) == 1,
                                 dplyr::first(terminal_year),
                                 max(terminal_year, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        arrange(peel)
      
      peel_labels <- setNames(as.character(terminal_by_peel$terminal_year), as.character(terminal_by_peel$peel))
      peel_colors <- if (length(peel_levels) == 1) {
        c("0" = "black")
      } else {
        c(
          "0" = "black",
          setNames(
            viridis(length(peel_levels[peel_levels > 0]), option = "C", direction = -1),
            as.character(peel_levels[peel_levels > 0])
          )
        )
      }
      
      rho_df <- info$rho
      dep_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, label = sprintf("Mohn's rho: %.3f", mohn_rho_depletion))
      } else {
        data.frame(scenario = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      sp_anno <- if (!is.null(rho_df) && nrow(rho_df) > 0) {
        rho_df %>% transmute(scenario, label = sprintf("Mohn's rho: %.3f", mohn_rho_spawning_potential))
      } else {
        data.frame(scenario = character(0), label = character(0), stringsAsFactors = FALSE)
      }
      
      dep_plot <- ggplot(
        retro_df,
        aes(
          x = year,
          y = depletion,
          color = factor(peel, levels = peel_levels_chr),
          group = interaction(scenario, peel)
        )
      ) +
        geom_line(linewidth = 1.1, alpha = 0.9) +
        geom_text(
          data = dep_anno,
          aes(x = Inf, y = Inf, label = label),
          inherit.aes = FALSE,
          hjust = 1.05, vjust = 1.2, size = 3.4, fontface = "bold", color = "black"
        ) +
        facet_wrap(~scenario, scales = "free_x", ncol = 2) +
        scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
        labs(
          x = "Year",
          y = bquote(SB/SB["F=0"]),
          color = "Terminal year",
          title = "Retrospective Depletion"
        ) +
        coord_cartesian(ylim = c(0, 1.1)) +
        theme_bw(base_size = 12)
      
      sp_plot <- ggplot(
        retro_df,
        aes(
          x = year,
          y = spawning_potential,
          color = factor(peel, levels = peel_levels_chr),
          group = interaction(scenario, peel)
        )
      ) +
        geom_line(linewidth = 1.1, alpha = 0.9) +
        geom_text(
          data = sp_anno,
          aes(x = Inf, y = Inf, label = label),
          inherit.aes = FALSE,
          hjust = 1.05, vjust = 1.2, size = 3.4, fontface = "bold", color = "black"
        ) +
        facet_wrap(~scenario, scales = "free_x", ncol = 2) +
        scale_color_manual(values = peel_colors, breaks = peel_levels_chr, labels = peel_labels) +
        labs(
          x = "Year",
          y = bquote("Spawning Potential (" * 10^3 * " MT)"),
          color = "Terminal year",
          title = "Retrospective Spawning Potential"
        ) +
        theme_bw(base_size = 12)
      
      return(cowplot::plot_grid(dep_plot, sp_plot, ncol = 1, align = "v"))
    }
    
    if (identical(plot_kind, "hessian")) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Hessian diagnostics table is shown below", size = 6, color = "#777") +
          theme_void()
      )
    }

    create_piner_plot(data, group_col, label)
  })

  output$likelihood_plot <- renderPlot({
    likelihood_plot_reactive()
  })
  
  output$likelihood_table_ui <- renderUI({
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (!identical(plot_kind, "hessian")) return(NULL)
    tagList(
      br(),
      h4("Hessian Diagnostics Table"),
      DTOutput("likelihood_table")
    )
  })
  
  output$likelihood_table <- renderDT({
    info <- profile_data_reactive()
    plot_kind <- if (!is.null(info$plot_kind)) info$plot_kind else "piner"
    if (!identical(plot_kind, "hessian") || nrow(info$data) == 0) return(NULL)
    
    datatable(
      info$data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  observeEvent(input$show_lik_download_modal, {
    show_download_modal("lik", "Likelihood Profile Plot")
  })

  observeEvent(input$lik_preset_wide, {
    updateNumericInput(session, "lik_width", value = 16)
    updateNumericInput(session, "lik_height", value = 9)
  })

  observeEvent(input$lik_preset_standard, {
    updateNumericInput(session, "lik_width", value = 12)
    updateNumericInput(session, "lik_height", value = 9)
  })

  observeEvent(input$lik_preset_square, {
    updateNumericInput(session, "lik_width", value = 10)
    updateNumericInput(session, "lik_height", value = 10)
  })

  output$lik_download_confirm <- downloadHandler(
    filename = function() {
      format <- input$lik_format
      paste0("likelihood_profile_", Sys.Date(), ".", format)
    },
    content = function(file) {
      p <- likelihood_plot_reactive()
      width <- input$lik_width
      height <- input$lik_height
      dpi <- as.numeric(input$lik_dpi)
      format <- input$lik_format

      if (format == "png") {
        ggsave(file, plot = p, width = width, height = height, dpi = dpi,
               device = "png", bg = "white")
      } else if (format == "pdf") {
        ggsave(file, plot = p, width = width, height = height,
               device = "pdf")
      } else if (format == "svg") {
        ggsave(file, plot = p, width = width, height = height,
               device = "svg", bg = "white")
      } else if (format == "jpeg") {
        ggsave(file, plot = p, width = width, height = height, dpi = dpi,
               device = "jpeg", bg = "white", quality = 95)
      }

      removeModal()
    }
  )
}
