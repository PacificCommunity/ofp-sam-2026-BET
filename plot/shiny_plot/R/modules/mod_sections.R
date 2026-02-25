mod_sections_download <- function(prefix, title, plot_reactive, input, session, output) {
  observeEvent(input[[paste0("show_", prefix, "_download_modal")]], {
    show_download_modal(prefix, title, current_save_dir = input$plot_export_dir)
  })

  observeEvent(input[[paste0(prefix, "_preset_wide")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 16)
    updateNumericInput(session, paste0(prefix, "_height"), value = 9)
  })
  observeEvent(input[[paste0(prefix, "_preset_standard")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 12)
    updateNumericInput(session, paste0(prefix, "_height"), value = 9)
  })
  observeEvent(input[[paste0(prefix, "_preset_square")]], {
    updateNumericInput(session, paste0(prefix, "_width"), value = 10)
    updateNumericInput(session, paste0(prefix, "_height"), value = 10)
  })

  output[[paste0(prefix, "_download_confirm")]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_", Sys.Date(), ".", input[[paste0(prefix, "_format")]])
    },
    content = function(file) {
      p <- plot_reactive()
      width <- input[[paste0(prefix, "_width")]]
      height <- input[[paste0(prefix, "_height")]]
      dpi <- as.numeric(input[[paste0(prefix, "_dpi")]])
      format <- input[[paste0(prefix, "_format")]]
      save_plot_with_format(p, file, width = width, height = height, dpi = dpi, format = format)
      removeModal()
    }
  )

  register_folder_save_button(
    plot_type = prefix,
    plot_reactive = plot_reactive,
    input = input,
    session = session,
    output = output,
    filename_fun = function() paste0(prefix, "_", Sys.Date(), ".", input[[paste0(prefix, "_format")]])
  )
}

subset_named <- function(x, keys) {
  if (is.null(x)) return(list())
  x[keys]
}

selected_window <- function(par_list) {
  minYear <- max(sapply(par_list, function(p) as.numeric(p@range["minyear"])), na.rm = TRUE)
  maxYear <- min(sapply(par_list, function(p) as.numeric(p@range["maxyear"])), na.rm = TRUE)
  if (!is.finite(minYear) || !is.finite(maxYear) || minYear > maxYear) {
    minYear <- min(sapply(par_list, function(p) as.numeric(p@range["minyear"])), na.rm = TRUE)
    maxYear <- max(sapply(par_list, function(p) as.numeric(p@range["maxyear"])), na.rm = TRUE)
  }
  list(minYear = minYear, maxYear = maxYear)
}

mod_harvest_ui <- function() {
  tabItem(
    tabName = "harvest",
    h2("Key Quantities", style = "color: #00a65a;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "success",
        pickerInput("harvest_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("harvest_plot", "Plot:", choices = c(
          "Spawning/Recruitment/SP/F (combined)" = "spawning",
          "Depletion by Area" = "depletion_area",
          "Recruitment by Area" = "rec_area",
          "Juvenile & Adult F by Area" = "fm_juv_adult",
          "Area Contribution to Total F" = "fm_area_contrib",
          "Spawning Potential (with/without fishing)" = "sp_combined",
          "Total Biomass (with/without fishing)" = "tb_combined"
        )),
        numericInput("harvest_facet_ncol", "Facet columns:", value = 2, min = 1, max = 12, step = 1),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_harvest_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(title = "Plot", width = 9, solidHeader = TRUE, status = "success", collapsible = TRUE,
          plotOutput("harvest_plot_output", height = "730px"))
    )
  )
}

mod_harvest_server <- function(input, output, session, rv) {
  observeEvent(rv$data_loaded, {
    sc <- names(rv$ParOut_list)
    updatePickerInput(session, "harvest_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  harvest_plot_reactive <- reactive({
    req(rv$data_loaded, input$harvest_scenarios)
    scenarios_name <- input$harvest_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    yw <- selected_window(ParOut_list)
    minYear <- yw$minYear
    maxYear <- yw$maxYear

    config <- list(linewidth = 1.2, alpha = 0.9)
    facet_ncol <- suppressWarnings(as.integer(input$harvest_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 3
    facet_ncol <- min(max(facet_ncol, 1), 12)

    plot_type <- if (is.null(input$harvest_plot)) "spawning" else input$harvest_plot

    if (plot_type == "spawning") {
      SBdep <- tryCatch(GetQuantSimple(SBSBF0, RepOut_list, minYear, maxYear), error = function(e) {
        map_dfr(names(RepOut_list), function(sc) {
          rep_obj <- RepOut_list[[sc]]
          bio_fish <- safe_array_to_df(rep_obj@adultBiomass) %>%
            group_by(year, season) %>%
            summarise(bio_fish = sum(data, na.rm = TRUE), .groups = "drop")
          
          bio_nofish <- safe_array_to_df(rep_obj@adultBiomass_nofish) %>%
            group_by(year, season) %>%
            summarise(bio_nofish = sum(data, na.rm = TRUE), .groups = "drop")
          
          bio_fish %>%
            inner_join(bio_nofish, by = c("year", "season")) %>%
            mutate(
              Scenario = sc,
              Year = as.numeric(year),
              Quant = bio_fish / pmax(bio_nofish, .Machine$double.eps)
            ) %>%
            group_by(Scenario, Year) %>%
            summarise(Quant = mean(Quant, na.rm = TRUE), .groups = "drop")
        })
      })

      rec_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        rec_temp <- safe_array_to_df(RepOut_list[[i]]@rec_region) %>% mutate(scenario = scenarios_name[i])
        rec_all <- bind_rows(rec_all, rec_temp)
      }
      rec_total <- rec_all %>% group_by(year, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop")
      Rec <- rec_total %>% rename(Year = year, Quant = data, Scenario = scenario)

      bioFish_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }
      SpawnPot <- bioFish_all %>%
        group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>%
        group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>%
        rename(Year = year, Quant = data, Scenario = scenario)

      fm_all <- data.frame(); popn_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i])
        fm_all <- bind_rows(fm_all, fm_temp)
        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data)
        popn_all <- bind_rows(popn_all, popn_temp)
      }

      FM <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(catch = data * N) %>%
        group_by(year, season, scenario) %>%
        summarise(total_catch = sum(catch, na.rm = TRUE), total_N = sum(N, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate = total_catch / total_N, inst_F = -log(pmax(1 - harvest_rate, 0.001))) %>%
        group_by(year, scenario) %>% summarise(Quant = sum(inst_F, na.rm = TRUE), .groups = "drop") %>%
        rename(Year = year, Scenario = scenario)

      scenario_levels <- unique(SBdep$Scenario)
      scenario_colors <- get_scenario_colors(scenario_levels)
      SBdep$Scenario <- factor(SBdep$Scenario, levels = scenario_levels)
      Rec$Scenario <- factor(Rec$Scenario, levels = scenario_levels)
      SpawnPot$Scenario <- factor(SpawnPot$Scenario, levels = scenario_levels)
      FM$Scenario <- factor(FM$Scenario, levels = scenario_levels)

      common_theme <- theme_bw() + theme(legend.position = "none")
      SBdepPlot <- ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
        labs(x = "Year", y = bquote(SB/SB["F=0"])) + coord_cartesian(ylim = c(0, 1)) +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") + common_theme

      RecPlot <- ggplot(Rec, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) + coord_cartesian(ylim = c(0, NA)) +
        scale_color_manual(values = scenario_colors) + labs(x = "Year", y = "Recruitment (Millions)") + common_theme

      SpawnPotPlot <- ggplot(SpawnPot, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
        labs(x = "Year", y = bquote("Spawning Potential (" * 10^3 * " MT)")) + coord_cartesian(ylim = c(0, NA)) + common_theme

      FMPlot <- ggplot(FM, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
        geom_line(linewidth = 1.2) + scale_color_manual(values = scenario_colors) +
        labs(x = "Year", y = "Annual Instantaneous F") + coord_cartesian(ylim = c(0, NA)) + common_theme

      scenario_legend <- cowplot::get_legend(
        ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )

      combined_ncol <- min(max(as.integer(facet_ncol), 1), 4)
      combined_plot <- cowplot::plot_grid(
        SBdepPlot, RecPlot, SpawnPotPlot, FMPlot,
        ncol = combined_ncol,
        align = "v"
      )
      return(cowplot::plot_grid(combined_plot, scenario_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    if (plot_type == "depletion_area") {
      bioNoFish_all <- data.frame(); bioFish_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        bioNoFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass_nofish) %>% mutate(scenario = scenarios_name[i])
        bioNoFish_all <- bind_rows(bioNoFish_all, bioNoFish_temp)
      }
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(RepOut_list[[i]]@adultBiomass) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }

      bioNoFish_yearly <- bioNoFish_all %>% group_by(year, area, scenario) %>% summarise(biomass_nofish = sum(data) / n_distinct(season), .groups = "drop")
      bioFish_yearly <- bioFish_all %>% group_by(year, area, scenario) %>% summarise(biomass_fish = sum(data) / n_distinct(season), .groups = "drop")

      depletion_area <- bioFish_yearly %>% left_join(bioNoFish_yearly, by = c("year", "area", "scenario")) %>% mutate(depletion = biomass_fish / biomass_nofish)
      bioNoFish_all_areas <- bioNoFish_all %>% group_by(year, season, scenario) %>% summarise(biomass_nofish = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(biomass_nofish = mean(biomass_nofish), .groups = "drop")
      bioFish_all_areas <- bioFish_all %>% group_by(year, season, scenario) %>% summarise(biomass_fish = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(biomass_fish = mean(biomass_fish), .groups = "drop")
      depletion_all <- bioFish_all_areas %>% left_join(bioNoFish_all_areas, by = c("year", "scenario")) %>% mutate(depletion = biomass_fish / biomass_nofish, area = "All")
      depletion_combined <- bind_rows(depletion_area, depletion_all)

      scenario_levels <- unique(depletion_combined$scenario)
      scenario_colors <- get_scenario_colors(scenario_levels)
      depletion_combined$scenario <- factor(depletion_combined$scenario, levels = scenario_levels)

      depletion_plot <- ggplot(depletion_combined, aes(x = year, y = depletion, color = scenario)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkred") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen") +
        scale_color_manual(values = scenario_colors) + coord_cartesian(ylim = c(0, 1)) +
        facet_wrap(~ area, ncol = facet_ncol) + labs(x = "Year", y = bquote(SB/SB["F=0"])) +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(depletion_combined, aes(x = year, y = depletion, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )
      return(cowplot::plot_grid(depletion_plot, scenario_legend, ncol = 2, rel_widths = c(4, 0.8)))
    }

    if (plot_type == "rec_area") {
      rec_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        rec_temp <- safe_array_to_df(RepOut_list[[i]]@rec_region) %>% mutate(scenario = scenarios_name[i])
        rec_all <- bind_rows(rec_all, rec_temp)
      }

      rec_yearly <- rec_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop")
      rec_total <- rec_all %>% group_by(year, scenario) %>% summarise(data = sum(data) / 1e6, .groups = "drop") %>% mutate(area = "All")
      rec_combined <- bind_rows(rec_yearly, rec_total)

      scenario_levels <- unique(rec_combined$scenario)
      scenario_colors <- get_scenario_colors(scenario_levels)
      rec_combined$scenario <- factor(rec_combined$scenario, levels = scenario_levels)

      rec_plot <- ggplot(rec_combined, aes(x = year, y = data, color = scenario)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) + scale_color_manual(values = scenario_colors) +
        coord_cartesian(ylim = c(0, NA)) + facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = "Recruitment (Millions)") + theme_bw() +
        theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(rec_combined, aes(x = year, y = data, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )
      return(cowplot::plot_grid(rec_plot, scenario_legend, ncol = 2, rel_widths = c(4, 0.8)))
    }

    if (plot_type == "fm_juv_adult") {
      fm_all <- data.frame(); mat_all <- data.frame(); popn_all <- data.frame()

      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i])
        fm_all <- bind_rows(fm_all, fm_temp)

        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data)
        popn_all <- bind_rows(popn_all, popn_temp)

        mat_temp <- safe_array_to_df(ParOut_list[[i]]@mat) %>% arrange(age, season) %>%
          mutate(scenario = scenarios_name[i], age_new = row_number()) %>%
          select(age = age_new, maturity = data, scenario)
        mat_all <- bind_rows(mat_all, mat_temp)
      }

      fm_juv_adult_area <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(juv = (1 - maturity) * N, adult = maturity * N, catch.juv = data * juv, catch.adult = data * adult) %>%
        group_by(year, season, area, scenario) %>%
        summarise(catch.juv = sum(catch.juv, na.rm = TRUE), juv = sum(juv, na.rm = TRUE), catch.adult = sum(catch.adult, na.rm = TRUE), adult = sum(adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate.juv = catch.juv / juv, harvest_rate.adult = catch.adult / adult,
               F.juv = -log(pmax(1 - harvest_rate.juv, 0.001)), F.adult = -log(pmax(1 - harvest_rate.adult, 0.001))) %>%
        group_by(year, area, scenario) %>% summarise(F.juv = sum(F.juv, na.rm = TRUE), F.adult = sum(F.adult, na.rm = TRUE), .groups = "drop")

      fm_juv_adult_all <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(juv = (1 - maturity) * N, adult = maturity * N, catch.juv = data * juv, catch.adult = data * adult) %>%
        group_by(year, season, scenario) %>%
        summarise(catch.juv = sum(catch.juv, na.rm = TRUE), juv = sum(juv, na.rm = TRUE), catch.adult = sum(catch.adult, na.rm = TRUE), adult = sum(adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(harvest_rate.juv = catch.juv / juv, harvest_rate.adult = catch.adult / adult,
               F.juv = -log(pmax(1 - harvest_rate.juv, 0.001)), F.adult = -log(pmax(1 - harvest_rate.adult, 0.001))) %>%
        group_by(year, scenario) %>% summarise(F.juv = sum(F.juv, na.rm = TRUE), F.adult = sum(F.adult, na.rm = TRUE), .groups = "drop") %>%
        mutate(area = "All")

      fm_juv_adult_combined <- bind_rows(fm_juv_adult_area, fm_juv_adult_all)
      fm_plot_data <- fm_juv_adult_combined %>%
        pivot_longer(cols = c(F.juv, F.adult), names_to = "type", values_to = "F") %>%
        mutate(type = factor(type, levels = c("F.juv", "F.adult"), labels = c("Juvenile F", "Adult F")))

      scenario_levels <- unique(fm_plot_data$scenario)
      scenario_colors <- get_scenario_colors(scenario_levels)
      fm_plot_data$scenario <- factor(fm_plot_data$scenario, levels = scenario_levels)

      fm_area_plot <- ggplot(fm_plot_data, aes(x = year, y = F, color = scenario, linetype = type)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        scale_color_manual(values = scenario_colors) +
        scale_linetype_manual(name = "Life Stage", values = c("Juvenile F" = "dashed", "Adult F" = "solid")) +
        coord_cartesian(ylim = c(0, NA)) + facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = "Annual Instantaneous F") +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(fm_plot_data, aes(x = year, y = F, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )

      lifestage_legend <- cowplot::get_legend(
        ggplot(fm_plot_data, aes(x = year, y = F, linetype = type)) + geom_line(linewidth = 2) +
          scale_linetype_manual(name = "Life Stage", values = c("Juvenile F" = "dashed", "Adult F" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(linewidth = 1.5))) +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(2.5, "cm"))
      )

      combined_legend <- cowplot::plot_grid(scenario_legend, lifestage_legend, ncol = 1)
      return(cowplot::plot_grid(fm_area_plot, combined_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    if (plot_type == "fm_area_contrib") {
      fm_all <- data.frame(); mat_all <- data.frame(); popn_all <- data.frame(); m_all <- data.frame()
      for (i in seq_along(RepOut_list)) {
        fm_temp <- safe_array_to_df(RepOut_list[[i]]@fm) %>% mutate(scenario = scenarios_name[i]); fm_all <- bind_rows(fm_all, fm_temp)
        popn_temp <- safe_array_to_df(RepOut_list[[i]]@popN) %>% mutate(scenario = scenarios_name[i]) %>% rename(N = data); popn_all <- bind_rows(popn_all, popn_temp)
        mat_temp <- safe_array_to_df(ParOut_list[[i]]@mat) %>% arrange(age, season) %>% mutate(scenario = scenarios_name[i], age_new = row_number()) %>% select(age = age_new, maturity = data, scenario); mat_all <- bind_rows(mat_all, mat_temp)
        m_temp <- safe_array_to_df(RepOut_list[[i]]@m_at_age) %>% arrange(age, season) %>% mutate(scenario = scenarios_name[i], age_new = row_number()) %>% select(age = age_new, M = data, scenario); m_all <- bind_rows(m_all, m_temp)
      }

      area_data <- fm_all %>%
        left_join(popn_all, by = c("age", "year", "unit", "season", "area", "iter", "scenario")) %>%
        mutate(age_num = as.numeric(as.character(age))) %>%
        left_join(mat_all, by = c("age_num" = "age", "scenario")) %>%
        left_join(m_all, by = c("age_num" = "age", "scenario")) %>%
        mutate(catch = data * N) %>%
        group_by(year, season, area, scenario) %>%
        summarise(catch = sum(catch, na.rm = TRUE), N = sum(N, na.rm = TRUE), .groups = "drop") %>%
        mutate(F_season = -log(1 - pmin(catch / N, 0.99)))

      area_contribution <- area_data %>%
        group_by(year, area, scenario) %>%
        summarise(weighted_F = sum(F_season * N, na.rm = TRUE), .groups = "drop") %>%
        group_by(year, scenario) %>%
        mutate(total_weighted_F = sum(weighted_F), contribution = (weighted_F / total_weighted_F) * 100) %>%
        ungroup()

      area_contribution$scenario <- factor(area_contribution$scenario, levels = unique(area_contribution$scenario))
      area_contribution$area <- factor(area_contribution$area)

      return(
        ggplot(area_contribution, aes(x = year, y = contribution, fill = area)) +
          geom_area(position = "stack", alpha = 0.8) +
          scale_fill_viridis_d(name = "Area") +
          facet_wrap(~ scenario, ncol = facet_ncol) +
          labs(x = "Year", y = "Contribution to Total F (%)") +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), strip.text = element_text(size = 10, face = "bold"))
      )
    }

    if (plot_type %in% c("sp_combined", "tb_combined")) {
      use_with <- if (plot_type == "sp_combined") "adultBiomass" else "totalBiomass"
      use_no <- if (plot_type == "sp_combined") "adultBiomass_nofish" else "totalBiomass_nofish"
      y_label <- if (plot_type == "sp_combined") bquote("Spawning Potential (" * 10^3 * " MT)") else bquote("Total Biomass (" * 10^3 * " MT)")

      scenarios_name <- names(RepOut_list)
      bioNoFish_all <- data.frame(); bioFish_all <- data.frame()

      for (i in seq_along(RepOut_list)) {
        bioNoFish_temp <- safe_array_to_df(slot(RepOut_list[[i]], use_no)) %>% mutate(scenario = scenarios_name[i])
        bioNoFish_all <- bind_rows(bioNoFish_all, bioNoFish_temp)
      }
      for (i in seq_along(RepOut_list)) {
        bioFish_temp <- safe_array_to_df(slot(RepOut_list[[i]], use_with)) %>% mutate(scenario = scenarios_name[i])
        bioFish_all <- bind_rows(bioFish_all, bioFish_temp)
      }

      bioNoFish_yearly <- bioNoFish_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / n_distinct(season) / 1e3, .groups = "drop")
      bioFish_yearly <- bioFish_all %>% group_by(year, area, scenario) %>% summarise(data = sum(data) / n_distinct(season) / 1e3, .groups = "drop")

      bioNoFish_total <- bioNoFish_all %>% group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>% mutate(area = "All")
      bioFish_total <- bioFish_all %>% group_by(year, season, scenario) %>% summarise(data = sum(data), .groups = "drop") %>% group_by(year, scenario) %>% summarise(data = mean(data) / 1e3, .groups = "drop") %>% mutate(area = "All")

      bioNoFish_combined <- bind_rows(bioNoFish_yearly, bioNoFish_total) %>% mutate(type = "No fishing")
      bioFish_combined <- bind_rows(bioFish_yearly, bioFish_total) %>% mutate(type = "Fished")
      bio_combined <- bind_rows(bioNoFish_combined, bioFish_combined)

      scenario_levels <- unique(bio_combined$scenario)
      scenario_colors <- get_scenario_colors(scenario_levels)
      bio_combined$scenario <- factor(bio_combined$scenario, levels = scenario_levels)
      bio_combined$type <- factor(bio_combined$type, levels = c("No fishing", "Fished"))

      bio_plot <- ggplot(bio_combined, aes(x = year, y = data, color = scenario, linetype = type)) +
        geom_line(linewidth = config$linewidth, alpha = config$alpha) +
        scale_color_manual(values = scenario_colors) +
        scale_linetype_manual(name = "Status", values = c("No fishing" = "dashed", "Fished" = "solid")) +
        coord_cartesian(ylim = c(0, NA)) +
        facet_wrap(~ area, scales = "free_y", ncol = facet_ncol) +
        labs(x = "Year", y = y_label) +
        theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 10, face = "bold"))

      scenario_legend <- cowplot::get_legend(
        ggplot(bio_combined, aes(x = year, y = data, color = scenario)) + geom_line(linewidth = 2) +
          scale_color_manual(values = scenario_colors) + theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm")) +
          labs(color = "Model") + guides(color = guide_legend(override.aes = list(linewidth = 2)))
      )

      status_legend <- cowplot::get_legend(
        ggplot(bio_combined, aes(x = year, y = data, linetype = type)) + geom_line(linewidth = 2) +
          scale_linetype_manual(name = "Status", values = c("No fishing" = "dashed", "Fished" = "solid")) +
          guides(linetype = guide_legend(override.aes = list(linewidth = 1.5))) +
          theme_bw() +
          theme(legend.position = "right", legend.title = element_text(face = "bold"), legend.key.width = unit(2.5, "cm"))
      )

      combined_legend <- cowplot::plot_grid(scenario_legend, status_legend, ncol = 1)
      return(cowplot::plot_grid(bio_plot, combined_legend, ncol = 2, rel_widths = c(4, 1)))
    }

    ggplot() + theme_void()
  })


  output$harvest_plot_output <- renderPlot({
    harvest_plot_reactive()
  })
  mod_sections_download("harvest", "Key Quantities Plot", harvest_plot_reactive, input, session, output)
}

mod_tagging_ui <- function() {
  tabItem(
    tabName = "tagging",
    h2("Tagging Dynamics", style = "color: #39cccc;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "info",
        pickerInput("tag_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        conditionalPanel(
          condition = "input.tag_plot != 'report'",
          radioButtons(
            "tag_time_mode",
            "Time axis:",
            choices = c(
              "By year" = "year",
              "By model step" = "step"
            ),
            selected = "year"
          )
        ),
        conditionalPanel(
          condition = "input.tag_plot == 'returns_all' || input.tag_plot == 'returns_group'",
          pickerInput(
            "tag_years",
            "Years:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              selectAllText = "Select All",
              deselectAllText = "Deselect All",
              selectedTextFormat = "count > 3",
              countSelectedText = "{0} years selected",
              liveSearch = TRUE,
              liveSearchPlaceholder = "Search years..."
            )
          )
        ),
        numericInput("tag_facet_ncol", "Facet columns:", value = 4, min = 1, max = 6, step = 1),
        conditionalPanel(
          condition = "input.tag_plot == 'report'",
          checkboxInput("tag_rr_nonneg_only", "Tag RR filter: exclude rr <= 0", value = FALSE)
        ),
        selectInput("tag_plot", "Plot:", choices = c(
          "Tag Reporting Rates by Group" = "report",
          "Tag Returns Over Time (All Combined)" = "returns_all",
          "Tag Returns by Recapture Group" = "returns_group",
          "Tag Attrition (All Fisheries Combined)" = "attr_all",
          "Tag Attrition (By Program)" = "attr_program",
          "Tag Attrition (By Region)" = "attr_region"
        )),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_tagging_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(title = "Plot", width = 9, solidHeader = TRUE, status = "info", collapsible = TRUE,
          plotOutput("tagging_plot_output", height = "730px"))
    )
  )
}

mod_tagging_server <- function(input, output, session, rv) {
  observeEvent(rv$data_loaded, {
    sc <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    updatePickerInput(session, "tag_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observe({
    req(rv$data_loaded)
    sc <- input$tag_scenarios
    if (is.null(sc) || length(sc) == 0) return()
    yrs <- unlist(lapply(sc, function(m) {
      yr <- rv$YearRanges[[m]]
      c(as.numeric(yr$minYear), as.numeric(yr$maxYear))
    }))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) == 0) return()
    y_min <- floor(min(yrs))
    y_max <- ceiling(max(yrs))
    choices <- as.character(seq(y_min, y_max))
    cur <- isolate(input$tag_years)
    if (is.null(cur) || length(cur) == 0) cur <- choices
    cur <- intersect(cur, choices)
    if (length(cur) == 0) cur <- choices
    updatePickerInput(session, "tag_years", choices = choices, selected = cur)
  })

  tagging_plot_reactive <- reactive({
    req(rv$data_loaded, input$tag_scenarios)
    scenarios_name <- input$tag_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    time_mode <- if (is.null(input$tag_time_mode)) "year" else input$tag_time_mode
    mode <- if (is.null(input$tag_plot)) "report" else input$tag_plot
    rr_nonneg_only <- isTRUE(input$tag_rr_nonneg_only)

    # Tagging plots now use the Scenarios picker directly:
    # 1 selected = single-model display, 2+ selected = overlay.
    selected_models <- intersect(scenarios_name, names(rv$ParOut_list))
    if (length(selected_models) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No valid models selected"))

    has_tagtemp <- names(rv$TagTempOut_list)[!sapply(rv$TagTempOut_list, is.null)]
    has_tagout <- names(rv$TagOut_list)[!sapply(rv$TagOut_list, is.null)]
    in_scope <- intersect(scenarios_name, names(rv$ParOut_list))

    if (mode %in% c("returns_all", "returns_group")) {
      candidate <- intersect(in_scope, has_tagtemp)
      selected_models <- intersect(selected_models, candidate)
    }

    if (mode %in% c("attr_all", "attr_program", "attr_region")) {
      candidate <- intersect(in_scope, intersect(has_tagtemp, has_tagout))
      selected_models <- intersect(selected_models, candidate)
    }

    if (length(selected_models) == 0 || is.na(selected_models[1])) {
      return(
        ggplot() + theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "Required tag data files are missing for selected model(s)")
      )
    }

    overlay <- length(selected_models) > 1
    facet_ncol <- suppressWarnings(as.integer(input$tag_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
    facet_ncol <- min(max(facet_ncol, 1), 6)

    ParOut_list <- subset_named(rv$ParOut_list, selected_models)
    TagRepOut_list <- subset_named(rv$TagRepOut_list, selected_models)
    TagOut_list <- subset_named(rv$TagOut_list, selected_models)
    TagTempOut_list <- subset_named(rv$TagTempOut_list, selected_models)
    seasons_per_model <- pm_get_seasons_per_model(subset_named(rv$ParOut_list, selected_models), fallback = 4)
    fishery_map <- rv$FISHERY_MAPS[[selected_models[1]]]
    scenario_colors <- get_scenario_colors(selected_models)

    is_compatible_by <- function(df, key_col) {
      if (!overlay) return(TRUE)
      sets <- split(df[[key_col]], df$Model)
      sets <- lapply(sets, function(x) sort(unique(as.character(x[!is.na(x)]))))
      if (length(sets) <= 1) return(TRUE)
      all(vapply(sets[-1], function(s) identical(s, sets[[1]]), logical(1)))
    }

    add_time_x <- function(df, x_col = "recap_ts") {
      if (nrow(df) == 0) return(df)
      if (identical(time_mode, "step")) {
        df$x <- df[[x_col]]
        df$x_label <- if (identical(x_col, "recap_ts")) "Time (model steps)" else "Periods at liberty (model steps)"
        return(df)
      }

      if (identical(x_col, "recap_ts")) {
        df$x <- floor(as.numeric(df$recap_ts))
        out <- df %>% group_by(Model, x, across(any_of(c("tag_recapture_name")))) %>%
          summarise(recap_obs = sum(recap_obs, na.rm = TRUE), recap_pred = sum(recap_pred, na.rm = TRUE), .groups = "drop")
        out$x_label <- "Time (year)"
        return(out)
      }

      spm <- seasons_per_model[as.character(df$Model)]
      spm[!is.finite(spm) | spm <= 0] <- 4
      df$x <- floor(as.numeric(df$period_at_liberty) / as.numeric(spm))
      out <- df %>% group_by(Model, x, across(any_of(c("program", "recap.region")))) %>%
        summarise(recap_obs = sum(recap_obs, na.rm = TRUE), recap_pred = sum(recap_pred, na.rm = TRUE), .groups = "drop")
      out$x_label <- "Years at liberty"
      out
    }

    apply_year_filter <- function(df) {
      if (!identical(time_mode, "year")) return(df)
      # Apply year filter only for calendar-time series (not years-at-liberty).
      if (!("x_label" %in% names(df)) || !all(df$x_label == "Time (year)")) return(df)
      yrs <- suppressWarnings(as.numeric(input$tag_years))
      if (is.null(yrs) || length(yrs) == 0 || all(!is.finite(yrs))) return(df)
      df %>% filter(x %in% yrs)
    }

    if (mode == "report") {
      get_reporting_group_labels <- function(model_name) {
        tag_map_df <- NULL
        if (!is.null(rv$tag_rep_map_dfs) && model_name %in% names(rv$tag_rep_map_dfs)) {
          tag_map_df <- rv$tag_rep_map_dfs[[model_name]]
        }
        if (!is.data.frame(tag_map_df) || nrow(tag_map_df) == 0) {
          scenario_dir <- tryCatch(file.path(input$model_dir, model_name), error = function(e) NULL)
          tag_map_r <- find_tag_rep_map_script(scenario_dir)
          tag_map_df <- load_tag_rep_map_from_r(tag_map_r)
        }
        if (is.data.frame(tag_map_df) && nrow(tag_map_df) > 0) {
          return(
            tag_map_df %>%
              transmute(
                group = as.numeric(tag_recapture_group),
                group_name = as.character(tag_recapture_name)
              ) %>%
              filter(is.finite(group), !is.na(group_name), nzchar(group_name)) %>%
              arrange(group)
          )
        }

        fmap <- rv$FISHERY_MAPS[[model_name]]
        if (is.null(fmap) || !is.data.frame(fmap)) {
          return(data.frame(group = numeric(0), group_name = character(0), stringsAsFactors = FALSE))
        }

        by_recapture <- fmap %>%
          filter(!is.na(tag_recapture_group)) %>%
          mutate(
            group = as.numeric(tag_recapture_group),
            group_name = as.character(tag_recapture_name),
            fallback_name = as.character(fishery_name)
          ) %>%
          mutate(group_name = if_else(is.na(group_name) | !nzchar(group_name), fallback_name, group_name)) %>%
          select(group, group_name)

        by_fishery <- fmap %>%
          filter(!is.na(fishery), !is.na(fishery_name), nzchar(fishery_name)) %>%
          transmute(group = as.numeric(fishery), group_name = as.character(fishery_name))

        bind_rows(by_recapture, by_fishery) %>%
          filter(is.finite(group)) %>%
          filter(!is.na(group_name), nzchar(group_name)) %>%
          arrange(group) %>%
          group_by(group) %>%
          summarise(group_name = first(group_name), .groups = "drop") %>%
          group_by(group_name) %>%
          mutate(
            name_idx = row_number(),
            name_n = dplyr::n(),
            group_name = if_else(name_n > 1, paste0(group_name, "-", name_idx), group_name)
          ) %>%
          ungroup() %>%
          select(group, group_name)
      }

      tag_rr_list <- list()
      for (i in seq_along(ParOut_list)) {
        model_name <- selected_models[i]
        group_labels <- get_reporting_group_labels(model_name)
        upper.bound <- tryCatch(subset(flags(ParOut_list[[i]]), flagtype == 1 & flag == 33)$value[1] / 100, error = function(e) 1)
        tag_dt <- data.frame(
          group = c(tag_fish_rep_grp(ParOut_list[[i]])),
          rr = c(tag_fish_rep_rate(ParOut_list[[i]])),
          prior_mean = c(tag_fish_rep_target(ParOut_list[[i]]) / 100),
          prior_sd = c(sqrt(1 / (2 * tag_fish_rep_pen(ParOut_list[[i]]))))
        ) %>% unique() %>% arrange(group) %>%
          left_join(group_labels, by = "group") %>%
          mutate(
            scenario = model_name,
            upper_bound = upper.bound,
            names = group_name
          ) %>%
          filter(!is.na(names), nzchar(names)) %>%
          select(-group_name)
        tag_rr_list[[model_name]] <- tag_dt
      }

      tag_rr_all <- bind_rows(tag_rr_list, .id = "Model")
      if (rr_nonneg_only) {
        tag_rr_all <- tag_rr_all %>% filter(is.finite(rr), rr > 0)
      }
      if (nrow(tag_rr_all) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No tag reporting-rate data"))

      x_seq <- seq(0, 1, length.out = 500)
      prior_curve <- bind_rows(lapply(seq_len(nrow(tag_rr_all)), function(i) {
        data.frame(Model = tag_rr_all$Model[i], group = tag_rr_all$group[i], names = tag_rr_all$names[i], x = x_seq,
                   density = dnorm(x_seq, mean = tag_rr_all$prior_mean[i], sd = tag_rr_all$prior_sd[i]))
      }))
      tag_rr_all <- tag_rr_all %>% mutate(panel = paste0("G", group))
      prior_curve <- prior_curve %>% mutate(panel = paste0("G", group))
      panel_labels <- tag_rr_all %>%
        distinct(panel, names) %>%
        { stats::setNames(.$names, .$panel) }

      compatible <- is_compatible_by(tag_rr_all, "names")
      if (!overlay || compatible) {
        p <- ggplot() +
          geom_line(data = prior_curve, aes(x = x, y = density), color = "black", linewidth = 1) +
          geom_vline(data = tag_rr_all, aes(xintercept = rr, color = Model), linewidth = 1.1) +
          geom_vline(data = tag_rr_all, aes(xintercept = upper_bound), color = "blue", linewidth = 0.9, linetype = "dashed") +
          coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol, labeller = as_labeller(panel_labels)) +
          scale_color_manual(values = scenario_colors, drop = FALSE) +
          labs(x = "Reporting rate", y = "Density",
               title = if (overlay) "Tag Reporting Rates (Overlay)" else paste("Tag Reporting Rates -", selected_models[1]),
               color = "Model") +
          theme_bw() +
          theme(strip.text = element_text(size = 9, face = "bold"), strip.background = element_rect(fill = "gray90"), panel.grid.minor = element_blank(),
                legend.position = if (overlay) "bottom" else "none")
        return(p)
      }

      tag_rr_all <- tag_rr_all %>% mutate(panel = paste(Model, panel, sep = " | "))
      prior_curve <- prior_curve %>% mutate(panel = paste(Model, panel, sep = " | "))
      return(
        ggplot() +
          geom_line(data = prior_curve, aes(x = x, y = density), color = "black", linewidth = 1) +
          geom_vline(data = tag_rr_all, aes(xintercept = rr), linewidth = 1.1, color = "#2c7fb8") +
          geom_vline(data = tag_rr_all, aes(xintercept = upper_bound), color = "blue", linewidth = 0.9, linetype = "dashed") +
          coord_cartesian(xlim = c(0, 1), ylim = c(0, NA)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
          labs(x = "Reporting rate", y = "Density", title = "Tag Reporting Rates (Model-specific panels; incompatible groups)") +
          theme_bw() +
          theme(strip.text = element_text(size = 8, face = "bold"), strip.background = element_rect(fill = "gray90"), panel.grid.minor = element_blank(), legend.position = "none")
      )
    }

    tagtemp_nonnull <- TagTempOut_list[!sapply(TagTempOut_list, is.null)]
    if (length(tagtemp_nonnull) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No temporary_tag_report loaded"))
    scenarios_name <- names(tagtemp_nonnull)
    seasons_per_model <- seasons_per_model[scenarios_name]

    if (mode == "returns_all") {
      mixing_periods <- pm_get_mixing_periods(
        scenarios_name,
        info_list = subset_named(rv$Info_list, scenarios_name),
        config_path = c(file.path("config", "mixing_periods.csv"), file.path("..", "config", "mixing_periods.csv"))
      )
      tag_all <- pm_apply_mixing_filter(tag_temp_out_list = tagtemp_nonnull, mixing_periods = mixing_periods, seasons_per_model = seasons_per_model)

      tag_summary <- tag_all %>%
        group_by(Model, recap_ts) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), mixing_period = first(mixing_period), .groups = "drop")

      tag_summary <- pm_pad_model_time(
        data = tag_summary, models = scenarios_name, seasons_per_model = seasons_per_model,
        time_col = "recap_ts", value_cols = c("recap_obs", "recap_pred")
      ) %>% left_join(data.frame(Model = names(mixing_periods), mixing_period = mixing_periods), by = "Model")
      tag_summary <- add_time_x(tag_summary, "recap_ts")
      tag_summary <- apply_year_filter(tag_summary)

      if (overlay) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(aes(y = recap_obs), na.rm = TRUE, size = 1.2, alpha = 0.6) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.9) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = "Tag Returns Over Time (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.9, color = "#2c7fb8") +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = paste("Tag Returns Over Time -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none")
      )
    }

    if (mode == "returns_group") {
      mixing_periods <- pm_get_mixing_periods(
        scenarios_name,
        info_list = subset_named(rv$Info_list, scenarios_name),
        config_path = c(file.path("config", "mixing_periods.csv"), file.path("..", "config", "mixing_periods.csv"))
      )
      tag_recapture_map <- bind_rows(lapply(scenarios_name, function(m) {
        pm_build_tag_recapture_map(rv$FISHERY_MAPS[[m]], include_index = FALSE) %>% mutate(Model = m)
      }))
      tag_all <- pm_apply_mixing_filter(tag_temp_out_list = tagtemp_nonnull, mixing_periods = mixing_periods, seasons_per_model = seasons_per_model)
      tag_all <- tag_all %>% left_join(tag_recapture_map, by = c("Model", "recap.fishery" = "fishery"))

      tag_summary <- tag_all %>%
        filter(!is.na(tag_recapture_group)) %>%
        group_by(Model, tag_recapture_group, tag_recapture_name, recap_ts) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      time_grid <- pm_pad_model_time(
        data = tag_summary, models = scenarios_name, seasons_per_model = seasons_per_model,
        time_col = "recap_ts", value_cols = c("recap_obs", "recap_pred")
      )

      all_groups <- unique(tag_summary$tag_recapture_group)
      all_names <- tag_recapture_map %>% select(Model, tag_recapture_group, tag_recapture_name) %>% distinct()

      tag_summary <- time_grid %>%
        select(Model, recap_ts) %>%
        tidyr::crossing(tag_recapture_group = all_groups) %>%
        left_join(all_names, by = c("Model", "tag_recapture_group")) %>%
        left_join(tag_summary, by = c("Model", "tag_recapture_group", "tag_recapture_name", "recap_ts")) %>%
        mutate(recap_obs = replace_na(recap_obs, 0), recap_pred = replace_na(recap_pred, 0), Model = factor(Model, levels = scenarios_name))
      tag_summary <- tag_summary %>% filter(!is.na(tag_recapture_name))
      tag_summary <- add_time_x(tag_summary, "recap_ts")
      tag_summary <- apply_year_filter(tag_summary)

      compatible <- is_compatible_by(tag_summary, "tag_recapture_name")
      if (overlay && compatible) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(aes(y = recap_obs), na.rm = TRUE, size = 0.9, alpha = 0.6) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7) +
            facet_wrap(~ tag_recapture_name, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = "Tag Returns by Recapture Group (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"),
                  strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "bottom")
        )
      }

      if (overlay && !compatible) {
        tag_summary <- tag_summary %>% mutate(panel = paste(Model, tag_recapture_name, sep = " | "))
        return(
          ggplot(tag_summary, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 0.9, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = "Tag Returns by Recapture Group (Model-specific panels; incompatible groups)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"),
                  strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "none")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.7, color = "#2c7fb8") +
          facet_wrap(~ tag_recapture_name, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures", title = paste("Tag Returns by Recapture Group -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 8), legend.position = "none")
      )
    }

    tag_releases <- pm_get_tag_releases(subset_named(TagOut_list, scenarios_name))

    if (mode == "attr_all") {
      tag_all <- pm_add_period_at_liberty(
        tag_data = bind_rows(tagtemp_nonnull, .id = "Model"),
        tag_releases = tag_releases,
        seasons_per_model = seasons_per_model
      )

      tag_summary <- tag_all %>% group_by(Model, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary <- pm_pad_period_series(
        data = tag_summary, models = scenarios_name, group_cols = character(0),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary <- add_time_x(tag_summary, "period_at_liberty")
      tag_summary <- apply_year_filter(tag_summary)

      tag_obs_only <- pm_build_obs_reference(tag_summary, keys = c("x"), obs_col = "recap_obs", fun = median)

      if (overlay) {
        return(
          ggplot(tag_summary, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 1) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = "Tag Attrition (Overlay)", color = "Model") +
            theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom")
        )
      }

      return(
        ggplot(tag_summary, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 2, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 1, color = "#2c7fb8") +
          labs(x = unique(tag_summary$x_label)[1], y = "Tag recaptures (all fisheries combined)", title = paste("Tag Attrition -", selected_models[1])) +
          theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none")
      )
    }

    if (mode == "attr_program") {
      tag_all <- pm_add_period_at_liberty(
        tag_data = bind_rows(tagtemp_nonnull, .id = "Model"),
        tag_releases = tag_releases,
        seasons_per_model = seasons_per_model
      )

      tag_summary_program <- tag_all %>%
        group_by(Model, program, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary_program <- pm_pad_period_series(
        data = tag_summary_program, models = scenarios_name, group_cols = c("program"),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary_program <- add_time_x(tag_summary_program, "period_at_liberty")
      tag_summary_program <- apply_year_filter(tag_summary_program)

      tag_obs_only <- pm_build_obs_reference(tag_summary_program, keys = c("program", "x"), obs_col = "recap_obs", fun = median)

      compatible <- is_compatible_by(tag_summary_program, "program")
      if (overlay && compatible) {
        return(
          ggplot(tag_summary_program, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8) +
            facet_wrap(~ program, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Program (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      if (overlay && !compatible) {
        tag_summary_program <- tag_summary_program %>% mutate(panel = paste(Model, program, sep = " | "))
        return(
          ggplot(tag_summary_program, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Program (Model-specific panels; incompatible programs)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(tag_summary_program, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
          facet_wrap(~ program, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary_program$x_label)[1], y = "Tag recaptures", title = paste("Tag Attrition by Program -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    if (mode == "attr_region") {
      tag_all <- bind_rows(tagtemp_nonnull, .id = "Model")
      region_map <- bind_rows(lapply(scenarios_name, function(m) {
        rv$FISHERY_MAPS[[m]][, c("fishery", "region")] %>% mutate(Model = m)
      }))
      tag_all <- tag_all %>% left_join(region_map, by = c("Model", "recap.fishery" = "fishery")) %>% rename(recap.region = region)
      tag_all <- pm_add_period_at_liberty(tag_data = tag_all, tag_releases = tag_releases, seasons_per_model = seasons_per_model)

      tag_summary_region <- tag_all %>%
        group_by(Model, recap.region, period_at_liberty) %>%
        summarise(recap_obs = sum(recap.obs, na.rm = TRUE), recap_pred = sum(recap.pred, na.rm = TRUE), .groups = "drop")

      tag_summary_region <- pm_pad_period_series(
        data = tag_summary_region, models = scenarios_name, group_cols = c("recap.region"),
        period_col = "period_at_liberty", value_cols = c("recap_obs", "recap_pred")
      )
      tag_summary_region <- add_time_x(tag_summary_region, "period_at_liberty")
      tag_summary_region <- apply_year_filter(tag_summary_region)

      tag_obs_only <- pm_build_obs_reference(tag_summary_region, keys = c("recap.region", "x"), obs_col = "recap_obs", fun = median)
      compatible <- is_compatible_by(tag_summary_region, "recap.region")

      if (overlay && compatible) {
        return(
          ggplot(tag_summary_region, aes(x = x, color = Model)) +
            geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.6, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8) +
            facet_wrap(~ recap.region, scales = "free_y", ncol = facet_ncol) +
            scale_color_manual(values = scenario_colors, drop = FALSE) +
            labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Recapture Region (Overlay)", color = "Model") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      if (overlay && !compatible) {
        tag_summary_region <- tag_summary_region %>% mutate(panel = paste(Model, recap.region, sep = " | "))
        return(
          ggplot(tag_summary_region, aes(x = x)) +
            geom_point(aes(y = recap_obs), color = "red", fill = "red", na.rm = TRUE, size = 1.2, alpha = 0.7, shape = 21) +
            geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
            facet_wrap(~ panel, scales = "free_y", ncol = facet_ncol) +
            labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = "Tag Attrition by Recapture Region (Model-specific panels; incompatible regions)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(tag_summary_region, aes(x = x)) +
          geom_point(data = tag_obs_only, aes(x = x, y = recap_obs), inherit.aes = FALSE, color = "red", fill = "red", na.rm = TRUE, size = 1.5, alpha = 0.7, shape = 21) +
          geom_line(aes(y = recap_pred), na.rm = TRUE, linewidth = 0.8, color = "#2c7fb8") +
          facet_wrap(~ recap.region, scales = "free_y", ncol = facet_ncol) +
          labs(x = unique(tag_summary_region$x_label)[1], y = "Tag recaptures", title = paste("Tag Attrition by Recapture Region -", selected_models[1])) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "none", strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    ggplot() + theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = paste("Unknown tag plot mode:", mode))
  })


  output$tagging_plot_output <- renderPlot({
    tagging_plot_reactive()
  })
  mod_sections_download("tagging", "Tagging Dynamics Plot", tagging_plot_reactive, input, session, output)
}

mod_fishery_process_ui <- function() {
  tabItem(
    tabName = "fishery_process",
    h2("Fishery Process Dynamics", style = "color: #f39c12;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "warning",
        pickerInput("fishery_process_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("fishery_process_plot", "Plot:", choices = c(
          "Fishery Selectivity (Age)" = "selectivity_age",
          "Fishery Selectivity (Length)" = "selectivity_length",
          "Fishery Selectivity (Weight)" = "selectivity_weight",
          "Regional Movement Matrix" = "movement"
        )),
        pickerInput(
          "fishery_process_fisheries",
          "Fisheries:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            selectedTextFormat = "count > 2",
            countSelectedText = "{0} fisheries selected",
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search fisheries..."
          )
        ),
        numericInput("fishery_process_facet_ncol", "Facet columns:", value = 4, min = 1, max = 12, step = 1),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_fishery_process_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(title = "Plot", width = 9, solidHeader = TRUE, status = "warning", collapsible = TRUE,
          plotOutput("fishery_process_plot_output", height = "730px"))
    )
  )
}

mod_fishery_process_server <- function(input, output, session, rv) {
  observeEvent(rv$data_loaded, {
    sc <- names(rv$FISHERY_MAPS)[!vapply(rv$FISHERY_MAPS, is.null, logical(1))]
    updatePickerInput(session, "fishery_process_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  observeEvent(list(input$fishery_process_scenarios, input$fishery_process_plot, rv$data_loaded), {
    req(rv$data_loaded, input$fishery_process_scenarios)
    sc <- input$fishery_process_scenarios
    if (length(sc) == 0) {
      updatePickerInput(session, "fishery_process_fisheries", choices = character(0), selected = character(0))
      return()
    }

    mode <- if (is.null(input$fishery_process_plot)) "selectivity_age" else input$fishery_process_plot
    if (!(mode %in% c("selectivity_age", "selectivity_length", "selectivity_weight"))) {
      updatePickerInput(session, "fishery_process_fisheries", choices = character(0), selected = character(0))
      return()
    }

    fish_ids <- unique(unlist(lapply(sc, function(m) {
      rep_obj <- rv$RepOut_list[[m]]
      if (is.null(rep_obj)) return(numeric(0))
      suppressWarnings(as.numeric(as.character(unique(as.data.frame(sel(rep_obj), drop = TRUE)$unit))))
    })))
    fish_ids <- sort(fish_ids[is.finite(fish_ids)])

    base_map <- rv$FISHERY_MAPS[[sc[1]]]
    fish_labels <- sapply(as.character(fish_ids), function(fid) get_fishery_name(fid, base_map))
    choices <- setNames(as.character(fish_ids), fish_labels)

    cur <- isolate(input$fishery_process_fisheries)
    if (is.null(cur) || length(cur) == 0) cur <- as.character(fish_ids)
    cur <- intersect(cur, as.character(fish_ids))
    if (length(cur) == 0) cur <- as.character(fish_ids)

    updatePickerInput(session, "fishery_process_fisheries", choices = choices, selected = cur)
  }, ignoreInit = FALSE)

  fishery_process_plot_reactive <- reactive({
    req(rv$data_loaded, input$fishery_process_scenarios)
    scenarios_name <- input$fishery_process_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    fishery_map <- rv$FISHERY_MAPS[[scenarios_name[1]]]
    scenario_colors <- get_scenario_colors(scenarios_name)
    mode <- if (is.null(input$fishery_process_plot)) "selectivity_age" else input$fishery_process_plot
    facet_ncol <- suppressWarnings(as.integer(input$fishery_process_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 4
    facet_ncol <- min(max(facet_ncol, 1), 12)

    if (mode %in% c("selectivity_age", "selectivity_length", "selectivity_weight")) {
      selected_fisheries <- suppressWarnings(as.numeric(input$fishery_process_fisheries))
      selected_fisheries <- selected_fisheries[is.finite(selected_fisheries)]

      sel_list <- lapply(names(RepOut_list), function(model_name) {
        tmp_rep <- RepOut_list[[model_name]]
        tmp_sel <- sel(tmp_rep)
        tmp_df <- as.data.frame(tmp_sel, drop = TRUE) %>%
          mutate(age = as.numeric(as.character(age)), fishery = as.numeric(as.character(unit)), selectivity = data, Model = model_name) %>%
          select(Model, fishery, age, selectivity)

        if (mode == "selectivity_length") {
          tmp_laa <- c(aperm(mean_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
          tmp_df$length <- tmp_laa[tmp_df$age]
        }
        if (mode == "selectivity_weight") {
          tmp_waa <- c(aperm(mean_waa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
          tmp_df$weight <- tmp_waa[tmp_df$age]
        }

        tmp_df
      })

      sel_data <- bind_rows(sel_list) %>%
        left_join(fishery_map[, c("fishery", "fishery_name", "group")], by = "fishery") %>%
        mutate(Model = factor(Model, levels = scenarios_name))
      if (length(selected_fisheries) > 0) {
        sel_data <- sel_data %>% filter(fishery %in% selected_fisheries)
      }
      if (nrow(sel_data) == 0) {
        return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No selectivity data for selected fisheries"))
      }

      if (mode == "selectivity_age") {
        return(
          ggplot(sel_data, aes(x = age, y = selectivity, color = Model)) +
            geom_line(linewidth = 1) +
            facet_wrap(~ fishery_name, ncol = facet_ncol, scales = "free_y") +
            scale_color_manual("Model", values = scenario_colors) +
            labs(x = "Age class", y = "Selectivity", title = "Estimated Selectivity by Fishery (Age-based)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }
      if (mode == "selectivity_length") {
        return(
          ggplot(sel_data, aes(x = length, y = selectivity, color = Model)) +
            geom_line(linewidth = 1) +
            facet_wrap(~ fishery_name, ncol = facet_ncol, scales = "free_y") +
            scale_color_manual("Model", values = scenario_colors) +
            labs(x = "Length (cm)", y = "Selectivity", title = "Estimated Selectivity by Fishery (Length-based)") +
            theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
        )
      }

      return(
        ggplot(sel_data, aes(x = weight, y = selectivity, color = Model)) +
          geom_line(linewidth = 1) +
          facet_wrap(~ fishery_name, ncol = facet_ncol, scales = "free_y") +
          scale_color_manual("Model", values = scenario_colors) +
          labs(x = "Weight (kg)", y = "Selectivity", title = "Estimated Selectivity by Fishery (Weight-based)") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9))
      )
    }

    move_all <- bind_rows(lapply(names(ParOut_list), function(model_name) {
      move_array <- diff_coffs_age_period(ParOut_list[[model_name]])
      move_df <- as.data.frame.table(move_array, stringsAsFactors = FALSE)
      colnames(move_df) <- c("from", "to", "age", "period", "value")
      move_df <- move_df %>%
        mutate(from = as.numeric(from), to = as.numeric(to), age = as.numeric(age), period = as.numeric(period), value = as.numeric(value), Model = model_name)
      move_df %>% group_by(Model, period, from, to) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% mutate(Quarter = paste0("Quarter ", period), panel = paste(Model, "-", Quarter))
    }))

    ggplot(move_all, aes(x = to, y = from, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      facet_wrap(~ panel, ncol = facet_ncol) +
      scale_fill_gradientn("Diffusion\nCoefficient", colors = c("royalblue3", "deepskyblue1", "gold", "orange1", "indianred1", "firebrick2", "#AC2020"), limits = c(0, NA)) +
      scale_x_continuous(breaks = sort(unique(move_all$to)), labels = paste0("R", sort(unique(move_all$to)))) +
      scale_y_continuous(breaks = sort(unique(move_all$from)), labels = paste0("R", sort(unique(move_all$from))), trans = "reverse") +
      labs(x = "From Region", y = "To Region", title = "Estimated Regional Movement by Quarter") +
      theme_bw() +
      theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_text(face = "bold", size = 10), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 9), axis.text = element_text(size = 8))
  })


  output$fishery_process_plot_output <- renderPlot({
    fishery_process_plot_reactive()
  })
  mod_sections_download("fishery_process", "Fishery Process Plot", fishery_process_plot_reactive, input, session, output)
}

mod_population_biology_ui <- function() {
  tabItem(
    tabName = "population_biology",
    h2("Population Biology", style = "color: #605ca8;"),
    fluidRow(
      box(
        title = "Settings", width = 3, solidHeader = TRUE, status = "primary",
        pickerInput("population_biology_scenarios", "Models:", choices = NULL, selected = NULL, multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")),
        selectInput("population_biology_plot", "Plot:", choices = c(
          "Stock-Recruitment Relationship" = "srr",
          "Maturity at Age" = "mat",
          "Natural Mortality at Age" = "natm",
          "Growth Curve" = "growth"
        )),
        conditionalPanel(
          condition = "input.population_biology_plot == 'growth'",
          checkboxInput("population_biology_show_growth_band", "Show growth band (LAA +/- 1.96 SD)", value = TRUE)
        ),
        numericInput("population_biology_facet_ncol", "Facet columns:", value = 2, min = 1, max = 12, step = 1),
        shiny::hr(),
        h5("Download Plot", style = "font-weight: bold;"),
        actionButton("show_population_biology_download_modal", "📥 Download Plot...", class = "btn-info", style = "width: 100%;")
      ),
      box(title = "Plot", width = 9, solidHeader = TRUE, status = "primary", collapsible = TRUE,
          plotOutput("population_biology_plot_output", height = "730px"))
    )
  )
}

mod_population_biology_server <- function(input, output, session, rv) {
  observeEvent(rv$data_loaded, {
    sc <- names(rv$ParOut_list)
    updatePickerInput(session, "population_biology_scenarios", choices = sc, selected = sc)
  }, ignoreInit = TRUE)

  population_biology_plot_reactive <- reactive({
    req(rv$data_loaded, input$population_biology_scenarios)
    scenarios_name <- input$population_biology_scenarios
    if (length(scenarios_name) == 0) return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No models selected"))

    RepOut_list <- subset_named(rv$RepOut_list, scenarios_name)
    ParOut_list <- subset_named(rv$ParOut_list, scenarios_name)
    scenario_colors <- get_scenario_colors(scenarios_name)
    mode <- if (is.null(input$population_biology_plot)) "srr" else input$population_biology_plot
    facet_ncol <- suppressWarnings(as.integer(input$population_biology_facet_ncol))
    if (!is.finite(facet_ncol) || facet_ncol < 1) facet_ncol <- 2
    facet_ncol <- min(max(facet_ncol, 1), 12)

    if (mode == "srr") {
      adult_biomass <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        tmp_ab <- areaSums(adultBiomass(RepOut_list[[model_name]]))
        as.data.frame(tmp_ab, drop = TRUE) %>%
          mutate(year = as.numeric(as.character(year)), season = as.numeric(as.character(season)), sb = data, Model = model_name) %>%
          select(Model, year, season, sb)
      }))

      recruitment <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        tmp_rec <- areaSums(popN(RepOut_list[[model_name]])[1, ])
        as.data.frame(tmp_rec, drop = TRUE) %>%
          mutate(year = as.numeric(as.character(year)), season = as.numeric(as.character(season)), rec = data, Model = model_name) %>%
          select(Model, year, season, rec)
      }))

      srr_data <- adult_biomass %>%
        left_join(recruitment, by = c("Model", "year", "season")) %>%
        group_by(Model, year) %>%
        summarise(sb = mean(sb, na.rm = TRUE), rec = sum(rec, na.rm = TRUE), .groups = "drop") %>%
        mutate(Model = factor(Model, levels = scenarios_name))

      bh_data <- bind_rows(lapply(names(RepOut_list), function(model_name) {
        bh_params <- srr(RepOut_list[[model_name]])
        max_sb_model <- max((srr_data %>% filter(Model == model_name))$sb, na.rm = TRUE) * 1.2
        sb_seq <- seq(0, max_sb_model, length = 100)
        a_val <- c(bh_params$a); b_val <- c(bh_params$b)
        data.frame(sb = sb_seq, rec = (sb_seq * a_val) / (b_val + sb_seq), Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      sb_units <- 1000; rec_units <- 1000000

      return(
        ggplot() +
          geom_line(data = bh_data, aes(x = sb / sb_units, y = rec / rec_units), color = "black", linewidth = 1.2) +
          geom_point(data = srr_data, aes(x = sb / sb_units, y = rec / rec_units, fill = year), shape = 21, color = "black", size = 2.5) +
          facet_wrap(~ Model, scales = "free", ncol = facet_ncol) +
          scale_fill_viridis_c("Year", option = "viridis") +
          ylim(c(0, NA)) +
          labs(x = paste0("Adult biomass (mt; ", format(sb_units, big.mark = ",", trim = TRUE, scientific = FALSE), "s)"),
               y = paste0("Recruitment (N; ", format(rec_units, big.mark = ",", trim = TRUE, scientific = FALSE), "s)"),
               title = "Stock-Recruitment Relationship by Model (Annual)") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"), strip.background = element_rect(fill = "gray90"), strip.text = element_text(face = "bold", size = 10))
      )
    }

    if (mode == "mat") {
      mat_age_data <- bind_rows(lapply(seq_along(ParOut_list), function(i) {
        tmp_par <- ParOut_list[[i]]
        model_name <- names(ParOut_list)[i]
        mat_vals <- mat(tmp_par)
        data.frame(age = 1:length(mat_vals), maturity = mat_vals, Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      return(
        ggplot(mat_age_data, aes(x = age, y = maturity, color = Model)) +
          geom_line(linewidth = 1.5) +
          scale_color_manual("Model", values = scenario_colors) +
          coord_cartesian(ylim = c(0, 1.05)) +
          labs(x = "Age class", y = "Maturity", title = "Maturity at Age") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
      )
    }

    if (mode == "natm") {
      m_age_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
        tmp_rep <- RepOut_list[[i]]
        model_name <- names(RepOut_list)[i]
        m_vals <- m_at_age(tmp_rep)
        data.frame(age = 1:length(m_vals), m = m_vals, Model = model_name)
      })) %>% mutate(Model = factor(Model, levels = scenarios_name))

      return(
        ggplot(m_age_data, aes(x = age, y = m, color = Model)) +
          geom_line(linewidth = 1.5) +
          scale_color_manual("Model", values = scenario_colors) +
          ylim(c(0, NA)) +
          labs(x = "Age class", y = "Natural mortality", title = "Natural Mortality at Age") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
      )
    }

    growth_data <- bind_rows(lapply(seq_along(RepOut_list), function(i) {
      tmp_rep <- RepOut_list[[i]]
      model_name <- names(RepOut_list)[i]
      tmp_laa <- c(aperm(mean_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
      tmp_sd_laa <- c(aperm(sd_laa(tmp_rep), c(4, 1, 2, 3, 5, 6)))
      tmp_lower <- tmp_laa - 1.96 * tmp_sd_laa
      tmp_upper <- tmp_laa + 1.96 * tmp_sd_laa
      data.frame(Model = model_name, age = 1:length(tmp_laa), length = tmp_laa, lower = tmp_lower, upper = tmp_upper)
    })) %>% mutate(Model = factor(Model, levels = scenarios_name))

    p_growth <- ggplot(growth_data, aes(x = age, y = length, color = Model, fill = Model))
    if (isTRUE(input$population_biology_show_growth_band)) {
      p_growth <- p_growth + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA)
    }
    p_growth +
      geom_line(linewidth = 1.5) +
      scale_color_manual("Model", values = scenario_colors) +
      scale_fill_manual("Model", values = scenario_colors) +
      labs(x = "Age class", y = "Length (cm)", title = "Model Growth Curve") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25, color = "gray85"), legend.position = "bottom", legend.title = element_text(face = "bold"))
  })


  output$population_biology_plot_output <- renderPlot({
    population_biology_plot_reactive()
  })
  mod_sections_download("population_biology", "Population Biology Plot", population_biology_plot_reactive, input, session, output)
}
