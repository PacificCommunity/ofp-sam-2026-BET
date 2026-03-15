mod_stock_ui <- function() {
      tabItem(
        tabName = "stock",
        h2("Stock Status", style = "color: #00a65a;"),
        
        fluidRow(
          # Settings panel
          box(
            title = "Settings",
            width = 3,
            solidHeader = TRUE,
            status = "success",
            
            # Scenarios selector with dropdown
            pickerInput(
              "stock_scenarios",
              "Models:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                selectedTextFormat = "count > 2",
                countSelectedText = "{0} models selected",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search models...",
                size = 10
              )
            ),
            sliderInput(
              "stock_plot_height",
              "Plot height (px)",
              min = 450,
              max = 1800,
              value = 900,
              step = 50
            ),
            sliderInput(
              "stock_plot_width",
              "Plot width (px)",
              min = 700,
              max = 2200,
              value = 1200,
              step = 50
            ),
            actionButton("stock_apply_filters", "Apply",
                         class = "btn-success",
                         style = "width: 100%;"),
            tags$small("Selections update the plot when you click Apply.",
                       style = "display:block; margin-top:6px; color:#666;"),
            
            shiny::hr(),
            h5("Download Plot", style = "font-weight: bold;"),
            actionButton("show_stock_download_modal", "📥 Download Plot...", 
                         class = "btn-info", 
                         style = "width: 100%;",
                         icon = icon("download")),
            helpText("Select models to display", style = "margin-top: 10px;")
          ),
          
          # Stock status plots
          box(
            title = "Spawning Biomass Depletion & Recruitment",
            width = 9,
            solidHeader = TRUE,
            status = "success",
            collapsible = TRUE,
            uiOutput("stock_plot_ui")
          )
        )
      )
}

mod_stock_server <- function(input, output, session, rv) {
    # TAB 4: STOCK STATUS
    # ===========================================================================
    stock_filters_current <- reactive({
      list(
        scenarios = input$stock_scenarios,
        plot_height = if (is.null(input$stock_plot_height)) 900 else suppressWarnings(as.integer(input$stock_plot_height)),
        plot_width = if (is.null(input$stock_plot_width)) 1200 else suppressWarnings(as.integer(input$stock_plot_width))
      )
    })
    stock_filters_applied <- reactiveVal(NULL)

    observe({
      req(rv$data_loaded)
      pending <- !isTRUE(input$live_update_plots) &&
        !filters_equal(stock_filters_current(), stock_filters_applied())
      set_apply_pending(session, "stock_apply_filters", pending)
    })

    observeEvent(input$stock_apply_filters, {
      stock_filters_applied(isolate(stock_filters_current()))
    }, ignoreInit = TRUE)

    observeEvent(list(input$live_update_plots, input$stock_scenarios, input$stock_plot_height, input$stock_plot_width), {
      req(rv$data_loaded)
      if (!isTRUE(input$live_update_plots)) return()
      if (length(input$stock_scenarios) == 0) return()
      stock_filters_applied(isolate(stock_filters_current()))
    }, ignoreInit = TRUE)

    observeEvent(list(rv$initial_render_nonce, input$stock_scenarios), {
      req(rv$data_loaded, rv$initial_render_nonce)
      if (length(input$stock_scenarios) == 0) return()
      stock_filters_applied(isolate(stock_filters_current()))
    }, ignoreInit = TRUE)
  
    # Reactive: generate stock status plot
    stock_plot_reactive <- reactive({
      filters <- stock_filters_applied()
      req(rv$data_loaded, filters, filters$scenarios)
      scenarios <- filters$scenarios
    
      # Check if any scenarios selected
      if (length(scenarios) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No models selected", size = 6, color = "#999") +
          theme_void()
        return(p)
      }
    
      tryCatch({
        # Extract spawning biomass depletion (SB/SBF0)
        SBdep <- map_dfr(scenarios, function(scenario) {
          rep_obj <- rv$RepOut_list[[scenario]]
          year_range <- rv$YearRanges[[scenario]]
        
          # Calculate SB/SBF0 using adultBiomass slots
          sb <- slot(rep_obj, "adultBiomass")
          sbf0 <- slot(rep_obj, "adultBiomass_nofish")
        
          # Sum across all dimensions except time (dimension 2)
          sb_vec <- apply(sb, 2, sum, na.rm = TRUE)
          sbf0_vec <- apply(sbf0, 2, sum, na.rm = TRUE)
        
          # Calculate depletion ratio
          sb_ratio <- as.numeric(sb_vec / sbf0_vec)
        
          # Generate years
          n_years <- length(sb_ratio)
          years <- year_range$minYear + seq(0, n_years - 1)
        
          data.frame(
            Scenario = scenario,
            Year = years,
            Quant = sb_ratio,
            stringsAsFactors = FALSE
          )
        })
      
        # Extract recruitment
        Rec <- map_dfr(scenarios, function(scenario) {
          rep_obj <- rv$RepOut_list[[scenario]]
          year_range <- rv$YearRanges[[scenario]]
        
          # Get recruitment data
          rec_data <- slot(rep_obj, "eq_rec")
        
          # Sum across all dimensions except time (dimension 2)
          rec_vec <- apply(rec_data, 2, sum, na.rm = TRUE)
          rec_vec <- as.numeric(rec_vec)
        
          # Generate years
          n_years <- length(rec_vec)
          years <- year_range$minYear + seq(0, n_years - 1)
        
          data.frame(
            Scenario = scenario,
            Year = years,
            Quant = rec_vec / 1e6,  # Convert to millions
            stringsAsFactors = FALSE
          )
        })
      
        # Remove invalid values
        SBdep <- SBdep[is.finite(SBdep$Year) & is.finite(SBdep$Quant), ]
        Rec <- Rec[is.finite(Rec$Year) & is.finite(Rec$Quant), ]
      
        # Check if data exists
        if (nrow(SBdep) < 2 || nrow(Rec) < 2) {
          p <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Insufficient data points\nSB data:", nrow(SBdep), 
                                   "points\nRec data:", nrow(Rec), "points"), 
                     size = 6, color = "#999") +
            theme_void()
          return(p)
        }

        min_year <- suppressWarnings(min(SBdep$Year, na.rm = TRUE))
        max_quant <- suppressWarnings(max(SBdep$Quant, na.rm = TRUE))
        if (!is.finite(min_year) || !is.finite(max_quant)) {
          p <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                     label = "No valid stock status data after filtering", 
                     size = 6, color = "#999") +
            theme_void()
          return(p)
        }
      
        # Generate color palette for scenarios
        scenario_colors <- get_scenario_colors(scenarios)
      
        # Plot 1: Spawning Biomass Depletion
        p1 <- ggplot(SBdep, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) +
          scale_color_manual(values = scenario_colors) +
          scale_y_continuous(limits = c(0, max(1, max_quant * 1.05)), 
                             expand = c(0, 0.02)) +
          labs(x = NULL, y = "SB / SB(F=0)") +
          geom_hline(yintercept = 0.2, linetype = "dashed", color = "#d9534f", linewidth = 0.8) +
          geom_hline(yintercept = 0.5, linetype = "dashed", color = "#5cb85c", linewidth = 0.8) +
          annotate("text", x = min_year, y = 0.2, label = "0.2", 
                   vjust = -0.5, hjust = -0.2, size = 3.5, color = "#d9534f") +
          annotate("text", x = min_year, y = 0.5, label = "0.5", 
                   vjust = -0.5, hjust = -0.2, size = 3.5, color = "#5cb85c") +
          theme_bw(base_size = 14) + 
          theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
      
        # Plot 2: Recruitment
        p2 <- ggplot(Rec, aes(x = Year, y = Quant, color = Scenario, group = Scenario)) +
          geom_line(linewidth = 1.2) +
          scale_color_manual(values = scenario_colors) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
          labs(x = "Year", y = "Recruitment (millions)", color = NULL) +
          theme_bw(base_size = 14) + 
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            panel.grid.minor = element_blank()
          ) +
          guides(color = guide_legend(nrow = 1))
      
        # Combine both plots vertically
        plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1.3))
      
      }, error = function(e) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error loading stock status:\n", e$message), 
                   size = 5, color = "red") +
          theme_void()
        return(p)
      })
    })
    stock_plot_reactive <- bindCache(
      stock_plot_reactive,
      rv$data_loaded,
      input$model_dir,
      stock_filters_applied()
    )
    stock_plot_reactive <- bindEvent(stock_plot_reactive, rv$data_loaded, input$stock_apply_filters, stock_filters_applied(), ignoreInit = FALSE)

    output$stock_plot_ui <- renderUI({
      filters <- stock_filters_applied()
      h <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_height)) else suppressWarnings(as.integer(input$stock_plot_height))
      w <- if (!is.null(filters)) suppressWarnings(as.integer(filters$plot_width)) else suppressWarnings(as.integer(input$stock_plot_width))
      if (!is.finite(h)) h <- 900
      if (!is.finite(w)) w <- 1200
      h <- min(max(h, 450), 1800)
      w <- min(max(w, 700), 2200)

      plotOutput("stock_plot", height = paste0(h, "px"), width = paste0(w, "px"))
    })
  
    # Render stock status plot
    output$stock_plot <- renderPlot({
      stock_plot_reactive()
    })
  
    # ===========================================================================

    # STOCK STATUS DOWNLOAD
    # ---------------------------------------------------------------------------

    observeEvent(input$show_stock_download_modal, {
      show_download_modal("stock", "Stock Status Plot", current_save_dir = input$plot_export_dir)
    })

    observeEvent(input$stock_preset_wide, {
      updateNumericInput(session, "stock_width", value = 16)
      updateNumericInput(session, "stock_height", value = 9)
    })

    observeEvent(input$stock_preset_standard, {
      updateNumericInput(session, "stock_width", value = 12)
      updateNumericInput(session, "stock_height", value = 9)
    })

    observeEvent(input$stock_preset_square, {
      updateNumericInput(session, "stock_width", value = 10)
      updateNumericInput(session, "stock_height", value = 10)
    })

    output$stock_download_confirm <- downloadHandler(
      filename = function() {
        format <- input$stock_format
        paste0("stock_status_", Sys.Date(), ".", format)
      },
      content = function(file) {
        p <- stock_plot_reactive()
        width <- input$stock_width
        height <- input$stock_height
        dpi <- as.numeric(input$stock_dpi)
        format <- input$stock_format

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

    register_folder_save_button(
      plot_type = "stock",
      plot_reactive = stock_plot_reactive,
      input = input,
      session = session,
      output = output,
      filename_fun = function() {
        format <- input$stock_format
        paste0("stock_status_", Sys.Date(), ".", format)
      }
    )

}
