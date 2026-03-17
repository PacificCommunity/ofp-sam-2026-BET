mod_bounds_ui <- function() {
      tabItem(
        tabName = "bounds",
        h2("Parameter Bound Hit Analysis", style = "color: #f39c12;"),
        
        # Overview table
        fluidRow(
          box(
            title = "Overview",
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            DTOutput("bounds_overview")
          )
        ),
        
        # Detailed bound hits table
        fluidRow(
          box(
            title = "Detailed Bound Hits",
            width = 12,
            solidHeader = TRUE,
            status = "danger",
            collapsible = TRUE,
            selectInput("bound_model", "Select Model:", choices = NULL),
            DTOutput("bounds_type_summary"),
            DTOutput("bounds_detail"),
            downloadButton("download_bounds", "Download CSV", class = "btn-info")
          )
        )
      )
}

mod_bounds_server <- function(input, output, session, rv) {
    # TAB 3: BOUND HITS
    # ===========================================================================

    bound_var_type <- function(var_name) {
      var_name <- as.character(var_name)
      var_name <- trimws(var_name)
      var_name <- sub("\\(.*$", "", var_name)
      var_name <- sub("\\[.*$", "", var_name)
      ifelse(nzchar(var_name), var_name, "Unknown")
    }

    parse_prefix <- function(var_name) {
      v <- tolower(trimws(as.character(var_name)))
      p <- sub("\\(.*$", "", v)
      p <- sub(":.*$", "", p)
      trimws(p)
    }

    lookup_param_reference <- function(var_name) {
      prefix <- parse_prefix(var_name)

      if (prefix %in% c("bs_selcoff_gp", "bs_selcoff", "selcoff", "ageselcoff", "sel_dev_coffs")) {
        return(list(
          Description = "Selectivity coefficient/deviate parameter used for fishery selectivity-at-age (including time-block/season/grouped selectivity variants).",
          Source = "MFCL User's Guide §3.4.2; src/variable.hpp and src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("diff_coffs", "diff_coffs2", "diff_coffs3", "xdiff_coffs", "zdiff_coffs")) {
        return(list(
          Description = "Movement diffusion coefficient parameter (including age-dependent/orthogonal-polynomial parameterizations).",
          Source = "MFCL User's Guide §4.5.12; src/variable.hpp and src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("region_rec_diffs", "region_rec_diff_coffs")) {
        return(list(
          Description = "Regional recruitment-deviation coefficients controlling time-varying recruitment distribution among regions.",
          Source = "MFCL User's Guide §3.2.1; src/newmau5a.cpp and src/variable.hpp"
        ))
      }
      if (prefix %in% c("tag_fish_rep", "rep_dev_coffs")) {
        return(list(
          Description = "Tag reporting-rate parameter or its deviation term for tagging likelihood formulations.",
          Source = "MFCL User's Guide §4.5.10; src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("recr")) {
        return(list(
          Description = "Recruitment time-series parameter in the core population dynamics model.",
          Source = "MFCL User's Guide §3.2; src/newmau5a.cpp"
        ))
      }
      if (prefix %in% c("region_pars")) {
        return(list(
          Description = "Region-level recruitment distribution parameters.",
          Source = "MFCL User's Guide §3.2.1; src/newmau5a.cpp and src/newmult.cpp"
        ))
      }
      if (prefix %in% c("vb_coff")) {
        return(list(
          Description = "Von Bertalanffy growth coefficients used in age-length conversions and size-based components.",
          Source = "MFCL User's Guide growth parameterization; src/newmaux5.cpp and src/lbselclc.cpp"
        ))
      }
      if (prefix %in% c("var_coff")) {
        return(list(
          Description = "Size-at-age variability coefficients used in length/weight distribution and selectivity-at-length calculations.",
          Source = "MFCL User's Guide growth/variance settings; src/newmaux5.cpp and src/onevar.cpp"
        ))
      }
      if (prefix %in% c("age_pars")) {
        return(list(
          Description = "Age-structured biological parameter block (growth/mortality related rows depending on flags).",
          Source = "MFCL User's Guide biology section; src/newmaux5.cpp"
        ))
      }
      if (prefix %in% c("sv")) {
        return(list(
          Description = "Structural/process scalar parameter (index-specific role in growth/recruitment/penalties).",
          Source = "MFCL User's Guide sv notes; src/newmaux5.cpp and src/callpen.cpp"
        ))
      }
      if (prefix %in% c("totpop", "totpop_coff")) {
        return(list(
          Description = "Population scaling parameter for initial total population/recruitment scale.",
          Source = "MFCL User's Guide initial population/recruitment; src/newmau5a.cpp (totpop)"
        ))
      }

      list(
        Description = paste0("Estimated MFCL parameter group from indepvar.rpt (group='", prefix, "')."),
        Source = "MFCL indepvar.rpt design + source code parameter vector"
      )
    }
  
    # Reactive: process bound hits data
    bounds_data <- reactive({
      req(rv$data_loaded, input$scenarios)
    
      # Process indepvar.rpt for each scenario
      results <- map(input$scenarios, function(model_name) {
        df <- parse_indepvar(rv$IndepOut_list[[model_name]])
        if (is.null(df)) return(NULL)
      
        # Calculate distances to bounds and identify hit type
        df <- df %>%
          mutate(
            Distance_to_lower = abs(Estimate - L_bound),
            Distance_to_upper = abs(Estimate - U_bound),
            Var_Type = bound_var_type(Var_name),
            Hit_Type = case_when(
              !Hit_Bound ~ "None",
              Distance_to_lower <= Distance_to_upper ~ "Lower",
              TRUE ~ "Upper"
            )
          )
      
        # Filter to only parameters that hit bounds
        bound_hits <- df %>% filter(Hit_Bound)
        if (nrow(bound_hits) > 0) {
          ref_info <- lapply(bound_hits$Var_name, lookup_param_reference)
          bound_hits$Description <- vapply(ref_info, function(x) x$Description, character(1))
          bound_hits$Source <- vapply(ref_info, function(x) x$Source, character(1))
        }
        list(total_params = nrow(df), bound_hits = bound_hits)
      })
      names(results) <- input$scenarios
      Filter(Negate(is.null), results)
    })
  
    # Render bound hits overview table
    output$bounds_overview <- renderDT({
      req(bounds_data())
    
      # Create summary table
      overview <- data.frame(
        Model = names(bounds_data()),
        Total_Params = sapply(bounds_data(), function(x) x$total_params),
        Bound_Hits = sapply(bounds_data(), function(x) nrow(x$bound_hits)),
        Lower_Hits = sapply(bounds_data(), function(x) sum(x$bound_hits$Hit_Type == "Lower", na.rm = TRUE)),
        Upper_Hits = sapply(bounds_data(), function(x) sum(x$bound_hits$Hit_Type == "Upper", na.rm = TRUE)),
        Hit_Rate = sapply(bounds_data(), function(x) 
          sprintf("%.2f%%", nrow(x$bound_hits) / x$total_params * 100))
      )
    
      datatable(overview, 
                options = list(pageLength = 10, dom = 'tip', deferRender = TRUE), 
                rownames = FALSE)
    })

    output$bounds_type_summary <- renderDT({
      req(input$bound_model, bounds_data())

      if (!input$bound_model %in% names(bounds_data())) {
        return(NULL)
      }

      bounds <- bounds_data()[[input$bound_model]]$bound_hits

      if (nrow(bounds) == 0) {
        return(datatable(
          data.frame(Message = "✓ No bound hits detected"),
          options = list(dom = "t", deferRender = TRUE),
          rownames = FALSE
        ))
      }

      summary_tbl <- bounds %>%
        group_by(Var_Type, Hit_Type) %>%
        summarise(
          Count = dplyr::n(),
          Description = dplyr::first(Description),
          Source = dplyr::first(Source),
          .groups = "drop"
        ) %>%
        arrange(Var_Type, match(Hit_Type, c("Lower", "Upper")))

      datatable(
        summary_tbl,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, deferRender = TRUE),
        rownames = FALSE
      ) %>%
        formatStyle(
          "Hit_Type",
          color = styleEqual(
            c("Lower", "Upper"),
            c("#1d4ed8", "#dc2626")
          ),
          fontWeight = "600"
        )
    })
  
    # Render detailed bound hits table
    output$bounds_detail <- renderDT({
      req(input$bound_model, bounds_data())
    
      # Check if data exists for selected model
      if (!input$bound_model %in% names(bounds_data())) {
        return(data.frame(Message = "No data available for this model"))
      }
    
      bounds <- bounds_data()[[input$bound_model]]$bound_hits
    
      # Display message if no bound hits
      if (nrow(bounds) == 0) {
        data.frame(Message = "✓ No bound hits detected")
      } else {
        # Display detailed bound hits
        bounds %>%
          select(Index, Var_Type, Var_name, Estimate, Hit_Type, L_bound, U_bound, Description, Source) %>%
          datatable(options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE), 
                    rownames = FALSE) %>%
          formatStyle(
            "Hit_Type",
            color = styleEqual(
              c("Lower", "Upper"),
              c("#1d4ed8", "#dc2626")
            ),
            fontWeight = "600"
          )
      }
    })
  
    # Download handler for bound hits CSV
    output$download_bounds <- downloadHandler(
      filename = function() {
        paste0("bound_hits_", input$bound_model, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(input$bound_model, bounds_data())
        bounds <- bounds_data()[[input$bound_model]]$bound_hits
        write.csv(bounds, file, row.names = FALSE)
      }
    )
  
    # ===========================================================================

}
