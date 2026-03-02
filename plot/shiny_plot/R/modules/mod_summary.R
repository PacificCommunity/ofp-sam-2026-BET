mod_summary_ui <- function() {
      tabItem(
        tabName = "summary",
        h2("Model Summary", style = "color: #3c8dbc;"),
        
        # Overall summary value boxes
        fluidRow(
          valueBoxOutput("n_models", width = 4),
          valueBoxOutput("total_scenarios", width = 4),
          valueBoxOutput("overall_year_range", width = 4)
        ),
        
        # Model-specific information boxes
        fluidRow(
          box(
            title = "Model-Specific Information",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            collapsible = TRUE,
            uiOutput("model_info_boxes")
          )
        ),
        
        # Model configuration table
        fluidRow(
          box(
            title = "Detailed Model Configuration",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            DTOutput("summary_table")
          )
        )
      )
}

mod_summary_server <- function(input, output, session, rv) {
    # TAB 1: MODEL SUMMARY
    # ===========================================================================
  
    # Render model summary table
    output$summary_table <- renderDT({
      req(rv$data_loaded, input$scenarios)
    
      # Extract parameters from each selected scenario
      params_df <- imap_dfr(rv$ParOut_list[input$scenarios], function(par, model_name) {
        dims <- as.list(par@dimensions)
        year_range <- rv$YearRanges[[model_name]]
        info <- rv$Info_list[[model_name]]
        description <- if (!is.null(info$description) && nzchar(info$description)) info$description else NA_character_
        config_summary <- if (!is.null(info$config_summary) && nzchar(info$config_summary)) info$config_summary else NA_character_
        data.frame(
          Model = model_name,
          Description = description,
          Config_Summary = config_summary,
          Max_Grad = sprintf("%.6f", as.numeric(par@max_grad)),
          Obj_Fun = sprintf("%.2f", as.numeric(par@obj_fun)),
          N_Pars = as.numeric(par@n_pars),
          Fisheries = dims$fisheries,
          Years = paste(year_range$minYear, "-", year_range$maxYear),
          Regions = dims$regions,
          Seasons = dims$seasons
        )
      })
    
      # Display as interactive table
      datatable(params_df, 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
                rownames = FALSE)
    })
  
    # Value box: number of models selected
    output$n_models <- renderValueBox({
      req(rv$data_loaded)
      valueBox(
        length(input$scenarios), "Models Selected", 
        icon = icon("check-square"),
        color = "blue"
      )
    })
  
    # Value box: total scenarios loaded
    output$total_scenarios <- renderValueBox({
      req(rv$data_loaded)
      valueBox(
        length(rv$ParOut_list), "Total Models Loaded", 
        icon = icon("cube"),
        color = "green"
      )
    })
  
    # Value box: overall year range
    output$overall_year_range <- renderValueBox({
      req(rv$data_loaded)
      all_years <- range(unlist(lapply(rv$YearRanges[input$scenarios], 
                                       function(x) c(x$minYear, x$maxYear))))
      valueBox(
        paste(all_years[1], "-", all_years[2]), "Overall Year Range", 
        icon = icon("calendar"),
        color = "yellow"
      )
    })
  
    # Render model-specific info boxes
    output$model_info_boxes <- renderUI({
      req(rv$data_loaded, input$scenarios)
    
      # Create a box for each selected model
      boxes <- lapply(input$scenarios, function(model_name) {
        par <- rv$ParOut_list[[model_name]]
        dims <- as.list(par@dimensions)
        year_range <- rv$YearRanges[[model_name]]
        info <- rv$Info_list[[model_name]]
        description <- if (!is.null(info$description) && nzchar(info$description)) info$description else "No description available"
        config_summary <- if (!is.null(info$config_summary) && nzchar(info$config_summary)) info$config_summary else "No config summary available"
      
        # Count index fisheries
        n_index <- length(rv$INDEX_FISHERIES_MAPS[[model_name]])
      
        # Create info box
        column(
          width = 4,
          box(
            title = model_name,
            width = NULL,
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            tags$div(
              style = "padding: 5px;",
              tags$div(
                style = "margin-bottom: 8px; padding: 8px 10px; background: #f4f8fb; border-left: 3px solid #3c8dbc; border-radius: 3px; font-size: 12px;",
                tags$strong("Description: "),
                description
              ),
              tags$div(
                style = "margin-bottom: 8px; padding: 8px 10px; background: #f8fafc; border-left: 3px solid #6b7280; border-radius: 3px; font-size: 12px;",
                tags$strong("Config summary: "),
                config_summary
              ),
              tags$table(
                style = "width: 100%; font-size: 13px;",
                tags$tr(
                  tags$td(tags$strong("🎣 Fisheries:"), style = "width: 60%;"),
                  tags$td(dims$fisheries, style = "text-align: right;")
                ),
                tags$tr(
                  tags$td(tags$strong("📊 Index Fisheries:"), style = "padding-top: 5px;"),
                  tags$td(n_index, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📅 Years:"), style = "padding-top: 5px;"),
                  tags$td(
                    paste(year_range$minYear, "-", year_range$maxYear),
                    style = "text-align: right; padding-top: 5px;"
                  )
                ),
                tags$tr(
                  tags$td(tags$strong("🗺️ Regions:"), style = "padding-top: 5px;"),
                  tags$td(dims$regions, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📆 Seasons:"), style = "padding-top: 5px;"),
                  tags$td(dims$seasons, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("📈 Parameters:"), style = "padding-top: 5px;"),
                  tags$td(par@n_pars, style = "text-align: right; padding-top: 5px;")
                ),
                tags$tr(
                  tags$td(tags$strong("🎯 Max Gradient:"), style = "padding-top: 5px;"),
                  tags$td(
                    sprintf("%.2e", as.numeric(par@max_grad)),
                    style = "text-align: right; padding-top: 5px;"
                  )
                ),
                tags$tr(
                  tags$td(tags$strong("💰 Obj Function:"), style = "padding-top: 5px;"),
                  tags$td(
                    sprintf("%.2f", as.numeric(par@obj_fun)),
                    style = "text-align: right; padding-top: 5px;"
                  )
                )
              )
            )
          )
        )
      })
    
      # Arrange boxes in rows of 3
      do.call(fluidRow, boxes)
    })
  
    # ===========================================================================

}
