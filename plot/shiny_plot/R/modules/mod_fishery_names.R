mod_fishery_names_ui <- function() {
  tabItem(
    tabName = "fishery_names",
    h2("Fishery Names (from fishery_map.R/.r)", style = "color: #17a2b8;"),

    fluidRow(
      box(
        title = "Instructions",
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        HTML("<ul>
              <li>🎯 <strong>Select Model:</strong> Choose which model fishery map to edit</li>
              <li>📝 <strong>Edit names:</strong> Edit the <code>fishery_name</code> column directly</li>
              <li>💾 <strong>Apply changes:</strong> Save edits to the currently loaded model map in app memory</li>
              <li>⚠ <strong>Required:</strong> <code>fishery_map.R</code> or <code>fishery_map.r</code> must exist in each model folder when loading models</li>
            </ul>")
      )
    ),

    fluidRow(
      box(
        title = "Fishery Map Table",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,

        fluidRow(
          column(
            4,
            selectInput("fishery_names_model", "Select Model:", choices = NULL, selected = NULL)
          ),
          column(
            8,
            div(
              style = "padding-top: 25px;",
              tags$span(
                id = "fishery_count_display",
                style = "font-size: 14px; color: #666; font-weight: bold;",
                textOutput("fishery_count_text", inline = TRUE)
              )
            )
          )
        ),

        shiny::hr(),

        uiOutput("fishery_map_warning"),

        div(
          style = "margin-bottom: 15px;",
          actionButton("apply_fishery_names", "💾 Apply Changes", class = "btn-success", icon = icon("check"))
        ),

        DTOutput("fishery_names_table")
      )
    )
  )
}

mod_fishery_names_server <- function(input, output, session, rv) {
  fishery_map_missing_for_selected <- reactive({
    req(rv$data_loaded, input$fishery_names_model)
    missing <- if (!is.null(rv$fishery_map_missing_models)) rv$fishery_map_missing_models else character(0)
    input$fishery_names_model %in% missing
  })

  observeEvent(list(input$tabs, rv$fishery_map_missing_models, rv$data_loaded), {
    req(rv$data_loaded)
    if (!identical(input$tabs, "fishery_names")) return()
    req(input$fishery_names_model)
    if (!fishery_map_missing_for_selected()) return()
    showNotification(
      HTML(paste0(
        "<strong>fishery_map.R / fishery_map.r not found</strong><br/>",
        "Model: ", input$fishery_names_model, "<br/>",
        "Please provide a valid fishery_map.R or fishery_map.r and reload model data."
      )),
      type = "warning",
      duration = 8
    )
  }, ignoreInit = TRUE)

  output$fishery_map_warning <- renderUI({
    req(rv$data_loaded, input$fishery_names_model)
    if (!fishery_map_missing_for_selected()) return(NULL)
    tags$div(
      class = "alert alert-warning",
      style = "margin-bottom: 15px;",
      HTML(paste0(
        "<strong>fishery_map.R / fishery_map.r is missing/invalid for selected model.</strong><br/>",
        "Model: ", input$fishery_names_model, "<br/>",
        "Provide a valid fishery_map.R or fishery_map.r in each model folder and click <strong>Load Data</strong> again."
      ))
    )
  })

  refresh_cpue_choices <- function() {
    if (is.null(input$cpue_scenarios) || length(input$cpue_scenarios) == 0) return()
    all_index_fish <- unique(unlist(rv$INDEX_FISHERIES_MAPS[input$cpue_scenarios]))
    if (length(all_index_fish) == 0) return()

    fishery_map <- rv$FISHERY_MAPS[[input$cpue_scenarios[1]]]
    choices <- setNames(all_index_fish, sapply(all_index_fish, function(x) get_fishery_name(x, fishery_map)))
    updatePickerInput(session, "cpue_fisheries", choices = choices, selected = intersect(isolate(input$cpue_fisheries), all_index_fish))
  }

  refresh_lf_choices <- function() {
    if (is.null(input$lf_model) || is.null(rv$LengOut_list[[input$lf_model]])) return()
    fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    fishery_map <- rv$FISHERY_MAPS[[input$lf_model]]
    choices <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    selected <- isolate(input$lf_fishery)
    if (is.null(selected) || !(selected %in% fisheries)) selected <- fisheries[1]
    updateSelectInput(session, "lf_fishery", choices = choices, selected = selected)

    all_models <- names(rv$LengOut_list)[!sapply(rv$LengOut_list, is.null)]
    compatible_models <- check_lf_compatibility_global(rv, input$lf_model, all_models)
    updatePickerInput(session, "lf_scenarios", choices = compatible_models,
                      selected = intersect(isolate(input$lf_scenarios), compatible_models))
  }

  refresh_wf_choices <- function() {
    if (is.null(input$wf_model) || is.null(rv$WeightOut_list[[input$wf_model]])) return()
    fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    fishery_map <- rv$FISHERY_MAPS[[input$wf_model]]
    choices <- setNames(fisheries, sapply(fisheries, function(x) get_fishery_name(x, fishery_map)))
    selected <- isolate(input$wf_fishery)
    if (is.null(selected) || !(selected %in% fisheries)) selected <- fisheries[1]
    updateSelectInput(session, "wf_fishery", choices = choices, selected = selected)

    all_models <- names(rv$WeightOut_list)[!sapply(rv$WeightOut_list, is.null)]
    compatible_models <- check_wf_compatibility_global(rv, input$wf_model, all_models)
    updatePickerInput(session, "wf_scenarios", choices = compatible_models,
                      selected = intersect(isolate(input$wf_scenarios), compatible_models))
  }

  refresh_dependents <- function() {
    refresh_cpue_choices()
    refresh_lf_choices()
    refresh_wf_choices()
  }

  apply_map_df_to_model <- function(model_name, map_df) {
    rv$FISHERY_MAPS[[model_name]] <- map_df[order(map_df$fishery), , drop = FALSE]
    rv$INDEX_FISHERIES_MAPS[[model_name]] <- detect_index_fisheries(rv$FISHERY_MAPS[[model_name]])
    rv$fishery_names_dfs[[model_name]] <- rv$FISHERY_MAPS[[model_name]][,
      c("fishery", "fishery_name", "group", "region", "tag_recapture_group", "tag_recapture_name")]
  }

  output$fishery_count_text <- renderText({
    req(input$fishery_names_model, rv$fishery_names_dfs)
    if (fishery_map_missing_for_selected()) return("⚠ fishery_map.R/.r missing/invalid for selected model")
    paste0("📊 Total fisheries in this model: ", nrow(rv$fishery_names_dfs[[input$fishery_names_model]]))
  })

  output$fishery_names_table <- renderDT({
    req(rv$data_loaded, input$fishery_names_model, rv$fishery_names_dfs)
    if (fishery_map_missing_for_selected()) {
      return(
        datatable(
          data.frame(Message = paste0("fishery_map.R/.r missing/invalid for model: ", input$fishery_names_model, ". Provide it and reload data.")),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        )
      )
    }
    df <- rv$fishery_names_dfs[[input$fishery_names_model]]

    datatable(
      df,
      editable = list(target = "cell", disable = list(columns = c(0, 2, 3, 4, 5))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "frtip",
        columnDefs = list(
          list(className = "dt-center", targets = 0),
          list(className = "editable-cell", targets = 1)
        )
      ),
      rownames = FALSE
    )
  })

  observeEvent(input$fishery_names_table_cell_edit, {
    if (fishery_map_missing_for_selected()) return()
    req(input$fishery_names_model)
    info <- input$fishery_names_table_cell_edit
    rv$fishery_names_dfs[[input$fishery_names_model]][info$row, info$col + 1] <- info$value
  })

  observeEvent(input$apply_fishery_names, {
    if (fishery_map_missing_for_selected()) {
      showNotification("fishery_map.R/.r is missing/invalid for selected model. Provide it and reload data first.", type = "warning", duration = 5)
      return()
    }
    req(input$fishery_names_model, rv$fishery_names_dfs)
    model_name <- input$fishery_names_model

    df <- rv$fishery_names_dfs[[model_name]]
    base_df <- rv$FISHERY_MAPS[[model_name]]
    idx <- match(df$fishery, base_df$fishery)
    base_df$fishery_name[idx] <- as.character(df$fishery_name)

    apply_map_df_to_model(model_name, base_df)
    refresh_dependents()

    showNotification(HTML(paste0("✓ Fishery names updated for model: <strong>", model_name, "</strong>")), type = "message", duration = 3)
  })
}
