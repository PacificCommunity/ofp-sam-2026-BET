mod_fishery_names_ui <- function() {
  tabItem(
    tabName = "fishery_names",
    h2("Fishery Names Manager", style = "color: #17a2b8;"),

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
              <li>💾 <strong>Apply changes:</strong> Update selected model mapping</li>
              <li>🔄 <strong>Apply to all:</strong> Copy <code>fishery_name</code> by fishery id to all models</li>
              <li>📥 <strong>CSV:</strong> Download / upload fishery map table</li>
              <li>↩️ <strong>Reset:</strong> Reset to default fishery map logic from plots_refactored modules</li>
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

        div(
          style = "margin-bottom: 15px;",
          actionButton("apply_fishery_names", "💾 Apply Changes to This Model", class = "btn-success", icon = icon("check")),
          actionButton("apply_to_all_models", "🔄 Apply to All Models", class = "btn-warning", icon = icon("copy")),
          actionButton("reset_fishery_names", "↩️ Reset to Default", class = "btn-danger", icon = icon("undo")),
          downloadButton("download_fishery_names", "📥 Download CSV", class = "btn-info"),
          tags$div(
            style = "display: inline-block; margin-left: 10px;",
            fileInput("upload_fishery_names", "📤 Upload CSV", accept = ".csv", buttonLabel = "Browse...", width = "280px")
          )
        ),

        DTOutput("fishery_names_table")
      )
    )
  )
}

mod_fishery_names_server <- function(input, output, session, rv) {
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
    paste0("📊 Total fisheries in this model: ", nrow(rv$fishery_names_dfs[[input$fishery_names_model]]))
  })

  output$fishery_names_table <- renderDT({
    req(rv$data_loaded, input$fishery_names_model, rv$fishery_names_dfs)
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
    req(input$fishery_names_model)
    info <- input$fishery_names_table_cell_edit
    rv$fishery_names_dfs[[input$fishery_names_model]][info$row, info$col + 1] <- info$value
  })

  observeEvent(input$apply_fishery_names, {
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

  observeEvent(input$apply_to_all_models, {
    req(input$fishery_names_model, rv$fishery_names_dfs)
    source_model <- input$fishery_names_model
    source_df <- rv$fishery_names_dfs[[source_model]][, c("fishery", "fishery_name")]

    n_updated <- 0
    for (model_name in names(rv$FISHERY_MAPS)) {
      model_df <- rv$FISHERY_MAPS[[model_name]]
      m <- match(source_df$fishery, model_df$fishery)
      hit <- which(!is.na(m))
      if (length(hit) > 0) {
        model_df$fishery_name[m[hit]] <- as.character(source_df$fishery_name[hit])
        apply_map_df_to_model(model_name, model_df)
        n_updated <- n_updated + 1
      }
    }

    refresh_dependents()
    showNotification(HTML(paste0("✓ Applied fishery_name updates to <strong>", n_updated, " model(s)</strong>.")), type = "message", duration = 4)
  })

  observeEvent(input$reset_fishery_names, {
    req(rv$ParOut_list)

    default_map <- if (exists("pm_default_fishery_map", mode = "function")) {
      pm_default_fishery_map()
    } else {
      data.frame(
        fishery = 1:50,
        fishery_name = paste("Fishery", 1:50),
        region = NA_real_,
        group = "Unknown",
        tag_recapture_group = 1:50,
        tag_recapture_name = paste("Fishery", 1:50),
        stringsAsFactors = FALSE
      )
    }

    base_map <- if (exists("pm_load_or_build_fishery_map", mode = "function")) {
      fishery_map_path <- if (exists("get_fishery_map_path", mode = "function")) get_fishery_map_path() else "config/fishery_map.csv"
      pm_load_or_build_fishery_map(
        default_map = default_map,
        map_path = fishery_map_path,
        rep_list = rv$RepOut_list,
        len_list = rv$LengOut_list,
        wgt_list = rv$WeightOut_list,
        tagtemp_list = rv$TagTempOut_list
      )
    } else {
      default_map
    }

    for (model_name in names(rv$ParOut_list)) {
      map_df <- build_model_fishery_map(
        par_obj = rv$ParOut_list[[model_name]],
        base_map = base_map,
        rep_obj = rv$RepOut_list[[model_name]],
        len_obj = rv$LengOut_list[[model_name]],
        wgt_obj = rv$WeightOut_list[[model_name]],
        tagtemp_obj = rv$TagTempOut_list[[model_name]]
      )
      apply_map_df_to_model(model_name, map_df)
    }

    refresh_dependents()
    showNotification("✓ Fishery maps reset to default fishery_name mapping.", type = "message", duration = 4)
  })

  output$download_fishery_names <- downloadHandler(
    filename = function() {
      model_name <- input$fishery_names_model
      paste0("fishery_map_", model_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$fishery_names_model, rv$fishery_names_dfs)
      write.csv(rv$fishery_names_dfs[[input$fishery_names_model]], file, row.names = FALSE)
    }
  )

  observeEvent(input$upload_fishery_names, {
    req(input$fishery_names_model, input$upload_fishery_names)
    model_name <- input$fishery_names_model

    uploaded <- tryCatch(read.csv(input$upload_fishery_names$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(uploaded)) {
      showNotification("❌ Failed to read CSV file", type = "error", duration = 4)
      return()
    }

    if (!all(c("fishery", "fishery_name") %in% names(uploaded))) {
      showNotification("❌ CSV must have columns: fishery, fishery_name", type = "error", duration = 5)
      return()
    }

    current <- rv$fishery_names_dfs[[model_name]]
    m <- match(uploaded$fishery, current$fishery)
    hit <- which(!is.na(m))

    if (length(hit) == 0) {
      showNotification("⚠️ No matching fishery IDs found in uploaded CSV", type = "warning", duration = 4)
      return()
    }

    current$fishery_name[m[hit]] <- as.character(uploaded$fishery_name[hit])
    rv$fishery_names_dfs[[model_name]] <- current

    showNotification(HTML(paste0("✓ Uploaded fishery_name updates: <strong>", length(hit), " rows</strong>. Click 'Apply Changes'.")), type = "message", duration = 5)
  })
}
