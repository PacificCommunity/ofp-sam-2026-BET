mod_fishery_names_ui <- function() {
  tabItem(
    tabName = "fishery_names",
    h2("Fishery / Tag Map Names", style = "color: #17a2b8;"),

    fluidRow(
      box(
        title = "Instructions",
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        HTML("<ul>
              <li>🎯 <strong>Select Model:</strong> Choose which model fishery map to edit</li>
              <li>📝 <strong>Edit fields:</strong> Edit all columns except <code>fishery</code> directly</li>
              <li>💾 <strong>Apply changes:</strong> Save edits to the currently loaded model map in app memory</li>
              <li>⚠ <strong>Recommended:</strong> Add <code>fishery_map.R</code> and <code>tag_rep_map.R</code> in each model folder for descriptive names (plots still work with fallback/default names)</li>
              <li>🏷 <strong>Tag reporting labels:</strong> <code>tag_rep_map.R</code> is shown below per model and can be edited in-app</li>
            </ul>")
      )
    ),

    fluidRow(
      box(
        title = "Model",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
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
        )
      )
    ),

    fluidRow(
      box(
        title = uiOutput("fishery_map_table_title"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,

        uiOutput("fishery_map_warning"),

        div(
          style = "margin-bottom: 15px;",
          actionButton("apply_fishery_names", "💾 Apply Changes", class = "btn-success", icon = icon("check"))
        ),

        DTOutput("fishery_names_table")
      )
    ),

    fluidRow(
      box(
        title = uiOutput("tag_rep_map_table_title"),
        width = 12,
        solidHeader = TRUE,
        status = "info",
        collapsible = TRUE,

        uiOutput("tag_rep_map_warning"),
        div(
          style = "margin-bottom: 15px;",
          actionButton("apply_tag_rep_map", "💾 Apply Tag Rep Map Changes", class = "btn-info", icon = icon("check"))
        ),
        DTOutput("tag_rep_map_table")
      )
    )
  )
}

mod_fishery_names_server <- function(input, output, session, rv) {
  selected_model_dir <- reactive({
    req(input$model_dir, input$fishery_names_model)
    file.path(input$model_dir, input$fishery_names_model)
  })

  output$fishery_map_table_title <- renderUI({
    if (is.null(input$fishery_names_model) || !nzchar(input$fishery_names_model)) {
      return(tags$span("Fishery Map Table (select a model)"))
    }

    model_dir <- selected_model_dir()
    map_path <- find_fishery_map_script(model_dir)
    source_label <- if (!is.null(map_path) && file.exists(map_path)) {
      file.path(input$fishery_names_model, basename(map_path))
    } else {
      file.path(input$fishery_names_model, "fishery_map.R")
    }

    tags$span(
      "Fishery Map Table ",
      tags$small(paste0("(from ", source_label, ")"))
    )
  })

  output$tag_rep_map_table_title <- renderUI({
    if (is.null(input$fishery_names_model) || !nzchar(input$fishery_names_model)) {
      return(tags$span("Tag Reporting Map Table (select a model)"))
    }

    model_dir <- selected_model_dir()
    map_path <- find_tag_rep_map_script(model_dir)
    source_label <- if (!is.null(map_path) && file.exists(map_path)) {
      file.path(input$fishery_names_model, basename(map_path))
    } else {
      file.path(input$fishery_names_model, "tag_rep_map.R")
    }

    tags$span(
      "Tag Reporting Map Table ",
      tags$small(paste0("(from ", source_label, ")"))
    )
  })

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
        "<strong>fishery_map.R / fishery_map.r missing/invalid (recommended)</strong><br/>",
        "Model: ", input$fishery_names_model, "<br/>",
        "Plots still run with fallback/default fishery names. Add fishery_map.R for descriptive naming."
      )),
      type = "warning",
      duration = 8
    )
  }, ignoreInit = TRUE)

  output$fishery_map_warning <- renderUI({
    req(rv$data_loaded, input$fishery_names_model)
    if (!fishery_map_missing_for_selected()) return(NULL)
    tags$div(
      class = "alert alert-info",
      style = "margin-bottom: 15px;",
      HTML(paste0(
        "<strong>fishery_map.R / fishery_map.r is missing/invalid for selected model (recommended file).</strong><br/>",
        "Model: ", input$fishery_names_model, "<br/>",
        "Plots use fallback/default fishery names. Add the file and click <strong>Load Data</strong> again for descriptive names."
      ))
    )
  })

  selected_tag_rep_map_df <- reactive({
    req(rv$data_loaded, input$fishery_names_model, rv$tag_rep_map_dfs)
    tag_map_df <- rv$tag_rep_map_dfs[[input$fishery_names_model]]
    if (!is.data.frame(tag_map_df)) {
      return(data.frame(
        tag_recapture_group = numeric(0),
        tag_recapture_name = character(0),
        stringsAsFactors = FALSE
      ))
    }
    tag_map_df
  })

  display_tag_rep_map_df <- function(df) {
    display_df <- df
    names(display_df) <- sub("^tag_recapture_group$", "tag_reporting_group", names(display_df))
    names(display_df) <- sub("^tag_recapture_name$", "tag_reporting_name", names(display_df))
    display_df
  }

  output$tag_rep_map_warning <- renderUI({
    req(rv$data_loaded, input$fishery_names_model, input$model_dir)
    model_dir <- selected_model_dir()
    tag_map_path <- find_tag_rep_map_script(model_dir)
    if (!is.null(tag_map_path) && file.exists(tag_map_path)) return(NULL)
    tags$div(
      class = "alert alert-info",
      style = "margin-bottom: 15px;",
      HTML(paste0(
        "<strong>tag_rep_map.R not found for selected model.</strong><br/>",
        "Model: ", input$fishery_names_model, "<br/>",
        "Plots use fallback/default tag reporting labels. Add <code>tag_rep_map.R</code> and click <strong>Load Data</strong> again for descriptive labels."
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
                      selected = compatible_models)
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
                      selected = compatible_models)
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

  coerce_to_reference_type <- function(values, reference) {
    if (is.factor(reference)) return(factor(values, levels = levels(reference)))
    if (inherits(reference, "Date")) return(as.Date(values))
    if (inherits(reference, "POSIXct")) return(as.POSIXct(values))
    if (inherits(reference, "POSIXlt")) return(as.POSIXct(values))
    if (is.logical(reference)) return(as.logical(values))
    if (is.integer(reference)) return(as.integer(values))
    if (is.numeric(reference)) return(as.numeric(values))
    as.character(values)
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
      editable = list(target = "cell", disable = list(columns = c(0))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        deferRender = TRUE,
        dom = "frtip",
        columnDefs = list(
          list(className = "dt-center", targets = 0),
          list(className = "editable-cell", targets = c(1, 2, 3, 4, 5))
        )
      ),
      rownames = FALSE
    )
  })

  output$tag_rep_map_table <- renderDT({
    req(rv$data_loaded, input$fishery_names_model, input$model_dir)
    model_dir <- file.path(input$model_dir, input$fishery_names_model)
    tag_map_path <- find_tag_rep_map_script(model_dir)
    if (is.null(tag_map_path) || !file.exists(tag_map_path)) {
      return(
        datatable(
          data.frame(Message = paste0("tag_rep_map.R not found for model: ", input$fishery_names_model)),
          options = list(dom = "t", paging = FALSE, deferRender = TRUE),
          rownames = FALSE
        )
      )
    }

    df <- selected_tag_rep_map_df()
    if (nrow(df) == 0) {
      return(
        datatable(
          data.frame(Message = paste0("tag_rep_map.R found but invalid/empty for model: ", input$fishery_names_model)),
          options = list(dom = "t", paging = FALSE, deferRender = TRUE),
          rownames = FALSE
        )
      )
    }
    df <- display_tag_rep_map_df(df)

    datatable(
      df,
      editable = list(target = "cell", disable = list(columns = c(0))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        deferRender = TRUE,
        dom = "frtip",
        columnDefs = list(
          list(className = "dt-center", targets = 0),
          list(className = "editable-cell", targets = c(1))
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

  observeEvent(input$tag_rep_map_table_cell_edit, {
    req(input$fishery_names_model, rv$tag_rep_map_dfs)
    if (is.null(rv$tag_rep_map_dfs[[input$fishery_names_model]])) return()
    info <- input$tag_rep_map_table_cell_edit
    rv$tag_rep_map_dfs[[input$fishery_names_model]][info$row, info$col + 1] <- info$value
  })

  observeEvent(input$apply_fishery_names, {
    req(input$fishery_names_model, rv$fishery_names_dfs)
    model_name <- input$fishery_names_model

    df <- rv$fishery_names_dfs[[model_name]]
    base_df <- rv$FISHERY_MAPS[[model_name]]
    idx <- match(df$fishery, base_df$fishery)
    editable_cols <- c("fishery_name", "group", "region", "tag_recapture_group", "tag_recapture_name")
    editable_cols <- editable_cols[editable_cols %in% names(base_df) & editable_cols %in% names(df)]

    for (col_nm in editable_cols) {
      base_df[[col_nm]][idx] <- coerce_to_reference_type(df[[col_nm]], base_df[[col_nm]])
    }

    apply_map_df_to_model(model_name, base_df)
    refresh_dependents()

    showNotification(HTML(paste0("✓ Fishery map fields updated in app memory for model: <strong>", model_name, "</strong>")), type = "message", duration = 3)
  })

  observeEvent(input$apply_tag_rep_map, {
    req(input$fishery_names_model, rv$tag_rep_map_dfs)
    model_name <- input$fishery_names_model
    tag_map_df <- rv$tag_rep_map_dfs[[model_name]]
    if (!is.data.frame(tag_map_df) || nrow(tag_map_df) == 0) {
      showNotification("tag_rep_map.R data is missing/invalid for selected model.", type = "warning", duration = 4)
      return()
    }

    tag_map_df$tag_recapture_group <- suppressWarnings(as.numeric(tag_map_df$tag_recapture_group))
    tag_map_df$tag_recapture_name <- as.character(tag_map_df$tag_recapture_name)
    tag_map_df <- tag_map_df[is.finite(tag_map_df$tag_recapture_group), , drop = FALSE]
    tag_map_df <- tag_map_df[!is.na(tag_map_df$tag_recapture_name) & nzchar(tag_map_df$tag_recapture_name), , drop = FALSE]
    tag_map_df <- tag_map_df[order(tag_map_df$tag_recapture_group), , drop = FALSE]
    rownames(tag_map_df) <- NULL
    rv$tag_rep_map_dfs[[model_name]] <- tag_map_df

    showNotification(
      HTML(paste0("✓ Tag reporting map fields updated for model: <strong>", model_name, "</strong>")),
      type = "message",
      duration = 3
    )
  })
}
