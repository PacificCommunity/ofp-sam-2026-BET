  observeEvent(input$refresh_saved_configs, {
    rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
    showNotification("Refreshed saved configurations list", type = "message", duration = 2)
  })

  # ========== LAUNCH UNIT SUMMARY ==========
  #
  # The user chooses existing inputs directly, or chooses base input(s) and lets
  # the launcher expand them into requested sensitivity variants.

  launch_input_mode <- function() {
    mode <- if (!is.null(input$input_launch_mode) && length(input$input_launch_mode) > 0) {
      as.character(input$input_launch_mode[[1]])
    } else {
      "existing"
    }
    if (mode %in% c("existing", "sensitivity")) mode else "existing"
  }

  sensitivity_mode_enabled <- function() {
    identical(launch_input_mode(), "sensitivity")
  }

  selected_existing_input_ids <- function() {
    rows <- scan_launch_input_dirs()
    if (nrow(rows) == 0) return(character(0))
    raw <- input$existing_input_choices
    if (is.null(raw)) return(rows$id[[1]])
    if (length(raw) == 0) return(character(0))
    selected <- as.character(raw)
    selected <- selected[selected %in% rows$id]
    if (length(selected) > 0) return(unique(selected))
    character(0)
  }

  selected_base_input_ids <- function() {
    rows <- scan_launch_input_dirs()
    if (nrow(rows) == 0) return(character(0))
    raw <- input$input_recipe_base_input_choice
    if (is.null(raw)) return(rows$id[[1]])
    if (length(raw) == 0) return(character(0))
    selected <- as.character(raw)
    selected <- selected[selected %in% rows$id]
    if (length(selected) > 0) return(unique(selected))
    character(0)
  }

  selected_base_input_id <- function() {
    ids <- selected_base_input_ids()
    if (length(ids) > 0) ids[[1]] else ""
  }

  current_launch_units <- function() {
    if (exists("selected_launch_units", mode = "function")) {
      return(selected_launch_units())
    }
    if (isTRUE(sensitivity_mode_enabled())) {
      selected_base_input_ids()
    }
    selected_existing_input_ids()
  }

  launch_unit_summary_card <- function(title, path, tokens = "", description = "", muted = FALSE) {
    div(
      class = "model-checkbox-row",
      style = if (isTRUE(muted)) "opacity: 0.78;" else NULL,
      div(
        class = "model-checkbox-content",
        div(class = "model-name-label", title),
        div(class = "model-desc-inline", path),
        if (nzchar(description)) div(class = "model-desc-inline", description),
        if (nzchar(tokens)) {
          span(class = "no-description", paste("tokens:", tokens))
        } else {
          span(class = "no-description", "base input")
        }
      )
    )
  }

  output$model_selection_ui <- renderUI({
    if (length(rv$models) == 0) {
      return(div(
        p("No launch settings loaded.", style = "color: red; font-weight: bold;"),
        p("Load a common settings config first.")
      ))
    }

    input_rows <- scan_launch_input_dirs()
    if (nrow(input_rows) == 0) {
      return(div(
        class = "model-selector-container",
        p("No MFCL input folders found under mfcl/inputs.",
          style = "color: #999; font-style: italic; text-align: center; padding: 20px;")
      ))
    }

    if (!isTRUE(sensitivity_mode_enabled())) {
      existing_ids <- selected_existing_input_ids()
      existing_rows <- input_rows[input_rows$id %in% existing_ids, , drop = FALSE]
      if (nrow(existing_rows) == 0) {
        return(div(
          class = "model-selector-container",
          p("Select one or more existing input folders.",
            style = "color: #999; font-style: italic; text-align: center; padding: 20px;")
        ))
      }
      existing_cards <- lapply(seq_len(nrow(existing_rows)), function(idx) {
        row <- existing_rows[idx, , drop = FALSE]
        launch_unit_summary_card(
          title = paste0("Existing: ", row$display_name[[1]]),
          path = row$display_base_dir[[1]],
          tokens = row$tokens[[1]],
          description = row$description[[1]]
        )
      })
      return(div(class = "model-selector-container", existing_cards))
    }

    base_ids <- selected_base_input_ids()
    base_rows <- input_rows[input_rows$id %in% base_ids, , drop = FALSE]
    if (nrow(base_rows) == 0) {
      return(div(
        class = "model-selector-container",
        p("Select one or more base input folders.",
          style = "color: #999; font-style: italic; text-align: center; padding: 20px;")
      ))
    }

    units <- current_launch_units()

    base_note <- div(
      style = "padding: 8px 12px; color: #777; font-size: 12px;",
      "Base input(s): ",
      paste(base_rows$display_name, collapse = ", ")
    )

    sens <- if (exists("selected_sensitivity_ids", mode = "function")) selected_sensitivity_ids() else character(0)
    include_base <- exists("include_base_launch_units", mode = "function") && isTRUE(include_base_launch_units())
    if (length(sens) == 0 && !isTRUE(include_base)) {
      return(div(
        class = "model-selector-container",
        base_note,
        div(
          style = "padding: 10px 12px; color: #777; font-style: italic;",
          "Select sensitivities above, or include the selected base input(s), to preview launch units."
        )
      ))
    }

    if (length(units) == 0) {
      return(div(
        class = "model-selector-container",
        base_note,
        div(
          style = "padding: 10px 12px; color: #777; font-style: italic;",
          "No launch units: the selected base input(s) already contain the selected sensitivity token(s), and base inputs are not included."
        )
      ))
    }

    unit_cards <- lapply(units, function(unit) {
      env <- if (exists("active_model_env", mode = "function")) active_model_env(unit) else NULL
      title <- if (exists("launch_unit_label", mode = "function")) launch_unit_label(unit) else unit
      path <- if (!is.null(env) && !is.null(env$display_base_dir)) {
        as.character(env$display_base_dir[[1]])
      } else if (!is.null(env) && !is.null(env$base_dir)) {
        as.character(env$base_dir[[1]])
      } else {
        unit
      }
      token_txt <- if (!is.null(env) && !is.null(env$launcher_input_recipe_label)) {
        as.character(env$launcher_input_recipe_label[[1]])
      } else {
        ""
      }
      description_txt <- if (!is.null(env) && !is.null(env$launcher_input_recipe_description)) {
        as.character(env$launcher_input_recipe_description[[1]])
      } else if (!is.null(env) && !is.null(env$description)) {
        as.character(env$description[[1]])
      } else {
        ""
      }
      launch_unit_summary_card(
        title = title,
        path = path,
        tokens = token_txt,
        description = description_txt
      )
    })

    div(
      class = "model-selector-container",
      unit_cards
    )
  })

  output$model_details_display <- renderUI({
    units <- current_launch_units()
    if (length(units) == 0) {
      return(p("No launch units selected.",
               style = "text-align: center; color: #999; padding: 20px;"))
    }

    summary_section <- NULL
    if (!is.null(rv$run_metadata$summary) && rv$run_metadata$summary != "") {
      summary_section <- div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 padding: 20px; margin-bottom: 25px; 
                 border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        tags$h3(
          icon("star"),
          " Run Summary",
          style = "color: white; margin-top: 0; margin-bottom: 15px; font-weight: bold;"
        ),
        p(rv$run_metadata$summary,
          style = "margin: 0; font-size: 15px; line-height: 1.8; color: white;")
      )
    }

    model_cards <- lapply(units, function(unit) {
      env <- if (exists("active_model_env", mode = "function")) active_model_env(unit) else NULL
      label <- if (exists("launch_unit_label", mode = "function")) launch_unit_label(unit) else unit
      if (is.null(env)) return(NULL)

      div(
        class = "model-details-card",
        div(class = "model-name-header", label),
        div(class = "model-desc", env$base_dir),
        tags$div(
          style = "margin-top: 10px;",
          if (!is.null(env$description) && nzchar(as.character(env$description[[1]]))) {
            div(class = "model-param", tags$strong("description:"), " ", env$description)
          },
          div(class = "model-param", tags$strong("base_dir:"), " ", env$base_dir),
          div(class = "model-param", tags$strong("model_dir:"), " ", env$model_dir),
          div(class = "model-param", tags$strong("tokens:"), " ",
              if (!is.null(env$launcher_input_recipe_label)) env$launcher_input_recipe_label else env$input_recipe_base_tokens),
          div(class = "model-param", tags$strong("settings:"), " ", rv$base_config_name)
        )
      )
    })

    div(
      class = "model-details-container",
      summary_section,
      model_cards
    )
  })
