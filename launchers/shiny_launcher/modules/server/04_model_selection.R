  observeEvent(input$refresh_saved_configs, {
    rv$saved_configs_trigger <- rv$saved_configs_trigger + 1
    showNotification("Refreshed saved configurations list", type = "message", duration = 2)
  })

  # ========== INPUT SELECTION HELPERS ==========
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
