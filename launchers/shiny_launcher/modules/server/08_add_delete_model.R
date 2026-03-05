  # ========== ADD/DELETE MODEL HANDLERS ==========
  
  observeEvent(input$add_new_model, {
    showModal(modalDialog(
      title = "Add New Model",
      textInput("new_model_name", "Model Name:", placeholder = "e.g., sensitivity_M_high"),
      selectInput("new_model_base", "Copy from existing model:", 
                  choices = c("Create from scratch" = "", names(rv$models))),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_add_model", "Create", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$confirm_add_model, {
    req(input$new_model_name)
    new_name <- input$new_model_name
    
    if (new_name %in% names(rv$models)) { 
      showNotification("Model name already exists!", type = "error")
      return() 
    }
    
    if (input$new_model_base == "") {
      new_model <- list(
        description = "New model", 
        mfcl_commands = "./doitall.sh",
        program_path = "mfcl/exe/mfclo64_2026_01_22_vsn2278", 
        base_dir = "mfcl/inputs/2023_rep",
        jitter_seeds = "1 2 3", 
        jitter_cv = "0.2",
        jitter_coverage = "0.2",
        jitter_amount = "0.2",
        jitter_hessian = "0",
        model_hessian = "0",
        prof_hessian = "0",
        retro_hessian = "0",
        retro_peels = "1 2 3 4 5",
        nsplit = "5", 
        scalers = "110 100 90", 
        Reps = "2 2 2 2 2 2",
        Af172 = "0",
        Af173 = "0",
        Af174 = "0"
      )
    } else {
      new_model <- rv$models[[input$new_model_base]]
      new_model$description <- paste("Copied from", input$new_model_base)
    }
    
    rv$models[[new_name]] <- new_model
    updateSelectInput(session, "edit_model_select", choices = names(rv$models), selected = new_name)
    removeModal()
    showNotification(paste("✓ Created model:", new_name), type = "message")
  })
  
  observeEvent(input$delete_model, {
    req(input$edit_model_select)
    showModal(modalDialog(
      title = "Confirm Deletion", 
      paste("Delete model:", input$edit_model_select, "?"),
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_delete_model", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_model, {
    model_name <- input$edit_model_select
    rv$models[[model_name]] <- NULL
    updateSelectInput(session, "edit_model_select", choices = names(rv$models), selected = names(rv$models)[1])
    removeModal()
    showNotification(paste("✓ Deleted model:", model_name), type = "message")
  })
  
  observeEvent(input$reset_model, {
    req(input$edit_model_select)
    model_name <- input$edit_model_select
    
    if (model_name %in% names(rv$models_original)) {
      rv$models[[model_name]] <- rv$models_original[[model_name]]
      showNotification(paste("✓ Reset", model_name), type = "message")
    } else {
      showNotification("Cannot reset - this is a new model", type = "warning")
    }
  })
  
