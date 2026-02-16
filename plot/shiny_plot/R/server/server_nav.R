server_nav <- function(input, output, session, rv) {
    # FISHERY NAVIGATION BUTTONS
    # ===========================================================================
  
    # Length Frequency: Previous fishery
    observeEvent(input$lf_prev, {
      req(rv$data_loaded, input$lf_model, input$lf_fishery)
    
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
      if (length(fisheries) == 0) return()
    
      current_idx <- which(fisheries == input$lf_fishery)
    
      if (length(current_idx) == 0) {
        new_selection <- fisheries[1]
      } else {
        new_idx <- ifelse(current_idx == 1, length(fisheries), current_idx - 1)
        new_selection <- fisheries[new_idx]
      }
    
      updateSelectInput(session, "lf_fishery", selected = new_selection)
    })
  
    # Length Frequency: Next fishery
    observeEvent(input$lf_next, {
      req(rv$data_loaded, input$lf_model, input$lf_fishery)
    
      if (is.null(rv$LengOut_list[[input$lf_model]])) return()
    
      fisheries <- unique(rv$LengOut_list[[input$lf_model]]@lenfits$fishery)
    
      if (length(fisheries) == 0) return()
    
      current_idx <- which(fisheries == input$lf_fishery)
    
      if (length(current_idx) == 0) {
        new_selection <- fisheries[1]
      } else {
        new_idx <- ifelse(current_idx == length(fisheries), 1, current_idx + 1)
        new_selection <- fisheries[new_idx]
      }
    
      updateSelectInput(session, "lf_fishery", selected = new_selection)
    })
  
    # Weight Frequency: Previous fishery
    observeEvent(input$wf_prev, {
      req(rv$data_loaded, input$wf_model, input$wf_fishery)
    
      if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
      fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
      if (length(fisheries) == 0) return()
    
      current_idx <- which(fisheries == input$wf_fishery)
    
      if (length(current_idx) == 0) {
        new_selection <- fisheries[1]
      } else {
        new_idx <- ifelse(current_idx == 1, length(fisheries), current_idx - 1)
        new_selection <- fisheries[new_idx]
      }
    
      updateSelectInput(session, "wf_fishery", selected = new_selection)
    })
  
    # Weight Frequency: Next fishery
    observeEvent(input$wf_next, {
      req(rv$data_loaded, input$wf_model, input$wf_fishery)
    
      if (is.null(rv$WeightOut_list[[input$wf_model]])) return()
    
      fisheries <- unique(rv$WeightOut_list[[input$wf_model]]@wgtfits$fishery)
    
      if (length(fisheries) == 0) return()
    
      current_idx <- which(fisheries == input$wf_fishery)
    
      if (length(current_idx) == 0) {
        new_selection <- fisheries[1]
      } else {
        new_idx <- ifelse(current_idx == length(fisheries), 1, current_idx + 1)
        new_selection <- fisheries[new_idx]
      }
    
      updateSelectInput(session, "wf_fishery", selected = new_selection)
    })
}
