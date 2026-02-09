server_data_load <- function(input, output, session, rv) {
    # DATA LOADING (PARALLELIZED)
    # ---------------------------------------------------------------------------
  
    observeEvent(input$load_data, {
    
      req(input$model_dir, input$models_to_load)
    
      # Check if any models selected
      if (length(input$models_to_load) == 0) {
        showNotification("Please select at least one model to load!", 
                         type = "warning", duration = 5)
        return(NULL)
      }
    
      MODEL_DIR <- input$model_dir
    
      # Validate directory exists
      if (!dir.exists(MODEL_DIR)) {
        showNotification("Model directory not found!", type = "error", duration = 5)
        return(NULL)
      }
    
      # Show progress bar
      withProgress(message = "Loading model data...", value = 0, {
      
        # Get only selected scenario folders
        model_names <- input$models_to_load
        model_folders <- file.path(MODEL_DIR, model_names)
      
        # Validate all selected folders exist
        existing_idx <- dir.exists(model_folders)
        if (!all(existing_idx)) {
          missing <- model_names[!existing_idx]
          showNotification(
            paste("Some selected models not found:", paste(missing, collapse = ", ")),
            type = "warning", duration = 5
          )
        }
      
        # Keep only existing folders
        model_folders <- model_folders[existing_idx]
        model_names <- model_names[existing_idx]
      
        # Check if any scenarios found
        if (length(model_folders) == 0) {
          showNotification("No valid scenario folders found!", type = "error", duration = 5)
          return(NULL)
        }
      
        incProgress(0.1, detail = paste("Loading", length(model_folders), "scenarios"))
      
        # =======================================================================
        # PARALLEL LOADING
        # =======================================================================
      
        # Determine optimal number of cores (total cores - 2, minimum 1)
        n_cores <- parallel::detectCores()
        n_cores <- max(1, min(n_cores - 2, length(model_folders)))
      
        # Show parallel info
        showNotification(
          paste0("🚀 Using ", n_cores, " parallel worker", 
                 if(n_cores > 1) "s" else "", " for faster loading"),
          type = "message",
          duration = 3
        )
      
        incProgress(0.15, detail = paste("Initializing", n_cores, "parallel workers..."))
      
        # Create cluster
        cl <- makeCluster(n_cores)
      
        # Ensure cluster cleanup on exit
        on.exit({
          tryCatch(stopCluster(cl), error = function(e) NULL)
        }, add = TRUE)
      
        # Export necessary packages to workers
        clusterEvalQ(cl, {
          library(FLR4MFCL)
          library(purrr)
        })
      
        # Export helper functions and variables
        clusterExport(cl, 
                      c("finalPar", "finalRep", "safe_read", 
                        "model_folders", "model_names"), 
                      envir = environment())
      
        # Load scenarios in parallel
        results <- tryCatch({
          parLapply(cl, seq_along(model_folders), function(i) {
            folder <- model_folders[i]
            scenario_name <- model_names[i]
          
            tryCatch({
              # Check if required files exist
              par_file <- finalPar(folder)
              rep_file <- finalRep(folder)
            
              # Validate .par file
              if (!file.exists(par_file)) {
                return(list(
                  name = scenario_name,
                  error = paste("Missing .par file in", scenario_name),
                  data = NULL
                ))
              }
            
              # Validate plot.rep file
              if (!file.exists(rep_file)) {
                return(list(
                  name = scenario_name,
                  error = paste("Missing plot.rep file in", scenario_name),
                  data = NULL
                ))
              }
            
              # Read model output files
              data <- list(
                ParOut = read.MFCLPar(par_file),
                RepOut = read.MFCLRep(rep_file),
                LengOut = tryCatch({
                  lf_file <- file.path(folder, "length.fit")
                  if (file.exists(lf_file)) read.MFCLLenFit(lf_file) else NULL
                }, error = function(e) NULL),
                WeightOut = tryCatch({
                  wf_file <- file.path(folder, "weight.fit")
                  if (file.exists(wf_file)) read.MFCLWgtFit(wf_file) else NULL
                }, error = function(e) NULL),
                TagOut = tryCatch({
                  tag_files <- list.files(folder, "\\.tag$", full.names = TRUE)
                  if (length(tag_files) > 0) read.MFCLTag(tag_files) else NULL
                }, error = function(e) NULL),
                AgeOut = tryCatch({
                  age_files <- list.files(folder, "\\.age_length$", full.names = TRUE)
                  if (length(age_files) > 0) read.MFCLALK(age_files) else NULL
                }, error = function(e) NULL),
                IndepOut = safe_read(file.path(folder, "indepvar.rpt"))
              )
            
              list(name = scenario_name, error = NULL, data = data)
            
            }, error = function(e) {
              list(
                name = scenario_name,
                error = paste("Error loading", scenario_name, ":", e$message),
                data = NULL
              )
            })
          })
        }, error = function(e) {
          showNotification(
            paste("Parallel loading error:", e$message),
            type = "error",
            duration = 5
          )
          return(NULL)
        })
      
        # Stop cluster
        stopCluster(cl)
      
        # Check if loading succeeded
        if (is.null(results)) {
          return(NULL)
        }
      
        incProgress(0.7, detail = "Processing loaded data...")
      
        # Process results
        errors <- Filter(function(x) !is.null(x$error), results)
        successes <- Filter(function(x) is.null(x$error), results)
      
        # Show errors if any
        if (length(errors) > 0) {
          for (err in errors) {
            showNotification(err$error, type = "warning", duration = 3)
          }
        }
      
        # Check if any scenarios loaded successfully
        if (length(successes) == 0) {
          showNotification("Failed to load any scenarios!", type = "error", duration = 5)
          return(NULL)
        }
      
        # Convert to named list
        results_named <- setNames(
          lapply(successes, function(x) x$data),
          sapply(successes, function(x) x$name)
        )
      
        incProgress(0.8, detail = "Creating fishery mappings...")
      
        # Extract data into separate lists
        rv$ParOut_list <- map(results_named, "ParOut")
        rv$RepOut_list <- map(results_named, "RepOut")
        rv$LengOut_list <- map(results_named, "LengOut")
        rv$WeightOut_list <- map(results_named, "WeightOut")
        rv$TagOut_list <- map(results_named, "TagOut")
        rv$AgeOut_list <- map(results_named, "AgeOut")
        rv$IndepOut_list <- map(results_named, "IndepOut")
      
        # Create fishery name mappings for each scenario
        rv$FISHERY_MAPS <- lapply(names(results_named), function(sc) {
          create_fishery_map(rv$ParOut_list[[sc]], GLOBAL_FISHERY_NAMES)
        })
        names(rv$FISHERY_MAPS) <- names(results_named)
      
        # Detect index fisheries (fisheries ending with 'i' or containing 'index')
        rv$INDEX_FISHERIES_MAPS <- lapply(rv$FISHERY_MAPS, detect_index_fisheries)
      
        # Extract year ranges for each scenario
        rv$YearRanges <- map(rv$ParOut_list, function(par) {
          list(minYear = par@range["minyear"], maxYear = par@range["maxyear"])
        })
      
        # Initialize fishery names dataframes (one per model)
        rv$fishery_names_dfs <- lapply(names(rv$FISHERY_MAPS), function(model_name) {
          fishery_map <- rv$FISHERY_MAPS[[model_name]]
          data.frame(
            Fishery_ID = names(fishery_map),
            Fishery_Name = as.character(fishery_map),
            stringsAsFactors = FALSE
          )
        })
        names(rv$fishery_names_dfs) <- names(rv$FISHERY_MAPS)
      
        incProgress(0.95, detail = "Finalizing...")
      
        # Set data loaded flag
        rv$data_loaded <- TRUE
      
        # Update UI components with loaded data
        updatePickerInput(session, "scenarios", 
                          choices = names(results_named),
                          selected = names(results_named))
      
        updateSelectInput(session, "bound_model", choices = names(results_named))
      
        # Update scenario pickers for all tabs (select all by default)
        updatePickerInput(session, "stock_scenarios", 
                          choices = names(results_named), 
                          selected = names(results_named))
        updatePickerInput(session, "cpue_scenarios", 
                          choices = names(results_named), 
                          selected = names(results_named))
      
        # Update model selectors for LF/WF tabs (single selection)
        updateSelectInput(session, "lf_model", 
                          choices = names(results_named),
                          selected = names(results_named)[1])
        updateSelectInput(session, "wf_model", 
                          choices = names(results_named),
                          selected = names(results_named)[1])
      
        # Update fishery names model selector
        updateSelectInput(session, "fishery_names_model",
                          choices = names(results_named),
                          selected = names(results_named)[1])
      
        incProgress(1)
      
        # Display success message with timing info
        showNotification(
          HTML(paste0(
            "<strong>✓ Successfully loaded (parallel mode)!</strong><br/>",
            "Directory: ", basename(MODEL_DIR), "<br/>",
            "Scenarios: ", length(results_named), "<br/>",
            "Workers: ", n_cores, " parallel core", if(n_cores > 1) "s" else "", "<br/>",
            "Names: ", paste(names(results_named), collapse = ", ")
          )), 
          type = "message", 
          duration = 10
        )
      })
    })
  
    # Output: data loaded flag
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
    # ===========================================================================

}
