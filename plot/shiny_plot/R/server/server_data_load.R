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
      rv$fishery_map_missing_models <- NULL
    
      # Validate directory exists
      if (!dir.exists(MODEL_DIR)) {
        showNotification("Model directory not found!", type = "error", duration = 5)
        return(NULL)
      }

      reset_loaded_data_state(rv)
      rv$initial_render_pending <- TRUE
      session$sendCustomMessage("toggleInitialRenderOverlay", TRUE)
    
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
          showNotification("No valid model folders found!", type = "error", duration = 5)
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
              payload_file <- file.path(folder, "model_payload.rds")
              payload_data <- if (file.exists(payload_file)) {
                tryCatch(readRDS(payload_file)$data, error = function(e) NULL)
              } else {
                NULL
              }

              if (!is.null(payload_data)) {
                par_obj <- payload_data$ParOut
                rep_obj <- payload_data$RepOut
              } else {
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

                par_obj <- read.MFCLPar(par_file)
                rep_obj <- read.MFCLRep(rep_file)
              }

              if (is.null(par_obj) || is.null(rep_obj)) {
                return(list(
                  name = scenario_name,
                  error = paste("Missing model payload/core outputs in", scenario_name),
                  data = NULL
                ))
              }

              info_obj <- if (!is.null(payload_data) && !is.null(payload_data$info)) {
                payload_data$info
              } else {
                info_file <- file.path(folder, "model_info.rds")
                if (file.exists(info_file)) tryCatch(readRDS(info_file), error = function(e) NULL) else NULL
              }

              model_min_year <- suppressWarnings(as.numeric(info_obj$min_year))
              model_min_year <- if (length(model_min_year) > 0) model_min_year[1] else NA_real_
              if (!is.finite(model_min_year)) {
                model_min_year <- suppressWarnings(as.numeric(tryCatch(par_obj@range["minyear"], error = function(e) NA_real_)))
                model_min_year <- if (length(model_min_year) > 0) model_min_year[1] else NA_real_
              }

              # Read model outputs primarily from model_payload.rds.
              # Raw-file fallbacks are intentionally avoided for optional objects
              # so compact-cleaned model folders still load cleanly.
              data <- list(
                ParOut = par_obj,
                RepOut = rep_obj,
                TagRepOut = if (!is.null(payload_data)) payload_data$TagRepOut else NULL,
                LengOut = if (!is.null(payload_data)) payload_data$LengOut else NULL,
                WeightOut = if (!is.null(payload_data)) payload_data$WeightOut else NULL,
                TagOut = if (!is.null(payload_data)) payload_data$TagOut else NULL,
                TagTempOut = if (!is.null(payload_data) && !is.null(payload_data$TagTempOut)) {
                  payload_data$TagTempOut
                } else {
                  tt_file <- file.path(folder, "temporary_tag_report")
                  if (file.exists(tt_file) && is.finite(model_min_year)) {
                    tryCatch(read.temporary_tag_report(tt_file, year1 = as.integer(model_min_year)), error = function(e) NULL)
                  } else {
                    NULL
                  }
                },
                AgeOut = if (!is.null(payload_data)) payload_data$AgeOut else NULL,
                IndepOut = if (!is.null(payload_data)) payload_data$IndepOut else NULL,
                info = info_obj,
                JitterPars = tryCatch({
                  jitter_dir <- file.path(folder, "jitter")
                  seed_dirs <- list.dirs(jitter_dir, full.names = TRUE, recursive = FALSE)
                  seed_dirs <- grep("jitter_seed_\\d+$", seed_dirs, value = TRUE)
                  if (length(seed_dirs) == 0) {
                    list()
                  } else {
                    seeds <- sub(".*_(\\d+)$", "\\1", basename(seed_dirs))
                    pars <- setNames(lapply(seed_dirs, function(d) {
                      result_file <- file.path(d, "jitter_result.rds")
                      if (file.exists(result_file)) {
                        existing <- tryCatch(readRDS(result_file), error = function(e) NULL)
                        needs_refresh <- FALSE
                        if (!is.null(existing)) {
                          dq <- existing$derived_quantities
                          if (!is.null(dq) && is.data.frame(dq)) {
                            needs_refresh <- !all(c("recruitment", "fishing_mortality") %in% names(dq))
                          }
                          if (!isTRUE(needs_refresh)) {
                            ac <- existing$age_curves
                            needs_refresh <- is.null(ac) || !is.data.frame(ac) || nrow(ac) == 0 ||
                              !all(c("age", "natural_mortality", "growth") %in% names(ac))
                          }
                        }
                        if (!is.null(existing) && !isTRUE(needs_refresh)) {
                          return(existing)
                        }
                        rebuilt <- tryCatch(mp_build_jitter_payload(d), error = function(e) NULL)
                        if (!is.null(rebuilt)) {
                          return(rebuilt)
                        }
                        return(existing)
                      }
                      tryCatch(mp_build_jitter_payload(d), error = function(e) NULL)
                    }), seeds)
                    Filter(Negate(is.null), pars)
                  }
                }, error = function(e) list()),
                JitterInfos = tryCatch({
                  jitter_dir <- file.path(folder, "jitter")
                  seed_dirs <- list.dirs(jitter_dir, full.names = TRUE, recursive = FALSE)
                  seed_dirs <- grep("jitter_seed_\\d+$", seed_dirs, value = TRUE)
                  if (length(seed_dirs) == 0) {
                    list()
                  } else {
                    seeds <- sub(".*_(\\d+)$", "\\1", basename(seed_dirs))
                    infos <- setNames(lapply(seed_dirs, function(d) {
                      info_file <- file.path(d, "jitter_info.rds")
                      if (file.exists(info_file)) readRDS(info_file) else NULL
                    }), seeds)
                    Filter(Negate(is.null), infos)
                  }
                }, error = function(e) list())
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
        rv$TagRepOut_list <- map(results_named, "TagRepOut")
        rv$LengOut_list <- map(results_named, "LengOut")
        rv$WeightOut_list <- map(results_named, "WeightOut")
        rv$TagOut_list <- map(results_named, "TagOut")
        rv$TagTempOut_list <- map(results_named, "TagTempOut")
        rv$AgeOut_list <- map(results_named, "AgeOut")
        rv$IndepOut_list <- map(results_named, "IndepOut")
        rv$Info_list <- purrr::imap(
          purrr::map(results_named, "info"),
          ~ sp_enrich_model_info(file.path(MODEL_DIR, .y), .y, .x)
        )
        rv$model_choice_labels <- sp_model_choice_labels(MODEL_DIR, names(results_named))
        model_choices_for <- function(models) {
          models <- as.character(models)
          choices <- rv$model_choice_labels[unname(rv$model_choice_labels) %in% models]
          if (length(choices) == 0) stats::setNames(models, models) else choices
        }
        rv$JitterPars_list <- map(results_named, "JitterPars")
        rv$JitterInfos_list <- map(results_named, "JitterInfos")
      
        # Create model-specific fishery maps from fishery_map.R when available.
        # If missing/invalid, build a fallback map so plots still render with default labels.
        map_status <- setNames(rep("ok", length(names(results_named))), names(results_named))

        rv$FISHERY_MAPS <- lapply(names(results_named), function(sc) {
          scenario_dir <- file.path(MODEL_DIR, sc)
          scenario_map_r <- find_fishery_map_script(scenario_dir)
          if (is.null(scenario_map_r) || !file.exists(scenario_map_r)) {
            map_status[[sc]] <<- "missing_fallback"
            return(build_fallback_fishery_map(
              rep_obj = rv$RepOut_list[[sc]],
              len_obj = rv$LengOut_list[[sc]],
              wgt_obj = rv$WeightOut_list[[sc]],
              tagtemp_obj = rv$TagTempOut_list[[sc]]
            ))
          }

          map_from_r <- load_fishery_map_from_r(scenario_map_r)
          if (is.null(map_from_r)) {
            map_status[[sc]] <<- "invalid_fallback"
            return(build_fallback_fishery_map(
              rep_obj = rv$RepOut_list[[sc]],
              len_obj = rv$LengOut_list[[sc]],
              wgt_obj = rv$WeightOut_list[[sc]],
              tagtemp_obj = rv$TagTempOut_list[[sc]]
            ))
          }

          build_model_fishery_map(
            par_obj = rv$ParOut_list[[sc]],
            base_map = map_from_r,
            rep_obj = rv$RepOut_list[[sc]],
            len_obj = rv$LengOut_list[[sc]],
            wgt_obj = rv$WeightOut_list[[sc]],
            tagtemp_obj = rv$TagTempOut_list[[sc]]
          )
        })
        names(rv$FISHERY_MAPS) <- names(results_named)

        missing_or_invalid_models <- names(map_status)[map_status != "ok"]
        rv$fishery_map_missing_models <- missing_or_invalid_models

        if (length(missing_or_invalid_models) > 0) {
          showNotification(
              HTML(paste0(
              "<strong>⚠ fishery_map.R / fishery_map.r missing/invalid for some models</strong><br/>",
              "Affected models: ", paste(missing_or_invalid_models, collapse = ", "), "<br/>",
              "Plots will still load using fallback numeric/default fishery names. Add fishery_map.R for descriptive names."
            )),
            type = "warning",
            duration = 12
          )
        }
      
        # Detect index fisheries
        rv$INDEX_FISHERIES_MAPS <- lapply(rv$FISHERY_MAPS, detect_index_fisheries)
      
        # Extract year ranges for each scenario
        rv$YearRanges <- map(rv$ParOut_list, function(par) {
          list(minYear = par@range["minyear"], maxYear = par@range["maxyear"])
        })
      
        # Initialize fishery names dataframes (one per model)
        rv$fishery_names_dfs <- lapply(names(rv$FISHERY_MAPS), function(model_name) {
          fishery_map <- rv$FISHERY_MAPS[[model_name]]
          if (is.null(fishery_map)) {
            data.frame(
              fishery = numeric(0),
              fishery_name = character(0),
              group = character(0),
              region = numeric(0),
              tag_recapture_group = numeric(0),
              tag_recapture_name = character(0),
              stringsAsFactors = FALSE
            )
          } else {
            fishery_map[, c("fishery", "fishery_name", "group", "region",
                            "tag_recapture_group", "tag_recapture_name")]
          }
        })
        names(rv$fishery_names_dfs) <- names(rv$FISHERY_MAPS)

        # Initialize tag reporting map dataframes (one per model) from tag_rep_map.R
        rv$tag_rep_map_dfs <- lapply(names(results_named), function(model_name) {
          map_path <- find_tag_rep_map_script(file.path(MODEL_DIR, model_name))
          map_df <- load_tag_rep_map_from_r(map_path)
          if (!is.data.frame(map_df)) {
            return(data.frame(
              tag_recapture_group = numeric(0),
              tag_recapture_name = character(0),
              stringsAsFactors = FALSE
            ))
          }
          map_df
        })
        names(rv$tag_rep_map_dfs) <- names(results_named)
      
        incProgress(0.95, detail = "Finalizing...")
      
        # Set data loaded flag
        rv$data_loaded <- TRUE
      
        # Update UI components with loaded data
        updatePickerInput(session, "scenarios", 
                          choices = model_choices_for(names(results_named)),
                          selected = names(results_named))
      
        updateSelectInput(session, "bound_model", choices = model_choices_for(names(results_named)))
      
        available_cpue_models <- names(rv$RepOut_list)[!vapply(rv$RepOut_list, is.null, logical(1))]
        available_lf_models <- names(rv$LengOut_list)[!vapply(rv$LengOut_list, is.null, logical(1))]
        available_wf_models <- names(rv$WeightOut_list)[!vapply(rv$WeightOut_list, is.null, logical(1))]

        # Update scenario pickers for all tabs (select all by default)
        updatePickerInput(session, "stock_scenarios", 
                          choices = model_choices_for(names(results_named)), 
                          selected = names(results_named))
        updatePickerInput(session, "cpue_scenarios", 
                          choices = model_choices_for(available_cpue_models), 
                          selected = available_cpue_models)
      
        # Update model selectors for LF/WF tabs (single selection)
        lf_selected <- if (length(available_lf_models) > 0) available_lf_models[1] else character(0)
        wf_selected <- if (length(available_wf_models) > 0) available_wf_models[1] else character(0)
        updateSelectInput(session, "lf_model", 
                          choices = model_choices_for(available_lf_models),
                          selected = lf_selected)
        updateSelectInput(session, "wf_model", 
                          choices = model_choices_for(available_wf_models),
                          selected = wf_selected)
      
        # Update fishery names model selector
        updateSelectInput(session, "fishery_names_model",
                          choices = model_choices_for(names(results_named)),
                          selected = names(results_named)[1])
      
        incProgress(1)
      
        # Display success message with timing info
        showNotification(
          HTML(paste0(
            "<strong>✓ Successfully loaded (parallel mode)!</strong><br/>",
            "Directory: ", basename(MODEL_DIR), "<br/>",
            "Models: ", length(results_named), "<br/>",
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
