  # ========== JOB HISTORY FUNCTIONS ==========
  
  get_job_history_file <- function(config_file) {
    if (is.null(config_file)) return(NULL)
    config_basename <- tools::file_path_sans_ext(basename(config_file))
    job_history_file <- file.path(resolve_repo_path(".models_ran"), 
                                  paste0(config_basename, "_job_history.rds"))
    return(job_history_file)
  }
  
  load_job_history <- function(config_file) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file) || !file.exists(job_file)) {
      return(data.frame(
        timestamp = character(),
        job_type = character(),
        model_names = character(),
        output_dir = character(),
        batch_names = character(),
        remote_dirs = character(),
        branch = character(),
        status = character(),
        stringsAsFactors = FALSE
      ))
    }
    readRDS(job_file)
  }
  
  save_job_history <- function(config_file, job_record) {
    job_file <- get_job_history_file(config_file)
    if (is.null(job_file)) return()
    job_dir <- dirname(job_file)
    if (!dir.exists(job_dir)) dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
    history <- load_job_history(config_file)
    history <- rbind(history, job_record)
    saveRDS(history, job_file)
  }

  # ========== LAUNCHER JOB LOG (GLOBAL) ==========

  get_launcher_job_log_file <- function() {
    file.path(resolve_repo_path(".models_ran"), "launcher_job_log.rds")
  }

  empty_launcher_job_log <- function() {
    data.frame(
      run_at = character(),
      output_dir = character(),
      summary = character(),
      run_description = character(),
      config_file = character(),
      job_types = character(),
      model_names = character(),
      total_jobs = integer(),
      launch_mode = character(),
      selected_condor_nodes = character(),
      status = character(),
      branch = character(),
      batch_names = character(),
      local_pid_files = character(),
      local_log_files = character(),
      config_details = character(),
      remote_dirs = character(),
      stringsAsFactors = FALSE
    )
  }

  load_launcher_job_log <- function() {
    required_cols <- c(
      "run_at", "output_dir", "summary", "run_description", "config_file",
      "job_types", "model_names", "total_jobs", "launch_mode", "selected_condor_nodes",
      "status", "branch", "batch_names", "local_pid_files", "local_log_files",
      "config_details", "remote_dirs"
    )

    log_file <- get_launcher_job_log_file()
    if (is.null(log_file) || !file.exists(log_file)) {
      return(empty_launcher_job_log())
    }
    out <- tryCatch(readRDS(log_file), error = function(e) NULL)
    if (!is.data.frame(out)) {
      return(empty_launcher_job_log())
    }
    for (cn in required_cols) {
      if (!cn %in% names(out)) {
        out[[cn]] <- if (identical(cn, "total_jobs")) NA_integer_ else NA_character_
      }
    }
    out <- out[, required_cols, drop = FALSE]
    out
  }

  save_launcher_job_log <- function(job_record) {
    log_file <- get_launcher_job_log_file()
    if (is.null(log_file)) return(invisible(FALSE))
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    history <- load_launcher_job_log()
    required_cols <- names(history)
    for (cn in required_cols) {
      if (!cn %in% names(job_record)) {
        job_record[[cn]] <- if (identical(cn, "total_jobs")) NA_integer_ else NA_character_
      }
    }
    job_record <- job_record[, required_cols, drop = FALSE]
    history <- rbind(history, job_record)
    saveRDS(history, log_file)
    invisible(TRUE)
  }

  clear_launcher_job_log <- function() {
    log_file <- get_launcher_job_log_file()
    if (is.null(log_file)) return(invisible(FALSE))
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(empty_launcher_job_log(), log_file)
    invisible(TRUE)
  }

  delete_launcher_job_log_rows <- function(row_ids) {
    if (is.null(row_ids) || length(row_ids) == 0) return(invisible(0L))
    log_file <- get_launcher_job_log_file()
    if (is.null(log_file) || !file.exists(log_file)) return(invisible(0L))

    history <- load_launcher_job_log()
    if (nrow(history) == 0) return(invisible(0L))

    idx <- suppressWarnings(as.integer(row_ids))
    idx <- unique(idx[is.finite(idx)])
    idx <- idx[idx >= 1 & idx <= nrow(history)]
    if (length(idx) == 0) return(invisible(0L))

    keep <- setdiff(seq_len(nrow(history)), idx)
    out <- if (length(keep) == 0) {
      empty_launcher_job_log()
    } else {
      history[keep, , drop = FALSE]
    }
    saveRDS(out, log_file)
    invisible(length(idx))
  }

  # ========== INPUT FOLDER DISCOVERY ==========

  empty_launch_input_dirs <- function() {
    data.frame(
      id = character(0),
      name = character(0),
      display_name = character(0),
      base_dir = character(0),
      display_base_dir = character(0),
      label = character(0),
      tokens = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    )
  }

  safe_launch_id <- function(x) {
    txt <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
    txt <- gsub("^_+|_+$", "", txt)
    ifelse(nzchar(txt), txt, "input")
  }

  read_input_tokens_safe <- function(input_dir) {
    if (exists("extract_input_change_tokens", mode = "function")) {
      return(extract_input_change_tokens(input_dir))
    }
    meta_path <- file.path(input_dir, "input_change_metadata.rds")
    if (!file.exists(meta_path)) return(character(0))
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    tokens <- if (is.list(meta) && !is.null(meta$tokens)) as.character(meta$tokens) else character(0)
    tokens <- tokens[!is.na(tokens) & nzchar(trimws(tokens))]
    unique(tokens)
  }

  read_input_description_safe <- function(input_dir) {
    if (exists("extract_input_change_description", mode = "function")) {
      return(extract_input_change_description(input_dir))
    }
    meta_path <- file.path(input_dir, "input_change_metadata.rds")
    if (!file.exists(meta_path)) return("")
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (!is.list(meta)) return("")
    explicit <- if (!is.null(meta$description)) trimws(as.character(meta$description[[1]])) else ""
    if (nzchar(explicit)) return(explicit)
    if (!is.null(meta$operations) && is.list(meta$operations)) {
      labels <- vapply(meta$operations, function(op) {
        if (!is.list(op) || is.null(op$label)) return("")
        trimws(as.character(op$label[[1]]))
      }, character(1))
      labels <- unique(labels[nzchar(labels)])
      if (length(labels) > 0) return(paste(labels, collapse = "; "))
    }
    paste(read_input_tokens_safe(input_dir), collapse = " + ")
  }

  looks_like_mfcl_input_dir <- function(path) {
    if (!dir.exists(path)) return(FALSE)
    files <- basename(list.files(path, all.files = FALSE, no.. = TRUE))
    any(grepl("\\.frq$", files, ignore.case = TRUE)) &&
    any(grepl("\\.ini$", files, ignore.case = TRUE))
  }

  repo_relative_path <- function(path) {
    p <- normalizePath(path, mustWork = FALSE, winslash = "/")
    root_norm <- normalizePath(resolve_repo_path("."), mustWork = FALSE, winslash = "/")
    root_pattern <- gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", root_norm)
    sub(paste0("^", root_pattern, "/?"), "", p)
  }

  launch_input_scan_cache <- new.env(parent = emptyenv())

  scan_launch_input_dirs <- function(inputs_root = "mfcl/inputs", force = FALSE) {
    root <- resolve_repo_path(inputs_root)
    if (!dir.exists(root)) {
      return(empty_launch_input_dirs())
    }

    all_dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    meta_paths <- file.path(all_dirs, "input_change_metadata.rds")
    meta_mtime <- suppressWarnings(file.info(meta_paths)$mtime)
    cache_key <- paste(
      normalizePath(root, mustWork = FALSE, winslash = "/"),
      suppressWarnings(as.numeric(file.info(root)$mtime)),
      paste(basename(all_dirs), collapse = "|"),
      paste(ifelse(is.na(meta_mtime), "", as.numeric(meta_mtime)), collapse = "|"),
      sep = "::"
    )
    if (!isTRUE(force) &&
        identical(launch_input_scan_cache$key, cache_key) &&
        is.data.frame(launch_input_scan_cache$value)) {
      return(launch_input_scan_cache$value)
    }

    dirs <- all_dirs
    dirs <- dirs[vapply(dirs, looks_like_mfcl_input_dir, logical(1))]
    if (length(dirs) == 0) {
      out <- empty_launch_input_dirs()
      launch_input_scan_cache$key <- cache_key
      launch_input_scan_cache$value <- out
      return(out)
    }

    rel <- vapply(dirs, repo_relative_path, character(1))
    names(rel) <- NULL

    ids <- safe_launch_id(rel)
    ids <- make.unique(ids, sep = "_")
    names(ids) <- NULL
    tokens <- vapply(dirs, function(d) paste(read_input_tokens_safe(d), collapse = ", "), character(1))
    tokens <- unname(tokens)
    description <- vapply(dirs, read_input_description_safe, character(1))
    description <- unname(description)
    name <- unname(basename(dirs))
    display_name <- if (exists("compact_input_name", mode = "function")) {
      vapply(name, compact_input_name, character(1))
    } else {
      name
    }
    display_base_dir <- file.path(dirname(rel), display_name)
    label <- ifelse(nzchar(tokens), paste0(display_name, " [", tokens, "]"), display_name)

    out <- data.frame(
      id = ids,
      name = name,
      display_name = display_name,
      base_dir = rel,
      display_base_dir = display_base_dir,
      label = label,
      tokens = tokens,
      description = description,
      stringsAsFactors = FALSE
    )
    out$.prefer_compact <- basename(out$base_dir) == out$display_name
    out <- out[order(out$display_name, !out$.prefer_compact, out$base_dir), , drop = FALSE]
    out <- out[!duplicated(out$display_name), , drop = FALSE]
    out$.prefer_compact <- NULL
    row.names(out) <- NULL
    launch_input_scan_cache$key <- cache_key
    launch_input_scan_cache$value <- out
    out
  }

  normalize_launch_defaults <- function(settings, summary_text = "") {
    if (is.null(settings)) settings <- list()
    settings <- as.list(settings, all.names = TRUE)
    if (is.null(settings$mfcl_commands) || length(settings$mfcl_commands) == 0) {
      settings$mfcl_commands <- "./doitall.sh"
    }
    command_txt <- trimws(as.character(settings$mfcl_commands[[1]]))
    program_txt <- if (!is.null(settings$program_path) && length(settings$program_path) > 0) {
      trimws(as.character(settings$program_path[[1]]))
    } else {
      ""
    }
    command_already_qualified <- nzchar(program_txt) && startsWith(command_txt, program_txt)

    out <- if (exists("apply_model_defaults", mode = "function")) {
      tryCatch(
        apply_model_defaults(list(common = settings))[[1]],
        error = function(e) settings
      )
    } else {
      settings
    }
    if (isTRUE(command_already_qualified)) {
      out$mfcl_commands <- settings$mfcl_commands
    }
    out$config_summary <- if (!is.null(summary_text) && nzchar(as.character(summary_text))) {
      as.character(summary_text)
    } else {
      ""
    }
    out
  }

  first_model_settings <- function(models) {
    if (is.null(models) || !is.list(models) || length(models) == 0) return(NULL)
    models[[1]]
  }

  launch_default_source_names <- c(
    "launch_defaults",
    "launcher_settings",
    "settings",
    "defaults",
    "model_template"
  )

  extract_launch_defaults_from_env <- function(env) {
    for (nm in launch_default_source_names) {
      if (exists(nm, envir = env, inherits = FALSE)) {
        return(list(
          settings = get(nm, envir = env, inherits = FALSE),
          source_label = nm,
          legacy_count = NA_integer_
        ))
      }
    }

    if (exists("models", envir = env, inherits = FALSE)) {
      models <- get("models", envir = env, inherits = FALSE)
      return(list(
        settings = first_model_settings(models),
        source_label = "legacy models[[1]]",
        legacy_count = length(models)
      ))
    }
    if (exists("config", envir = env, inherits = FALSE)) {
      config <- get("config", envir = env, inherits = FALSE)
      if (is.list(config)) {
        return(list(settings = config, source_label = "config", legacy_count = NA_integer_))
      }
    }
    NULL
  }

  extract_launch_defaults_from_list <- function(x) {
    for (nm in launch_default_source_names) {
      if (!is.null(x[[nm]])) {
        return(list(settings = x[[nm]], source_label = nm, legacy_count = NA_integer_))
      }
    }
    if (!is.null(x$models)) {
      return(list(
        settings = first_model_settings(x$models),
        source_label = "legacy models[[1]]",
        legacy_count = length(x$models)
      ))
    }
    if (!is.null(x$config) && is.list(x$config)) {
      return(list(settings = x$config, source_label = "config", legacy_count = NA_integer_))
    }
    NULL
  }

  extract_launch_defaults <- function(obj) {
    out <- if (is.environment(obj)) {
      extract_launch_defaults_from_env(obj)
    } else if (is.list(obj)) {
      extract_launch_defaults_from_list(obj)
    } else {
      NULL
    }

    if (is.null(out) || is.null(out$settings)) {
      stop("No common launch settings found. Define a 'launch_defaults' list in the config script.")
    }
    out
  }

  launch_defaults_env <- function() {
    if (!is.null(rv$launch_defaults)) return(rv$launch_defaults)
    first_model_settings(rv$models)
  }

  template_model_name <- function() {
    "common launch settings"
  }

  template_model_env <- function() {
    launch_defaults_env()
  }
  
  # ========== LOAD MODELS FUNCTION ==========
  
  load_models <- function(config_path = NULL, is_saved_run = FALSE, original_filename = NULL) {
    
    current_wd <- getwd()
    possible_paths <- c()
    
    if (!is.null(config_path)) {
      possible_paths <- c(possible_paths, config_path)
    }
    
    if (!is.null(input$config_file_path) && input$config_file_path != "") {
      possible_paths <- c(possible_paths, input$config_file_path)
    }
    
    if (!is.null(input$launch_config_path) && input$launch_config_path != "") {
      possible_paths <- c(possible_paths, input$launch_config_path)
    }
    
    possible_paths <- c(
      possible_paths,
      "configs/2023R4_launch_settings.R",
      "set_model.R"
    )
    
    possible_paths <- unique(possible_paths[!is.na(possible_paths) & possible_paths != ""])
    possible_paths <- vapply(possible_paths, resolve_repo_path, character(1))
    
    rv$config_status_msg <- paste0(
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📁 Current Working Directory:\n",
      current_wd, "\n\n",
      "🔍 Searching for launch settings script...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
    )
    
    found_path <- NULL
    for (path in possible_paths) {
      exists <- file.exists(path)
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     ifelse(exists, "✓ ", "✗ "),
                                     path, "\n")
      
      if (exists && is.null(found_path)) {
        found_path <- path
      }
    }
    
    if (is.null(found_path)) {
      rv$config_status_msg <- paste0(rv$config_status_msg, 
                                     "\n❌ No launch settings script found!\n",
                                     "Please provide a path to an R script containing a 'launch_defaults' list object.")
      showNotification("Config file not found", type = "error")
      return(FALSE)
    }
    
    rv$config_status_msg <- paste0(rv$config_status_msg, 
                                   "\n✅ Found R script at:\n", found_path, "\n\n")
    
    tryCatch({
      if (grepl("\\.rds|\\.RDS", found_path)) {
        saved_data <- readRDS(found_path)
        meta <- if (!is.null(saved_data$metadata)) saved_data$metadata else list()
        extracted <- extract_launch_defaults(saved_data)
        rv$run_metadata <- meta
        rv$base_config_name <- if (!is.null(meta$base_config)) meta$base_config else basename(found_path)
        rv$current_config_file <- found_path
        
        # Extract summary from saved config
        if (!is.null(meta$summary)) {
          rv$run_metadata$summary <- meta$summary
        }

        rv$launch_defaults <- normalize_launch_defaults(extracted$settings, rv$run_metadata$summary)
        rv$models <- list(common = rv$launch_defaults)
        rv$models_original <- rv$models

        run_name <- if (!is.null(meta$run_name)) meta$run_name else basename(found_path)
        description <- if (!is.null(meta$description)) meta$description else ""
        created_by <- if (!is.null(meta$created_by)) meta$created_by else ""
        date_text <- if (!is.null(meta$date)) format(meta$date, "%Y-%m-%d %H:%M") else ""

        legacy_line <- if (is.finite(extracted$legacy_count)) {
          paste0("\n  - Legacy model entries ignored as launch units: ", extracted$legacy_count)
        } else {
          ""
        }
        
        job_history <- load_job_history(found_path)
        rv$config_status_msg <- paste0(rv$config_status_msg, 
                                       "\n✓ Loaded saved launch settings",
                                       "\n  - Run Name: ", run_name,
                                       "\n  - Description: ", description,
                                       "\n  - Date: ", date_text,
                                       "\n  - Base Config: ", rv$base_config_name,
                                       "\n  - Created By: ", created_by,
                                       "\n  - Settings source: ", extracted$source_label,
                                       legacy_line)
        
        if (nrow(job_history) > 0) {
          rv$config_status_msg <- paste0(rv$config_status_msg,
                                         "\n📊 Job Run History (", nrow(job_history), " runs):\n")
          
          for (i in 1:min(5, nrow(job_history))) {
            row <- job_history[nrow(job_history) - i + 1, ]
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ", i, ". ", row$timestamp, " - ", row$job_type, 
                                           " (", row$model_names, ")\n",
                                           "     Output: ", row$output_dir, "\n",
                                           "     Branch: ", row$branch, "\n")
          }
          
          if (nrow(job_history) > 5) {
            rv$config_status_msg <- paste0(rv$config_status_msg,
                                           "  ... and ", nrow(job_history) - 5, " more runs\n")
          }
        }
        
        rv$config_status_msg <- paste0(rv$config_status_msg,
                                       "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      } else {
        env <- new.env()
        source(found_path, local = env, chdir = TRUE)
        
        # Extract summary if defined in the script
        if (exists("summary", envir = env)) {
          rv$run_metadata$summary <- env$summary
        } else {
          rv$run_metadata$summary <- NULL
        }
        
        extracted <- extract_launch_defaults(env)
        rv$launch_defaults <- normalize_launch_defaults(extracted$settings, rv$run_metadata$summary)
        rv$models <- list(common = rv$launch_defaults)
        rv$models_original <- rv$models
        rv$base_config_name <- basename(found_path)
        rv$current_config_file <- NULL

        legacy_line <- if (is.finite(extracted$legacy_count)) {
          paste0("\n  - Legacy model entries ignored as launch units: ", extracted$legacy_count)
        } else {
          ""
        }

        rv$config_status_msg <- paste0(rv$config_status_msg,
                                       "\n✓ Successfully loaded R script",
                                       "\n  - Script: ", basename(found_path),
                                       "\n  - Settings source: ", extracted$source_label,
                                       legacy_line,
                                       "\n  - Launch units come from mfcl/inputs, not config model names.\n")
      }

      if (!is.null(rv$models) && length(rv$models) > 0) {
        rv$models <- lapply(rv$models, function(x) {
          x$config_summary <- if (!is.null(rv$run_metadata$summary)) rv$run_metadata$summary else ""
          x
        })
        rv$models_original <- lapply(rv$models_original, function(x) {
          x$config_summary <- if (!is.null(rv$run_metadata$summary)) rv$run_metadata$summary else ""
          x
        })
      }
      
      rv$config_loaded <- TRUE
      
      # Store the actual full path for saving
      if (!is.null(original_filename)) {
        rv$config_path <- found_path  # Store full path, not just filename
      } else {
        rv$config_path <- found_path
      }
      
      # Store the original filename for display
      rv$uploaded_filename <- if (!is.null(original_filename)) original_filename else basename(found_path)
      
      
      input_rows <- tryCatch(scan_launch_input_dirs(), error = function(e) data.frame())
      rv$selected_models <- if (is.data.frame(input_rows) && nrow(input_rows) > 0) input_rows$id else character(0)
      updateSelectInput(session, "edit_model_select", 
                        choices = names(rv$models), 
                        selected = names(rv$models)[1])
      
      showNotification("Loaded common launch settings", type = "message")
      
      # Save this config path for next time
      save_settings()
      
      
      return(TRUE)
      
      
    }, error = function(e) {
      rv$config_status_msg <- paste0(rv$config_status_msg, "❌ Error loading R script:\n", e$message)
      showNotification(paste("Error:", e$message), type = "error")
      return(FALSE)
    })
  }
  
  observe({
    if (!isTRUE(rv$settings_ready)) return()

    if (!rv$config_loaded) {
      # Check if we have a saved last config path
      tryCatch({
        root_settings <- ensure_named_list(read_rds_safe(settings_path()))
        launcher_settings <- ensure_named_list(read_rds_safe(launcher_settings_path()))
        saved_config <- latest_setting_value(
          "last_config_file",
          launcher_settings,
          root_settings,
          file_mtime_safe(launcher_settings_path()),
          file_mtime_safe(settings_path())
        )
        if (is.null(saved_config) || !nzchar(as.character(saved_config))) {
          saved_config <- latest_setting_value(
            "config_path",
            launcher_settings,
            root_settings,
            file_mtime_safe(launcher_settings_path()),
            file_mtime_safe(settings_path())
          )
        }

        # If we have a saved config file, try to load it
        if (!is.null(saved_config) && nzchar(as.character(saved_config))) {
          saved_config <- as.character(saved_config)

          # Try to find the file
          possible_paths <- c(
            saved_config,
            file.path(resolve_repo_path(".launcher_configs"), saved_config),
            file.path(resolve_repo_path(".launcher_configs"), basename(saved_config))
          )

          for (path in possible_paths) {
            if (file.exists(path)) {
              load_models(config_path = path,
                          original_filename = basename(path))
              return()
            }
          }
        }
      }, error = function(e) {
        # Silently ignore errors
      })
      
      # Don't load anything by default - user must choose
      # This prevents auto-loading set_model.R every time
    }
  })
  
  
  
  observeEvent(input$reload_config, { load_models() })
  
  observeEvent(input$config_file_upload, {
    req(input$config_file_upload)
    load_models(input$config_file_upload$datapath)
  })
  
  # ---- Launch tab config loader handlers ----
  # observeEvent(input$launch_load_config, {
  #   load_models()
  # })
  
  # observeEvent(input$launch_config_upload, {
  #   req(input$launch_config_upload)
  #   
  #   # Store the actual uploaded filename and temp path
  #   rv$uploaded_filename <- input$launch_config_upload$name
  #   rv$uploaded_temp_path <- input$launch_config_upload$datapath
  #   
  #   # Load models with original filename
  #   load_models(
  #     config_path = input$launch_config_upload$datapath,
  #     original_filename = input$launch_config_upload$name
  #   )
  # })
  # 
  # 
  
  
