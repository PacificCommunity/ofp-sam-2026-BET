  # ========== BROWSE REMOTE OUTPUT DIRECTORY ==========
  
  observeEvent(input$browse_remote_output, {
    showNotification("Loading remote directories...", type = "message", duration = 2)
    
    tryCatch({
      base_path <- input$github_repo
      find_cmd <- sprintf("find %s -maxdepth 2 -mindepth 1 -type d | sort", base_path)
      cmd <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      result <- system(cmd, intern = TRUE)
      
      if (length(result) > 0) {
        dirs <- gsub(paste0("^", base_path, "/?"), "", result)
        dirs <- dirs[nzchar(dirs)]
        
        dir_items <- lapply(dirs, function(d) {
          depth <- length(strsplit(d, "/")[[1]])
          indent <- paste(rep("&nbsp;&nbsp;&nbsp;&nbsp;", depth - 1), collapse = "")
          dir_name <- basename(d)
          
          list(
            path = d,
            depth = depth,
            indent = indent,
            name = dir_name
          )
        })
        
        rv$pending_remote_path <- NULL
        showModal(modalDialog(
          title = "Select Remote Output Directory",
          size = "l",
          p(strong("Available directories on remote:"), style = "margin-bottom: 10px;"),
          p(paste("Base path:", base_path), style = "color: #666; font-size: 12px; margin-bottom: 15px;"),
          div(
            style = "max-height: 500px; overflow-y: auto; background: #f9f9f9; padding: 15px; border: 1px solid #ddd; border-radius: 4px; font-family: 'Courier New', monospace;",
            if (length(dirs) > 0) {
              lapply(dir_items, function(item) {
                icon_type <- if (item$depth == 1) "folder" else "folder-open"
                icon_color <- if (item$depth == 1) "#f39c12" else if (item$depth == 2) "#3c8dbc" else "#95a5a6"
                
                tags$div(
                  HTML(item$indent),
                  tags$a(
                    href = "#", 
                    onclick = sprintf(
                      "Shiny.setInputValue('selected_remote_path', '%s', {priority: 'event'}); return false;", 
                      item$path
                    ),
                    icon(icon_type, style = paste0("color: ", icon_color, ";")), " ", item$name,
                    style = "color: #333; cursor: pointer; text-decoration: none; font-size: 13px;"
                  ),
                  style = "padding: 3px 0; transition: background 0.2s;",
                  onmouseover = "this.style.background='#e8f4f8'; this.style.borderRadius='3px'; this.style.paddingLeft='5px';",
                  onmouseout = "this.style.background=''; this.style.paddingLeft='0px';"
                )
              })
            } else {
              p("No directories found", style = "text-align: center; color: #999; padding: 20px;")
            }
          ),
          shiny::hr(),
          div(style = "font-size:12px; color:#666;",
              "Selected: ",
              strong(textOutput("remote_path_pending_display", inline = TRUE))
          ),
          textInput("remote_output_manual", "Or enter path manually:", 
                    value = input$scan_output_dir,
                    placeholder = "e.g., quick/test_run, output/models"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_remote_output", "Select", class = "btn-primary")
          )
        ))
      } else {
        showNotification("No directories found", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$selected_remote_path, {
    req(input$selected_remote_path)
    rv$pending_remote_path <- input$selected_remote_path
  }, ignoreInit = TRUE)
  
  output$remote_path_pending_display <- renderText({
    if (!is.null(rv$pending_remote_path) && rv$pending_remote_path != "") {
      rv$pending_remote_path
    } else {
      "(none)"
    }
  })
  
  observeEvent(input$confirm_remote_output, {
    req(input$remote_output_manual)
    if (!is.null(rv$pending_remote_path) && rv$pending_remote_path != "") {
      updateTextInput(session, "scan_output_dir", value = rv$pending_remote_path)
    } else if (input$remote_output_manual != "") {
      updateTextInput(session, "scan_output_dir", value = input$remote_output_manual)
    }
    removeModal()
  })
  observe({
    # Get all inputs that start with select_dir_
    dir_inputs <- names(input)
    dir_inputs <- dir_inputs[grepl("^select_dir_", dir_inputs)]
    
    for (input_name in dir_inputs) {
      local({
        local_input <- input_name
        observeEvent(input[[local_input]], {
          selected_path <- input[[local_input]]
          rv$pending_remote_path <- selected_path
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$confirm_remote_output, {
    if (!is.null(rv$pending_remote_path) && rv$pending_remote_path != "") {
      updateTextInput(session, "scan_output_dir", value = rv$pending_remote_path)
      removeModal()
    } else if (!is.null(input$remote_output_manual) && input$remote_output_manual != "") {
      updateTextInput(session, "scan_output_dir", value = input$remote_output_manual)
      removeModal()
    }
  })
  
  
  
  # ========== RETRIEVE RESULTS HANDLERS ==========
  
  parallel_cores <- function() {
    req(input$retrieve_parallel_cores)
    max_cores <- max(1, parallel::detectCores() - 2)
    max(1, min(as.integer(input$retrieve_parallel_cores), max_cores))
  }
  
  rsync_base_opts <- function(fast = isTRUE(input$fast_download)) {
    if (isTRUE(fast)) {
      "-az"
    } else {
      "-avz --progress"
    }
  }

  retrieve_archive_source_path <- function(temp_dir, extract_path, folder_name) {
    source_path <- file.path(temp_dir, extract_path)

    if (!dir.exists(source_path)) {
      extract_parts <- strsplit(extract_path, "/", fixed = TRUE)[[1]]
      if (length(extract_parts) >= 3) {
        repo_part <- extract_parts[1]
        rest_parts <- extract_parts[-1]
        if (length(rest_parts) >= 1) {
          alt_parts <- c(rest_parts[1], folder_name, rest_parts[-1])
          alt_extract_path <- paste(c(repo_part, alt_parts), collapse = "/")
          alt_source_path <- file.path(temp_dir, alt_extract_path)
          if (dir.exists(alt_source_path)) source_path <- alt_source_path
        }
      }
    }

    if (!dir.exists(source_path)) {
      candidate_dirs <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
      pattern <- paste0("/", gsub("([.])", "\\\\.", extract_path), "$")
      matches <- candidate_dirs[grepl(pattern, candidate_dirs)]
      if (length(matches) > 0) source_path <- matches[1]
    }

    if (!dir.exists(source_path)) {
      candidate_dirs <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
      selftest_matches <- candidate_dirs[basename(candidate_dirs) == "selftest"]
      if (length(selftest_matches) > 0) source_path <- selftest_matches[1]
    }

    source_path
  }

  retrieve_archive_target_dir <- function(download_dir, source_path) {
    source_base <- basename(normalizePath(source_path, winslash = "/", mustWork = FALSE))
    if (!identical(source_base, "selftest")) return(download_dir)

    normalizePath(download_dir, winslash = "/", mustWork = FALSE)
  }

  retrieve_profile_relative_path <- function(source_path) {
    source_norm <- normalizePath(source_path, winslash = "/", mustWork = FALSE)
    parts <- strsplit(source_norm, "/", fixed = TRUE)[[1]]
    prof_idx <- which(parts %in% c("prof", "prof_indepvar"))
    if (length(prof_idx) == 0) return("")
    paste(parts[prof_idx[[1]]:length(parts)], collapse = "/")
  }

  retrieve_canonical_job_folder <- function(folder_name) {
    folder_name <- basename(trimws(as.character(folder_name[[1]])))
    if (!nzchar(folder_name)) return(folder_name)

    canonical <- folder_name
    canonical <- sub("_[^_]+_profchain_(down|up)$", "", canonical, ignore.case = TRUE, perl = TRUE)
    canonical <- sub("(_profchain_(down|up)|_prof2d|_model)$", "", canonical, ignore.case = TRUE, perl = TRUE)
    canonical <- sub("(_seed[0-9]+|_part[0-9]+|_peel[0-9]+|_selftest_rep[0-9]+)$", "", canonical, ignore.case = TRUE, perl = TRUE)
    canonical <- sub("_sc[-+]?[0-9.]+$", "", canonical, ignore.case = TRUE, perl = TRUE)
    if (nzchar(canonical)) canonical else folder_name
  }

  retrieve_archive_target_item <- function(target_dir, source_path, item, folder_name) {
    item_name <- basename(item)
    selftest_source <- identical(basename(normalizePath(source_path, winslash = "/", mustWork = FALSE)), "selftest")
    if (isTRUE(selftest_source) && file.info(item)$isdir) {
      return(file.path(target_dir, item_name, "selftest"))
    }

    profile_rel <- retrieve_profile_relative_path(source_path)
    if (nzchar(profile_rel)) {
      source_base <- basename(normalizePath(source_path, winslash = "/", mustWork = FALSE))
      if (grepl("^scalar_", source_base)) {
        return(file.path(target_dir, retrieve_canonical_job_folder(folder_name), profile_rel, item_name))
      }
      if (file.info(item)$isdir && grepl("^scalar_", item_name)) {
        return(file.path(target_dir, retrieve_canonical_job_folder(folder_name), profile_rel, item_name))
      }
    }

    canonical_folder <- retrieve_canonical_job_folder(folder_name)
    source_is_job_folder <- identical(
      basename(normalizePath(source_path, winslash = "/", mustWork = FALSE)),
      folder_name
    )
    if (isTRUE(source_is_job_folder) && !identical(canonical_folder, folder_name)) {
      return(file.path(target_dir, canonical_folder, item_name))
    }

    if (file.info(item)$isdir && identical(item_name, folder_name) && !identical(canonical_folder, folder_name)) {
      return(file.path(target_dir, canonical_folder))
    }

    file.path(target_dir, item_name)
  }
  
  download_and_extract_from_folder_raw <- function(spec, common) {
    folder_name <- spec$folder_name
    folder_path <- spec$folder_path
    download_dir <- common$download_dir
    extract_path <- common$extract_path
    remote_user <- common$remote_user
    remote_host <- common$remote_host
    rsync_opts <- common$rsync_opts
    
    find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", folder_path)
    ssh_find <- sprintf('ssh %s@%s "%s"', remote_user, remote_host, find_tar_cmd)
    tar_files <- system(ssh_find, intern = TRUE)
    if (length(tar_files) == 0) {
      return(list(ok = FALSE, message = "No tar.gz files found"))
    }
    
    extracted_any <- FALSE
    
    for (tar_file in tar_files) {
      tar_name <- basename(tar_file)
      
      temp_dir <- file.path(
        tempdir(),
        paste0("condor_sel_", gsub("[^a-zA-Z0-9]", "_", folder_name), "_", format(Sys.time(), "%H%M%S"))
      )
      if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
      
      remote_tar <- sprintf("%s@%s:%s", remote_user, remote_host, tar_file)
      local_tar <- file.path(temp_dir, tar_name)
      
      rsync_cmd <- sprintf('rsync %s %s %s', rsync_opts, shQuote(remote_tar), shQuote(local_tar))
      rsync_status <- system(rsync_cmd)
      if (rsync_status != 0) {
        unlink(temp_dir, recursive = TRUE)
        next
      }
      
      extract_cmd <- sprintf('tar -xzf %s -C %s', shQuote(local_tar), shQuote(temp_dir))
      extract_status <- system(extract_cmd)
      if (extract_status != 0) {
        unlink(temp_dir, recursive = TRUE)
        next
      }
      
      source_path <- retrieve_archive_source_path(temp_dir, extract_path, folder_name)
      
      if (dir.exists(source_path)) {
        job_child_path <- file.path(source_path, folder_name)
        if (dir.exists(job_child_path)) source_path <- job_child_path
        
        items_in_source <- list.files(source_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
        
        if (length(items_in_source) == 1 && file.info(items_in_source[1])$isdir) {
          single_folder_name <- basename(items_in_source[1])
          if (single_folder_name == folder_name) {
            items_in_source <- list.files(items_in_source[1], full.names = TRUE, all.files = TRUE, no.. = TRUE)
          }
        }
        
        if (length(items_in_source) > 0) {
          target_dir <- retrieve_archive_target_dir(download_dir, source_path)
          if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
          
          for (item in items_in_source) {
            target_item <- retrieve_archive_target_item(target_dir, source_path, item, folder_name)
            
            if (file.info(item)$isdir) {
              if (!dir.exists(target_item)) dir.create(target_item, recursive = TRUE)
              system(sprintf('rsync -a --ignore-existing %s %s', 
                             shQuote(file.path(item, "")), shQuote(file.path(target_item, ""))))
            } else {
              file.copy(item, target_item, overwrite = FALSE)
            }
          }
          
          extracted_any <- TRUE
        }
      }
      
      unlink(temp_dir, recursive = TRUE)
    }
    
    if (!extracted_any) {
      return(list(ok = FALSE, message = "No items extracted"))
    }
    list(ok = TRUE, message = "ok")
  }
  
  process_one_tar_raw <- function(tar_file, common) {
    tar_name <- basename(tar_file)
    folder_name <- basename(dirname(tar_file))
    extract_path <- common$extract_path
    download_dir <- common$download_dir
    remote_user <- common$remote_user
    remote_host <- common$remote_host
    rsync_opts <- common$rsync_opts
    
    temp_dir <- file.path(
      tempdir(),
      paste0("condor_all_", gsub("[^a-zA-Z0-9]", "_", folder_name), "_", format(Sys.time(), "%H%M%S"))
    )
    if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
    
    remote_tar <- sprintf("%s@%s:%s", remote_user, remote_host, tar_file)
    local_tar <- file.path(temp_dir, tar_name)
    
    rsync_cmd <- sprintf('rsync %s %s %s', rsync_opts, shQuote(remote_tar), shQuote(local_tar))
    rsync_status <- system(rsync_cmd)
    if (rsync_status != 0) {
      unlink(temp_dir, recursive = TRUE)
      return(list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " rsync failed")))
    }
    
    extract_cmd <- sprintf('tar -xzf %s -C %s', shQuote(local_tar), shQuote(temp_dir))
    extract_status <- system(extract_cmd)
    if (extract_status != 0) {
      unlink(temp_dir, recursive = TRUE)
      return(list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " extract failed")))
    }
    
    source_path <- retrieve_archive_source_path(temp_dir, extract_path, folder_name)
    
    if (dir.exists(source_path)) {
      job_child_path <- file.path(source_path, folder_name)
      if (dir.exists(job_child_path)) source_path <- job_child_path
      
      items_in_source <- list.files(source_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      
      if (length(items_in_source) == 1 && file.info(items_in_source[1])$isdir) {
        single_folder_name <- basename(items_in_source[1])
        if (single_folder_name == folder_name) {
          items_in_source <- list.files(items_in_source[1], full.names = TRUE, all.files = TRUE, no.. = TRUE)
        }
      }
      
      if (length(items_in_source) > 0) {
        target_dir <- retrieve_archive_target_dir(download_dir, source_path)
        if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
        
        for (item in items_in_source) {
          target_item <- retrieve_archive_target_item(target_dir, source_path, item, folder_name)
          
          if (file.info(item)$isdir) {
            if (!dir.exists(target_item)) dir.create(target_item, recursive = TRUE)
            system(sprintf('rsync -a --ignore-existing %s %s', 
                           shQuote(file.path(item, "")), shQuote(file.path(target_item, ""))))
          } else {
            file.copy(item, target_item, overwrite = FALSE)
          }
        }
      }
      
      unlink(temp_dir, recursive = TRUE)
      return(list(ok = TRUE, msg = paste(folder_name, "/", tar_name, " ok")))
    }
    
    unlink(temp_dir, recursive = TRUE)
    list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " path not found"))
  }
  
  download_and_extract_from_folder <- function(folder_name, folder_path, download_dir, extract_path, extract_subpath) {
    find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", folder_path)
    ssh_find <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_tar_cmd)
    
    tar_files <- system(ssh_find, intern = TRUE)
    if (length(tar_files) == 0) {
      return(list(ok = FALSE, message = "No tar.gz files found"))
    }
    
    extracted_any <- FALSE
    
    for (tar_file in tar_files) {
      tar_name <- basename(tar_file)
      
      rv$retrieval_log <- paste0(rv$retrieval_log, "  • ", tar_name, "\n")
      
      temp_dir <- file.path(
        tempdir(),
        paste0("condor_sel_", gsub("[^a-zA-Z0-9]", "_", folder_name), "_", format(Sys.time(), "%H%M%S"))
      )
      if (!dir.exists(temp_dir)) {
        dir.create(temp_dir, recursive = TRUE)
      }
      
      remote_tar <- sprintf("%s@%s:%s", input$remote_user, input$remote_host, tar_file)
      local_tar <- file.path(temp_dir, tar_name)
      
      rsync_cmd <- sprintf('rsync %s %s %s', rsync_base_opts(), shQuote(remote_tar), shQuote(local_tar))
      rsync_status <- system(rsync_cmd)
      if (rsync_status != 0) {
        rv$retrieval_log <- paste0(rv$retrieval_log, "    ❌ rsync failed\n")
        unlink(temp_dir, recursive = TRUE)
        next
      }
      
      extract_cmd <- sprintf('tar -xzf %s -C %s', shQuote(local_tar), shQuote(temp_dir))
      extract_status <- system(extract_cmd)
      if (extract_status != 0) {
        rv$retrieval_log <- paste0(rv$retrieval_log, "    ❌ extract failed\n")
        unlink(temp_dir, recursive = TRUE)
        next
      }
      
      source_path <- retrieve_archive_source_path(temp_dir, extract_path, folder_name)
      
      if (dir.exists(source_path)) {
        job_child_path <- file.path(source_path, folder_name)
        if (dir.exists(job_child_path)) {
          source_path <- job_child_path
        }
        
        items_in_source <- list.files(source_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
        
        if (length(items_in_source) == 1 && file.info(items_in_source[1])$isdir) {
          single_folder_name <- basename(items_in_source[1])
          if (single_folder_name == folder_name) {
            items_in_source <- list.files(items_in_source[1], full.names = TRUE, all.files = TRUE, no.. = TRUE)
          }
        }
        
        if (length(items_in_source) > 0) {
          target_dir <- retrieve_archive_target_dir(download_dir, source_path)
          if (!dir.exists(target_dir)) {
            dir.create(target_dir, recursive = TRUE)
          }
          
          for (item in items_in_source) {
            target_item <- retrieve_archive_target_item(target_dir, source_path, item, folder_name)
            
            if (file.info(item)$isdir) {
              if (!dir.exists(target_item)) {
                dir.create(target_item, recursive = TRUE)
              }
              # Merge contents without overwriting existing files
              system(sprintf('rsync -a --ignore-existing %s %s', 
                             shQuote(file.path(item, "")), shQuote(file.path(target_item, ""))))
            } else {
              file.copy(item, target_item, overwrite = FALSE)
            }
          }
          
          rv$retrieval_log <- paste0(rv$retrieval_log, "    ✓ Extracted to ", target_dir, "\n")
          extracted_any <- TRUE
        } else {
          rv$retrieval_log <- paste0(rv$retrieval_log, "    ⚠ No items under extract path\n")
        }
      } else {
        rv$retrieval_log <- paste0(rv$retrieval_log, "    ⚠ Path not found: ", extract_path, "\n")
      }
      
      unlink(temp_dir, recursive = TRUE)
    }
    
    if (!extracted_any) {
      return(list(ok = FALSE, message = "No items extracted"))
    }
    
    list(ok = TRUE, message = "ok")
  }
  
  
  # Download All - downloads entire directory without scanning individual folders
  observeEvent(input$download_all, {
    shinyjs::disable("download_all")
    rv$action_status$download_all <- "Starting..."
    scan_dir <- input$scan_output_dir
    
    if (is.null(scan_dir) || scan_dir == "") {
      showNotification("Please specify a remote output directory first", type = "warning")
      rv$action_status$download_all <- "Not started"
      shinyjs::enable("download_all")
      return()
    }
    
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      showNotification("Please specify a download location first", type = "warning")
      rv$action_status$download_all <- "Not started"
      shinyjs::enable("download_all")
      return()
    }
    download_dir <- resolve_repo_path(download_dir)
    
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      showNotification("Repository name not specified", type = "error")
      rv$action_status$download_all <- "Not started"
      shinyjs::enable("download_all")
      return()
    }
    
    extract_subpath <- trimws(input$extract_path_manual)
    if (extract_subpath == "") {
      extract_path <- repo_name
    } else {
      extract_path <- paste0(repo_name, "/", extract_subpath)
    }
    
    if (!dir.exists(download_dir)) {
      dir.create(download_dir, recursive = TRUE)
    }
    
    remote_path <- paste0(input$github_repo, "/", scan_dir)
    
    rv$retrieval_log <- paste0(Sys.time(), " - Download All Started\n",
                               "   Remote: ", remote_path, "\n",
                               "   Local: ", normalizePath(download_dir, mustWork = FALSE), "\n",
                               "   Extract path: ", extract_path, "\n\n")
    
    showNotification("Downloading all files... This may take a while", 
                     type = "message", duration = NULL, id = "download_all_progress")
    
    parallel_on <- isTRUE(input$parallel_download) && parallel_cores() > 1
    cores <- parallel_cores()
    rv$retrieval_log <- paste0(
      rv$retrieval_log,
      "Parallel download: ",
      if (parallel_on) "ON" else "OFF",
      " (cores: ", cores, ")\n"
    )
    showNotification(
      paste0("Download All started - parallel ", if (parallel_on) "ON" else "OFF", " (", cores, " cores)"),
      type = "message", duration = 3
    )

    tryCatch({
      find_cmd <- sprintf('find %s -maxdepth 1 -mindepth 1 -type d', remote_path)
      ssh_find <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      folder_paths <- system(ssh_find, intern = TRUE)
      
      if (length(folder_paths) == 0) {
        rv$retrieval_log <- paste0(rv$retrieval_log, "✗ No folders found\n")
        removeNotification("download_all_progress")
        showNotification("No folders found in directory", type = "warning")
        rv$action_status$download_all <- "0/0"
        shinyjs::enable("download_all")
        return()
      }

      rv$retrieval_log <- paste0(rv$retrieval_log, "✓ Found ", length(folder_paths), " folders\n\n")
      rv$action_status$download_all <- paste0("0/", length(folder_paths))
      
      specs <- lapply(folder_paths, function(p) {
        list(folder_name = basename(p), folder_path = p)
      })
      
      common <- list(
        remote_user = input$remote_user,
        remote_host = input$remote_host,
        extract_path = extract_path,
        download_dir = download_dir,
        rsync_opts = rsync_base_opts()
      )
      
      if (parallel_on) {
        cores <- parallel_cores()
        rv$retrieval_log <- paste0(rv$retrieval_log, "⚡ Parallel mode ON (cores: ", cores, ")\n")
        cl <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterExport(
          cl,
          varlist = c(
            "download_and_extract_from_folder_raw", "retrieve_archive_source_path",
            "retrieve_archive_target_dir", "retrieve_canonical_job_folder",
            "retrieve_archive_target_item", "common"
          ),
          envir = environment()
        )
        results <- parallel::parLapply(cl, specs, function(spec) download_and_extract_from_folder_raw(spec, common))
      } else {
        results <- list()
        for (spec in specs) {
          rv$retrieval_log <- paste0(rv$retrieval_log, "➡️ Folder: ", spec$folder_name, "\n")
          res <- download_and_extract_from_folder_raw(spec, common)
          results[[length(results) + 1]] <- res
          if (isTRUE(res$ok)) {
            rv$retrieval_log <- paste0(rv$retrieval_log, "  ✓ Done\n")
          } else {
            rv$retrieval_log <- paste0(rv$retrieval_log, "  ❌ Failed: ", res$message, "\n")
          }
        }
      }
      
      ok_count <- sum(vapply(results, function(x) isTRUE(x$ok), logical(1)))
      fail_count <- length(results) - ok_count
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "✓ Processed ", length(folder_paths), " folders (", 
                                 ok_count, " ok, ", fail_count, " failed)\n")
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "\n", strrep("=", 70), "\n",
                                 "✅ Download All Complete!\n",
                                 "   Processed: ", length(folder_paths), " folders\n",
                                 "   Location: ", normalizePath(download_dir, mustWork = FALSE), "\n",
                                 strrep("=", 70), "\n")
      
      removeNotification("download_all_progress")
      showNotification("✓ Download complete!", type = "message", duration = 5)
      rv$action_status$download_all <- paste0(length(folder_paths), "/", length(folder_paths), " done")
      shinyjs::enable("download_all")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "\n✗ ERROR: ", e$message, "\n")
      removeNotification("download_all_progress")
      showNotification(paste("Error:", e$message), type = "error")
      rv$action_status$download_all <- "Error"
      shinyjs::enable("download_all")
    })
  })
  
  observeEvent(input$scan_results, {
    shinyjs::disable("scan_results")
    rv$action_status$scan_results <- "Loading..."
    rv$retrieval_log <- paste0(Sys.time(), " - Scanning remote directory...\n")
    
    tryCatch({
      scan_path <- paste0(input$github_repo, "/", input$scan_output_dir)
      
      find_cmd <- sprintf(
        "find %s -maxdepth 1 -mindepth 1 -type d; echo '---'; find %s -mindepth 2 -maxdepth 2 -type f \\( -name '*.tar.gz' -o -name '*.tgz' \\) -printf '%%h\\n' | sort | uniq -c",
        scan_path, scan_path
      )
      cmd <- sprintf('ssh %s@%s "%s"', input$remote_user, input$remote_host, find_cmd)
      
      raw_out <- system(cmd, intern = TRUE)
      if (length(raw_out) == 0) raw_out <- c()
      
      sep_idx <- which(raw_out == "---")
      if (length(sep_idx) == 0) {
        folders <- raw_out
        counts <- character(0)
      } else {
        folders <- raw_out[1:(sep_idx[1] - 1)]
        counts <- raw_out[(sep_idx[1] + 1):length(raw_out)]
      }
      
      if (length(folders) == 0) {
        rv$folders_data <- data.frame()
        rv$selected_folders <- c()
        rv$retrieval_log <- paste0(rv$retrieval_log, "⚠ No folders found\n")
        showNotification("No folders found", type = "warning")
        rv$action_status$scan_results <- "0 folders"
        shinyjs::enable("scan_results")
        return()
      }
      
      count_map <- list()
      if (length(counts) > 0) {
        for (line in counts) {
          line <- trimws(line)
          if (nchar(line) == 0) next
          parts <- strsplit(line, "\\s+", perl = TRUE)[[1]]
          if (length(parts) < 2) next
          cnt <- as.numeric(parts[1])
          dir_path <- parts[length(parts)]
          count_map[[dir_path]] <- cnt
        }
      }
      
      folder_info <- lapply(folders, function(folder_path) {
        folder_name <- basename(folder_path)
        file_count <- count_map[[folder_path]]
        if (is.null(file_count) || is.na(file_count)) file_count <- 0
        data.frame(
          Folder = folder_name,
          Path = folder_path,
          TarGzFiles = file_count,
          stringsAsFactors = FALSE
        )
      })
      
      rv$folders_data <- do.call(rbind, folder_info)
      
      # Select all by default
      rv$selected_folders <- rv$folders_data$Folder
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "✓ Found ", nrow(rv$folders_data), " folders with ", 
                                 sum(rv$folders_data$TarGzFiles), " tar.gz files total\n",
                                 "All folders selected by default\n")
      showNotification(paste("Found", nrow(rv$folders_data), "folders (all selected)"), type = "message")
      rv$action_status$scan_results <- paste0(nrow(rv$folders_data), " folders")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
      rv$action_status$scan_results <- "Error"
    }, finally = {
      shinyjs::enable("scan_results")
    })
  })
  
  
  # ========== DELETE REMOTE DIRECTORY ==========
  observeEvent(input$delete_remote_dir, {
    remote_dir <- input$scan_output_dir
    
    if (is.null(remote_dir) || remote_dir == "") {
      showNotification("Please specify a remote directory", type = "warning")
      return()
    }
    
    full_path <- paste0(input$github_repo, "/", remote_dir)
    
    showModal(modalDialog(
      title = "Delete Remote Directory",
      p(strong("Delete entire directory:"), style = "margin-bottom: 10px;"),
      p(code(full_path), style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 13px;"),
      p(strong("This will delete ALL files and folders!"), 
        style = "color: #dc3545; margin-top: 15px;"),
      p("This action cannot be undone.", style = "color: #666;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_remote_dir", "Yes, Delete", 
                     class = "btn-danger", icon = icon("trash"))
      )
    ))
  })
  
  observeEvent(input$confirm_delete_remote_dir, {
    removeModal()
    shinyjs::disable("delete_remote_dir")
    rv$action_status$delete_remote_dir <- "1/1"
    
    remote_dir <- input$scan_output_dir
    full_path <- paste0(input$github_repo, "/", remote_dir)
    
    rv$retrieval_log <- paste0(
      "\n", strrep("=", 70), "\n",
      Sys.time(), " - DELETING REMOTE DIRECTORY\n",
      strrep("=", 70), "\n",
      "Target: ", full_path, "\n"
    )
    
    showNotification(
      paste("Deleting:", remote_dir),
      type = "warning",
      duration = NULL,
      id = "delete_progress"
    )
    
    tryCatch({
      delete_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", 
                            input$remote_user, 
                            input$remote_host, 
                            full_path)
      
      result <- system(delete_cmd, intern = FALSE)
      
      if (result == 0) {
        verify_cmd <- sprintf("ssh %s@%s '[ ! -d %s ] && echo DELETED || echo EXISTS'", 
                              input$remote_user, 
                              input$remote_host, 
                              full_path)
        
        verify_result <- system(verify_cmd, intern = TRUE)
        
        if (length(verify_result) > 0 && verify_result[1] == "DELETED") {
          rv$retrieval_log <- paste0(rv$retrieval_log,
                                     "✓ Successfully deleted\n",
                                     strrep("=", 70), "\n\n")
          
          rv$folders_data <- data.frame()
          rv$selected_folders <- c()
          
          removeNotification("delete_progress")
          showNotification("✓ Directory deleted", type = "message", duration = 3)
          rv$action_status$delete_remote_dir <- "1/1 done"
        } else {
          rv$retrieval_log <- paste0(rv$retrieval_log, "⚠ Verification failed\n\n")
          removeNotification("delete_progress")
          showNotification("Deletion may have failed", type = "warning", duration = 5)
          rv$action_status$delete_remote_dir <- "Verification failed"
        }
      } else {
        rv$retrieval_log <- paste0(rv$retrieval_log, "✗ Deletion failed\n\n")
        removeNotification("delete_progress")
        showNotification("Failed to delete", type = "error", duration = 5)
        rv$action_status$delete_remote_dir <- "Failed"
      }
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n\n")
      removeNotification("delete_progress")
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      rv$action_status$delete_remote_dir <- "Error"
    }, finally = {
      shinyjs::enable("delete_remote_dir")
    })
  })
  
  observeEvent(input$refresh_folders, {
    if (nrow(rv$folders_data) > 0) {
      showNotification("Refreshing folder list...", type = "message", duration = 1)
    }
  })
  
  # Render folders checkboxes
  output$folders_selection_ui <- renderUI({
    if (nrow(rv$folders_data) == 0) {
      return(p("No folders available. Click 'Scan Remote Directory' first.", 
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    folder_checkboxes <- lapply(1:nrow(rv$folders_data), function(i) {
      folder_name <- rv$folders_data$Folder[i]
      tar_count <- rv$folders_data$TarGzFiles[i]
      
      checkbox_id <- paste0("folder_check_", gsub("[^a-zA-Z0-9]", "_", folder_name))
      
      observeEvent(input[[checkbox_id]], {
        if (input[[checkbox_id]]) {
          if (!(folder_name %in% rv$selected_folders)) {
            rv$selected_folders <- c(rv$selected_folders, folder_name)
          }
        } else {
          rv$selected_folders <- rv$selected_folders[rv$selected_folders != folder_name]
        }
      }, ignoreInit = TRUE)
      
      div(
        class = "folder-checkbox-item",
        div(class = "folder-left-content",
            checkboxInput(
              checkbox_id,
              label = folder_name,
              value = folder_name %in% rv$selected_folders
            )
        ),
        span(class = "folder-files-count", paste(tar_count, "files"))
      )
    })
    
    div(
      class = "folders-selector-container",
      folder_checkboxes
    )
  })
  
  # Update folder checkboxes when selection changes
  observe({
    req(nrow(rv$folders_data) > 0)
    
    for (i in 1:nrow(rv$folders_data)) {
      folder_name <- rv$folders_data$Folder[i]
      checkbox_id <- paste0("folder_check_", gsub("[^a-zA-Z0-9]", "_", folder_name))
      is_selected <- folder_name %in% rv$selected_folders
      updateCheckboxInput(session, checkbox_id, value = is_selected)
    }
  })
  
  observeEvent(input$select_all_folders, {
    if (nrow(rv$folders_data) > 0) {
      rv$selected_folders <- rv$folders_data$Folder
    }
  })
  
  observeEvent(input$deselect_all_folders, {
    rv$selected_folders <- c()
  })
  
  observeEvent(input$preview_archives, {
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    rv$retrieval_log <- paste0(rv$retrieval_log, Sys.time(), " - Loading archive contents preview...\n")
    
    tryCatch({
      selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
      
      rv$archive_contents <- list()
      
      for (i in 1:nrow(selected_data)) {
        folder_name <- selected_data$Folder[i]
        folder_path <- selected_data$Path[i]
        
        rv$retrieval_log <- paste0(rv$retrieval_log, "\n📁 ", folder_name, ":\n")
        
        find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", 
                                folder_path)
        cmd <- sprintf('ssh %s@%s "%s"', 
                       input$remote_user, input$remote_host, find_tar_cmd)
        tar_files <- system(cmd, intern = TRUE)
        
        if (length(tar_files) == 0) next
        
        folder_contents <- list()
        
        for (tar_file in tar_files) {
          tar_name <- basename(tar_file)
          
          list_cmd <- sprintf('ssh %s@%s "tar -tzf %s"', 
                              input$remote_user, input$remote_host, tar_file)
          contents <- system(list_cmd, intern = TRUE)
          
          folder_contents[[tar_name]] <- contents
          
          rv$retrieval_log <- paste0(rv$retrieval_log, 
                                     "  ", tar_name, ": ", 
                                     length(contents), " items\n")
        }
        
        rv$archive_contents[[folder_name]] <- folder_contents
      }
      
      rv$retrieval_log <- paste0(rv$retrieval_log, "\n✅ Preview loaded\n")
      
      showModal(modalDialog(
        title = HTML(paste(icon("eye"), " Archive Contents Preview")),
        size = "l",
        
        p("Below is the complete folder structure inside the selected archives.", 
          style = "color: #666; font-size: 13px; margin-bottom: 15px;"),
        
        lapply(names(rv$archive_contents), function(folder_name) {
          folder_data <- rv$archive_contents[[folder_name]]
          
          tagList(
            h5(icon("folder"), " ", folder_name, style = "color: #3c8dbc; margin-top: 15px;"),
            
            lapply(names(folder_data), function(tar_name) {
              items <- folder_data[[tar_name]]
              
              div(
                style = "margin-left: 15px; margin-bottom: 10px;",
                strong(icon("file-archive"), " ", tar_name),
                div(
                  class = "archive-tree",
                  pre(paste(items[1:min(100, length(items))], collapse = "\n")),
                  if (length(items) > 100) {
                    p(paste("... and", length(items) - 100, "more items"), 
                      style = "color: #999; font-style: italic; margin: 5px 0;")
                  }
                )
              )
            })
          )
        }),
        
        footer = modalButton("Close")
      ))
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Download and extract
  observeEvent(input$fetch_selected, {
    # Validate folder selection
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    download_dir <- input$download_location
    if (is.null(download_dir) || download_dir == "") {
      showNotification("Please specify a download location first", type = "warning")
      return()
    }
    download_dir <- resolve_repo_path(download_dir)
    
    repo_name <- input$extract_repo_name
    if (is.null(repo_name) || repo_name == "") {
      repo_name <- input$github_repo
    }
    if (is.null(repo_name) || repo_name == "") {
      showNotification("Repository name not specified", type = "error")
      return()
    }
    
    extract_subpath <- trimws(input$extract_path_manual)
    if (extract_subpath == "") {
      extract_path <- repo_name
    } else {
      extract_path <- paste0(repo_name, "/", extract_subpath)
    }
    
    # Disable button during processing
    shinyjs::disable("fetch_selected")
    rv$action_status$download_selected <- paste0("0/", length(rv$selected_folders))
    
    total_folders <- length(rv$selected_folders)
    
    # Initialize retrieval log
    rv$retrieval_log <- paste0(
      rv$retrieval_log,
      "\n", Sys.time(), " - Starting download...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📦 Total folders to download: ", total_folders, "\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    # Show initial notification
    showNotification(
      paste0("📥 Downloading ", total_folders, " folder(s)..."),
      type = "message",
      duration = NULL,
      id = "download_progress"
    )
    
    parallel_on <- isTRUE(input$parallel_download) && parallel_cores() > 1
    cores <- parallel_cores()
    rv$retrieval_log <- paste0(
      rv$retrieval_log,
      "Parallel download: ",
      if (parallel_on) "ON" else "OFF",
      " (cores: ", cores, ")\n"
    )
    showNotification(
      paste0("Download Selected started - parallel ", if (parallel_on) "ON" else "OFF", " (", cores, " cores)"),
      type = "message", duration = 3
    )

    folder_map <- rv$folders_data
    specs <- lapply(rv$selected_folders, function(folder_name) {
      folder_row <- folder_map[folder_map$Folder == folder_name, ]
      if (nrow(folder_row) == 0) {
        return(list(folder_name = folder_name, folder_path = NA_character_))
      }
      list(folder_name = folder_name, folder_path = folder_row$Path[1])
    })

    tryCatch({
      common <- list(
        remote_user = input$remote_user,
        remote_host = input$remote_host,
        extract_path = extract_path,
        download_dir = download_dir,
        rsync_opts = rsync_base_opts()
      )
      
      if (parallel_on) {
        cores <- parallel_cores()
        rv$retrieval_log <- paste0(rv$retrieval_log, "⚡ Parallel mode ON (cores: ", cores, ")\n")
        cl <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterExport(
          cl,
          varlist = c(
            "download_and_extract_from_folder_raw", "retrieve_archive_source_path",
            "retrieve_archive_target_dir", "retrieve_canonical_job_folder",
            "retrieve_archive_target_item", "common"
          ),
          envir = environment()
        )
        results <- parallel::parLapply(cl, specs, function(spec) {
          if (is.na(spec$folder_path)) {
            return(list(ok = FALSE, message = "Folder not found in scanned results"))
          }
          download_and_extract_from_folder_raw(spec, common)
        })
      } else {
        results <- lapply(specs, function(spec) {
          if (is.na(spec$folder_path)) {
            return(list(ok = FALSE, message = "Folder not found in scanned results"))
          }
          rv$retrieval_log <- paste0(
            rv$retrieval_log,
            "➡️ Folder: ", spec$folder_name, "\n"
          )
          download_and_extract_from_folder_raw(spec, common)
        })
      }
      
      success_count <- sum(vapply(results, function(x) isTRUE(x$ok), logical(1)))
      failed_count <- length(results) - success_count
      
      # Final completion message
      rv$retrieval_log <- paste0(
        rv$retrieval_log,
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        sprintf("✅ Download complete: %d succeeded, %d failed\n", 
                success_count, failed_count),
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
      )
      
      showNotification(
        paste0("✅ Downloaded ", success_count, "/", total_folders, " folder(s)"),
        type = "message",
        duration = 5,
        id = "download_progress"
      )
      rv$action_status$download_selected <- paste0(total_folders, "/", total_folders, " done")
      
      if (failed_count > 0) {
        showNotification(
          paste0("⚠️ ", failed_count, " folder(s) failed to download"),
          type = "warning",
          duration = 5
        )
      }
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(
        rv$retrieval_log,
        "\n❌ ERROR: ", e$message, "\n"
      )
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      rv$action_status$download_selected <- "Error"
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("fetch_selected")
    })
  })
  
  
  observeEvent(input$delete_selected, {
    # Validate folder selection
    if (length(rv$selected_folders) == 0) {
      showNotification("No folders selected", type = "warning")
      return()
    }
    
    # Confirmation dialog
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste0("Delete ", length(rv$selected_folders), " selected folder(s) from remote server?"),
      tags$ul(
        lapply(rv$selected_folders, function(f) tags$li(f))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_folders", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_folders, {
    removeModal()
    
    # Disable button during processing
    shinyjs::disable("delete_selected")
    
    total_folders <- length(rv$selected_folders)
    rv$action_status$delete_selected <- paste0("0/", total_folders)
    
    # Initialize retrieval log
    rv$retrieval_log <- paste0(
      rv$retrieval_log,
      "\n", Sys.time(), " - Starting deletion...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "🗑️ Total folders to delete: ", total_folders, "\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    # Show initial notification
    showNotification(
      paste0("🗑️ Deleting ", total_folders, " folder(s)..."),
      type = "message",
      duration = NULL,
      id = "delete_folder_progress"
    )
    
    tryCatch({
      success_count <- 0
      failed_count <- 0
      
      for (i in seq_along(rv$selected_folders)) {
        folder_name <- rv$selected_folders[i]
        rv$action_status$delete_selected <- paste0(i, "/", total_folders)
        
        # Update progress in log
        rv$retrieval_log <- paste0(
          rv$retrieval_log,
          sprintf("[%d/%d] 🗑️ Deleting: %s\n", i, total_folders, folder_name)
        )
        
        # Update notification with progress
        showNotification(
          paste0("🗑️ Deleting ", i, "/", total_folders, ": ", folder_name),
          type = "message",
          duration = 2,
          id = "delete_folder_progress"
        )
        
        # Perform deletion operation
        result <- tryCatch({
          remote_path <- file.path(input$scan_output_dir, folder_name)
          cmd <- sprintf("ssh %s@%s 'rm -rf %s'",
                         input$remote_user, input$remote_host, remote_path)
          system(cmd, intern = TRUE)
          TRUE
        }, error = function(e) {
          rv$retrieval_log <- paste0(
            rv$retrieval_log,
            sprintf("  ❌ ERROR: %s\n", e$message)
          )
          FALSE
        })
        
        if (result) {
          success_count <- success_count + 1
          rv$retrieval_log <- paste0(
            rv$retrieval_log,
            sprintf("  ✓ Deleted successfully\n\n")
          )
        } else {
          failed_count <- failed_count + 1
        }
      }
      
      # Final completion message
      rv$retrieval_log <- paste0(
        rv$retrieval_log,
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        sprintf("✅ Deletion complete: %d succeeded, %d failed\n", 
                success_count, failed_count),
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
      )
      
      showNotification(
        paste0("✅ Deleted ", success_count, "/", total_folders, " folder(s)"),
        type = "message",
        duration = 5,
        id = "delete_folder_progress"
      )
      rv$action_status$delete_selected <- paste0(total_folders, "/", total_folders, " done")
      
      # Refresh folder list
      shinyjs::click("scan_results")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "\n❌ ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      rv$action_status$delete_selected <- "Error"
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("delete_selected")
    })
  })
  
  
  observeEvent(input$confirm_delete_results, {
    removeModal()
    
    selected_data <- rv$folders_data[rv$folders_data$Folder %in% rv$selected_folders, ]
    rv$retrieval_log <- paste0(Sys.time(), " - Deleting selected folders...\n")
    
    tryCatch({
      for (i in 1:nrow(selected_data)) {
        folder_name <- selected_data$Folder[i]
        folder_path <- selected_data$Path[i]
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "\n📁 Deleting folder: ", folder_name, "\n")
        
        delete_cmd <- sprintf('ssh %s@%s "rm -rf %s"', 
                              input$remote_user, 
                              input$remote_host, 
                              folder_path)
        
        system(delete_cmd)
        
        rv$retrieval_log <- paste0(rv$retrieval_log, 
                                   "  ✓ Deleted: ", folder_name, "\n")
      }
      
      rv$folders_data <- rv$folders_data[!(rv$folders_data$Folder %in% rv$selected_folders), ]
      rv$selected_folders <- c()
      
      rv$retrieval_log <- paste0(rv$retrieval_log, 
                                 "\n✅ Deletion complete!\n")
      showNotification("Deletion complete!", type = "message")
      
    }, error = function(e) {
      rv$retrieval_log <- paste0(rv$retrieval_log, "ERROR: ", e$message, "\n")
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Retrieval log output
  output$retrieval_log <- renderText({
    rv$retrieval_log
  })
  
