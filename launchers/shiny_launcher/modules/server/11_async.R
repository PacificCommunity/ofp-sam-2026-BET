  # ========== ASYNC TASKS (LAUNCH / RETRIEVE) ==========
  
  async_root <- file.path(tempdir(), "shiny_launcher_async")
  if (!dir.exists(async_root)) dir.create(async_root, recursive = TRUE)
  
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan(future::multisession, workers = max(1, parallel::detectCores() - 2))
  }
  
  async_task_init <- function(task) {
    id <- paste0(task, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1))
    task_dir <- file.path(async_root, id)
    if (!dir.exists(task_dir)) dir.create(task_dir, recursive = TRUE)
    list(
      id = id,
      dir = task_dir,
      log_path = file.path(task_dir, "log.txt"),
      status_path = file.path(task_dir, "status.rds"),
      progress_path = file.path(task_dir, "progress.log")
    )
  }
  
  write_status <- function(path, status, message = NULL, total = NULL, done = NULL) {
    saveRDS(list(status = status, message = message, time = Sys.time(), total = total, done = done), path)
  }
  
  append_log <- function(path, text) {
    cat(text, file = path, append = TRUE)
  }
  
  read_status <- function(path) {
    if (!is.null(path) && file.exists(path)) {
      return(readRDS(path))
    }
    NULL
  }
  
  read_progress_count <- function(path) {
    if (!is.null(path) && file.exists(path)) {
      return(length(readLines(path, warn = FALSE)))
    }
    0
  }
  
  read_log_text <- function(path) {
    if (!is.null(path) && file.exists(path)) {
      return(paste(readLines(path, warn = FALSE), collapse = "\n"))
    }
    ""
  }
  
  run_launch_task <- function(params, log_path, status_path) {
    append_log(log_path, paste0(Sys.time(), " - Async launch started\n"))
    progress_path <- params$progress_path
    write_status(status_path, "running")
    
    launch_one <- function(spec, common_params) {
      model_env_list <- common_params$model_env_lists[[spec$model_name]]
      if (is.null(model_env_list)) {
        stop(paste("Model env not found for", spec$model_name))
      }
      job_env <- list2env(model_env_list, parent = emptyenv())
      job_env$DOCKER_IMAGE <- common_params$docker_image
      remote_dir_suffix <- paste0(spec$model_name, "_model")
      batch_suffix <- ""
      
      if (!is.null(spec$seed)) {
        job_env$jitter_seed <- as.character(spec$seed)
        remote_dir_suffix <- paste0(spec$model_name, "_seed", spec$seed, "_model")
        batch_suffix <- paste0("-jitter", spec$seed)
      } else if (!is.null(spec$part)) {
        job_env$hessian_part <- as.character(spec$part)
        remote_dir_suffix <- paste0(spec$model_name, "_part", spec$part, "_model")
        batch_suffix <- paste0("-hess", spec$part)
      } else if (!is.null(spec$peel)) {
        job_env$retro_peel <- as.character(spec$peel)
        remote_dir_suffix <- paste0(spec$model_name, "_peel", spec$peel, "_model")
        batch_suffix <- paste0("-retro", spec$peel)
      } else if (!is.null(spec$scaler)) {
        job_env$scaler <- as.character(spec$scaler)
        remote_dir_suffix <- paste0(spec$model_name, "_sc", spec$scaler, "_model")
        batch_suffix <- paste0("-sc", spec$scaler)
      }
      
      remote_dir <- paste0(common_params$github_repo, "/", common_params$output_dir, "/", remote_dir_suffix)
      batch_name <- paste0(
        spec$model_name,
        batch_suffix,
        "-",
        format(Sys.time(), "%H:%M:%S"),
        "-",
        Sys.getpid()
      )
      
      work_dir <- file.path(
        tempdir(),
        paste0("condorbox_", Sys.getpid(), "_", gsub("[^a-zA-Z0-9]", "_", spec$model_name))
      )
      if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)
      old_wd <- getwd()
      setwd(work_dir)
      on.exit(setwd(old_wd), add = TRUE)
      
      CondorBox::CondorBox(
        make_options = spec$job_type, 
        remote_user = common_params$remote_user, 
        remote_host = common_params$remote_host,
        remote_dir = remote_dir, 
        github_pat = common_params$github_pat, 
        github_username = common_params$github_username,
        github_org = common_params$github_org, 
        github_repo = common_params$github_repo, 
        docker_image = common_params$docker_image,
        condor_cpus = common_params$condor_cpus,
        condor_memory = common_params$condor_memory,
        condor_disk = common_params$condor_disk,
        stream_error = "TRUE", 
        branch = common_params$branch, 
        rmclone_script = "no", 
        ghcr_login = common_params$ghcr_login,
        exclude_slots = common_params$exclude_slots,
        custom_batch_name = batch_name, 
        condor_environment = as.list(job_env, all.names = TRUE)
      )
      
      list(batch_name = batch_name, remote_dir = remote_dir)
    }
    
    job_specs <- params$job_specs
    common_params <- params$common_params
    total_jobs <- length(job_specs)
    
    append_log(log_path, paste0("Total jobs: ", total_jobs, "\n"))
    write_status(status_path, "running", total = total_jobs, done = 0)
    
    append_progress <- function() {
      cat("1\n", file = progress_path, append = TRUE)
    }
    
    results <- NULL
    if (isTRUE(params$parallel_launch) && total_jobs > 1 && params$cores > 1) {
      append_log(log_path, paste0("Parallel launch ON (cores: ", params$cores, ")\n"))
      cl <- parallel::makeCluster(params$cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, { library(CondorBox) })
      parallel::clusterExport(cl, varlist = c("launch_one", "common_params", "progress_path"), envir = environment())
      results <- parallel::parLapply(cl, job_specs, function(spec) {
        tryCatch({
          res <- launch_one(spec, common_params)
          cat("1\n", file = progress_path, append = TRUE)
          res
        }, error = function(e) {
          cat("1\n", file = progress_path, append = TRUE)
          list(batch_name = NA_character_, remote_dir = NA_character_, error = e$message)
        })
      })
    } else {
      append_log(log_path, "Parallel launch OFF\n")
      results <- lapply(job_specs, function(spec) {
        tryCatch({
          res <- launch_one(spec, common_params)
          append_progress()
          write_status(status_path, "running", total = total_jobs, done = read_progress_count(progress_path))
          res
        }, error = function(e) {
          append_progress()
          write_status(status_path, "running", total = total_jobs, done = read_progress_count(progress_path))
          list(batch_name = NA_character_, remote_dir = NA_character_, error = e$message)
        })
      })
    }
    
    ok_count <- sum(vapply(results, function(x) is.null(x$error), logical(1)))
    err_msgs <- unique(vapply(results, function(x) if (is.null(x$error)) "" else x$error, character(1)))
    err_msgs <- err_msgs[nzchar(err_msgs)]
    
    append_log(log_path, paste0("Completed: ", ok_count, "/", total_jobs, "\n"))
    if (length(err_msgs) > 0) {
      append_log(log_path, "Errors:\n")
      append_log(log_path, paste0("  - ", err_msgs, collapse = "\n"))
      append_log(log_path, "\n")
    }
    
    write_status(status_path, "done", total = total_jobs, done = read_progress_count(progress_path))
    TRUE
  }
  
  run_retrieve_task <- function(params, log_path, status_path) {
    append_log(log_path, paste0(Sys.time(), " - Async retrieve started (", params$mode, ")\n"))
    append_log(log_path, paste0("Download dir: ", params$download_dir, "\n"))
    write_status(status_path, "running")
    
    rsync_opts <- params$rsync_opts
    remote_user <- params$remote_user
    remote_host <- params$remote_host
    extract_path <- params$extract_path
    download_dir <- params$download_dir
    progress_path <- params$progress_path
    ssh_opts <- "-o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=10 -o ServerAliveCountMax=2"
    rsync_rsh <- paste("ssh", ssh_opts)
    
    log_checkpoint <- function(msg) {
      append_log(log_path, paste0(Sys.time(), " - ", msg, "\n"))
    }
    
    append_progress <- function() {
      cat("1\n", file = progress_path, append = TRUE)
    }
    
    download_and_extract_from_folder_raw <- function(spec) {
      folder_name <- spec$folder_name
      folder_path <- spec$folder_path
      
      find_tar_cmd <- sprintf("find %s -maxdepth 1 \\( -name '*.tar.gz' -o -name '*.tgz' \\)", folder_path)
      ssh_find <- sprintf('ssh %s %s@%s \"%s\" 2>&1', ssh_opts, remote_user, remote_host, find_tar_cmd)
      log_checkpoint(paste0("Finding tars in: ", folder_path))
      tar_files <- system(ssh_find, intern = TRUE)
      status <- attr(tar_files, "status")
      if (!is.null(status) && status != 0) {
        append_log(log_path, paste0("SSH find failed (status ", status, "): ", paste(tar_files, collapse = " | "), "\n"))
      }
      if (length(tar_files) > 0 && any(grepl("Permission denied|Could not resolve|Connection timed out|No route to host", tar_files))) {
        append_log(log_path, paste0("SSH error: ", paste(tar_files, collapse = " | "), "\n"))
      }
      if (length(tar_files) == 0) {
        append_progress()
        return(list(ok = FALSE, message = "No tar.gz files found"))
      }
      log_checkpoint(paste0("Found ", length(tar_files), " tar(s) in ", folder_name))
      
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
        
        rsync_cmd <- sprintf('rsync -e \"%s\" --contimeout=10 %s %s %s', rsync_rsh, rsync_opts, shQuote(remote_tar), shQuote(local_tar))
        rsync_status <- system(rsync_cmd)
        if (rsync_status != 0) {
          append_log(log_path, paste0("rsync failed for ", tar_name, "\n"))
          unlink(temp_dir, recursive = TRUE)
          next
        }
        
        extract_cmd <- sprintf('tar -xzf %s -C %s', shQuote(local_tar), shQuote(temp_dir))
        extract_status <- system(extract_cmd)
        if (extract_status != 0) {
          unlink(temp_dir, recursive = TRUE)
          next
        }
        
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
            target_dir <- download_dir
            if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
            
            for (item in items_in_source) {
              item_name <- basename(item)
              target_item <- file.path(target_dir, item_name)
              
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
        append_progress()
        return(list(ok = FALSE, message = "No items extracted"))
      }
      append_progress()
      list(ok = TRUE, message = "ok")
    }
    
    process_one_tar_raw <- function(tar_file) {
      tar_name <- basename(tar_file)
      folder_name <- basename(dirname(tar_file))
      
      temp_dir <- file.path(
        tempdir(),
        paste0("condor_all_", gsub("[^a-zA-Z0-9]", "_", folder_name), "_", format(Sys.time(), "%H%M%S"))
      )
      if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
      
      remote_tar <- sprintf("%s@%s:%s", remote_user, remote_host, tar_file)
      local_tar <- file.path(temp_dir, tar_name)
      
      rsync_cmd <- sprintf('rsync -e \"%s\" --contimeout=10 %s %s %s', rsync_rsh, rsync_opts, shQuote(remote_tar), shQuote(local_tar))
      rsync_status <- system(rsync_cmd)
      if (rsync_status != 0) {
        append_log(log_path, paste0("rsync failed for ", tar_name, "\n"))
        append_progress()
        unlink(temp_dir, recursive = TRUE)
        return(list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " rsync failed")))
      }
      
      extract_cmd <- sprintf('tar -xzf %s -C %s', shQuote(local_tar), shQuote(temp_dir))
      extract_status <- system(extract_cmd)
      if (extract_status != 0) {
        unlink(temp_dir, recursive = TRUE)
        return(list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " extract failed")))
      }
      
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
          target_dir <- download_dir
          if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
          
          for (item in items_in_source) {
            item_name <- basename(item)
            target_item <- file.path(target_dir, item_name)
            
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
        append_progress()
        return(list(ok = TRUE, msg = paste(folder_name, "/", tar_name, " ok")))
      }
      
      unlink(temp_dir, recursive = TRUE)
      append_progress()
      list(ok = FALSE, msg = paste(folder_name, "/", tar_name, " path not found"))
    }
    
    append_log(
      log_path,
      paste0(
        "Parallel download: ",
        if (isTRUE(params$parallel_download) && params$cores > 1) "ON" else "OFF",
        " (cores: ",
        params$cores,
        ")\n"
      )
    )
    
    results <- NULL
    if (params$mode == "download_all") {
      find_cmd <- sprintf('find %s -name \"*.tar.gz\" -o -name \"*.tgz\"', params$remote_path)
      ssh_find <- sprintf('ssh %s %s@%s \"%s\" 2>&1', ssh_opts, remote_user, remote_host, find_cmd)
      log_checkpoint(paste0("Finding all tars under: ", params$remote_path))
      tar_files <- system(ssh_find, intern = TRUE)
      status <- attr(tar_files, "status")
      if (!is.null(status) && status != 0) {
        append_log(log_path, paste0("SSH find failed (status ", status, "): ", paste(tar_files, collapse = " | "), "\n"))
      }
      if (length(tar_files) > 0 && any(grepl("Permission denied|Could not resolve|Connection timed out|No route to host", tar_files))) {
        append_log(log_path, paste0("SSH error: ", paste(tar_files, collapse = " | "), "\n"))
      }
      if (length(tar_files) == 0) {
        append_log(log_path, "No tar.gz files found\n")
        write_status(status_path, "done", total = 0, done = 0)
        return(TRUE)
      }
      write_status(status_path, "running", total = length(tar_files), done = 0)
      
      if (isTRUE(params$parallel_download) && params$cores > 1) {
        cl <- parallel::makeCluster(params$cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterExport(cl, varlist = c("process_one_tar_raw", "remote_user", "remote_host", "extract_path", "download_dir", "rsync_opts"), envir = environment())
        results <- parallel::parLapply(cl, tar_files, process_one_tar_raw)
      } else {
        results <- lapply(tar_files, process_one_tar_raw)
      }
      
      ok_count <- sum(vapply(results, function(x) isTRUE(x$ok), logical(1)))
      append_log(log_path, paste0("Processed ", length(tar_files), " archives (", ok_count, " ok)\n"))
    } else if (params$mode == "download_selected") {
      specs <- params$folder_specs
      write_status(status_path, "running", total = length(specs), done = 0)
      log_checkpoint(paste0("Selected folders: ", length(specs)))
      if (isTRUE(params$parallel_download) && params$cores > 1) {
        cl <- parallel::makeCluster(params$cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterExport(cl, varlist = c("download_and_extract_from_folder_raw", "remote_user", "remote_host", "extract_path", "download_dir", "rsync_opts"), envir = environment())
        results <- parallel::parLapply(cl, specs, download_and_extract_from_folder_raw)
      } else {
        results <- lapply(specs, function(spec) {
          log_checkpoint(paste0("Processing folder: ", spec$folder_name))
          download_and_extract_from_folder_raw(spec)
        })
      }
      
      ok_count <- sum(vapply(results, function(x) isTRUE(x$ok), logical(1)))
      append_log(log_path, paste0("Downloaded ", ok_count, "/", length(specs), " folders\n"))
    }
    
    write_status(status_path, "done", total = read_status(status_path)$total, done = read_progress_count(progress_path))
    TRUE
  }
  
  start_async_launch <- function(params) {
    if (isTRUE(rv$async$launch$running)) {
      showNotification("Launch already running", type = "warning")
      return()
    }
    task <- async_task_init("launch")
    rv$async$launch <- list(
      running = TRUE,
      id = task$id,
      status_path = task$status_path,
      log_path = task$log_path,
      progress_path = task$progress_path,
      started_at = Sys.time()
    )
    rv$launch_log <- paste0(Sys.time(), " - Launch started in background\n")
    write_status(task$status_path, "running")
    append_log(task$log_path, rv$launch_log)
    
    shinyjs::disable("launch_btn")
    
    params$progress_path <- task$progress_path
    params$progress_path <- task$progress_path
    future::future(
      tryCatch(
        run_launch_task(params, task$log_path, task$status_path),
        error = function(e) {
          append_log(task$log_path, paste0("ERROR: ", e$message, "\n"))
          write_status(task$status_path, "error", e$message)
          FALSE
        }
      ),
      globals = list(run_launch_task = run_launch_task, params = params, log_path = task$log_path, status_path = task$status_path)
    )
  }
  
  start_async_retrieve <- function(params) {
    if (isTRUE(rv$async$retrieve$running)) {
      showNotification("Retrieve already running", type = "warning")
      return()
    }
    task <- async_task_init("retrieve")
    rv$async$retrieve <- list(
      running = TRUE,
      id = task$id,
      status_path = task$status_path,
      log_path = task$log_path,
      progress_path = task$progress_path,
      mode = params$mode,
      started_at = Sys.time()
    )
    rv$retrieval_log <- paste0(Sys.time(), " - Retrieve started in background\n")
    write_status(task$status_path, "running")
    append_log(task$log_path, rv$retrieval_log)
    
    if (params$mode == "download_all") shinyjs::disable("download_all")
    if (params$mode == "download_selected") shinyjs::disable("fetch_selected")
    if (params$mode == "download_all") rv$action_status$download_all <- "Running (background)"
    if (params$mode == "download_selected") rv$action_status$download_selected <- "Running (background)"
    
    params$progress_path <- task$progress_path
    future::future(
      tryCatch(
        run_retrieve_task(params, task$log_path, task$status_path),
        error = function(e) {
          append_log(task$log_path, paste0("ERROR: ", e$message, "\n"))
          write_status(task$status_path, "error", e$message)
          FALSE
        }
      ),
      globals = list(run_retrieve_task = run_retrieve_task, params = params, log_path = task$log_path, status_path = task$status_path)
    )
  }
  
  observe({
    invalidateLater(1000, session)
    
    # Launch polling
    if (isTRUE(rv$async$launch$running)) {
      rv$launch_log <- read_log_text(rv$async$launch$log_path)
      st <- read_status(rv$async$launch$status_path)
      if (!is.null(st) && !is.null(st$total)) {
        done <- read_progress_count(rv$async$launch$progress_path)
        write_status(rv$async$launch$status_path, st$status, st$message, total = st$total, done = done)
      }
      if (!is.null(st) && st$status %in% c("done", "error")) {
        rv$async$launch$running <- FALSE
        shinyjs::enable("launch_btn")
        shinyjs::removeClass("launch_btn", "loading")
        if (st$status == "done") {
          showNotification("Launch completed (background)", type = "message", duration = 3)
        } else {
          showNotification("Launch failed (background)", type = "error", duration = 5)
        }
      }
    }
    
    # Retrieve polling
    if (isTRUE(rv$async$retrieve$running)) {
      rv$retrieval_log <- read_log_text(rv$async$retrieve$log_path)
      if (!is.null(rv$async$retrieve$started_at)) {
        elapsed <- round(as.numeric(difftime(Sys.time(), rv$async$retrieve$started_at, units = "secs")))
        if (rv$async$retrieve$mode == "download_all") {
          rv$action_status$download_all <- paste0("Running (", elapsed, "s)")
        }
        if (rv$async$retrieve$mode == "download_selected") {
          rv$action_status$download_selected <- paste0("Running (", elapsed, "s)")
        }
      }
      st <- read_status(rv$async$retrieve$status_path)
      if (!is.null(st) && !is.null(st$total)) {
        done <- read_progress_count(rv$async$retrieve$progress_path)
        write_status(rv$async$retrieve$status_path, st$status, st$message, total = st$total, done = done)
      }
      if (!is.null(st) && st$status %in% c("done", "error")) {
        if (rv$async$retrieve$mode == "download_all") shinyjs::enable("download_all")
        if (rv$async$retrieve$mode == "download_selected") shinyjs::enable("fetch_selected")
        if (rv$async$retrieve$mode == "download_all") removeNotification("download_all_progress")
        if (rv$async$retrieve$mode == "download_selected") removeNotification("download_progress")
        rv$async$retrieve$running <- FALSE
        if (rv$async$retrieve$mode == "download_all") rv$action_status$download_all <- "Done"
        if (rv$async$retrieve$mode == "download_selected") rv$action_status$download_selected <- "Done"
        if (st$status == "done") {
          showNotification("Retrieve completed (background)", type = "message", duration = 3)
        } else {
          showNotification("Retrieve failed (background)", type = "error", duration = 5)
        }
      }
    }
  })
