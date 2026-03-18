  # ========== LAUNCH JOB HANDLERS ==========

  local_env_strings <- function(job_env) {
    env_names <- names(job_env)
    if (is.null(env_names) || length(env_names) == 0) {
      return(character(0))
    }
    vals <- vapply(job_env, function(x) {
      if (length(x) == 0 || is.null(x)) {
        ""
      } else {
        paste(as.character(x), collapse = " ")
      }
    }, character(1))
    paste0(env_names, "=", shQuote(vals))
  }

  local_job_runner <- function(job_type) {
    switch(
      job_type,
      model = "runners/run_model.R",
      jitter = "runners/run_jitter.R",
      hessian = "runners/run_hessian.R",
      retro = "runners/run_retro.R",
      prof = "runners/run_prof.R",
      prof_chain = "runners/run_prof_chain.R",
      stop("Unsupported local job type: ", job_type)
    )
  }

  parse_prof_target_map <- function(txt) {
    out <- list()
    if (is.null(txt) || !nzchar(trimws(txt))) return(out)
    parts <- trimws(unlist(strsplit(as.character(txt), ",")))
    parts <- parts[nzchar(parts)]
    for (p in parts) {
      kv <- trimws(unlist(strsplit(p, ":", fixed = TRUE)))
      if (length(kv) < 2) next
      k <- suppressWarnings(as.numeric(kv[1]))
      v <- paste(kv[-1], collapse = ":")
      if (is.finite(k) && nzchar(v)) out[[format(k, scientific = FALSE, trim = TRUE)]] <- v
    }
    out
  }

  parse_prof_target_scalars <- function(txt) {
    mp <- parse_prof_target_map(txt)
    if (length(mp) == 0) return(numeric(0))
    keys <- suppressWarnings(as.numeric(names(mp)))
    keys[is.finite(keys)]
  }

  parse_first_nonempty_numeric <- function(...) {
    items <- list(...)
    for (it in items) {
      vals <- parse_numeric_tokens(it)
      if (length(vals) > 0) return(vals)
    }
    numeric(0)
  }

  first_scalar_string <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    txt <- trimws(as.character(x[[1]]))
    if (!nzchar(txt)) default else txt
  }

  sanitize_profile_job_tag <- function(x) {
    txt <- trimws(paste(as.character(x), collapse = "_"))
    if (!nzchar(txt)) return("")
    txt <- gsub("[^A-Za-z0-9]+", "_", txt)
    txt <- gsub("^_+|_+$", "", txt)
    txt
  }

  resolve_profile_job_envs <- function(model_env) {
    base_env <- as.list(model_env, all.names = TRUE)
    base_env$profile_sets <- NULL

    raw_sets <- model_env$profile_sets
    if (is.null(raw_sets) || length(raw_sets) == 0) {
      return(list(base_env))
    }

    set_names <- names(raw_sets)
    if (is.null(set_names)) set_names <- rep("", length(raw_sets))

    out <- list()
    for (i in seq_along(raw_sets)) {
      spec <- raw_sets[[i]]
      if (!is.list(spec)) next

      enabled_raw <- spec$enabled
      enabled <- TRUE
      if (!is.null(enabled_raw) && length(enabled_raw) > 0) {
        enabled_chr <- tolower(trimws(as.character(enabled_raw[[1]])))
        enabled <- !(identical(enabled_raw[[1]], FALSE) || enabled_chr %in% c("0", "false", "no", "off"))
      }
      if (!enabled) next

      spec_name <- trimws(as.character(set_names[[i]]))
      if (!nzchar(spec_name) && !is.null(spec$name) && length(spec$name) > 0) {
        spec_name <- trimws(as.character(spec$name[[1]]))
      }

      env <- modifyList(base_env, spec)
      env$profile_sets <- NULL
      env$enabled <- NULL
      env$name <- NULL
      env$label <- NULL
      env$key <- NULL

      label <- if (!is.null(spec$label) && length(spec$label) > 0 && nzchar(trimws(as.character(spec$label[[1]])))) {
        trimws(as.character(spec$label[[1]]))
      } else if (nzchar(spec_name)) {
        spec_name
      } else if (!is.null(env$prof_fix_indepvar) && length(env$prof_fix_indepvar) > 0 && nzchar(trimws(as.character(env$prof_fix_indepvar[[1]])))) {
        trimws(as.character(env$prof_fix_indepvar[[1]]))
      } else {
        "standard"
      }

      key_txt <- if (!is.null(spec$key) && length(spec$key) > 0 && nzchar(trimws(as.character(spec$key[[1]])))) {
        trimws(as.character(spec$key[[1]]))
      } else {
        label
      }

      env$profile_set_name <- label
      env$profile_set_label <- label
      env$profile_set_tag <- sanitize_profile_job_tag(key_txt)
      out[[length(out) + 1L]] <- env
    }

    if (length(out) == 0) list(base_env) else out
  }

  profile_launch_name <- function(model_name, profile_env = NULL) {
    if (is.null(profile_env)) return(model_name)
    set_name <- first_scalar_string(profile_env$profile_set_name, default = "")
    if (!nzchar(set_name)) return(model_name)
    paste0(model_name, " [", set_name, "]")
  }

  resolve_prof_scalars <- function(model_env) {
    default_scalars <- parse_first_nonempty_numeric(
      model_env$scalars,
      model_env$scalar,
      model_env$prof_scalars,
      model_env$profile_scalars,
      model_env$Scalars
    )
    map_scalars <- unique(c(
      parse_prof_target_scalars(model_env$init_from_scalar_map),
      parse_prof_target_scalars(model_env$init_par_override_map)
    ))
    map_scalars <- sort(unique(map_scalars[is.finite(map_scalars)]))
    if (length(map_scalars) > 0) {
      return(map_scalars)
    }
    default_scalars
  }

  resolve_prof_scalars_for_mode <- function(model_env, prof_chain_mode = FALSE) {
    # In sequential-chain mode, always use explicit profile scalars.
    if (isTRUE(prof_chain_mode)) {
      return(parse_first_nonempty_numeric(
        model_env$scalars,
        model_env$scalar,
        model_env$prof_scalars,
        model_env$profile_scalars,
        model_env$Scalars
      ))
    }
    resolve_prof_scalars(model_env)
  }

  resolve_prof_anchor_and_chains <- function(scalars, anchor) {
    scalars <- sort(unique(scalars[is.finite(scalars)]))
    if (length(scalars) == 0) {
      return(list(anchor = NA_real_, lower = numeric(0), upper = numeric(0)))
    }
    anchor <- suppressWarnings(as.numeric(anchor))
    if (!is.finite(anchor)) anchor <- 100
    anchor_eff <- scalars[which.min(abs(scalars - anchor))]
    lower <- sort(scalars[scalars < anchor_eff], decreasing = TRUE)
    upper <- sort(scalars[scalars > anchor_eff], decreasing = FALSE)
    list(anchor = anchor_eff, lower = lower, upper = upper)
  }

  apply_prof_init_mapping <- function(job_env, scalar_value) {
    sc <- suppressWarnings(as.numeric(scalar_value))
    if (!is.finite(sc)) return(job_env)
    sc_key <- format(sc, scientific = FALSE, trim = TRUE)

    override_map <- parse_prof_target_map(job_env$init_par_override_map)
    donor_map <- parse_prof_target_map(job_env$init_from_scalar_map)

    ov <- override_map[[sc_key]]
    if (!is.null(ov) && nzchar(ov)) {
      job_env$init_par_override <- ov
      return(job_env)
    }

    dn <- donor_map[[sc_key]]
    if (!is.null(dn) && nzchar(dn)) {
      job_env$init_from_scalar <- dn
    }
    job_env
  }

  local_run_command <- function(spec, common_params, job_env, log_file) {
    if (isTRUE(common_params$local_use_docker)) {
      uid <- tryCatch(system2("id", "-u", stdout = TRUE), error = function(e) character(0))
      gid <- tryCatch(system2("id", "-g", stdout = TRUE), error = function(e) character(0))
      env_values <- vapply(job_env, function(x) {
        if (length(x) == 0 || is.null(x)) "" else paste(as.character(x), collapse = " ")
      }, character(1))
      env_args <- unlist(
        Map(function(k, v) c("-e", shQuote(paste0(k, "=", v))), names(env_values), env_values),
        use.names = FALSE
      )
      user_args <- if (length(uid) == 1 && nzchar(uid) && length(gid) == 1 && nzchar(gid)) {
        c("--user", shQuote(paste0(uid, ":", gid)))
      } else {
        character(0)
      }
      list(
        command = "docker",
        args = c(
          "run", "--rm",
          user_args,
          "-v", shQuote(paste0(common_params$repo_root, ":/workspace")),
          "-w", shQuote("/workspace"),
          env_args,
          shQuote(common_params$docker_image),
          "Rscript",
          shQuote(local_job_runner(spec$job_type))
        ),
        env = character(0),
        log_file = log_file
      )
    } else {
      list(
        command = "make",
        args = spec$job_type,
        env = local_env_strings(job_env),
        log_file = log_file
      )
    }
  }

  local_command_preview <- function(run_spec) {
    args <- if (length(run_spec$args) > 0) paste(shQuote(run_spec$args), collapse = " ") else ""
    paste(trimws(paste(run_spec$command, args)), collapse = " ")
  }

  default_condor_exclude_slots <- function() {
    c(
      "slot1@nouofpcand27", "slot1@nouofpcand28", "slot1@nouofpcand29", "slot1@nouofpcand30",
      "slot1_1@suvofpcand26.corp.spc.int", "slot1_2@suvofpcand26.corp.spc.int", "slot1_3@suvofpcand26.corp.spc.int"
    )
  }

  parse_condor_exclude_patterns <- function(raw_pattern) {
    if (is.null(raw_pattern) || !nzchar(trimws(raw_pattern))) {
      return(character(0))
    }
    pats <- trimws(unlist(strsplit(raw_pattern, "[,\n;]+")))
    pats[nzchar(pats)]
  }

  fetch_condor_slot_table <- function(remote_user, remote_host) {
    if (is.null(remote_user) || !nzchar(trimws(remote_user)) || is.null(remote_host) || !nzchar(trimws(remote_host))) {
      return(data.frame(slot = character(0), machine = character(0), stringsAsFactors = FALSE))
    }

    ssh_target <- sprintf("%s@%s", trimws(remote_user), trimws(remote_host))
    slot_lines <- tryCatch(
      suppressWarnings(system2(
        "ssh",
        args = c("-o", "BatchMode=yes", "-o", "ConnectTimeout=8", ssh_target, "condor_status -af Name Machine"),
        stdout = TRUE,
        stderr = FALSE,
        timeout = 20
      )),
      error = function(e) character(0)
    )
    if (length(slot_lines) == 0) {
      return(data.frame(slot = character(0), machine = character(0), stringsAsFactors = FALSE))
    }

    slot_names <- trimws(sub("\\s+.*$", "", slot_lines))
    machine_names <- trimws(sub("^\\S+\\s*", "", slot_lines))
    keep <- nzchar(slot_names)
    data.frame(
      slot = slot_names[keep],
      machine = machine_names[keep],
      stringsAsFactors = FALSE
    )
  }

  match_condor_slots_by_patterns <- function(slot_table, patterns) {
    if (!is.data.frame(slot_table) || nrow(slot_table) == 0 || length(patterns) == 0) {
      return(character(0))
    }
    matched <- vapply(seq_len(nrow(slot_table)), function(i) {
      target <- paste(slot_table$slot[[i]], slot_table$machine[[i]], sep = " ")
      any(vapply(patterns, function(pat) grepl(pat, target, ignore.case = TRUE, perl = TRUE), logical(1)))
    }, logical(1))
    unique(slot_table$slot[matched])
  }

  condor_target_patterns <- function(run_target) {
    target <- if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
    switch(
      target,
      nouofp = c("nouofp", "nou"),
      suvofp = c("suvofp", "suv"),
      character(0)
    )
  }

  build_condor_exclude_slots <- function(remote_user, remote_host, run_target) {
    base_exclude <- default_condor_exclude_slots()
    patterns <- condor_target_patterns(run_target = run_target)

    slot_table <- fetch_condor_slot_table(remote_user = remote_user, remote_host = remote_host)
    all_slots <- unique(slot_table$slot)
    matched_slots <- match_condor_slots_by_patterns(slot_table, patterns)
    if (length(all_slots) == 0 || length(matched_slots) == 0) {
      return(list(
        exclude_slots = base_exclude,
        matched_slots = matched_slots,
        all_slots = all_slots,
        target_mode = if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
      ))
    }
    additional_exclude <- setdiff(all_slots, matched_slots)
    list(
      exclude_slots = unique(c(base_exclude, additional_exclude)),
      matched_slots = matched_slots,
      all_slots = all_slots,
      target_mode = if (!is.null(run_target) && nzchar(run_target)) run_target else "all"
    )
  }

  launch_single_job_local_raw <- function(spec, common_params) {
    model_env_list <- if (!is.null(spec$profile_env) && is.list(spec$profile_env)) spec$profile_env else common_params$model_env_lists[[spec$model_name]]
    if (is.null(model_env_list)) {
      stop(paste("Model env not found for", spec$model_name))
    }

    job_env <- model_env_list
    batch_suffix <- ""
    profile_tag <- if (identical(spec$job_type, "prof") || identical(spec$job_type, "prof_chain")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""

    if (!is.null(spec$seed)) {
      job_env$jitter_seed <- as.character(spec$seed)
      batch_suffix <- paste0("-jitter", spec$seed)
    } else if (!is.null(spec$part)) {
      job_env$hessian_part <- as.character(spec$part)
      batch_suffix <- paste0("-hess", spec$part)
    } else if (!is.null(spec$peel)) {
      job_env$retro_peel <- as.character(spec$peel)
      batch_suffix <- paste0("-retro", spec$peel)
    } else if (!is.null(spec$scalar)) {
      job_env$scalar <- as.character(spec$scalar)
      if (identical(spec$job_type, "prof")) {
        job_env <- apply_prof_init_mapping(job_env, spec$scalar)
        if (!is.null(spec$init_from_scalar_override) && is.finite(suppressWarnings(as.numeric(spec$init_from_scalar_override)))) {
          job_env$init_from_scalar <- as.character(spec$init_from_scalar_override)
        }
      }
      batch_suffix <- paste0(profile_suffix, "-sc", spec$scalar)
    } else if (identical(spec$job_type, "prof_chain")) {
      if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) {
        job_env$chain_name <- as.character(spec$chain_name)
      }
      if (!is.null(spec$chain_scalars) && nzchar(as.character(spec$chain_scalars))) {
        job_env$chain_scalars <- as.character(spec$chain_scalars)
      }
      if (!is.null(spec$chain_first_init_from) && nzchar(as.character(spec$chain_first_init_from))) {
        job_env$chain_first_init_from <- as.character(spec$chain_first_init_from)
      }
      if (!is.null(spec$chain_anchor) && nzchar(as.character(spec$chain_anchor))) {
        job_env$chain_anchor <- as.character(spec$chain_anchor)
      }
      batch_suffix <- paste0(profile_suffix, "-profchain", if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) paste0("-", as.character(spec$chain_name)) else "")
    }

    batch_name <- paste0(spec$model_name, batch_suffix, "-local-", format(Sys.time(), "%H:%M:%S"), "-", Sys.getpid())
    log_dir <- file.path(common_params$repo_root, "logs", "shiny_launcher_local")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(log_dir, paste0("shiny_launcher_local_", gsub("[^A-Za-z0-9_\\-]", "_", batch_name), ".log"))

    if (!dir.exists(common_params$repo_root)) {
      stop("Repo root does not exist for local run: ", common_params$repo_root)
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(common_params$repo_root)

    run_spec <- local_run_command(spec, common_params, job_env, log_file)

    writeLines(
      c(
        paste("mode:", if (isTRUE(common_params$local_use_docker)) "docker" else "native"),
        paste("job_type:", spec$job_type),
        paste("model_name:", spec$model_name)
      ),
      con = log_file
    )

    run_spec <- local_run_command(spec, common_params, job_env, log_file)
    write(paste("command:", local_command_preview(run_spec)), file = log_file, append = TRUE)
    write("", file = log_file, append = TRUE)

    exit_status <- tryCatch(
      system2(
        run_spec$command,
        args = run_spec$args,
        env = run_spec$env,
        stdout = run_spec$log_file,
        stderr = run_spec$log_file
      ),
      error = function(e) {
        structure(127L, message = conditionMessage(e))
      }
    )

    if (!is.numeric(exit_status) || length(exit_status) != 1 || is.na(exit_status)) {
      exit_status <- 127L
    }
    if (!identical(as.integer(exit_status), 0L)) {
      output <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
      cmd_error <- attr(exit_status, "message")
      stop(
        paste(
          c(
            sprintf("Local %s run failed for %s (status %s).", spec$job_type, spec$model_name, exit_status),
            if (!is.null(cmd_error) && nzchar(cmd_error)) paste("Command error:", cmd_error),
            paste("Log file:", log_file),
            tail(as.character(output), 20)
          ),
          collapse = "\n"
        )
      )
    }

    list(
      batch_name = batch_name,
      remote_dir = if (!is.null(job_env$model_dir)) as.character(job_env$model_dir) else "",
      job_id = batch_name,
      mode = "local",
      log_file = log_file,
      model_name = spec$model_name,
      job_type = spec$job_type,
      job_env = job_env
    )
  }

  launch_single_job_local <- function(model_name, model_env, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL, log = TRUE) {
    display_name <- if (identical(job_type, "prof") || identical(job_type, "prof_chain")) profile_launch_name(model_name, model_env) else model_name
    if (isTRUE(log)) {
      rv$launch_log <- paste0(
        rv$launch_log,
        "  → ",
        display_name,
        if (!is.null(seed)) paste0(" seed ", seed) else if (!is.null(part)) paste0(" part ", part) else if (!is.null(peel)) paste0(" peel ", peel) else if (!is.null(scalar)) paste0(" scalar ", scalar) else "",
        " [local",
        if (identical(input$launch_mode, "local_docker")) ", docker" else ", native",
        "]\n"
      )
    }

    common_params <- list(
      repo_root = isolate(repo_root_val()),
      local_use_docker = identical(input$launch_mode, "local_docker"),
      docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
      model_env_lists = setNames(list(as.list(model_env, all.names = TRUE)), model_name)
    )

    launch_single_job_local_raw(
      spec = list(
        model_name = model_name,
        job_type = job_type,
        seed = seed,
        part = part,
        peel = peel,
        scalar = scalar
      ),
      common_params = common_params
    )
  }

  parse_numeric_tokens <- function(x) {
    txt <- paste(as.character(x), collapse = " ")
    if (is.null(x) || !nzchar(trimws(txt))) return(numeric(0))
    m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", txt, perl = TRUE)
    toks <- regmatches(txt, m)[[1]]
    vals <- suppressWarnings(as.numeric(toks))
    vals[is.finite(vals)]
  }

  collapse_model_field <- function(x) {
    if (is.null(x) || length(x) == 0) return("NA")
    if (is.list(x)) return(paste(vapply(x, collapse_model_field, character(1)), collapse = " | "))
    vals <- as.character(x)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) "NA" else paste(vals, collapse = " ")
  }

  build_model_config_details <- function(model_names) {
    if (length(model_names) == 0) return("No selected models.")
    sections <- lapply(model_names, function(model_name) {
      model_env <- rv$models[[model_name]]
      if (is.null(model_env) || !is.list(model_env)) {
        return(paste0(model_name, "\n  <model config not found>"))
      }
      fields <- names(model_env)
      if (is.null(fields) || length(fields) == 0) {
        return(paste0(model_name, "\n  <empty model config>"))
      }
      field_lines <- vapply(fields, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(model_env[[nm]]))
      }, character(1))
      paste(c(model_name, field_lines), collapse = "\n")
    })
    paste(sections, collapse = "\n\n")
  }

  build_single_job_config_block <- function(job_result) {
    if (is.null(job_result)) return("NA")
    model_name <- if (!is.null(job_result$model_name) && nzchar(as.character(job_result$model_name))) as.character(job_result$model_name) else "NA"
    job_type <- if (!is.null(job_result$job_type) && nzchar(as.character(job_result$job_type))) as.character(job_result$job_type) else "NA"
    batch_name <- if (!is.null(job_result$batch_name) && nzchar(as.character(job_result$batch_name))) as.character(job_result$batch_name) else "NA"
    env <- job_result$job_env
    if (is.null(env) || !is.list(env) || length(env) == 0) {
      env_lines <- "  <job env not captured>"
    } else {
      env_names_all <- names(env)
      env_names_all <- env_names_all[!is.na(env_names_all) & nzchar(env_names_all)]

      common_fields <- c(
        "description",
        "config_summary",
        "profile_set_name",
        "base_dir",
        "model_dir",
        "program_path",
        "mfcl_commands",
        "Reps",
        "scalars",
        "min_year",
        "n_mixing_periods",
        "DOCKER_IMAGE"
      )
    type_fields <- switch(
      job_type,
      model = c("model_hessian"),
      retro = c("retro_peel", "retro_peels", "retro_hessian"),
      jitter = c("jitter_seed", "jitter_cv", "jitter_seeds", "jitter_hessian", "jitter_base_source"),
      hessian = c("hessian_part", "nsplit", "model_hessian"),
      prof = c("profile_set_name", "scalar", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "init_from_scalar", "init_par_override", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
      prof_chain = c("profile_set_name", "chain_name", "chain_anchor", "chain_scalars", "chain_first_init_from", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
      character(0)
      )

      preferred_order <- unique(c(common_fields, type_fields))
      env_names <- intersect(preferred_order, env_names_all)
      if (length(env_names) == 0) {
        env_names <- sort(env_names_all)
      }

      env_lines <- vapply(env_names, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(env[[nm]]))
      }, character(1))
    }
    paste(c(
      paste0(model_name, " [", job_type, "]"),
      paste0("  batch_name : ", batch_name),
      env_lines
    ), collapse = "\n")
  }

  build_launched_config_details <- function(job_results) {
    if (length(job_results) == 0) return("No launched jobs.")

    common_fields <- c(
      "description",
      "config_summary",
      "profile_set_name",
      "base_dir",
      "model_dir",
      "program_path",
      "mfcl_commands",
      "Reps",
      "scalars",
      "min_year",
      "n_mixing_periods",
      "DOCKER_IMAGE"
    )
    type_fields_for <- function(job_type) {
      switch(
        job_type,
        model = c("model_hessian"),
        retro = c("retro_peel", "retro_peels", "retro_hessian"),
        jitter = c("jitter_seed", "jitter_cv", "jitter_seeds", "jitter_hessian", "jitter_base_source"),
        hessian = c("hessian_part", "nsplit", "model_hessian"),
        prof = c("profile_set_name", "scalar", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "init_from_scalar", "init_par_override", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
        prof_chain = c("profile_set_name", "chain_name", "chain_anchor", "chain_scalars", "chain_first_init_from", "scalars", "prof_hessian", "prof_init_map_rds", "init_from_scalar_map", "init_par_override_map", "prof_fix_indepvar", "prof_fix_values", "prof_fix_indepvar_file", "indepvar_reps", "prof_extra_switch"),
        character(0)
      )
    }
    format_env_lines <- function(env, fields) {
      if (is.null(env) || !is.list(env) || length(env) == 0) return(character(0))
      env_names <- names(env)
      env_names <- env_names[!is.na(env_names) & nzchar(env_names)]
      keep <- intersect(fields, env_names)
      vapply(keep, function(nm) {
        paste0("  ", nm, " : ", collapse_model_field(env[[nm]]))
      }, character(1))
    }

    valid <- Filter(function(x) !is.null(x) && is.list(x), job_results)
    if (length(valid) == 0) return("No launched jobs.")

    model_keys <- vapply(valid, function(x) {
      if (!is.null(x$model_name) && nzchar(as.character(x$model_name))) as.character(x$model_name) else "NA"
    }, character(1))
    grouped <- split(valid, model_keys)

    sections <- lapply(names(grouped), function(model_name) {
      jobs <- grouped[[model_name]]
      first_env <- NULL
      for (j in jobs) {
        if (!is.null(j$job_env) && is.list(j$job_env) && length(j$job_env) > 0) {
          first_env <- j$job_env
          break
        }
      }
      common_lines <- format_env_lines(first_env, common_fields)
      if (length(common_lines) == 0) {
        common_lines <- "  <common config not captured>"
      }

      job_blocks <- vapply(jobs, function(j) {
        jt <- if (!is.null(j$job_type) && nzchar(as.character(j$job_type))) as.character(j$job_type) else "NA"
        bn <- if (!is.null(j$batch_name) && nzchar(as.character(j$batch_name))) as.character(j$batch_name) else "NA"
        env_use <- j$job_env
        if ((is.null(env_use) || !is.list(env_use) || length(env_use) == 0) &&
            !is.null(j$spec) && is.list(j$spec) && length(j$spec) > 0) {
          env_use <- j$spec
        }
        type_lines <- format_env_lines(env_use, type_fields_for(jt))
        if (length(type_lines) == 0) {
          if (!is.null(env_use) && is.list(env_use) && length(env_use) > 0) {
            env_names_all <- names(env_use)
            env_names_all <- env_names_all[!is.na(env_names_all) & nzchar(env_names_all)]
            fallback_fields <- setdiff(env_names_all, common_fields)
            if (length(fallback_fields) > 0) {
              type_lines <- vapply(fallback_fields, function(nm) {
                paste0("  ", nm, " : ", collapse_model_field(env_use[[nm]]))
              }, character(1))
            } else {
              type_lines <- "  <no job-type specific config>"
            }
          } else {
            type_lines <- "  <no job-type specific config>"
          }
        }
        if (identical(jt, "prof_chain")) {
          has_chain <- any(grepl("^\\s+chain_(name|anchor|scalars|first_init_from)\\s*:", type_lines))
          if (!has_chain) {
            chain_nm <- if (!is.null(j$spec$chain_name) && nzchar(as.character(j$spec$chain_name))) {
              as.character(j$spec$chain_name)
            } else if (grepl("profchain-up", bn, fixed = TRUE)) {
              "up"
            } else if (grepl("profchain-down", bn, fixed = TRUE)) {
              "down"
            } else {
              "NA"
            }
            chain_sc <- if (!is.null(j$spec$chain_scalars) && nzchar(as.character(j$spec$chain_scalars))) as.character(j$spec$chain_scalars) else "NA"
            chain_init <- if (!is.null(j$spec$chain_first_init_from) && nzchar(as.character(j$spec$chain_first_init_from))) as.character(j$spec$chain_first_init_from) else "NA"
            chain_anchor <- if (!is.null(j$spec$chain_anchor) && nzchar(as.character(j$spec$chain_anchor))) as.character(j$spec$chain_anchor) else "NA"
            type_lines <- c(
              paste0("  chain_name : ", chain_nm),
              paste0("  chain_anchor : ", chain_anchor),
              paste0("  chain_scalars : ", chain_sc),
              paste0("  chain_first_init_from : ", chain_init),
              type_lines
            )
          }
        }
        paste(c(
          paste0(model_name, " [", jt, "]"),
          paste0("  batch_name : ", bn),
          type_lines
        ), collapse = "\n")
      }, character(1))

      paste(
        c(
          paste0(model_name, " [common]"),
          common_lines,
          "",
          paste(job_blocks, collapse = "\n\n----------------------------------------\n\n")
        ),
        collapse = "\n"
      )
    })

    paste(sections, collapse = "\n\n========================================\n\n")
  }

  selected_models_from_checkboxes <- function() {
    if (length(rv$models) == 0) return(character(0))
    names(rv$models)[vapply(names(rv$models), function(model_name) {
      checkbox_id <- paste0("model_check_", gsub("[^a-zA-Z0-9]", "_", model_name))
      isTRUE(input[[checkbox_id]])
    }, logical(1))]
  }

  estimate_total_jobs <- function(selected_models, selected_job_types, prof_chain_mode = FALSE, is_local_mode = FALSE, prof_anchor_requested = 100, local_prof_chain_parallel = FALSE) {
    total_jobs <- 0L
    if (length(selected_models) == 0 || length(selected_job_types) == 0) return(total_jobs)

    for (model_name in selected_models) {
      model_env <- rv$models[[model_name]]
      if (is.null(model_env)) next
      for (job_type in selected_job_types) {
        if (identical(job_type, "jitter")) {
          total_jobs <- total_jobs + length(parse_numeric_tokens(model_env$jitter_seeds))
        } else if (identical(job_type, "hessian")) {
          nsplit <- suppressWarnings(as.integer(model_env$nsplit))
          if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
          total_jobs <- total_jobs + nsplit
        } else if (identical(job_type, "retro")) {
          total_jobs <- total_jobs + length(parse_numeric_tokens(model_env$retro_peels))
        } else if (identical(job_type, "prof")) {
          for (prof_env in resolve_profile_job_envs(model_env)) {
            scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
            if (isTRUE(prof_chain_mode) && (!isTRUE(is_local_mode) || isTRUE(local_prof_chain_parallel))) {
              plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
              down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
              up_chain <- plan$upper
              if (length(down_chain) > 0) total_jobs <- total_jobs + 1L
              if (length(up_chain) > 0) total_jobs <- total_jobs + 1L
            } else {
              total_jobs <- total_jobs + length(scalars)
            }
          }
        } else {
          total_jobs <- total_jobs + 1L
        }
      }
    }
    as.integer(total_jobs)
  }

  output$estimated_jobs_text <- renderText({
    if (length(rv$models) == 0) return("0 (load config first)")
    selected_job_types <- input$job_types
    if (is.null(selected_job_types) || length(selected_job_types) == 0) return("0 (select job type)")
    selected_models <- selected_models_from_checkboxes()
    if (length(selected_models) == 0) return("0 (select model)")
    launch_mode <- if (!is.null(input$launch_mode) && nzchar(input$launch_mode)) input$launch_mode else "condor"
    is_local_mode <- identical(launch_mode, "local_native") || identical(launch_mode, "local_docker")
    prof_chain_mode <- identical(input$prof_launch_strategy, "seq_anchor_bidir") && "prof" %in% selected_job_types
    prof_anchor_requested <- suppressWarnings(as.numeric(input$prof_anchor_scalar))
    if (!is.finite(prof_anchor_requested)) prof_anchor_requested <- 100

    est <- estimate_total_jobs(
      selected_models,
      selected_job_types,
      prof_chain_mode = prof_chain_mode,
      is_local_mode = is_local_mode,
      prof_anchor_requested = prof_anchor_requested,
      local_prof_chain_parallel = isTRUE(input$parallel_launch)
    )

    if (isTRUE(prof_chain_mode) && !isTRUE(is_local_mode) && length(selected_models) == 1 && "prof" %in% selected_job_types) {
      m <- selected_models[[1]]
      me <- rv$models[[m]]
      prof_envs <- resolve_profile_job_envs(me)
      if (length(prof_envs) == 1) {
        sc <- resolve_prof_scalars_for_mode(prof_envs[[1]], prof_chain_mode = TRUE)
        plan <- resolve_prof_anchor_and_chains(sc, prof_anchor_requested)
        down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
        up_chain <- plan$upper
        return(sprintf(
          "%d [seq:%s raw=%s class=%s direct=%s parsed=%s anchor=%s req=%s down=%d up=%d]",
          as.integer(est),
          m,
          paste(as.character(me$scalars), collapse = "|"),
          paste(class(me$scalars), collapse = "/"),
          {
            dvals <- parse_numeric_tokens(me$scalars)
            if (length(dvals) > 0) paste(dvals, collapse = ",") else "<none>"
          },
          if (length(sc) > 0) paste(sc, collapse = ",") else "<none>",
          ifelse(is.finite(plan$anchor), format(plan$anchor, scientific = FALSE, trim = TRUE), "NA"),
          format(prof_anchor_requested, scientific = FALSE, trim = TRUE),
          length(down_chain),
          length(up_chain)
        ))
      }

      prof_desc <- vapply(prof_envs, function(env) {
        nm <- first_scalar_string(env$profile_set_name, default = "profile")
        sc <- resolve_prof_scalars_for_mode(env, prof_chain_mode = TRUE)
        sprintf("%s:%d", nm, length(sc))
      }, character(1))
      return(sprintf("%d [seq:%s multi-profile %s]", as.integer(est), m, paste(prof_desc, collapse = "; ")))
    }

    as.character(est)
  })
  
  observeEvent(input$launch_btn, {
    if (length(rv$models) == 0) { 
      showNotification("Please load models first", type = "error")
      return() 
    }
    
    # Recompute selected models from current checkbox inputs
    selected_models <- selected_models_from_checkboxes()
    rv$selected_models <- selected_models
    
    # Validate model selection
    if (length(selected_models) == 0) { 
      showNotification("Please select at least one model", type = "error")
      return() 
    }
    
    # Disable button during processing
    shinyjs::disable("launch_btn")
    shinyjs::addClass("launch_btn", "loading")
    
    selected_job_types <- input$job_types
    if (is.null(selected_job_types) || length(selected_job_types) == 0) {
      showNotification("Please select at least one job type", type = "error")
      shinyjs::enable("launch_btn")
      shinyjs::removeClass("launch_btn", "loading")
      return()
    }

    launch_mode <- if (!is.null(input$launch_mode) && nzchar(input$launch_mode)) input$launch_mode else "condor"
    is_local_mode <- launch_mode %in% c("local_native", "local_docker")
    is_local_docker <- identical(launch_mode, "local_docker")
    prof_chain_mode <- identical(input$prof_launch_strategy, "seq_anchor_bidir") && "prof" %in% selected_job_types
    allow_local_prof_chain_parallel <- isTRUE(prof_chain_mode) && isTRUE(is_local_mode) && isTRUE(input$parallel_launch)
    prof_anchor_requested <- suppressWarnings(as.numeric(input$prof_anchor_scalar))
    if (!is.finite(prof_anchor_requested)) prof_anchor_requested <- 100
    
    # Calculate total number of jobs to be launched
    total_jobs <- 0
    job_specs <- list()
  add_job_spec <- function(model_name, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL,
                           chain_name = NULL, chain_scalars = NULL, chain_first_init_from = NULL, chain_anchor = NULL,
                           profile_env = NULL) {
      job_specs[[length(job_specs) + 1]] <<- list(
        model_name = model_name,
        job_type = job_type,
        seed = seed,
        part = part,
        peel = peel,
        scalar = scalar,
        chain_name = chain_name,
        chain_scalars = chain_scalars,
        chain_first_init_from = chain_first_init_from,
        chain_anchor = chain_anchor,
        profile_env = profile_env
      )
    }
    for (model_name in selected_models) {
      model_env <- rv$models[[model_name]]
      for (job_type in selected_job_types) {
        if (job_type == "jitter") {
          seeds <- parse_numeric_tokens(model_env$jitter_seeds)
          total_jobs <- total_jobs + length(seeds)
          for (seed in seeds) {
            add_job_spec(model_name, job_type, seed = seed)
          }
        } else if (job_type == "hessian") {
          nsplit <- suppressWarnings(as.integer(model_env$nsplit))
          if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
          total_jobs <- total_jobs + nsplit
          for (part in seq_len(nsplit)) {
            add_job_spec(model_name, job_type, part = part)
          }
        } else if (job_type == "retro") {
          peels <- parse_numeric_tokens(model_env$retro_peels)
          total_jobs <- total_jobs + length(peels)
          for (peel in peels) {
            add_job_spec(model_name, job_type, peel = peel)
          }
        } else if (job_type == "prof") {
          for (prof_env in resolve_profile_job_envs(model_env)) {
            scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
            if (isTRUE(prof_chain_mode) && (!is_local_mode || isTRUE(allow_local_prof_chain_parallel))) {
              plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
              down_chain <- c(if (is.finite(plan$anchor)) plan$anchor else numeric(0), plan$lower)
              up_chain <- plan$upper
              if (length(down_chain) > 0) {
                total_jobs <- total_jobs + 1L
                add_job_spec(
                  model_name = model_name,
                  job_type = "prof_chain",
                  chain_name = "down",
                  chain_scalars = paste(down_chain, collapse = ","),
                  chain_first_init_from = NULL,
                  chain_anchor = as.character(plan$anchor),
                  profile_env = prof_env
                )
              }
              if (length(up_chain) > 0) {
                total_jobs <- total_jobs + 1L
                add_job_spec(
                  model_name = model_name,
                  job_type = "prof_chain",
                  chain_name = "up",
                  chain_scalars = paste(up_chain, collapse = ","),
                  chain_first_init_from = as.character(plan$anchor),
                  chain_anchor = as.character(plan$anchor),
                  profile_env = prof_env
                )
              }
            } else {
              total_jobs <- total_jobs + length(scalars)
              for (sc in scalars) {
                add_job_spec(model_name, job_type, scalar = sc, profile_env = prof_env)
              }
            }
          }
        } else {
          total_jobs <- total_jobs + 1
          add_job_spec(model_name, job_type)
        }
      }
    }
    
    model_env_lists <- lapply(selected_models, function(m) {
      as.list(rv$models[[m]], all.names = TRUE)
    })
    names(model_env_lists) <- selected_models

    action_word <- if (is_local_mode) "run" else "launch"
    completion_word <- if (is_local_mode) "completed" else "submitted"
    progress_prefix <- if (is_local_mode) "Running local" else "Launching"
    effective_output_dir <- if (is_local_mode) {
      paste(unique(vapply(selected_models, function(m) {
        md <- rv$models[[m]]$model_dir
        if (is.null(md) || !nzchar(md)) m else as.character(md)
      }, character(1))), collapse = ", ")
    } else {
      input$output_dir
    }
    condor_target <- "all"
    condor_target_info <- list(
      exclude_slots = default_condor_exclude_slots(),
      matched_slots = character(0),
      all_slots = character(0),
      target_mode = "all"
    )
    if (identical(launch_mode, "condor")) {
      condor_target <- if (!is.null(input$condor_run_target) && nzchar(input$condor_run_target)) input$condor_run_target else "all"
      condor_target_info <- build_condor_exclude_slots(
        remote_user = input$remote_user,
        remote_host = input$remote_host,
        run_target = condor_target
      )
    }
    condor_exclude_slots <- condor_target_info$exclude_slots

    # Initialize log with total job count
    rv$launch_log <- paste0(
      Sys.time(), " - Starting ", action_word, "...\n",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
      "📊 Total jobs to ", action_word, ": ", total_jobs, "\n",
      "Mode: ", if (identical(launch_mode, "local_docker")) "Local Docker" else if (identical(launch_mode, "local_native")) "Local Native" else "Condor", "\n",
      if (identical(launch_mode, "condor")) {
        paste0(
          "Run target: ", condor_target, "\n",
          "Matched slots: ", length(condor_target_info$matched_slots), "\n",
          "Exclude slots: ", length(condor_exclude_slots), "\n"
        )
      } else {
        ""
      },
      "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
    )
    
    cancel_launch <- reactiveVal(FALSE)

    update_launch_notification <- function(text, type = "message") {
      showNotification(text, type = type, duration = NULL, id = "launch_progress")
    }

    update_launch_notification(sprintf("%s jobs... (0/%d)", progress_prefix, total_jobs))
    
    tryCatch({
      batch_names <- c()
      remote_dirs <- c()
      launched_job_results <- list()
      current_job <- 0  # Track current job number
      progress_details <- c()  # Store progress messages
      progress_every_n <- 5
      progress_keep <- 50
      
      maybe_send_progress_details <- function(force = FALSE) {
        if (length(progress_details) > progress_keep) {
          progress_details <<- tail(progress_details, progress_keep)
        }
        if (force || current_job %% progress_every_n == 0 || current_job == total_jobs || current_job == 1) {
          session$sendCustomMessage(
            type = "updateProgress",
            message = list(
              id = "launch_progress_details",
              text = paste(progress_details, collapse = "<br/>")
            )
          )
        }
      }

      collect_job_result <- function(result) {
        if (is.null(result)) return(invisible(NULL))
        batch_names <<- c(batch_names, as.character(result$batch_name))
        remote_dirs <<- c(remote_dirs, as.character(result$remote_dir))
        launched_job_results[[length(launched_job_results) + 1]] <<- result
        invisible(NULL)
      }
      
      did_parallel <- FALSE
      run_parallel_submit <- (
        isTRUE(prof_chain_mode) && length(job_specs) >= 1 && (!is_local_mode || isTRUE(allow_local_prof_chain_parallel))
      ) || (
        isTRUE(input$parallel_launch) && length(job_specs) > 1 && !(isTRUE(prof_chain_mode) && is_local_mode && !isTRUE(allow_local_prof_chain_parallel))
      )
      if (isTRUE(run_parallel_submit)) {
        max_cores <- max(1, parallel::detectCores() - 2)
        cores <- max(1, min(as.integer(input$launch_parallel_cores), max_cores))
        if (isTRUE(prof_chain_mode) && !is_local_mode) cores <- max(2L, cores)
        if (cores <= 1) {
          rv$launch_log <- paste0(
            rv$launch_log,
            "⚠️ Parallel launch requested but not used (cores=",
            cores,
            ", jobs=",
            length(job_specs),
            "). Using sequential.\n"
          )
        } else {
          rv$launch_log <- paste0(
            rv$launch_log,
            "⚡ Parallel ",
              if (is_local_mode) "local run" else "launch",
              " ON (cores: ",
              cores,
              ")\n"
          )
          
          update_launch_notification(
            sprintf(
              "%s %d jobs in parallel (cores: %d)",
              if (is_local_mode) "Running" else "Launching",
              total_jobs,
              cores
            )
          )
          
          if (cancel_launch()) stop("Launch cancelled")

          results <- tryCatch({
            cl <- parallel::makeCluster(cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)

            if (identical(launch_mode, "condor")) {
              common_params <- list(
                remote_user = input$remote_user,
                remote_host = input$remote_host,
                github_pat = Sys.getenv("GIT_PAT"),
                github_username = input$github_username,
                github_org = input$github_org,
                github_repo = input$github_repo,
                docker_image = input$docker_image,
                condor_cpus = as.integer(input$condor_cpus),
                condor_memory = paste0(input$condor_memory, "GB"),
                condor_disk = paste0(input$condor_disk, "GB"),
                branch = input$branch,
                ghcr_login = isTRUE(input$ghcr_login),
                output_dir = input$output_dir,
                model_env_lists = model_env_lists,
                exclude_slots = condor_exclude_slots
              )
              parallel::clusterEvalQ(cl, { library(CondorBox) })
              parallel::clusterExport(cl, varlist = c("launch_single_job_raw", "common_params"), envir = environment())
              parallel::parLapply(cl, job_specs, function(spec) {
                tryCatch({
                  launch_single_job_raw(spec, common_params)
                }, error = function(e) {
                  list(batch_name = NA_character_, remote_dir = NA_character_, job_id = NA_character_, error = e$message, spec = spec)
                })
              })
            } else {
              common_params <- list(
                repo_root = isolate(repo_root_val()),
                local_use_docker = is_local_docker,
                local_async = TRUE,
                docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
                model_env_lists = model_env_lists
              )
              parallel::clusterExport(
                cl,
                varlist = c("local_env_strings", "local_job_runner", "local_run_command", "launch_single_job_local_raw", "common_params"),
                envir = environment()
              )
              parallel::parLapply(cl, job_specs, function(spec) {
                tryCatch({
                  launch_single_job_local_raw(spec, common_params)
                }, error = function(e) {
                  list(batch_name = NA_character_, remote_dir = NA_character_, job_id = NA_character_, error = e$message, spec = spec)
                })
              })
            }
          }, error = function(e) {
            rv$launch_log <- paste0(
              rv$launch_log,
              "⚠️ Parallel ",
              if (is_local_mode) "local run" else "launch",
              " failed: ",
              e$message,
              "\n"
            )
            NULL
          })
          
          if (!is.null(results)) {
            ok_mask <- vapply(results, function(x) is.null(x$error), logical(1))
            results_ok <- results[ok_mask]
            if (length(results_ok) > 0) {
              for (res in results_ok) collect_job_result(res)
            }
            current_job <- length(results_ok)
            did_parallel <- TRUE
            
            if (!all(ok_mask)) {
              err_msgs <- unique(vapply(results[!ok_mask], function(x) x$error, character(1)))
              rv$launch_log <- paste0(
                rv$launch_log,
                "⚠️ Parallel launch errors:\n",
                paste0("  - ", err_msgs, collapse = "\n"),
                "\n"
              )
            }
          }
        }
      }
      if (!did_parallel) {
        for (model_name in selected_models) {
          if (cancel_launch()) stop("Launch cancelled")
          model_env <- rv$models[[model_name]]
          
          for (job_type in selected_job_types) {
            if (cancel_launch()) stop("Launch cancelled")
            
            if (job_type == "jitter") {
              seeds <- parse_numeric_tokens(model_env$jitter_seeds)
              for (seed in seeds) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (seed %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, seed)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (seed %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, seed)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (seed %d)", 
                          current_job, total_jobs, model_name, seed)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, seed = seed, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "hessian") {
              nsplit <- suppressWarnings(as.integer(model_env$nsplit))
              if (!is.finite(nsplit) || nsplit < 1) nsplit <- 0L
              for (part in seq_len(nsplit)) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (part %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, part)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (part %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, part)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (part %d)", 
                          current_job, total_jobs, model_name, part)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, part = part, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "retro") {
              peels <- parse_numeric_tokens(model_env$retro_peels)
              for (peel in peels) {
                if (cancel_launch()) stop("Launch cancelled")
                current_job <- current_job + 1
                
                update_launch_notification(
                  sprintf("%s job %d/%d: %s (peel %d)",
                          progress_prefix,
                          current_job, total_jobs, model_name, peel)
                )
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("[%d/%d] 🔄 %s: %s (peel %d)\n", 
                          current_job, total_jobs, progress_prefix, model_name, peel)
                )
                
                progress_details <- c(
                  progress_details,
                  sprintf("[%d/%d] 🔄 %s (peel %d)", 
                          current_job, total_jobs, model_name, peel)
                )
                
                result <- launch_single_job(model_name, model_env, job_type = job_type, peel = peel, exclude_slots = condor_exclude_slots)
                collect_job_result(result)
                
                progress_details[length(progress_details)] <- paste0(
                  progress_details[length(progress_details)], " ✓"
                )
                maybe_send_progress_details()
                
                rv$launch_log <- paste0(
                  rv$launch_log,
                  sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                )
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else if (job_type == "prof") {
              for (prof_env in resolve_profile_job_envs(model_env)) {
                profile_name <- profile_launch_name(model_name, prof_env)
                scalars <- resolve_prof_scalars_for_mode(prof_env, prof_chain_mode = prof_chain_mode)
                if (isTRUE(prof_chain_mode) && is_local_mode && length(scalars) > 0) {
                  chain_plan <- resolve_prof_anchor_and_chains(scalars, prof_anchor_requested)
                  anchor_sc <- chain_plan$anchor

                  rv$launch_log <- paste0(
                    rv$launch_log,
                    sprintf(
                      "🔗 Profile sequential strategy [%s]: anchor=%g (requested=%g), lower=%s, upper=%s\n",
                      profile_name,
                      anchor_sc,
                      prof_anchor_requested,
                      if (length(chain_plan$lower) > 0) paste(chain_plan$lower, collapse = " ") else "<none>",
                      if (length(chain_plan$upper) > 0) paste(chain_plan$upper, collapse = " ") else "<none>"
                    )
                  )

                  run_prof_local_one <- function(sc, donor = NA_real_) {
                    common_params_prof <- list(
                      repo_root = isolate(repo_root_val()),
                      local_use_docker = is_local_docker,
                      local_async = FALSE,
                      docker_image = if (!is.null(input$local_docker_image) && nzchar(input$local_docker_image)) input$local_docker_image else input$docker_image,
                      model_env_lists = setNames(list(as.list(prof_env, all.names = TRUE)), model_name)
                    )
                    launch_single_job_local_raw(
                      spec = list(
                        model_name = model_name,
                        job_type = "prof",
                        scalar = sc,
                        init_from_scalar_override = if (is.finite(donor)) donor else NULL,
                        profile_env = as.list(prof_env, all.names = TRUE)
                      ),
                      common_params = common_params_prof
                    )
                  }

                  run_chain_local <- function(chain_scalars, start_donor) {
                    out <- list()
                    donor <- start_donor
                    for (sc in chain_scalars) {
                      res <- run_prof_local_one(sc = sc, donor = donor)
                      res$chain <- if (sc < anchor_sc) "down" else "up"
                      res$donor_scalar <- donor
                      out[[length(out) + 1L]] <- res
                      donor <- sc
                    }
                    out
                  }

                  if (cancel_launch()) stop("Launch cancelled")
                  current_job <- current_job + 1
                  update_launch_notification(sprintf("%s job %d/%d: %s (anchor scalar %g)", progress_prefix, current_job, total_jobs, profile_name, anchor_sc))
                  progress_details <- c(progress_details, sprintf("[%d/%d] 🔄 %s (anchor scalar %g)", current_job, total_jobs, profile_name, anchor_sc))
                  result_anchor <- run_prof_local_one(sc = anchor_sc, donor = NA_real_)
                  collect_job_result(result_anchor)
                  progress_details[length(progress_details)] <- paste0(progress_details[length(progress_details)], " ✓")
                  maybe_send_progress_details()
                  rv$launch_log <- paste0(rv$launch_log, sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result_anchor$batch_name))

                  chain_results <- list()
                  run_two_chains_parallel <- isTRUE(input$parallel_launch) &&
                    length(chain_plan$lower) > 0 &&
                    length(chain_plan$upper) > 0 &&
                    .Platform$OS.type != "windows"

                  if (run_two_chains_parallel) {
                    rv$launch_log <- paste0(rv$launch_log, "⚡ Running lower/upper profile chains in parallel (2 forks)\n")
                    parts <- parallel::mclapply(
                      list(
                        list(scalars = chain_plan$lower, donor = anchor_sc),
                        list(scalars = chain_plan$upper, donor = anchor_sc)
                      ),
                      function(ch) run_chain_local(ch$scalars, ch$donor),
                      mc.cores = 2
                    )
                    chain_results <- unlist(parts, recursive = FALSE)
                  } else {
                    chain_results <- c(
                      run_chain_local(chain_plan$lower, anchor_sc),
                      run_chain_local(chain_plan$upper, anchor_sc)
                    )
                  }

                  for (res in chain_results) {
                    if (cancel_launch()) stop("Launch cancelled")
                    current_job <- current_job + 1
                    progress_details <- c(
                      progress_details,
                      sprintf("[%d/%d] 🔄 %s (scalar %s, donor %s)",
                              current_job, total_jobs, profile_name,
                              if (!is.null(res$job_env$scalar)) as.character(res$job_env$scalar) else "?",
                              if (!is.null(res$donor_scalar) && is.finite(res$donor_scalar)) as.character(res$donor_scalar) else "<none>")
                    )
                    collect_job_result(res)
                    progress_details[length(progress_details)] <- paste0(progress_details[length(progress_details)], " ✓")
                    maybe_send_progress_details()
                    rv$launch_log <- paste0(rv$launch_log, sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), res$batch_name))
                  }
                } else {
                  for (sc in scalars) {
                    if (cancel_launch()) stop("Launch cancelled")
                    current_job <- current_job + 1

                    update_launch_notification(
                      sprintf("%s job %d/%d: %s (scalar %g)",
                              progress_prefix,
                              current_job, total_jobs, profile_name, sc)
                    )

                    rv$launch_log <- paste0(
                      rv$launch_log,
                      sprintf("[%d/%d] 🔄 %s: %s (scalar %g)\n",
                              current_job, total_jobs, progress_prefix, profile_name, sc)
                    )

                    progress_details <- c(
                      progress_details,
                      sprintf("[%d/%d] 🔄 %s (scalar %g)",
                              current_job, total_jobs, profile_name, sc)
                    )

                    result <- launch_single_job(model_name, prof_env, job_type = job_type, scalar = sc, exclude_slots = condor_exclude_slots)
                    collect_job_result(result)

                    progress_details[length(progress_details)] <- paste0(
                      progress_details[length(progress_details)], " ✓"
                    )
                    maybe_send_progress_details()

                    rv$launch_log <- paste0(
                      rv$launch_log,
                      sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
                    )
                  }
                }
              }
              if (cancel_launch()) stop("Launch cancelled")
            } else {
              if (cancel_launch()) stop("Launch cancelled")
              current_job <- current_job + 1
              
              update_launch_notification(
                sprintf("%s job %d/%d: %s",
                        progress_prefix,
                        current_job, total_jobs, model_name)
              )
              
              rv$launch_log <- paste0(
                rv$launch_log,
                sprintf("[%d/%d] 🔄 %s: %s\n", 
                        current_job, total_jobs, progress_prefix, model_name)
              )
              
              progress_details <- c(
                progress_details,
                sprintf("[%d/%d] 🔄 %s", 
                        current_job, total_jobs, model_name)
              )
              
              result <- launch_single_job(model_name, model_env, job_type = job_type, exclude_slots = condor_exclude_slots)
              collect_job_result(result)
              
              progress_details[length(progress_details)] <- paste0(
                progress_details[length(progress_details)], " ✓"
              )
              maybe_send_progress_details()
              
              rv$launch_log <- paste0(
                rv$launch_log,
                sprintf("  ✓ %s: %s\n\n", tools::toTitleCase(completion_word), result$batch_name)
              )
            }
          }
        }
      }
      
      # Save launch records
      launch_status <- if (cancel_launch()) {
        "cancelled"
      } else if (is_local_mode && !isTRUE(prof_chain_mode)) {
        "launched_local_async"
      } else if (is_local_mode) {
        "completed_local"
      } else {
        "launched"
      }
      job_record <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        job_type = paste(selected_job_types, collapse = ", "),
        model_names = paste(selected_models, collapse = ", "),
        output_dir = effective_output_dir,
        batch_names = paste(batch_names, collapse = ", "),
        remote_dirs = paste(remote_dirs, collapse = ", "),
        branch = input$branch,
        status = launch_status,
        launch_mode = launch_mode,
        stringsAsFactors = FALSE
      )

      if (!is.null(rv$current_config_file)) {
        save_job_history(rv$current_config_file, job_record)
        rv$launch_log <- paste0(rv$launch_log, "📝 Job history saved to config file\n")
      }

      launcher_log_record <- data.frame(
        run_at = job_record$timestamp,
        output_dir = as.character(effective_output_dir),
        summary = if (!is.null(rv$run_metadata$summary) && nzchar(as.character(rv$run_metadata$summary))) {
          as.character(rv$run_metadata$summary)
        } else {
          "NA"
        },
        run_description = if (!is.null(input$run_description) && nzchar(trimws(as.character(input$run_description)))) {
          as.character(input$run_description)
        } else {
          "NA"
        },
        config_file = if (!is.null(rv$config_path) && nzchar(as.character(rv$config_path))) {
          as.character(rv$config_path)
        } else {
          "NA"
        },
        job_types = as.character(job_record$job_type),
        model_names = as.character(job_record$model_names),
        total_jobs = as.integer(total_jobs),
        launch_mode = as.character(job_record$launch_mode),
        status = as.character(job_record$status),
        branch = as.character(job_record$branch),
        batch_names = as.character(job_record$batch_names),
        local_pid_files = if (length(launched_job_results) > 0) {
          paste(
            vapply(launched_job_results, function(x) {
              if (!is.null(x$pid_file) && nzchar(as.character(x$pid_file))) as.character(x$pid_file) else ""
            }, character(1)),
            collapse = ", "
          )
        } else {
          ""
        },
        local_log_files = if (length(launched_job_results) > 0) {
          paste(
            vapply(launched_job_results, function(x) {
              if (!is.null(x$log_file) && nzchar(as.character(x$log_file))) as.character(x$log_file) else ""
            }, character(1)),
            collapse = ", "
          )
        } else {
          ""
        },
        config_details = if (length(launched_job_results) > 0) {
          build_launched_config_details(launched_job_results)
        } else {
          build_model_config_details(selected_models)
        },
        remote_dirs = as.character(job_record$remote_dirs),
        stringsAsFactors = FALSE
      )
      save_launcher_job_log(launcher_log_record)
      rv$launcher_job_log_trigger <- rv$launcher_job_log_trigger + 1
      rv$launch_log <- paste0(rv$launch_log, "📝 Launcher job log updated\n")
      
      if (cancel_launch()) {
        removeNotification("launch_progress")
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "⚠️ ", Sys.time(), " - ", tools::toTitleCase(action_word), " cancelled by user after ", current_job, " job(s)\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #f39c12;",
            icon("ban"), " Launch Cancelled"
          ),
          size = "m",
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #f39c12;",
              sprintf("⚠️ Cancelled after %d job(s).", current_job)
            )
          ),
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-warning")
          )
        ))
      } else {
        removeNotification("launch_progress")
        # Final completion message
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "✅ ", Sys.time(), " - All ", total_jobs, " jobs ", completion_word, " successfully!\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        # Update modal to show completion
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #00a65a;",
            icon("check-circle"), if (is_local_mode) " Local Run Complete" else " Launch Complete"
          ),
          size = "m",
          
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #00a65a;",
              sprintf("✅ Successfully %s all %d jobs!", completion_word, total_jobs)
            )
          ),
          
          div(
            style = "background: #f0f9f0; border: 1px solid #c3e6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
            strong("Summary:"),
            tags$ul(
              tags$li(paste("Total jobs:", total_jobs)),
              tags$li(paste("Models:", paste(selected_models, collapse = ", "))),
              tags$li(paste("Mode:", if (identical(launch_mode, "local_docker")) "Local Docker" else if (identical(launch_mode, "local_native")) "Local Native" else "Condor")),
              tags$li(paste(if (is_local_mode) "Model dir:" else "Output directory:", effective_output_dir)),
              tags$li(paste("Branch:", input$branch))
            )
          ),
          
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-success")
          )
        ))
      }
      
    }, error = function(e) {
      removeNotification("launch_progress")
      if (grepl("Launch cancelled", e$message)) {
        rv$launch_log <- paste0(
          rv$launch_log,
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
          "⚠️ ", Sys.time(), " - ", tools::toTitleCase(action_word), " cancelled by user after ", current_job, " job(s)\n",
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
        )
        
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #f39c12;",
            icon("ban"), " Launch Cancelled"
          ),
          size = "m",
          div(
            style = "text-align: center; margin: 20px 0;",
            h3(
              style = "color: #f39c12;",
              sprintf("⚠️ Cancelled after %d job(s).", current_job)
            )
          ),
          footer = tagList(
            actionButton("close_launch_modal", "Close", class = "btn-warning")
          )
        ))
      } else {
        rv$launch_log <- paste0(rv$launch_log, "\n❌ ERROR: ", e$message, "\n")
        
        # Show error modal
        showModal(modalDialog(
          title = div(
            style = "font-size: 18px; font-weight: bold; color: #dd4b39;",
            icon("times-circle"), " Launch Failed"
          ),
          size = "m",
          
          div(
            style = "background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 15px; margin: 15px 0;",
            h4(style = "color: #721c24;", "Error occurred during job launch:"),
            p(style = "font-family: monospace; color: #721c24;", e$message)
          ),
          
          footer = actionButton("close_launch_error_modal", "Close", class = "btn-danger")
        ))
      }
      
    }, finally = {
      # Re-enable button after completion or error
      shinyjs::enable("launch_btn")
      shinyjs::removeClass("launch_btn", "loading")
    })
  })
  
  # Handler to close completion modal
  observeEvent(input$close_launch_modal, {
    removeModal()
  })
  
  # Handler to close error modal
  observeEvent(input$close_error_modal, {
    removeModal()
  })
  
  
  
  launch_single_job_raw <- function(spec, common_params) {
    model_env_list <- if (!is.null(spec$profile_env) && is.list(spec$profile_env)) spec$profile_env else common_params$model_env_lists[[spec$model_name]]
    if (is.null(model_env_list)) {
      stop(paste("Model env not found for", spec$model_name))
    }
    job_env <- list2env(model_env_list, parent = emptyenv())
    job_env$DOCKER_IMAGE <- common_params$docker_image
    remote_dir_suffix <- spec$model_name
    batch_suffix <- ""
    profile_tag <- if (identical(spec$job_type, "prof") || identical(spec$job_type, "prof_chain")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("_", profile_tag) else ""
    profile_batch_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""
    
    if (!is.null(spec$seed)) {
      job_env$jitter_seed <- as.character(spec$seed)
      remote_dir_suffix <- paste0(spec$model_name, "_seed", spec$seed)
      batch_suffix <- paste0("-jitter", spec$seed)
    } else if (!is.null(spec$part)) {
      job_env$hessian_part <- as.character(spec$part)
      remote_dir_suffix <- paste0(spec$model_name, "_part", spec$part)
      batch_suffix <- paste0("-hess", spec$part)
    } else if (!is.null(spec$peel)) {
      job_env$retro_peel <- as.character(spec$peel)
      remote_dir_suffix <- paste0(spec$model_name, "_peel", spec$peel)
      batch_suffix <- paste0("-retro", spec$peel)
    } else if (!is.null(spec$scalar)) {
      job_env$scalar <- as.character(spec$scalar)
      if (identical(spec$job_type, "prof")) {
        mapped_env <- apply_prof_init_mapping(as.list(job_env, all.names = TRUE), spec$scalar)
        job_env <- list2env(mapped_env, parent = emptyenv())
      }
      remote_dir_suffix <- paste0(spec$model_name, profile_suffix, "_sc", spec$scalar)
      batch_suffix <- paste0(profile_batch_suffix, "-sc", spec$scalar)
    } else if (identical(spec$job_type, "prof_chain")) {
      if (!is.null(spec$chain_name) && nzchar(as.character(spec$chain_name))) {
        job_env$chain_name <- as.character(spec$chain_name)
      }
        if (!is.null(spec$chain_scalars) && nzchar(as.character(spec$chain_scalars))) {
          chain_vals <- parse_numeric_tokens(spec$chain_scalars)
          job_env$chain_scalars <- as.character(spec$chain_scalars)
          if (length(chain_vals) > 0) {
            job_env$chain_count <- as.character(length(chain_vals))
            job_env$CHAIN_COUNT <- as.character(length(chain_vals))
            for (ii in seq_along(chain_vals)) {
              v <- format(chain_vals[[ii]], scientific = FALSE, trim = TRUE)
              job_env[[paste0("chain_scalar_", ii)]] <- v
              job_env[[paste0("CHAIN_SCALAR_", ii)]] <- v
            }
          }
        }
      if (!is.null(spec$chain_first_init_from) && nzchar(as.character(spec$chain_first_init_from))) {
        job_env$chain_first_init_from <- as.character(spec$chain_first_init_from)
      }
      if (!is.null(spec$chain_anchor) && nzchar(as.character(spec$chain_anchor))) {
        job_env$chain_anchor <- as.character(spec$chain_anchor)
      }
      remote_dir_suffix <- paste0(spec$model_name, profile_suffix, "_profchain_", if (!is.null(spec$chain_name)) as.character(spec$chain_name) else "chain")
      batch_suffix <- paste0(profile_batch_suffix, "-profchain", if (!is.null(spec$chain_name)) paste0("-", as.character(spec$chain_name)) else "")
    } else {
      # model job only
      remote_dir_suffix <- paste0(spec$model_name, "_model")
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
    
    job_id <- CondorBox::CondorBox(
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
    
    return(list(
      batch_name = batch_name,
      remote_dir = remote_dir,
      job_id = job_id,
      model_name = spec$model_name,
      job_type = spec$job_type,
      job_env = as.list(job_env, all.names = TRUE)
    ))
  }
  
  launch_single_job <- function(model_name, model_env, job_type, seed = NULL, part = NULL, peel = NULL, scalar = NULL, log = TRUE, exclude_slots = NULL) {
    if (input$launch_mode %in% c("local_native", "local_docker")) {
      return(
        launch_single_job_local(
          model_name = model_name,
          model_env = model_env,
          job_type = job_type,
          seed = seed,
          part = part,
          peel = peel,
          scalar = scalar,
          log = log
        )
      )
    }

    job_env <- model_env
    job_env$DOCKER_IMAGE <- input$docker_image
    remote_dir_suffix <- model_name
    batch_suffix <- ""
    profile_tag <- if (identical(job_type, "prof") || identical(job_type, "prof_chain")) {
      sanitize_profile_job_tag(first_scalar_string(job_env$profile_set_tag, default = first_scalar_string(job_env$profile_set_name, default = "")))
    } else {
      ""
    }
    profile_suffix <- if (nzchar(profile_tag)) paste0("_", profile_tag) else ""
    profile_batch_suffix <- if (nzchar(profile_tag)) paste0("-", profile_tag) else ""
    
    if (!is.null(seed)) {
      job_env$jitter_seed <- as.character(seed)
      remote_dir_suffix <- paste0(model_name, "_seed", seed)
      batch_suffix <- paste0("-jitter", seed)
    } else if (!is.null(part)) {
      job_env$hessian_part <- as.character(part)
      remote_dir_suffix <- paste0(model_name, "_part", part)
      batch_suffix <- paste0("-hess", part)
    } else if (!is.null(peel)) {
      job_env$retro_peel <- as.character(peel)
      remote_dir_suffix <- paste0(model_name, "_peel", peel)
      batch_suffix <- paste0("-retro", peel)
    } else if (!is.null(scalar)) {
      job_env$scalar <- as.character(scalar)
      if (identical(job_type, "prof")) {
        job_env <- apply_prof_init_mapping(job_env, scalar)
      }
      remote_dir_suffix <- paste0(model_name, profile_suffix, "_sc", scalar)
      batch_suffix <- paste0(profile_batch_suffix, "-sc", scalar)
    } else {
      # model job only
      remote_dir_suffix <- paste0(model_name, "_model")
    }
    
    remote_dir <- paste0(input$github_repo, "/", input$output_dir, "/", remote_dir_suffix)
    batch_name <- paste0(model_name, batch_suffix, "-", format(Sys.time(), "%H:%M:%S"))
    
    if (isTRUE(log)) {
      rv$launch_log <- paste0(rv$launch_log, "  → ", batch_name, "\n")
    }
    
    job_id <- CondorBox::CondorBox(
      make_options = job_type, 
      remote_user = input$remote_user, 
      remote_host = input$remote_host,
      remote_dir = remote_dir, 
      github_pat = Sys.getenv("GIT_PAT"), 
      github_username = input$github_username,
      github_org = input$github_org, 
      github_repo = input$github_repo, 
      docker_image = input$docker_image,
      condor_cpus = as.integer(input$condor_cpus),
      condor_memory = paste0(input$condor_memory, "GB"),
      condor_disk = paste0(input$condor_disk, "GB"),
      stream_error = "TRUE", 
      branch = input$branch, 
      rmclone_script = "no", 
      ghcr_login = isTRUE(input$ghcr_login),
      exclude_slots = if (!is.null(exclude_slots)) exclude_slots else default_condor_exclude_slots(),
      custom_batch_name = batch_name, 
      condor_environment = as.list(job_env, all.names = TRUE)
    )
    
    return(list(
      batch_name = batch_name,
      remote_dir = remote_dir,
      job_id = job_id,
      model_name = model_name,
      job_type = job_type,
      job_env = as.list(job_env, all.names = TRUE)
    ))
  }
  
