pmo_normalize_path <- function(path, repo_root = ".") {
  if (is.null(path) || !nzchar(trimws(as.character(path)))) return(NA_character_)
  path <- trimws(as.character(path))
  if (!grepl("^(/|~|[A-Za-z]:[\\\\/])", path)) {
    path <- file.path(repo_root, path)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

pmo_relative_path <- function(path, repo_root = ".") {
  path <- pmo_normalize_path(path, repo_root = repo_root)
  root <- pmo_normalize_path(repo_root, repo_root = ".")
  prefix <- paste0(gsub("/+$", "", root), "/")
  if (!is.na(path) && startsWith(path, prefix)) {
    return(substr(path, nchar(prefix) + 1L, nchar(path)))
  }
  path
}

pmo_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

pmo_file_time <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) return(NA_character_)
  format(file.info(path)$mtime, "%Y-%m-%d %H:%M")
}

pmo_payload_time <- function(payload, payload_file) {
  created <- payload$created_at
  if (!is.null(created) && length(created) > 0 && nzchar(as.character(created[[1]]))) {
    return(as.character(created[[1]]))
  }
  pmo_file_time(payload_file)
}

pmo_file_state <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) return("missing")
  paste0("exists (", pmo_file_time(path), ")")
}

pmo_action_label <- function(status) {
  switch(
    as.character(status),
    will_update = "update available",
    updated = "updated",
    up_to_date = "up to date",
    missing_source = "no source",
    missing_base_dir = "no input target",
    failed = "failed",
    missing_model_info = "no model info",
    as.character(status)
  )
}

pmo_same_file <- function(a, b) {
  if (!file.exists(a) || !file.exists(b)) return(FALSE)
  ai <- file.info(a)
  bi <- file.info(b)
  if (is.na(ai$size) || is.na(bi$size) || ai$size != bi$size) return(FALSE)
  identical(readBin(a, "raw", n = ai$size), readBin(b, "raw", n = bi$size))
}

pmo_same_lines <- function(lines, path) {
  if (is.null(lines) || length(lines) == 0 || !file.exists(path)) return(FALSE)
  old <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  identical(as.character(lines), as.character(old))
}

pmo_latest_par <- function(folder) {
  hits <- list.files(folder, pattern = "\\.par$", full.names = TRUE)
  if (length(hits) == 0) return(NA_character_)
  stems <- tools::file_path_sans_ext(basename(hits))
  nums <- suppressWarnings(as.integer(stems))
  numeric_idx <- which(!is.na(nums) & grepl("^[0-9]+$", stems))
  if (length(numeric_idx) > 0) {
    return(hits[numeric_idx[which.max(nums[numeric_idx])]])
  }
  hits[which.max(file.info(hits)$mtime)]
}

pmo_find_model_dirs <- function(source_dirs, repo_root = ".", model_names = NULL) {
  source_dirs <- unique(vapply(source_dirs, pmo_normalize_path, character(1), repo_root = repo_root))
  source_dirs <- source_dirs[!is.na(source_dirs) & dir.exists(source_dirs)]
  if (length(source_dirs) == 0) return(character(0))

  info_files <- unique(unlist(lapply(source_dirs, function(x) {
    list.files(x, pattern = "^model_info\\.rds$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  model_dirs <- unique(dirname(info_files))
  if (!is.null(model_names) && length(model_names) > 0) {
    keep <- basename(model_dirs) %in% model_names
    model_dirs <- model_dirs[keep]
  }
  sort(model_dirs)
}

pmo_plan_one <- function(model_dir, repo_root = ".") {
  model_dir <- pmo_normalize_path(model_dir, repo_root = repo_root)
  info <- pmo_read_rds(file.path(model_dir, "model_info.rds"))
  payload <- pmo_read_rds(file.path(model_dir, "model_payload.rds"))

  if (is.null(info) || !is.list(info)) {
    return(data.frame(
      model = basename(model_dir), source_dir = model_dir, base_dir = NA_character_,
      run_at = NA_character_, output_folder = pmo_relative_path(model_dir, repo_root), input_target = NA_character_,
      par_source = NA_character_, par_target = NA_character_, par_status = "missing_model_info",
      par_source_state = "missing", par_target_state = "missing", par_action = "no model info",
      indepvar_source = NA_character_, indepvar_target = NA_character_, indepvar_status = "missing_model_info",
      indepvar_source_state = "missing", indepvar_target_state = "missing", indepvar_action = "no model info",
      stringsAsFactors = FALSE
    ))
  }

  base_dir <- pmo_normalize_path(info$base_dir, repo_root = repo_root)
  payload_file <- file.path(model_dir, "model_payload.rds")
  run_at <- pmo_payload_time(payload, payload_file)
  par_source <- NA_character_
  if (!is.null(payload$files$par) && file.exists(payload$files$par)) {
    par_source <- normalizePath(payload$files$par, winslash = "/", mustWork = TRUE)
  } else if (!is.null(info$par_out) && nzchar(as.character(info$par_out)) && file.exists(file.path(model_dir, info$par_out))) {
    par_source <- normalizePath(file.path(model_dir, info$par_out), winslash = "/", mustWork = TRUE)
  } else {
    par_source <- pmo_latest_par(model_dir)
  }

  par_target <- if (!is.na(par_source) && nzchar(par_source) && !is.na(base_dir)) {
    file.path(base_dir, basename(par_source))
  } else {
    NA_character_
  }

  indep_file <- file.path(model_dir, "indepvar.rpt")
  indep_lines <- NULL
  indep_source <- NA_character_
  if (file.exists(indep_file)) {
    indep_source <- indep_file
    indep_lines <- tryCatch(readLines(indep_file, warn = FALSE), error = function(e) NULL)
  } else if (!is.null(payload$data$IndepOut) && length(payload$data$IndepOut) > 0) {
    indep_source <- "model_payload.rds:data$IndepOut"
    indep_lines <- as.character(payload$data$IndepOut)
  }
  indep_target <- if (!is.na(base_dir)) file.path(base_dir, "indepvar.rpt") else NA_character_
  indep_source_state <- if (identical(indep_source, "model_payload.rds:data$IndepOut")) {
    paste0("in payload (", run_at, ")")
  } else {
    pmo_file_state(indep_source)
  }

  par_status <- if (is.na(base_dir) || !nzchar(base_dir)) {
    "missing_base_dir"
  } else if (is.na(par_source) || !file.exists(par_source)) {
    "missing_source"
  } else if (file.exists(par_target) && pmo_same_file(par_source, par_target)) {
    "up_to_date"
  } else {
    "will_update"
  }

  indep_status <- if (is.na(base_dir) || !nzchar(base_dir)) {
    "missing_base_dir"
  } else if (is.null(indep_lines) || length(indep_lines) == 0) {
    "missing_source"
  } else if (file.exists(indep_target) && pmo_same_lines(indep_lines, indep_target)) {
    "up_to_date"
  } else {
    "will_update"
  }

  data.frame(
    model = basename(model_dir),
    source_dir = model_dir,
    base_dir = base_dir,
    run_at = run_at,
    output_folder = pmo_relative_path(model_dir, repo_root),
    input_target = if (!is.na(base_dir)) basename(base_dir) else NA_character_,
    par_source = par_source,
    par_target = par_target,
    par_status = par_status,
    par_source_state = pmo_file_state(par_source),
    par_target_state = pmo_file_state(par_target),
    par_action = pmo_action_label(par_status),
    indepvar_source = indep_source,
    indepvar_target = indep_target,
    indepvar_status = indep_status,
    indepvar_source_state = indep_source_state,
    indepvar_target_state = pmo_file_state(indep_target),
    indepvar_action = pmo_action_label(indep_status),
    stringsAsFactors = FALSE
  )
}

pmo_plan_promotions <- function(source_dirs = "model", repo_root = ".", model_names = NULL) {
  model_dirs <- pmo_find_model_dirs(source_dirs, repo_root = repo_root, model_names = model_names)
  if (length(model_dirs) == 0) {
    return(data.frame(
      model = character(), source_dir = character(), base_dir = character(),
      run_at = character(), output_folder = character(), input_target = character(),
      par_source = character(), par_target = character(), par_status = character(),
      par_source_state = character(), par_target_state = character(), par_action = character(),
      indepvar_source = character(), indepvar_target = character(), indepvar_status = character(),
      indepvar_source_state = character(), indepvar_target_state = character(), indepvar_action = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(model_dirs, pmo_plan_one, repo_root = repo_root))
}

pmo_backup_path <- function(path) {
  paste0(path, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

pmo_apply_promotions <- function(plan, backup = TRUE) {
  if (is.null(plan) || nrow(plan) == 0) return(plan)
  plan$par_action <- "skipped"
  plan$indepvar_action <- "skipped"

  for (i in seq_len(nrow(plan))) {
    if (identical(plan$par_status[[i]], "will_update")) {
      dir.create(dirname(plan$par_target[[i]]), recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(backup) && file.exists(plan$par_target[[i]])) {
        file.copy(plan$par_target[[i]], pmo_backup_path(plan$par_target[[i]]), overwrite = FALSE)
      }
      ok <- file.copy(plan$par_source[[i]], plan$par_target[[i]], overwrite = TRUE)
      plan$par_action[[i]] <- if (isTRUE(ok)) "updated" else "failed"
      plan$par_status[[i]] <- if (isTRUE(ok)) "updated" else "failed"
      plan$par_target_state[[i]] <- pmo_file_state(plan$par_target[[i]])
    } else {
      plan$par_action[[i]] <- pmo_action_label(plan$par_status[[i]])
    }

    if (identical(plan$indepvar_status[[i]], "will_update")) {
      lines <- if (file.exists(plan$indepvar_source[[i]])) {
        readLines(plan$indepvar_source[[i]], warn = FALSE)
      } else {
        payload <- pmo_read_rds(file.path(plan$source_dir[[i]], "model_payload.rds"))
        as.character(payload$data$IndepOut)
      }
      dir.create(dirname(plan$indepvar_target[[i]]), recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(backup) && file.exists(plan$indepvar_target[[i]])) {
        file.copy(plan$indepvar_target[[i]], pmo_backup_path(plan$indepvar_target[[i]]), overwrite = FALSE)
      }
      ok <- tryCatch({
        writeLines(lines, plan$indepvar_target[[i]])
        TRUE
      }, error = function(e) FALSE)
      plan$indepvar_action[[i]] <- if (isTRUE(ok)) "updated" else "failed"
      plan$indepvar_status[[i]] <- if (isTRUE(ok)) "updated" else "failed"
      plan$indepvar_target_state[[i]] <- pmo_file_state(plan$indepvar_target[[i]])
    } else {
      plan$indepvar_action[[i]] <- pmo_action_label(plan$indepvar_status[[i]])
    }
  }

  plan
}

pmo_parse_args <- function(args) {
  out <- list(source = "model", repo_root = ".", model = character(0), apply = FALSE, backup = TRUE)
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    val <- if (i < length(args)) args[[i + 1L]] else ""
    if (identical(key, "--source")) {
      out$source <- val
      i <- i + 2L
    } else if (identical(key, "--repo-root")) {
      out$repo_root <- val
      i <- i + 2L
    } else if (identical(key, "--model")) {
      out$model <- c(out$model, trimws(unlist(strsplit(val, ","))))
      i <- i + 2L
    } else if (identical(key, "--apply")) {
      out$apply <- TRUE
      i <- i + 1L
    } else if (identical(key, "--no-backup")) {
      out$backup <- FALSE
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }
  out$model <- out$model[nzchar(out$model)]
  out
}

pmo_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- pmo_parse_args(args)
  plan <- pmo_plan_promotions(
    source_dirs = opts$source,
    repo_root = opts$repo_root,
    model_names = opts$model
  )
  if (isTRUE(opts$apply)) {
    plan <- pmo_apply_promotions(plan, backup = opts$backup)
  }
  show_cols <- intersect(
    c("model", "output_folder", "run_at", "input_target", "par_action", "indepvar_action"),
    names(plan)
  )
  print(plan[, show_cols, drop = FALSE], row.names = FALSE)
  invisible(plan)
}

cmd <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", cmd, value = TRUE)
if (length(script_arg) > 0 && grepl("promote_model_outputs\\.R$", sub("^--file=", "", script_arg[[1]]))) {
  pmo_main()
}
