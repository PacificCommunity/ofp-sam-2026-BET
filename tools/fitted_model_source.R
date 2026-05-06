fms_truthy <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- tolower(trimws(as.character(x[[1]])))
  if (!nzchar(txt)) return(default)
  txt %in% c("1", "true", "yes", "y", "on")
}

fms_first <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- trimws(as.character(x[[1]]))
  if (!nzchar(txt) || is.na(txt)) default else txt
}

fms_safe_id <- function(x, default = "fitted") {
  txt <- gsub("[^A-Za-z0-9]+", "_", as.character(x[[1]]))
  txt <- gsub("^_+|_+$", "", txt)
  if (!nzchar(txt)) default else txt
}

fms_path_relative <- function(path, repo_root = ".") {
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
  sub(paste0("^", gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", root_norm), "/?"), "", path_norm)
}

fms_resolve_path <- function(path, project_root = getwd(), parent_ok = TRUE) {
  path <- fms_first(path)
  if (!nzchar(path)) return("")
  candidates <- unique(c(
    path,
    if (!grepl("^/", path)) file.path(project_root, path) else NA_character_,
    if (isTRUE(parent_ok) && !grepl("^/", path)) file.path(project_root, "..", path) else NA_character_
  ))
  candidates <- candidates[is.character(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) normalizePath(hit[[1]], winslash = "/", mustWork = FALSE) else candidates[[1]]
}

fms_latest_par <- function(dir_path) {
  if (!dir.exists(dir_path)) return(NA_character_)
  pars <- list.files(dir_path, pattern = "\\.par([0-9]+)?$", full.names = TRUE)
  if (length(pars) == 0) return(NA_character_)
  base_names <- basename(pars)
  stems <- sub("\\.par[0-9]*$", "", base_names)
  nums <- suppressWarnings(as.integer(stems))
  exact <- grepl("^[0-9]+\\.par$", base_names)
  info <- file.info(pars)
  ord <- order(
    !exact,
    -ifelse(is.finite(nums), nums, -1L),
    -ifelse(is.finite(as.numeric(info$mtime)), as.numeric(info$mtime), -Inf),
    base_names
  )
  pars[ord][[1]]
}

fms_core_source_files <- function(source_dir) {
  if (!dir.exists(source_dir)) return(character(0))
  files <- list.files(source_dir, full.names = TRUE, recursive = FALSE, all.files = FALSE, no.. = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (length(files) == 0) return(character(0))
  names <- basename(files)
  keep <- grepl("\\.par([0-9]+)?$", names) |
    grepl("\\.rpt$", names, ignore.case = TRUE) |
    grepl("\\.rds$", names, ignore.case = TRUE) |
    grepl("_map\\.[Rr]$", names) |
    names %in% c("avg_bio", "relative_depletion")
  keep <- keep & names != "fitted_source_manifest.rds"
  files[keep]
}

fms_file_record <- function(path) {
  info <- file.info(path)
  size <- if (is.na(info$size)) 0 else info$size
  raw <- if (size > 0) {
    readBin(path, what = "raw", n = size)
  } else {
    raw(0)
  }
  list(
    name = basename(path),
    size = size,
    mtime = as.character(info$mtime),
    raw = raw
  )
}

fms_restore_compact_bundle <- function(source_dir) {
  manifest_path <- file.path(source_dir, "fitted_source_manifest.rds")
  if (!file.exists(manifest_path)) return(invisible(FALSE))
  manifest <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
  if (!is.list(manifest) || !isTRUE(manifest$compact)) return(invisible(FALSE))

  records <- manifest$file_records
  if (!is.list(records) || length(records) == 0) {
    stop("Compact fitted source manifest has no file records: ", manifest_path)
  }

  for (rec in records) {
    name <- fms_first(rec$name)
    if (!nzchar(name) || basename(name) != name) {
      stop("Invalid compact fitted source file name in manifest: ", name)
    }
    out_path <- file.path(source_dir, name)
    writeBin(if (is.raw(rec$raw)) rec$raw else raw(0), out_path)
  }
  invisible(TRUE)
}

fms_model_info_field <- function(info, field) {
  if (is.list(info) && !is.null(info[[field]]) && length(info[[field]]) > 0) {
    return(as.character(info[[field]][[1]]))
  }
  ""
}

scan_fitted_model_dirs <- function(model_root = "model", repo_root = ".") {
  root <- fms_resolve_path(model_root, project_root = repo_root, parent_ok = FALSE)
  empty <- data.frame(
    id = character(0),
    name = character(0),
    source_dir = character(0),
    label = character(0),
    par_file = character(0),
    has_indepvar = logical(0),
    has_quantity = logical(0),
    base_dir = character(0),
    tokens = character(0),
    stringsAsFactors = FALSE
  )
  if (!dir.exists(root)) return(empty)

  info_files <- list.files(root, pattern = "^model_info\\.rds$", full.names = TRUE, recursive = TRUE)
  dirs <- unique(dirname(info_files))
  dirs <- dirs[vapply(dirs, function(d) file.exists(fms_latest_par(d)), logical(1))]
  if (length(dirs) == 0) return(empty)

  rows <- lapply(dirs, function(d) {
    info <- tryCatch(readRDS(file.path(d, "model_info.rds")), error = function(e) NULL)
    rel <- fms_path_relative(d, repo_root = repo_root)
    par <- fms_latest_par(d)
    tokens <- character(0)
    if (is.list(info) && !is.null(info$change_tokens)) {
      tokens <- as.character(info$change_tokens)
    } else if (is.list(info) && is.list(info$input_change_metadata) && !is.null(info$input_change_metadata$tokens)) {
      tokens <- as.character(info$input_change_metadata$tokens)
    }
    tokens <- unique(tokens[!is.na(tokens) & nzchar(trimws(tokens))])
    base_dir <- fms_model_info_field(info, "base_dir")
    display <- basename(d)
    if (exists("compact_input_name", mode = "function")) {
      display <- compact_input_name(display)
    }
    label_bits <- c(
      display,
      if (length(tokens) > 0) paste0("[", paste(tokens, collapse = ", "), "]") else "",
      if (nzchar(base_dir)) paste0("base=", basename(base_dir)) else "",
      paste0("par=", basename(par))
    )
    data.frame(
      id = fms_safe_id(rel),
      name = basename(d),
      source_dir = rel,
      label = paste(label_bits[nzchar(label_bits)], collapse = " "),
      par_file = basename(par),
      has_indepvar = file.exists(file.path(d, "indepvar.rpt")),
      has_quantity = any(file.exists(file.path(d, c("avg_bio", "relative_depletion")))),
      base_dir = base_dir,
      tokens = paste(tokens, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$name, out$source_dir), , drop = FALSE]
  out$id <- make.unique(out$id, sep = "_")
  row.names(out) <- NULL
  out
}

fms_create_bundle <- function(source_dir, bundle_dir = tempdir(), bundle_name = NULL, compact = FALSE) {
  source_dir <- fms_resolve_path(source_dir, project_root = getwd(), parent_ok = FALSE)
  if (!dir.exists(source_dir)) stop("Fitted source directory does not exist: ", source_dir)

  core_files <- fms_core_source_files(source_dir)
  par_file <- fms_latest_par(source_dir)
  if (is.na(par_file) || !file.exists(par_file)) {
    stop("Fitted source has no .par file: ", source_dir)
  }
  if (length(core_files) == 0) core_files <- par_file

  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  key <- fms_safe_id(fms_path_relative(source_dir, repo_root = getwd()))
  if (is.null(bundle_name) || !nzchar(as.character(bundle_name))) {
    bundle_name <- paste0("fitted_source_", key, ".tar.gz")
  }
  bundle_path <- file.path(bundle_dir, basename(bundle_name))

  stage <- tempfile("fitted_source_stage_", tmpdir = bundle_dir)
  dir.create(file.path(stage, "fitted_source"), recursive = TRUE, showWarnings = FALSE)
  file.copy(core_files, to = file.path(stage, "fitted_source"), overwrite = TRUE)
  manifest <- list(
    created_at = as.character(Sys.time()),
    source_dir = normalizePath(source_dir, winslash = "/", mustWork = FALSE),
    files = basename(core_files),
    par_file = basename(par_file),
    compact = FALSE,
    file_records = NULL
  )
  saveRDS(manifest, file = file.path(stage, "fitted_source", "fitted_source_manifest.rds"), compress = "xz")

  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(stage, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  setwd(stage)
  utils::tar(tarfile = bundle_path, files = "fitted_source", compression = "gzip", tar = "internal")
  if (!file.exists(bundle_path)) stop("Failed to create fitted source bundle: ", bundle_path)
  normalizePath(bundle_path, winslash = "/", mustWork = FALSE)
}

fms_run_prerequisite_model <- function(base_dir, model_dir, project_root = getwd()) {
  model_dir <- fms_first(model_dir)
  if (!nzchar(model_dir)) stop("auto_fitted_model_dir is empty.")

  model_dir_abs <- if (grepl("^/", model_dir)) model_dir else file.path(project_root, model_dir)
  par_file <- fms_latest_par(model_dir_abs)
  if (!is.na(par_file) && file.exists(par_file)) {
    cat("Using existing prerequisite model output:", model_dir_abs, "\n")
    return(normalizePath(model_dir_abs, winslash = "/", mustWork = FALSE))
  }

  runner <- file.path(project_root, "runners", "run_model.R")
  if (!file.exists(runner)) stop("Cannot find prerequisite model runner: ", runner)
  dir.create(dirname(model_dir_abs), recursive = TRUE, showWarnings = FALSE)

  env <- c(
    paste0("base_dir=", base_dir),
    paste0("model_dir=", model_dir),
    "fitted_model_source_enabled=0",
    "fitted_model_source_dir=",
    "fitted_model_bundle=",
    "auto_run_model_before_dependency=0"
  )

  cat("No fitted source selected; running prerequisite model first:\n")
  cat("  base_dir:", base_dir, "\n")
  cat("  model_dir:", model_dir, "\n")
  status <- system2(
    "Rscript",
    args = c("runners/run_model.R"),
    env = env,
    stdout = "",
    stderr = ""
  )
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || as.integer(status) != 0L) {
    stop("Prerequisite model run failed before dependent job (status=", status, ").")
  }

  par_file <- fms_latest_par(model_dir_abs)
  if (is.na(par_file) || !file.exists(par_file)) {
    stop("Prerequisite model finished but no .par was found in: ", model_dir_abs)
  }
  normalizePath(model_dir_abs, winslash = "/", mustWork = FALSE)
}

fms_extract_bundle <- function(bundle_path, project_root = getwd(), source_id = "") {
  bundle_path <- fms_resolve_path(bundle_path, project_root = project_root, parent_ok = TRUE)
  if (!file.exists(bundle_path)) stop("Fitted model bundle does not exist: ", bundle_path)
  key <- fms_safe_id(if (nzchar(source_id)) source_id else basename(bundle_path))
  extract_dir <- file.path(project_root, ".fitted_sources", "extracted", key)
  if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE, force = TRUE)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(bundle_path, exdir = extract_dir)
  source_dir <- file.path(extract_dir, "fitted_source")
  if (!dir.exists(source_dir)) source_dir <- extract_dir
  fms_restore_compact_bundle(source_dir)
  par_file <- fms_latest_par(source_dir)
  if (is.na(par_file) || !file.exists(par_file)) {
    manifest_path <- file.path(source_dir, "fitted_source_manifest.rds")
    files_now <- paste(basename(list.files(source_dir, all.files = FALSE, no.. = TRUE)), collapse = ", ")
    stop(
      "Extracted fitted source bundle has no .par file after restore: ", source_dir,
      "; manifest=", file.exists(manifest_path),
      "; files=", if (nzchar(files_now)) files_now else "<none>"
    )
  }
  source_dir
}

ensure_fitted_model_source <- function(base_dir_abs, base_dir = "", model_dir = "", project_root = getwd()) {
  enabled <- fms_truthy(Sys.getenv("fitted_model_source_enabled", "0"))
  bundle <- fms_first(Sys.getenv("fitted_model_bundle", ""))
  source_dir_env <- fms_first(Sys.getenv("fitted_model_source_dir", ""))
  auto_run_model <- fms_truthy(Sys.getenv("auto_run_model_before_dependency", "0"))
  if (!enabled && !nzchar(bundle) && !nzchar(source_dir_env) && !auto_run_model) return(base_dir_abs)

  source_id <- fms_first(Sys.getenv("fitted_model_source_id", "fitted"))
  source_dir <- if (nzchar(bundle)) {
    fms_extract_bundle(bundle, project_root = project_root, source_id = source_id)
  } else if (!enabled && !nzchar(source_dir_env) && auto_run_model) {
    source_model_dir <- fms_first(
      Sys.getenv("auto_fitted_model_dir", ""),
      default = file.path(model_dir, "_source_model")
    )
    source_id <- fms_first(Sys.getenv("fitted_model_source_id", ""), default = fms_safe_id(source_model_dir))
    fms_run_prerequisite_model(
      base_dir = if (nzchar(base_dir)) base_dir else base_dir_abs,
      model_dir = source_model_dir,
      project_root = project_root
    )
  } else {
    fms_resolve_path(source_dir_env, project_root = project_root, parent_ok = FALSE)
  }

  if (!dir.exists(source_dir)) stop("Resolved fitted model source does not exist: ", source_dir)
  par_file <- fms_latest_par(source_dir)
  if (is.na(par_file) || !file.exists(par_file)) {
    stop("Resolved fitted model source has no .par file: ", source_dir)
  }

  base_key <- fms_safe_id(if (nzchar(base_dir)) base_dir else base_dir_abs, "base")
  source_key <- fms_safe_id(source_id, "fitted")
  merge_dir <- file.path(
    project_root,
    ".fitted_sources",
    "merged",
    paste0(base_key, "__", source_key, "__", Sys.getpid())
  )
  if (dir.exists(merge_dir)) unlink(merge_dir, recursive = TRUE, force = TRUE)
  dir.create(merge_dir, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(base_dir_abs)) stop("Base input directory does not exist before fitted-source merge: ", base_dir_abs)
  base_files <- list.files(base_dir_abs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(base_files) > 0) file.copy(base_files, to = merge_dir, overwrite = TRUE, recursive = TRUE)

  fitted_files <- fms_core_source_files(source_dir)
  if (length(fitted_files) == 0) fitted_files <- par_file
  file.copy(fitted_files, to = merge_dir, overwrite = TRUE, recursive = TRUE)

  Sys.setenv(fitted_model_source_resolved_dir = normalizePath(source_dir, winslash = "/", mustWork = FALSE))
  Sys.setenv(fitted_model_merged_base_dir = normalizePath(merge_dir, winslash = "/", mustWork = FALSE))
  cat("Using fitted model source overlay:\n")
  cat("  source:", normalizePath(source_dir, winslash = "/", mustWork = FALSE), "\n")
  cat("  par:", basename(par_file), "\n")
  cat("  merged base:", normalizePath(merge_dir, winslash = "/", mustWork = FALSE), "\n")
  normalizePath(merge_dir, winslash = "/", mustWork = FALSE)
}
