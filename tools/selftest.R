st_truthy <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- tolower(trimws(as.character(x[[1]])))
  if (!nzchar(txt) || is.na(txt)) return(default)
  txt %in% c("1", "true", "yes", "y", "on")
}

st_first <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- trimws(as.character(x[[1]]))
  if (!nzchar(txt) || is.na(txt)) default else txt
}

st_safe_id <- function(x, default = "selftest") {
  txt <- gsub("[^A-Za-z0-9]+", "_", as.character(x[[1]]))
  txt <- gsub("^_+|_+$", "", txt)
  if (!nzchar(txt)) default else txt
}

st_resolve_path <- function(path, project_root = getwd(), must_work = FALSE) {
  path <- st_first(path)
  if (!nzchar(path)) return("")
  out <- if (grepl("^/", path)) path else file.path(project_root, path)
  normalizePath(out, winslash = "/", mustWork = must_work)
}

st_latest_par <- function(dir_path) {
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

st_first_file <- function(dir_path, pattern, required = TRUE) {
  hits <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  if (length(hits) == 0) {
    if (isTRUE(required)) stop("No file matching ", pattern, " in ", dir_path)
    return(NA_character_)
  }
  hits[[1]]
}

st_copy_dir_contents <- function(from, to, overwrite = TRUE) {
  if (!dir.exists(from)) stop("Source directory does not exist: ", from)
  if (dir.exists(to)) unlink(to, recursive = TRUE, force = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(files) > 0) {
    ok <- file.copy(files, to = to, overwrite = overwrite, recursive = TRUE, copy.date = TRUE)
    if (!all(ok)) stop("Failed to copy some files from ", from, " to ", to)
  }
  invisible(to)
}

st_remove_mfcl_run_outputs <- function(dir_path, keep_par = character(), remove_par = TRUE) {
  if (!dir.exists(dir_path)) return(invisible(0L))
  keep_par <- basename(as.character(keep_par))
  keep_par <- keep_par[nzchar(keep_par) & !is.na(keep_par)]

  stale <- list.files(
    dir_path,
    pattern = "(\\.rep$|\\.rpt$|^model_info\\.rds$|^model_payload\\.rds$|^mfcl_.*_log\\.txt$)",
    full.names = TRUE
  )
  if (isTRUE(remove_par)) {
    pars <- list.files(dir_path, pattern = "\\.par([0-9]+)?$", full.names = TRUE)
    if (length(keep_par) > 0) pars <- pars[!basename(pars) %in% keep_par]
    stale <- c(stale, pars)
  }
  stale <- unique(stale[file.exists(stale)])
  if (length(stale) > 0) unlink(stale, force = TRUE)
  invisible(length(stale))
}

st_key <- function(year, month, week, fishery) {
  paste(as.integer(year), as.integer(month), as.integer(week), as.integer(fishery), sep = "_")
}

st_num_key <- function(x) {
  sprintf("%.10g", suppressWarnings(as.numeric(x)))
}

st_parse_numeric_tokens <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  toks <- unlist(strsplit(paste(as.character(x), collapse = " "), "[,[:space:]]+"))
  toks <- toks[nzchar(toks)]
  suppressWarnings(as.numeric(toks))
}

st_make_switch <- function(changes) {
  if (is.data.frame(changes)) changes <- as.matrix(changes)
  if (is.null(dim(changes))) {
    if (length(changes) %% 3 != 0) stop("Switch changes must be type/index/value triplets.")
    changes <- matrix(changes, ncol = 3, byrow = TRUE)
  }
  if (ncol(changes) != 3) stop("Switch changes must have three columns: type, index, value.")
  changes <- apply(changes, 2, as.integer)
  n_changes <- nrow(changes)
  paste("-switch", n_changes, paste(as.vector(t(changes)), collapse = " "))
}

st_normalise_switch <- function(switch_args) {
  switch_args <- st_first(switch_args)
  if (!nzchar(switch_args)) return(st_default_switch())
  if (!grepl("(^|[[:space:]])-switch([[:space:]]|$)", switch_args)) {
    switch_args <- paste("-switch", switch_args)
  }
  tokens <- unlist(strsplit(trimws(sub("(^|.*[[:space:]])-switch[[:space:]]+", "", switch_args)), "[[:space:]]+"))
  tokens <- tokens[nzchar(tokens)]
  vals <- suppressWarnings(as.integer(tokens))
  if (length(vals) < 1L || anyNA(vals)) stop("Invalid -switch argument: ", switch_args)
  n_changes <- vals[[1]]
  expected <- 1L + 3L * n_changes
  if (length(vals) != expected) {
    stop(
      "Invalid -switch argument: declared ", n_changes, " changes but found ",
      (length(vals) - 1L) / 3, " triplets in: ", switch_args
    )
  }
  paste("-switch", paste(vals, collapse = " "))
}

st_ensure_switch_change <- function(switch_args, type, index, value) {
  switch_args <- st_normalise_switch(switch_args)
  vals <- suppressWarnings(as.integer(unlist(strsplit(trimws(sub("(^|.*[[:space:]])-switch[[:space:]]+", "", switch_args)), "[[:space:]]+"))))
  if (length(vals) < 1L || anyNA(vals)) stop("Invalid -switch argument: ", switch_args)
  n_changes <- vals[[1]]
  changes <- if (n_changes > 0L) matrix(vals[-1L], ncol = 3, byrow = TRUE) else matrix(integer(0), ncol = 3)
  keep <- !(changes[, 1] == as.integer(type) & changes[, 2] == as.integer(index))
  changes <- rbind(changes[keep, , drop = FALSE], c(as.integer(type), as.integer(index), as.integer(value)))
  st_make_switch(changes)
}

st_default_switch <- function() {
  st_make_switch(rbind(
    c(1, 1, 1),
    c(1, 241, 1),
    c(1, 242, 1),
    c(1, 244, 1),
    c(1, 190, 1),
    c(1, 186, 1),
    c(1, 187, 1),
    c(1, 188, 1),
    c(1, 189, 1),
    c(2, 96, 200)
  ))
}

st_projection_step7_switch <- function(nsims = 1L) {
  st_make_switch(rbind(
    c(1, 145, 7),
    c(2, 20, as.integer(nsims))
  ))
}

st_projection_step8_switch <- function(nsims = 1L) {
  st_make_switch(rbind(
    c(1, 145, 8),
    c(1, 234, 1),
    c(1, 235, 20),
    c(1, 237, 0),
    c(2, 20, as.integer(nsims))
  ))
}

st_native_tag_switch <- function(nsims = 1L) {
  st_make_switch(rbind(
    c(1, 1, 1),
    c(1, 241, 1),
    c(1, 242, 1),
    c(1, 190, 1),
    c(1, 186, 1),
    c(1, 187, 1),
    c(1, 188, 1),
    c(1, 246, 0),
    c(2, 96, 200),
    c(2, 20, as.integer(nsims)),
    c(2, 183, 1)
  ))
}

st_system_mfcl <- function(cmd, log_file) {
  cat("MFCL command:\n", cmd, "\n")
  status <- system(paste(cmd, ">", shQuote(log_file), "2>&1"), intern = FALSE)
  as.integer(status)
}

st_write_minimal_tag_sim <- function(out_file,
                                     frq_file,
                                     n_release = 100,
                                     reporting_rate = 0.5,
                                     year = NA_integer_,
                                     month = NA_integer_,
                                     fishery = NA_integer_) {
  frq_obj <- read.MFCLFrq(frq_file)
  real_df <- realisations(frq_obj)
  real_df <- real_df[is.finite(real_df$year) & is.finite(real_df$month), , drop = FALSE]
  if (nrow(real_df) == 0) stop("Cannot build .tag_sim because the .frq has no fishing incidents: ", frq_file)

  terminal_year <- if (is.finite(year)) as.integer(year) else max(real_df$year, na.rm = TRUE)
  terminal_month <- if (is.finite(month)) as.integer(month) else max(real_df$month[real_df$year == terminal_year], na.rm = TRUE)
  terminal_fishery <- if (is.finite(fishery)) {
    as.integer(fishery)
  } else {
    suppressWarnings(as.integer(real_df$fishery[real_df$year == terminal_year & real_df$month == terminal_month][[1]]))
  }
  if (!is.finite(terminal_fishery)) terminal_fishery <- 1L
  n_fisheries <- suppressWarnings(as.integer(frq_obj@n_fisheries))
  if (!is.finite(n_fisheries) || n_fisheries < 1L) n_fisheries <- max(real_df$fishery, na.rm = TRUE)

  lines <- c(
    "# RELEASE GROUPS",
    "1",
    "#",
    "# 1 - RELEASE REGION YEAR MONTH Fishery Predicted numbers",
    sprintf("1 %d %d %d %d", as.integer(terminal_year), as.integer(terminal_month), terminal_fishery, as.integer(n_release)),
    "#",
    "# Reporting rates for each event: rows = fisheries; cols = tag events",
    rep(sprintf("%.6f", reporting_rate), n_fisheries)
  )
  writeLines(lines, out_file)
  invisible(out_file)
}

st_projection_controls <- function(frq_obj, average_years = numeric(0)) {
  max_year <- as.integer(range(frq_obj)["maxyear"])
  if (length(average_years) == 0 || !all(is.finite(average_years))) {
    average_years <- (max_year - 2L):max_year
  }
  average_years <- as.character(as.integer(average_years))
  n_fish <- as.integer(n_fisheries(frq_obj))
  dff <- data_flags(frq_obj)
  rd <- realisations(frq_obj)
  has_effort <- vapply(seq_len(n_fish), function(i) {
    vals <- rd$effort[rd$fishery == i]
    any(is.finite(vals) & vals > 0)
  }, logical(1))
  caeff <- ifelse(dff[1, seq_len(n_fish)] == 1 | !has_effort, 1L, 2L)
  controls <- data.frame(
    name = paste0("F", seq_len(n_fish)),
    region = as.numeric(c(region_fish(frq_obj))),
    caeff = as.integer(caeff),
    scaler = 1,
    ess_length = NA_real_,
    ess_weight = NA_real_
  )
  list(average_years = average_years, controls = controls)
}

st_prepare_projection_input <- function(sim_dir,
                                        program_path_abs,
                                        frq_file,
                                        par_file,
                                        projection_years = NA_integer_,
                                        average_years = numeric(0),
                                        projection_root = "selftest_proj") {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sim_dir)

  frq_obj <- read.MFCLFrq(frq_file)
  par_obj <- read.MFCLPar(par_file)
  if (length(projection_years) == 0 || !is.finite(projection_years)) {
    projection_years <- max(30L, as.integer(dimensions(par_obj)["agecls"]))
  }
  ctl <- st_projection_controls(frq_obj, average_years = average_years)
  first_projection_year <- as.integer(range(frq_obj)["maxyear"]) + 1L
  proj_ctl <- MFCLprojControl(
    nyears = as.integer(projection_years),
    nsims = 1,
    avyrs = ctl$average_years,
    fprojyr = first_projection_year,
    controls = ctl$controls
  )
  proj_frq <- generate(frq_obj, proj_ctl)

  proj_frq_file <- file.path(sim_dir, paste0(projection_root, ".frq"))
  zero_par_file <- file.path(sim_dir, paste0(projection_root, "_00.par"))
  proj_par_file <- file.path(sim_dir, paste0(projection_root, ".par"))
  FLR4MFCL::write(proj_frq, proj_frq_file)

  tag_file <- st_first_file(sim_dir, "\\.tag$", required = FALSE)
  if (!is.na(tag_file)) file.copy(tag_file, file.path(sim_dir, paste0(projection_root, ".tag")), overwrite = TRUE)
  alk_file <- st_first_file(sim_dir, "\\.age_length$", required = FALSE)
  if (!is.na(alk_file)) file.copy(alk_file, file.path(sim_dir, paste0(projection_root, ".age_length")), overwrite = TRUE)
  ini_file <- st_first_file(sim_dir, "\\.ini$")

  makepar_cmd <- paste(
    shQuote(program_path_abs),
    shQuote(basename(proj_frq_file)),
    shQuote(basename(ini_file)),
    shQuote(basename(zero_par_file)),
    "-makepar"
  )
  makepar_log <- file.path(sim_dir, "mfcl_selftest_projection_makepar_log.txt")
  makepar_status <- st_system_mfcl(makepar_cmd, makepar_log)
  if (!file.exists(zero_par_file)) {
    stop("Projection makepar did not create ", zero_par_file, "; see ", makepar_log)
  }

  zero_par <- read.MFCLPar(zero_par_file)
  proj_par <- generate(par_obj, zero_par, proj_frq)
  FLR4MFCL::write(proj_par, proj_par_file)

  st_write_minimal_tag_sim(
    file.path(sim_dir, paste0(projection_root, ".tag_sim")),
    proj_frq_file
  )

  info <- list(
    projection_root = projection_root,
    projection_years = as.integer(projection_years),
    average_years = ctl$average_years,
    projection_frq = proj_frq_file,
    projection_par = proj_par_file,
    first_projection_year = first_projection_year,
    last_projection_year = as.integer(range(proj_frq)["maxyear"]),
    makepar_status = makepar_status,
    makepar_log = makepar_log
  )
  saveRDS(info, file.path(sim_dir, "selftest_projection_info.rds"), compress = "xz")
  info
}

st_run_native_tag_simulation <- function(sim_dir,
                                         program_path_abs,
                                         projection_info,
                                         seeds,
                                         nsims = 1L,
                                         output_par = "selftest_native_tag.par") {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sim_dir)

  writeLines(as.character(as.integer(seeds$tag)), file.path(sim_dir, "simseed"))
  step7_cmd <- paste(
    shQuote(program_path_abs),
    shQuote(basename(projection_info$projection_frq)),
    shQuote(basename(projection_info$projection_par)),
    shQuote("selftest_stoch.par"),
    st_projection_step7_switch(nsims)
  )
  step7_log <- file.path(sim_dir, "mfcl_selftest_stoch7_log.txt")
  step7_status <- st_system_mfcl(step7_cmd, step7_log)
  if (!file.exists(file.path(sim_dir, paste0(projection_info$projection_root, ".dep"))) ||
      !file.exists(file.path(sim_dir, "proparams.val")) ||
      !file.exists(file.path(sim_dir, "proparams_noeff.val"))) {
    stop("Stochastic projection step 7 did not create dependency files; see ", step7_log)
  }

  step8_cmd <- paste(
    shQuote(program_path_abs),
    shQuote(basename(projection_info$projection_frq)),
    shQuote(basename(projection_info$projection_par)),
    shQuote("selftest_stoch.par"),
    st_projection_step8_switch(nsims)
  )
  step8_log <- file.path(sim_dir, "mfcl_selftest_stoch8_log.txt")
  step8_status <- st_system_mfcl(step8_cmd, step8_log)
  required_stoch <- c("simulated_numbers_at_age", "simulated_numbers_at_age_noeff", "simyears")
  missing_stoch <- required_stoch[!file.exists(file.path(sim_dir, required_stoch))]
  if (length(missing_stoch) > 0) {
    stop("Stochastic projection step 8 missing: ", paste(missing_stoch, collapse = ", "), "; see ", step8_log)
  }

  seed_args <- c(
    "-length_seed", as.integer(seeds$length),
    "-weight_seed", as.integer(seeds$weight),
    "-catch_seed", as.integer(seeds$catch),
    "-effort_seed", as.integer(seeds$effort),
    "-cpue_seed", as.integer(seeds$cpue),
    "-tag_seed", as.integer(seeds$tag)
  )
  tag_cmd <- paste(
    shQuote(program_path_abs),
    shQuote(basename(projection_info$projection_frq)),
    shQuote(basename(projection_info$projection_par)),
    shQuote(output_par),
    st_native_tag_switch(nsims),
    paste(seed_args, collapse = " ")
  )
  tag_log <- file.path(sim_dir, "mfcl_selftest_native_tag_log.txt")
  tag_status <- st_system_mfcl(tag_cmd, tag_log)
  realtag <- list.files(sim_dir, pattern = "^report\\.realtag_[0-9]+$", full.names = TRUE)
  if (length(realtag) == 0) {
    stop("Native tag simulation did not create report.realtag_#; see ", tag_log)
  }

  info <- list(
    step7_status = step7_status,
    step8_status = step8_status,
    tag_status = tag_status,
    report_realtag = basename(realtag[[1]]),
    step7_log = step7_log,
    step8_log = step8_log,
    native_tag_log = tag_log
  )
  saveRDS(info, file.path(sim_dir, "selftest_native_tag_info.rds"), compress = "xz")
  info
}

st_run_mfcl_simulation <- function(sim_dir,
                                   program_path_abs,
                                   frq_file,
                                   par_file,
                                   output_par = "selftest_sim.par",
                                   switch_args = st_default_switch(),
                                   seeds = list(length = 101L, weight = 102L, catch = 103L, effort = 104L, cpue = 105L, tag = 106L),
                                   log_file = file.path(sim_dir, "mfcl_selftest_sim_log.txt")) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sim_dir)

  seed_args <- c(
    "-length_seed", as.integer(seeds$length),
    "-weight_seed", as.integer(seeds$weight),
    "-catch_seed", as.integer(seeds$catch),
    "-effort_seed", as.integer(seeds$effort),
    "-cpue_seed", as.integer(seeds$cpue),
    "-tag_seed", as.integer(seeds$tag)
  )
  switch_args <- st_ensure_switch_change(switch_args, 1L, 244L, 1L)
  cmd <- paste(
    shQuote(program_path_abs),
    shQuote(basename(frq_file)),
    shQuote(basename(par_file)),
    shQuote(output_par),
    switch_args,
    paste(seed_args, collapse = " ")
  )

  cat("MFCL simulation command:\n", cmd, "\n")
  status <- system(paste(cmd, ">", shQuote(log_file), "2>&1"), intern = FALSE)

  sim_files <- c("test_lw_sim", "test_lw_sim_alt", "catch_sim", "effort_sim", "cpue_sim", "cpue_sim_true")
  present <- sim_files[file.exists(file.path(sim_dir, sim_files))]
  info <- list(
    command = cmd,
    status = as.integer(status),
    output_par = output_par,
    seeds = seeds,
    expected_files = sim_files,
    present_files = present,
    log_file = log_file
  )
  saveRDS(info, file.path(sim_dir, "selftest_sim_info.rds"), compress = "xz")
  info
}

st_parse_catch_sim <- function(path, frq_obj, value = c("simulated", "expected")) {
  value <- match.arg(value)
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  data_lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  if (length(data_lines) == 0) return(NULL)
  vals <- utils::read.table(text = paste(data_lines, collapse = "\n"), fill = TRUE)
  sim_val <- if (identical(value, "expected") || ncol(vals) < 2) vals[[1]] else vals[[2]]
  real_df <- realisations(frq_obj)
  if (length(sim_val) < nrow(real_df)) {
    warning("catch_sim has fewer rows than .frq realisations; using available rows only.")
  }
  n <- min(length(sim_val), nrow(real_df))
  data.frame(
    key = st_key(real_df$year[seq_len(n)], real_df$month[seq_len(n)], real_df$week[seq_len(n)], real_df$fishery[seq_len(n)]),
    catch = suppressWarnings(as.numeric(sim_val[seq_len(n)])),
    stringsAsFactors = FALSE
  )
}

st_parse_effort_sim <- function(path, frq_obj) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  data_lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
  if (length(data_lines) == 0) return(NULL)
  real_df <- realisations(frq_obj)
  out <- lapply(data_lines, function(line) {
    vals <- suppressWarnings(as.numeric(unlist(strsplit(trimws(line), "[[:space:]]+"))))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2) return(NULL)
    fishery <- as.integer(vals[[1]])
    effort <- vals[-1]
    fish_df <- real_df[real_df$fishery == fishery, , drop = FALSE]
    n <- min(length(effort), nrow(fish_df))
    if (n == 0) return(NULL)
    data.frame(
      key = st_key(fish_df$year[seq_len(n)], fish_df$month[seq_len(n)], fish_df$week[seq_len(n)], fish_df$fishery[seq_len(n)]),
      fishery = fish_df$fishery[seq_len(n)],
      effort = suppressWarnings(as.numeric(effort[seq_len(n)])),
      stringsAsFactors = FALSE
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

st_parse_cpue_sim <- function(path, frq_obj) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  fish_lines <- grep("^\\s*Fishery\\s*:", lines, value = TRUE)
  if (length(fish_lines) == 0) return(NULL)
  real_df <- realisations(frq_obj)
  out <- lapply(fish_lines, function(line) {
    fishery <- suppressWarnings(as.integer(sub("^\\s*Fishery\\s*:\\s*([0-9]+).*", "\\1", line)))
    value_txt <- sub("^\\s*Fishery\\s*:\\s*[0-9]+\\s*", "", line)
    vals <- suppressWarnings(as.numeric(unlist(strsplit(trimws(value_txt), "[[:space:]]+"))))
    vals <- vals[is.finite(vals)]
    fish_df <- real_df[real_df$fishery == fishery, , drop = FALSE]
    n <- min(length(vals), nrow(fish_df))
    if (n == 0) return(NULL)
    data.frame(
      key = st_key(fish_df$year[seq_len(n)], fish_df$month[seq_len(n)], fish_df$week[seq_len(n)], fish_df$fishery[seq_len(n)]),
      fishery = fish_df$fishery[seq_len(n)],
      cpue = vals[seq_len(n)],
      stringsAsFactors = FALSE
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

st_fishery_flag_values <- function(par_file, flag) {
  if (is.null(par_file) || length(par_file) == 0 || is.na(par_file) || !file.exists(par_file)) {
    return(numeric(0))
  }
  par_obj <- read.MFCLPar(par_file)
  flags <- par_obj@flags
  vals <- flags[flags$flagtype < 0 & flags$flag == as.integer(flag), c("flagtype", "value"), drop = FALSE]
  if (nrow(vals) == 0) return(numeric(0))
  out <- suppressWarnings(as.numeric(vals$value))
  names(out) <- as.character(abs(as.integer(vals$flagtype)))
  out
}

st_flag_value <- function(par_file, flagtype, flag) {
  if (is.null(par_file) || is.na(par_file) || !file.exists(par_file)) return(NA_real_)
  par_obj <- tryCatch(read.MFCLPar(par_file), error = function(e) NULL)
  if (is.null(par_obj)) return(NA_real_)
  st_flag_from_obj(par_obj, flagtype, flag)
}

st_flag_from_obj <- function(par_obj, flagtype, flag) {
  flags <- tryCatch(par_obj@flags, error = function(e) NULL)
  if (!is.data.frame(flags) || !all(c("flagtype", "flag", "value") %in% names(flags))) return(NA_real_)
  hit <- flags$flagtype == as.integer(flagtype) & flags$flag == as.integer(flag)
  if (!any(hit, na.rm = TRUE)) return(NA_real_)
  val <- suppressWarnings(as.numeric(flags$value[which(hit)[[1]]]))
  if (length(val) == 0 || !is.finite(val[[1]])) NA_real_ else val[[1]]
}

st_composition_likelihood_info <- function(par_file, par_obj = NULL) {
  if (is.null(par_obj)) par_obj <- tryCatch(read.MFCLPar(par_file), error = function(e) NULL)
  flag_value <- function(flag) {
    if (is.null(par_obj)) st_flag_value(par_file, 1L, flag) else st_flag_from_obj(par_obj, 1L, flag)
  }
  len_code <- flag_value(141L)
  weight_code <- flag_value(139L)
  length_name <- switch(
    as.character(as.integer(len_code)),
    "0" = "square",
    "1" = "square0",
    "2" = "square0a",
    "3" = "square_fita",
    "4" = "dirichlet_multinomial",
    "5" = "dirichlet_multinomial_mixture",
    "6" = "multinomial",
    "7" = if (isTRUE(flag_value(299L) != 0)) "logistic_normal_heteroscedastic" else "logistic_normal",
    "8" = "square_t",
    "9" = "self_scaling_multinomial_re",
    "10" = "self_scaling_multinomial",
    "11" = "dirichlet_multinomial_nore",
    "12" = "self_scaling_multinomial_re_v3",
    paste0("unknown_", len_code)
  )
  weight_name <- switch(
    as.character(as.integer(weight_code)),
    "0" = "square",
    "3" = "square_fita",
    "7" = if (isTRUE(flag_value(289L) != 0)) "logistic_normal_heteroscedastic" else "logistic_normal",
    "8" = "square_t",
    "9" = "self_scaling_multinomial_re",
    "10" = "self_scaling_multinomial",
    "11" = "dirichlet_multinomial_nore",
    "12" = "self_scaling_multinomial_re_v3",
    paste0("unknown_", weight_code)
  )
  list(
    length_code = as.integer(len_code),
    weight_code = as.integer(weight_code),
    length_name = length_name,
    weight_name = weight_name
  )
}

st_supported_composition_sampler <- function(name) {
  name %in% c(
    "multinomial",
    "square",
    "square0",
    "square0a",
    "square_fita",
    "square_t",
    "dirichlet_multinomial",
    "dirichlet_multinomial_mixture",
    "dirichlet_multinomial_nore",
    "logistic_normal",
    "logistic_normal_heteroscedastic"
  )
}

st_assert_supported_composition_likelihood <- function(par_file) {
  info <- st_composition_likelihood_info(par_file)
  unsupported <- c(
    if (!st_supported_composition_sampler(info$length_name)) paste0("length=", info$length_name, " (flag 141=", info$length_code, ")"),
    if (!st_supported_composition_sampler(info$weight_name)) paste0("weight=", info$weight_name, " (flag 139=", info$weight_code, ")")
  )
  if (length(unsupported) > 0) {
    stop(
      "Self-test composition pseudo sampling is not implemented for the active likelihood: ",
      paste(unsupported, collapse = "; "),
      ". Refusing to use MFCL native multinomial test_lw_sim or an unsupported approximate R sampler."
    )
  }
  info
}

st_catch_conditioned <- function(par_file) {
  af373 <- st_flag_value(par_file, 1L, 373L)
  ff92 <- st_flag_value(par_file, 2L, 92L)
  isTRUE(is.finite(af373) && af373 != 0) || isTRUE(is.finite(ff92) && ff92 != 0)
}

st_effort_conditioned <- function(par_file) {
  FALSE
}

st_apply_cpue_likelihood_error <- function(cpue_df, par_file, seed = 105L) {
  if (is.null(cpue_df) || nrow(cpue_df) == 0) return(cpue_df)
  ff92 <- st_fishery_flag_values(par_file, 92L)
  if (length(ff92) == 0) {
    warning("Cannot read fish_flags(92); using MFCL cpue_sim values as generated.")
    cpue_df$error_source <- "mfcl_cpue_sim"
    cpue_df$cpue_cv <- NA_real_
    cpue_df$cpue_sdlog <- NA_real_
    return(cpue_df)
  }
  cv <- abs(suppressWarnings(as.numeric(ff92[as.character(cpue_df$fishery)]))) / 100
  cv[!is.finite(cv)] <- 0
  sdlog <- sqrt(log1p(cv * cv))
  set.seed(as.integer(seed) + 2L)
  eps <- stats::rnorm(nrow(cpue_df), mean = -0.5 * sdlog * sdlog, sd = sdlog)
  cpue_df$cpue <- cpue_df$cpue * exp(eps)
  cpue_df$error_source <- "cpue_sim_true + fish_flags(92) CV lognormal"
  cpue_df$cpue_cv <- cv
  cpue_df$cpue_sdlog <- sdlog
  cpue_df
}

st_read_next_numeric_line <- function(lines, i) {
  n <- length(lines)
  while (i <= n) {
    txt <- trimws(lines[[i]])
    if (nzchar(txt) && !grepl("^#", txt)) {
      vals <- suppressWarnings(as.numeric(unlist(strsplit(txt, "[[:space:]]+"))))
      return(list(index = i, values = vals[!is.na(vals)]))
    }
    i <- i + 1L
  }
  NULL
}

st_parse_lw_sim <- function(path, frq_obj) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  lf <- lf_range(frq_obj)
  length_bins <- seq(lf["LFFirst"], by = lf["LFWidth"], length.out = lf["LFIntervals"])
  weight_bins <- seq(lf["WFFirst"], by = lf["WFWidth"], length.out = lf["WFIntervals"])

  out <- list()
  type <- NA_character_
  projection <- NA_integer_
  seed <- NA_integer_
  i <- 1L
  while (i <= length(lines)) {
    txt <- trimws(lines[[i]])
    if (!nzchar(txt)) {
      i <- i + 1L
      next
    }
    if (grepl("^#\\s*Simulated length", txt, ignore.case = TRUE)) {
      type <- "length"
      i <- i + 1L
      next
    }
    if (grepl("^#\\s*Simulated weight", txt, ignore.case = TRUE)) {
      type <- "weight"
      i <- i + 1L
      next
    }
    if (grepl("^#\\s*projection", txt, ignore.case = TRUE)) {
      projection <- suppressWarnings(as.integer(tail(strsplit(txt, "[[:space:]]+")[[1]], 1)))
      i <- i + 1L
      next
    }
    if (grepl("^#\\s*seed", txt, ignore.case = TRUE)) {
      seed <- suppressWarnings(as.integer(tail(strsplit(txt, "[[:space:]]+")[[1]], 1)))
      i <- i + 1L
      next
    }
    if (grepl("^#", txt) || is.na(type)) {
      i <- i + 1L
      next
    }

    key_vals <- suppressWarnings(as.numeric(unlist(strsplit(txt, "[[:space:]]+"))))
    if (length(key_vals) < 4) {
      i <- i + 1L
      next
    }
    sample_line <- st_read_next_numeric_line(lines, i + 1L)
    data_line <- if (!is.null(sample_line)) st_read_next_numeric_line(lines, sample_line$index + 1L) else NULL
    if (is.null(sample_line) || is.null(data_line)) break

    bins <- if (identical(type, "length")) length_bins else weight_bins
    vals <- data_line$values
    if (length(vals) > length(bins)) vals <- vals[seq_along(bins)]
    if (length(vals) < length(bins)) vals <- c(vals, rep(NA_real_, length(bins) - length(vals)))
    out[[length(out) + 1L]] <- data.frame(
      type = type,
      projection = projection,
      seed = seed,
      year = as.integer(key_vals[[1]]),
      month = as.integer(key_vals[[2]]),
      week = as.integer(key_vals[[3]]),
      fishery = as.integer(key_vals[[4]]),
      bin = bins,
      freq = vals,
      sample_size = suppressWarnings(as.numeric(sample_line$values[[1]])),
      stringsAsFactors = FALSE
    )
    i <- data_line$index + 1L
  }

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

st_simple_rdirichlet <- function(alpha) {
  alpha <- pmax(suppressWarnings(as.numeric(alpha)), 1e-8)
  x <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
  if (!is.finite(sum(x)) || sum(x) <= 0) return(rep(1 / length(alpha), length(alpha)))
  x / sum(x)
}

st_multinom_freq <- function(p, n) {
  p <- pmax(suppressWarnings(as.numeric(p)), 0)
  if (!is.finite(sum(p)) || sum(p) <= 0) p[] <- 1
  p <- p / sum(p)
  n <- max(0L, as.integer(floor(n)))
  as.numeric(stats::rmultinom(1L, size = n, prob = p)[, 1L])
}

st_age_length_draw_size_mode <- function() {
  mode <- tolower(st_first(Sys.getenv("selftest_age_length_draw_size", "effective")))
  if (mode %in% c("effective", "ess", "likelihood", "likelihood_matched")) return("effective")
  if (mode %in% c("observed", "obs", "input", "raw")) return("observed")
  stop("Unsupported selftest_age_length_draw_size: ", mode, ". Use effective or observed.")
}

st_keep_model_payload <- function() {
  st_truthy(Sys.getenv("selftest_keep_model_payload", "0"), default = FALSE)
}

st_par_slot_value <- function(par_file, slot_name) {
  if (is.null(par_file) || length(par_file) == 0 || is.na(par_file) || !file.exists(par_file)) {
    return(NA_real_)
  }
  par_obj <- tryCatch(read.MFCLPar(par_file), error = function(e) NULL)
  suppressWarnings(tryCatch(as.numeric(slot(par_obj, slot_name)), error = function(e) NA_real_))
}

st_profile_value_vector <- function(par_obj, profile_name) {
  out <- tryCatch({
    if (identical(profile_name, "totpop")) {
      tot_pop(par_obj)
    } else if (identical(profile_name, "LorenM")) {
      as.vector(aperm(log_m(par_obj), c(4, 1, 2, 3, 5, 6)))
    } else if (identical(profile_name, "L1")) {
      growth(par_obj)[1, 1]
    } else if (identical(profile_name, "L2")) {
      growth(par_obj)[2, 1]
    } else if (identical(profile_name, "kappa")) {
      growth(par_obj)[3, 1]
    } else if (identical(profile_name, "s1")) {
      growth_var_pars(par_obj)[1, 1]
    } else if (identical(profile_name, "s2")) {
      growth_var_pars(par_obj)[2, 1]
    } else if (identical(profile_name, "BetaScale")) {
      season_growth_pars(par_obj)[21]
    } else {
      numeric()
    }
  }, error = function(e) numeric())
  out <- suppressWarnings(as.numeric(out))
  out[is.finite(out)]
}

st_profile_key_values <- function(par_obj) {
  if (is.null(par_obj)) return(data.frame())
  profile_names <- c("totpop", "LorenM", "L1", "L2", "kappa", "s1", "s2", "BetaScale")
  rows <- lapply(profile_names, function(group) {
    val <- st_profile_value_vector(par_obj, group)
    if (length(val) == 0) return(NULL)
    data.frame(
      parameter = group,
      index = seq_along(val),
      value = val,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) data.frame() else out
}

st_profile_parameter_recovery <- function(truth_par, refit_par, out_file) {
  truth_obj <- tryCatch(read.MFCLPar(truth_par), error = function(e) NULL)
  refit_obj <- tryCatch(read.MFCLPar(refit_par), error = function(e) NULL)
  truth <- st_profile_key_values(truth_obj)
  refit <- st_profile_key_values(refit_obj)
  if (nrow(truth) == 0 || nrow(refit) == 0) {
    utils::write.csv(data.frame(), out_file, row.names = FALSE)
    return(invisible(NULL))
  }
  names(truth)[names(truth) == "value"] <- "truth_value"
  names(refit)[names(refit) == "value"] <- "refit_value"
  out <- merge(truth, refit, by = c("parameter", "index"), all = FALSE)
  if (nrow(out) > 0) {
    out$rel_diff <- ifelse(
      out$parameter == "LorenM",
      (out$refit_value - out$truth_value) / abs(out$truth_value),
      (out$refit_value - out$truth_value) / out$truth_value
    )
    out <- out[is.finite(out$rel_diff) & abs(out$truth_value) > 1e-8, , drop = FALSE]
  }
  utils::write.csv(out, out_file, row.names = FALSE)
  invisible(out)
}

st_make_correlation_matrix <- function(n, rho = 0, psi = 0, mode = 0) {
  rho <- max(min(suppressWarnings(as.numeric(rho)), 0.98), -0.98)
  psi <- suppressWarnings(as.numeric(psi))
  if (!is.finite(psi)) psi <- 0
  mode <- suppressWarnings(as.integer(mode))
  rp <- numeric(n)
  rp[[1]] <- 1
  if (n >= 2) {
    if (identical(mode, 1L)) {
      rp[[2]] <- rho + psi / (1 + (rho + psi)^2 / pmax(1 - rho^2, 1e-8))
    } else if (identical(mode, 2L)) {
      phi2 <- -1 + (2 - abs(rho)) * psi
      rp[[2]] <- rho / pmax(1 - phi2, 1e-8)
    } else {
      rp[[2]] <- rho
    }
    if (n >= 3) {
      for (i in 3:n) {
        if (identical(mode, 2L)) {
          phi2 <- -1 + (2 - abs(rho)) * psi
          rp[[i]] <- rho * rp[[i - 1L]] + phi2 * rp[[i - 2L]]
        } else {
          rp[[i]] <- rho * rp[[i - 1L]]
        }
      }
    }
  }
  outer(seq_len(n), seq_len(n), function(i, j) rp[abs(i - j) + 1L])
}

st_mvrnorm1 <- function(mu, Sigma) {
  Sigma <- as.matrix(Sigma)
  diag(Sigma) <- pmax(diag(Sigma), 1e-10)
  chol_sigma <- tryCatch(chol(Sigma), error = function(e) chol(Sigma + diag(1e-6, nrow(Sigma))))
  as.numeric(mu + drop(stats::rnorm(length(mu)) %*% chol_sigma))
}

st_logistic_normal_freq <- function(p, n, par_obj, type = c("length", "weight"), fishery = 1L, student = FALSE) {
  type <- match.arg(type)
  p <- pmax(p, 1e-10)
  p <- p / sum(p)
  if (length(p) <= 1L) return(n)
  lnp <- tryCatch(as.numeric(unlist(strsplit(par_obj@logistic_normal_params, "[[:space:]]+"))), error = function(e) numeric(0))
  lnp <- suppressWarnings(as.numeric(lnp[is.finite(lnp)]))
  if (identical(type, "length")) {
    log_var <- if (length(lnp) >= 1L) lnp[[1L]] else 0
    rho <- if (length(lnp) >= 2L) lnp[[2L]] else 0
    dof <- if (length(lnp) >= 3L) exp(lnp[[3L]]) else Inf
    psi <- if (length(lnp) >= 4L) lnp[[4L]] else 0
    exp_flag <- attr(par_obj, "selftest_flag_295")
    exp_val <- if (length(lnp) >= 5L) lnp[[5L]] else 0
    mode <- attr(par_obj, "selftest_flag_297")
  } else {
    off <- 5L
    log_var <- if (length(lnp) >= off + 1L) lnp[[off + 1L]] else 0
    rho <- if (length(lnp) >= off + 2L) lnp[[off + 2L]] else 0
    dof <- if (length(lnp) >= off + 3L) exp(lnp[[off + 3L]]) else Inf
    psi <- if (length(lnp) >= off + 4L) lnp[[off + 4L]] else 0
    exp_flag <- attr(par_obj, "selftest_flag_285")
    exp_val <- if (length(lnp) >= off + 5L) lnp[[off + 5L]] else 0
    mode <- attr(par_obj, "selftest_flag_297")
  }
  S <- st_make_correlation_matrix(length(p), rho, psi, mode)
  M <- cbind(diag(length(p) - 1L), -1)
  Sigma <- exp(log_var) * (M %*% S %*% t(M))
  if (isTRUE(is.finite(exp_flag) && exp_flag != 0)) {
    scale_n <- max(1, floor(n))^(2 * exp_val)
    Sigma <- Sigma / scale_n
  }
  if (isTRUE(student) && is.finite(dof) && dof > 0) {
    Sigma <- Sigma * dof / stats::rchisq(1L, df = dof)
  }
  mu <- log(p[-length(p)]) - log(p[[length(p)]])
  z <- st_mvrnorm1(mu, Sigma)
  ex <- exp(c(z, 0))
  st_multinom_freq(ex / sum(ex), n)
}

st_square_freq <- function(p, n, par_file, type = c("length", "weight"), student = FALSE) {
  type <- match.arg(type)
  p <- pmax(p, 0)
  if (!is.finite(sum(p)) || sum(p) <= 0) p[] <- 1
  p <- p / sum(p)
  eps_flag <- attr(par_file, "selftest_flag_193")
  if (is.null(eps_flag) || length(eps_flag) == 0) eps_flag <- st_flag_value(par_file, 1L, 193L)
  eps <- if (is.finite(eps_flag) && eps_flag != 0) eps_flag / (100 * length(p)) else 1 / length(p)
  target_var <- pmax((p * (1 - p) + eps) / max(floor(n), 1), 1e-12)
  base_var <- p * (1 - p)
  alpha_i <- base_var / target_var - 1
  alpha_i <- alpha_i[is.finite(alpha_i) & alpha_i > 0]
  alpha <- if (length(alpha_i) > 0) stats::median(alpha_i) else max(floor(n), 1)
  if (isTRUE(student)) {
    alpha <- alpha / sqrt(4 / stats::rchisq(1L, df = 4))
  }
  alpha <- max(alpha, 1e-3)
  max(0, floor(n)) * st_simple_rdirichlet(alpha * p)
}

st_fish_param <- function(par_obj, row, fishery, default = 0) {
  x <- tryCatch(par_obj@fish_params, error = function(e) NULL)
  if (is.null(x) || row > nrow(x) || fishery > ncol(x)) return(default)
  val <- suppressWarnings(as.numeric(x[row, fishery]))
  if (length(val) == 0 || !is.finite(val)) default else val
}

st_fishery_group_values <- function(par_obj, flag, n_fisheries) {
  flags <- tryCatch(par_obj@flags, error = function(e) NULL)
  out <- rep(seq_len(n_fisheries), length.out = n_fisheries)
  if (!is.data.frame(flags) || !all(c("flagtype", "flag", "value") %in% names(flags))) return(out)
  rows <- flags[flags$flagtype < 0 & flags$flag == as.integer(flag), , drop = FALSE]
  if (nrow(rows) == 0) return(out)
  idx <- abs(suppressWarnings(as.integer(rows$flagtype)))
  val <- suppressWarnings(as.integer(rows$value))
  ok <- is.finite(idx) & idx >= 1L & idx <= n_fisheries & is.finite(val) & val > 0L
  if (any(ok)) out[idx[ok]] <- val[ok]
  if (sum(out, na.rm = TRUE) == 0) out <- rep(seq_len(n_fisheries), length.out = n_fisheries)
  out
}

st_average_sample_size_by_group <- function(fit, group, type = c("length", "weight")) {
  type <- match.arg(type)
  value_col <- if (identical(type, "length")) "length" else "weight"
  if (!all(c("fishery", "year", "month", "sample_size", value_col) %in% names(fit))) return(numeric(0))
  key <- unique(fit[, c("fishery", "year", "month", "sample_size"), drop = FALSE])
  fishery <- suppressWarnings(as.integer(key$fishery))
  gp <- group[pmax(1L, pmin(length(group), fishery))]
  ss <- suppressWarnings(as.numeric(key$sample_size))
  stats::tapply(ss[is.finite(ss) & is.finite(gp)], gp[is.finite(ss) & is.finite(gp)], mean)
}

st_dm_nore_lambda <- function(par_obj, fit, type = c("length", "weight"), fishery, sample_size) {
  type <- match.arg(type)
  n_fisheries <- ncol(tryCatch(par_obj@fish_params, error = function(e) matrix(0, 0, 0)))
  if (n_fisheries <= 0) return(1)
  if (identical(type, "length")) {
    group <- st_fishery_group_values(par_obj, 68L, n_fisheries)
    avg <- st_average_sample_size_by_group(fit, group, type)
    fp_lambda <- st_fish_param(par_obj, 22L, fishery, 0)
    fp_exp <- st_fish_param(par_obj, 23L, fishery, 0)
  } else {
    group <- st_fishery_group_values(par_obj, 77L, n_fisheries)
    avg <- st_average_sample_size_by_group(fit, group, type)
    fp_lambda <- st_fish_param(par_obj, 24L, fishery, 0)
    fp_exp <- st_fish_param(par_obj, 25L, fishery, 0)
  }
  gp <- group[[pmax(1L, pmin(length(group), as.integer(fishery)))]]
  avg_ss <- suppressWarnings(as.numeric(avg[as.character(gp)]))
  if (!is.finite(avg_ss) || avg_ss <= 0) avg_ss <- max(1, suppressWarnings(as.numeric(sample_size)))
  rel_ss <- 0.001 + suppressWarnings(as.numeric(sample_size)) / (0.001 + avg_ss)
  exp(fp_lambda) * rel_ss^fp_exp
}

st_dm_nore_nmax <- function(par_obj) {
  nmax <- st_flag_from_obj(par_obj, 1L, 342L)
  if (is.finite(nmax) && nmax > 0) nmax else 1000
}

st_lw_sample_by_likelihood <- function(p,
                                       n,
                                       par_obj,
                                       par_file,
                                       type = c("length", "weight"),
                                       fishery = 1L,
                                       likelihood = "multinomial",
                                       fit = NULL) {
  type <- match.arg(type)
  p <- pmax(suppressWarnings(as.numeric(p)), 1e-10)
  p <- p / sum(p)
  n <- max(0L, as.integer(floor(n)))
  if (n <= 0L) return(rep(0, length(p)))
  if (likelihood %in% c("multinomial")) {
    return(st_multinom_freq(p, n))
  }
  if (likelihood %in% c("square", "square0", "square0a", "square_fita", "square_t")) {
    return(st_square_freq(p, n, par_file, type, student = identical(likelihood, "square_t")))
  }
  if (likelihood %in% c("dirichlet_multinomial", "dirichlet_multinomial_mixture")) {
    A <- exp(st_fish_param(par_obj, 12L, fishery, 0))
    if (identical(likelihood, "dirichlet_multinomial_mixture") && stats::runif(1L) < 0.05) A <- A / 5
    q <- st_simple_rdirichlet(A * (0.05 / length(p) + p))
    return(st_multinom_freq(q, n))
  }
  if (identical(likelihood, "dirichlet_multinomial_nore")) {
    lambda <- st_dm_nore_lambda(par_obj, fit, type, fishery, n)
    q <- st_simple_rdirichlet(lambda * p)
    counts <- st_multinom_freq(q, st_dm_nore_nmax(par_obj))
    if (sum(counts) <= 0) return(rep(0, length(p)))
    return(n * counts / sum(counts))
  }
  if (likelihood %in% c("logistic_normal", "logistic_normal_heteroscedastic")) {
    pdftype <- if (identical(type, "length")) attr(par_obj, "selftest_flag_293") else attr(par_obj, "selftest_flag_283")
    if (is.null(pdftype) || length(pdftype) == 0) {
      pdftype <- st_flag_value(par_file, 1L, if (identical(type, "length")) 293L else 283L)
    }
    return(st_logistic_normal_freq(p, n, par_obj, type, fishery, student = isTRUE(pdftype == 1)))
  }
  stop("Unsupported composition likelihood sampler: ", likelihood)
}

st_lw_resample_from_payload <- function(sim_dir, par_file, type = c("length", "weight"), seed = 101L) {
  type <- match.arg(type)
  par_obj <- read.MFCLPar(par_file)
  info <- st_composition_likelihood_info(par_file, par_obj = par_obj)
  likelihood <- if (identical(type, "length")) info$length_name else info$weight_name
  if (identical(likelihood, "multinomial")) return(NULL)
  if (!st_supported_composition_sampler(likelihood)) {
    stop("Unsupported ", type, " composition likelihood sampler: ", likelihood)
  }
  payload_file <- file.path(sim_dir, "model_payload.rds")
  if (!file.exists(payload_file)) return(NULL)
  payload <- readRDS(payload_file)
  attr(par_obj, "selftest_flag_193") <- st_flag_from_obj(par_obj, 1L, 193L)
  attr(par_obj, "selftest_flag_283") <- st_flag_from_obj(par_obj, 1L, 283L)
  attr(par_obj, "selftest_flag_293") <- st_flag_from_obj(par_obj, 1L, 293L)
  attr(par_obj, "selftest_flag_284") <- st_flag_from_obj(par_obj, 1L, 284L)
  attr(par_obj, "selftest_flag_285") <- st_flag_from_obj(par_obj, 1L, 285L)
  attr(par_obj, "selftest_flag_295") <- st_flag_from_obj(par_obj, 1L, 295L)
  attr(par_obj, "selftest_flag_297") <- st_flag_from_obj(par_obj, 1L, 297L)
  par_ref <- par_file
  attr(par_ref, "selftest_flag_193") <- attr(par_obj, "selftest_flag_193")
  fit <- if (identical(type, "length")) payload$data$LengOut@lenfits else payload$data$WeightOut@wgtfits
  value_col <- if (identical(type, "length")) "length" else "weight"
  if (!is.data.frame(fit) || !all(c("fishery", "year", "month", "sample_size", value_col, "pred") %in% names(fit))) return(NULL)
  set.seed(as.integer(seed))
  split_key <- paste(fit$fishery, fit$year, fit$month, fit$sample_size, sep = "\r")
  rows <- lapply(split(fit, split_key), function(dat) {
    dat <- dat[order(dat[[value_col]]), , drop = FALSE]
    k <- dat[1L, c("fishery", "year", "month", "sample_size"), drop = FALSE]
    p <- suppressWarnings(as.numeric(dat$pred))
    freq <- st_lw_sample_by_likelihood(
      p = p,
      n = k$sample_size,
      par_obj = par_obj,
      par_file = par_ref,
      type = type,
      fishery = as.integer(k$fishery),
      likelihood = likelihood,
      fit = fit
    )
    data.frame(
      type = type,
      projection = NA_integer_,
      seed = as.integer(seed),
      year = as.integer(k$year),
      month = as.integer(k$month),
      week = 1L,
      fishery = as.integer(k$fishery),
      bin = suppressWarnings(as.numeric(dat[[value_col]])),
      freq = freq,
      sample_size = suppressWarnings(as.numeric(k$sample_size)),
      source = paste0("likelihood_", likelihood),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[is.finite(out$bin) & is.finite(out$freq), , drop = FALSE]
}

st_parse_agelength_predictions <- function(path, n_age) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  header_re <- "^\\s*Fishery\\s+([0-9]+)\\s+Year\\s+([0-9]+)\\s+Month\\s+([0-9]+)"
  out <- list()
  i <- 1L
  sample_index <- 0L

  while (i <= length(lines)) {
    line <- lines[[i]]
    if (!grepl(header_re, line, ignore.case = TRUE)) {
      i <- i + 1L
      next
    }

    sample_index <- sample_index + 1L
    fishery <- as.integer(sub(header_re, "\\1", line, ignore.case = TRUE))
    year <- as.integer(sub(header_re, "\\2", line, ignore.case = TRUE))
    month <- as.integer(sub(header_re, "\\3", line, ignore.case = TRUE))
    i <- i + 1L
    length_index <- 0L

    while (i + 2L <= length(lines) && !grepl(header_re, lines[[i]], ignore.case = TRUE)) {
      total_line <- st_parse_numeric_tokens(lines[[i]])
      observed <- st_parse_numeric_tokens(lines[[i + 1L]])
      predicted <- st_parse_numeric_tokens(lines[[i + 2L]])
      if (length(total_line) == 0 && length(observed) == 0 && length(predicted) == 0) {
        i <- i + 1L
        next
      }
      if (length(observed) < n_age || length(predicted) < n_age) {
        stop(
          "Age-length residual block has fewer age columns than expected at sample ",
          sample_index, " in ", path
        )
      }
      length_index <- length_index + 1L
      out[[length(out) + 1L]] <- data.frame(
        sample_index = sample_index,
        fishery = fishery,
        year = year,
        month = month,
        length_index = length_index,
        age = seq_len(n_age),
        total = if (length(total_line) > 0) total_line[[1]] else NA_real_,
        observed_prop = observed[seq_len(n_age)],
        predicted_prop = predicted[seq_len(n_age)],
        stringsAsFactors = FALSE
      )
      i <- i + 3L
    }
  }

  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

st_apply_pseudo_to_age_length <- function(base_alk_file,
                                          out_alk_file,
                                          sim_dir,
                                          seed = 1007L,
                                          draw_size_mode = st_age_length_draw_size_mode()) {
  pred_path <- file.path(sim_dir, "agelengthresids.dat")
  if (!file.exists(pred_path)) {
    stop("Cannot simulate age-length data: missing MFCL agelengthresids.dat in ", sim_dir)
  }

  alk <- read.MFCLALK(base_alk_file)
  alk_df <- alk@ALK
  n_age <- max(alk_df$age, na.rm = TRUE)
  if (!is.finite(n_age) || n_age < 1L) stop("Cannot infer number of ages from ", base_alk_file)

  preds <- st_parse_agelength_predictions(pred_path, n_age = n_age)
  if (is.null(preds) || nrow(preds) == 0) {
    stop("Cannot parse age-length predictions from ", pred_path)
  }

  sample_key <- paste(alk_df$year, alk_df$month, alk_df$fishery, alk_df$species, sep = "_")
  sample_levels <- unique(sample_key)
  pred_key <- paste(preds$year, preds$month, preds$fishery, sep = "_")
  pred_levels <- unique(pred_key)

  if (length(sample_levels) != length(pred_levels)) {
    stop(
      "Age-length sample count mismatch: input has ", length(sample_levels),
      " samples but agelengthresids.dat has ", length(pred_levels), "."
    )
  }

  set.seed(as.integer(seed))
  original_counts <- alk_df$obs
  ess <- suppressWarnings(as.numeric(alk@ESS))
  has_ess <- length(ess) >= length(sample_levels) && any(is.finite(ess) & ess > 0)
  draw_size_mode <- match.arg(draw_size_mode, c("effective", "observed"))
  simulated_records <- 0L
  simulated_length_bins <- 0L
  zero_prediction_bins <- 0L
  draw_n_total <- 0
  draw_n_min <- Inf
  draw_n_max <- -Inf

  for (sample_id in seq_along(sample_levels)) {
    idx <- which(sample_key == sample_levels[[sample_id]])
    sample_meta <- alk_df[idx[1L], , drop = FALSE]
    key3 <- paste(sample_meta$year, sample_meta$month, sample_meta$fishery, sep = "_")
    pred_sample <- preds[pred_key == key3, , drop = FALSE]
    if (nrow(pred_sample) == 0) {
      stop("No age-length predictions found for sample ", sample_levels[[sample_id]])
    }

    n_len <- length(idx) / n_age
    if (n_len != floor(n_len)) {
      stop("Age-length sample is not a rectangular length x age matrix: ", sample_levels[[sample_id]])
    }
    if (max(pred_sample$length_index, na.rm = TRUE) < n_len) {
      stop("Age-length prediction block has too few length bins for ", sample_levels[[sample_id]])
    }
    pred_profiles <- lapply(seq_len(n_len), function(len_id) {
      pred_len <- pred_sample[pred_sample$length_index == len_id, , drop = FALSE]
      pred_len <- pred_len[order(pred_len$age), , drop = FALSE]
      pmax(as.numeric(pred_len$predicted_prop[seq_len(n_age)]), 0)
    })
    pred_sums <- vapply(pred_profiles, sum, numeric(1), na.rm = TRUE)

    for (len_id in seq_len(n_len)) {
      len_idx <- idx[((len_id - 1L) * n_age + 1L):(len_id * n_age)]
      n_obs <- round(sum(original_counts[len_idx], na.rm = TRUE))
      prob <- pred_profiles[[len_id]]

      if (n_obs <= 0L) {
        alk_df$obs[len_idx] <- 0
        next
      }
      if (!is.finite(sum(prob)) || sum(prob) <= 0) {
        donor <- which(is.finite(pred_sums) & pred_sums > 0)
        if (length(donor) == 0) {
          stop("All age-length predicted probabilities are zero for ", sample_levels[[sample_id]])
        }
        donor <- donor[which.min(abs(donor - len_id))]
        prob <- pred_profiles[[donor]]
        zero_prediction_bins <- zero_prediction_bins + 1L
      }
      ess_i <- if (has_ess) ess[[sample_id]] else NA_real_
      draw_n_real <- if (
        identical(draw_size_mode, "effective") &&
          is.finite(ess_i) && ess_i > 0
      ) n_obs * ess_i else n_obs
      draw_n <- max(1L, as.integer(round(draw_n_real)))
      draw <- as.numeric(rmultinom(1L, size = draw_n, prob = prob))
      if (identical(draw_size_mode, "effective")) {
        alk_df$obs[len_idx] <- draw * (n_obs / draw_n)
      } else {
        alk_df$obs[len_idx] <- draw
      }
      draw_n_total <- draw_n_total + draw_n
      draw_n_min <- min(draw_n_min, draw_n)
      draw_n_max <- max(draw_n_max, draw_n)
      simulated_length_bins <- simulated_length_bins + 1L
    }
    simulated_records <- simulated_records + 1L
  }

  old_by_bin <- rowsum(original_counts, rep(seq_len(length(original_counts) / n_age), each = n_age), reorder = FALSE)
  new_by_bin <- rowsum(alk_df$obs, rep(seq_len(length(alk_df$obs) / n_age), each = n_age), reorder = FALSE)
  sample_sizes_matched <- isTRUE(all.equal(as.numeric(old_by_bin[, 1]), as.numeric(new_by_bin[, 1]), tolerance = 0))

  alk@ALK <- alk_df
  FLR4MFCL::write(alk, file = out_alk_file)

  list(
    age_length_source = basename(pred_path),
    age_length_records = simulated_records,
    age_length_nonzero_length_bins = simulated_length_bins,
    age_length_zero_prediction_bins = zero_prediction_bins,
    age_length_total_obs = sum(original_counts, na.rm = TRUE),
    age_length_total_sim = sum(alk_df$obs, na.rm = TRUE),
    age_length_draw_size_mode = draw_size_mode,
    age_length_ess_min = if (has_ess) min(ess[is.finite(ess)], na.rm = TRUE) else NA_real_,
    age_length_ess_max = if (has_ess) max(ess[is.finite(ess)], na.rm = TRUE) else NA_real_,
    age_length_draw_n_total = draw_n_total,
    age_length_draw_n_min = if (is.finite(draw_n_min)) draw_n_min else NA_integer_,
    age_length_draw_n_max = if (is.finite(draw_n_max)) draw_n_max else NA_integer_,
    age_length_sample_sizes_matched = sample_sizes_matched,
    out_age_length_file = out_alk_file
  )
}

st_tag_summary <- function(path) {
  tag <- read.MFCLTag(path)
  list(
    release_groups = as.integer(tag@release_groups),
    release_rows = nrow(tag@releases),
    recapture_rows = nrow(tag@recaptures),
    recaptures_total = sum(tag@recaptures$recap.number, na.rm = TRUE)
  )
}

st_apply_native_realtag <- function(base_tag_file, out_tag_file, sim_dir) {
  realtag <- list.files(sim_dir, pattern = "^report\\.realtag_[0-9]+$", full.names = TRUE)
  if (length(realtag) == 0) {
    stoch_files <- c("simulated_numbers_at_age", "simulated_numbers_at_age_noeff", "simyears")
    stoch_present <- stoch_files[file.exists(file.path(sim_dir, stoch_files))]
    log_file <- file.path(sim_dir, "mfcl_selftest_sim_log.txt")
    log_hint <- ""
    if (file.exists(log_file)) {
      log_txt <- readLines(log_file, warn = FALSE)
      if (any(grepl("simulated_numbers_at_age", log_txt, fixed = TRUE))) {
        log_hint <- " MFCL log mentions simulated_numbers_at_age, which usually means age_flag(20)>0 entered the stochastic simulation loop but the stochastic projection input files were missing."
      }
    }
    stop(
      "Cannot simulate tag data: MFCL did not write report.realtag_# in ", sim_dir,
      ". Historical tag self-tests must use MFCL sim_realtag output: parest_flags(241)=1 ",
      "activates pseudo-observations, parest_flags(242)=1 activates real estimation-period tags, ",
      "and the random seed is read from the simseed input file. The -tag_seed option applies to ",
      "virtual projection tags (sim_tag), not the historical real-tag replacement. Manual evidence ",
      "indicates report.realtag_# is written inside the numbered stochastic simulation loop; with ",
      "age_flag(20)=0 MFCL can still write projection-0 size/catch/CPUE pseudo files but does not read ",
      "simseed or write report.realtag_#. With age_flag(20)>0, MFCL also needs stochastic projection ",
      "inputs generated by the preceding simulation setup steps: ", paste(stoch_files, collapse = ", "),
      ". The projection horizon must also be long enough to age historical tag releases through the ",
      "sim_realtag bookkeeping; this runner defaults to max(30, age classes) projection years for that reason. ",
      "Present here: ", if (length(stoch_present)) paste(stoch_present, collapse = ", ") else "none",
      ".", log_hint, " This self-test requires MFCL-native tag pseudo-observations so tag likelihood ",
      "structure is not faked."
    )
  }

  file.copy(realtag[[1]], out_tag_file, overwrite = TRUE)
  base_summary <- st_tag_summary(base_tag_file)
  sim_summary <- st_tag_summary(out_tag_file)
  if (!identical(base_summary$release_groups, sim_summary$release_groups)) {
    stop(
      "Simulated tag release-group count does not match base .tag: base=",
      base_summary$release_groups, " simulated=", sim_summary$release_groups
    )
  }
  list(
    tag_source = basename(realtag[[1]]),
    tag_release_groups = sim_summary$release_groups,
    tag_release_rows = sim_summary$release_rows,
    tag_recapture_rows = sim_summary$recapture_rows,
    tag_recaptures_total = sim_summary$recaptures_total
  )
}

st_apply_pseudo_to_frq <- function(base_frq_file,
                                   out_frq_file,
                                   sim_dir,
                                   par_file = NULL,
                                   update_catch = TRUE,
                                   update_effort = FALSE,
                                   update_lw = TRUE,
                                   update_cpue = TRUE,
                                   seeds = list(cpue = 105L)) {
  frq_obj <- read.MFCLFrq(base_frq_file)
  frq_df <- freq(frq_obj)
  base_key <- st_key(frq_df$year, frq_df$month, frq_df$week, frq_df$fishery)
  diagnostics <- list()
  cpue_df <- NULL
  if (isTRUE(update_cpue)) {
    cpue_true <- st_parse_cpue_sim(file.path(sim_dir, "cpue_sim_true"), frq_obj)
    if (!is.null(cpue_true) && nrow(cpue_true) > 0) {
      cpue_seed <- if (is.null(seeds$cpue) || length(seeds$cpue) == 0 || is.na(seeds$cpue[[1]])) 105L else seeds$cpue[[1]]
      cpue_df <- st_apply_cpue_likelihood_error(cpue_true, par_file, seed = cpue_seed)
    } else {
      cpue_df <- st_parse_cpue_sim(file.path(sim_dir, "cpue_sim"), frq_obj)
      if (!is.null(cpue_df) && nrow(cpue_df) > 0) {
        cpue_df$error_source <- "mfcl_cpue_sim"
        cpue_df$cpue_cv <- NA_real_
        cpue_df$cpue_sdlog <- NA_real_
      }
    }
  }
  cpue_keys <- if (is.null(cpue_df)) character(0) else unique(cpue_df$key)

  if (isTRUE(update_catch)) {
    catch_df <- st_parse_catch_sim(file.path(sim_dir, "catch_sim"), frq_obj)
    diagnostics$catch_rows <- if (is.null(catch_df)) 0L else nrow(catch_df)
    if (!is.null(catch_df) && nrow(catch_df) > 0) {
      m <- match(base_key, catch_df$key)
      hit <- which(!is.na(m) & is.finite(catch_df$catch[m]) & !(base_key %in% cpue_keys))
      frq_df$catch[hit] <- catch_df$catch[m[hit]]
      diagnostics$catch_replaced_rows <- length(hit)
      diagnostics$catch_skipped_cpue_rows <- sum(!is.na(m) & base_key %in% cpue_keys)
    }
  }

  if (isTRUE(update_effort)) {
    effort_df <- st_parse_effort_sim(file.path(sim_dir, "effort_sim"), frq_obj)
    diagnostics$effort_rows <- if (is.null(effort_df)) 0L else nrow(effort_df)
    if (!is.null(effort_df) && nrow(effort_df) > 0) {
      m <- match(base_key, effort_df$key)
      hit <- which(!is.na(m) & is.finite(effort_df$effort[m]) & effort_df$effort[m] > 0 & !(base_key %in% cpue_keys))
      frq_df$effort[hit] <- effort_df$effort[m[hit]]
      diagnostics$effort_replaced_rows <- length(hit)
      diagnostics$effort_skipped_cpue_rows <- sum(!is.na(m) & base_key %in% cpue_keys)
    }
  }

  if (isTRUE(update_cpue)) {
    diagnostics$cpue_rows <- if (is.null(cpue_df)) 0L else nrow(cpue_df)
    if (!is.null(cpue_df) && nrow(cpue_df) > 0) {
      m <- match(base_key, cpue_df$key)
      hit <- which(!is.na(m) & is.finite(cpue_df$cpue[m]) & cpue_df$cpue[m] > 0)
      frq_df$effort[hit] <- frq_df$catch[hit] / cpue_df$cpue[m[hit]]
      diagnostics$cpue_replaced_rows <- length(hit)
      diagnostics$cpue_zero_or_missing <- sum(!is.na(m)) - length(hit)
      diagnostics$cpue_error_source <- unique(cpue_df$error_source)[[1]]
      finite_cv <- cpue_df$cpue_cv[is.finite(cpue_df$cpue_cv)]
      finite_sdlog <- cpue_df$cpue_sdlog[is.finite(cpue_df$cpue_sdlog)]
      diagnostics$cpue_cv_min <- if (length(finite_cv) > 0) min(finite_cv) else NA_real_
      diagnostics$cpue_cv_max <- if (length(finite_cv) > 0) max(finite_cv) else NA_real_
      diagnostics$cpue_sdlog_min <- if (length(finite_sdlog) > 0) min(finite_sdlog) else NA_real_
      diagnostics$cpue_sdlog_max <- if (length(finite_sdlog) > 0) max(finite_sdlog) else NA_real_
    }
  }

  if (isTRUE(update_lw)) {
    comp_info <- if (!is.null(par_file) && file.exists(par_file)) st_assert_supported_composition_likelihood(par_file) else NULL
    native_lw_df <- st_parse_lw_sim(file.path(sim_dir, "test_lw_sim"), frq_obj)
    len_seed <- if (is.null(seeds$length) || length(seeds$length) == 0 || is.na(seeds$length[[1]])) 101L else seeds$length[[1]]
    wgt_seed <- if (is.null(seeds$weight) || length(seeds$weight) == 0 || is.na(seeds$weight[[1]])) 102L else seeds$weight[[1]]
    len_lw_df <- if (!is.null(comp_info) && !identical(comp_info$length_name, "multinomial")) {
      st_lw_resample_from_payload(sim_dir, par_file, "length", seed = len_seed)
    } else if (!is.null(native_lw_df)) {
      native_lw_df[native_lw_df$type == "length", , drop = FALSE]
    } else {
      NULL
    }
    wgt_lw_df <- if (!is.null(comp_info) && !identical(comp_info$weight_name, "multinomial")) {
      st_lw_resample_from_payload(sim_dir, par_file, "weight", seed = wgt_seed)
    } else if (!is.null(native_lw_df)) {
      native_lw_df[native_lw_df$type == "weight", , drop = FALSE]
    } else {
      NULL
    }
    lw_df <- do.call(rbind, Filter(Negate(is.null), list(len_lw_df, wgt_lw_df)))
    diagnostics$lw_rows <- if (is.null(lw_df)) 0L else nrow(lw_df)
    if (!is.null(comp_info)) {
      diagnostics$length_likelihood <- comp_info$length_name
      diagnostics$weight_likelihood <- comp_info$weight_name
      diagnostics$length_sampler <- if (identical(comp_info$length_name, "multinomial")) "mfcl_native_multinomial" else paste0("selftest_", comp_info$length_name, "_moment_dirichlet")
      diagnostics$weight_sampler <- if (identical(comp_info$weight_name, "multinomial")) "mfcl_native_multinomial" else paste0("selftest_", comp_info$weight_name, "_moment_dirichlet")
    }
    if (!is.null(lw_df) && nrow(lw_df) > 0) {
      len_df <- lw_df[lw_df$type == "length", , drop = FALSE]
      if (nrow(len_df) > 0) {
        len_key_frq <- paste(base_key, st_num_key(frq_df$length), sep = "_")
        len_key_sim <- paste(st_key(len_df$year, len_df$month, len_df$week, len_df$fishery), st_num_key(len_df$bin), sep = "_")
        idx <- which(is.finite(frq_df$length))
        m <- match(len_key_frq[idx], len_key_sim)
        hit <- idx[!is.na(m) & is.finite(len_df$freq[m])]
        frq_df$freq[hit] <- len_df$freq[m[!is.na(m) & is.finite(len_df$freq[m])]]
        diagnostics$length_replaced_rows <- length(hit)
        len_tot <- aggregate(freq ~ year + month + week + fishery + sample_size, len_df, sum)
        diagnostics$length_sample_size_mismatch <- sum(abs(len_tot$freq - floor(len_tot$sample_size)) > 1.000001, na.rm = TRUE)
      }

      wgt_df <- lw_df[lw_df$type == "weight", , drop = FALSE]
      if (nrow(wgt_df) > 0) {
        wgt_key_frq <- paste(base_key, st_num_key(frq_df$weight), sep = "_")
        wgt_key_sim <- paste(st_key(wgt_df$year, wgt_df$month, wgt_df$week, wgt_df$fishery), st_num_key(wgt_df$bin), sep = "_")
        idx <- which(is.finite(frq_df$weight))
        m <- match(wgt_key_frq[idx], wgt_key_sim)
        ok <- !is.na(m) & is.finite(wgt_df$freq[m])
        hit <- idx[ok]
        frq_df$freq[hit] <- wgt_df$freq[m[ok]]
        diagnostics$weight_replaced_rows <- length(hit)
        wgt_tot <- aggregate(freq ~ year + month + week + fishery + sample_size, wgt_df, sum)
        diagnostics$weight_sample_size_mismatch <- sum(abs(wgt_tot$freq - floor(wgt_tot$sample_size)) > 1.000001, na.rm = TRUE)
      }
    }
  }

  freq(frq_obj) <- frq_df
  FLR4MFCL::write(frq_obj, file = out_frq_file)
  diagnostics$out_frq_file <- out_frq_file
  diagnostics
}

st_weighted_mean <- function(value, weight) {
  value <- suppressWarnings(as.numeric(value))
  weight <- suppressWarnings(as.numeric(weight))
  ok <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  sum(value[ok] * weight[ok], na.rm = TRUE) / sum(weight[ok], na.rm = TRUE)
}

st_weighted_quantile <- function(value, weight, prob = 0.5) {
  value <- suppressWarnings(as.numeric(value))
  weight <- suppressWarnings(as.numeric(weight))
  ok <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  value <- value[ok]
  weight <- weight[ok]
  ord <- order(value)
  value <- value[ord]
  weight <- weight[ord]
  cw <- cumsum(weight) / sum(weight)
  value[which(cw >= prob)[[1]]]
}

st_data_summary_rows <- function(component, base_df, pseudo_df, value_fun) {
  if (is.null(base_df) || is.null(pseudo_df) || nrow(base_df) == 0 || nrow(pseudo_df) == 0) {
    return(data.frame())
  }
  if (!"series" %in% names(base_df)) base_df$series <- "all"
  if (!"series" %in% names(pseudo_df)) pseudo_df$series <- "all"
  keys <- unique(rbind(
    data.frame(year = base_df$year, series = as.character(base_df$series), stringsAsFactors = FALSE),
    data.frame(year = pseudo_df$year, series = as.character(pseudo_df$series), stringsAsFactors = FALSE)
  ))
  keys <- keys[is.finite(keys$year) & nzchar(keys$series), , drop = FALSE]
  keys <- keys[order(keys$series, keys$year), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    yr <- keys$year[[i]]
    ser <- keys$series[[i]]
    b <- base_df[base_df$year == yr & as.character(base_df$series) == ser, , drop = FALSE]
    p <- pseudo_df[pseudo_df$year == yr & as.character(pseudo_df$series) == ser, , drop = FALSE]
    base_value <- value_fun(b)
    pseudo_value <- value_fun(p)
    data.frame(
      component = component,
      series = ser,
      year = as.integer(yr),
      n = nrow(p),
      base_value = base_value,
      pseudo_value = pseudo_value,
      ratio = pseudo_value / pmax(base_value, .Machine$double.eps),
      delta = pseudo_value - base_value,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[is.finite(out$base_value) & is.finite(out$pseudo_value), , drop = FALSE]
}

st_lw_expected_rows_from_payload <- function(sim_dir, type = c("length", "weight")) {
  type <- match.arg(type)
  if (is.null(sim_dir) || length(sim_dir) == 0 || !nzchar(as.character(sim_dir[[1]]))) return(data.frame())
  payload_file <- file.path(sim_dir, "model_payload.rds")
  if (!file.exists(payload_file)) return(data.frame())
  payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
  dat <- tryCatch(payload$data, error = function(e) NULL)
  fit <- if (identical(type, "length")) {
    tryCatch(dat$LengOut@lenfits, error = function(e) NULL)
  } else {
    tryCatch(dat$WeightOut@wgtfits, error = function(e) NULL)
  }
  value_col <- if (identical(type, "length")) "length" else "weight"
  if (!is.data.frame(fit) || !all(c("fishery", "year", value_col, "pred", "sample_size") %in% names(fit))) {
    return(data.frame())
  }
  value <- suppressWarnings(as.numeric(fit[[value_col]]))
  pred <- suppressWarnings(as.numeric(fit$pred))
  sample_size <- suppressWarnings(as.numeric(fit$sample_size))
  ok <- is.finite(value) & is.finite(pred) & pred >= 0 & is.finite(sample_size) & sample_size > 0
  if (!any(ok)) return(data.frame())
  data.frame(
    component = if (identical(type, "length")) "length mean" else "weight mean",
    series = paste0("fishery_", as.integer(fit$fishery[ok])),
    year = as.integer(fit$year[ok]),
    value = value[ok],
    weight = pred[ok] * sample_size[ok],
    source = "fitted_pred",
    stringsAsFactors = FALSE
  )
}

st_lw_pseudo_rows <- function(lw_df, type = c("length", "weight")) {
  type <- match.arg(type)
  if (is.null(lw_df) || !is.data.frame(lw_df) || nrow(lw_df) == 0) return(data.frame())
  dat <- lw_df[lw_df$type == type, , drop = FALSE]
  if (nrow(dat) == 0) return(data.frame())
  value <- suppressWarnings(as.numeric(dat$bin))
  weight <- suppressWarnings(as.numeric(dat$freq))
  ok <- is.finite(value) & is.finite(weight) & weight >= 0
  if (!any(ok)) return(data.frame())
  data.frame(
    component = if (identical(type, "length")) "length mean" else "weight mean",
    series = paste0("fishery_", as.integer(dat$fishery[ok])),
    year = as.integer(dat$year[ok]),
    value = value[ok],
    weight = weight[ok],
    source = "pseudo_multinomial",
    stringsAsFactors = FALSE
  )
}

st_tag_annual_summary <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(data.frame())
  tag <- read.MFCLTag(path)
  rec <- tag@recaptures
  if (is.null(rec) || nrow(rec) == 0 || !"recap.year" %in% names(rec)) return(data.frame())
  rec$year <- suppressWarnings(as.integer(rec$recap.year))
  rec$value <- suppressWarnings(as.numeric(rec$recap.number))
  rec <- rec[is.finite(rec$year) & is.finite(rec$value), , drop = FALSE]
  if (nrow(rec) == 0) return(data.frame())
  annual <- stats::aggregate(value ~ year, data = rec, FUN = sum)
  annual$series <- "all"
  fishery <- data.frame()
  if ("recap.fishery" %in% names(rec)) {
    rec$series <- paste0("fishery_", as.integer(rec$recap.fishery))
    fishery <- stats::aggregate(value ~ year + series, data = rec, FUN = sum)
  }
  rbind(annual[, c("year", "series", "value"), drop = FALSE], fishery[, c("year", "series", "value"), drop = FALSE])
}

st_tag_expected_annual_summary <- function(sim_dir) {
  payload_file <- file.path(sim_dir, "model_payload.rds")
  if (!file.exists(payload_file)) return(data.frame())
  payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
  dat <- tryCatch(payload$data$TagTempOut, error = function(e) NULL)
  if (!is.data.frame(dat) || !all(c("recap.year", "recap.pred") %in% names(dat))) return(data.frame())
  dat$year <- suppressWarnings(as.integer(dat$recap.year))
  dat$value <- suppressWarnings(as.numeric(dat$recap.pred))
  dat <- dat[is.finite(dat$year) & is.finite(dat$value), , drop = FALSE]
  if (nrow(dat) == 0) return(data.frame())
  annual <- stats::aggregate(value ~ year, data = dat, FUN = sum)
  annual$series <- "all"
  fishery <- data.frame()
  if ("recap.fishery" %in% names(dat)) {
    dat$series <- paste0("fishery_", as.integer(dat$recap.fishery))
    fishery <- stats::aggregate(value ~ year + series, data = dat, FUN = sum)
  }
  rbind(annual[, c("year", "series", "value"), drop = FALSE], fishery[, c("year", "series", "value"), drop = FALSE])
}

st_alk_annual_summary <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(data.frame())
  alk <- suppressWarnings(read.MFCLALK(path))
  dat <- alk@ALK
  if (is.null(dat) || nrow(dat) == 0) return(data.frame())
  dat$year <- suppressWarnings(as.integer(dat$year))
  dat$age <- suppressWarnings(as.numeric(dat$age))
  dat$value <- suppressWarnings(as.numeric(dat$obs))
  dat <- dat[is.finite(dat$year) & is.finite(dat$age) & is.finite(dat$value), , drop = FALSE]
  if (nrow(dat) == 0) return(data.frame())
  split_rows <- split(dat, dat$year)
  out <- do.call(rbind, lapply(names(split_rows), function(yr) {
    x <- split_rows[[yr]]
    data.frame(year = as.integer(yr), value = st_weighted_mean(x$age, x$value))
  }))
  out[is.finite(out$value), , drop = FALSE]
}

st_alk_annual_rows <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(data.frame())
  alk <- suppressWarnings(read.MFCLALK(path))
  dat <- alk@ALK
  if (is.null(dat) || nrow(dat) == 0) return(data.frame())
  dat$year <- suppressWarnings(as.integer(dat$year))
  dat$value <- suppressWarnings(as.numeric(dat$age))
  dat$weight <- suppressWarnings(as.numeric(dat$obs))
  dat <- dat[is.finite(dat$year) & is.finite(dat$value) & is.finite(dat$weight) & dat$weight >= 0, , drop = FALSE]
  if (nrow(dat) == 0) return(data.frame())
  data.frame(
    series = "all",
    year = dat$year,
    value = dat$value,
    weight = dat$weight,
    stringsAsFactors = FALSE
  )
}

st_alk_expected_annual_summary <- function(sim_dir, base_alk_file) {
  if (is.null(base_alk_file) || is.na(base_alk_file) || !file.exists(base_alk_file)) return(data.frame())
  pred_path <- file.path(sim_dir, "agelengthresids.dat")
  if (!file.exists(pred_path)) return(data.frame())
  alk <- suppressWarnings(read.MFCLALK(base_alk_file))
  alk_df <- alk@ALK
  n_age <- max(alk_df$age, na.rm = TRUE)
  if (!is.finite(n_age) || n_age < 1L) return(data.frame())
  preds <- st_parse_agelength_predictions(pred_path, n_age = n_age)
  if (is.null(preds) || nrow(preds) == 0) return(data.frame())
  preds$value <- suppressWarnings(as.numeric(preds$age))
  preds$weight <- suppressWarnings(as.numeric(preds$predicted_prop) * as.numeric(preds$total))
  preds <- preds[is.finite(preds$year) & is.finite(preds$value) & is.finite(preds$weight) & preds$weight >= 0, , drop = FALSE]
  if (nrow(preds) == 0) return(data.frame())
  split_rows <- split(preds, preds$year)
  out <- do.call(rbind, lapply(names(split_rows), function(yr) {
    x <- split_rows[[yr]]
    data.frame(year = as.integer(yr), value = st_weighted_mean(x$value, x$weight))
  }))
  out[is.finite(out$value), , drop = FALSE]
}

st_alk_expected_annual_rows <- function(sim_dir, base_alk_file) {
  if (is.null(base_alk_file) || is.na(base_alk_file) || !file.exists(base_alk_file)) return(data.frame())
  pred_path <- file.path(sim_dir, "agelengthresids.dat")
  if (!file.exists(pred_path)) return(data.frame())
  alk <- suppressWarnings(read.MFCLALK(base_alk_file))
  alk_df <- alk@ALK
  n_age <- max(alk_df$age, na.rm = TRUE)
  if (!is.finite(n_age) || n_age < 1L) return(data.frame())
  preds <- st_parse_agelength_predictions(pred_path, n_age = n_age)
  if (is.null(preds) || nrow(preds) == 0) return(data.frame())
  preds$value <- suppressWarnings(as.numeric(preds$age))
  preds$weight <- suppressWarnings(as.numeric(preds$predicted_prop) * as.numeric(preds$total))
  preds <- preds[is.finite(preds$year) & is.finite(preds$value) & is.finite(preds$weight) & preds$weight >= 0, , drop = FALSE]
  if (nrow(preds) == 0) return(data.frame())
  data.frame(
    series = "all",
    year = as.integer(preds$year),
    value = preds$value,
    weight = preds$weight,
    stringsAsFactors = FALSE
  )
}

st_summarise_data_simulation <- function(base_frq_file,
                                         pseudo_frq_file,
                                         sim_dir = NULL,
                                         par_file = NULL,
                                         seeds = list(cpue = 105L),
                                         base_tag_file = NA_character_,
                                         pseudo_tag_file = NA_character_,
                                         base_alk_file = NA_character_,
                                         pseudo_alk_file = NA_character_) {
  base_frq <- read.MFCLFrq(base_frq_file)
  pseudo_frq <- read.MFCLFrq(pseudo_frq_file)
  base_real <- realisations(base_frq)
  pseudo_real <- realisations(pseudo_frq)
  base_df <- freq(base_frq)
  pseudo_df <- freq(pseudo_frq)
  n_real <- min(nrow(base_real), nrow(pseudo_real))
  n_freq <- min(nrow(base_df), nrow(pseudo_df))
  if (n_real == 0 && n_freq == 0) return(data.frame())

  if (n_real > 0) {
    base_real <- base_real[seq_len(n_real), , drop = FALSE]
    pseudo_real <- pseudo_real[seq_len(n_real), , drop = FALSE]
  }
  if (n_freq > 0) {
    base_df <- base_df[seq_len(n_freq), , drop = FALSE]
    pseudo_df <- pseudo_df[seq_len(n_freq), , drop = FALSE]
  }
  real_key <- st_key(base_real$year, base_real$month, base_real$week, base_real$fishery)
  base_catch <- suppressWarnings(as.numeric(base_real$catch))
  sim_catch <- suppressWarnings(as.numeric(pseudo_real$catch))
  catch_sim_file <- if (!is.null(sim_dir) && nzchar(as.character(sim_dir))) file.path(sim_dir, "catch_sim") else NA_character_
  catch_expected_df <- if (!is.na(catch_sim_file) && file.exists(catch_sim_file)) st_parse_catch_sim(catch_sim_file, base_frq, value = "expected") else NULL
  catch_sim_df <- if (!is.na(catch_sim_file) && file.exists(catch_sim_file)) st_parse_catch_sim(catch_sim_file, base_frq) else NULL
  if (!is.null(catch_expected_df) && nrow(catch_expected_df) > 0) {
    m_catch_expected <- match(real_key, catch_expected_df$key)
    hit_catch_expected <- which(!is.na(m_catch_expected) & is.finite(catch_expected_df$catch[m_catch_expected]))
    base_catch[hit_catch_expected] <- catch_expected_df$catch[m_catch_expected[hit_catch_expected]]
  }
  if (!is.null(catch_sim_df) && nrow(catch_sim_df) > 0) {
    m_catch_sim <- match(real_key, catch_sim_df$key)
    hit_catch_sim <- which(!is.na(m_catch_sim) & is.finite(catch_sim_df$catch[m_catch_sim]))
    sim_catch[hit_catch_sim] <- catch_sim_df$catch[m_catch_sim[hit_catch_sim]]
  }
  base_cpue <- suppressWarnings(as.numeric(base_real$catch / base_real$effort))
  sim_cpue <- suppressWarnings(as.numeric(pseudo_real$catch / pseudo_real$effort))
  cpue_sim_true_file <- if (!is.null(sim_dir) && nzchar(as.character(sim_dir))) file.path(sim_dir, "cpue_sim_true") else NA_character_
  cpue_sim_file <- if (!is.null(sim_dir) && nzchar(as.character(sim_dir))) file.path(sim_dir, "cpue_sim") else NA_character_
  cpue_sim_df <- NULL
  cpue_true_df <- NULL
  if (!is.na(cpue_sim_true_file) && file.exists(cpue_sim_true_file)) {
    cpue_seed <- if (is.null(seeds$cpue) || length(seeds$cpue) == 0 || is.na(seeds$cpue[[1]])) 105L else seeds$cpue[[1]]
    cpue_true_df <- st_parse_cpue_sim(cpue_sim_true_file, base_frq)
    cpue_sim_df <- st_apply_cpue_likelihood_error(cpue_true_df, par_file, seed = cpue_seed)
  }
  if (!is.null(cpue_true_df) && nrow(cpue_true_df) > 0) {
    m_cpue_true <- match(real_key, cpue_true_df$key)
    hit_cpue_true <- which(!is.na(m_cpue_true) & is.finite(cpue_true_df$cpue[m_cpue_true]) & cpue_true_df$cpue[m_cpue_true] > 0)
    base_cpue[hit_cpue_true] <- cpue_true_df$cpue[m_cpue_true[hit_cpue_true]]
  }
  if ((is.null(cpue_sim_df) || nrow(cpue_sim_df) == 0) && !is.na(cpue_sim_file) && file.exists(cpue_sim_file)) {
    cpue_sim_df <- st_parse_cpue_sim(cpue_sim_file, base_frq)
  }
  if (!is.null(cpue_sim_df) && nrow(cpue_sim_df) > 0) {
    m_cpue_sim <- match(real_key, cpue_sim_df$key)
    hit_cpue_sim <- which(!is.na(m_cpue_sim) & is.finite(cpue_sim_df$cpue[m_cpue_sim]) & cpue_sim_df$cpue[m_cpue_sim] > 0)
    sim_cpue[hit_cpue_sim] <- cpue_sim_df$cpue[m_cpue_sim[hit_cpue_sim]]
  }
  cpue_key_set <- unique(c(
    if (!is.null(cpue_true_df) && nrow(cpue_true_df) > 0) cpue_true_df$key else character(0),
    if (!is.null(cpue_sim_df) && nrow(cpue_sim_df) > 0) cpue_sim_df$key else character(0)
  ))
  if (length(cpue_key_set) == 0 && !is.null(par_file) && file.exists(par_file)) {
    ff92 <- st_fishery_flag_values(par_file, 92L)
    cpue_fisheries <- suppressWarnings(as.integer(names(ff92)[is.finite(ff92) & ff92 != 0]))
    cpue_key_set <- real_key[base_real$fishery %in% cpue_fisheries]
  }
  catch_component <- is.finite(base_catch) & base_catch >= 0 &
    is.finite(sim_catch) & sim_catch >= 0
  cpue_component <- is.finite(base_real$catch) & base_real$catch > 0 &
    is.finite(base_real$effort) & base_real$effort > 0 &
    is.finite(sim_cpue) & sim_cpue > 0 &
    real_key %in% cpue_key_set

  catch_rows <- data.frame(
    component = "catch total",
    series = paste0("fishery_", as.integer(base_real$fishery[catch_component])),
    year = as.integer(base_real$year[catch_component]),
    base = suppressWarnings(as.numeric(base_catch[catch_component])),
    pseudo = suppressWarnings(as.numeric(sim_catch[catch_component])),
    stringsAsFactors = FALSE
  )
  cpue_rows <- data.frame(
    component = "CPUE",
    series = paste0("fishery_", as.integer(base_real$fishery[cpue_component])),
    year = as.integer(base_real$year[cpue_component]),
    base = suppressWarnings(as.numeric(base_cpue[cpue_component])),
    pseudo = suppressWarnings(as.numeric(sim_cpue[cpue_component])),
    stringsAsFactors = FALSE
  )
  effort_component <- is.finite(base_real$effort) & base_real$effort > 0 &
    is.finite(pseudo_real$effort) & pseudo_real$effort > 0
  effort_rows <- data.frame(
    component = "effort",
    series = paste0("fishery_", as.integer(base_real$fishery[effort_component])),
    year = as.integer(base_real$year[effort_component]),
    base = suppressWarnings(as.numeric(base_real$effort[effort_component])),
    pseudo = suppressWarnings(as.numeric(pseudo_real$effort[effort_component])),
    stringsAsFactors = FALSE
  )
  lw_df <- if (!is.null(sim_dir) && nzchar(as.character(sim_dir))) {
    native_lw_df <- st_parse_lw_sim(file.path(sim_dir, "test_lw_sim"), base_frq)
    comp_info <- if (!is.null(par_file) && file.exists(par_file)) st_composition_likelihood_info(par_file) else NULL
    len_seed <- if (is.null(seeds$length) || length(seeds$length) == 0 || is.na(seeds$length[[1]])) 101L else seeds$length[[1]]
    wgt_seed <- if (is.null(seeds$weight) || length(seeds$weight) == 0 || is.na(seeds$weight[[1]])) 102L else seeds$weight[[1]]
    len_lw_df <- if (!is.null(comp_info) && !identical(comp_info$length_name, "multinomial")) {
      st_lw_resample_from_payload(sim_dir, par_file, "length", seed = len_seed)
    } else if (!is.null(native_lw_df)) {
      native_lw_df[native_lw_df$type == "length", , drop = FALSE]
    } else {
      NULL
    }
    wgt_lw_df <- if (!is.null(comp_info) && !identical(comp_info$weight_name, "multinomial")) {
      st_lw_resample_from_payload(sim_dir, par_file, "weight", seed = wgt_seed)
    } else if (!is.null(native_lw_df)) {
      native_lw_df[native_lw_df$type == "weight", , drop = FALSE]
    } else {
      NULL
    }
    do.call(rbind, Filter(Negate(is.null), list(len_lw_df, wgt_lw_df)))
  } else {
    NULL
  }
  length_base_rows <- st_lw_expected_rows_from_payload(sim_dir, "length")
  length_pseudo_rows <- st_lw_pseudo_rows(lw_df, "length")
  weight_base_rows <- st_lw_expected_rows_from_payload(sim_dir, "weight")
  weight_pseudo_rows <- st_lw_pseudo_rows(lw_df, "weight")

  if (nrow(length_base_rows) == 0 || nrow(length_pseudo_rows) == 0) {
    length_component <- is.finite(base_df$length) & is.finite(base_df$freq) & base_df$freq >= 0 &
      is.finite(pseudo_df$length) & is.finite(pseudo_df$freq) & pseudo_df$freq >= 0
    length_base_rows <- data.frame(
      component = "length mean",
      series = paste0("fishery_", as.integer(base_df$fishery[length_component])),
      year = as.integer(base_df$year[length_component]),
      value = suppressWarnings(as.numeric(base_df$length[length_component])),
      weight = suppressWarnings(as.numeric(base_df$freq[length_component])),
      source = "observed_fallback",
      stringsAsFactors = FALSE
    )
    length_pseudo_rows <- data.frame(
      component = "length mean",
      series = paste0("fishery_", as.integer(pseudo_df$fishery[length_component])),
      year = as.integer(pseudo_df$year[length_component]),
      value = suppressWarnings(as.numeric(pseudo_df$length[length_component])),
      weight = suppressWarnings(as.numeric(pseudo_df$freq[length_component])),
      source = "pseudo_frq_fallback",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(weight_base_rows) == 0 || nrow(weight_pseudo_rows) == 0) {
    weight_component <- is.finite(base_df$weight) & is.finite(base_df$freq) & base_df$freq >= 0 &
      is.finite(pseudo_df$weight) & is.finite(pseudo_df$freq) & pseudo_df$freq >= 0
    weight_base_rows <- data.frame(
      component = "weight mean",
      series = paste0("fishery_", as.integer(base_df$fishery[weight_component])),
      year = as.integer(base_df$year[weight_component]),
      value = suppressWarnings(as.numeric(base_df$weight[weight_component])),
      weight = suppressWarnings(as.numeric(base_df$freq[weight_component])),
      source = "observed_fallback",
      stringsAsFactors = FALSE
    )
    weight_pseudo_rows <- data.frame(
      component = "weight mean",
      series = paste0("fishery_", as.integer(pseudo_df$fishery[weight_component])),
      year = as.integer(pseudo_df$year[weight_component]),
      value = suppressWarnings(as.numeric(pseudo_df$weight[weight_component])),
      weight = suppressWarnings(as.numeric(pseudo_df$freq[weight_component])),
      source = "pseudo_frq_fallback",
      stringsAsFactors = FALSE
    )
  }

  rows <- rbind(catch_rows, effort_rows, cpue_rows)
  rows <- rows[is.finite(rows$year) & is.finite(rows$base) & is.finite(rows$pseudo), , drop = FALSE]
  aggregate_one <- function(x) {
    data.frame(
      n = nrow(x),
      base_total = if (identical(x$component[[1]], "catch total")) sum(x$base, na.rm = TRUE) else NA_real_,
      pseudo_total = if (identical(x$component[[1]], "catch total")) sum(x$pseudo, na.rm = TRUE) else NA_real_,
      base_mean = mean(x$base, na.rm = TRUE),
      pseudo_mean = mean(x$pseudo, na.rm = TRUE),
      base_median = stats::median(x$base, na.rm = TRUE),
      pseudo_median = stats::median(x$pseudo, na.rm = TRUE)
    )
  }
  frq_out <- data.frame()
  if (nrow(rows) > 0) {
    split_rows <- split(rows, paste(rows$component, rows$series, rows$year, sep = "\r"))
    frq_out <- do.call(rbind, lapply(split_rows, aggregate_one))
    key <- do.call(rbind, strsplit(names(split_rows), "\r", fixed = TRUE))
    rownames(frq_out) <- NULL
    frq_out$component <- key[, 1]
    frq_out$series <- key[, 2]
    frq_out$year <- as.integer(key[, 3])
    frq_out$base_value <- ifelse(frq_out$component == "catch total", frq_out$base_total, frq_out$base_mean)
    frq_out$pseudo_value <- ifelse(frq_out$component == "catch total", frq_out$pseudo_total, frq_out$pseudo_mean)
    frq_out$ratio <- frq_out$pseudo_value / pmax(frq_out$base_value, .Machine$double.eps)
    frq_out$delta <- frq_out$pseudo_value - frq_out$base_value
  }

  length_out <- st_data_summary_rows(
    "length mean", length_base_rows, length_pseudo_rows,
    function(x) st_weighted_mean(x$value, x$weight)
  )
  if (nrow(length_out) > 0) length_out$base_source <- unique(length_base_rows$source)[[1]]
  length_probs <- c(`length q10` = 0.10, `length median` = 0.50, `length q90` = 0.90)
  length_quant_out <- do.call(rbind, Map(
    function(component_name, prob) {
      st_data_summary_rows(
        component_name, length_base_rows, length_pseudo_rows,
        function(x) st_weighted_quantile(x$value, x$weight, prob)
      )
    },
    names(length_probs), as.list(length_probs)
  ))
  if (nrow(length_quant_out) > 0) length_quant_out$base_source <- unique(length_base_rows$source)[[1]]

  weight_out <- st_data_summary_rows(
    "weight mean", weight_base_rows, weight_pseudo_rows,
    function(x) st_weighted_mean(x$value, x$weight)
  )
  if (nrow(weight_out) > 0) weight_out$base_source <- unique(weight_base_rows$source)[[1]]
  weight_probs <- c(`weight q10` = 0.10, `weight median` = 0.50, `weight q90` = 0.90)
  weight_quant_out <- do.call(rbind, Map(
    function(component_name, prob) {
      st_data_summary_rows(
        component_name, weight_base_rows, weight_pseudo_rows,
        function(x) st_weighted_quantile(x$value, x$weight, prob)
      )
    },
    names(weight_probs), as.list(weight_probs)
  ))
  if (nrow(weight_quant_out) > 0) weight_quant_out$base_source <- unique(weight_base_rows$source)[[1]]

  tag_out <- st_data_summary_rows(
    "tag recaptures", st_tag_expected_annual_summary(sim_dir), st_tag_annual_summary(pseudo_tag_file),
    function(x) sum(x$value, na.rm = TRUE)
  )
  if (nrow(tag_out) > 0) tag_out$base_source <- "fitted_tag_pred"
  tag_fishery_out <- tag_out[tag_out$series != "all", , drop = FALSE]
  if (nrow(tag_fishery_out) > 0) tag_fishery_out$component <- "tag recaptures by fishery"
  tag_out <- tag_out[tag_out$series == "all", , drop = FALSE]
  alk_out <- st_data_summary_rows(
    "age-length mean age", st_alk_expected_annual_summary(sim_dir, base_alk_file), st_alk_annual_summary(pseudo_alk_file),
    function(x) mean(x$value, na.rm = TRUE)
  )
  if (nrow(alk_out) > 0) alk_out$base_source <- "fitted_age_length_pred"
  alk_probs <- c(`age-length age q10` = 0.10, `age-length age median` = 0.50, `age-length age q90` = 0.90)
  alk_quant_out <- do.call(rbind, Map(
    function(component_name, prob) {
      st_data_summary_rows(
        component_name, st_alk_expected_annual_rows(sim_dir, base_alk_file), st_alk_annual_rows(pseudo_alk_file),
        function(x) st_weighted_quantile(x$value, x$weight, prob)
      )
    },
    names(alk_probs), as.list(alk_probs)
  ))
  if (nrow(alk_quant_out) > 0) alk_quant_out$base_source <- "fitted_age_length_pred"

  summary_cols <- c(
    "component", "series", "year", "n", "base_value", "pseudo_value", "ratio", "delta",
    "base_total", "pseudo_total", "base_mean", "pseudo_mean", "base_median", "pseudo_median",
    "base_source"
  )
  complete_summary <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(data.frame())
    for (nm in summary_cols) {
      if (!nm %in% names(x)) x[[nm]] <- if (nm %in% c("component", "series", "base_source")) "all" else NA_real_
    }
    x[, summary_cols, drop = FALSE]
  }
  out <- do.call(rbind, lapply(
    list(frq_out, length_out, length_quant_out, weight_out, weight_quant_out, tag_out, tag_fishery_out, alk_out, alk_quant_out),
    complete_summary
  ))
  if (nrow(out) == 0) return(data.frame())
  out[order(out$component, out$year), , drop = FALSE]
}

st_build_pseudo_input <- function(base_dir,
                                  sim_dir,
                                  input_dir,
                                  par_file = NULL,
                                  update_catch = TRUE,
                                  update_effort = FALSE,
                                  update_lw = TRUE,
                                  update_cpue = TRUE,
                                  update_tags = TRUE,
                                  update_age_length = TRUE,
                                  require_native_tags = TRUE,
                                  seeds = list(age_length = 1007L)) {
  st_copy_dir_contents(base_dir, input_dir)
  frq_file <- st_first_file(input_dir, "\\.frq$")
  base_frq_file <- file.path(base_dir, basename(frq_file))
  diagnostics <- st_apply_pseudo_to_frq(
    base_frq_file = base_frq_file,
    out_frq_file = frq_file,
    sim_dir = sim_dir,
    par_file = par_file,
    update_catch = update_catch,
    update_effort = update_effort,
    update_lw = update_lw,
    update_cpue = update_cpue,
    seeds = seeds
  )
  diagnostics$catch_conditioned <- if (!is.null(par_file) && file.exists(par_file)) st_catch_conditioned(par_file) else NA
  diagnostics$effort_conditioned <- if (!is.null(par_file) && file.exists(par_file)) st_effort_conditioned(par_file) else NA
  diagnostics$update_catch <- isTRUE(update_catch)
  diagnostics$update_effort <- isTRUE(update_effort)
  base_frq_obj <- read.MFCLFrq(base_frq_file)
  pseudo_frq_obj <- read.MFCLFrq(frq_file)
  base_range <- as.integer(range(base_frq_obj)[c("minyear", "maxyear")])
  pseudo_range <- as.integer(range(pseudo_frq_obj)[c("minyear", "maxyear")])
  if (!identical(base_range, pseudo_range)) {
    stop(
      "Self-test pseudo .frq year range changed from fitted input range: base ",
      paste(base_range, collapse = "-"),
      ", pseudo ",
      paste(pseudo_range, collapse = "-")
    )
  }
  diagnostics$base_minyear <- base_range[[1]]
  diagnostics$base_maxyear <- base_range[[2]]
  diagnostics$pseudo_minyear <- pseudo_range[[1]]
  diagnostics$pseudo_maxyear <- pseudo_range[[2]]
  diagnostics$estimation_period_matched <- TRUE

  if (!is.null(par_file) && file.exists(par_file)) {
    file.copy(par_file, file.path(input_dir, basename(par_file)), overwrite = TRUE)
  }

  if (isTRUE(update_tags)) {
    tag_file <- st_first_file(input_dir, "\\.tag$", required = FALSE)
    if (!is.na(tag_file)) {
      base_tag_file <- file.path(base_dir, basename(tag_file))
      tag_info <- tryCatch(
        st_apply_native_realtag(base_tag_file, tag_file, sim_dir),
        error = function(e) {
          if (isTRUE(require_native_tags)) stop(e)
          list(tag_source = "unchanged_no_native_realtag", tag_error = conditionMessage(e))
        }
      )
      diagnostics <- c(diagnostics, tag_info)
    }
  }

  if (isTRUE(update_age_length)) {
    age_file <- st_first_file(input_dir, "\\.age_length$", required = FALSE)
    if (!is.na(age_file)) {
      base_age_file <- file.path(base_dir, basename(age_file))
      age_info <- st_apply_pseudo_to_age_length(
        base_alk_file = base_age_file,
        out_alk_file = age_file,
        sim_dir = sim_dir,
        seed = seeds$age_length
      )
      diagnostics <- c(diagnostics, age_info)
    }
  }

  tag_file <- st_first_file(input_dir, "\\.tag$", required = FALSE)
  base_tag_file <- if (!is.na(tag_file)) file.path(base_dir, basename(tag_file)) else NA_character_
  age_file <- st_first_file(input_dir, "\\.age_length$", required = FALSE)
  base_age_file <- if (!is.na(age_file)) file.path(base_dir, basename(age_file)) else NA_character_
  data_summary <- st_summarise_data_simulation(
    base_frq_file = base_frq_file,
    pseudo_frq_file = frq_file,
    sim_dir = sim_dir,
    par_file = par_file,
    seeds = seeds,
    base_tag_file = base_tag_file,
    pseudo_tag_file = tag_file,
    base_alk_file = base_age_file,
    pseudo_alk_file = age_file
  )
  saveRDS(data_summary, file.path(input_dir, "data_simulation_summary.rds"), compress = "xz")
  saveRDS(diagnostics, file.path(input_dir, "selftest_input_info.rds"), compress = "xz")
  diagnostics
}

st_run_refit <- function(input_dir,
                         refit_dir,
                         program_path_abs,
                         fevals = 500L,
                         mode = "last_par",
                         output_par = "selftest_refit.par",
                         log_file = file.path(refit_dir, "mfcl_selftest_refit_log.txt"),
                         tag_report_year1 = "auto") {
  st_copy_dir_contents(input_dir, refit_dir)
  frq_file <- basename(st_first_file(refit_dir, "\\.frq$"))
  par_file <- basename(st_latest_par(refit_dir))
  mode <- tolower(trimws(as.character(mode[[1]])))
  if (!mode %in% c("last_par", "doitall")) {
    stop("Unsupported self-test refit mode: ", mode, ". Use last_par or doitall.")
  }

  if (identical(mode, "last_par")) {
    if (is.na(par_file) || !file.exists(file.path(refit_dir, par_file))) {
      stop("Refit input has no .par start file: ", refit_dir)
    }
    st_remove_mfcl_run_outputs(refit_dir, keep_par = par_file, remove_par = TRUE)
    cmd <- paste(
      shQuote(program_path_abs),
      shQuote(frq_file),
      shQuote(par_file),
      shQuote(output_par),
      "-switch 1",
      "1 1",
      as.integer(fevals)
    )
  } else {
    if (!file.exists(file.path(refit_dir, "doitall.sh"))) {
      stop("Refit mode doitall requires doitall.sh in pseudo input folder: ", refit_dir)
    }
    removed <- st_remove_mfcl_run_outputs(refit_dir, remove_par = TRUE)
    if (removed > 0) cat("Removed", removed, "copied MFCL output files before doitall refit\n")
    old_program_path <- Sys.getenv("PROGRAM_PATH", unset = NA_character_)
    Sys.setenv(PROGRAM_PATH = program_path_abs)
    on.exit({
      if (is.na(old_program_path)) {
        Sys.unsetenv("PROGRAM_PATH")
      } else {
        Sys.setenv(PROGRAM_PATH = old_program_path)
      }
    }, add = TRUE)
    cmd <- "./doitall.sh"
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(refit_dir)
  cat("MFCL refit command:\n", cmd, "\n")
  if (identical(mode, "doitall")) Sys.chmod("doitall.sh", mode = "0755")
  status <- system(paste(cmd, ">", shQuote(log_file), "2>&1"), intern = FALSE)
  setwd(old_wd)

  final_par <- file.path(refit_dir, output_par)
  if (identical(mode, "last_par") && !file.exists(final_par)) {
    stop("MFCL last_par refit did not create expected output .par: ", final_par, "; see ", log_file)
  }
  if (!file.exists(final_par)) final_par <- mp_final_par(refit_dir)
  if (is.null(final_par) || !file.exists(final_par)) {
    stop("MFCL refit did not create a final .par in ", refit_dir, "; see ", log_file)
  }
  obj_fun <- if (!is.null(final_par) && file.exists(final_par)) mp_extract_par_obj_fun(final_par) else NA_real_
  max_grad <- if (!is.null(final_par) && file.exists(final_par)) mp_extract_par_max_grad(final_par) else NA_real_
  info <- list(
    description = "MFCL self-test refit",
    program_path = program_path_abs,
    mfcl_commands = cmd,
    frq_file = frq_file,
    par_in = if (identical(mode, "last_par")) par_file else NA_character_,
    par_out = if (!is.null(final_par)) basename(final_par) else output_par,
    refit_mode = mode,
    refit_fevals = if (identical(mode, "last_par")) as.integer(fevals) else NA_integer_,
    base_dir = input_dir,
    model_dir = refit_dir,
    selftest = TRUE,
    exit_status = as.integer(status),
    obj_fun = obj_fun,
    tag_lik = st_par_slot_value(final_par, "tag_lik"),
    mn_len_pen = st_par_slot_value(final_par, "mn_len_pen"),
    max_grad = max_grad,
    converged = isTRUE(is.finite(max_grad) && abs(max_grad) <= 0.01)
  )
  saveRDS(info, file.path(refit_dir, "model_info.rds"), compress = "xz")

  if (isTRUE(st_keep_model_payload())) {
    payload <- mp_build_model_payload(refit_dir, tag_report_year1 = tag_report_year1)
    saveRDS(payload, file.path(refit_dir, "model_payload.rds"), compress = "xz")
  } else {
    unlink(file.path(refit_dir, "model_payload.rds"), force = TRUE)
  }
  info
}

st_run_truth_on_pseudo <- function(input_dir,
                                   eval_dir,
                                   program_path_abs,
                                   output_par = "truth_on_pseudo.par",
                                   log_file = file.path(eval_dir, "mfcl_truth_on_pseudo_log.txt"),
                                   tag_report_year1 = "auto") {
  st_copy_dir_contents(input_dir, eval_dir)
  frq_file <- basename(st_first_file(eval_dir, "\\.frq$"))
  par_file <- basename(st_latest_par(eval_dir))
  st_remove_mfcl_run_outputs(eval_dir, keep_par = par_file, remove_par = TRUE)
  if (is.na(par_file) || !file.exists(file.path(eval_dir, par_file))) {
    stop("Truth-on-pseudo input has no .par file: ", eval_dir)
  }

  cmd <- paste(
    shQuote(program_path_abs),
    shQuote(frq_file),
    shQuote(par_file),
    shQuote(output_par),
    "-switch 1",
    "1 1",
    "0"
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(eval_dir)
  cat("MFCL truth-on-pseudo command:\n", cmd, "\n")
  status <- system(paste(cmd, ">", shQuote(log_file), "2>&1"), intern = FALSE)
  setwd(old_wd)

  parse_total_func <- function(log_file) {
    if (!file.exists(log_file)) return(NA_real_)
    log_lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
    total_func <- grep("Total func", log_lines, value = TRUE)
    if (length(total_func) > 0) {
      nums <- regmatches(total_func[[length(total_func)]], gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", total_func[[length(total_func)]], perl = TRUE))[[1]]
      vals <- suppressWarnings(as.numeric(nums))
      vals <- vals[is.finite(vals)]
      if (length(vals) > 0) return(vals[[length(vals)]])
    }
    NA_real_
  }

  final_par <- file.path(eval_dir, output_par)
  output_par_exists <- file.exists(final_par)
  obj_fun <- parse_total_func(log_file)
  if (!is.finite(obj_fun) && output_par_exists) obj_fun <- mp_extract_par_obj_fun(final_par)
  max_grad <- if (output_par_exists) mp_extract_par_max_grad(final_par) else NA_real_
  info <- list(
    description = "MFCL self-test truth par evaluated on pseudo data",
    program_path = program_path_abs,
    mfcl_commands = cmd,
    frq_file = frq_file,
    par_in = par_file,
    par_out = if (!is.null(final_par)) basename(final_par) else output_par,
    base_dir = input_dir,
    model_dir = eval_dir,
    selftest = TRUE,
    truth_on_pseudo = TRUE,
    exit_status = as.integer(status),
    obj_fun = obj_fun,
    tag_lik = st_par_slot_value(final_par, "tag_lik"),
    mn_len_pen = st_par_slot_value(final_par, "mn_len_pen"),
    max_grad = max_grad,
    converged = isTRUE(is.finite(max_grad) && abs(max_grad) <= 0.01)
  )
  saveRDS(info, file.path(eval_dir, "model_info.rds"), compress = "xz")

  if (isTRUE(output_par_exists) && isTRUE(st_keep_model_payload())) {
    payload <- tryCatch(
      mp_build_model_payload(eval_dir, tag_report_year1 = tag_report_year1),
      error = function(e) {
        warning("Could not build truth-on-pseudo payload for ", basename(eval_dir), ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(payload)) {
      payload$obj_fun <- obj_fun
      payload$max_grad <- max_grad
      saveRDS(payload, file.path(eval_dir, "model_payload.rds"), compress = "xz")
    }
  } else {
    unlink(file.path(eval_dir, "model_payload.rds"), force = TRUE)
  }
  info
}

st_read_indepvar <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(
    utils::read.table(path, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, comment.char = ""),
    error = function(e) NULL
  )
  if (is.null(out) || nrow(out) == 0) return(NULL)
  names(out) <- sub("^Var_name$", "name", names(out))
  names(out) <- sub("^Estimate$", "estimate", names(out))
  names(out) <- sub("^Index$", "index", names(out))
  out$index <- suppressWarnings(as.integer(out$index))
  out$estimate <- suppressWarnings(as.numeric(out$estimate))
  out
}

st_parameter_recovery <- function(truth_indepvar,
                                  refit_indepvar,
                                  out_file,
                                  key_parameters = character(0)) {
  truth <- st_read_indepvar(truth_indepvar)
  refit <- st_read_indepvar(refit_indepvar)
  if (is.null(truth) || is.null(refit)) return(NULL)
  truth <- truth[, intersect(c("index", "name", "estimate"), names(truth)), drop = FALSE]
  refit <- refit[, intersect(c("index", "name", "estimate"), names(refit)), drop = FALSE]
  names(truth)[names(truth) == "estimate"] <- "truth"
  names(refit)[names(refit) == "estimate"] <- "estimate"
  merged <- merge(truth, refit, by = c("index", "name"), all = FALSE)
  merged$delta <- merged$estimate - merged$truth
  merged$rel_delta <- merged$delta / ifelse(is.finite(merged$truth) & abs(merged$truth) > 1e-12, abs(merged$truth), NA_real_)
  merged$pct_delta <- 100 * merged$rel_delta
  if (length(key_parameters) > 0) {
    keep <- Reduce(`|`, lapply(key_parameters, function(pat) grepl(pat, merged$name, fixed = TRUE)))
    merged$key_parameter <- keep
  } else {
    merged$key_parameter <- FALSE
  }
  utils::write.csv(merged, out_file, row.names = FALSE)
  merged
}

st_derived_recovery <- function(truth_dir, refit_dir, out_file) {
  truth_rep <- mp_final_rep(truth_dir)
  refit_rep <- mp_final_rep(refit_dir)
  if (is.null(truth_rep) || is.null(refit_rep) || !file.exists(truth_rep) || !file.exists(refit_rep)) {
    return(NULL)
  }
  truth_obj <- mp_safe(read.MFCLRep(truth_rep))
  refit_obj <- mp_safe(read.MFCLRep(refit_rep))
  truth_ts <- mp_extract_rep_timeseries(truth_obj, scenario = "truth", peel = 0L)
  refit_ts <- mp_extract_rep_timeseries(refit_obj, scenario = "refit", peel = 0L)
  if (is.null(truth_ts) || is.null(refit_ts)) return(NULL)
  truth_ts$scenario <- NULL
  truth_ts$peel <- NULL
  refit_ts$scenario <- NULL
  refit_ts$peel <- NULL
  names(truth_ts)[names(truth_ts) != "year"] <- paste0(names(truth_ts)[names(truth_ts) != "year"], "_truth")
  names(refit_ts)[names(refit_ts) != "year"] <- paste0(names(refit_ts)[names(refit_ts) != "year"], "_estimate")
  out <- merge(truth_ts, refit_ts, by = "year", all = FALSE)
  for (nm in names(out)) {
    if (!grepl("_truth$", nm)) next
    base <- sub("_truth$", "", nm)
    est_nm <- paste0(base, "_estimate")
    if (est_nm %in% names(out)) {
      out[[paste0(base, "_delta")]] <- out[[est_nm]] - out[[nm]]
      out[[paste0(base, "_rel_delta")]] <- out[[paste0(base, "_delta")]] /
        ifelse(is.finite(out[[nm]]) & abs(out[[nm]]) > 1e-12, abs(out[[nm]]), NA_real_)
    }
  }
  utils::write.csv(out, out_file, row.names = FALSE)
  out
}
