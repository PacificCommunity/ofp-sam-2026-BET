#!/usr/bin/env Rscript

library(FLR4MFCL)
source("tools/jitter.R")

parse_numeric_tokens <- function(x) {
  txt <- paste(as.character(x), collapse = " ")
  if (is.null(x) || !nzchar(trimws(txt))) return(numeric(0))
  m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", txt, perl = TRUE)
  toks <- regmatches(txt, m)[[1]]
  vals <- suppressWarnings(as.numeric(toks))
  vals[is.finite(vals)]
}

parse_tokens <- function(x) {
  txt <- paste(as.character(x), collapse = " ")
  if (!nzchar(trimws(txt))) return(character(0))
  toks <- unlist(strsplit(txt, "[[:space:],]+", perl = TRUE))
  toks <- trimws(toks)
  toks[nzchar(toks)]
}

parse_env_kv <- function(env_kv) {
  if (!length(env_kv)) return(list())
  out <- list()
  for (entry in env_kv) {
    eq_pos <- regexpr("=", entry, fixed = TRUE)[[1]]
    if (!is.finite(eq_pos) || eq_pos < 2) next
    key <- substr(entry, 1, eq_pos - 1)
    val <- substr(entry, eq_pos + 1, nchar(entry))
    out[[key]] <- val
  }
  out
}

run_with_env <- function(env_list, code) {
  set_named_env <- function(key, value) {
    do.call(Sys.setenv, stats::setNames(list(value), key))
  }

  env_names <- names(env_list)
  if (length(env_names) == 0) return(eval.parent(substitute(code)))
  old_vals <- Sys.getenv(env_names, unset = NA_character_)
  on.exit({
    for (nm in env_names) {
      old <- old_vals[[nm]]
      if (is.na(old)) Sys.unsetenv(nm) else set_named_env(nm, old)
    }
  }, add = TRUE)
  do.call(Sys.setenv, env_list)
  eval.parent(substitute(code))
}

prof_2d_indepvar <- Sys.getenv("prof_2d_indepvar", "")
prof_2d_scalars_x <- Sys.getenv("prof_2d_scalars_x", "")
prof_2d_scalars_y <- Sys.getenv("prof_2d_scalars_y", "")
prof_2d_values_x <- Sys.getenv("prof_2d_values_x", "")
prof_2d_values_y <- Sys.getenv("prof_2d_values_y", "")
prof_2d_path <- tolower(trimws(Sys.getenv("prof_2d_path", "snake_x")))
prof_2d_first_init_from <- suppressWarnings(as.numeric(Sys.getenv("prof_2d_first_init_from", "")))
prof_2d_anchor_x <- suppressWarnings(as.numeric(Sys.getenv("prof_2d_anchor_x", "")))
prof_2d_anchor_y <- suppressWarnings(as.numeric(Sys.getenv("prof_2d_anchor_y", "")))
prof_2d_parallel_jobs <- suppressWarnings(as.integer(Sys.getenv("prof_2d_parallel_jobs", "")))
init_par_override <- Sys.getenv("init_par_override", "")
profile_set_name <- Sys.getenv("profile_set_name", "")
profile_set_label <- Sys.getenv("profile_set_label", "")
profile_set_tag <- Sys.getenv("profile_set_tag", "")
prof_hessian <- Sys.getenv("prof_hessian", "")
prof_init_map_rds <- Sys.getenv("prof_init_map_rds", "")
prof_fix_indepvar_file <- Sys.getenv("prof_fix_indepvar_file", "")
indepvar_reps <- Sys.getenv("indepvar_reps", "")
prof_extra_switch <- Sys.getenv("prof_extra_switch", "")
prof_2d_extra_switch <- Sys.getenv("prof_2d_extra_switch", "")
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

params <- parse_tokens(prof_2d_indepvar)
x_scalars <- sort(unique(parse_numeric_tokens(prof_2d_scalars_x)))
y_scalars <- sort(unique(parse_numeric_tokens(prof_2d_scalars_y)))
x_vals_raw <- sort(unique(parse_numeric_tokens(prof_2d_values_x)))
y_vals_raw <- sort(unique(parse_numeric_tokens(prof_2d_values_y)))

if (length(params) != 2L) {
  stop("prof_2d_indepvar must contain exactly two indepvar tokens.")
}
if ((length(x_scalars) == 0L || length(y_scalars) == 0L) && (length(x_vals_raw) == 0L || length(y_vals_raw) == 0L)) {
  stop("Provide either prof_2d_scalars_x/y or prof_2d_values_x/y.")
}
if (!prof_2d_path %in% c("snake_x", "x_major", "axis_chains")) {
  stop("prof_2d_path must be one of: snake_x, x_major, axis_chains")
}
if (!is.finite(prof_2d_parallel_jobs) || prof_2d_parallel_jobs < 1L) prof_2d_parallel_jobs <- 2L

combine_switch_text <- function(...) {
  vals <- vapply(list(...), function(x) trimws(paste(as.character(x), collapse = " ")), character(1))
  vals <- vals[nzchar(vals)]
  paste(vals, collapse = " ")
}

combined_extra_switch <- combine_switch_text(prof_extra_switch, prof_2d_extra_switch)

project_root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) getwd())
run_prof_script <- file.path(project_root, "runners", "run_prof.R")
if (!file.exists(run_prof_script)) {
  stop("Cannot find runners/run_prof.R under project root: ", project_root)
}
base_dir_abs <- file.path(project_root, base_dir)

resolve_indepvar_path <- function(model_dir, base_dir_abs) {
  candidates <- c(
    file.path(model_dir, "indepvar.rpt"),
    file.path(base_dir_abs, "indepvar.rpt")
  )
  candidates <- unique(candidates[is.character(candidates) & nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

resolve_baseline_par_file <- function(model_dir, base_dir_abs) {
  rank_par_candidates <- function(paths) {
    paths <- unique(paths[file.exists(paths)])
    if (length(paths) == 0) return(character(0))

    base_names <- basename(paths)
    numeric_id <- suppressWarnings(as.integer(sub("^([0-9]+)\\.par.*$", "\\1", base_names, perl = TRUE)))
    exact_par <- grepl("^[0-9]+\\.par$", base_names)
    info <- file.info(paths)
    info_mtime <- if (!is.null(info$mtime)) as.numeric(info$mtime) else rep(NA_real_, length(paths))

    ord <- order(
      !exact_par,
      -ifelse(is.finite(numeric_id), numeric_id, -1L),
      -ifelse(is.finite(info_mtime), info_mtime, -Inf),
      base_names
    )
    paths[ord]
  }

  collect_pars <- function(dir_path) {
    if (!is.character(dir_path) || !nzchar(dir_path) || !dir.exists(dir_path)) return(character(0))
    list.files(dir_path, pattern = "\\.par($|[^/]*$)", full.names = TRUE)
  }

  preferred <- c(
    file.path(model_dir, "11.par"),
    file.path(model_dir, "10.par"),
    file.path(model_dir, "01.par"),
    file.path(model_dir, "00.par"),
    file.path(model_dir, "1.par"),
    file.path(base_dir_abs, "11.par"),
    file.path(base_dir_abs, "10.par"),
    file.path(base_dir_abs, "01.par"),
    file.path(base_dir_abs, "00.par"),
    file.path(base_dir_abs, "1.par")
  )
  preferred <- preferred[file.exists(preferred)]
  if (length(preferred) > 0) return(preferred[[1]])

  all_candidates <- c(collect_pars(model_dir), collect_pars(base_dir_abs))
  ranked <- rank_par_candidates(all_candidates)
  if (length(ranked) == 0) return(NA_character_)
  ranked[[1]]
}

resolve_baseline_indepvar_values <- function(tokens, model_dir, base_dir_abs) {
  indepvar_path <- resolve_indepvar_path(model_dir, base_dir_abs)
  if (!is.character(indepvar_path) || !nzchar(indepvar_path) || !file.exists(indepvar_path)) {
    stop("2D scalar mode requires indepvar.rpt in model_dir or base_dir.")
  }
  baseline_par_file <- resolve_baseline_par_file(model_dir, base_dir_abs)
  if (!is.character(baseline_par_file) || !nzchar(baseline_par_file) || !file.exists(baseline_par_file)) {
    stop("2D scalar mode requires a baseline .par file in model_dir or base_dir.")
  }
  cat("2D scalar mode baseline par:", basename(baseline_par_file), "\n")
  base_par <- suppressWarnings(tryCatch(read.MFCLPar(baseline_par_file), error = function(e) NULL))
  if (is.null(base_par)) stop("Failed to read baseline .par for 2D scalar mode: ", baseline_par_file)
  indepvar_map <- build_indepvar_mapping(base_par, indepvar_file = indepvar_path, tol = 1e-14)
  if (is.null(indepvar_map) || nrow(indepvar_map$mapping) == 0) {
    stop("Failed to build indepvar mapping for 2D scalar mode from: ", indepvar_path)
  }
  report <- parse_indepvar_report(indepvar_path)
  if (is.null(report) || nrow(report) != nrow(indepvar_map$mapping)) {
    stop("Could not parse indepvar.rpt consistently for 2D scalar mode: ", indepvar_path)
  }
  current_values <- extract_indepvar_values(base_par, indepvar_map)
  target_idx <- vapply(tokens, function(tok) {
    hit <- if (grepl("^[0-9]+$", tok)) which(report$Index == as.integer(tok)) else which(report$Var_name == tok)
    if (length(hit) == 0) stop("2D scalar mode indepvar token not found: ", tok)
    hit[[1]]
  }, integer(1))
  vals <- as.numeric(current_values[target_idx])
  names(vals) <- tokens
  vals
}

if (length(x_scalars) > 0L && length(y_scalars) > 0L) {
  baseline_vals <- resolve_baseline_indepvar_values(params, model_dir, base_dir_abs)
  x_vals <- baseline_vals[[1]] * x_scalars / 100
  y_vals <- baseline_vals[[2]] * y_scalars / 100
} else {
  x_vals <- x_vals_raw
  y_vals <- y_vals_raw
}

grid <- expand.grid(
  x_index = seq_along(x_vals),
  y_index = seq_along(y_vals),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
grid$x_value <- x_vals[grid$x_index]
grid$y_value <- y_vals[grid$y_index]

if (identical(prof_2d_path, "snake_x")) {
  row_parts <- lapply(seq_along(x_vals), function(ix) {
    yy <- if (ix %% 2L == 1L) seq_along(y_vals) else rev(seq_along(y_vals))
    data.frame(x_index = ix, y_index = yy, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  })
  run_order <- do.call(rbind, row_parts)
} else if (identical(prof_2d_path, "x_major")) {
  run_order <- grid[order(grid$x_index, grid$y_index), c("x_index", "y_index"), drop = FALSE]
} else {
  run_order <- grid[order(grid$x_index, grid$y_index), c("x_index", "y_index"), drop = FALSE]
}

grid <- merge(
  run_order,
  grid,
  by = c("x_index", "y_index"),
  sort = FALSE
)
grid$point_id <- seq_len(nrow(grid))
grid$point_key <- sprintf("x%03d_y%03d", grid$x_index, grid$y_index)

cat("=== 2D Indepvar Profile Run ===\n")
cat("prof_2d_indepvar:", paste(params, collapse = " "), "\n")
if (length(x_scalars) > 0L && length(y_scalars) > 0L) {
  cat("prof_2d_scalars_x:", paste(x_scalars, collapse = " "), "\n")
  cat("prof_2d_scalars_y:", paste(y_scalars, collapse = " "), "\n")
}
cat("prof_2d_values_x:", paste(x_vals, collapse = " "), "\n")
cat("prof_2d_values_y:", paste(y_vals, collapse = " "), "\n")
cat("prof_2d_path:", prof_2d_path, "\n")
cat("prof_2d_parallel_jobs:", prof_2d_parallel_jobs, "\n")
cat("prof_2d_extra_switch:", ifelse(nzchar(prof_2d_extra_switch), prof_2d_extra_switch, "<none>"), "\n")
cat("combined_extra_switch:", ifelse(nzchar(combined_extra_switch), combined_extra_switch, "<none>"), "\n")
cat("profile_set_name:", ifelse(nzchar(profile_set_name), profile_set_name, "<none>"), "\n")
cat("profile_set_label:", ifelse(nzchar(profile_set_label), profile_set_label, "<none>"), "\n")
cat("profile_set_tag:", ifelse(nzchar(profile_set_tag), profile_set_tag, "<none>"), "\n")

run_point <- function(pt, donor = NA_real_, use_init_override = FALSE) {
  point_label <- sprintf(
    "%s=%s | %s=%s",
    params[[1]], format(pt$x_value[[1]], scientific = FALSE, trim = TRUE),
    params[[2]], format(pt$y_value[[1]], scientific = FALSE, trim = TRUE)
  )

  env_kv <- c(
    paste0("scalar=", pt$point_id[[1]]),
    paste0("prof_fix_indepvar=", paste(params, collapse = " ")),
    paste0(
      "prof_fix_values=",
      paste(
        format(pt$x_value[[1]], scientific = FALSE, trim = TRUE),
        format(pt$y_value[[1]], scientific = FALSE, trim = TRUE)
      )
    ),
    "prof_use_quantity_penalty=0",
    "skip_condor_archive_cleanup=1",
    paste0("prof_2d_enabled=1"),
    paste0("prof_2d_x_param=", params[[1]]),
    paste0("prof_2d_y_param=", params[[2]]),
    paste0("prof_2d_x_index=", pt$x_index[[1]]),
    paste0("prof_2d_y_index=", pt$y_index[[1]]),
    paste0("prof_2d_x_value=", format(pt$x_value[[1]], scientific = FALSE, trim = TRUE)),
    paste0("prof_2d_y_value=", format(pt$y_value[[1]], scientific = FALSE, trim = TRUE)),
    paste0("prof_2d_point_key=", pt$point_key[[1]]),
    paste0("prof_2d_point_label=", point_label),
    paste0("prof_2d_grid_nx=", length(x_vals)),
    paste0("prof_2d_grid_ny=", length(y_vals))
  )

  if (nzchar(profile_set_name)) env_kv <- c(env_kv, paste0("profile_set_name=", profile_set_name))
  if (nzchar(profile_set_label)) env_kv <- c(env_kv, paste0("profile_set_label=", profile_set_label))
  if (nzchar(profile_set_tag)) env_kv <- c(env_kv, paste0("profile_set_tag=", profile_set_tag))
  if (nzchar(prof_hessian)) env_kv <- c(env_kv, paste0("prof_hessian=", prof_hessian))
  if (nzchar(prof_init_map_rds)) env_kv <- c(env_kv, paste0("prof_init_map_rds=", prof_init_map_rds))
  if (nzchar(prof_fix_indepvar_file)) env_kv <- c(env_kv, paste0("prof_fix_indepvar_file=", prof_fix_indepvar_file))
  if (nzchar(indepvar_reps)) env_kv <- c(env_kv, paste0("indepvar_reps=", indepvar_reps))
  if (nzchar(combined_extra_switch)) env_kv <- c(env_kv, paste0("prof_extra_switch=", combined_extra_switch))
  if (isTRUE(use_init_override) && nzchar(init_par_override)) env_kv <- c(env_kv, paste0("init_par_override=", init_par_override))
  if (is.finite(donor)) env_kv <- c(env_kv, paste0("init_from_scalar=", format(donor, scientific = FALSE, trim = TRUE)))

  cat(
    "\n--- 2D profile point ",
    " point_id=", pt$point_id[[1]],
    " key=", pt$point_key[[1]],
    " donor=", ifelse(is.finite(donor), as.character(donor), "<none>"),
    " ---\n",
    sep = ""
  )
  cat("    ", point_label, "\n", sep = "")

  env_list <- parse_env_kv(env_kv)
  status <- run_with_env(
    env_list,
    system2(
      "Rscript",
      args = c("runners/run_prof.R"),
      stdout = "",
      stderr = ""
    )
  )

  if (!is.numeric(status) || length(status) != 1 || is.na(status) || as.integer(status) != 0L) {
    stop(
      "run_prof.R failed in 2D profile at point ",
      pt$point_key[[1]],
      " (status=",
      status,
      ")."
    )
  }
  invisible(pt$point_id[[1]])
}

run_chain <- function(chain_pts, start_donor = NA_real_, use_init_override = FALSE) {
  donor <- start_donor
  out <- numeric(0)
  for (ii in seq_len(nrow(chain_pts))) {
    donor <- run_point(chain_pts[ii, , drop = FALSE], donor = donor, use_init_override = use_init_override && ii == 1L)
    out <- c(out, donor)
  }
  out
}

run_parallel_chains <- function(chain_list, mc_cores) {
  valid <- Filter(function(x) is.data.frame(x$pts) && nrow(x$pts) > 0, chain_list)
  if (length(valid) == 0) return(list())
  can_parallel <- .Platform$OS.type != "windows" && length(valid) > 1L && mc_cores > 1L
  if (!can_parallel) {
    return(lapply(valid, function(ch) run_chain(ch$pts, start_donor = ch$start_donor, use_init_override = FALSE)))
  }
  parallel::mclapply(
    valid,
    function(ch) run_chain(ch$pts, start_donor = ch$start_donor, use_init_override = FALSE),
    mc.cores = min(mc_cores, length(valid))
  )
}

find_point <- function(x_index, y_index) {
  out <- grid[grid$x_index == x_index & grid$y_index == y_index, , drop = FALSE]
  if (nrow(out) != 1L) stop("Expected exactly one grid point for x_index=", x_index, ", y_index=", y_index)
  out
}

build_chain_points <- function(x_indices, y_indices) {
  pts <- list()
  for (ix in x_indices) {
    for (iy in y_indices) {
      pts[[length(pts) + 1L]] <- find_point(ix, iy)
    }
  }
  if (length(pts) == 0) return(grid[FALSE, , drop = FALSE])
  do.call(rbind, pts)
}

if (identical(prof_2d_path, "axis_chains")) {
  anchor_x_target <- if (is.finite(prof_2d_anchor_x)) prof_2d_anchor_x else stats::median(x_vals)
  anchor_y_target <- if (is.finite(prof_2d_anchor_y)) prof_2d_anchor_y else stats::median(y_vals)
  anchor_x_index <- which.min(abs(x_vals - anchor_x_target))
  anchor_y_index <- which.min(abs(y_vals - anchor_y_target))
  anchor_pt <- find_point(anchor_x_index, anchor_y_index)
  cat(
    "axis_chains anchor:",
    params[[1]], "=", format(anchor_pt$x_value[[1]], scientific = FALSE, trim = TRUE),
    "|", params[[2]], "=", format(anchor_pt$y_value[[1]], scientific = FALSE, trim = TRUE), "\n"
  )

  anchor_donor <- if (is.finite(prof_2d_first_init_from)) prof_2d_first_init_from else NA_real_
  run_point(anchor_pt, donor = anchor_donor, use_init_override = TRUE)

  x_left <- if (anchor_x_index > 1L) {
    build_chain_points(rev(seq_len(anchor_x_index - 1L)), anchor_y_index)
  } else grid[FALSE, , drop = FALSE]
  x_right <- if (anchor_x_index < length(x_vals)) {
    build_chain_points(seq(anchor_x_index + 1L, length(x_vals)), anchor_y_index)
  } else grid[FALSE, , drop = FALSE]

  run_parallel_chains(
    list(
      list(pts = x_left, start_donor = anchor_pt$point_id[[1]]),
      list(pts = x_right, start_donor = anchor_pt$point_id[[1]])
    ),
    mc_cores = min(2L, prof_2d_parallel_jobs)
  )

  vertical_tasks <- list()
  for (ix in seq_along(x_vals)) {
    base_pt <- find_point(ix, anchor_y_index)
    y_down <- if (anchor_y_index > 1L) {
      build_chain_points(ix, rev(seq_len(anchor_y_index - 1L)))
    } else grid[FALSE, , drop = FALSE]
    y_up <- if (anchor_y_index < length(y_vals)) {
      build_chain_points(ix, seq(anchor_y_index + 1L, length(y_vals)))
    } else grid[FALSE, , drop = FALSE]
    vertical_tasks[[length(vertical_tasks) + 1L]] <- list(pts = y_down, start_donor = base_pt$point_id[[1]])
    vertical_tasks[[length(vertical_tasks) + 1L]] <- list(pts = y_up, start_donor = base_pt$point_id[[1]])
  }
  run_parallel_chains(vertical_tasks, mc_cores = prof_2d_parallel_jobs)
} else {
  prev_point_id <- if (is.finite(prof_2d_first_init_from)) prof_2d_first_init_from else NA_real_
  for (i in seq_len(nrow(grid))) {
    pt <- grid[i, , drop = FALSE]
    donor <- if (i == 1L) prev_point_id else grid$point_id[[i - 1L]]
    prev_point_id <- run_point(pt, donor = donor, use_init_override = (i == 1L))
  }
}

cat("\n✅ 2D indepvar profile completed\n")
