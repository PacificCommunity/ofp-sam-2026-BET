#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(FLR4MFCL))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) == 0L) {
  stop("Could not determine script path", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)

source(file.path(script_dir, "collapse_regions_9to4.R"), local = TRUE)
source(file.path(script_dir, "retro.R"), local = TRUE)

PAR_INDEX_FISHERY_GROUPS <- c(
  as.list(as.integer(NON_INDEX_FISHERY_IDS)),
  list(33L, 39L, 35L, 37L)
)

stopf_par <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

print_usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript tools/collapse_par_9to4_representative.R --source-dir <dir> --target-dir <dir> --program-path <exe> [options]",
      "",
      "Options:",
      "  --source-par-name <file>    Source par file name inside source-dir (default: 11.par)",
      "  --output-par-name <file>    Output par file name inside target-dir (default: 11.par)",
      "  --template-par-name <file>  Template makepar output name to write into target-dir (default: 00.par)",
      "  --index-csv <file>          Optional 4-region abundance-index CSV; updates fish flag 92 using mean(CV)*100",
      "  --overwrite                 Allow overwriting an existing output par",
      "  --help                      Show this message",
      "",
      "This script collapses the 9-region representative-mode 11.par into the current 4-region representative setup.",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  opts <- list(
    source_dir = NULL,
    target_dir = NULL,
    program_path = NULL,
    source_par_name = "11.par",
    output_par_name = "11.par",
    template_par_name = "00.par",
    index_csv = NULL,
    overwrite = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("-h", "--help")) {
      print_usage()
      quit(save = "no", status = 0)
    }

    if (arg == "--overwrite") {
      opts$overwrite <- TRUE
      i <- i + 1L
      next
    }

    if (!startsWith(arg, "--")) {
      stopf_par("Unexpected argument: %s", arg)
    }

    if (i == length(args)) {
      stopf_par("Missing value for %s", arg)
    }

    value <- args[[i + 1L]]

    if (arg == "--source-dir") {
      opts$source_dir <- value
    } else if (arg == "--target-dir") {
      opts$target_dir <- value
    } else if (arg == "--program-path") {
      opts$program_path <- value
    } else if (arg == "--source-par-name") {
      opts$source_par_name <- value
    } else if (arg == "--output-par-name") {
      opts$output_par_name <- value
    } else if (arg == "--template-par-name") {
      opts$template_par_name <- value
    } else if (arg == "--index-csv") {
      opts$index_csv <- value
    } else {
      stopf_par("Unknown option: %s", arg)
    }

    i <- i + 2L
  }

  if (is.null(opts$source_dir) || is.null(opts$target_dir) || is.null(opts$program_path)) {
    print_usage()
    stopf_par("--source-dir, --target-dir and --program-path are required")
  }

  opts
}

reduce_numeric <- function(values, reducer = c("mean", "sum", "first", "any")) {
  reducer <- match.arg(reducer)
  if (reducer == "mean") {
    mean(values, na.rm = TRUE)
  } else if (reducer == "sum") {
    sum(values, na.rm = TRUE)
  } else if (reducer == "any") {
    as.numeric(any(values != 0, na.rm = TRUE))
  } else {
    values[[1L]]
  }
}

collapse_vector_groups <- function(x, groups, reducer = c("mean", "sum", "first", "any"), keep_last = FALSE) {
  reducer <- match.arg(reducer)
  out <- vapply(
    groups,
    function(idx) reduce_numeric(x[idx], reducer = reducer),
    numeric(1)
  )
  if (keep_last) {
    out <- c(out, x[[length(x)]])
  }
  out
}

collapse_array_margin <- function(arr, margin, groups, reducer = c("mean", "sum", "first", "any")) {
  reducer <- match.arg(reducer)
  arr <- as.array(arr)
  dims <- dim(arr)
  out_dims <- dims
  out_dims[[margin]] <- length(groups)
  dn <- dimnames(arr)
  if (!is.null(dn) && length(dn) >= margin) {
    dn[[margin]] <- as.character(seq_along(groups))
  }

  out <- array(0, dim = out_dims, dimnames = dn)
  other_margins <- setdiff(seq_along(dims), margin)

  for (i in seq_along(groups)) {
    idx <- rep(list(TRUE), length(dims))
    idx[[margin]] <- groups[[i]]
    block <- do.call(`[`, c(list(arr), idx, list(drop = FALSE)))
    reduced <- if (length(other_margins) == 0L) {
      reduce_numeric(as.numeric(block), reducer = reducer)
    } else {
      apply(block, other_margins, reduce_numeric, reducer = reducer)
    }
    out_idx <- rep(list(TRUE), length(dims))
    out_idx[[margin]] <- i
    out <- do.call(`[<-`, c(list(out), out_idx, list(value = reduced)))
  }

  out
}

collapse_flq_margin <- function(flq, margin, groups, reducer = c("mean", "sum", "first", "any")) {
  arr <- array(as.numeric(flq), dim = dim(flq), dimnames = dimnames(flq))
  FLQuant(collapse_array_margin(arr, margin = margin, groups = groups, reducer = reducer))
}

collapse_square_region_array <- function(arr, groups, reducer = c("mean", "sum", "first", "any")) {
  reducer <- match.arg(reducer)
  arr <- as.array(arr)
  dims <- dim(arr)
  out_dims <- dims
  out_dims[1:2] <- length(groups)
  dn <- dimnames(arr)
  if (!is.null(dn) && length(dn) >= 2L) {
    dn[[1L]] <- as.character(seq_along(groups))
    dn[[2L]] <- as.character(seq_along(groups))
  }
  out <- array(0, dim = out_dims, dimnames = dn)
  other_margins <- seq.int(3L, length(dims))

  for (i in seq_along(groups)) {
    for (j in seq_along(groups)) {
      idx <- rep(list(TRUE), length(dims))
      idx[[1L]] <- groups[[i]]
      idx[[2L]] <- groups[[j]]
      block <- do.call(`[`, c(list(arr), idx, list(drop = FALSE)))
      reduced <- apply(block, other_margins, reduce_numeric, reducer = reducer)
      out_idx <- rep(list(TRUE), length(dims))
      out_idx[[1L]] <- i
      out_idx[[2L]] <- j
      out <- do.call(`[<-`, c(list(out), out_idx, list(value = reduced)))
    }
  }

  out
}

collapse_pair_coffs <- function(pair_mat, old_move_matrix, directed = TRUE) {
  pair_mat <- as.matrix(pair_mat)
  old_pairs <- movement_pairs(old_move_matrix)
  pair_index <- if (directed) directed_pairs(old_pairs) else old_pairs

  if (ncol(pair_mat) != nrow(pair_index)) {
    stopf_par(
      "The diffusion matrix has %d columns but the 9-region movement matrix implies %d %s movements",
      ncol(pair_mat),
      nrow(pair_index),
      if (directed) "directed" else "undirected"
    )
  }

  new_move_matrix <- collapse_move_matrix(old_move_matrix)
  new_pairs <- movement_pairs(new_move_matrix)
  new_index <- if (directed) directed_pairs(new_pairs) else new_pairs

  out <- matrix(0, nrow = nrow(pair_mat), ncol = nrow(new_index))
  for (col_idx in seq_len(nrow(new_index))) {
    from_region <- new_index[col_idx, 1L]
    to_region <- new_index[col_idx, 2L]
    contributing_cols <- which(
      REGION_MAP[as.character(pair_index[, 1L])] == from_region &
        REGION_MAP[as.character(pair_index[, 2L])] == to_region
    )
    if (!directed) {
      contributing_cols <- which(
        (REGION_MAP[as.character(pair_index[, 1L])] == from_region &
           REGION_MAP[as.character(pair_index[, 2L])] == to_region) |
          (REGION_MAP[as.character(pair_index[, 1L])] == to_region &
             REGION_MAP[as.character(pair_index[, 2L])] == from_region)
      )
    }
    out[, col_idx] <- rowMeans(pair_mat[, contributing_cols, drop = FALSE], na.rm = TRUE)
  }

  out
}

collapse_list_groups <- function(lst, groups, reducer = c("mean", "sum", "first")) {
  reducer <- match.arg(reducer)
  lapply(groups, function(idx) {
    values <- lst[idx]
    if (length(values) == 1L || reducer == "first") {
      return(values[[1L]])
    }

    lengths <- lengths(values)
    if (length(unique(lengths)) != 1L) {
      return(values[[1L]])
    }

    stacked <- do.call(cbind, values)
    if (reducer == "sum") {
      rowSums(stacked, na.rm = TRUE)
    } else {
      rowMeans(stacked, na.rm = TRUE)
    }
  })
}

collapse_flag_matrix <- function(source_flags, template_flags) {
  out <- template_flags

  copy_exact_flagtypes <- c(1L, 2L, seq.int(-10117L, -10000L))
  for (ft in copy_exact_flagtypes) {
    out[out[, 1L] == ft, 3L] <- source_flags[source_flags[, 1L] == ft, 3L]
  }

  for (fishery_id in NON_INDEX_FISHERY_IDS) {
    out[out[, 1L] == -fishery_id, 3L] <- source_flags[source_flags[, 1L] == -fishery_id, 3L]
  }

  for (new_fishery in NEW_INDEX_FISHERY_IDS) {
    source_fishery <- PAR_INDEX_FISHERY_GROUPS[[new_fishery]][[1L]]
    out[out[, 1L] == -new_fishery, 3L] <- source_flags[source_flags[, 1L] == -source_fishery, 3L]
  }

  region_groups <- split(seq_len(length(REGION_MAP)), unname(REGION_MAP))
  region_flag_types <- sort(unique(template_flags[template_flags[, 1L] <= -100000L, 1L]))
  for (ft in region_flag_types) {
    source_rows <- source_flags[source_flags[, 1L] == ft, , drop = FALSE]
    target_rows <- out[out[, 1L] == ft, , drop = FALSE]
    for (new_region in seq_len(length(region_groups))) {
      source_vals <- source_rows[source_rows[, 2L] %in% region_groups[[new_region]], 3L]
      out[out[, 1L] == ft & out[, 2L] == new_region, 3L] <- reduce_numeric(source_vals, reducer = "any")
    }
  }

  out
}

apply_index_sigma_flags <- function(flag_matrix, index_lookup) {
  sigma_flags <- compute_index_cpue_sigma_flags(index_lookup = index_lookup)

  for (fishery_id in NEW_INDEX_FISHERY_IDS) {
    row_idx <- which(flag_matrix[, 1L] == -fishery_id & flag_matrix[, 2L] == 92L)
    if (length(row_idx) != 1L) {
      stopf_par("Could not uniquely locate fish flag 92 for fishery %d in output par", fishery_id)
    }
    flag_matrix[row_idx, 3L] <- sigma_flags[[as.character(fishery_id)]]
  }

  flag_matrix
}

build_template_par <- function(target_dir, program_path, out_template_path) {
  tmp_dir <- tempfile("mfcl_par_template_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  files_to_copy <- list.files(target_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(files_to_copy) > 0L) {
    ok <- file.copy(files_to_copy, tmp_dir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
    if (!all(ok)) {
      stopf_par("Failed to copy target files into temporary template directory")
    }
  }

  frq_name <- detect_single_file(tmp_dir, NULL, "\\.frq$", "frq")
  ini_name <- detect_single_file(tmp_dir, NULL, "\\.ini$", "ini")
  tag_name <- detect_single_file(tmp_dir, NULL, "\\.tag$", "tag")

  frq <- read.MFCLFrq(file.path(tmp_dir, frq_name))
  ini <- read.MFCLIni(file.path(tmp_dir, ini_name))
  tag <- read.MFCLTag(file.path(tmp_dir, tag_name))
  max_year <- as.integer(frq@range["maxyear"])
  mix_fixed <- retro.ini(ini, tag.obj = tag, max_year = max_year, n_mixing_periods = 2L)

  FLR4MFCL::write(mix_fixed$ini, file = file.path(tmp_dir, ini_name))
  FLR4MFCL::write(mix_fixed$tag, file = file.path(tmp_dir, tag_name))

  args <- c(frq_name, ini_name, "00.par", "-makepar")
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  result <- system2(program_path, args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (is.null(status)) {
    status <- 0L
  }

  template_path <- file.path(tmp_dir, "00.par")
  if (!file.exists(template_path)) {
    stopf_par("Failed to create template 00.par in temporary directory")
  }

  file.copy(template_path, out_template_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  read.MFCLPar(template_path)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))

  source_dir <- normalizePath(opts$source_dir, winslash = "/", mustWork = TRUE)
  target_dir <- normalizePath(opts$target_dir, winslash = "/", mustWork = TRUE)
  program_path <- normalizePath(opts$program_path, winslash = "/", mustWork = TRUE)

  source_par_path <- file.path(source_dir, opts$source_par_name)
  if (!file.exists(source_par_path)) {
    stopf_par("Source par file not found: %s", source_par_path)
  }

  out_par_path <- file.path(target_dir, opts$output_par_name)
  out_template_path <- file.path(target_dir, opts$template_par_name)
  if (file.exists(out_par_path) && !opts$overwrite) {
    stopf_par("Output par already exists: %s. Re-run with --overwrite.", out_par_path)
  }

  source_frq_name <- detect_single_file(source_dir, NULL, "\\.frq$", "frq")
  source_frq <- read.MFCLFrq(file.path(source_dir, source_frq_name))

  source_par <- read.MFCLPar(source_par_path)
  template_par <- build_template_par(target_dir, program_path, out_template_path)
  out_par <- template_par
  index_lookup <- NULL
  if (!is.null(opts$index_csv)) {
    index_lookup <- load_index_csv(normalizePath(opts$index_csv, winslash = "/", mustWork = TRUE))
  }

  excluded_slots <- c(
    "dimensions", "flags", "tag_fish_rep_rate", "tag_fish_rep_grp", "tag_fish_rep_flags",
    "tag_fish_rep_target", "tag_fish_rep_pen", "rel_ini_pop", "rec_standard", "rec_orthogonal",
    "control_flags", "diff_coffs", "xdiff_coffs", "y1diff_coffs", "y2diff_coffs", "zdiff_coffs",
    "diff_coffs_mat", "diff_coffs_age_period", "diff_coffs_age", "diff_coffs_nl",
    "diff_coffs_priors", "diff_coffs_age_priors", "diff_coffs_nl_priors", "region_rec_var",
    "region_pars", "fishery_sel", "fishery_sel_age_comp", "av_q_coffs", "ini_q_coffs",
    "q0_miss", "q_dev_coffs", "effort_dev_coffs", "rep_rate_dev_coffs", "sel_dev_corr",
    "sel_dev_coffs", "sel_dev_coffs2", "season_q_pars", "fish_params",
    "fm_level_regression_pars", "kludged_eq_coffs", "len_bias_pars",
    "common_len_bias_pars", "common_len_bias_coffs", "kludged_eq_level_coffs",
    "obj_fun", "n_pars", "tag_lik", "mn_len_pen", "max_grad", "av_fish_mort_inst",
    "av_fish_mort_year", "av_fish_mort_age", "lagrangian", "historic_flags"
  )

  for (slot_name in setdiff(slotNames(out_par), excluded_slots)) {
    src <- slot(source_par, slot_name)
    dst <- slot(out_par, slot_name)
    same_dim <- identical(dim(src), dim(dst))
    same_len <- is.null(dim(src)) && is.null(dim(dst)) && length(src) == length(dst)
    same_class <- identical(class(src), class(dst))
    if (same_class && (same_dim || same_len)) {
      slot(out_par, slot_name) <- src
    }
  }

  fish_groups <- PAR_INDEX_FISHERY_GROUPS
  region_groups <- split(seq_len(length(REGION_MAP)), unname(REGION_MAP))

  out_par@flags <- collapse_flag_matrix(source_par@flags, template_par@flags)
  if (!is.null(index_lookup)) {
    out_par@flags <- apply_index_sigma_flags(out_par@flags, index_lookup)
  }
  out_par@tag_fish_rep_rate <- collapse_array_margin(source_par@tag_fish_rep_rate, margin = 2L, groups = fish_groups, reducer = "first")
  out_par@tag_fish_rep_grp <- collapse_array_margin(source_par@tag_fish_rep_grp, margin = 2L, groups = fish_groups, reducer = "first")
  out_par@tag_fish_rep_flags <- collapse_array_margin(source_par@tag_fish_rep_flags, margin = 2L, groups = fish_groups, reducer = "first")
  out_par@tag_fish_rep_target <- collapse_array_margin(source_par@tag_fish_rep_target, margin = 2L, groups = fish_groups, reducer = "first")
  out_par@tag_fish_rep_pen <- collapse_array_margin(source_par@tag_fish_rep_pen, margin = 2L, groups = fish_groups, reducer = "first")

  out_par@rel_ini_pop <- template_par@rel_ini_pop
  out_par@rec_standard <- collapse_flq_margin(source_par@rec_standard, margin = 5L, groups = region_groups, reducer = "sum")
  out_par@rec_orthogonal <- collapse_flq_margin(source_par@rec_orthogonal, margin = 5L, groups = region_groups, reducer = "mean")
  out_par@control_flags <- collapse_array_margin(source_par@control_flags, margin = 2L, groups = region_groups, reducer = "any")
  out_par@diff_coffs <- collapse_diff_coffs(source_par@diff_coffs, source_frq@move_matrix)
  out_par@xdiff_coffs <- collapse_diff_coffs(source_par@xdiff_coffs, source_frq@move_matrix)
  out_par@y1diff_coffs <- collapse_pair_coffs(source_par@y1diff_coffs, source_frq@move_matrix, directed = FALSE)
  out_par@y2diff_coffs <- collapse_pair_coffs(source_par@y2diff_coffs, source_frq@move_matrix, directed = FALSE)
  out_par@zdiff_coffs <- collapse_diff_coffs(source_par@zdiff_coffs, source_frq@move_matrix)
  out_par@diff_coffs_mat <- template_par@diff_coffs_mat
  out_par@diff_coffs_age_period <- collapse_square_region_array(source_par@diff_coffs_age_period, groups = region_groups, reducer = "mean")
  out_par@diff_coffs_age <- collapse_diff_coffs(source_par@diff_coffs_age, source_frq@move_matrix)
  out_par@diff_coffs_nl <- collapse_diff_coffs(source_par@diff_coffs_nl, source_frq@move_matrix)
  out_par@diff_coffs_priors <- collapse_diff_coffs(source_par@diff_coffs_priors, source_frq@move_matrix)
  out_par@diff_coffs_age_priors <- collapse_diff_coffs(source_par@diff_coffs_age_priors, source_frq@move_matrix)
  out_par@diff_coffs_nl_priors <- collapse_diff_coffs(source_par@diff_coffs_nl_priors, source_frq@move_matrix)
  out_par@region_rec_var <- collapse_flq_margin(source_par@region_rec_var, margin = 5L, groups = region_groups, reducer = "mean")
  out_par@region_pars <- template_par@region_pars

  out_par@fishery_sel <- collapse_flq_margin(source_par@fishery_sel, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@fishery_sel_age_comp <- collapse_flq_margin(source_par@fishery_sel_age_comp, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@av_q_coffs <- collapse_flq_margin(source_par@av_q_coffs, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@ini_q_coffs <- collapse_flq_margin(source_par@ini_q_coffs, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@q0_miss <- collapse_flq_margin(source_par@q0_miss, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@q_dev_coffs <- collapse_list_groups(source_par@q_dev_coffs, groups = fish_groups, reducer = "mean")
  out_par@effort_dev_coffs <- collapse_list_groups(source_par@effort_dev_coffs, groups = fish_groups, reducer = "mean")
  out_par@rep_rate_dev_coffs <- collapse_list_groups(source_par@rep_rate_dev_coffs, groups = fish_groups, reducer = "mean")
  out_par@sel_dev_corr <- collapse_flq_margin(source_par@sel_dev_corr, margin = 3L, groups = fish_groups, reducer = "mean")
  out_par@sel_dev_coffs <- template_par@sel_dev_coffs
  out_par@sel_dev_coffs2 <- collapse_list_groups(source_par@sel_dev_coffs2, groups = fish_groups, reducer = "mean")
  out_par@season_q_pars <- collapse_array_margin(source_par@season_q_pars, margin = 1L, groups = fish_groups, reducer = "mean")
  out_par@fish_params <- collapse_array_margin(source_par@fish_params, margin = 2L, groups = fish_groups, reducer = "mean")
  out_par@fm_level_regression_pars <- collapse_array_margin(source_par@fm_level_regression_pars, margin = 1L, groups = fish_groups, reducer = "mean")
  out_par@kludged_eq_coffs <- template_par@kludged_eq_coffs
  out_par@len_bias_pars <- collapse_vector_groups(source_par@len_bias_pars, groups = fish_groups, reducer = "mean")
  out_par@common_len_bias_pars <- collapse_vector_groups(source_par@common_len_bias_pars, groups = fish_groups, reducer = "mean", keep_last = TRUE)
  out_par@common_len_bias_coffs <- collapse_vector_groups(source_par@common_len_bias_coffs, groups = fish_groups, reducer = "mean")
  out_par@kludged_eq_level_coffs <- template_par@kludged_eq_level_coffs

  out_par@dimensions <- template_par@dimensions
  out_par@n_pars <- template_par@n_pars

  FLR4MFCL::write(out_par, file = out_par_path)

  cat(
    paste(
      "Created representative-mode 4-region par files:",
      sprintf("  source-par:   %s", source_par_path),
      sprintf("  template-00:  %s", out_template_path),
      sprintf("  output-11par: %s", out_par_path),
      sprintf("  regions:      %d", out_par@dimensions["regions"]),
      sprintf("  fisheries:    %d", out_par@dimensions["fisheries"]),
      sep = "\n"
    )
  )
}

if (sys.nframe() == 0L) {
  main()
}
