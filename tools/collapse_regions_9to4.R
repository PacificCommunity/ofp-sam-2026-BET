#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(FLR4MFCL))

REGION_MAP <- c(
  `1` = 1L,
  `2` = 1L,
  `3` = 3L,
  `4` = 3L,
  `5` = 4L,
  `6` = 4L,
  `7` = 2L,
  `8` = 3L,
  `9` = 4L
)

NEW_REGIONS <- sort(unique(unname(REGION_MAP)))
OLD_INDEX_FISHERY_IDS <- 33:41
NEW_INDEX_FISHERY_IDS <- 33:36
NON_INDEX_FISHERY_IDS <- 1:32
REDUCED_FISHERY_COUNT <- max(NEW_INDEX_FISHERY_IDS)
QTR_TO_MONTH <- c(`1` = 2L, `2` = 5L, `3` = 8L, `4` = 11L)
INDEX_COMP_SOURCE_MAPS <- list(
  representative = list(
    `33` = c(1L),
    `34` = c(7L),
    `35` = c(4L),
    `36` = c(11L)
  ),
  merge = list(
    `33` = c(1L, 2L),
    `34` = c(7L),
    `35` = c(4L, 9L, 8L),
    `36` = c(11L, 12L, 29L)
  )
)
INDEX_COMP_SOURCE_LABELS <- list(
  representative = c(
    `33` = "Fish 1",
    `34` = "Fish 7",
    `35` = "Fish 4",
    `36` = "Fish 11"
  ),
  merge = c(
    `33` = "Fish 1+2",
    `34` = "Fish 7",
    `35` = "Fish 4+9+8",
    `36` = "Fish 11+12+29"
  )
)
stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

print_usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript tools/collapse_regions_9to4.R --input-dir <dir> --output-dir <dir> [options]",
      "",
      "Options:",
      "  --frq-name <file>           Override the .frq file name inside the input directory",
      "  --ini-name <file>           Override the .ini file name inside the input directory",
      "  --tag-name <file>           Override the .tag file name inside the input directory",
      "  --index-csv <file>          Optional 4-region abundance-index CSV for new fisheries 33:36",
      "  --index-comp-mode <mode>    One of: representative, merge. Default: representative",
      "  --region-size-mode <mode>   One of: uniform, sum. Default: uniform",
      "  --overwrite                 Allow writing into an existing output directory",
      "  --help                      Show this message",
      "",
      "Mapping used by this script:",
      "  1 -> 1, 2 -> 1, 7 -> 2, 3 -> 3, 4 -> 3, 8 -> 3, 9 -> 4, 5 -> 4, 6 -> 4",
      "",
      "Example:",
      "  Rscript tools/collapse_regions_9to4.R \\",
      "    --input-dir mfcl/inputs/2023_rep \\",
      "    --output-dir mfcl/inputs/2023_4region \\",
      "    --index-comp-mode representative \\",
      "    --index-csv mfcl/bet.2023.indices.4-region.csv",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  opts <- list(
    input_dir = NULL,
    output_dir = NULL,
    frq_name = NULL,
    ini_name = NULL,
    tag_name = NULL,
    index_csv = NULL,
    index_comp_mode = "representative",
    region_size_mode = "uniform",
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
      stopf("Unexpected argument: %s", arg)
    }

    if (i == length(args)) {
      stopf("Missing value for %s", arg)
    }

    value <- args[[i + 1L]]

    if (arg == "--input-dir") {
      opts$input_dir <- value
    } else if (arg == "--output-dir") {
      opts$output_dir <- value
    } else if (arg == "--frq-name") {
      opts$frq_name <- value
    } else if (arg == "--ini-name") {
      opts$ini_name <- value
    } else if (arg == "--tag-name") {
      opts$tag_name <- value
    } else if (arg == "--index-csv") {
      opts$index_csv <- value
    } else if (arg == "--index-comp-mode") {
      opts$index_comp_mode <- value
    } else if (arg == "--region-size-mode") {
      opts$region_size_mode <- value
    } else {
      stopf("Unknown option: %s", arg)
    }

    i <- i + 2L
  }

  if (is.null(opts$input_dir) || is.null(opts$output_dir)) {
    print_usage()
    stopf("--input-dir and --output-dir are required")
  }

  opts$index_comp_mode <- match.arg(opts$index_comp_mode, c("representative", "merge"))
  opts$region_size_mode <- match.arg(opts$region_size_mode, c("uniform", "sum"))
  opts
}

load_index_csv <- function(path) {
  if (!file.exists(path)) {
    stopf("Index CSV not found: %s", path)
  }

  index_df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(index_df) <- trimws(names(index_df))
  required_cols <- c("Year", "Qtr", "Region", "Index", "CV")
  missing_cols <- setdiff(required_cols, names(index_df))
  if (length(missing_cols) > 0L) {
    stopf(
      "Index CSV is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    )
  }

  index_df$Year <- suppressWarnings(as.integer(index_df$Year))
  index_df$Qtr <- suppressWarnings(as.integer(index_df$Qtr))
  index_df$Region <- suppressWarnings(as.integer(index_df$Region))
  index_df$Index <- suppressWarnings(as.numeric(index_df$Index))
  index_df$CV <- suppressWarnings(as.numeric(index_df$CV))

  bad_rows <- is.na(index_df$Year) |
    is.na(index_df$Qtr) |
    is.na(index_df$Region) |
    is.na(index_df$Index) |
    is.na(index_df$CV)
  if (any(bad_rows)) {
    stopf("Index CSV contains missing or non-numeric values in required columns")
  }

  if (!all(index_df$Qtr %in% as.integer(names(QTR_TO_MONTH)))) {
    bad_qtr <- sort(unique(index_df$Qtr[!(index_df$Qtr %in% as.integer(names(QTR_TO_MONTH)))]))
    stopf("Index CSV contains unexpected quarter values: %s", paste(bad_qtr, collapse = ", "))
  }

  if (!all(index_df$Region %in% NEW_REGIONS)) {
    bad_regions <- sort(unique(index_df$Region[!(index_df$Region %in% NEW_REGIONS)]))
    stopf("Index CSV contains unexpected region values: %s", paste(bad_regions, collapse = ", "))
  }

  if (any(index_df$Index <= 0)) {
    stopf("Index CSV contains non-positive Index values")
  }

  if (any(index_df$CV <= 0)) {
    stopf("Index CSV contains non-positive CV values")
  }

  index_df$month <- unname(QTR_TO_MONTH[as.character(index_df$Qtr)])
  index_df$cv <- index_df$CV
  key_counts <- table(index_df$Year, index_df$month, index_df$Region)
  if (any(key_counts > 1L)) {
    stopf("Index CSV contains duplicate Year/Qtr(or month)/Region combinations")
  }

  index_df$effort <- 1 / index_df$Index
  index_df$penalty <- 0.5 / (index_df$CV ^ 2)
  index_df[, c("Year", "month", "Region", "effort", "penalty", "cv")]
}

detect_single_file <- function(input_dir, explicit_name, pattern, label) {
  if (!is.null(explicit_name)) {
    path <- file.path(input_dir, explicit_name)
    if (!file.exists(path)) {
      stopf("%s file not found: %s", label, path)
    }
    return(basename(path))
  }

  matches <- list.files(
    input_dir,
    pattern = pattern,
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  if (length(matches) == 0L) {
    stopf("Could not find a %s file in %s", label, input_dir)
  }

  if (length(matches) > 1L) {
    stopf(
      "Found multiple %s files in %s: %s. Please specify --%s-name",
      label,
      input_dir,
      paste(matches, collapse = ", "),
      label
    )
  }

  matches[[1L]]
}

map_regions <- function(values, context) {
  values_int <- suppressWarnings(as.integer(as.character(values)))
  invalid <- is.na(values_int) | !(values_int %in% as.integer(names(REGION_MAP)))
  if (any(invalid)) {
    bad <- unique(values[invalid])
    stopf("Unexpected region values in %s: %s", context, paste(bad, collapse = ", "))
  }
  unname(REGION_MAP[as.character(values_int)])
}

collapse_vector_by_region <- function(values, reducer = c("sum", "any", "first")) {
  reducer <- match.arg(reducer)
  out <- vapply(
    NEW_REGIONS,
    function(region_id) {
      slice <- values[unname(REGION_MAP) == region_id]
      if (reducer == "sum") {
        sum(slice, na.rm = TRUE)
      } else if (reducer == "any") {
        as.numeric(any(slice != 0, na.rm = TRUE))
      } else {
        slice[[1L]]
      }
    },
    numeric(1)
  )
  unname(out)
}

collapse_matrix_columns <- function(mat, reducer = c("any", "sum", "first")) {
  reducer <- match.arg(reducer)

  out <- sapply(
    NEW_REGIONS,
    function(region_id) {
      block <- mat[, unname(REGION_MAP) == region_id, drop = FALSE]
      if (reducer == "any") {
        apply(block, 1, function(x) as.numeric(any(x != 0, na.rm = TRUE)))
      } else if (reducer == "sum") {
        rowSums(block, na.rm = TRUE)
      } else {
        block[, 1L]
      }
    }
  )

  as.matrix(out)
}

collapse_region_size <- function(region_size, mode = c("uniform", "sum")) {
  mode <- match.arg(mode)
  if (mode == "uniform") {
    return(rep(1, length(NEW_REGIONS)))
  }
  collapse_vector_by_region(as.numeric(region_size), reducer = "sum")
}

make_region_size_flq <- function(template, values) {
  dn <- dimnames(template)
  dn$area <- as.character(seq_along(values))
  FLQuant(array(as.numeric(values), dim = c(1, 1, 1, 1, length(values), 1), dimnames = dn))
}

make_region_fish_flq <- function(template, values) {
  dn <- dimnames(template)
  dn$unit <- as.character(seq_along(values))
  FLQuant(array(as.numeric(values), dim = c(1, 1, length(values), 1, 1, 1), dimnames = dn))
}

refresh_frq_metadata <- function(frq) {
  frq@lf_range["Datasets"] <- nrow(unique(frq@freq[, c("year", "month", "week", "fishery")]))
  frq
}

make_index_comp_rows <- function(source_freq, new_fishery, index_lookup, frq_template, source_map) {
  source_ids <- source_map[[as.character(new_fishery)]]
  if (is.null(source_ids)) {
    stopf("No source fishery mapping defined for new index fishery %d", new_fishery)
  }

  region <- new_fishery - 32L
  wf_template <- data.frame(
    weight = seq(
      from = as.integer(frq_template@lf_range["WFFirst"]),
      by = as.integer(frq_template@lf_range["WFWidth"]),
      length.out = as.integer(frq_template@lf_range["WFIntervals"])
    )
  )

  index_region <- index_lookup[index_lookup$Region == region, c("Year", "month", "effort", "penalty")]
  names(index_region)[names(index_region) == "Year"] <- "year"
  index_region$week <- 1L

  source_wf <- source_freq[
    source_freq$fishery %in% source_ids &
      is.na(source_freq$length) &
      !is.na(source_freq$weight),
    c("year", "month", "week", "weight", "freq"),
    drop = FALSE
  ]

  comp_rows <- source_freq[0, , drop = FALSE]

  if (nrow(source_wf) > 0L) {
    source_wf$weight <- as.integer(source_wf$weight)
    wf_sum <- aggregate(freq ~ year + month + week + weight, data = source_wf, FUN = sum)
    wf_keys <- unique(wf_sum[, c("year", "month", "week"), drop = FALSE])
    wf_keys$key_id <- seq_len(nrow(wf_keys))
    full_grid <- merge(wf_keys, wf_template, by = NULL)
    full_grid <- full_grid[, c("year", "month", "week", "weight"), drop = FALSE]
    wf_full <- merge(full_grid, wf_sum, by = c("year", "month", "week", "weight"), all.x = TRUE, sort = FALSE)
    wf_full$freq[is.na(wf_full$freq)] <- 0
    wf_full <- merge(wf_full, index_region, by = c("year", "month", "week"), all.x = TRUE, sort = FALSE)

    if (anyNA(wf_full$effort) || anyNA(wf_full$penalty)) {
      stopf("Missing effort/penalty while rebuilding WF comps for index fishery %d", new_fishery)
    }

    comp_rows <- data.frame(
      year = wf_full$year,
      month = wf_full$month,
      week = wf_full$week,
      fishery = rep.int(new_fishery, nrow(wf_full)),
      catch = rep.int(1, nrow(wf_full)),
      effort = wf_full$effort,
      penalty = wf_full$penalty,
      length = rep(NA_real_, nrow(wf_full)),
      weight = wf_full$weight,
      freq = wf_full$freq,
      stringsAsFactors = FALSE
    )
  }

  wf_key_strings <- if (nrow(comp_rows) > 0L) {
    unique(paste(comp_rows$year, comp_rows$month, comp_rows$week, sep = "|"))
  } else {
    character(0)
  }

  ce_periods <- index_region[!(paste(index_region$year, index_region$month, index_region$week, sep = "|") %in% wf_key_strings), , drop = FALSE]
  ce_rows <- data.frame(
    year = ce_periods$year,
    month = ce_periods$month,
    week = ce_periods$week,
    fishery = rep.int(new_fishery, nrow(ce_periods)),
    catch = rep.int(1, nrow(ce_periods)),
    effort = ce_periods$effort,
    penalty = ce_periods$penalty,
    length = rep(NA_real_, nrow(ce_periods)),
    weight = rep(NA_real_, nrow(ce_periods)),
    freq = rep(-1, nrow(ce_periods)),
    stringsAsFactors = FALSE
  )

  out <- rbind(ce_rows, comp_rows)
  out[order(out$year, out$month, out$week, is.na(out$weight), out$weight), , drop = FALSE]
}

movement_pairs <- function(move_matrix) {
  n_regions <- nrow(move_matrix)
  pairs <- vector("list", 0L)

  for (i in seq_len(n_regions - 1L)) {
    for (j in seq.int(i + 1L, n_regions)) {
      value <- move_matrix[i, j]
      if (!is.na(value) && value != 0) {
        pairs[[length(pairs) + 1L]] <- c(i, j)
      }
    }
  }

  if (length(pairs) == 0L) {
    return(matrix(integer(0), nrow = 0, ncol = 2))
  }

  do.call(rbind, pairs)
}

directed_pairs <- function(undirected_pairs) {
  if (nrow(undirected_pairs) == 0L) {
    return(matrix(integer(0), nrow = 0, ncol = 2))
  }

  out <- matrix(NA_integer_, nrow = nrow(undirected_pairs) * 2L, ncol = 2L)
  cursor <- 1L

  for (row_idx in seq_len(nrow(undirected_pairs))) {
    from <- undirected_pairs[row_idx, 1L]
    to <- undirected_pairs[row_idx, 2L]
    out[cursor, ] <- c(from, to)
    out[cursor + 1L, ] <- c(to, from)
    cursor <- cursor + 2L
  }

  out
}

collapse_move_matrix <- function(old_move_matrix) {
  old_adj <- matrix(FALSE, nrow = nrow(old_move_matrix), ncol = ncol(old_move_matrix))
  upper_idx <- upper.tri(old_adj)
  old_adj[upper_idx] <- !is.na(old_move_matrix[upper_idx]) & old_move_matrix[upper_idx] != 0
  old_adj <- old_adj | t(old_adj)

  new_adj <- matrix(FALSE, nrow = length(NEW_REGIONS), ncol = length(NEW_REGIONS))

  for (from in seq_len(nrow(old_adj) - 1L)) {
    for (to in seq.int(from + 1L, ncol(old_adj))) {
      if (!old_adj[from, to]) {
        next
      }

      new_from <- REGION_MAP[[as.character(from)]]
      new_to <- REGION_MAP[[as.character(to)]]

      if (new_from == new_to) {
        next
      }

      new_adj[min(new_from, new_to), max(new_from, new_to)] <- TRUE
    }
  }

  out <- matrix(NA_real_, nrow = length(NEW_REGIONS), ncol = length(NEW_REGIONS))
  out[upper.tri(out)] <- as.numeric(new_adj[upper.tri(new_adj)])
  out
}

collapse_diff_coffs <- function(diff_coffs, old_move_matrix) {
  diff_matrix <- as.matrix(diff_coffs)
  old_pairs <- movement_pairs(old_move_matrix)
  old_directed <- directed_pairs(old_pairs)

  if (ncol(diff_matrix) != nrow(old_directed)) {
    stopf(
      "The diffusion matrix has %d columns but the 9-region movement matrix implies %d directed movements",
      ncol(diff_matrix),
      nrow(old_directed)
    )
  }

  new_move_matrix <- collapse_move_matrix(old_move_matrix)
  new_pairs <- movement_pairs(new_move_matrix)
  new_directed <- directed_pairs(new_pairs)

  out <- matrix(NA_real_, nrow = nrow(diff_matrix), ncol = nrow(new_directed))

  for (col_idx in seq_len(nrow(new_directed))) {
    from_region <- new_directed[col_idx, 1L]
    to_region <- new_directed[col_idx, 2L]

    contributing_cols <- which(
      REGION_MAP[as.character(old_directed[, 1L])] == from_region &
        REGION_MAP[as.character(old_directed[, 2L])] == to_region
    )

    if (length(contributing_cols) == 0L) {
      stopf("No source diffusion coefficients found for new movement %d -> %d", from_region, to_region)
    }

    out[, col_idx] <- rowMeans(diff_matrix[, contributing_cols, drop = FALSE], na.rm = TRUE)
  }

  out
}

prepare_output_dir <- function(output_dir, overwrite = FALSE) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    return(invisible(NULL))
  }

  if (!overwrite) {
    return(invisible(NULL))
  }

  existing <- list.files(output_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(existing) == 0L) {
    return(invisible(NULL))
  }

  removed <- unlink(existing, recursive = TRUE, force = TRUE)
  if (removed != 0L) {
    stopf("Failed to clean output directory before rebuild: %s", output_dir)
  }
}

copy_first_existing <- function(input_dir, output_dir, candidates, label, overwrite = FALSE) {
  candidates <- unique(candidates[nzchar(candidates)])
  if (length(candidates) == 0L) {
    return(invisible(FALSE))
  }

  existing <- candidates[file.exists(file.path(input_dir, candidates))]
  if (length(existing) == 0L) {
    return(invisible(FALSE))
  }

  if (length(existing) > 1L) {
    stopf(
      "Found multiple candidate %s files in %s: %s",
      label,
      input_dir,
      paste(existing, collapse = ", ")
    )
  }

  src <- file.path(input_dir, existing[[1L]])
  dst <- file.path(output_dir, basename(existing[[1L]]))
  copied <- file.copy(
    from = src,
    to = dst,
    overwrite = overwrite,
    recursive = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!copied) {
    stopf("Failed to copy %s file into %s: %s", label, output_dir, src)
  }

  invisible(TRUE)
}

copy_core_support_files <- function(input_dir, output_dir, frq_name, overwrite = FALSE) {
  age_length_name <- sub("\\.frq$", ".age_length", frq_name, ignore.case = TRUE)

  copy_first_existing(input_dir, output_dir, c("mfcl.cfg"), "mfcl.cfg", overwrite = overwrite)
  copy_first_existing(input_dir, output_dir, c(age_length_name), "age_length", overwrite = overwrite)
  copy_first_existing(input_dir, output_dir, c("fishery_map.R", "fishery_map.r"), "fishery_map", overwrite = overwrite)
  copy_first_existing(input_dir, output_dir, c("tag_rep_map.R", "tag_rep_map.r"), "tag_rep_map", overwrite = overwrite)
  copy_first_existing(input_dir, output_dir, c("doitall.sh"), "doitall", overwrite = overwrite)
}

collapse_frq <- function(frq, region_size_mode, reduce_index_fisheries = FALSE) {
  if (frq@n_regions != length(REGION_MAP)) {
    stopf("Expected a 9-region .frq file, found %d regions", frq@n_regions)
  }

  old_move_matrix <- frq@move_matrix
  old_region_size <- frq@region_size
  old_region_fish <- frq@region_fish

  mapped_region_fish <- map_regions(as.numeric(old_region_fish), "frq region_fish")
  if (reduce_index_fisheries) {
    mapped_region_fish <- c(mapped_region_fish[NON_INDEX_FISHERY_IDS], NEW_REGIONS)
  }

  frq@n_regions <- length(NEW_REGIONS)
  frq@region_size <- make_region_size_flq(
    old_region_size,
    collapse_region_size(as.numeric(old_region_size), mode = region_size_mode)
  )
  frq@region_fish <- make_region_fish_flq(old_region_fish, mapped_region_fish)
  frq@move_matrix <- collapse_move_matrix(old_move_matrix)
  frq@season_flags <- collapse_matrix_columns(frq@season_flags, reducer = "any")

  if (reduce_index_fisheries) {
    frq@n_fisheries <- REDUCED_FISHERY_COUNT
    frq@data_flags <- frq@data_flags[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
    frq@freq <- frq@freq[frq@freq$fishery <= REDUCED_FISHERY_COUNT, , drop = FALSE]
  }

  frq <- refresh_frq_metadata(frq)

  list(frq = frq, old_move_matrix = old_move_matrix)
}

rewrite_index_fisheries <- function(frq, source_frq, index_lookup, index_comp_mode) {
  if (frq@n_fisheries < REDUCED_FISHERY_COUNT) {
    stopf(
      "Expected at least %d fisheries in .frq to rewrite index fisheries, found %d",
      REDUCED_FISHERY_COUNT,
      frq@n_fisheries
    )
  }

  if (!any(source_frq@freq$fishery %in% OLD_INDEX_FISHERY_IDS)) {
    stopf("No original fisheries 33:41 found in the source .frq frequency table")
  }

  source_map <- INDEX_COMP_SOURCE_MAPS[[index_comp_mode]]
  needed_periods <- expand.grid(
    Region = NEW_REGIONS,
    year = sort(unique(index_lookup$Year)),
    month = unname(QTR_TO_MONTH),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  coverage <- merge(
    needed_periods,
    index_lookup,
    by.x = c("Region", "year", "month"),
    by.y = c("Region", "Year", "month"),
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  if (anyNA(coverage$effort) || anyNA(coverage$penalty)) {
    missing <- unique(coverage[is.na(coverage$effort) | is.na(coverage$penalty), c("Region", "year", "month")])
    stopf(
      "Index CSV is missing data for some 4-region year-month combinations, e.g. %s",
      paste(
        apply(utils::head(missing, 5L), 1, function(x) paste(x, collapse = "/")),
        collapse = ", "
      )
    )
  }

  base_rows <- frq@freq[frq@freq$fishery <= max(NON_INDEX_FISHERY_IDS), , drop = FALSE]
  rebuilt_index_rows <- do.call(
    rbind,
    lapply(
      NEW_INDEX_FISHERY_IDS,
      function(fishery_id) {
        make_index_comp_rows(
          source_frq@freq,
          fishery_id,
          index_lookup,
          frq,
          source_map = source_map
        )
      }
    )
  )

  frq@freq <- rbind(base_rows, rebuilt_index_rows)
  refresh_frq_metadata(frq)
}

collapse_ini <- function(ini, old_move_matrix, reduce_index_fisheries = FALSE) {
  if (ini@dimensions["regions"] != length(REGION_MAP)) {
    stopf("Expected a 9-region .ini file, found %d regions", ini@dimensions["regions"])
  }

  ini@dimensions["regions"] <- length(NEW_REGIONS)

  if (length(ini@region_flags) > 0L) {
    ini@region_flags <- collapse_matrix_columns(ini@region_flags, reducer = "any")
  } else {
    ini@region_flags <- matrix(0, nrow = 10, ncol = length(NEW_REGIONS))
  }

  ini@diff_coffs <- collapse_diff_coffs(ini@diff_coffs, old_move_matrix)
  ini@rec_dist <- collapse_vector_by_region(as.numeric(ini@rec_dist), reducer = "sum")
  ini@rec_dist <- ini@rec_dist / sum(ini@rec_dist)

  if (reduce_index_fisheries) {
    ini@dimensions["fisheries"] <- REDUCED_FISHERY_COUNT
    ini@tag_fish_rep_rate <- ini@tag_fish_rep_rate[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
    ini@tag_fish_rep_grp <- ini@tag_fish_rep_grp[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
    ini@tag_fish_rep_flags <- ini@tag_fish_rep_flags[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
    ini@tag_fish_rep_target <- ini@tag_fish_rep_target[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
    ini@tag_fish_rep_pen <- ini@tag_fish_rep_pen[, seq_len(REDUCED_FISHERY_COUNT), drop = FALSE]
  }

  ini
}

collapse_tag <- function(tag) {
  if (nrow(tag@releases) > 0L) {
    tag@releases$region <- map_regions(tag@releases$region, "tag releases")
  }

  if (nrow(tag@recaptures) > 0L) {
    tag@recaptures$region <- map_regions(tag@recaptures$region, "tag recaptures")
  }

  tag
}

rewrite_region_suffix_labels <- function(x) {
  x <- as.character(x)
  pattern <- "([.-])([1-9])(\\*?)$"

  vapply(
    x,
    function(label) {
      if (is.na(label) || !grepl(pattern, label)) {
        return(label)
      }

      suffix_match <- regmatches(label, regexec(pattern, label))[[1L]]
      old_region <- as.integer(suffix_match[[3L]])
      new_region <- map_regions(old_region, sprintf("label suffix in '%s'", label))
      sub(pattern, sprintf("\\1%d\\3", new_region), label)
    },
    character(1)
  )
}

rewrite_fishery_map <- function(path, reduce_index_fisheries = FALSE, index_comp_mode = "representative") {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  env <- new.env(parent = baseenv())
  sys.source(path, envir = env)
  if (!exists("fishery_map", envir = env, inherits = FALSE)) {
    return(invisible(FALSE))
  }

  fishery_map <- get("fishery_map", envir = env, inherits = FALSE)

  if (!("region" %in% names(fishery_map))) {
    stopf("fishery_map file does not contain a 'region' column: %s", path)
  }

  fishery_map$region <- map_regions(fishery_map$region, "fishery_map regions")
  if ("fishery_name" %in% names(fishery_map)) {
    fishery_map$fishery_name <- rewrite_region_suffix_labels(fishery_map$fishery_name)
  }
  if ("tag_recapture_name" %in% names(fishery_map)) {
    fishery_map$tag_recapture_name <- rewrite_region_suffix_labels(fishery_map$tag_recapture_name)
  }

  if (reduce_index_fisheries) {
    source_labels <- INDEX_COMP_SOURCE_LABELS[[index_comp_mode]]
    fishery_map <- fishery_map[fishery_map$fishery <= REDUCED_FISHERY_COUNT, , drop = FALSE]
    fishery_map$fishery <- seq_len(nrow(fishery_map))
    fishery_map$region[NEW_INDEX_FISHERY_IDS] <- NEW_REGIONS
    fishery_map$fishery_name[NEW_INDEX_FISHERY_IDS] <- sprintf(
      "%02d.Index R%d (%s)",
      NEW_INDEX_FISHERY_IDS,
      NEW_REGIONS,
      unname(source_labels[as.character(NEW_INDEX_FISHERY_IDS)])
    )
    fishery_map$group[NEW_INDEX_FISHERY_IDS] <- "Index"
    if ("tag_recapture_name" %in% names(fishery_map)) {
      fishery_map$tag_recapture_name[NEW_INDEX_FISHERY_IDS] <- fishery_map$fishery_name[NEW_INDEX_FISHERY_IDS]
    }
  }

  dump_lines <- capture.output(dput(fishery_map))
  dump_lines[1] <- paste0("fishery_map <- ", dump_lines[1])
  writeLines(c("# Fishery map", "", dump_lines), con = path, useBytes = TRUE)
  invisible(TRUE)
}

rewrite_tag_rep_map <- function(path) {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  env <- new.env(parent = baseenv())
  sys.source(path, envir = env)
  if (!exists("tag_rep_map", envir = env, inherits = FALSE)) {
    return(invisible(FALSE))
  }

  tag_rep_map <- get("tag_rep_map", envir = env, inherits = FALSE)
  if (!("tag_rep_name" %in% names(tag_rep_map))) {
    return(invisible(FALSE))
  }

  tag_rep_map$tag_rep_name <- rewrite_region_suffix_labels(tag_rep_map$tag_rep_name)

  dump_lines <- capture.output(dput(tag_rep_map))
  dump_lines[1] <- paste0("tag_rep_map <- ", dump_lines[1])
  writeLines(c("", dump_lines), con = path, useBytes = TRUE)
  invisible(TRUE)
}

compute_index_cpue_sigma_flags <- function(index_lookup = NULL) {
  if (is.null(index_lookup)) {
    stopf("Index CSV data are required to compute fish flag 92 from mean CV")
  }

  region_cv <- stats::aggregate(cv ~ Region, data = index_lookup, FUN = mean)
  missing_regions <- setdiff(NEW_REGIONS, region_cv$Region)
  if (length(missing_regions) > 0L) {
    stopf(
      "Index CSV is missing CV data for region(s): %s",
      paste(missing_regions, collapse = ", ")
    )
  }

  cv_by_region <- setNames(region_cv$cv, region_cv$Region)
  setNames(
    as.integer(round(unname(cv_by_region[as.character(NEW_REGIONS)]) * 100)),
    as.character(NEW_INDEX_FISHERY_IDS)
  )
}

rewrite_doitall <- function(path, reduce_index_fisheries = FALSE, index_lookup = NULL) {
  if (!file.exists(path) || !reduce_index_fisheries) {
    return(invisible(FALSE))
  }

  lines <- readLines(path, warn = FALSE)
  drop_line <- grepl("^\\s*-3[7-9]\\b", lines) | grepl("^\\s*-4[01]\\b", lines)
  lines <- lines[!drop_line]

  sigma_flags <- compute_index_cpue_sigma_flags(index_lookup = index_lookup)
  cpue_comment_idx <- grep("^# CPUE variation Index wt\\s+Time varying CV\\s*$", lines)
  if (length(cpue_comment_idx) == 1L) {
    lines[[cpue_comment_idx]] <- "# fish flag 92 = round(mean index CV * 100), fish flag 94 = allow unequal sigma, fish flag 66 = 0"
  }

  for (fishery_id in NEW_INDEX_FISHERY_IDS) {
    line_idx <- grep(sprintf("^\\s*-%d\\s+94\\b", fishery_id), lines)
    if (length(line_idx) != 1L) {
      stopf("Could not uniquely locate CPUE control line for fishery %d in %s", fishery_id, path)
    }

    lines[[line_idx]] <- sprintf(
      "  -%d 94 1       -%d 92 %d   -%d 66 0",
      fishery_id,
      fishery_id,
      sigma_flags[[as.character(fishery_id)]],
      fishery_id
    )
  }

  writeLines(lines, con = path, useBytes = TRUE)
  invisible(TRUE)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))

  input_dir <- normalizePath(opts$input_dir, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(opts$output_dir, winslash = "/", mustWork = FALSE)

  if (!dir.exists(input_dir)) {
    stopf("Input directory does not exist: %s", input_dir)
  }

  if (identical(input_dir, output_dir)) {
    stopf("Input and output directories must be different")
  }

  if (startsWith(output_dir, paste0(input_dir, "/"))) {
    stopf("Output directory cannot be nested inside the input directory")
  }

  if (dir.exists(output_dir) && !opts$overwrite) {
    existing <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0L) {
      stopf("Output directory already exists and is not empty: %s. Re-run with --overwrite to allow this.", output_dir)
    }
  }

  frq_name <- detect_single_file(input_dir, opts$frq_name, "\\.frq$", "frq")
  ini_name <- detect_single_file(input_dir, opts$ini_name, "\\.ini$", "ini")
  tag_name <- detect_single_file(input_dir, opts$tag_name, "\\.tag$", "tag")

  frq_in <- file.path(input_dir, frq_name)
  ini_in <- file.path(input_dir, ini_name)
  tag_in <- file.path(input_dir, tag_name)

  prepare_output_dir(output_dir, overwrite = opts$overwrite)
  copy_core_support_files(
    input_dir,
    output_dir,
    frq_name = frq_name,
    overwrite = TRUE
  )

  frq <- read.MFCLFrq(frq_in)
  ini <- read.MFCLIni(ini_in)
  tag <- read.MFCLTag(tag_in)
  index_lookup <- NULL
  if (!is.null(opts$index_csv)) {
    index_lookup <- load_index_csv(normalizePath(opts$index_csv, winslash = "/", mustWork = TRUE))
  }

  reduce_index_fisheries <- !is.null(index_lookup)
  frq_out <- collapse_frq(
    frq,
    region_size_mode = opts$region_size_mode,
    reduce_index_fisheries = reduce_index_fisheries
  )
  if (!is.null(index_lookup)) {
    frq_out$frq <- rewrite_index_fisheries(
      frq_out$frq,
      frq,
      index_lookup,
      opts$index_comp_mode
    )
  }
  ini_out <- collapse_ini(
    ini,
    old_move_matrix = frq_out$old_move_matrix,
    reduce_index_fisheries = reduce_index_fisheries
  )
  tag_out <- collapse_tag(tag)

  frq_path_out <- file.path(output_dir, frq_name)
  ini_path_out <- file.path(output_dir, ini_name)
  tag_path_out <- file.path(output_dir, tag_name)

  FLR4MFCL::write(frq_out$frq, file = frq_path_out)
  FLR4MFCL::write(ini_out, file = ini_path_out)
  FLR4MFCL::write(tag_out, file = tag_path_out)
  rewrite_fishery_map(
    file.path(output_dir, "fishery_map.R"),
    reduce_index_fisheries = reduce_index_fisheries,
    index_comp_mode = opts$index_comp_mode
  )
  rewrite_tag_rep_map(file.path(output_dir, "tag_rep_map.R"))
  rewrite_doitall(
    file.path(output_dir, "doitall.sh"),
    reduce_index_fisheries = reduce_index_fisheries,
    index_lookup = index_lookup
  )

  cat(
    paste(
      "Created 4-region MFCL inputs:",
      sprintf("  input:  %s", input_dir),
      sprintf("  output: %s", output_dir),
      sprintf("  frq:    %s", frq_path_out),
      sprintf("  ini:    %s", ini_path_out),
      sprintf("  tag:    %s", tag_path_out),
      if (!is.null(index_lookup)) sprintf("  index-csv: %s", normalizePath(opts$index_csv, winslash = "/", mustWork = TRUE)) else "  index-csv: <unchanged>",
      if (!is.null(index_lookup)) sprintf("  index-comp-mode: %s", opts$index_comp_mode) else "  index-comp-mode: <unchanged>",
      sprintf("  fisheries: %d", frq_out$frq@n_fisheries),
      sprintf("  region-size-mode: %s", opts$region_size_mode),
      sprintf("  new recruitment distribution: %s", paste(format(signif(ini_out@rec_dist, 8), trim = TRUE), collapse = " ")),
      sep = "\n"
    )
  )
}

if (sys.nframe() == 0L) {
  main()
}
