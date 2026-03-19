# ============================================================
# Final: Tag program exclusion (RTTP / JPTP / PTTP)
#
# Behavior:
# - Exclude by tag program using tag.obj@releases$program
# - Preserve original release-group ORDER
# - Reindex tag rel.group to 1..N AFTER exclusion (required for writing .tag)
# - Keep kept_original_groups for correct INI row subsetting
# - Subset all INI *_rep_* slots with (taggrps + 1) rows
#   (keep last aggregate row)
# - Update FRQ n_tag_groups from updated INI taggrps
# - Write outputs with generic write()
# ============================================================

# -----------------------------
# Utilities
# -----------------------------

normalize_program <- function(x) {
  toupper(trimws(as.character(x)))
}

normalize_release_groups <- function(x) {
  vals <- suppressWarnings(as.integer(x))
  vals <- vals[!is.na(vals)]
  vals <- vals[vals > 0L]
  unique(vals)
}

# Compress grouping ids to remove holes while preserving 0 as "inactive/no-group".
# Example: c(1, 1, 3, 7, 0, 7) -> c(1, 1, 2, 3, 0, 3)
compact_grouping_vector <- function(x, keep_zero = TRUE) {
  if (!is.numeric(x)) stop("Grouping vector must be numeric/integer.")
  
  x_num <- as.numeric(x)
  finite_mask <- is.finite(x_num)
  if (any(finite_mask & (x_num != floor(x_num)))) {
    stop("Grouping vector contains non-integer values.")
  }
  
  out <- as.integer(x_num)
  pos_vals <- sort(unique(out[finite_mask & out > 0]))
  if (length(pos_vals) == 0) return(out)
  
  map <- setNames(seq_along(pos_vals), as.character(pos_vals))
  pos_idx <- which(finite_mask & out > 0)
  out[pos_idx] <- unname(map[as.character(out[pos_idx])])
  
  if (!keep_zero && any(out == 0, na.rm = TRUE)) {
    stop("Grouping vector contains 0 but keep_zero = FALSE.")
  }
  
  attr(out, "group_map") <- map
  out
}

compact_ini_tag_fish_rep_groups <- function(ini.obj) {
  grp <- ini.obj@tag_fish_rep_grp
  if (!is.matrix(grp)) stop("ini.obj@tag_fish_rep_grp must be a matrix.")
  
  compact_vec <- compact_grouping_vector(as.vector(grp), keep_zero = TRUE)
  compacted <- matrix(
    compact_vec,
    nrow = nrow(grp),
    ncol = ncol(grp),
    byrow = FALSE
  )
  
  ini.obj@tag_fish_rep_grp <- compacted
  attr(ini.obj, "tag_fish_rep_group_map") <- attr(compact_vec, "group_map")
  ini.obj
}

# Validate rel.group -> program is 1:1 in tag releases
assert_tag_program_mapping <- function(tag.obj) {
  rel <- tag.obj@releases
  
  if (!("rel.group" %in% names(rel))) stop("tag.obj@releases must contain 'rel.group'")
  if (!("program" %in% names(rel))) stop("tag.obj@releases must contain 'program'")
  
  rel_map <- unique(data.frame(
    rel.group = as.numeric(rel$rel.group),
    program = normalize_program(rel$program),
    stringsAsFactors = FALSE
  ))
  
  chk <- aggregate(program ~ rel.group, rel_map, function(z) length(unique(z)))
  if (any(chk$program != 1)) {
    bad <- chk$rel.group[chk$program != 1]
    stop(sprintf("Some rel.group values map to multiple programs: %s",
                 paste(bad, collapse = ", ")))
  }
  
  rel_map
}

# -----------------------------
# TAG exclusion
# - Keeps original group order
# - Reindexes surviving groups to 1..N for writing .tag
# - Returns kept_original_groups for INI row mapping
# -----------------------------

exclude_tag_programs <- function(tag.obj, exclude_programs) {
  exclude_programs <- normalize_program(exclude_programs)
  
  rel_map <- assert_tag_program_mapping(tag.obj)
  
  # Original release-group order (first appearance order)
  all_groups <- unique(as.numeric(rel_map$rel.group))
  program_by_group <- rel_map$program[match(all_groups, rel_map$rel.group)]
  
  drop_groups <- all_groups[program_by_group %in% exclude_programs]
  keep_groups <- all_groups[!(all_groups %in% drop_groups)]
  
  if (length(keep_groups) == 0) stop("All release groups would be removed.")
  
  rel <- tag.obj@releases
  rec <- tag.obj@recaptures
  rel$rel.group <- as.numeric(rel$rel.group)
  rec$rel.group <- as.numeric(rec$rel.group)
  
  keep_releases <- rel[rel$rel.group %in% keep_groups, , drop = FALSE]
  keep_recaps   <- rec[rec$rel.group %in% keep_groups, , drop = FALSE]
  
  # Reindex to 1..N, preserving original keep_groups order
  new_ids <- setNames(seq_along(keep_groups), as.character(keep_groups))
  keep_releases$rel.group <- unname(new_ids[as.character(keep_releases$rel.group)])
  keep_recaps$rel.group   <- unname(new_ids[as.character(keep_recaps$rel.group)])
  
  tag.obj@releases <- keep_releases
  tag.obj@recaptures <- keep_recaps
  tag.obj@release_groups <- length(keep_groups)
  tag.obj@recoveries <- tabulate(as.numeric(keep_recaps$rel.group), nbins = tag.obj@release_groups)
  
  list(
    tag = tag.obj,
    kept_original_groups = as.numeric(keep_groups),   # for INI row subsetting
    dropped_original_groups = as.numeric(drop_groups),
    excluded_programs = exclude_programs,
    original_release_groups = length(all_groups)
  )
}

exclude_tag_release_groups <- function(tag.obj, exclude_release_groups) {
  drop_groups <- normalize_release_groups(exclude_release_groups)
  if (length(drop_groups) == 0) {
    stop("No valid release-group ids were supplied.")
  }

  rel <- tag.obj@releases
  rec <- tag.obj@recaptures
  rel$rel.group <- as.numeric(rel$rel.group)
  rec$rel.group <- as.numeric(rec$rel.group)

  all_groups <- sort(unique(rel$rel.group))
  missing_groups <- setdiff(drop_groups, all_groups)
  if (length(missing_groups) > 0) {
    stop(sprintf(
      "Requested release-group ids not found in tag releases: %s",
      paste(missing_groups, collapse = ", ")
    ))
  }

  keep_groups <- all_groups[!(all_groups %in% drop_groups)]
  if (length(keep_groups) == 0) stop("All release groups would be removed.")

  keep_releases <- rel[rel$rel.group %in% keep_groups, , drop = FALSE]
  keep_recaps   <- rec[rec$rel.group %in% keep_groups, , drop = FALSE]

  new_ids <- setNames(seq_along(keep_groups), as.character(keep_groups))
  keep_releases$rel.group <- unname(new_ids[as.character(keep_releases$rel.group)])
  keep_recaps$rel.group   <- unname(new_ids[as.character(keep_recaps$rel.group)])

  tag.obj@releases <- keep_releases
  tag.obj@recaptures <- keep_recaps
  tag.obj@release_groups <- length(keep_groups)
  tag.obj@recoveries <- tabulate(as.numeric(keep_recaps$rel.group), nbins = tag.obj@release_groups)

  list(
    tag = tag.obj,
    kept_original_groups = as.numeric(keep_groups),
    dropped_original_groups = as.numeric(drop_groups),
    excluded_release_groups = as.numeric(drop_groups),
    original_release_groups = length(all_groups)
  )
}

# -----------------------------
# INI helper: subset all *_rep_* slots by keep_rows
# Applies when first dimension / nrow == old_taggrps + 1
# -----------------------------

subset_ini_rep_rows <- function(ini.obj, keep_rows, old_taggrps) {
  sn <- methods::slotNames(ini.obj)
  rep_slots <- sn[grepl("_rep_", sn)]
  touched <- character(0)
  
  for (nm in rep_slots) {
    x <- methods::slot(ini.obj, nm)
    
    if (is.matrix(x) && nrow(x) == (old_taggrps + 1)) {
      methods::slot(ini.obj, nm) <- x[keep_rows, , drop = FALSE]
      touched <- c(touched, nm)
      next
    }
    
    if (is.data.frame(x) && nrow(x) == (old_taggrps + 1)) {
      methods::slot(ini.obj, nm) <- x[keep_rows, , drop = FALSE]
      touched <- c(touched, nm)
      next
    }
    
    if (is.array(x) && length(dim(x)) >= 1 && dim(x)[1] == (old_taggrps + 1)) {
      idx <- vector("list", length(dim(x)))
      idx[[1]] <- keep_rows
      if (length(dim(x)) > 1) {
        for (i in 2:length(dim(x))) idx[[i]] <- seq_len(dim(x)[i])
      }
      methods::slot(ini.obj, nm) <- do.call(`[`, c(list(x), idx, list(drop = FALSE)))
      touched <- c(touched, nm)
      next
    }
    
    if (is.atomic(x) && is.null(dim(x)) && length(x) == (old_taggrps + 1)) {
      methods::slot(ini.obj, nm) <- x[keep_rows]
      touched <- c(touched, nm)
      next
    }
  }
  
  attr(ini.obj, "rep_slots_touched") <- touched
  ini.obj
}

# -----------------------------
# INI exclusion
# - Uses kept_original_groups to subset rows in *_rep_* slots
# - Keeps last aggregate row (+1 row)
# - Updates dimensions["taggrps"] to active kept group count
# -----------------------------

exclude_tag_programs_ini <- function(ini.obj, tag.obj, exclude_programs) {
  tag_res <- exclude_tag_programs(tag.obj, exclude_programs)
  
  keep_groups_original <- as.numeric(tag_res$kept_original_groups)
  old_taggrps <- as.numeric(ini.obj@dimensions["taggrps"])
  
  n_rows_rate <- nrow(ini.obj@tag_fish_rep_rate)
  if (!isTRUE(n_rows_rate == old_taggrps + 1)) {
    stop(sprintf(
      "Unexpected tag_fish_rep_rate rows: %d (expected %d = taggrps + 1)",
      n_rows_rate, old_taggrps + 1
    ))
  }
  
  aggregate_row <- n_rows_rate
  keep_rows <- c(keep_groups_original, aggregate_row)
  
  ini.obj <- subset_ini_rep_rows(ini.obj, keep_rows = keep_rows, old_taggrps = old_taggrps)
  
  # After row subsetting, group ids can have holes (e.g., 1,2,3,16,...).
  # MFCL expects grouping ids to be contiguous.
  ini.obj <- compact_ini_tag_fish_rep_groups(ini.obj)
  
  # Active tag groups after exclusion (for INI and FRQ)
  ini.obj@dimensions["taggrps"] <- length(keep_groups_original)
  
  # Core checks
  stopifnot(
    nrow(ini.obj@tag_fish_rep_rate)   == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_grp)    == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_flags)  == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_target) == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_pen)    == as.numeric(ini.obj@dimensions["taggrps"]) + 1
  )
  
  list(
    ini = ini.obj,
    tag = tag_res$tag,
    kept_original_groups = keep_groups_original,
    dropped_original_groups = as.numeric(tag_res$dropped_original_groups),
    excluded_programs = tag_res$excluded_programs,
    original_release_groups = tag_res$original_release_groups
  )
}

exclude_tag_release_groups_ini <- function(ini.obj, tag.obj, exclude_release_groups) {
  tag_res <- exclude_tag_release_groups(tag.obj, exclude_release_groups)

  keep_groups_original <- as.numeric(tag_res$kept_original_groups)
  old_taggrps <- as.numeric(ini.obj@dimensions["taggrps"])

  n_rows_rate <- nrow(ini.obj@tag_fish_rep_rate)
  if (!isTRUE(n_rows_rate == old_taggrps + 1)) {
    stop(sprintf(
      "Unexpected tag_fish_rep_rate rows: %d (expected %d = taggrps + 1)",
      n_rows_rate, old_taggrps + 1
    ))
  }

  aggregate_row <- n_rows_rate
  keep_rows <- c(keep_groups_original, aggregate_row)

  ini.obj <- subset_ini_rep_rows(ini.obj, keep_rows = keep_rows, old_taggrps = old_taggrps)
  ini.obj <- compact_ini_tag_fish_rep_groups(ini.obj)
  ini.obj@dimensions["taggrps"] <- length(keep_groups_original)

  stopifnot(
    nrow(ini.obj@tag_fish_rep_rate)   == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_grp)    == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_flags)  == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_target) == as.numeric(ini.obj@dimensions["taggrps"]) + 1,
    nrow(ini.obj@tag_fish_rep_pen)    == as.numeric(ini.obj@dimensions["taggrps"]) + 1
  )

  list(
    ini = ini.obj,
    tag = tag_res$tag,
    kept_original_groups = keep_groups_original,
    dropped_original_groups = as.numeric(tag_res$dropped_original_groups),
    excluded_release_groups = as.numeric(tag_res$excluded_release_groups),
    original_release_groups = tag_res$original_release_groups
  )
}

# -----------------------------
# FRQ exclusion (program exclusion version)
# - Keep frq@freq unchanged
# - Update n_tag_groups from updated ini taggrps (preferred)
# -----------------------------

exclude.frq <- function(frq.obj, exclude.ini.obj = NULL, exclude.tag.obj = NULL) {
  if (!is.null(exclude.ini.obj)) {
    frq.obj@n_tag_groups <- as.numeric(exclude.ini.obj@dimensions["taggrps"])
  } else if (!is.null(exclude.tag.obj)) {
    frq.obj@n_tag_groups <- as.numeric(exclude.tag.obj@release_groups)
  } else {
    stop("Need exclude.ini.obj or exclude.tag.obj to update frq n_tag_groups.")
  }
  frq.obj
}

# -----------------------------
# Build one exclusion case
# -----------------------------

exclude_one_program_case <- function(ini.obj, tag.obj, frq.obj = NULL, exclude_program) {
  res <- exclude_tag_programs_ini(
    ini.obj = ini.obj,
    tag.obj = tag.obj,
    exclude_programs = exclude_program
  )
  
  out <- list(
    ini = res$ini,
    tag = res$tag,
    kept_original_groups = res$kept_original_groups,
    dropped_original_groups = res$dropped_original_groups,
    excluded_programs = res$excluded_programs,
    original_release_groups = res$original_release_groups
  )
  
  if (!is.null(frq.obj)) {
    out$frq <- exclude.frq(
      frq.obj = frq.obj,
      exclude.ini.obj = res$ini,
      exclude.tag.obj = res$tag
    )
  }
  
  out
}

exclude_one_release_group_case <- function(ini.obj, tag.obj, frq.obj = NULL, exclude_release_groups) {
  res <- exclude_tag_release_groups_ini(
    ini.obj = ini.obj,
    tag.obj = tag.obj,
    exclude_release_groups = exclude_release_groups
  )

  out <- list(
    ini = res$ini,
    tag = res$tag,
    kept_original_groups = res$kept_original_groups,
    dropped_original_groups = res$dropped_original_groups,
    excluded_release_groups = res$excluded_release_groups,
    original_release_groups = res$original_release_groups
  )

  if (!is.null(frq.obj)) {
    out$frq <- exclude.frq(
      frq.obj = frq.obj,
      exclude.ini.obj = res$ini,
      exclude.tag.obj = res$tag
    )
  }

  out
}

# -----------------------------
# Build multiple exclusion cases
# -----------------------------

make_program_exclusion_cases <- function(ini.obj, tag.obj, frq.obj = NULL,
                                         programs = c("RTTP", "JPTP", "PTTP")) {
  programs <- normalize_program(programs)
  out <- setNames(vector("list", length(programs)), paste0("exclude_", programs))
  
  for (i in seq_along(programs)) {
    out[[i]] <- exclude_one_program_case(
      ini.obj = ini.obj,
      tag.obj = tag.obj,
      frq.obj = frq.obj,
      exclude_program = programs[i]
    )
  }
  
  out
}

make_release_group_exclusion_case <- function(ini.obj, tag.obj, frq.obj = NULL,
                                              release_groups) {
  release_groups <- normalize_release_groups(release_groups)
  if (length(release_groups) == 0) stop("release_groups is empty")

  nm <- paste0("exclude_rg_", paste(release_groups, collapse = "_"))
  out <- vector("list", 1L)
  out[[1]] <- exclude_one_release_group_case(
    ini.obj = ini.obj,
    tag.obj = tag.obj,
    frq.obj = frq.obj,
    exclude_release_groups = release_groups
  )
  names(out) <- nm
  out
}

# -----------------------------
# Diagnostics / checks
# -----------------------------

check_program_exclusion <- function(case_obj, excluded_program) {
  excluded_program <- normalize_program(excluded_program)
  
  remaining_programs <- sort(unique(normalize_program(case_obj$tag@releases$program)))
  rel_groups_release <- sort(unique(as.numeric(case_obj$tag@releases$rel.group)))
  rel_groups_recap   <- sort(unique(as.numeric(case_obj$tag@recaptures$rel.group)))
  
  ini_taggrps <- as.numeric(case_obj$ini@dimensions["taggrps"])
  ini_rows <- nrow(case_obj$ini@tag_fish_rep_rate)
  grp_vals <- sort(unique(as.integer(case_obj$ini@tag_fish_rep_grp)))
  grp_pos <- grp_vals[grp_vals > 0]
  grp_holes <- if (length(grp_pos)) setdiff(seq_len(max(grp_pos)), grp_pos) else integer(0)
  
  cat("--------------------------------------------------\n")
  cat("Excluded:", excluded_program, "\n")
  cat("Remaining programs:", paste(remaining_programs, collapse = ", "), "\n")
  cat("Original release group count:", case_obj$original_release_groups, "\n")
  cat("Kept original groups:", length(case_obj$kept_original_groups), "\n")
  cat("Dropped original groups:", length(case_obj$dropped_original_groups), "\n")
  cat("Tag release_groups (new count for .tag header):", case_obj$tag@release_groups, "\n")
  cat("INI taggrps (active count):", ini_taggrps, "\n")
  cat("INI rows(tag_fish_rep_rate):", ini_rows, "\n")
  cat("INI tag_fish_rep_grp unique positive ids:", length(grp_pos),
      if (length(grp_holes)) paste0(" | holes=", paste(grp_holes, collapse = ",")) else " | holes=none",
      "\n")
  if (!is.null(case_obj$frq)) cat("FRQ n_tag_groups:", case_obj$frq@n_tag_groups, "\n")
  
  expected_groups <- seq_len(case_obj$tag@release_groups)
  
  stopifnot(
    !(excluded_program %in% remaining_programs),
    case_obj$tag@release_groups == length(case_obj$kept_original_groups),
    length(rel_groups_release) == 0 || all(as.integer(rel_groups_release) == expected_groups),
    length(rel_groups_recap) == 0 || max(rel_groups_recap) <= case_obj$tag@release_groups,
    ini_rows == ini_taggrps + 1,
    length(grp_holes) == 0
  )
  
  if (!is.null(case_obj$frq)) {
    stopifnot(as.numeric(case_obj$frq@n_tag_groups) == ini_taggrps)
  }
  
  invisible(TRUE)
}

summarize_program_exclusion_cases <- function(cases) {
  do.call(rbind, lapply(names(cases), function(nm) {
    x <- cases[[nm]]
    data.frame(
      case = nm,
      excluded = paste(x$excluded_programs, collapse = ","),
      original_release_groups = x$original_release_groups,
      kept_groups = length(x$kept_original_groups),
      dropped_groups = length(x$dropped_original_groups),
      tag_release_groups = x$tag@release_groups,
      ini_taggrps = as.numeric(x$ini@dimensions["taggrps"]),
      ini_rows_tag_fish_rep_rate = nrow(x$ini@tag_fish_rep_rate),
      frq_n_tag_groups = if (!is.null(x$frq)) as.numeric(x$frq@n_tag_groups) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

check_release_group_exclusion <- function(case_obj, excluded_release_groups) {
  excluded_release_groups <- normalize_release_groups(excluded_release_groups)

  remaining_release_groups <- sort(unique(as.numeric(case_obj$tag@releases$rel.group)))
  rel_groups_recap <- sort(unique(as.numeric(case_obj$tag@recaptures$rel.group)))

  ini_taggrps <- as.numeric(case_obj$ini@dimensions["taggrps"])
  ini_rows <- nrow(case_obj$ini@tag_fish_rep_rate)
  grp_vals <- sort(unique(as.integer(case_obj$ini@tag_fish_rep_grp)))
  grp_pos <- grp_vals[grp_vals > 0]
  grp_holes <- if (length(grp_pos)) setdiff(seq_len(max(grp_pos)), grp_pos) else integer(0)

  cat("--------------------------------------------------\n")
  cat("Excluded release groups:", paste(excluded_release_groups, collapse = ", "), "\n")
  cat("Original release group count:", case_obj$original_release_groups, "\n")
  cat("Kept original groups:", length(case_obj$kept_original_groups), "\n")
  cat("Dropped original groups:", length(case_obj$dropped_original_groups), "\n")
  cat("Tag release_groups (new count for .tag header):", case_obj$tag@release_groups, "\n")
  cat("INI taggrps (active count):", ini_taggrps, "\n")
  cat("INI rows(tag_fish_rep_rate):", ini_rows, "\n")
  cat("INI tag_fish_rep_grp unique positive ids:", length(grp_pos),
      if (length(grp_holes)) paste0(" | holes=", paste(grp_holes, collapse = ",")) else " | holes=none",
      "\n")
  if (!is.null(case_obj$frq)) cat("FRQ n_tag_groups:", case_obj$frq@n_tag_groups, "\n")

  expected_groups <- seq_len(case_obj$tag@release_groups)

  stopifnot(
    !any(excluded_release_groups %in% case_obj$kept_original_groups),
    case_obj$tag@release_groups == length(case_obj$kept_original_groups),
    length(remaining_release_groups) == 0 || all(as.integer(remaining_release_groups) == expected_groups),
    length(rel_groups_recap) == 0 || max(rel_groups_recap) <= case_obj$tag@release_groups,
    ini_rows == ini_taggrps + 1,
    length(grp_holes) == 0
  )

  if (!is.null(case_obj$frq)) {
    stopifnot(as.numeric(case_obj$frq@n_tag_groups) == ini_taggrps)
  }

  invisible(TRUE)
}

summarize_release_group_exclusion_cases <- function(cases) {
  do.call(rbind, lapply(names(cases), function(nm) {
    x <- cases[[nm]]
    data.frame(
      case = nm,
      excluded = paste(x$excluded_release_groups, collapse = ","),
      original_release_groups = x$original_release_groups,
      kept_groups = length(x$kept_original_groups),
      dropped_groups = length(x$dropped_original_groups),
      tag_release_groups = x$tag@release_groups,
      ini_taggrps = as.numeric(x$ini@dimensions["taggrps"]),
      ini_rows_tag_fish_rep_rate = nrow(x$ini@tag_fish_rep_rate),
      frq_n_tag_groups = if (!is.null(x$frq)) as.numeric(x$frq@n_tag_groups) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

# -----------------------------
# Write cases to separate folders (generic write())
# -----------------------------

write_program_exclusion_cases <- function(cases,
                                          out_dir = "tag_program_exclusions",
                                          ini_name = "test.ini",
                                          tag_name = "tag",
                                          frq_name = "test.frq",
                                          validate_before_write = TRUE) {
  if (is.null(cases) || length(cases) == 0) stop("cases is empty")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (nm in names(cases)) {
    x <- cases[[nm]]
    case_dir <- file.path(out_dir, nm)
    dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (validate_before_write) {
      check_program_exclusion(x, sub("^exclude_", "", nm))
    }
    
    write(x$ini, file.path(case_dir, ini_name))
    write(x$tag, file.path(case_dir, tag_name))
    if (!is.null(x$frq)) write(x$frq, file.path(case_dir, frq_name))
    
    cat(sprintf(
      "[%s] written -> %s | tag release_groups=%d | ini taggrps=%d | frq n_tag_groups=%s\n",
      nm, case_dir,
      x$tag@release_groups,
      as.numeric(x$ini@dimensions["taggrps"]),
      if (!is.null(x$frq)) as.character(x$frq@n_tag_groups) else "NA"
    ))
  }
  
  invisible(TRUE)
}
