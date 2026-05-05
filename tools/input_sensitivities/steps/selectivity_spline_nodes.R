# ============================================================
# Create input sensitivities with fewer cubic spline nodes for all
# cubic-spline selectivity groups.
#
# This is intentionally a doitall.sh change, not a .par edit. The run creates
# 00.par from bet.ini and then applies flag overrides from doitall.sh.
# ============================================================

source("tools/input_sensitivities/helpers.R")
source("tools/input_change_metadata.R")

parse_int_list <- function(x) {
  vals <- unlist(strsplit(as.character(x), "[,[:space:]]+"))
  vals <- suppressWarnings(as.integer(vals[nzchar(vals)]))
  unique(vals[is.finite(vals) & vals > 0])
}

project_root <- getwd()
inputs_root <- env_or_default("inputs_root", "mfcl/inputs")
single_base_dir <- env_or_default("base_dir", "")
single_out_dir <- env_or_default("out_dir", "")
suffix <- env_or_default("suffix", "_sel_spline4")
node_count <- suppressWarnings(as.integer(env_or_default("node_count", "4")))
sel_fisheries_arg <- env_or_default("sel_fisheries", "")

if (!is.finite(node_count) || node_count <= 0) stop("node_count must be a positive integer")

dirs <- resolve_single_or_batch_dirs(
  inputs_root = inputs_root,
  base_dir = single_base_dir,
  out_dir = single_out_dir,
  suffix = suffix,
  project_root = project_root
)
source_dirs <- dirs$source_dirs
target_dirs <- dirs$target_dirs

flag_entries <- function(lines, flag_id) {
  pattern <- paste0("^([[:space:]]*)(-?[0-9]+)([[:space:]]+", flag_id, "[[:space:]]+)([0-9]+)(.*)$")
  m <- regexec(pattern, lines)
  hits <- regmatches(lines, m)
  rows <- lapply(seq_along(hits), function(idx) {
    h <- hits[[idx]]
    if (length(h) != 6L) return(NULL)
    fishery <- suppressWarnings(as.integer(h[[3]]))
    value <- suppressWarnings(as.integer(h[[5]]))
    if (!is.finite(fishery) || !is.finite(value)) return(NULL)
    data.frame(
      line = idx,
      prefix = paste0(h[[2]], h[[3]], h[[4]]),
      fishery = fishery,
      value = value,
      suffix = h[[6]],
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      line = integer(0),
      prefix = character(0),
      fishery = integer(0),
      value = integer(0),
      suffix = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

set_selectivity_nodes <- function(doitall_path, node_count, sel_fisheries = integer(0)) {
  lines <- readLines(doitall_path, warn = FALSE)

  start_marker <- "# Selectivity spline-node sensitivity overrides"
  end_marker <- "# End selectivity spline-node sensitivity overrides"
  if (any(lines == start_marker) && any(lines == end_marker)) {
    start <- which(lines == start_marker)[1]
    end <- which(lines == end_marker)[1]
    if (is.finite(start) && is.finite(end) && end >= start) {
      lines <- lines[-seq.int(start, end)]
    }
  }

  entries <- flag_entries(lines, 61)
  if (nrow(entries) == 0) stop("Could not find any fish flag 61 line in ", doitall_path)

  if (length(sel_fisheries) > 0) {
    entries <- entries[abs(entries$fishery) %in% abs(sel_fisheries), , drop = FALSE]
    if (nrow(entries) == 0) {
      stop(
        "No requested fish flag 61 entries found in ", doitall_path,
        ". Requested fisheries: ", paste(sel_fisheries, collapse = ", ")
      )
    }
  } else if (any(entries$fishery == -999L)) {
    entries <- entries[entries$fishery == -999L, , drop = FALSE]
  }

  entries$updated <- as.integer(node_count)
  for (idx in seq_len(nrow(entries))) {
    line_idx <- entries$line[[idx]]
    lines[[line_idx]] <- paste0(entries$prefix[[idx]], entries$updated[[idx]], entries$suffix[[idx]])
  }

  writeLines(lines, doitall_path, useBytes = TRUE)
  entries[, c("line", "fishery", "value", "updated"), drop = FALSE]
}

target_sel_fisheries <- parse_int_list(sel_fisheries_arg)

created <- character(0)
for (idx in seq_along(source_dirs)) {
  source_dir <- source_dirs[[idx]]
  target_dir <- target_dirs[[idx]]
  copy_input_dir(source_dir, target_dir)

  par_files <- list.files(target_dir, pattern = "\\.par$", full.names = TRUE)
  if (length(par_files) > 0) unlink(par_files, force = TRUE)

  doitall_path <- file.path(target_dir, "doitall.sh")
  if (!file.exists(doitall_path)) stop("No doitall.sh found in ", target_dir)
  sel_change <- set_selectivity_nodes(doitall_path, node_count, target_sel_fisheries)
  change_meta <- append_input_change_metadata(
    target_dir,
    token = paste0("sel", node_count),
    label = paste0(node_count, "-node selectivity splines"),
    operation = "selectivity_spline_nodes",
    source_dir = source_dir,
    details = list(
      node_count = node_count,
      target_fisheries = sel_change$fishery,
      original_flag_61 = stats::setNames(sel_change$value, as.character(sel_change$fishery)),
      updated_flag_61 = stats::setNames(sel_change$updated, as.character(sel_change$fishery))
    )
  )

  info <- list(
    source_dir = source_dir,
    created_at = Sys.time(),
    input_change_tokens = change_meta$tokens,
    target_fisheries = sel_change$fishery,
    flag_61_entries = sel_change,
    node_count = node_count,
    modified_file = "doitall.sh",
    par_files_removed = TRUE
  )
  saveRDS(info, file = file.path(target_dir, "selectivity_spline_nodes_info.rds"), compress = "xz")

  created <- c(created, target_dir)
}

cat("Created selectivity spline-node sensitivities:\n")
cat(paste0("  ", created, collapse = "\n"), "\n")
cat(
  "Target fish flag 61 fisheries:",
  if (length(target_sel_fisheries) > 0) paste(target_sel_fisheries, collapse = ", ") else "global -999 if present, otherwise all found",
  "\n"
)
cat("Node count:", node_count, "\n")
