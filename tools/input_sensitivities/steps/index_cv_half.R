# ============================================================
# Create input sensitivities with index CPUE CV flags halved.
#
# This edits doitall.sh only. By default it finds fishery-specific index CV
# flag 92 entries in the selected base input and halves those values. These
# are the negative fishery rows such as "-33 92 49". Positive rows such as
# "2 92 2" are MFCL options and must not be changed. Set index_fisheries to
# a comma/space separated list if a base model needs an explicit subset.
# ============================================================

source("tools/input_sensitivities/helpers.R")
source("tools/input_change_metadata.R")

project_root <- getwd()
inputs_root <- env_or_default("inputs_root", "mfcl/inputs")
single_base_dir <- env_or_default("base_dir", "")
single_out_dir <- env_or_default("out_dir", "")
suffix <- env_or_default("suffix", "_index_cv_half")
index_fisheries_arg <- env_or_default("index_fisheries", "")

dirs <- resolve_single_or_batch_dirs(
  inputs_root = inputs_root,
  base_dir = single_base_dir,
  out_dir = single_out_dir,
  suffix = suffix,
  project_root = project_root
)
source_dirs <- dirs$source_dirs
target_dirs <- dirs$target_dirs

parse_index_fisheries <- function(x) {
  vals <- unlist(strsplit(as.character(x), "[,[:space:]]+"), use.names = FALSE)
  vals <- suppressWarnings(as.integer(vals[nzchar(vals)]))
  unique(vals[is.finite(vals)])
}

flag92_entries <- function(lines) {
  pattern <- "(^|[[:space:]])(-?[0-9]+)([[:space:]]+92[[:space:]]+)([0-9]+)"
  rows <- list()
  for (idx in seq_along(lines)) {
    matches <- gregexpr(pattern, lines[[idx]], perl = TRUE)[[1]]
    if (identical(matches[[1]], -1L)) next
    lengths <- attr(matches, "match.length")
    texts <- substring(lines[[idx]], matches, matches + lengths - 1L)
    line_rows <- lapply(seq_along(texts), function(match_idx) {
      h <- regmatches(
        texts[[match_idx]],
        regexec("^([[:space:]]*)(-?[0-9]+)([[:space:]]+92[[:space:]]+)([0-9]+)$", texts[[match_idx]])
      )[[1]]
      if (length(h) != 5L) return(NULL)
      fishery <- suppressWarnings(as.integer(h[[3]]))
      value <- suppressWarnings(as.integer(h[[5]]))
      if (!is.finite(fishery) || !is.finite(value)) return(NULL)
      data.frame(
        line = idx,
        match_start = matches[[match_idx]],
        match_length = lengths[[match_idx]],
        prefix = paste0(h[[2]], h[[3]], h[[4]]),
        fishery = fishery,
        value = value,
        stringsAsFactors = FALSE
      )
    })
    rows <- c(rows, line_rows)
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      line = integer(0),
      match_start = integer(0),
      match_length = integer(0),
      prefix = character(0),
      fishery = integer(0),
      value = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

replace_flag92_values <- function(lines, entries) {
  entries$replacement <- paste0(entries$prefix, entries$updated)
  for (line_idx in unique(entries$line)) {
    line_entries <- entries[entries$line == line_idx, , drop = FALSE]
    line_entries <- line_entries[order(line_entries$match_start, decreasing = TRUE), , drop = FALSE]
    line <- lines[[line_idx]]
    for (entry_idx in seq_len(nrow(line_entries))) {
      start <- line_entries$match_start[[entry_idx]]
      end <- start + line_entries$match_length[[entry_idx]] - 1L
      line <- paste0(
        substr(line, 1L, start - 1L),
        line_entries$replacement[[entry_idx]],
        substr(line, end + 1L, nchar(line))
      )
    }
    lines[[line_idx]] <- line
  }
  lines
}

halve_index_cv_flags <- function(doitall_path, index_fisheries = integer(0)) {
  lines <- readLines(doitall_path, warn = FALSE)
  all_entries <- flag92_entries(lines)
  if (nrow(all_entries) == 0) {
    stop("No flag 92 entries found in ", doitall_path)
  }

  skipped_entries <- all_entries[all_entries$fishery >= 0, , drop = FALSE]
  entries <- all_entries[all_entries$fishery < 0, , drop = FALSE]
  if (nrow(entries) == 0) {
    skipped_lines <- paste(skipped_entries$line, collapse = ", ")
    stop(
      "No negative fishery flag 92 entries found in ", doitall_path,
      if (nzchar(skipped_lines)) paste0(". Non-fishery flag 92 rows were left unchanged at lines: ", skipped_lines) else ""
    )
  }

  if (length(index_fisheries) > 0) {
    entries <- entries[abs(entries$fishery) %in% abs(index_fisheries), , drop = FALSE]
    if (nrow(entries) == 0) {
      stop(
        "No requested negative fishery flag 92 entries found in ", doitall_path,
        ". Requested fisheries: ", paste(index_fisheries, collapse = ", ")
      )
    }
  }

  entries$updated <- floor(entries$value / 2 + 0.5)
  lines <- replace_flag92_values(lines, entries)

  writeLines(lines, doitall_path, useBytes = TRUE)
  list(
    entries = entries[, c("line", "fishery", "value", "updated"), drop = FALSE],
    skipped_entries = skipped_entries[, c("line", "fishery", "value"), drop = FALSE],
    original = stats::setNames(entries$value, as.character(entries$fishery)),
    updated = stats::setNames(entries$updated, as.character(entries$fishery))
  )
}

target_index_fisheries <- parse_index_fisheries(index_fisheries_arg)

created <- character(0)
for (idx in seq_along(source_dirs)) {
  source_dir <- source_dirs[[idx]]
  target_dir <- target_dirs[[idx]]
  copy_input_dir(source_dir, target_dir)

  par_files <- list.files(target_dir, pattern = "\\.par$", full.names = TRUE)
  if (length(par_files) > 0) unlink(par_files, force = TRUE)

  doitall_path <- file.path(target_dir, "doitall.sh")
  if (!file.exists(doitall_path)) stop("No doitall.sh found in ", target_dir)
  cv_change <- halve_index_cv_flags(doitall_path, target_index_fisheries)
  change_meta <- append_input_change_metadata(
    target_dir,
    token = "cvH",
    label = "Index CPUE CV flags halved",
    operation = "index_cv_half",
    source_dir = source_dir,
    details = list(
      index_fisheries = cv_change$entries$fishery,
      original_flag_92 = cv_change$original,
      updated_flag_92 = cv_change$updated,
      skipped_nonfishery_flag_92 = cv_change$skipped_entries
    )
  )

  info <- list(
    source_dir = source_dir,
    created_at = Sys.time(),
    input_change_tokens = change_meta$tokens,
    index_fisheries = cv_change$entries$fishery,
    flag_92_entries = cv_change$entries,
    skipped_nonfishery_flag_92 = cv_change$skipped_entries,
    original_flag_92 = cv_change$original,
    updated_flag_92 = cv_change$updated,
    modified_file = "doitall.sh",
    par_files_removed = TRUE
  )
  saveRDS(info, file = file.path(target_dir, "index_cv_half_info.rds"), compress = "xz")

  created <- c(created, target_dir)
}

cat("Created index CV half sensitivities:\n")
cat(paste0("  ", created, collapse = "\n"), "\n")
cat(
  "fish flag 92 fisheries:",
  if (length(target_index_fisheries) > 0) paste(target_index_fisheries, collapse = ", ") else "all negative fishery rows found in each base input",
  "\n"
)
