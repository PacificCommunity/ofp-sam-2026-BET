# ============================================================
# Create input sensitivities with index CPUE CV flags halved.
#
# This edits doitall.sh only. By default it finds every fish flag 92 entry
# in the selected base input and halves those values. Set index_fisheries to
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
  m <- regexec("^([[:space:]]*)(-?[0-9]+)([[:space:]]+92[[:space:]]+)([0-9]+)(.*)$", lines)
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

halve_index_cv_flags <- function(doitall_path, index_fisheries = integer(0)) {
  lines <- readLines(doitall_path, warn = FALSE)
  entries <- flag92_entries(lines)
  if (nrow(entries) == 0) {
    stop("No fish flag 92 entries found in ", doitall_path)
  }

  if (length(index_fisheries) > 0) {
    entries <- entries[abs(entries$fishery) %in% abs(index_fisheries), , drop = FALSE]
    if (nrow(entries) == 0) {
      stop(
        "No requested fish flag 92 entries found in ", doitall_path,
        ". Requested fisheries: ", paste(index_fisheries, collapse = ", ")
      )
    }
  }

  entries$updated <- floor(entries$value / 2 + 0.5)
  for (idx in seq_len(nrow(entries))) {
    line_idx <- entries$line[[idx]]
    lines[[line_idx]] <- paste0(entries$prefix[[idx]], entries$updated[[idx]], entries$suffix[[idx]])
  }

  writeLines(lines, doitall_path, useBytes = TRUE)
  list(
    entries = entries[, c("line", "fishery", "value", "updated"), drop = FALSE],
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
      updated_flag_92 = cv_change$updated
    )
  )

  info <- list(
    source_dir = source_dir,
    created_at = Sys.time(),
    input_change_tokens = change_meta$tokens,
    index_fisheries = cv_change$entries$fishery,
    flag_92_entries = cv_change$entries,
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
  if (length(target_index_fisheries) > 0) paste(target_index_fisheries, collapse = ", ") else "all found in each base input",
  "\n"
)
