# ============================================================
# Create 4-region sensitivities with fewer cubic spline nodes for all
# cubic-spline selectivity groups.
#
# This is intentionally a doitall.sh change, not a .par edit. The run creates
# 00.par from bet.ini and then applies flag overrides from doitall.sh.
# ============================================================

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

parse_int_list <- function(x) {
  vals <- unlist(strsplit(as.character(x), "[,[:space:]]+"))
  vals <- suppressWarnings(as.integer(vals[nzchar(vals)]))
  unique(vals[is.finite(vals) & vals > 0])
}

project_root <- getwd()
inputs_root <- env_or_default("inputs_root", "mfcl/inputs")
suffix <- env_or_default("suffix", "_sel_spline4")
node_count <- suppressWarnings(as.integer(env_or_default("node_count", "4")))

if (!is.finite(node_count) || node_count <= 0) stop("node_count must be a positive integer")

inputs_root_abs <- if (grepl("^/", inputs_root)) inputs_root else file.path(project_root, inputs_root)
if (!dir.exists(inputs_root_abs)) stop("inputs_root does not exist: ", inputs_root_abs)

base_dirs <- list.dirs(inputs_root_abs, full.names = FALSE, recursive = FALSE)
base_dirs <- base_dirs[grepl("^2023_4region", base_dirs)]
base_dirs <- base_dirs[!endsWith(base_dirs, suffix)]

if (length(base_dirs) == 0) stop("No 4-region input directories found in ", inputs_root_abs)

set_global_selectivity_nodes <- function(doitall_path, node_count) {
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

  anchor <- grep("^-999[[:space:]]+61[[:space:]]+[0-9]+", trimws(lines))
  if (length(anchor) == 0) stop("Could not find global fish flag 61 line in ", doitall_path)
  anchor <- anchor[1]

  lines[anchor] <- sprintf("  -999 61 %d  # with %d nodes for cubic spline", node_count, node_count)
  writeLines(lines, doitall_path, useBytes = TRUE)
}

created <- character(0)
for (base_name in base_dirs) {
  source_dir <- file.path(inputs_root_abs, base_name)
  target_dir <- file.path(inputs_root_abs, paste0(base_name, suffix))

  if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- file.copy(files, to = target_dir, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) warning("Some files failed to copy for ", base_name)

  par_files <- list.files(target_dir, pattern = "\\.par$", full.names = TRUE)
  if (length(par_files) > 0) unlink(par_files, force = TRUE)

  doitall_path <- file.path(target_dir, "doitall.sh")
  if (!file.exists(doitall_path)) stop("No doitall.sh found in ", target_dir)
  set_global_selectivity_nodes(doitall_path, node_count)

  info <- list(
    source_dir = source_dir,
    created_at = Sys.time(),
    target_fisheries = "all",
    node_count = node_count,
    modified_file = "doitall.sh",
    par_files_removed = TRUE
  )
  saveRDS(info, file = file.path(target_dir, "selectivity_spline_nodes_info.rds"), compress = "xz")

  created <- c(created, target_dir)
}

cat("Created selectivity spline-node sensitivities:\n")
cat(paste0("  ", created, collapse = "\n"), "\n")
cat("Target fish flag 61 fisheries: all (-999)\n")
cat("Node count:", node_count, "\n")
