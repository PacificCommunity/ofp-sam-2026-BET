# ============================================================
# Create an input folder with only selected direct movement links.
#
# Default: keep only R2 <-> R3 in the supplied input.
# This updates bet.frq and bet.ini. Copied .par files are removed because
# movement dimensions changed and phase-based runs should rebuild from bet.ini.
# ============================================================

library(FLR4MFCL)
source("tools/input_sensitivities/helpers.R")
source("tools/input_change_metadata.R")

parse_movement_pairs <- function(x) {
  tokens <- unlist(strsplit(as.character(x), "[,[:space:]]+"))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) stop("movement_pairs is empty")

  pairs <- lapply(tokens, function(token) {
    vals <- suppressWarnings(as.integer(unlist(strsplit(token, "[-:>]"))))
    vals <- vals[is.finite(vals)]
    if (length(vals) != 2 || any(vals <= 0)) {
      stop("Invalid movement pair: ", token, ". Use values like 2-3 or 1-2,3-4.")
    }
    sort(vals)
  })

  unique(do.call(rbind, pairs))
}

movement_pairs_from_matrix <- function(move_matrix) {
  pairs <- vector("list", 0L)
  for (i in seq_len(nrow(move_matrix) - 1L)) {
    for (j in seq.int(i + 1L, ncol(move_matrix))) {
      value <- move_matrix[i, j]
      if (!is.na(value) && value != 0) {
        pairs[[length(pairs) + 1L]] <- c(i, j)
      }
    }
  }
  if (length(pairs) == 0) return(matrix(integer(0), ncol = 2))
  do.call(rbind, pairs)
}

directed_pairs <- function(undirected_pairs) {
  if (nrow(undirected_pairs) == 0) return(matrix(integer(0), ncol = 2))
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

pair_key <- function(x) paste(x[, 1L], x[, 2L], sep = "-")

detect_single_file <- function(path, pattern, label) {
  x <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(x) == 0) stop("No ", label, " found in ", path)
  if (length(x) > 1) warning("Multiple ", label, " files found; using first: ", x[1])
  x[1]
}

base_dir <- env_or_default("base_dir", "")
movement_pairs_arg <- env_or_default("movement_pairs", "2-3")
out_dir <- env_or_default("out_dir", "")

project_root <- getwd()
if (!nzchar(base_dir)) stop("base_dir is required")
if (!nzchar(out_dir)) {
  movement_suffix <- movement_pairs_to_suffix(movement_pairs_arg)
  out_dir <- file.path(dirname(base_dir), paste0(basename(base_dir), "_", movement_suffix))
}
base_dir_abs <- resolve_path(base_dir, project_root)
target_dir <- resolve_path(out_dir, project_root)

if (!dir.exists(base_dir_abs)) stop("Base inputs directory does not exist: ", base_dir_abs)
if (normalizePath(target_dir, winslash = "/", mustWork = FALSE) ==
    normalizePath(base_dir_abs, winslash = "/", mustWork = TRUE)) {
  stop("out_dir must be different from base_dir: ", target_dir)
}

keep_pairs <- parse_movement_pairs(movement_pairs_arg)

frq_file <- detect_single_file(base_dir_abs, "\\.frq$", ".frq")
ini_file <- detect_single_file(base_dir_abs, "\\.ini$", ".ini")

frq <- read.MFCLFrq(file.path(base_dir_abs, frq_file))
ini <- read.MFCLIni(file.path(base_dir_abs, ini_file))

old_undirected <- movement_pairs_from_matrix(frq@move_matrix)
old_directed <- directed_pairs(old_undirected)

missing_pairs <- setdiff(pair_key(keep_pairs), pair_key(old_undirected))
if (length(missing_pairs) > 0) {
  stop("Requested movement pair(s) not present in base .frq: ", paste(missing_pairs, collapse = ", "))
}

keep_directed <- which(pair_key(old_directed) %in% pair_key(directed_pairs(keep_pairs)))
if (length(keep_directed) == 0) stop("No directed movement columns selected.")

new_move_matrix <- matrix(NA_real_, nrow = nrow(frq@move_matrix), ncol = ncol(frq@move_matrix))
new_move_matrix[upper.tri(new_move_matrix)] <- 0
for (row_idx in seq_len(nrow(keep_pairs))) {
  i <- keep_pairs[row_idx, 1L]
  j <- keep_pairs[row_idx, 2L]
  new_move_matrix[min(i, j), max(i, j)] <- 1
}

frq@move_matrix <- new_move_matrix

if (!is.matrix(ini@diff_coffs) || ncol(ini@diff_coffs) != nrow(old_directed)) {
  stop(
    "ini@diff_coffs has ", ncol(ini@diff_coffs),
    " columns but base movement matrix implies ", nrow(old_directed), " directed movements."
  )
}
ini@diff_coffs <- ini@diff_coffs[, keep_directed, drop = FALSE]

copy_input_dir(base_dir_abs, target_dir)
removed_par_files <- remove_input_par_files(target_dir)

write(frq, file.path(target_dir, frq_file))
write(ini, file.path(target_dir, ini_file))

movement_token <- paste0("m", paste(sort(unique(as.vector(keep_pairs))), collapse = ""))
change_meta <- append_input_change_metadata(
  target_dir,
  token = movement_token,
  label = paste0("Movement subset ", paste(pair_key(keep_pairs), collapse = ", ")),
  operation = "movement_subset",
  source_dir = base_dir_abs,
  details = list(movement_pairs = keep_pairs, kept_directed_columns = keep_directed)
)

info <- list(
  source_base_dir = base_dir_abs,
  created_at = Sys.time(),
  input_change_tokens = change_meta$tokens,
  movement_pairs = keep_pairs,
  old_undirected_pairs = old_undirected,
  old_directed_pairs = old_directed,
  kept_directed_columns = keep_directed,
  frq_file = frq_file,
  ini_file = ini_file,
  par_files_removed = length(removed_par_files) > 0
)
saveRDS(info, file = file.path(target_dir, "movement_subset_info.rds"), compress = "xz")

cat("Created movement-subset input:\n")
cat("  base:   ", base_dir_abs, "\n")
cat("  output: ", target_dir, "\n")
cat("  kept movement pairs:", paste(pair_key(keep_pairs), collapse = ", "), "\n")
cat("  kept directed columns:", paste(keep_directed, collapse = ", "), "\n")
cat("  diff_coffs dimensions:", paste(dim(ini@diff_coffs), collapse = " x "), "\n")
cat("  removed copied par files:", ifelse(length(removed_par_files) > 0, paste(basename(removed_par_files), collapse = ", "), "<none>"), "\n")
