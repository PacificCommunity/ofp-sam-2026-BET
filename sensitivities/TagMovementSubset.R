# ============================================================
# Create an input folder with only selected direct movement links.
#
# Default: keep only R2 <-> R3 in the existing 4-region input.
# This updates bet.frq and bet.ini. Existing .par files are copied unchanged;
# phase-based runs using doitall.sh rebuild from bet.ini via -makepar.
# ============================================================

library(FLR4MFCL)

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

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

base_dir <- env_or_default("base_dir", "mfcl/inputs/2023_4region")
movement_pairs_arg <- env_or_default("movement_pairs", "2-3")
out_dir <- env_or_default("out_dir", "mfcl/inputs/2023_4region_movement_R2_R3")

project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
target_dir <- if (grepl("^/", out_dir)) out_dir else file.path(project_root, out_dir)

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

if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

files_to_copy <- list.files(base_dir_abs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
ok <- file.copy(files_to_copy, to = target_dir, overwrite = TRUE, recursive = TRUE)
if (!all(ok)) warning("Some files failed to copy into: ", target_dir)

write(frq, file.path(target_dir, frq_file))
write(ini, file.path(target_dir, ini_file))

info <- list(
  source_base_dir = base_dir_abs,
  created_at = Sys.time(),
  movement_pairs = keep_pairs,
  old_undirected_pairs = old_undirected,
  old_directed_pairs = old_directed,
  kept_directed_columns = keep_directed,
  frq_file = frq_file,
  ini_file = ini_file,
  par_file_modified = FALSE
)
saveRDS(info, file = file.path(target_dir, "movement_subset_info.rds"), compress = "xz")

cat("Created movement-subset input:\n")
cat("  base:   ", base_dir_abs, "\n")
cat("  output: ", target_dir, "\n")
cat("  kept movement pairs:", paste(pair_key(keep_pairs), collapse = ", "), "\n")
cat("  kept directed columns:", paste(keep_directed, collapse = ", "), "\n")
cat("  diff_coffs dimensions:", paste(dim(ini@diff_coffs), collapse = " x "), "\n")
cat("  par files copied unchanged.\n")
