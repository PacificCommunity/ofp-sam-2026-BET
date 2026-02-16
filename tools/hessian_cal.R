#!/usr/bin/env Rscript

# Hessian diagnostics utility
# - Checks PDH status from neigenvalues
# - Builds per-parameter SE table for non-positivised and positivised outputs
# - Loads positivised covariance/correlation matrices
# - Performs full-matrix SPD diagnostics
# - Saves results to .rds or .RData/.rda

args <- commandArgs(trailingOnly = TRUE)
hessian_dir <- if (length(args) >= 1) args[1] else "model/base/hessian"
out_path <- if (length(args) >= 2) args[2] else NA_character_

if (!dir.exists(hessian_dir)) stop("Hessian directory not found: ", hessian_dir)

xinit_file <- file.path(hessian_dir, "xinit.rpt")
if (!file.exists(xinit_file)) stop("xinit.rpt not found in: ", hessian_dir)

xinit <- read.table(
  xinit_file,
  header = FALSE,
  stringsAsFactors = FALSE,
  col.names = c("idx", "par"),
  colClasses = c("integer", "character")
)
n_par <- nrow(xinit)

# Parse vector files that may appear in multiple formats.
parse_vector <- function(file_path, n_expected) {
  if (is.na(file_path) || !file.exists(file_path)) {
    return(list(ok = FALSE, values = rep(NA_real_, n_expected), parser = "missing", n_raw = NA_integer_))
  }

  values <- tryCatch(scan(file_path, quiet = TRUE), error = function(e) numeric(0))
  n_raw <- length(values)

  if (n_raw == n_expected) {
    return(list(ok = TRUE, values = values, parser = "raw_vector", n_raw = n_raw))
  }

  if (n_raw == n_expected + 1 && is.finite(values[1]) && as.integer(round(values[1])) == n_expected) {
    return(list(ok = TRUE, values = values[-1], parser = "n_then_vector", n_raw = n_raw))
  }

  if (n_raw == 2L * n_expected) {
    idx <- values[seq.int(1, n_raw, by = 2)]
    val <- values[seq.int(2, n_raw, by = 2)]
    if (all(idx == seq_len(n_expected))) {
      return(list(ok = TRUE, values = val, parser = "index_value_pairs", n_raw = n_raw))
    }
  }

  if (n_raw == 2L * n_expected + 1L && is.finite(values[1]) && as.integer(round(values[1])) == n_expected) {
    body <- values[-1]
    idx <- body[seq.int(1, length(body), by = 2)]
    val <- body[seq.int(2, length(body), by = 2)]
    if (all(idx == seq_len(n_expected))) {
      return(list(ok = TRUE, values = val, parser = "n_then_index_value_pairs", n_raw = n_raw))
    }
  }

  list(ok = FALSE, values = rep(NA_real_, n_expected), parser = "unmatched", n_raw = n_raw)
}

# Parse matrix files that may be raw numeric or table format.
parse_matrix <- function(file_path, n_expected) {
  if (is.na(file_path) || !file.exists(file_path)) {
    return(list(ok = FALSE, matrix = NULL, parser = "missing", n_raw = NA_integer_))
  }

  values <- tryCatch(scan(file_path, quiet = TRUE), error = function(e) numeric(0))
  n_raw <- length(values)

  if (n_raw == n_expected * n_expected) {
    return(list(
      ok = TRUE,
      matrix = matrix(values, nrow = n_expected, ncol = n_expected, byrow = TRUE),
      parser = "raw_n2",
      n_raw = n_raw
    ))
  }

  if (n_raw == n_expected * n_expected + 1L && is.finite(values[1]) && as.integer(round(values[1])) == n_expected) {
    vals <- values[-1]
    return(list(
      ok = TRUE,
      matrix = matrix(vals, nrow = n_expected, ncol = n_expected, byrow = TRUE),
      parser = "n_then_raw_n2",
      n_raw = n_raw
    ))
  }

  tab <- tryCatch(read.table(file_path, header = TRUE, row.names = 1, check.names = FALSE), error = function(e) NULL)
  if (!is.null(tab) && nrow(tab) == n_expected && ncol(tab) == n_expected) {
    mat <- as.matrix(tab)
    storage.mode(mat) <- "numeric"
    return(list(ok = TRUE, matrix = mat, parser = "table_with_rowcol_names", n_raw = n_raw))
  }

  list(ok = FALSE, matrix = NULL, parser = "unmatched", n_raw = n_raw)
}

find_by_suffix <- function(dir_path, suffix_regex) {
  files <- list.files(dir_path, full.names = TRUE)
  hit <- files[grepl(suffix_regex, basename(files), ignore.case = TRUE)]
  if (length(hit) == 0) return(NA_character_)
  sort(hit)[1]
}

compute_se <- function(var_diag) {
  se <- rep(NA_real_, length(var_diag))
  ok <- is.finite(var_diag) & var_diag >= 0
  se[ok] <- sqrt(var_diag[ok])
  se
}

# Full-matrix SPD diagnostics.
matrix_diag <- function(M, eig_tol = 1e-10) {
  if (is.null(M)) {
    return(list(
      available = FALSE,
      symmetry_max_abs_diff = NA_real_,
      min_eigen = NA_real_,
      max_eigen = NA_real_,
      n_negative_eig = NA_integer_,
      n_nonpositive_eig = NA_integer_,
      chol_success = NA,
      chol_error = NA_character_
    ))
  }

  sym_M <- (M + t(M)) / 2
  ev <- eigen(sym_M, symmetric = TRUE, only.values = TRUE)$values
  chol_ok <- TRUE
  chol_err <- NA_character_
  tryCatch(chol(sym_M), error = function(e) {
    chol_ok <<- FALSE
    chol_err <<- conditionMessage(e)
  })

  list(
    available = TRUE,
    symmetry_max_abs_diff = max(abs(M - t(M)), na.rm = TRUE),
    min_eigen = min(ev),
    max_eigen = max(ev),
    n_negative_eig = sum(ev < -eig_tol),
    n_nonpositive_eig = sum(ev <= eig_tol),
    chol_success = chol_ok,
    chol_error = chol_err
  )
}

# PDH status from neigenvalues file.
read_pdh <- function(hessian_dir, n_par) {
  f <- file.path(hessian_dir, "neigenvalues")
  if (!file.exists(f)) {
    return(list(source = NA_character_, n_negative = NA_integer_, n_total = n_par, is_pdh = NA))
  }

  line1 <- readLines(f, n = 1, warn = FALSE)
  nums <- suppressWarnings(as.numeric(strsplit(trimws(line1), "\\s+")[[1]]))
  nums <- nums[is.finite(nums)]

  if (length(nums) < 1) {
    return(list(source = f, n_negative = NA_integer_, n_total = n_par, is_pdh = NA))
  }

  n_neg <- as.integer(nums[1])
  n_total <- if (length(nums) >= 2) as.integer(nums[2]) else n_par
  list(source = f, n_negative = n_neg, n_total = n_total, is_pdh = (n_neg == 0L))
}

suffix <- c(
  hess_inv_diag = "_hess_inv_diag$",
  pos_hess_inv_diag = "_pos_hess_inv_diag2?$",
  pos_hess_cov = "_pos_hess_cov$",
  pos_hess_cor = "_pos_hess_cor$",
  new_std = "_new\\.std$",
  pos_new_std = "_pos_new\\.std$"
)

paths <- lapply(suffix, function(rx) find_by_suffix(hessian_dir, rx))

v_hess_inv <- parse_vector(paths$hess_inv_diag, n_par)
v_pos_hess_inv <- parse_vector(paths$pos_hess_inv_diag, n_par)
v_new_std <- parse_vector(paths$new_std, n_par)
v_pos_new_std <- parse_vector(paths$pos_new_std, n_par)

m_pos_cov <- parse_matrix(paths$pos_hess_cov, n_par)
m_pos_cor <- parse_matrix(paths$pos_hess_cor, n_par)

se_nonpos <- compute_se(v_hess_inv$values)
se_pos <- compute_se(v_pos_hess_inv$values)

cov_diag_pos <- if (m_pos_cov$ok) diag(m_pos_cov$matrix) else rep(NA_real_, n_par)
cor_diag_file <- if (m_pos_cor$ok) diag(m_pos_cor$matrix) else rep(NA_real_, n_par)
cor_from_cov <- if (m_pos_cov$ok) cov2cor(m_pos_cov$matrix) else NULL

diag_cov <- matrix_diag(if (m_pos_cov$ok) m_pos_cov$matrix else NULL)
diag_cor_file <- matrix_diag(if (m_pos_cor$ok) m_pos_cor$matrix else NULL)
diag_cor_from_cov <- matrix_diag(cor_from_cov)

cor_compare <- if (!is.null(cor_from_cov) && m_pos_cor$ok) {
  d <- abs(m_pos_cor$matrix - cor_from_cov)
  list(max_abs_diff = max(d, na.rm = TRUE), mean_abs_diff = mean(d, na.rm = TRUE))
} else {
  list(max_abs_diff = NA_real_, mean_abs_diff = NA_real_)
}

pdh <- read_pdh(hessian_dir, n_par)

parameter_table <- data.frame(
  idx = xinit$idx,
  par = xinit$par,
  var_nonpos_diag = v_hess_inv$values,
  se_nonpos = se_nonpos,
  var_pos_diag = v_pos_hess_inv$values,
  se_pos = se_pos,
  var_pos_from_cov_diag = cov_diag_pos,
  cor_pos_file_diag = cor_diag_file,
  new_std = v_new_std$values,
  pos_new_std = v_pos_new_std$values,
  stringsAsFactors = FALSE
)

# Keep a short top-correlation table for quick review.
cor_for_analysis <- if (!is.null(cor_from_cov)) cor_from_cov else if (m_pos_cor$ok) m_pos_cor$matrix else NULL
top_abs_cor <- data.frame()
if (!is.null(cor_for_analysis)) {
  ut <- upper.tri(cor_for_analysis, diag = FALSE)
  vals <- cor_for_analysis[ut]
  idx <- which(ut, arr.ind = TRUE)
  ok <- is.finite(vals)
  vals <- vals[ok]
  idx <- idx[ok, , drop = FALSE]
  if (length(vals) > 0) {
    ord <- order(abs(vals), decreasing = TRUE)
    keep <- ord[seq_len(min(20L, length(ord)))]
    top_abs_cor <- data.frame(
      i = idx[keep, 1],
      j = idx[keep, 2],
      cor = vals[keep],
      abs_cor = abs(vals[keep]),
      par_i = xinit$par[idx[keep, 1]],
      par_j = xinit$par[idx[keep, 2]],
      stringsAsFactors = FALSE
    )
  }
}

files <- data.frame(
  key = names(paths),
  file = unlist(lapply(paths, function(x) ifelse(is.na(x), NA_character_, basename(x))), use.names = FALSE),
  path = unlist(paths, use.names = FALSE),
  parser = c(v_hess_inv$parser, v_pos_hess_inv$parser, m_pos_cov$parser, m_pos_cor$parser, v_new_std$parser, v_pos_new_std$parser),
  loaded = c(v_hess_inv$ok, v_pos_hess_inv$ok, m_pos_cov$ok, m_pos_cor$ok, v_new_std$ok, v_pos_new_std$ok),
  stringsAsFactors = FALSE
)

summary <- list(
  pdh = pdh,
  positivised_cov_is_spd = isTRUE(diag_cov$available) && diag_cov$n_negative_eig == 0L && isTRUE(diag_cov$chol_success),
  recommended_correlation_source = if (!is.null(cor_from_cov)) "cov2cor(pos_hess_cov)" else if (m_pos_cor$ok) "pos_hess_cor" else NA_character_,
  key_numbers = list(
    n_parameters = n_par,
    n_missing_se_nonpos = sum(is.na(se_nonpos)),
    n_missing_se_pos = sum(is.na(se_pos)),
    min_var_nonpos_diag = min(v_hess_inv$values, na.rm = TRUE),
    min_var_pos_diag = min(v_pos_hess_inv$values, na.rm = TRUE),
    min_eigen_pos_cov = diag_cov$min_eigen
  ),
  calculation_notes = c(
    "SE(non-positivised) = sqrt(var_nonpos_diag) for var_nonpos_diag >= 0, else NA.",
    "SE(positivised) = sqrt(var_pos_diag) for var_pos_diag >= 0, else NA.",
    "SPD is evaluated from the full positivised covariance matrix using symmetry, eigenvalues, and Cholesky.",
    "If pos_hess_cor differs from cov2cor(pos_hess_cov), prefer cov2cor(pos_hess_cov) for analysis."
  )
)

diagnostics <- list(
  matrix = list(
    pos_cov = diag_cov,
    pos_cor_file = diag_cor_file,
    pos_cor_from_cov = diag_cor_from_cov,
    pos_cor_file_vs_cov2cor = cor_compare
  )
)

matrices <- list(
  covariance_pos = if (m_pos_cov$ok) m_pos_cov$matrix else NULL,
  correlation_pos_file = if (m_pos_cor$ok) m_pos_cor$matrix else NULL,
  correlation_pos_from_cov = cor_from_cov,
  correlation_for_analysis = cor_for_analysis
)

info <- list(
  meta = list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    hessian_dir = normalizePath(hessian_dir),
    script = "hessian_cal.R"
  ),
  summary = summary,
  files = files,
  parameter_table = parameter_table,
  top_abs_correlations = top_abs_cor,
  diagnostics = diagnostics,
  matrices = matrices
)

if (!is.na(out_path) && nzchar(out_path)) {
  if (grepl("\\.rds$", out_path, ignore.case = TRUE)) {
    saveRDS(info, file = out_path)
  } else if (grepl("\\.rdata$|\\.rda$", out_path, ignore.case = TRUE)) {
    save(info, file = out_path)
  } else {
    stop("Unsupported output extension. Use .rds, .RData, or .rda")
  }
  cat("Saved info object:\n")
  cat("  ", out_path, "\n", sep = "")
}

cat("\nSummary\n")
cat("  is_PDH:", info$summary$pdh$is_pdh, "\n")
cat("  negative eigenvalues:", info$summary$pdh$n_negative, "/", info$summary$pdh$n_total, "\n")
cat("  positivised covariance SPD:", info$summary$positivised_cov_is_spd, "\n")
cat("  min eigen (positivised covariance):", signif(info$summary$key_numbers$min_eigen_pos_cov, 8), "\n")
cat("  recommended correlation source:", info$summary$recommended_correlation_source, "\n")
