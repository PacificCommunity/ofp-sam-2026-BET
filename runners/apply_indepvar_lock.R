#!/usr/bin/env Rscript

library(FLR4MFCL)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) %% 2 != 0 || length(args) == 0) {
  stop("Usage: Rscript apply_indepvar_lock.R --par <parfile> --lock <lock_rds> --indepvar <indepvar.rpt>")
}

kv <- setNames(args[c(FALSE, TRUE)], sub("^--", "", args[c(TRUE, FALSE)]))
par_file <- kv[["par"]]
lock_rds <- kv[["lock"]]
indepvar_file <- kv[["indepvar"]]

if (!is.character(par_file) || !nzchar(par_file) || !file.exists(par_file)) {
  stop("Missing --par or file not found: ", par_file)
}
if (!is.character(lock_rds) || !nzchar(lock_rds) || !file.exists(lock_rds)) {
  stop("Missing --lock or file not found: ", lock_rds)
}
if (!is.character(indepvar_file) || !nzchar(indepvar_file) || !file.exists(indepvar_file)) {
  stop("Missing --indepvar or file not found: ", indepvar_file)
}

script_path <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), mustWork = TRUE)
project_root <- dirname(dirname(script_path))
source(file.path(project_root, "tools", "jitter.R"))

lock_spec <- readRDS(lock_rds)
if (!is.list(lock_spec) || is.null(lock_spec$Index) || is.null(lock_spec$value_after)) {
  stop("Invalid lock spec RDS: expected list(Index, value_after)")
}

idx <- suppressWarnings(as.integer(lock_spec$Index))
vals <- suppressWarnings(as.numeric(lock_spec$value_after))
if (length(idx) == 0 || length(vals) == 0 || length(idx) != length(vals)) {
  stop("Invalid lock vectors in lock spec")
}
if (any(!is.finite(idx)) || any(!is.finite(vals))) {
  stop("Non-finite values in lock spec")
}

par_obj <- suppressWarnings(tryCatch(read.MFCLPar(par_file), error = function(e) NULL))
if (is.null(par_obj)) {
  stop("Failed to read par file: ", par_file)
}

indepvar_map <- build_indepvar_mapping(par_obj, indepvar_file = indepvar_file, tol = 1e-14)
if (is.null(indepvar_map) || nrow(indepvar_map$mapping) == 0) {
  stop("Failed to build indepvar mapping from: ", indepvar_file)
}

map_idx <- indepvar_map$mapping$Index
row_hits <- match(idx, map_idx)
if (any(is.na(row_hits))) {
  missing_idx <- idx[is.na(row_hits)]
  stop("Some lock Index values not found in mapping: ", paste(missing_idx, collapse = ", "))
}

current_values <- extract_indepvar_values(par_obj, indepvar_map)
new_values <- current_values
new_values[row_hits] <- vals

locked_par <- inject_indepvar_values(par_obj, indepvar_map, new_values)
FLR4MFCL::write(locked_par, file = par_file)
