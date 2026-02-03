#!/usr/bin/env Rscript
## Collate Hessian calculation results from parallel jobs
## Uses MFCL's built-in stitching routine (parest_flags(145)=11)
## Prepares component files and ASCII parall_hess input file
## Then runs MFCL to stitch components into complete parallel_hess
## Usage: Rscript collate_hessian.R [model_dir] [part_dir1] [part_dir2] ...

library(FLR4MFCL)

## Get model directory and optional part directories from command line
args <- commandArgs(trailingOnly = TRUE)

## If first arg is provided, use it as model directory or hessian directory
## If first arg ends with /hessian, use it directly
## Otherwise append /hessian to it
if(length(args) > 0) {
  first_arg <- args[1]
  
  # Check if first arg is hessian directory or model directory
  if(grepl("/hessian$", first_arg) && dir.exists(first_arg)) {
    hessian_dir <- first_arg
    part_dirs_arg <- if(length(args) > 1) args[-1] else character(0)
  } else if(dir.exists(file.path(first_arg, "hessian"))) {
    model_dir <- first_arg
    hessian_dir <- file.path(model_dir, "hessian")
    part_dirs_arg <- if(length(args) > 1) args[-1] else character(0)
  } else {
    # Assume it's a part directory
    hessian_dir <- dirname(first_arg)
    part_dirs_arg <- args
  }
} else {
  model_dir <- Sys.getenv("model_dir", "model/base")
  hessian_dir <- file.path(model_dir, "hessian")
  part_dirs_arg <- character(0)
}

cat("==============================================\n")
cat("MFCL Hessian Collation Utility\n")
cat("==============================================\n\n")
cat("Hessian directory:", hessian_dir, "\n")

if(!dir.exists(hessian_dir)) {
  stop("Hessian directory does not exist: ", hessian_dir)
}

## Find all hessian part directories or use provided list
if(length(part_dirs_arg) > 0) {
  cat("Using provided part directories from command line\n")
  part_dirs <- part_dirs_arg
} else {
  cat("Auto-detecting part directories...\n")
  part_dirs <- list.dirs(hessian_dir, recursive = FALSE, full.names = TRUE)
  part_dirs <- part_dirs[grepl("part_", basename(part_dirs))]
}

if(length(part_dirs) == 0) {
  stop("No hessian part directories found in: ", hessian_dir)
}

cat("Found", length(part_dirs), "part directories\n")

## Read info from each part
part_infos <- lapply(part_dirs, function(dir) {
  info_file <- file.path(dir, "hessian_info.rds")
  if(file.exists(info_file)) {
    readRDS(info_file)
  } else {
    NULL
  }
})

## Remove NULL entries
part_infos <- part_infos[!sapply(part_infos, is.null)]

if(length(part_infos) == 0) {
  stop("No valid hessian_info.rds files found")
}

cat("Found", length(part_infos), "completed parts\n")

## Sort by part number
part_numbers <- sapply(part_infos, function(x) x$hessian_part)
part_infos <- part_infos[order(part_numbers)]
part_dirs <- part_dirs[order(part_numbers)]

## Get total number of parameters from first part
npars <- part_infos[[1]]$npars
cat("Total parameters:", npars, "\n")

## Find hessian files (.hes files)
hes_files <- lapply(part_dirs, function(dir) {
  # Look for .hes files
  hes <- list.files(dir, pattern = "\\.hes$", full.names = TRUE)
  if(length(hes) > 0) {
    return(hes[1])
  } else {
    return(NULL)
  }
})

## Remove NULL entries
valid_hes <- !sapply(hes_files, is.null)
hes_files <- hes_files[valid_hes]
part_infos_valid <- part_infos[valid_hes]

if(length(hes_files) == 0) {
  stop("No .hes files found in part directories")
}

cat("Found", length(hes_files), ".hes files to collate\n\n")

##################################################################
## MFCL Hessian File Format:
## - Binary file with header: [npars (int), start_row (int), end_row (int)]
## - Followed by (end_row - start_row + 1) rows
## - Each row contains npars double values
##################################################################

## Read first file to get dimensions and verify format
cat("Reading dimensions from first file...\n")
con_test <- file(hes_files[[1]], open = "rb")
header1 <- readBin(con_test, "integer", n = 3)
close(con_test)

npars_from_file <- header1[1]
cat("Parameters from file header:", npars_from_file, "\n")
cat("Parameters from metadata:", npars, "\n")

if(npars_from_file != npars) {
  cat("⚠️  Warning: Mismatch between file header and metadata\n")
  cat("Using value from file header:", npars_from_file, "\n")
  npars <- npars_from_file
}

cat("\nTotal parameters:", npars, "\n")

cat("\nTotal parameters:", npars, "\n")

## Build a map of which parts cover which parameter ranges
cat("\nBuilding parameter range map...\n")
part_map <- data.frame(
  part_num = sapply(part_infos_valid, function(x) x$hessian_part),
  start_par = sapply(part_infos_valid, function(x) x$start_par),
  end_par = sapply(part_infos_valid, function(x) x$end_par),
  file_path = unlist(hes_files),
  stringsAsFactors = FALSE
)
part_map <- part_map[order(part_map$start_par), ]

cat("\nParameter coverage:\n")
print(part_map[, c("part_num", "start_par", "end_par")])

## Check for gaps or overlaps
gaps <- c()
overlaps <- c()
for(i in 1:(nrow(part_map)-1)) {
  if(part_map$end_par[i] + 1 < part_map$start_par[i+1]) {
    gaps <- c(gaps, paste0(part_map$end_par[i] + 1, "-", part_map$start_par[i+1] - 1))
  }
  if(part_map$end_par[i] >= part_map$start_par[i+1]) {
    overlaps <- c(overlaps, paste0("Part ", part_map$part_num[i], " and ", part_map$part_num[i+1]))
  }
}

if(length(gaps) > 0) {
  cat("\n⚠️  Warning: Gaps detected in parameter coverage:\n")
  cat("  Missing ranges:", paste(gaps, collapse = ", "), "\n")
}

if(length(overlaps) > 0) {
  cat("\n⚠️  Warning: Overlaps detected:\n")
  cat("  ", paste(overlaps, collapse = ", "), "\n")
}

## Collate hessian files using MFCL standard format
cat("\n==============================================\n")
cat("Collating Hessian files...\n")
cat("==============================================\n")
output_file <- file.path(hessian_dir, "complete.hes")
cat("Output file:", output_file, "\n\n")

## Initialize output file (binary write)
con_out <- file(output_file, open = "wb")

## Track total rows written
total_rows_written <- 0

## Process each hessian file in order
for(i in 1:nrow(part_map)) {
  row <- part_map[i, ]
  
  cat("Processing part", row$part_num, "\n")
  cat("  File:", row$file_path, "\n")
  cat("  Parameters:", row$start_par, "to", row$end_par, "\n")
  
  ## Open binary file for reading
  con_in <- file(row$file_path, open = "rb")
  
  ## Read header (npars, start_row, end_row)
  header <- readBin(con_in, "integer", n = 3)
  npars_file <- header[1]
  start_row <- header[2]
  end_row <- header[3]
  
  cat("  File header: npars=", npars_file, ", rows ", start_row, "-", end_row, "\n", sep="")
  
  ## Verify consistency
  if(npars_file != npars) {
    warning("Parameter count mismatch in file: expected ", npars, ", got ", npars_file)
  }
  
  ## Calculate expected number of rows
  nrows_expected <- end_row - start_row + 1
  nrows_metadata <- row$end_par - row$start_par + 1
  
  if(nrows_expected != nrows_metadata) {
    cat("  ⚠️  Warning: Row count mismatch - file header says ", nrows_expected, 
        ", metadata says ", nrows_metadata, "\n", sep="")
  }
  
  ## Read and write each row
  rows_written_this_part <- 0
  for(j in 1:nrows_expected) {
    row_data <- readBin(con_in, "double", n = npars)
    
    if(length(row_data) == npars) {
      writeBin(row_data, con_out)
      rows_written_this_part <- rows_written_this_part + 1
      total_rows_written <- total_rows_written + 1
    } else {
      warning("  Incomplete row ", j, " in part ", row$part_num, 
              ": expected ", npars, " values, got ", length(row_data))
      break
    }
  }
  
  close(con_in)
  cat("  ✓ Processed", rows_written_this_part, "of", nrows_expected, "rows\n\n")
}

close(con_out)

cat("\n==============================================\n")
cat("✅ Hessian collation complete\n")
cat("==============================================\n")
cat("Output file:", output_file, "\n")
cat("Total rows written:", total_rows_written, "of", npars, "expected\n")
cat("File size:", round(file.size(output_file) / 1024 / 1024, 2), "MB\n")

## Verify the result by reading it back
cat("\n==============================================\n")
cat("Verifying and analyzing result...\n")
cat("==============================================\n")

if(total_rows_written == 0) {
  cat("❌ Error: No rows were written to the output file\n")
  cat("Check the input files for errors\n")
  quit(status = 1)
  
} else if(total_rows_written < npars) {
  cat("⚠️  Partial Hessian matrix:", total_rows_written, "/", npars, "rows",
      "(", round(100 * total_rows_written / npars, 1), "%)\n", sep="")
  cat("Reading back the partial matrix...\n")
  
  con_verify <- file(output_file, open = "rb")
  hessian_matrix <- matrix(NA, nrow = npars, ncol = npars)
  
  rows_read <- 0
  for(i in 1:total_rows_written) {
    row_data <- readBin(con_verify, "double", n = npars)
    if(length(row_data) == npars) {
      hessian_matrix[i, ] <- row_data
      rows_read <- rows_read + 1
    } else {
      cat("  Warning: Could not read row", i, "\n")
      break
    }
  }
  close(con_verify)
  
  cat("Successfully read back:", rows_read, "rows\n")
  
  ## Save as RDS for easier use in R
  hessian_rds <- file.path(hessian_dir, "hessian_matrix_partial.rds")
  saveRDS(hessian_matrix, file = hessian_rds, compress = "xz")
  cat("Partial matrix saved as:", hessian_rds, "\n")
  
  ## Write MFCL-format parallel_hess file (even for partial matrix)
  ## Header will indicate actual row range covered
  parallel_hess_file <- file.path(hessian_dir, "parallel_hess")
  cat("\nWriting MFCL parallel_hess file (partial)...\n")
  
  con_ph <- file(parallel_hess_file, open = "wb")
  
  ## Write header: [npars, 1, rows_written] to indicate partial coverage
  writeBin(as.integer(c(npars, 1L, as.integer(rows_read))), con_ph)
  
  ## Write available rows of the Hessian matrix
  for(i in 1:rows_read) {
    writeBin(as.double(hessian_matrix[i, ]), con_ph)
  }
  
  close(con_ph)
  
  cat("✓ MFCL partial parallel_hess:", parallel_hess_file, "\n")
  cat("  File size:", round(file.size(parallel_hess_file) / 1024 / 1024, 2), "MB\n")
  cat("  Format: binary [npars=", npars, ", start=1, end=", rows_read, "] + ", 
      rows_read, "×", npars, " double matrix\n", sep="")
  cat("  ⚠️  This is a PARTIAL Hessian (", rows_read, " of ", npars, " rows)\n", sep="")
  
  ## Save collation info
  collation_info <- list(
    npars = npars,
    n_parts = nrow(part_map),
    part_map = part_map,
    rows_written = total_rows_written,
    rows_expected = npars,
    is_complete = FALSE,
    completion_pct = round(100 * total_rows_written / npars, 2),
    output_file = output_file,
    parallel_hess_file = parallel_hess_file,
    collation_time = Sys.time(),
    gaps = if(length(gaps) > 0) gaps else NULL,
    overlaps = if(length(overlaps) > 0) overlaps else NULL
  )
  
} else {
  # Complete matrix - perform full diagnostics
  cat("Reading back complete matrix...\n")
  con_verify <- file(output_file, open = "rb")
  hessian_matrix <- matrix(nrow = npars, ncol = npars)
  
  rows_read <- 0
  for(i in 1:npars) {
    row_data <- readBin(con_verify, "double", n = npars)
    if(length(row_data) == npars) {
      hessian_matrix[i, ] <- row_data
      rows_read <- rows_read + 1
    } else {
      warning("Could not read row ", i)
      break
    }
  }
  close(con_verify)
  
  cat("✓ Successfully read:", nrow(hessian_matrix), "×", ncol(hessian_matrix), "matrix\n")
  
  ## Check symmetry
  is_symmetric <- isSymmetric(hessian_matrix, tol = 1e-10)
  cat("✓ Symmetric:", is_symmetric, "\n")
  
  if(!is_symmetric) {
    ## Check how close to symmetric
    max_diff <- max(abs(hessian_matrix - t(hessian_matrix)), na.rm = TRUE)
    cat("  Maximum asymmetry:", format(max_diff, scientific = TRUE), "\n")
  }
  
  ## Check for NaN or Inf values
  has_nan <- any(is.nan(hessian_matrix))
  has_inf <- any(is.infinite(hessian_matrix))
  if(has_nan) cat("⚠️  Matrix contains NaN values\n")
  if(has_inf) cat("⚠️  Matrix contains Inf values\n")
  
  ## Calculate eigenvalues to check positive definiteness
  ## This is critical for parameter uncertainty estimation
  cat("\nCalculating eigenvalues (may take a while for large matrices)...\n")
  tryCatch({
    eigen_result <- eigen(hessian_matrix, symmetric = TRUE, only.values = TRUE)
    eigen_values <- eigen_result$values
    
    n_positive <- sum(eigen_values > 1e-10)
    n_negative <- sum(eigen_values < -1e-10)
    n_zero <- sum(abs(eigen_values) <= 1e-10)
    
    cat("\nEigenvalue diagnostics:\n")
    cat("  Positive:", n_positive, "\n")
    cat("  Negative:", n_negative, "\n")
    cat("  Near-zero:", n_zero, "\n")
    cat("  Range: [", format(min(eigen_values), scientific = TRUE), 
        ", ", format(max(eigen_values), scientific = TRUE), "]\n", sep="")
    
    is_positive_definite <- all(eigen_values > 1e-10)
    cat("  Positive definite:", is_positive_definite, "\n")
    
    if(n_negative > 0) {
      cat("\n⚠️  Warning: Negative eigenvalues detected!\n")
      cat("   This may indicate:\n")
      cat("   - Model not fully converged at optimum\n")
      cat("   - Parameters at or near bounds\n")
      cat("   - Numerical precision issues\n")
      cat("   - Model misspecification\n")
    }
    
  }, error = function(e) {
    cat("Could not calculate eigenvalues:", e$message, "\n")
    eigen_values <- NULL
    is_positive_definite <- NA
    n_positive <- NA
    n_negative <- NA
    n_zero <- NA
  })
  
  ## Save as RDS for easier use in R
  hessian_rds <- file.path(hessian_dir, "hessian_matrix.rds")
  saveRDS(hessian_matrix, file = hessian_rds, compress = "xz")
  cat("\n✓ Complete matrix saved as:", hessian_rds, "\n")
  
  ## Write MFCL-format parallel_hess file
  ## This file can be used directly by MFCL for uncertainty estimation
  parallel_hess_file <- file.path(hessian_dir, "parallel_hess")
  cat("\nWriting MFCL parallel_hess file...\n")
  
  con_ph <- file(parallel_hess_file, open = "wb")
  
  ## Write header: [npars, 1, npars] to indicate complete Hessian
  writeBin(as.integer(c(npars, 1L, npars)), con_ph)
  
  ## Write all rows of the Hessian matrix
  for(i in 1:npars) {
    writeBin(as.double(hessian_matrix[i, ]), con_ph)
  }
  
  close(con_ph)
  
  cat("✓ MFCL parallel_hess file:", parallel_hess_file, "\n")
  cat("  File size:", round(file.size(parallel_hess_file) / 1024 / 1024, 2), "MB\n")
  cat("  Format: binary [npars=", npars, ", start=1, end=", npars, "] + ", npars, "×", npars, " double matrix\n", sep="")
  
  ## Save collation info with diagnostics
  collation_info <- list(
    npars = npars,
    n_parts = nrow(part_map),
    part_map = part_map,
    rows_written = total_rows_written,
    rows_expected = npars,
    is_complete = TRUE,
    completion_pct = 100,
    output_file = output_file,
    parallel_hess_file = parallel_hess_file,
    collation_time = Sys.time(),
    is_symmetric = is_symmetric,
    has_nan = has_nan,
    has_inf = has_inf,
    eigenvalue_summary = if(exists("eigen_values") && !is.null(eigen_values)) {
      list(
        n_positive = n_positive,
        n_negative = n_negative,
        n_zero = n_zero,
        min = min(eigen_values),
        max = max(eigen_values),
        is_positive_definite = is_positive_definite,
        condition_number = max(eigen_values) / min(eigen_values[eigen_values > 1e-10])
      )
    } else NULL
  )
}

saveRDS(
  collation_info,
  file = file.path(hessian_dir, "collation_info.rds"),
  compress = "xz"
)

cat("\n✅ All done!\n")
