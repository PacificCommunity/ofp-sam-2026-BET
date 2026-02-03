#!/usr/bin/env Rscript
## Read MFCL Hessian binary file
## Format: [npars (int), start_row (int), end_row (int)] + matrix data (double)

read_hessian <- function(hes_file) {
  
  if(!file.exists(hes_file)) {
    stop("Hessian file not found: ", hes_file)
  }
  
  cat("Reading Hessian file:", hes_file, "\n")
  cat("File size:", round(file.size(hes_file) / 1024 / 1024, 2), "MB\n\n")
  
  ## Open binary file
  con <- file(hes_file, open = "rb")
  
  ## Read header (1 integer: npars only)
  npars <- readBin(con, "integer", n = 1)
  
  cat("Header information:\n")
  cat("  Total parameters (npars):", npars, "\n")
  
  ## Calculate expected size based on complete matrix
  ## File format: 4 bytes (npars) + npars × npars × 8 bytes (doubles)
  expected_size <- 4 + npars * npars * 8
  actual_size <- file.size(hes_file)
  
  cat("\nFile size check:\n")
  cat("  Expected:", round(expected_size / 1024 / 1024, 2), "MB (for complete", npars, "×", npars, "matrix)\n")
  cat("  Actual:", round(actual_size / 1024 / 1024, 2), "MB\n")
  
  if(abs(expected_size - actual_size) > 1000) {
    warning("File size mismatch - file may be partial or corrupted!")
  } else {
    cat("  ✓ Size matches\n")
  }
  
  cat("\nReading matrix data...\n")
  
  ## Complete Hessian matrix
  hessian_matrix <- matrix(nrow = npars, ncol = npars)
  cat("Reading complete", npars, "×", npars, "Hessian matrix\n")
  
  ## Read all rows
  for(i in 1:npars) {
    row_data <- readBin(con, "double", n = npars)
    if(length(row_data) != npars) {
      close(con)
      stop("Error reading row ", i, ": expected ", npars, " values, got ", length(row_data))
    }
    hessian_matrix[i, ] <- row_data
    
    ## Progress indicator
    if(i %% 100 == 0 || i == npars) {
      cat("\r  Progress:", i, "/", npars, "(", round(100*i/npars, 1), "%)")
    }
  }
  cat("\n")
  
  close(con)
  
  cat("\n✓ Successfully loaded Hessian matrix\n\n")
  
  ## Summary statistics
  cat("Matrix properties:\n")
  cat("  Dimensions:", nrow(hessian_matrix), "×", ncol(hessian_matrix), "\n")
  cat("  NA values:", sum(is.na(hessian_matrix)), "\n")
  cat("  Range: [", format(min(hessian_matrix, na.rm=TRUE), scientific=TRUE), 
      ", ", format(max(hessian_matrix, na.rm=TRUE), scientific=TRUE), "]\n", sep="")
  
  ## Check symmetry
  is_sym <- isSymmetric(hessian_matrix, tol = 1e-10)
  cat("  Symmetric:", is_sym, "\n")
  
  if(!is_sym) {
    max_asym <- max(abs(hessian_matrix - t(hessian_matrix)), na.rm = TRUE)
    cat("    Max asymmetry:", format(max_asym, scientific = TRUE), "\n")
  }
  
  ## Return result
  result <- list(
    matrix = hessian_matrix,
    npars = npars,
    is_complete = TRUE,
    file = hes_file
  )
  
  class(result) <- "mfcl_hessian"
  
  return(result)
}

## Print method
print.mfcl_hessian <- function(x, ...) {
  cat("MFCL Hessian Object\n")
  cat("  File:", x$file, "\n")
  cat("  Parameters:", x$npars, "\n")
  cat("  Complete:", x$is_complete, "\n")
  cat("  Matrix:", nrow(x$matrix), "×", ncol(x$matrix), "\n")
}

## Symmetrize Hessian matrix (average with transpose)
symmetrize_hessian <- function(hess_obj) {
  H <- hess_obj$matrix
  
  cat("Symmetrizing Hessian matrix...\n")
  cat("  Original max asymmetry:", format(max(abs(H - t(H))), scientific = TRUE), "\n")
  
  ## Average with transpose
  H_sym <- (H + t(H)) / 2
  
  cat("  Symmetrized max asymmetry:", format(max(abs(H_sym - t(H_sym))), scientific = TRUE), "\n")
  cat("  ✓ Symmetrization complete\n")
  
  hess_obj$matrix <- H_sym
  hess_obj$symmetrized <- TRUE
  
  return(hess_obj)
}

## Example usage
if(interactive()) {
  ## Read complete stitched Hessian
  hess <- read_hessian("model/base/hessian/bet.hes")
  
  ## Symmetrize if needed
  if(!isSymmetric(hess$matrix, tol = 1e-6)) {
    hess <- symmetrize_hessian(hess)
  }
  
  ## Access the matrix
  H <- hess$matrix
  
  ## Calculate eigenvalues
  cat("\nCalculating eigenvalues...\n")
  eig <- eigen(H, symmetric = TRUE, only.values = TRUE)
  
  cat("Eigenvalue summary:\n")
  cat("  Positive:", sum(eig$values > 1e-10), "\n")
  cat("  Negative:", sum(eig$values < -1e-10), "\n")
  cat("  Near-zero:", sum(abs(eig$values) <= 1e-10), "\n")
  
  ## Invert to get covariance matrix (if positive definite)
  if(all(eig$values > 1e-10)) {
    cat("\n✓ Hessian is positive definite\n")
    cat("Inverting to get covariance matrix...\n")
    Sigma <- solve(H)
    
    ## Parameter standard errors
    se <- sqrt(diag(Sigma))
    cat("Standard errors:\n")
    print(summary(se))
  } else {
    cat("\n⚠️  Hessian is not positive definite\n")
    cat("Cannot compute parameter uncertainties\n")
    cat("\nSmallest eigenvalues:\n")
    print(head(sort(eig$values), 20))
  }
}

## Command line usage
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0) {
  hess <- read_hessian(args[1])
  
  ## Optionally save as RDS
  if(length(args) > 1) {
    output_file <- args[2]
    saveRDS(hess, file = output_file, compress = "xz")
    cat("\nSaved to:", output_file, "\n")
  }
}
