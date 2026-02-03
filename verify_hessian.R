#!/usr/bin/env Rscript
## Verify MFCL Hessian file quality
## Based on BET 2023 assessment verification code

library(FLR4MFCL)

## Command line arguments
args <- commandArgs(trailingOnly = TRUE)

hes_file <- if(length(args) > 0) args[1] else "model/base/hessian/bet.hes"
par_file <- if(length(args) > 1) args[2] else "model/base/12.par"

if(!file.exists(hes_file)) {
  stop("Hessian file not found: ", hes_file)
}

if(!file.exists(par_file)) {
  stop("Parameter file not found: ", par_file)
}

cat("==============================================\n")
cat("MFCL Hessian Verification\n")
cat("==============================================\n\n")
cat("Hessian file:", hes_file, "\n")
cat("Parameter file:", par_file, "\n\n")

## Read Hessian binary file
cat("* Reading Hessian matrix ... ")
con <- file(hes_file, open = "rb")

## Read header (1 integer: npars)
npars <- readBin(con, "integer", n = 1)

## Read complete matrix
H <- matrix(nrow = npars, ncol = npars)
for(i in 1:npars) {
  row_data <- readBin(con, "double", n = npars)
  H[i, ] <- row_data
}
close(con)
cat("done (", npars, " × ", npars, ")\n", sep="")

## Check and symmetrize if needed
if(!isSymmetric(H, tol = 1e-6)) {
  cat("* Hessian is not symmetric, applying symmetrization ... ")
  H <- (H + t(H)) / 2
  cat("done\n")
}

## Basic properties
cat("\n==============================================\n")
cat("Matrix Properties\n")
cat("==============================================\n")
cat("Dimensions:", nrow(H), "×", ncol(H), "\n")
cat("Symmetric:", isSymmetric(H, tol = 1e-10), "\n")
cat("Range: [", format(min(H), scientific=TRUE), ", ", 
    format(max(H), scientific=TRUE), "]\n", sep="")

## Diagonal statistics
diag_H <- diag(H)
cat("\nDiagonal elements:\n")
cat("  Range: [", format(min(diag_H), scientific=TRUE), ", ", 
    format(max(diag_H), scientific=TRUE), "]\n", sep="")
cat("  Mean:", format(mean(diag_H), scientific=TRUE), "\n")
cat("  Positive:", sum(diag_H > 0), "/", length(diag_H), "\n")
cat("  Negative:", sum(diag_H < 0), "/", length(diag_H), "\n")
cat("  Near-zero (<1e-6):", sum(abs(diag_H) < 1e-6), "\n")

## Eigenvalue analysis
cat("\n==============================================\n")
cat("Eigenvalue Analysis\n")
cat("==============================================\n")
cat("Computing eigenvalues ... ")
eig <- eigen(H, symmetric = TRUE, only.values = TRUE)
cat("done\n\n")

n_pos <- sum(eig$values > 1e-10)
n_neg <- sum(eig$values < -1e-10)
n_zero <- sum(abs(eig$values) <= 1e-10)

cat("Eigenvalue summary:\n")
cat("  Positive:", n_pos, "/", length(eig$values), 
    "(", round(100*n_pos/length(eig$values), 1), "%)\n")
cat("  Negative:", n_neg, "/", length(eig$values),
    "(", round(100*n_neg/length(eig$values), 1), "%)\n")
cat("  Near-zero:", n_zero, "/", length(eig$values), "\n")

cat("\nEigenvalue range:\n")
cat("  Min:", format(min(eig$values), scientific=TRUE), "\n")
cat("  Max:", format(max(eig$values), scientific=TRUE), "\n")

if(n_neg > 0) {
  cat("\n⚠️  Warning: Hessian has negative eigenvalues\n")
  cat("   (Not positive definite - cannot compute covariance)\n")
  cat("\nMost negative eigenvalues:\n")
  neg_vals <- sort(eig$values)[1:min(10, n_neg)]
  for(i in 1:length(neg_vals)) {
    cat("  ", i, ":", format(neg_vals[i], scientific=TRUE), "\n")
  }
}

## Condition number
cond_num <- max(abs(eig$values)) / min(abs(eig$values[abs(eig$values) > 1e-10]))
cat("\nCondition number:", format(cond_num, scientific=TRUE), "\n")
if(cond_num > 1e10) {
  cat("  ⚠️  Warning: Matrix is poorly conditioned\n")
} else if(cond_num > 1e6) {
  cat("  ⚠️  Caution: Matrix is moderately ill-conditioned\n")
} else {
  cat("  ✓ Condition number is acceptable\n")
}

## Try to compute covariance matrix
cat("\n==============================================\n")
cat("Covariance Matrix\n")
cat("==============================================\n")

if(n_pos == length(eig$values) && n_neg == 0) {
  cat("✓ Hessian is positive definite\n")
  cat("Computing inverse (covariance matrix) ... ")
  
  tryCatch({
    invH <- solve(H)
    cat("done\n\n")
    
    ## Covariance diagonal (variances)
    var_vec <- diag(invH)
    
    cat("Variance statistics:\n")
    cat("  Range: [", format(min(var_vec), scientific=TRUE), ", ", 
        format(max(var_vec), scientific=TRUE), "]\n", sep="")
    cat("  Positive:", sum(var_vec > 0), "/", length(var_vec), "\n")
    cat("  Negative:", sum(var_vec < 0), "/", length(var_vec), "\n")
    
    if(any(var_vec < 0)) {
      cat("\n⚠️  Warning: Some variances are negative!\n")
      cat("   This should not happen for positive definite Hessian\n")
      neg_idx <- which(var_vec < 0)
      cat("   Negative variance indices:", head(neg_idx, 20), "\n")
    }
    
    ## Standard errors
    se_vec <- sqrt(abs(var_vec))
    cat("\nParameter standard errors:\n")
    cat("  Range: [", format(min(se_vec), scientific=TRUE), ", ", 
        format(max(se_vec), scientific=TRUE), "]\n", sep="")
    cat("  Mean:", format(mean(se_vec), scientific=TRUE), "\n")
    cat("  Median:", format(median(se_vec), scientific=TRUE), "\n")
    
    ## Save results
    cat("\nSaving results ...\n")
    
    output_dir <- dirname(hes_file)
    
    # Save diagonal elements
    diag_df <- data.frame(
      parameter = 1:nrow(H),
      hessian_diag = diag_H,
      variance = var_vec,
      std_error = se_vec
    )
    diag_file <- file.path(output_dir, "hessian_diagnostics.csv")
    write.csv(diag_df, file = diag_file, row.names = FALSE)
    cat("  Hessian diagnostics:", diag_file, "\n")
    
    # Save full covariance matrix (if not too large)
    cov_size_mb <- length(invH) * 8 / 1024 / 1024
    if(cov_size_mb < 500) {  # Less than 500 MB
      cov_file <- file.path(output_dir, "covariance_matrix.rds")
      saveRDS(invH, file = cov_file, compress = "xz")
      cat("  Covariance matrix:", cov_file, "\n")
    } else {
      cat("  (Covariance matrix too large to save:", round(cov_size_mb, 1), "MB)\n")
    }
    
    # Save correlation matrix
    cat("  Computing correlation matrix ... ")
    cor_mat <- cov2cor(invH)
    cor_file <- file.path(output_dir, "correlation_matrix.rds")
    saveRDS(cor_mat, file = cor_file, compress = "xz")
    cat("done\n")
    cat("  Correlation matrix:", cor_file, "\n")
    
  }, error = function(e) {
    cat("\n✗ Failed to compute inverse:", e$message, "\n")
  })
  
} else {
  cat("✗ Hessian is not positive definite\n")
  cat("  Cannot compute covariance matrix\n")
  cat("  Need to use positive approximation or other methods\n")
}

## Compare with growth parameters (if par file available)
if(file.exists(par_file)) {
  cat("\n==============================================\n")
  cat("Growth Parameter Estimates\n")
  cat("==============================================\n")
  
  tryCatch({
    par <- read.MFCLPar(par_file)
    
    ## Key parameters
    logM <- drop(log_m(par))[1,1]
    vonB <- unname(growth(par)[,1])
    vpar <- unname(growth_var_pars(par)[,1])
    
    cat("Parameter values:\n")
    cat("  log(M):", format(logM, digits=6), "\n")
    cat("  L1:", format(vonB[1], digits=6), "\n")
    cat("  L2:", format(vonB[2], digits=6), "\n")
    cat("  K:", format(vonB[3], digits=6), "\n")
    if(length(vpar) >= 2) {
      cat("  SD1:", format(vpar[1], digits=6), "\n")
      cat("  SD2:", format(vpar[2], digits=6), "\n")
    }
    
    ## Standard errors (if available)
    if(exists("se_vec") && length(se_vec) >= 6) {
      npars <- length(se_vec)
      cat("\nStandard errors (last 6 parameters):\n")
      param_names <- c("logM", "L1", "L2", "K", "SD1", "SD2")
      for(i in 1:6) {
        idx <- npars - 6 + i
        cat("  ", param_names[i], ": ", format(se_vec[idx], digits=6), "\n", sep="")
      }
      
      ## Coefficient of variation
      cat("\nCoefficient of variation (%):\n")
      est_vals <- c(logM, vonB, vpar[1:2])
      for(i in 1:6) {
        idx <- npars - 6 + i
        cv <- abs(se_vec[idx] / est_vals[i]) * 100
        cat("  ", param_names[i], ": ", format(cv, digits=3), "%\n", sep="")
      }
    }
    
  }, error = function(e) {
    cat("Could not read parameter file:", e$message, "\n")
  })
}

cat("\n==============================================\n")
cat("Verification Complete\n")
cat("==============================================\n")
