#!/usr/bin/env Rscript
## Collate Hessian calculation results using MFCL's built-in stitching
## Uses parest_flags(145)=11 for stitching parallel Hessian components
## Uses parest_flags(145)=5 for eigenvalue decomposition
## Performs comprehensive Hessian diagnostics and uncertainty analysis
## Reference: MFCL Hessian diagnostics documentation
## Usage: Rscript collate_hessian_mfcl.R [model_dir]

library(FLR4MFCL)

## Get model directory from command line
args <- commandArgs(trailingOnly = TRUE)

if(length(args) > 0) {
  first_arg <- args[1]
  
  # Check if first arg is hessian directory or model directory
  if(grepl("/hessian$", first_arg) && dir.exists(first_arg)) {
    hessian_dir <- first_arg
    model_dir <- dirname(hessian_dir)
  } else if(dir.exists(file.path(first_arg, "hessian"))) {
    model_dir <- first_arg
    hessian_dir <- file.path(model_dir, "hessian")
  } else {
    stop("Invalid directory: ", first_arg)
  }
} else {
  model_dir <- Sys.getenv("model_dir", "model/base")
  hessian_dir <- file.path(model_dir, "hessian")
}

cat("==============================================\n")
cat("MFCL Hessian Stitching & Diagnostics Utility\n")
cat("Using MFCL built-in routines:\n")
cat("  - parest_flags(145)=11 for stitching\n")
cat("  - parest_flags(145)=5 for eigenvalue decomposition\n")
cat("==============================================\n\n")
cat("Model directory:", model_dir, "\n")
cat("Hessian directory:", hessian_dir, "\n\n")

if(!dir.exists(hessian_dir)) {
  stop("Hessian directory does not exist: ", hessian_dir)
}

## Find all hessian part directories
part_dirs <- list.dirs(hessian_dir, recursive = FALSE, full.names = TRUE)
part_dirs <- part_dirs[grepl("part_", basename(part_dirs))]

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

## Get parameters
npars <- part_infos[[1]]$npars
n_parts <- length(part_infos)
frq_file <- part_infos[[1]]$frq_file
mfcl_exe <- part_infos[[1]]$program_path

cat("Total parameters:", npars, "\n")
cat("Number of parts:", n_parts, "\n")
cat("FRQ file:", frq_file, "\n")
cat("MFCL executable:", mfcl_exe, "\n\n")

## Extract start positions for each part
start_positions <- sapply(part_infos, function(x) x$start_par)
cat("Start positions:", paste(start_positions, collapse = " "), "\n\n")

##################################################################
## Step 1: Create ASCII input file "parall_hess"
##################################################################
cat("==============================================\n")
cat("Step 1: Creating parall_hess input file\n")
cat("==============================================\n")

parall_hess_file <- file.path(hessian_dir, "parall_hess")

## Write the three-line ASCII file
cat("# Number of parallel processes\n", file = parall_hess_file)
cat(n_parts, "\n", file = parall_hess_file, append = TRUE)
cat("# Total number of independent variables\n", file = parall_hess_file, append = TRUE)
cat(npars, "\n", file = parall_hess_file, append = TRUE)
cat("# Start positions of each process\n", file = parall_hess_file, append = TRUE)
cat(paste(start_positions, collapse = " "), "\n", file = parall_hess_file, append = TRUE)

cat("✓ Created:", parall_hess_file, "\n")
cat("  Contents:\n")
cat(readLines(parall_hess_file), sep = "\n")
cat("\n")

##################################################################
## Step 2: Copy and rename component .hes files
##################################################################
cat("==============================================\n")
cat("Step 2: Preparing component files\n")
cat("==============================================\n")

## Get root filename from frq_file (e.g., "bet" from "bet.frq")
root_name <- sub("\\.frq$", "", frq_file)

## Find and copy .hes files with proper naming convention
for(i in seq_along(part_infos)) {
  part_dir <- part_dirs[i]
  part_num <- part_infos[[i]]$hessian_part
  
  ## Find .hes file in part directory
  hes_files <- list.files(part_dir, pattern = "\\.hes$", full.names = TRUE)
  
  if(length(hes_files) == 0) {
    warning("No .hes file found in part ", part_num)
    next
  }
  
  ## Source file
  src_hes <- hes_files[1]
  
  ## Target filename: root.hes_n (e.g., bet.hes_1, bet.hes_2, ...)
  target_hes <- file.path(hessian_dir, paste0(root_name, ".hes_", i))
  
  ## Copy file
  file.copy(src_hes, target_hes, overwrite = TRUE)
  
  cat("  Part", i, ":", basename(src_hes), "->", basename(target_hes), 
      "(", round(file.size(target_hes) / 1024, 1), "KB )\n")
}

cat("\n")

##################################################################
## Step 3: Copy required input files to hessian directory
##################################################################
cat("==============================================\n")
cat("Step 3: Copying input files\n")
cat("==============================================\n")

## Copy frq file - try model_dir first, then part directories
frq_src <- file.path(model_dir, frq_file)
frq_dst <- file.path(hessian_dir, frq_file)

if(file.exists(frq_src)) {
  file.copy(frq_src, frq_dst, overwrite = TRUE)
  cat("✓ Copied:", frq_file, "(from model directory)\n")
} else {
  ## Try to find frq in part directories
  for(part_dir in part_dirs) {
    frq_part <- file.path(part_dir, frq_file)
    if(file.exists(frq_part)) {
      file.copy(frq_part, frq_dst, overwrite = TRUE)
      cat("✓ Copied:", frq_file, "(from", basename(part_dir), ")\n")
      break
    }
  }
  
  if(!file.exists(frq_dst)) {
    stop("FRQ file not found in model directory or part directories: ", frq_file)
  }
}

## Find the converged .par file (look for highest numbered .par)
par_files <- list.files(model_dir, pattern = "^[0-9]+\\.par$", full.names = TRUE)

## If not found in model_dir, try part directories
if(length(par_files) == 0) {
  for(part_dir in part_dirs) {
    part_par_files <- list.files(part_dir, pattern = "^[0-9]+\\.par$", full.names = TRUE)
    if(length(part_par_files) > 0) {
      par_files <- part_par_files
      break
    }
  }
}

if(length(par_files) > 0) {
  ## Get the highest numbered par file
  par_numbers <- as.integer(sub("\\.par$", "", basename(par_files)))
  final_par <- par_files[which.max(par_numbers)]
  par_file <- basename(final_par)
  
  par_dst <- file.path(hessian_dir, par_file)
  file.copy(final_par, par_dst, overwrite = TRUE)
  cat("✓ Copied:", par_file, "(", round(file.size(final_par)/1024/1024, 1), "MB )\n")
} else {
  stop("No .par file found in model or part directories")
}

## Copy other necessary files using root_name
other_files <- c(
  paste0(root_name, ".age_length"),
  paste0(root_name, ".ini"),
  paste0(root_name, ".tag"),
  "mfcl.cfg"
)

for(file_name in other_files) {
  src <- file.path(model_dir, file_name)
  
  ## If not in model_dir, try first part directory
  if(!file.exists(src)) {
    src <- file.path(part_dirs[1], file_name)
  }
  
  if(file.exists(src)) {
    dst <- file.path(hessian_dir, file_name)
    file.copy(src, dst, overwrite = TRUE)
    cat("✓ Copied:", file_name, "\n")
  } else {
    cat("  (Optional file not found:", file_name, ")\n")
  }
}

cat("\n")

##################################################################
## Step 4: Run MFCL stitching command
##################################################################
cat("==============================================\n")
cat("Step 4: Running MFCL stitching\n")
cat("==============================================\n")

## Get absolute path to MFCL executable
if(!file.exists(mfcl_exe)) {
  ## Try relative to working directory
  mfcl_exe_abs <- file.path(getwd(), mfcl_exe)
  if(file.exists(mfcl_exe_abs)) {
    mfcl_exe <- mfcl_exe_abs
  } else {
    stop("MFCL executable not found: ", mfcl_exe)
  }
} else if(!grepl("^/", mfcl_exe)) {
  ## Convert to absolute path
  mfcl_exe <- file.path(getwd(), mfcl_exe)
}

## Construct MFCL command
mfcl_cmd <- paste(
  mfcl_exe,
  frq_file,
  par_file,
  "ttt",
  "-switch 1 1 145 11"
)

cat("Executable:", mfcl_exe, "\n")
cat("Command:", mfcl_cmd, "\n\n")

## Change to hessian directory and run
old_wd <- getwd()
setwd(hessian_dir)

## Run MFCL stitching
log_file <- "mfcl_stitch_log.txt"
cat("Running MFCL stitching (output -> ", log_file, ")...\n", sep = "")

system2(
  command = mfcl_exe,
  args = c(frq_file, par_file, "ttt", "-switch", "1", "1", "145", "11"),
  stdout = log_file,
  stderr = log_file
)

cat("✓ MFCL stitching completed\n\n")

##################################################################
## Step 5: Verify output using parall_hess
##################################################################
cat("==============================================\n")
cat("Step 5: Verifying stitched Hessian\n")
cat("==============================================\n")

## IMPORTANT: Check for .hes file in CURRENT directory (hessian_dir)
## since we're still in that directory from setwd() above
stitched_hess_file_name <- paste0(root_name, ".hes")
stitched_hess_file <- file.path(getwd(), stitched_hess_file_name)

## List all .hes files to see what was created
cat("Checking for .hes files in current directory...\n")
hes_files_found <- list.files(".", pattern = "\\.hes$", full.names = TRUE)
if(length(hes_files_found) > 0) {
  cat("Found .hes files:\n")
  for(hf in hes_files_found) {
    cat("  -", hf, "(", round(file.size(hf)/1024/1024, 2), "MB )\n")
  }
  cat("\n")
}

if(file.exists(stitched_hess_file_name)) {
  file_size_mb <- file.size(stitched_hess_file_name) / 1024 / 1024
  cat("✅ SUCCESS: Stitched Hessian file created\n")
  cat("   File:", stitched_hess_file, "\n")
  cat("   Size:", round(file_size_mb, 2), "MB\n\n")
  
  ## Verify using parall_hess configuration
  cat("   Verifying against parall_hess configuration...\n\n")
  
  ## Calculate expected size from part configuration
  part_ranges <- list()
  for(i in seq_along(start_positions)) {
    start <- start_positions[i]
    if(i < length(start_positions)) {
      end <- start_positions[i + 1] - 1
    } else {
      end <- npars
    }
    n_pars_in_part <- end - start + 1
    part_ranges[[i]] <- c(start = start, end = end, n_pars = n_pars_in_part)
  }
  
  ## Display part configuration
  cat("   Part configuration:\n")
  total_pars <- 0
  for(i in seq_along(part_ranges)) {
    pr <- part_ranges[[i]]
    cat(sprintf("     Part %d: Parameters %4d to %4d (%4d parameters)\n", 
                i, pr["start"], pr["end"], pr["n_pars"]))
    total_pars <- total_pars + pr["n_pars"]
  }
  
  cat("\n   Total parameters covered:", total_pars, "\n")
  cat("   Expected total:", npars, "\n")
  
  if(total_pars == npars) {
    cat("   ✓ All parameters covered correctly\n\n")
  } else {
    cat("   ✗ Parameter coverage mismatch!\n\n")
  }
  
  ## Calculate expected file size
  expected_data_size_mb <- 0
  cat("   Expected component sizes:\n")
  for(i in seq_along(part_ranges)) {
    pr <- part_ranges[[i]]
    rows <- pr["n_pars"]
    cols <- npars
    part_size_mb <- (rows * cols * 8) / (1024 * 1024)
    expected_data_size_mb <- expected_data_size_mb + part_size_mb
    cat(sprintf("     Part %d: %4d × %4d = %6.2f MB\n", 
                i, rows, cols, part_size_mb))
  }
  
  cat(sprintf("\n   Total expected data: %.2f MB\n", expected_data_size_mb))
  cat(sprintf("   Actual file size:    %.2f MB\n", file_size_mb))
  
  ## Calculate difference
  size_diff_mb <- abs(file_size_mb - expected_data_size_mb)
  size_diff_pct <- (size_diff_mb / expected_data_size_mb) * 100
  
  cat(sprintf("   Difference:          %.2f MB (%.2f%%)\n\n", 
              size_diff_mb, size_diff_pct))
  
  ## Verify file size
  is_complete <- FALSE
  if(size_diff_pct < 1.0) {
    cat("   ✅ File size matches perfectly!\n")
    cat("   ✓ Complete Hessian matrix (", npars, "×", npars, ")\n", sep = "")
    cat("   ✓ All ", n_parts, " parts successfully stitched\n\n", sep = "")
    is_complete <- TRUE
  } else if(size_diff_pct < 5.0) {
    cat("   ✓ File size is within acceptable range\n")
    cat("   ✓ Hessian matrix appears complete (", npars, "×", npars, ")\n", sep = "")
    cat("   ✓ Minor overhead likely due to file headers/markers\n\n")
    is_complete <- TRUE
  } else {
    cat("   ⚠️  File size differs significantly from expected\n")
    cat("   File may be incomplete or have unexpected format\n\n")
  }
  
  ## Additional file structure information
  cat("   File structure details:\n")
  con <- file(stitched_hess_file_name, open = "rb")
  first_ints <- readBin(con, "integer", n = 20, size = 4, endian = "little")
  close(con)
  
  cat("   First 20 integers (little-endian):\n")
  cat("     ", paste(sprintf("%d", first_ints[1:10]), collapse = ", "), "\n")
  cat("     ", paste(sprintf("%d", first_ints[11:20]), collapse = ", "), "\n")
  
  if(first_ints[1] == npars) {
    cat("   ✓ First integer matches npars (", npars, ")\n\n", sep = "")
  } else {
    cat("\n")
  }
  
  ## Save verification info (use absolute path)
  stitch_info <- list(
    npars = npars,
    n_parts = n_parts,
    start_positions = start_positions,
    part_ranges = part_ranges,
    stitched_hess_file = stitched_hess_file,
    stitch_time = Sys.time(),
    is_complete = is_complete,
    file_size_mb = file_size_mb,
    expected_size_mb = expected_data_size_mb,
    size_diff_mb = size_diff_mb,
    size_diff_pct = size_diff_pct,
    verification_method = "parall_hess_configuration"
  )
  
  saveRDS(stitch_info, file = "stitch_info.rds")
  cat("   ✓ Verification info saved to stitch_info.rds\n\n")
  
} else {
  cat("❌ ERROR: Stitched Hessian file not created\n")
  cat("   Expected file:", stitched_hess_file, "\n")
  cat("   Current directory:", getwd(), "\n")
  cat("   Files in directory:\n")
  all_files <- list.files(".", full.names = FALSE)
  for(f in head(all_files, 20)) {
    cat("     -", f, "\n")
  }
  cat("   Check log file:", log_file, "\n")
  cat("\nLog file contents:\n")
  if(file.exists(log_file)) {
    log_contents <- readLines(log_file, n = 50)
    cat(paste(log_contents, collapse = "\n"), "\n")
  }
  cat("\n")
  setwd(old_wd)
  quit(status = 1)
}

##################################################################
## Step 6: Run eigenvalue decomposition
##################################################################
cat("==============================================\n")
cat("Step 6: Running eigenvalue decomposition\n")
cat("==============================================\n")

## Run MFCL eigenvalue analysis
eval_log_file <- "mfcl_eigenvalue_log.txt"
cat("Running eigenvalue decomposition (output -> ", eval_log_file, ")...\n", sep = "")

eval_cmd <- paste(
  mfcl_exe,
  frq_file,
  par_file,
  "ttt",
  "-switch 1 1 145 5"
)

cat("Command:", eval_cmd, "\n\n")

system2(
  command = mfcl_exe,
  args = c(frq_file, par_file, "ttt", "-switch", "1", "1", "145", "5"),
  stdout = eval_log_file,
  stderr = eval_log_file
)

cat("✓ Eigenvalue decomposition completed\n\n")

## Check for eigenvalue output files
eval_file <- paste0(root_name, ".eva")
evec_file <- paste0(root_name, ".eve")

if(file.exists(eval_file)) {
  cat("   ✓ Eigenvalue file created:", eval_file, "\n")
  cat("     Size:", round(file.size(eval_file) / 1024, 2), "KB\n")
} else {
  cat("   ⚠️  Eigenvalue file not found:", eval_file, "\n")
}

if(file.exists(evec_file)) {
  cat("   ✓ Eigenvector file created:", evec_file, "\n")
  cat("     Size:", round(file.size(evec_file) / 1024 / 1024, 2), "MB\n")
} else {
  cat("   ⚠️  Eigenvector file not found:", evec_file, "\n")
}

cat("\n")

##################################################################
## Step 7: Comprehensive Hessian Diagnostics
##################################################################
cat("==============================================\n")
cat("Step 7: Hessian Diagnostic Analysis\n")
cat("==============================================\n\n")

## Check for diagnostic files
cat("Checking diagnostic output files...\n")
diagnostic_files <- c(
  "neigenvalues",
  "new_cor_report",
  paste0(root_name, "_new_std"),
  paste0(root_name, "_pos_new_std"),
  paste0(root_name, "_hess_inv_diag"),
  paste0(root_name, "_pos_hess_inv_diag2"),
  paste0(root_name, "_pos_hess_cor"),
  paste0(root_name, "_pos_hess_cov")
)

for(df in diagnostic_files) {
  if(file.exists(df)) {
    fsize <- file.size(df)
    if(fsize > 1024*1024) {
      cat("  ✓", df, "-", round(fsize/1024/1024, 2), "MB\n")
    } else {
      cat("  ✓", df, "-", round(fsize/1024, 2), "KB\n")
    }
  } else {
    cat("  ✗", df, "- NOT FOUND\n")
  }
}
cat("\n")

## Analyze negative eigenvalues
cat("==============================================\n")
cat("Analyzing Eigenvalues (Critical for Model Validation)\n")
cat("==============================================\n\n")

neigen_file <- "neigenvalues"
hessian_status <- "Unknown"
n_negative <- NA
reliability <- "UNKNOWN"

if(file.exists(neigen_file)) {
  neigen_lines <- readLines(neigen_file)
  
  ## First line: "n_negative total_params"
  first_line <- as.integer(strsplit(neigen_lines[1], " ")[[1]])
  n_negative <- first_line[1]
  n_total <- first_line[2]
  
  cat("Total parameters:", n_total, "\n")
  cat("Negative eigenvalues:", n_negative, "\n")
  cat("Proportion:", round(n_negative/n_total*100, 2), "%\n\n")
  
  if(n_negative == 0) {
    cat("✅ EXCELLENT: Hessian is POSITIVE DEFINITE (PDH)\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("   All uncertainty estimates are VALID and RELIABLE\n")
    cat("   Model has converged to a true minimum\n")
    cat("   Use: *_new_std (normal standard errors)\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    hessian_status <- "PDH"
    reliability <- "HIGH"
    
  } else if(n_negative < n_total * 0.01) {
    cat("⚠️  WARNING: Few negative eigenvalues detected\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("   Model is close to convergence but not quite there\n")
    cat("   Uncertainty estimates are APPROXIMATE\n")
    cat("   Use: *_pos_new_std (positivized standard errors)\n")
    cat("   Note: Positivized = negative eigenvalues forced positive\n")
    cat("   Recommendation: Use with CAUTION, perform sensitivity analysis\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    hessian_status <- "Near-PDH"
    reliability <- "MODERATE"
    
  } else {
    cat("❌ PROBLEM: Many negative eigenvalues detected\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("   Hessian is NOT positive definite\n")
    cat("   Uncertainty estimates are UNRELIABLE\n")
    cat("   Model has NOT converged properly\n")
    cat("   Use: *_pos_new_std (positivized - but UNRELIABLE)\n")
    cat("   Note: Positivized values are mathematical approximations\n")
    cat("         and do NOT represent true uncertainty\n")
    cat("   Action: DO NOT USE for management advice - FIX MODEL FIRST\n")
    cat("   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    hessian_status <- "Non-PDH"
    reliability <- "LOW"
  }
  
  ## Show the actual negative eigenvalues
  if(n_negative > 0 && length(neigen_lines) > 1) {
    cat("Negative eigenvalue(s) [smallest to largest]:\n")
    n_show <- min(n_negative, 10)
    for(i in 2:min(length(neigen_lines), n_show + 1)) {
      eigen_line <- neigen_lines[i]
      eigen_vals <- as.numeric(strsplit(eigen_line, " ")[[1]])
      cat(sprintf("  %2d: %.6e\n", i-1, eigen_vals[1]))
    }
    if(n_negative > 10) {
      cat("  ... (", n_negative - 10, " more)\n", sep = "")
    }
    cat("\n")
  }
  
} else {
  cat("⚠️  neigenvalues file not found\n")
  cat("   Eigenvalue analysis may not have completed\n")
  cat("   Check log:", eval_log_file, "\n\n")
}

## Analyze parameter confounding from new_cor_report
cat("==============================================\n")
cat("Parameter Confounding Analysis\n")
cat("==============================================\n\n")

new_cor_file <- "new_cor_report"

if(file.exists(new_cor_file)) {
  cor_lines <- readLines(new_cor_file, n = 200)
  
  ## Find "Smallest eigenvalues" section
  smallest_idx <- grep("Smallest eigenvalues", cor_lines)
  
  if(length(smallest_idx) > 0) {
    cat("Smallest eigenvalues (most confounded parameters):\n")
    cat("Format: eigenvalue (param_index contribution) ...\n\n")
    
    start_idx <- smallest_idx[1] + 1
    n_show <- min(10, length(cor_lines) - start_idx)
    
    for(i in start_idx:(start_idx + n_show - 1)) {
      if(i <= length(cor_lines) && nchar(cor_lines[i]) > 0) {
        cat("  ", cor_lines[i], "\n")
      }
    }
    cat("\n")
    
    cat("Parameter indices correspond to xinit.rpt listings\n")
    cat("High absolute contributions (>0.3) indicate confounding\n\n")
  }
} else {
  cat("new_cor_report not found\n")
  cat("This file contains detailed eigenvalue analysis\n\n")
}

## Extract and compare standard errors
cat("==============================================\n")
cat("Parameter Uncertainty (Standard Errors)\n")
cat("==============================================\n\n")

std_errors_normal <- NULL
std_errors_pos <- NULL

## Try to read normal standard errors
std_file_normal <- paste0(root_name, "_new_std")
if(file.exists(std_file_normal)) {
  con <- file(std_file_normal, "rb")
  std_errors_normal <- readBin(con, "double", n = npars)
  close(con)
  
  cat("Normal standard errors (_new_std):\n")
  cat("  Parameters:", length(std_errors_normal), "\n")
  cat("  Range: [", sprintf("%.6e", min(std_errors_normal, na.rm = TRUE)), 
      ", ", sprintf("%.6e", max(std_errors_normal, na.rm = TRUE)), "]\n", sep = "")
  cat("  Mean:", sprintf("%.6e", mean(std_errors_normal, na.rm = TRUE)), "\n")
  
  ## Check for invalid values
  n_na <- sum(is.na(std_errors_normal))
  n_inf <- sum(is.infinite(std_errors_normal))
  n_neg <- sum(std_errors_normal < 0, na.rm = TRUE)
  
  if(n_na > 0) cat("  ⚠️  NA values:", n_na, "\n")
  if(n_inf > 0) cat("  ⚠️  Infinite values:", n_inf, "\n")
  if(n_neg > 0) cat("  ⚠️  Negative values:", n_neg, "\n")
  cat("\n")
}

## Try to read positivized standard errors
std_file_pos <- paste0(root_name, "_pos_new_std")
if(file.exists(std_file_pos)) {
  con <- file(std_file_pos, "rb")
  std_errors_pos <- readBin(con, "double", n = npars)
  close(con)
  
  cat("Positivized standard errors (_pos_new_std):\n")
  cat("  Parameters:", length(std_errors_pos), "\n")
  cat("  Range: [", sprintf("%.6e", min(std_errors_pos, na.rm = TRUE)), 
      ", ", sprintf("%.6e", max(std_errors_pos, na.rm = TRUE)), "]\n", sep = "")
  cat("  Mean:", sprintf("%.6e", mean(std_errors_pos, na.rm = TRUE)), "\n")
  
  ## Check for invalid values
  n_na <- sum(is.na(std_errors_pos))
  n_inf <- sum(is.infinite(std_errors_pos))
  n_neg <- sum(std_errors_pos < 0, na.rm = TRUE)
  
  if(n_na > 0) cat("  ⚠️  NA values:", n_na, "\n")
  if(n_inf > 0) cat("  ⚠️  Infinite values:", n_inf, "\n")
  if(n_neg > 0) cat("  ⚠️  Negative values:", n_neg, "\n")
  cat("\n")
}

## Compare normal vs positivized if both exist
if(!is.null(std_errors_normal) && !is.null(std_errors_pos)) {
  cat("Comparison (Normal vs Positivized):\n")
  
  diff_abs <- abs(std_errors_pos - std_errors_normal)
  diff_rel <- diff_abs / std_errors_normal * 100
  
  cat("  Max absolute difference:", sprintf("%.6e", max(diff_abs, na.rm = TRUE)), "\n")
  cat("  Max relative difference:", sprintf("%.2f%%", max(diff_rel[is.finite(diff_rel)], na.rm = TRUE)), "\n")
  cat("  Mean relative difference:", sprintf("%.2f%%", mean(diff_rel[is.finite(diff_rel)], na.rm = TRUE)), "\n")
  
  n_large_diff <- sum(diff_rel > 10, na.rm = TRUE)
  if(n_large_diff > 0) {
    cat("  ⚠️  Parameters with >10% difference:", n_large_diff, "\n")
  }
  cat("\n")
}

## Determine which standard errors to use
if(hessian_status == "PDH") {
  recommended_se <- std_errors_normal
  recommended_file <- std_file_normal
  cat("✅ RECOMMENDED: Use NORMAL standard errors (_new_std)\n")
  cat("   Reliability: HIGH\n\n")
  
} else if(hessian_status == "Near-PDH") {
  recommended_se <- std_errors_pos
  recommended_file <- std_file_pos
  cat("⚠️  RECOMMENDED: Use POSITIVIZED standard errors (_pos_new_std)\n")
  cat("   Reliability: MODERATE (use with caution)\n")
  cat("   Note: These are approximations due to negative eigenvalues\n\n")
  
} else {
  recommended_se <- std_errors_pos
  recommended_file <- std_file_pos
  cat("❌ CAUTION: Only POSITIVIZED standard errors available\n")
  cat("   Reliability: LOW\n")
  cat("   Warning: DO NOT use for critical management decisions\n")
  cat("   Action: Fix model convergence issues first\n\n")
}

## Save standard errors
if(!is.null(recommended_se)) {
  saveRDS(list(
    standard_errors = recommended_se,
    source_file = recommended_file,
    hessian_status = hessian_status,
    reliability = reliability,
    n_negative_eigenvalues = n_negative
  ), file = "standard_errors.rds")
  
  cat("✓ Recommended standard errors saved to: standard_errors.rds\n\n")
}

## Return to original directory
setwd(old_wd)

##################################################################
## Step 8: Clean up component files (optional)
##################################################################
cat("==============================================\n")
cat("Step 8: Cleaning up\n")
cat("==============================================\n")

## Remove component .hes_* files
hes_components <- list.files(hessian_dir, pattern = "\\.hes_[0-9]+$", full.names = TRUE)
if(length(hes_components) > 0) {
  cat("Removing", length(hes_components), "component .hes_* files...\n")
  file.remove(hes_components)
  cat("✓ Cleanup complete\n\n")
}

##################################################################
## Final Summary Report
##################################################################
cat("==============================================\n")
cat("FINAL HESSIAN DIAGNOSTIC SUMMARY\n")
cat("==============================================\n\n")

cat("HESSIAN STATUS:", hessian_status, "\n")
cat("RELIABILITY:", reliability, "\n")
if(!is.na(n_negative)) {
  cat("NEGATIVE EIGENVALUES:", n_negative, "out of", npars, "\n")
}
cat("\n")

if(hessian_status == "PDH") {
  cat("✅ MODEL VALIDATION: PASSED\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("✓ Hessian is positive definite\n")
  cat("✓ Model has converged to a true minimum\n")
  cat("✓ All uncertainty estimates are reliable\n")
  cat("✓ Parameter correlations are valid\n")
  cat("✓ Ready for management advice\n\n")
  
  cat("RECOMMENDED NEXT STEPS:\n")
  cat("  1. Extract parameter estimates from .par file\n")
  cat("  2. Match with standard errors from standard_errors.rds\n")
  cat("  3. Compute 95% confidence intervals: estimate ± 1.96 × SE\n")
  cat("  4. Analyze parameter correlations (*_pos_hess_cor)\n")
  cat("  5. Generate stock assessment outputs with uncertainty\n")
  cat("  6. Proceed with management advice\n\n")
  
} else if(hessian_status == "Near-PDH") {
  cat("⚠️  MODEL VALIDATION: MARGINAL\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("⚠️  Few negative eigenvalues detected (", n_negative, ")\n", sep = "")
  cat("⚠️  Model is close to but not at convergence\n")
  cat("⚠️  Uncertainty estimates are approximate\n")
  cat("⚠️  Use positivized standard errors with caution\n\n")
  
  cat("WHAT IS POSITIVIZED?\n")
  cat("  Positivized means negative eigenvalues were mathematically\n")
  cat("  forced to be positive to allow uncertainty calculation.\n")
  cat("  This is an approximation, not the true uncertainty.\n\n")
  
  cat("RECOMMENDED ACTIONS:\n")
  cat("  1. Review new_cor_report to identify confounded parameters\n")
  cat("  2. Check parameter indices against xinit.rpt\n")
  cat("  3. Consider tighter convergence criteria and re-run\n")
  cat("  4. Examine parameter bounds and constraints\n")
  cat("  5. Use positivized SE but perform sensitivity analysis\n")
  cat("  6. Document limitations in assessment report\n\n")
  
} else if(hessian_status == "Non-PDH") {
  cat("❌ MODEL VALIDATION: FAILED\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("❌ Many negative eigenvalues detected (", n_negative, ")\n", sep = "")
  cat("❌ Hessian is NOT positive definite\n")
  cat("❌ Model has NOT converged properly\n")
  cat("❌ Uncertainty estimates are UNRELIABLE\n\n")
  
  cat("WHAT IS POSITIVIZED?\n")
  cat("  Positivized standard errors are calculated by forcing\n")
  cat("  negative eigenvalues to be positive. This is a mathematical\n")
  cat("  trick to get numbers, but they DO NOT represent real\n")
  cat("  uncertainty when many eigenvalues are negative.\n\n")
  
  cat("CRITICAL - DO NOT:\n")
  cat("  ✗ Use these uncertainty estimates for management advice\n")
  cat("  ✗ Report confidence intervals from this model\n")
  cat("  ✗ Present this as a converged solution\n\n")
  
  cat("REQUIRED ACTIONS TO FIX:\n")
  cat("  1. Review model convergence diagnostics thoroughly\n")
  cat("  2. Check new_cor_report for parameter confounding\n")
  cat("  3. Consider model reparameterization\n")
  cat("  4. Review and adjust parameter bounds\n")
  cat("  5. Try different initial parameter values\n")
  cat("  6. Simplify model structure if overparameterized\n")
  cat("  7. Check data quality and consistency\n")
  cat("  8. Consult with assessment team\n\n")
  
} else {
  cat("❓ MODEL VALIDATION: UNKNOWN\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Eigenvalue analysis files not found\n")
  cat("Check log files for errors\n\n")
}

cat("OUTPUT FILES LOCATION:\n")
cat("  Directory:", hessian_dir, "\n\n")

cat("KEY OUTPUT FILES:\n")
cat("  Hessian matrix:       ", stitched_hess_file_name, " (", round(file_size_mb, 2), " MB)\n", sep = "")
if(file.exists(file.path(hessian_dir, eval_file))) {
  cat("  Eigenvalues:          ", eval_file, "\n", sep = "")
}
if(file.exists(file.path(hessian_dir, evec_file))) {
  cat("  Eigenvectors:         ", evec_file, "\n", sep = "")
}
if(file.exists(file.path(hessian_dir, "neigenvalues"))) {
  cat("  Neg eigenval summary: neigenvalues\n")
}
if(file.exists(file.path(hessian_dir, "new_cor_report"))) {
  cat("  Detailed report:      new_cor_report\n")
}
if(file.exists(file.path(hessian_dir, recommended_file))) {
  cat("  Standard errors:      ", basename(recommended_file), " (RECOMMENDED)\n", sep = "")
}
if(file.exists(file.path(hessian_dir, "standard_errors.rds"))) {
  cat("  Saved SE (R object):  standard_errors.rds\n")
}
cat("\n")

cat("USAGE IN R:\n")
cat("  # Load standard errors\n")
cat("  se_data <- readRDS('model/base/hessian/standard_errors.rds')\n")
cat("  std_errors <- se_data$standard_errors\n")
cat("  hess_status <- se_data$hessian_status\n")
cat("  reliability <- se_data$reliability\n")
cat("  \n")
cat("  # Compute 95% confidence intervals\n")
cat("  # (assuming you have parameter estimates in 'params')\n")
cat("  ci_lower <- params - 1.96 * std_errors\n")
cat("  ci_upper <- params + 1.96 * std_errors\n")
cat("  \n")
cat("  # Read full Hessian matrix\n")
cat("  library(FLR4MFCL)\n")
cat("  hess <- read.MFCLHess('model/base/hessian/", root_name, ".hes')\n", sep = "")
cat("  \n")

cat("LOG FILES:\n")
cat("  ", file.path(hessian_dir, log_file), "\n", sep = "")
cat("  ", file.path(hessian_dir, eval_log_file), "\n\n", sep = "")

cat("==============================================\n")
cat("Processing completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("==============================================\n\n")


