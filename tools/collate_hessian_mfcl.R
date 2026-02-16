#!/usr/bin/env Rscript
## Collate Hessian calculation results using MFCL's built-in stitching
## Uses parest_flags(145)=11 for stitching parallel Hessian components
## Uses parest_flags(145)=5 for eigenvalue decomposition
## Performs comprehensive Hessian diagnostics and uncertainty analysis
## Reference: MFCL Hessian diagnostics documentation
## Usage:
##   Rscript collate_hessian_mfcl.R [model_dir_or_hessian_dir]
##   Rscript collate_hessian_mfcl.R --all
##   Rscript collate_hessian_mfcl.R        # auto-detect all model/*/hessian

library(FLR4MFCL)

## Get model directory from command line
args <- commandArgs(trailingOnly = TRUE)
all_args <- commandArgs(trailingOnly = FALSE)
launch_wd <- getwd()

## Resolve current script path for batch self-invocation
script_arg <- grep("^--file=", all_args, value = TRUE)
script_path <- if(length(script_arg) > 0) sub("^--file=", "", script_arg[1]) else NA_character_
if(!is.na(script_path) && !grepl("^/", script_path)) {
  script_path <- file.path(launch_wd, script_path)
}
if(!is.na(script_path)) {
  script_path <- normalizePath(script_path, mustWork = FALSE)
}

## Auto-detect model directories that contain a hessian subdirectory
discover_model_dirs <- function(model_root = "model") {
  if(!dir.exists(model_root)) return(character(0))
  candidates <- list.dirs(model_root, recursive = FALSE, full.names = TRUE)
  candidates[dir.exists(file.path(candidates, "hessian"))]
}

## Batch mode:
## - no argument  -> process all model/* directories with hessian
## - --all / all  -> process all model/* directories with hessian
batch_mode <- length(args) == 0 || (length(args) > 0 && args[1] %in% c("--all", "all"))

if(batch_mode) {
  if(is.na(script_path)) {
    stop("Cannot resolve script path for batch mode self-invocation")
  }
  
  model_dirs <- discover_model_dirs("model")
  if(length(model_dirs) == 0) {
    stop("No model directories with hessian found under: model")
  }

  detected_cores <- parallel::detectCores(logical = TRUE)
  if(!is.finite(detected_cores) || is.na(detected_cores)) detected_cores <- 1L
  max_workers <- max(1L, as.integer(detected_cores) - 2L)
  workers <- min(length(model_dirs), max_workers)
  
  cat("==============================================\n")
  cat("Batch Hessian Collation Mode\n")
  cat("Detected", length(model_dirs), "model(s):\n")
  for(md in model_dirs) cat("  -", md, "\n")
  cat("Detected cores:", detected_cores, "\n")
  cat("Parallel workers (max cores-2):", workers, "\n")
  cat("==============================================\n\n")

  run_one_model <- function(md) {
    log_file <- file.path(md, "hessian", "batch_collate.log")
    status <- system2(
      command = "Rscript",
      args = c(script_path, md),
      stdout = log_file,
      stderr = log_file
    )
    list(
      model_dir = md,
      status = as.integer(status),
      log_file = log_file
    )
  }

  results <- parallel::mclapply(
    model_dirs,
    run_one_model,
    mc.cores = workers
  )
  results <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))

  failures <- results$model_dir[results$status != 0]
  successes <- results$model_dir[results$status == 0]

  if(length(successes) > 0) {
    cat("Completed models:\n")
    for(md in successes) cat("  ✓", md, "\n")
    cat("\n")
  }

  if(length(failures) > 0) {
    cat("Failed models:\n")
    for(md in failures) {
      log_file <- results$log_file[results$model_dir == md][1]
      cat("  ✗", md, "(see", log_file, ")\n")
    }
    cat("\n")
  }
  
  cat("\n==============================================\n")
  cat("Batch run finished\n")
  if(length(failures) == 0) {
    cat("All models completed successfully\n")
    cat("==============================================\n")
    quit(status = 0)
  } else {
    cat("Failed models:", length(failures), "\n")
    for(md in failures) cat("  -", md, "\n")
    cat("==============================================\n")
    quit(status = 1)
  }
}

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
  ## This branch is now only used if batch_mode is explicitly bypassed.
  model_dir <- Sys.getenv("model_dir", "model/base")
  hessian_dir <- file.path(model_dir, "hessian")
}

## Lock to absolute paths early to avoid setwd-relative path issues.
model_dir <- normalizePath(model_dir, mustWork = TRUE)
hessian_dir <- normalizePath(hessian_dir, mustWork = TRUE)

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
part_numbers <- part_numbers[order(part_numbers)]

## Validate part numbering per model (part count can differ by model)
if(any(duplicated(part_numbers))) {
  stop("Duplicate hessian_part detected: ", paste(unique(part_numbers[duplicated(part_numbers)]), collapse = ", "))
}

## Validate expected split count when available in metadata
nsplit_values <- suppressWarnings(as.integer(sapply(part_infos, function(x) {
  if(!is.null(x$nsplit)) x$nsplit else NA
})))
nsplit_values <- nsplit_values[is.finite(nsplit_values)]

if(length(nsplit_values) > 0) {
  expected_nsplit <- unique(nsplit_values)
  if(length(expected_nsplit) > 1) {
    stop("Inconsistent nsplit values across part metadata: ", paste(expected_nsplit, collapse = ", "))
  }
  expected_nsplit <- expected_nsplit[1]
  missing_parts <- setdiff(seq_len(expected_nsplit), part_numbers)
  if(length(missing_parts) > 0) {
    stop(
      "Missing hessian parts for this model. Expected 1..", expected_nsplit,
      ", found: ", paste(part_numbers, collapse = ", "),
      ", missing: ", paste(missing_parts, collapse = ", ")
    )
  }
} else {
  ## Fallback check using observed max part number if nsplit is absent
  missing_parts <- setdiff(seq_len(max(part_numbers)), part_numbers)
  if(length(missing_parts) > 0) {
    warning(
      "Potential missing hessian parts (nsplit not available). Found parts: ",
      paste(part_numbers, collapse = ", "),
      "; missing in 1..max(part): ",
      paste(missing_parts, collapse = ", ")
    )
  }
}

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
}

## Read eigenvalue summary for PDH status
neigen_file <- "neigenvalues"
n_negative <- NA_integer_
n_total <- npars
if(file.exists(neigen_file)) {
  first_line <- suppressWarnings(as.integer(strsplit(readLines(neigen_file, n = 1), " +")[[1]]))
  first_line <- first_line[is.finite(first_line)]
  if(length(first_line) >= 1) {
    n_negative <- first_line[1]
    if(length(first_line) >= 2) n_total <- first_line[2]
  }
}

hessian_status <- "Unknown"
reliability <- "UNKNOWN"
if(!is.na(n_negative) && n_negative == 0) {
  hessian_status <- "PDH"
  reliability <- "HIGH"
} else if(!is.na(n_negative) && n_negative < n_total * 0.01) {
  hessian_status <- "Near-PDH"
  reliability <- "MODERATE"
} else if(!is.na(n_negative)) {
  hessian_status <- "Non-PDH"
  reliability <- "LOW"
}

if(hessian_status == "PDH" && !is.null(std_errors_normal)) {
  recommended_se <- std_errors_normal
  recommended_file <- std_file_normal
} else if(!is.null(std_errors_pos)) {
  recommended_se <- std_errors_pos
  recommended_file <- std_file_pos
} else {
  recommended_se <- NULL
  recommended_file <- NA_character_
}

## Save standard errors
standard_errors_info <- list(
  source_file = recommended_file,
  available = !is.null(recommended_se),
  values = recommended_se
)

##################################################################
## Step 7b: Build compact diagnostics info object (hessian_cal.R)
##################################################################
## Resolve hessian_cal.R path relative to this script
hessian_cal_script <- NA_character_
if(!is.na(script_path)) {
  tools_dir <- dirname(normalizePath(script_path))
  candidate <- file.path(tools_dir, "hessian_cal.R")
  if(file.exists(candidate)) hessian_cal_script <- candidate
}

if(is.na(hessian_cal_script)) {
  candidate <- file.path(old_wd, "tools", "hessian_cal.R")
  if(file.exists(candidate)) hessian_cal_script <- candidate
}

hessian_diagnostics_info <- NULL
hessian_diag_build_log <- character(0)

if(!is.na(hessian_cal_script) && file.exists(hessian_cal_script)) {
  info_build_log <- file.path(hessian_dir, "hessian_cal_build.log")
  info_tmp <- tempfile(pattern = "hessian_diagnostics_", fileext = ".rds")
  info_status <- system2(
    command = "Rscript",
    args = c(hessian_cal_script, hessian_dir, info_tmp),
    stdout = info_build_log,
    stderr = info_build_log
  )
  hessian_diag_build_log <- info_build_log
  
  if(identical(info_status, 0L)) {
    hessian_diagnostics_info <- tryCatch(readRDS(info_tmp), error = function(e) NULL)
    if(file.exists(info_tmp)) file.remove(info_tmp)
    cat("✓ Built integrated diagnostics payload\n")
  } else {
    cat("⚠ Failed to build diagnostics payload via hessian_cal.R\n")
  }
} else {
  cat("⚠ hessian_cal.R not found; skipping diagnostics info generation\n")
}

##################################################################
## Step 8: Save unified hessian_info and clean up
##################################################################
hessian_info <- list(
  meta = list(
    model_dir = normalizePath(model_dir),
    hessian_dir = normalizePath(hessian_dir),
    root_name = root_name,
    created_at = Sys.time()
  ),
  stitch = list(
    npars = npars,
    n_parts = n_parts,
    start_positions = start_positions,
    part_ranges = part_ranges,
    stitched_hessian_file = stitched_hess_file,
    is_complete = is_complete,
    file_size_mb = file_size_mb,
    expected_size_mb = expected_data_size_mb,
    size_diff_mb = size_diff_mb,
    size_diff_pct = size_diff_pct
  ),
  eigen = list(
    n_negative_eigenvalues = n_negative,
    n_total_eigenvalues = n_total,
    hessian_status = hessian_status,
    reliability = reliability
  ),
  standard_errors = standard_errors_info,
  diagnostics = hessian_diagnostics_info,
  diagnostics_build_log = hessian_diag_build_log
)

saveRDS(hessian_info, file = file.path(hessian_dir, "hessian_info.rds"))
cat("✓ Saved: hessian_info.rds\n")

## Remove component .hes_* files
hes_components <- list.files(hessian_dir, pattern = "\\.hes_[0-9]+$", full.names = TRUE)
if(length(hes_components) > 0) {
  file.remove(hes_components)
  cat("✓ Removed component files:", length(hes_components), "\n")
}

## Optional compact cleanup: keep only compact artifacts after successful stitch
compact_cleanup <- tolower(Sys.getenv("hessian_compact_cleanup", "true")) %in% c("1", "true", "yes", "y")
if (isTRUE(compact_cleanup)) {
  keep_top <- c(
    "hessian_info.rds",
    "stitch_info.rds",
    basename(log_file),
    basename(eval_log_file),
    "hessian_cal_build.log"
  )

  ## Remove raw part directories once hessian_info.rds has been created.
  if (length(part_dirs) > 0) {
    unlink(part_dirs, recursive = TRUE, force = TRUE)
  }

  top_files <- list.files(hessian_dir, full.names = TRUE, recursive = FALSE, all.files = FALSE, no.. = TRUE)
  top_files <- top_files[file.info(top_files)$isdir %in% FALSE]
  keep_paths <- normalizePath(file.path(hessian_dir, keep_top), winslash = "/", mustWork = FALSE)
  top_paths <- normalizePath(top_files, winslash = "/", mustWork = FALSE)
  rm_files <- top_files[!(top_paths %in% keep_paths)]
  if (length(rm_files) > 0) {
    file.remove(rm_files)
  }
  cat("✓ Compact cleanup complete in hessian directory\n")
}

## Return to original directory
setwd(old_wd)

cat("\nSummary\n")
cat("  status:", hessian_status, "\n")
cat("  reliability:", reliability, "\n")
cat("  negative eigenvalues:", ifelse(is.na(n_negative), "NA", n_negative), "/", n_total, "\n")
cat("  saved:", file.path(hessian_dir, "hessian_info.rds"), "\n")
cat("  log files:\n")
cat("    ", file.path(hessian_dir, log_file), "\n", sep = "")
cat("    ", file.path(hessian_dir, eval_log_file), "\n", sep = "")
