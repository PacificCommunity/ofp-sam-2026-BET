#!/usr/bin/env Rscript
## Collate Hessian calculation results using MFCL's built-in stitching
## Uses parest_flags(145)=11 for stitching parallel Hessian components
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
cat("MFCL Hessian Stitching Utility\n")
cat("Using MFCL built-in routine (parest_flags 145=11)\n")
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
## Format: ./mfclo64 bet.frq 12.par ttt -switch 1 1 145 11
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

## Run MFCL
log_file <- "mfcl_stitch_log.txt"
cat("Running MFCL stitching (output -> ", log_file, ")...\n", sep = "")

system2(
  command = mfcl_exe,
  args = c(frq_file, par_file, "ttt", "-switch", "1", "1", "145", "11"),
  stdout = log_file,
  stderr = log_file
)

setwd(old_wd)

cat("✓ MFCL stitching completed\n\n")

##################################################################
## Step 5: Verify output
##################################################################
cat("==============================================\n")
cat("Step 5: Verifying result\n")
cat("==============================================\n")

parallel_hess_file <- file.path(hessian_dir, "parallel_hess")

if(file.exists(parallel_hess_file)) {
  file_size_mb <- file.size(parallel_hess_file) / 1024 / 1024
  cat("✅ SUCCESS: parallel_hess file created\n")
  cat("   File:", parallel_hess_file, "\n")
  cat("   Size:", round(file_size_mb, 2), "MB\n\n")
  
  ## Read and verify the file
  con <- file(parallel_hess_file, open = "rb")
  header <- readBin(con, "integer", n = 3)
  close(con)
  
  cat("   Header: [npars=", header[1], ", start=", header[2], ", end=", header[3], "]\n", sep = "")
  
  if(header[1] == npars && header[2] == 1 && header[3] == npars) {
    cat("   ✓ Complete Hessian matrix (", npars, "×", npars, ")\n\n", sep = "")
    
    ## Save verification info
    stitch_info <- list(
      npars = npars,
      n_parts = n_parts,
      start_positions = start_positions,
      parallel_hess_file = parallel_hess_file,
      stitch_time = Sys.time(),
      is_complete = TRUE,
      file_size_mb = file_size_mb
    )
    
    saveRDS(stitch_info, file = file.path(hessian_dir, "stitch_info.rds"))
    
  } else {
    cat("   ⚠️  Unexpected header values\n\n")
  }
  
} else {
  cat("❌ ERROR: parallel_hess file not created\n")
  cat("   Check log file:", file.path(hessian_dir, log_file), "\n\n")
  quit(status = 1)
}

##################################################################
## Step 6: Clean up component files (optional)
##################################################################
cat("==============================================\n")
cat("Step 6: Cleaning up\n")
cat("==============================================\n")

## Remove component .hes_* files
hes_components <- list.files(hessian_dir, pattern = "\\.hes_[0-9]+$", full.names = TRUE)
if(length(hes_components) > 0) {
  cat("Removing", length(hes_components), "component .hes_* files...\n")
  file.remove(hes_components)
  cat("✓ Cleanup complete\n\n")
}

cat("==============================================\n")
cat("✅ Hessian stitching complete!\n")
cat("==============================================\n")
cat("Output file:", parallel_hess_file, "\n")
cat("Size:", round(file_size_mb, 2), "MB\n")
cat("\nThe parallel_hess file can now be used for:\n")
cat("  - Parameter uncertainty estimation\n")
cat("  - Variance-covariance matrix calculation\n")
cat("  - Model diagnostics\n\n")
