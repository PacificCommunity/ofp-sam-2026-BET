#!/usr/bin/env Rscript
## Diagnostic tool to inspect MFCL Hessian binary file format

args <- commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  cat("Usage: Rscript inspect_hessian.R <hessian_file.hes>\n")
  quit(status = 1)
}

hes_file <- args[1]

if(!file.exists(hes_file)) {
  stop("File not found: ", hes_file)
}

cat("==============================================\n")
cat("MFCL Hessian File Inspector\n")
cat("==============================================\n\n")
cat("File:", hes_file, "\n")
cat("Size:", round(file.size(hes_file) / 1024 / 1024, 2), "MB\n\n")

## Open binary file
con <- file(hes_file, open = "rb")

## Try reading as different integer sizes
cat("Trying to read header as 4-byte integers:\n")
header_int32 <- readBin(con, "integer", n = 3, size = 4)
cat("  Values:", paste(header_int32, collapse = ", "), "\n")

## Reset and try as raw bytes
seek(con, 0)
raw_bytes <- readBin(con, "raw", n = 12)
cat("\nFirst 12 bytes (hex):\n")
cat("  ", paste(sprintf("%02x", as.integer(raw_bytes)), collapse = " "), "\n")

## Try little-endian interpretation
cat("\nAs little-endian integers:\n")
int1 <- sum(as.integer(raw_bytes[1:4]) * c(1, 256, 256^2, 256^3))
int2 <- sum(as.integer(raw_bytes[5:8]) * c(1, 256, 256^2, 256^3))
int3 <- sum(as.integer(raw_bytes[9:12]) * c(1, 256, 256^2, 256^3))
cat("  Value 1:", int1, "\n")
cat("  Value 2:", int2, "\n")
cat("  Value 3:", int3, "\n")

## Try big-endian interpretation
cat("\nAs big-endian integers:\n")
int1_be <- sum(as.integer(raw_bytes[4:1]) * c(1, 256, 256^2, 256^3))
int2_be <- sum(as.integer(raw_bytes[8:5]) * c(1, 256, 256^2, 256^3))
int3_be <- sum(as.integer(raw_bytes[12:9]) * c(1, 256, 256^2, 256^3))
cat("  Value 1:", int1_be, "\n")
cat("  Value 2:", int2_be, "\n")
cat("  Value 3:", int3_be, "\n")

## Try reading first value as double to see if entire file is doubles
seek(con, 0)
first_doubles <- readBin(con, "double", n = 6)
cat("\nFirst 6 values as doubles:\n")
for(i in 1:6) {
  cat("  ", i, ":", first_doubles[i], "\n")
}

## Check if npars around 3000 makes sense
cat("\n==============================================\n")
cat("Interpretation check:\n")
cat("==============================================\n")

if(header_int32[1] > 2000 && header_int32[1] < 5000) {
  npars <- header_int32[1]
  start_row <- header_int32[2]
  end_row <- header_int32[3]
  
  cat("Likely correct interpretation (standard format):\n")
  cat("  npars:", npars, "\n")
  cat("  start_row:", start_row, "\n")
  cat("  end_row:", end_row, "\n")
  cat("  nrows:", end_row - start_row + 1, "\n")
  
  nrows <- end_row - start_row + 1
  expected_size <- 12 + nrows * npars * 8
  actual_size <- file.size(hes_file)
  
  cat("\nExpected size: ", round(expected_size / 1024 / 1024, 2), " MB\n", sep="")
  cat("Actual size: ", round(actual_size / 1024 / 1024, 2), " MB\n", sep="")
  
  if(abs(expected_size - actual_size) < 1000) {
    cat("✓ Size matches - format is correct!\n")
  } else {
    cat("✗ Size mismatch - format may be different\n")
  }
  
} else if(int1 > 2000 && int1 < 5000) {
  cat("Header might be little-endian:\n")
  cat("  npars:", int1, "\n")
  cat("  start_row:", int2, "\n")
  cat("  end_row:", int3, "\n")
  
} else if(int1_be > 2000 && int1_be < 5000) {
  cat("Header might be big-endian:\n")
  cat("  npars:", int1_be, "\n")
  cat("  start_row:", int2_be, "\n")
  cat("  end_row:", int3_be, "\n")
  
} else {
  cat("⚠️  Cannot identify standard header format\n")
  cat("File might be in different format or corrupted\n")
}

## Try reading first row of data (after header)
seek(con, 12)
cat("\n==============================================\n")
cat("First row sample (after 12-byte header):\n")
cat("==============================================\n")

sample_values <- readBin(con, "double", n = min(10, header_int32[1]))
cat("First", length(sample_values), "values:\n")
for(i in 1:length(sample_values)) {
  cat("  ", i, ":", format(sample_values[i], scientific = TRUE), "\n")
}

close(con)

cat("\n==============================================\n")
