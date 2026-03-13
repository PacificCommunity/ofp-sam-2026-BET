#!/usr/bin/env Rscript

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required. Install with install.packages('testthat').")
}

testthat::test_dir("tests/testthat", reporter = "summary")

