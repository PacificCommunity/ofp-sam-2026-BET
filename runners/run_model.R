## ============================================================
## MFCL run script (tidied) 
## - auto-detect .frq
## - auto-detect latest .par and write +1 .par
## - copy base inputs -> model_dir
## - run MFCL
## - save model_info + payload
## - cleanup
## ============================================================

## -------------------------
## 0) Libraries + sources
## -------------------------
library(FLR4MFCL)
library(CondorBox)
source("tools/model_payload.R")
source("tools/post_hessian.R")
source("tools/condor_archive_cleanup.R")

## -------------------------
## 1) Environment + paths
## -------------------------
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
base_dir     <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir    <- Sys.getenv("model_dir", "model/base")
description  <- Sys.getenv("description", "")
config_summary <- Sys.getenv("config_summary", "")
model_hessian <- tolower(Sys.getenv("model_hessian", Sys.getenv("hessian", "0"))) %in% c("1", "true", "yes", "y")

n_mixing_periods <- as.numeric(Sys.getenv("n_mixing_periods", ""))
min_year         <- as.numeric(Sys.getenv("min_year", ""))

## PROGRAM_PATH used by some FLR4MFCL utilities (keep as you had it)
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))

project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)

if (!dir.exists(base_dir_abs)) {
  stop("Base inputs directory does not exist: ", base_dir_abs)
}

## -------------------------
## 2) Auto-detect .frq
## -------------------------
frq_files <- list.files(base_dir_abs, pattern = "\\.frq$", full.names = FALSE)

if (length(frq_files) == 0) {
  stop("No .frq file found in ", base_dir_abs)
}
if (length(frq_files) > 1) {
  warning("Multiple .frq files found; using first: ", frq_files[1])
}
frq_file <- frq_files[1]
cat("Found .frq file:", frq_file, "\n")

## -------------------------
## 3) Auto-detect latest .par and set output as +1
## -------------------------
par_files <- list.files(base_dir_abs, pattern = "\\.par$", full.names = FALSE)

if (length(par_files) == 0) {
  stop("No .par files found in ", base_dir_abs)
}

par_nums <- suppressWarnings(as.numeric(gsub("\\.par$", "", par_files)))
if (any(is.na(par_nums))) {
  stop(
    "Some .par files do not follow '<number>.par': ",
    paste(par_files[is.na(par_nums)], collapse = ", ")
  )
}

last_par_num <- max(par_nums, na.rm = TRUE)
par_in  <- paste0(last_par_num, ".par")
par_out <- paste0(last_par_num + 1, ".par")

cat("Using input .par :", par_in,  "\n")
cat("Writing output .par:", par_out, "\n")

## Optional safety: refuse to overwrite output if it already exists in base inputs
par_out_abs <- file.path(base_dir_abs, par_out)
if (file.exists(par_out_abs)) {
  stop("Output .par already exists in base inputs: ", par_out_abs)
}

## -------------------------
## 4) Switches
## -------------------------
defaultswitch <- paste(
  "-switch 1",
  "1 1 10",
  # "1 145 5",
  # "1 223 3039",
  # "1 224 3067",
  sep = " "
)

## -------------------------
## 5) Build mfcl_commands
## Notes:
## - run_commands() expects the executable to be invoked relative to work_dir,
##   so we prepend "../../" unless using "./doitall.sh".
## - We keep your behaviour: allow overriding via Sys.getenv("mfcl_commands").
## -------------------------
mfcl_commands_raw <- Sys.getenv(
  "mfcl_commands",
  paste(program_path, frq_file, par_in, par_out, defaultswitch)
)

mfcl_commands <- if (identical(mfcl_commands_raw, "./doitall.sh")) {
  mfcl_commands_raw
} else {
  paste0("../../", mfcl_commands_raw)
}

cat("Running MFCL with commands:\n", mfcl_commands, "\n")
cat("Model post-hessian:", model_hessian, "\n")

## -------------------------
## 6) Prepare model directory and copy inputs
## -------------------------
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

files_to_copy <- list.files(base_dir_abs, full.names = TRUE, all.files = TRUE, no.. = TRUE)
file.copy(files_to_copy, to = model_dir, overwrite = TRUE, recursive = TRUE)

cat("Base inputs directory:", base_dir_abs, "\n")
cat("Model directory      :", model_dir, "\n")

## When using phase-based legacy scripts (doitall.sh), copied .par phase files
## can short-circuit all phases and silently skip optimization. Remove them to
## force a fresh run from bet.ini in the model directory.
if (identical(mfcl_commands_raw, "./doitall.sh")) {
  phase_pars <- list.files(model_dir, pattern = "^[0-9]{2}\\.par$", full.names = TRUE)
  if (length(phase_pars) > 0) {
    unlink(phase_pars, force = TRUE)
    cat("Removed", length(phase_pars), "existing phase .par files before doitall run\n")
  }
}

## -------------------------
## 7) Run MFCL
## -------------------------
run_commands(
  commands  = mfcl_commands,
  work_dirs = model_dir,
  save_log  = TRUE,
  parallel  = FALSE,
  verbose   = TRUE,
  log_file  = file.path(model_dir, "mfcl_log.txt")
)

## -------------------------
## 7b) Optional post-hessian
## -------------------------
final_model_par <- mp_final_par(model_dir)
post_hessian_input_par <- if (!is.null(final_model_par) && file.exists(final_model_par)) {
  basename(final_model_par)
} else {
  par_out
}

hessian_summary <- mp_run_post_hessian(
  work_dir = model_dir,
  program_path_abs = file.path(project_root, program_path),
  program_path = program_path,
  frq_file = frq_file,
  input_par = post_hessian_input_par,
  project_root = project_root,
  requested = model_hessian
)

## -------------------------
## 8) Save model run info
## -------------------------
info_list <- list(
  description      = description,
  config_summary   = config_summary,
  program_path     = program_path,
  mfcl_commands    = mfcl_commands_raw,  # raw (without ../../) is usually easier to inspect later
  frq_file         = frq_file,
  par_in           = par_in,
  par_out          = post_hessian_input_par,
  base_dir         = base_dir,
  model_dir        = model_dir,
  n_mixing_periods = n_mixing_periods,
  min_year         = min_year,
  hessian          = hessian_summary
)

saveRDS(
  info_list,
  file = file.path(model_dir, "model_info.rds"),
  compress = "xz"
)

## -------------------------
## 9) Build + save payload
## -------------------------
payload <- mp_build_model_payload(model_dir, tag_report_year1 = min_year)

saveRDS(
  payload,
  file = file.path(model_dir, "model_payload.rds"),
  compress = "xz"
)

## -------------------------
## 10) Cleanup top-level artifacts (keep only core files)
## -------------------------
keep_top <- c(
  "model_payload.rds",
  "model_info.rds",
  "fishery_map.R",
  "fishery_map.r",
  "tag_rep_map.r",
  "tag_rep_map.R"
)

if (!is.null(payload$files$par) && file.exists(payload$files$par)) {
  keep_top <- c(keep_top, basename(payload$files$par))
}

deleted_n <- mp_cleanup_files(model_dir, keep = keep_top, recursive = FALSE)
cat("Cleanup removed", deleted_n, "non-core top-level files in", model_dir, "\n")

cb_condor_keep_only_model_cleanup()

cat("✅ Model run completed for", basename(model_dir), "\n")
