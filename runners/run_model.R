## ============================================================
## MFCL run script (tidied) 
## - auto-detect .frq
## - auto-detect .ini/.par start state
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
source("tools/input_change_metadata.R")
source("tools/input_recipe_runner.R")

## -------------------------
## 1) Environment + paths
## -------------------------
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
base_dir     <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir    <- Sys.getenv("model_dir", "model/base")
description  <- Sys.getenv("description", "")
config_summary <- Sys.getenv("config_summary", "")
model_hessian <- tolower(Sys.getenv("model_hessian", Sys.getenv("hessian", "0"))) %in% c("1", "true", "yes", "y")
prefer_par_start <- tolower(Sys.getenv("prefer_par_start", "1")) %in% c("1", "true", "yes", "y", "on")
allow_sensitivity_par_start <- tolower(Sys.getenv("allow_sensitivity_par_start", "0")) %in% c("1", "true", "yes", "y", "on")

n_mixing_periods <- as.numeric(Sys.getenv("n_mixing_periods", ""))
min_year         <- as.numeric(Sys.getenv("min_year", ""))

## PROGRAM_PATH used by some FLR4MFCL utilities (keep as you had it)
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))

project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
base_dir_abs <- ensure_input_dir_available(base_dir, project_root)

if (!dir.exists(base_dir_abs)) {
  stop("Base inputs directory does not exist: ", base_dir_abs)
}

input_change_metadata <- read_input_change_metadata(base_dir_abs)
input_change_tokens <- normalize_input_change_tokens(input_change_metadata$tokens)
recipe_env_enabled <- tolower(Sys.getenv("input_recipe_enabled", "0")) %in% c("1", "true", "yes", "y", "on")
sensitivity_input <- length(input_change_tokens) > 0 || isTRUE(recipe_env_enabled)

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
## 3) Auto-detect optional .ini and latest .par
## -------------------------
ini_files <- list.files(base_dir_abs, pattern = "\\.ini$", full.names = FALSE)
if (length(ini_files) > 1) {
  warning("Multiple .ini files found; using first: ", ini_files[1])
}
ini_file <- if (length(ini_files) > 0) ini_files[1] else NA_character_
if (!is.na(ini_file)) {
  cat("Found .ini file:", ini_file, "\n")
}

par_files <- list.files(base_dir_abs, pattern = "\\.par$", full.names = FALSE)

par_in <- NA_character_
par_out <- "01.par"
if (length(par_files) > 0) {
  par_stems <- sub("\\.par$", "", par_files)
  par_nums <- suppressWarnings(as.integer(par_stems))
  numeric_idx <- which(!is.na(par_nums) & grepl("^[0-9]+$", par_stems))

  if (length(numeric_idx) > 0) {
    last_idx <- numeric_idx[which.max(par_nums[numeric_idx])]
    last_par_num <- par_nums[[last_idx]]
    par_in <- par_files[[last_idx]]
    out_width <- max(nchar(par_stems[[last_idx]]), 2L)
    par_out <- paste0(sprintf(paste0("%0", out_width, "d"), last_par_num + 1L), ".par")
  } else {
    par_info <- file.info(file.path(base_dir_abs, par_files))
    par_in <- par_files[[which.max(par_info$mtime)]]
    par_out <- "01.par"
  }

  cat("Using input .par :", par_in, "\n")
  cat("Writing output .par:", par_out, "\n")
} else {
  cat("No .par file found in base inputs; run must start from .ini/doitall makepar.\n")
}

allow_par_start <- isTRUE(prefer_par_start) && (!isTRUE(sensitivity_input) || isTRUE(allow_sensitivity_par_start))
if (isTRUE(sensitivity_input) && !is.na(par_in) && isTRUE(prefer_par_start) && !isTRUE(allow_sensitivity_par_start)) {
  input_change_label <- if (length(input_change_tokens) > 0) paste(input_change_tokens, collapse = ", ") else "input recipe enabled"
  cat(
    "Sensitivity/change-token input detected (",
    input_change_label,
    "); ignoring copied input .par for model start. Use allow_sensitivity_par_start=1 only for a verified fitted par.\n",
    sep = ""
  )
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
  if (!is.na(par_in) && isTRUE(allow_par_start)) paste(program_path, frq_file, par_in, par_out, defaultswitch) else "./doitall.sh"
)

is_doitall_command <- function(x) identical(trimws(as.character(x)), "./doitall.sh")
if (is_doitall_command(mfcl_commands_raw)) {
  mfcl_commands_raw <- "./doitall.sh"
}

if (isTRUE(sensitivity_input) && !isTRUE(allow_sensitivity_par_start) && !is_doitall_command(mfcl_commands_raw) && !is.na(par_in)) {
  if (!is.na(ini_file) && file.exists(file.path(base_dir_abs, "doitall.sh"))) {
    mfcl_commands_raw <- "./doitall.sh"
    cat("Configured MFCL command uses an input .par, but par-start is disabled for this input; using ./doitall.sh instead.\n")
  } else {
    stop(
      "Configured MFCL command requires an input .par, but par-start is disabled for change-token input: ",
      if (length(input_change_tokens) > 0) paste(input_change_tokens, collapse = ", ") else "input recipe enabled",
      ". Set allow_sensitivity_par_start=1 only for a verified fitted .par."
    )
  }
}

if (!is.na(par_in) && is_doitall_command(mfcl_commands_raw) && isTRUE(allow_par_start)) {
  mfcl_commands_raw <- paste(program_path, frq_file, par_in, par_out, defaultswitch)
  cat("Input .par found; using .par-based MFCL command instead of ./doitall.sh\n")
}

if (is_doitall_command(mfcl_commands_raw) && is.na(ini_file)) {
  if (is.na(par_in)) {
    stop("doitall.sh requires a .ini file when no .par is available in ", base_dir_abs)
  }
  mfcl_commands_raw <- paste(program_path, frq_file, par_in, par_out, defaultswitch)
  cat("No .ini file found; using .par-based MFCL command instead of ./doitall.sh\n")
}

if (!is_doitall_command(mfcl_commands_raw) && is.na(par_in)) {
  stop("No .par files found in ", base_dir_abs, " for MFCL command: ", mfcl_commands_raw)
}

mfcl_commands <- if (is_doitall_command(mfcl_commands_raw)) {
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

## When using phase-based legacy scripts (doitall.sh), copied .par files
## can short-circuit phases and silently skip optimization. Remove all copied
## .par variants to force a fresh run from bet.ini in the model directory.
if (is_doitall_command(mfcl_commands_raw)) {
  copied_pars <- list.files(model_dir, pattern = "\\.par([0-9]+)?$", full.names = TRUE)
  if (length(copied_pars) > 0) {
    unlink(copied_pars, force = TRUE)
    cat("Removed", length(copied_pars), "existing copied .par files before doitall run\n")
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
  input_change_metadata = input_change_metadata,
  change_tokens    = input_change_tokens,
  change_token_source = if (length(input_change_tokens) > 0) "input_change_metadata.rds" else NA_character_,
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
  "indepvar.rpt",
  "avg_bio",
  "relative_depletion",
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
