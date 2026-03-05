## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/jitter.R")
source("tools/retro.R")
source("tools/model_payload.R")
source("tools/condor_archive_cleanup.R")


## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
program_path_abs <- file.path(project_root, program_path)
Sys.setenv("PROGRAM_PATH" = program_path_abs)

## Jitter settings
## Single seed value for parallel execution via condor
jitter_seed <- as.integer(Sys.getenv("jitter_seed", "40"))
jitter_cv_env <- Sys.getenv("jitter_cv", "")
jitter_amount_env <- Sys.getenv("jitter_amount", "")
jitter_coverage_env <- Sys.getenv("jitter_coverage", "")
jitter_cv <- suppressWarnings(as.numeric(
  if (nzchar(jitter_cv_env)) jitter_cv_env else if (nzchar(jitter_amount_env)) jitter_amount_env else if (nzchar(jitter_coverage_env)) jitter_coverage_env else "0.2"
))
jitter_hessian <- tolower(Sys.getenv("jitter_hessian", "0")) %in% c("1", "true", "yes", "y")
jitter_smoke_only <- tolower(Sys.getenv("jitter_smoke_only", "0")) %in% c("1", "true", "yes", "y")
jitter_smoke_hessian <- tolower(Sys.getenv("jitter_smoke_hessian", "0")) %in% c("1", "true", "yes", "y")
n_mixing_periods <- 2L

## Create jitter-specific directory inside jitter folder
jitter_dir <- file.path(model_dir, "jitter")
seed_dir <- file.path(jitter_dir, paste0("jitter_seed_", jitter_seed))
jitter_dir_abs <- file.path(project_root, jitter_dir)
seed_dir_abs <- file.path(project_root, seed_dir)

cat("Running Jitter Analysis\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Jitter directory:", jitter_dir_abs, "\n")
cat("Seed directory:", seed_dir_abs, "\n")
cat("Jitter seed:", jitter_seed, "\n")
cat("Jitter CV:", jitter_cv, "\n")
cat("Jitter Hessian:", jitter_hessian, "\n")
cat("Jitter smoke-only:", jitter_smoke_only, "\n")
cat("Jitter smoke hessian:", jitter_smoke_hessian, "\n")
cat("Mixing periods (fixed for jitter pre-makepar patch):", n_mixing_periods, "\n")

## Create seed directory and copy all files from base_dir (inputs)
dir.create(seed_dir_abs, recursive = TRUE, showWarnings = FALSE)
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = seed_dir_abs, overwrite = TRUE, recursive = TRUE)

par_files <- list.files(seed_dir_abs, pattern = "\\.par$", full.names = TRUE)
if (length(par_files) == 0) {
  stop("No reference .par files found in directory: ", seed_dir_abs)
}
file_info <- file.info(par_files)
reference_par_file <- rownames(file_info)[which.max(file_info$mtime)]
cat("Reference par for indepvar mapping:", basename(reference_par_file), "\n")
copied_reference_par_files <- par_files

############################
## Generate jittered 00.par ##
############################

frq_file <- list.files(seed_dir_abs, pattern = "\\.frq$", full.names = FALSE)
ini_files <- list.files(seed_dir_abs, pattern = "\\.ini$", full.names = FALSE)
tag_files <- list.files(seed_dir_abs, pattern = "\\.tag$", full.names = FALSE)

if (length(frq_file) == 0) {
  stop("No .frq file found in directory: ", seed_dir_abs)
}
if (length(ini_files) == 0) {
  stop("No .ini file found in directory: ", seed_dir_abs)
}
if (length(tag_files) == 0) {
  stop("No .tag file found in directory: ", seed_dir_abs)
}

frq_file <- frq_file[[1]]
ini_file <- ini_files[[1]]
tag_file <- tag_files[[1]]

tag_data <- read.MFCLTag(file.path(seed_dir_abs, tag_file))
ini_data <- read.MFCLIni(file.path(seed_dir_abs, ini_file))
max_year <- suppressWarnings(as.integer(read.MFCLFrq(file.path(seed_dir_abs, frq_file))@range["maxyear"]))

mix_fixed <- retro.ini(
  ini_data,
  tag.obj = tag_data,
  max_year = max_year,
  n_mixing_periods = n_mixing_periods
)
FLR4MFCL::write(mix_fixed$tag, file = file.path(seed_dir_abs, tag_file))
FLR4MFCL::write(mix_fixed$ini, file = file.path(seed_dir_abs, ini_file))
cat("Applied pre-makepar tag/ini mixing-period fix using n_mixing_periods =", n_mixing_periods, "\n")

base_00_par <- file.path(seed_dir_abs, "00.par")
makepar_cmd <- sprintf("%s %s %s %s -makepar", shQuote(program_path_abs), shQuote(frq_file), shQuote(ini_file), shQuote("00.par"))
cat("Creating initial 00.par from ini:", makepar_cmd, "\n")

old_wd <- setwd(seed_dir_abs)
makepar_status <- suppressWarnings(system(makepar_cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE))
setwd(old_wd)
if (!identical(makepar_status, 0L) || !file.exists(base_00_par)) {
  stop("Failed to create initial 00.par from ini using -makepar")
}

cat("Initial 00.par created from", ini_file, "\n")

## Apply jitter into the ini-generated 00.par for doitall phase1+
jittered_par_name <- "00.par"
indepvar_in_seed <- file.path(seed_dir_abs, "indepvar.rpt")

if (!file.exists(indepvar_in_seed)) {
  stop("Missing indepvar.rpt required for jitter run: ", indepvar_in_seed)
}

reference_par <- read.MFCLPar(reference_par_file)
base_par_label <- basename(reference_par_file)
cleanup_reference_par_files <- copied_reference_par_files[normalizePath(copied_reference_par_files, winslash = "/", mustWork = FALSE) != normalizePath(base_00_par, winslash = "/", mustWork = FALSE)]
if (length(cleanup_reference_par_files) > 0) {
  unlink(cleanup_reference_par_files, force = TRUE)
}
cat("Removed copied reference par files after indepvar mapping load (kept 00.par)\n")
base_00_par_obj <- read.MFCLPar(base_00_par)
indepvar_map <- build_indepvar_mapping(reference_par, indepvar_file = indepvar_in_seed, tol = 1e-14)
if (is.null(indepvar_map) || !all(indepvar_map$mapping$mapped)) {
  stop("Exact indepvar mapping could not be resolved for all parameters in reference par.")
}
if (any(!is.finite(indepvar_map$mapping$L_bound)) || any(!is.finite(indepvar_map$mapping$U_bound))) {
  stop("CV jitter requires finite L_bound and U_bound for all mapped free parameters.")
}

if (!is.null(jitter_seed)) set.seed(jitter_seed)
jittered_00_par_obj <- apply_indepvar_cv_jitter(base_00_par_obj, indepvar_map, jitter_cv = jitter_cv)
if (is.null(jittered_00_par_obj)) {
  stop("CV jitter application failed on initial 00.par.")
}
FLR4MFCL::write(jittered_00_par_obj, file = file.path(seed_dir_abs, jittered_par_name))

jitter_run <- list(
  comparison = compare_indepvar_mapped(
    base_par = base_00_par_obj,
    jittered_par = jittered_00_par_obj,
    indepvar_map = indepvar_map,
    change_tol = 1e-14
  )
)

cat("Jittered 00.par written for doitall workflow\n")

if (isTRUE(jitter_smoke_only)) {
  hessian_summary_smoke <- list(
    requested = isTRUE(jitter_hessian),
    attempted = FALSE,
    run_ok = NA,
    error = NA_character_,
    info_file = NA_character_,
    is_pdh = NA,
    is_spd = NA,
    hessian_ok = NA,
    n_negative_eigenvalues = NA_integer_,
    n_total_eigenvalues = NA_integer_,
    hessian_status = NA_character_,
    reliability = NA_character_,
    smoke_hessian = isTRUE(jitter_smoke_hessian)
  )

  if (isTRUE(jitter_hessian) && isTRUE(jitter_smoke_hessian)) {
    hessian_summary_smoke$attempted <- TRUE
    hessian_dir <- file.path(seed_dir_abs, "hessian")
    part_dir <- file.path(hessian_dir, "part_1")
    dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

    seed_files <- list.files(seed_dir_abs, full.names = TRUE, recursive = FALSE)
    seed_files <- seed_files[file.info(seed_files)$isdir %in% FALSE]
    if (length(seed_files) > 0) {
      file.copy(seed_files, to = part_dir, overwrite = TRUE, recursive = FALSE)
    }

    hessian_input_par <- basename(base_00_par)
    npars <- NA_integer_
    par_in_part <- file.path(part_dir, hessian_input_par)
    if (file.exists(par_in_part)) {
      par_lines <- readLines(par_in_part, warn = FALSE)
      npars_line <- grep("# The number of parameters", par_lines)
      if (length(npars_line) > 0) {
        npars <- suppressWarnings(as.integer(scan(par_in_part, skip = npars_line, nlines = 1, quiet = TRUE)))
      }
    }
    if ((!is.finite(npars) || is.na(npars) || npars <= 0) && !is.null(indepvar_map$mapping)) {
      npars <- nrow(indepvar_map$mapping)
    }

    if (!is.finite(npars) || is.na(npars) || npars <= 0) {
      hessian_summary_smoke$run_ok <- FALSE
      hessian_summary_smoke$error <- "Failed to parse number of parameters from 00.par for smoke hessian."
    } else {
      hessian_switch <- paste("-switch 3", "1 145 1", "1 223 1", "1 224", npars, sep = " ")
      hessian_out_par <- "hessian_1.par"
      hessian_cmd <- paste(
        shQuote(program_path_abs),
        shQuote(frq_file),
        shQuote(hessian_input_par),
        shQuote(hessian_out_par),
        hessian_switch
      )
      hessian_run_ok <- TRUE
      hessian_error <- NULL
      tryCatch(
        {
          run_commands(
            commands = hessian_cmd,
            work_dirs = part_dir,
            save_log = TRUE,
            parallel = FALSE,
            verbose = TRUE,
            log_file = file.path(part_dir, "mfcl_hessian_log.txt")
          )
        },
        error = function(e) {
          hessian_run_ok <<- FALSE
          hessian_error <<- conditionMessage(e)
        }
      )

      if (!isTRUE(hessian_run_ok)) {
        hessian_summary_smoke$run_ok <- FALSE
        hessian_summary_smoke$error <- if (!is.null(hessian_error) && nzchar(hessian_error)) hessian_error else "Smoke hessian MFCL run failed."
      } else {
        part_info <- list(
          hessian_part = 1L,
          nsplit = 1L,
          start_par = 1L,
          end_par = as.integer(npars),
          npars = as.integer(npars),
          frq_file = frq_file,
          program_path = program_path,
          model_dir = seed_dir_abs,
          part_dir = part_dir,
          base_dir = seed_dir_abs,
          input_par = hessian_input_par,
          output_par = hessian_out_par
        )
        saveRDS(part_info, file = file.path(part_dir, "hessian_info.rds"), compress = "xz")
        collate_script <- file.path(project_root, "tools", "collate_hessian_mfcl.R")
        collate_cmd <- paste("Rscript", shQuote(collate_script), shQuote(seed_dir_abs))
        collate_status <- suppressWarnings(system(collate_cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE))
        hessian_info_file <- file.path(hessian_dir, "hessian_info.rds")
        hessian_summary_smoke$info_file <- hessian_info_file

        if (!identical(collate_status, 0L) || !file.exists(hessian_info_file)) {
          hessian_summary_smoke$run_ok <- FALSE
          hessian_summary_smoke$error <- "Smoke hessian collate failed or hessian_info.rds not produced."
        } else {
          hinfo <- suppressWarnings(tryCatch(readRDS(hessian_info_file), error = function(e) NULL))
          pdh_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$pdh$is_pdh), error = function(e) NA))
          spd_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$positivised_cov_is_spd), error = function(e) NA))
          n_neg <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_negative_eigenvalues), error = function(e) NA_integer_))
          n_tot <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_total_eigenvalues), error = function(e) NA_integer_))
          h_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$hessian_status), error = function(e) NA_character_))
          rel_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$reliability), error = function(e) NA_character_))

          pdh_scalar <- if (length(pdh_val) > 0) pdh_val[[1]] else NA
          spd_scalar <- if (length(spd_val) > 0) spd_val[[1]] else NA
          hessian_summary_smoke$run_ok <- TRUE
          hessian_summary_smoke$is_pdh <- pdh_scalar
          hessian_summary_smoke$is_spd <- spd_scalar
          hessian_summary_smoke$hessian_ok <- if (!is.na(pdh_scalar)) {
            if (!is.na(spd_scalar)) isTRUE(pdh_scalar) && isTRUE(spd_scalar) else isTRUE(pdh_scalar)
          } else {
            NA
          }
          hessian_summary_smoke$n_negative_eigenvalues <- if (length(n_neg) > 0) n_neg[[1]] else NA_integer_
          hessian_summary_smoke$n_total_eigenvalues <- if (length(n_tot) > 0) n_tot[[1]] else NA_integer_
          hessian_summary_smoke$hessian_status <- if (length(h_status) > 0) h_status[[1]] else NA_character_
          hessian_summary_smoke$reliability <- if (length(rel_status) > 0) rel_status[[1]] else NA_character_
        }
      }
    }
  } else if (isTRUE(jitter_hessian) && !isTRUE(jitter_smoke_hessian)) {
    hessian_summary_smoke$run_ok <- FALSE
    hessian_summary_smoke$error <- "Skipped in smoke-only mode. Set jitter_smoke_hessian=1 to run hessian on jittered 00.par."
  }

  info_list <- list(
    jitter_seed = jitter_seed,
    jitter_cv = jitter_cv,
    jitter_coverage = jitter_cv,
    jitter_amount = jitter_cv,
    smoke_only = TRUE,
    frq_file = frq_file,
    program_path = program_path,
    model_dir = model_dir,
    seed_dir = seed_dir,
    seed_dir_abs = seed_dir_abs,
    input_par = base_par_label,
    input_00_par = basename(base_00_par),
    input_ini = ini_file,
    jittered_par = jittered_par_name,
    parameter_changes = jitter_run$comparison,
    parameter_change_summary = jitter_run$comparison$summary,
    parameter_change_overall = data.frame(
      seed = jitter_seed,
      n = nrow(jitter_run$comparison$labels),
      changed = sum(jitter_run$comparison$labels$changed, na.rm = TRUE),
      changed_pct = 100 * mean(jitter_run$comparison$labels$changed, na.rm = TRUE),
      mean_abs_delta = mean(abs(jitter_run$comparison$labels$delta), na.rm = TRUE),
      median_abs_delta = stats::median(abs(jitter_run$comparison$labels$delta), na.rm = TRUE),
      mean_abs_pct_change = mean(abs(100 * (jitter_run$comparison$labels$delta / ifelse(abs(jitter_run$comparison$labels$before) > .Machine$double.eps, abs(jitter_run$comparison$labels$before), NA_real_))), na.rm = TRUE),
      median_abs_pct_change = stats::median(abs(100 * (jitter_run$comparison$labels$delta / ifelse(abs(jitter_run$comparison$labels$before) > .Machine$double.eps, abs(jitter_run$comparison$labels$before), NA_real_))), na.rm = TRUE)
    ),
    mfcl_run = list(
      command = NA_character_,
      run_ok = NA,
      error = NA_character_,
      output_par = NA_character_,
      output_par_exists = FALSE,
      log_file = file.path(seed_dir_abs, "mfcl_log.txt"),
      smoke_only = TRUE
    ),
    hessian = hessian_summary_smoke
  )

  saveRDS(info_list, file = file.path(seed_dir_abs, "jitter_info.rds"), compress = "xz")
  jitter_payload <- mp_build_jitter_payload(seed_dir_abs, jitter_seed)
  saveRDS(jitter_payload, file = file.path(seed_dir_abs, "jitter_result.rds"), compress = "xz")
  deleted_n <- mp_cleanup_files(seed_dir_abs, keep = c("jitter_result.rds", "jitter_info.rds"), recursive = TRUE)
  cat("Smoke-only jitter complete; cleanup removed", deleted_n, "non-core files in", seed_dir, "\n")
  cb_condor_keep_only_model_cleanup()
  cat("✅ Jitter smoke test completed for seed", jitter_seed, "\n")
  quit(status = 0)
}

##############
## run MFCL ##
##############

doitall_path <- file.path(seed_dir_abs, "doitall.sh")
if (!file.exists(doitall_path)) {
  base_doitall_path <- file.path(base_dir_abs, "doitall.sh")
  if (file.exists(base_doitall_path)) {
    file.copy(base_doitall_path, doitall_path, overwrite = TRUE)
    cat("Restored missing doitall.sh from base inputs\n")
  }
}
if (!file.exists(doitall_path)) {
  stop("Jitter workflow requires doitall.sh: ", doitall_path)
}

doitall_jitter_path <- file.path(seed_dir_abs, "doitall_jitter.sh")
doitall_lines <- readLines(doitall_path, warn = FALSE)
makepar_idx <- grep("-makepar", doitall_lines, fixed = TRUE)
if (length(makepar_idx) == 0) {
  stop("Could not locate phase0 -makepar command in doitall.sh")
}
doitall_lines[makepar_idx[1]] <- "echo 'Skipping phase 0 makepar; using jittered 00.par'"
legacy_output_par <- paste0("jittered_out_", jitter_seed, ".par")
legacy_output_rep <- paste0("plot-", legacy_output_par, ".rep")
doitall_lines <- c(
  doitall_lines,
  "",
  "# Preserve legacy jitter output names for downstream tooling",
  "final_par=$(ls -1 *.par 2>/dev/null | grep -E '^[0-9]+\\.par$' | sort -V | tail -n 1)",
  "if [ -n \"$final_par\" ] && [ -f \"$final_par\" ]; then",
  sprintf("  cp -f \"$final_par\" %s", shQuote(legacy_output_par)),
  "  final_rep=\"plot-${final_par}.rep\"",
  "  if [ -f \"$final_rep\" ]; then",
  sprintf("    cp -f \"$final_rep\" %s", shQuote(legacy_output_rep)),
  "  fi",
  "fi"
)
writeLines(doitall_lines, doitall_jitter_path)

mfcl_commands <- "sh ./doitall_jitter.sh"

cat("Running jittered doitall workflow:", mfcl_commands, "\n")

mfcl_error <- NULL
mfcl_run_ok <- TRUE
tryCatch(
  {
    run_commands(commands = mfcl_commands,
                 work_dirs = seed_dir_abs, 
                 save_log = TRUE, 
                 parallel = FALSE, 
                 verbose = TRUE, 
                 log_file = file.path(seed_dir_abs, "mfcl_log.txt"))
  },
  error = function(e) {
    mfcl_run_ok <<- FALSE
    mfcl_error <<- conditionMessage(e)
  }
)

fitted_parameter_changes <- NULL
fitted_parameter_change_summary <- NULL
fitted_parameter_change_overall <- NULL
legacy_final_par_path <- file.path(seed_dir_abs, legacy_output_par)
final_par_path <- if (file.exists(legacy_final_par_path)) legacy_final_par_path else mp_final_par(seed_dir_abs)
output_par_name <- if (!is.null(final_par_path)) basename(final_par_path) else NA_character_
base_par_obj <- reference_par
output_par_obj <- if (!is.null(final_par_path) && file.exists(final_par_path)) {
  suppressWarnings(tryCatch(read.MFCLPar(final_par_path), error = function(e) NULL))
} else {
  NULL
}

hessian_summary <- list(
  requested = isTRUE(jitter_hessian),
  attempted = FALSE,
  run_ok = NA,
  error = NA_character_,
  info_file = NA_character_,
  is_pdh = NA,
  is_spd = NA,
  hessian_ok = NA,
  n_negative_eigenvalues = NA_integer_,
  n_total_eigenvalues = NA_integer_,
  hessian_status = NA_character_,
  reliability = NA_character_
)

if (isTRUE(jitter_hessian)) {
  if (is.null(final_par_path) || !file.exists(final_par_path) || !isTRUE(mfcl_run_ok)) {
    hessian_summary$attempted <- FALSE
    hessian_summary$run_ok <- FALSE
    hessian_summary$error <- "Skipped: jitter fit did not complete successfully or final par missing."
  } else {
    hessian_summary$attempted <- TRUE
    hessian_dir <- file.path(seed_dir_abs, "hessian")
    part_dir <- file.path(hessian_dir, "part_1")
    dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

    seed_files <- list.files(seed_dir_abs, full.names = TRUE, recursive = FALSE)
    seed_files <- seed_files[file.info(seed_files)$isdir %in% FALSE]
    if (length(seed_files) > 0) {
      file.copy(seed_files, to = part_dir, overwrite = TRUE, recursive = FALSE)
    }

    final_par_for_hessian <- basename(final_par_path)
    final_par_in_part <- file.path(part_dir, final_par_for_hessian)
    npars <- NA_integer_
    if (file.exists(final_par_in_part)) {
      par_lines <- readLines(final_par_in_part, warn = FALSE)
      npars_line <- grep("# The number of parameters", par_lines)
      if (length(npars_line) > 0) {
        npars <- suppressWarnings(as.integer(scan(final_par_in_part, skip = npars_line, nlines = 1, quiet = TRUE)))
      }
    }

    if (!is.finite(npars) || is.na(npars) || npars <= 0) {
      hessian_summary$run_ok <- FALSE
      hessian_summary$error <- "Failed to parse number of parameters from final par for jitter hessian."
    } else {
      hessian_switch <- paste(
        "-switch 3",
        "1 145 1",
        "1 223 1",
        "1 224", npars,
        sep = " "
      )
      hessian_out_par <- "hessian_1.par"
      hessian_cmd <- paste(
        shQuote(program_path_abs),
        shQuote(frq_file),
        shQuote(final_par_for_hessian),
        shQuote(hessian_out_par),
        hessian_switch
      )

      hessian_run_ok <- TRUE
      hessian_error <- NULL
      tryCatch(
        {
          run_commands(
            commands = hessian_cmd,
            work_dirs = part_dir,
            save_log = TRUE,
            parallel = FALSE,
            verbose = TRUE,
            log_file = file.path(part_dir, "mfcl_hessian_log.txt")
          )
        },
        error = function(e) {
          hessian_run_ok <<- FALSE
          hessian_error <<- conditionMessage(e)
        }
      )

      if (!isTRUE(hessian_run_ok)) {
        hessian_summary$run_ok <- FALSE
        hessian_summary$error <- if (!is.null(hessian_error) && nzchar(hessian_error)) hessian_error else "Hessian MFCL run failed."
      } else {
        part_info <- list(
          hessian_part = 1L,
          nsplit = 1L,
          start_par = 1L,
          end_par = as.integer(npars),
          npars = as.integer(npars),
          frq_file = frq_file,
          program_path = program_path,
          model_dir = seed_dir_abs,
          part_dir = part_dir,
          base_dir = seed_dir_abs,
          input_par = final_par_for_hessian,
          output_par = hessian_out_par
        )
        saveRDS(part_info, file = file.path(part_dir, "hessian_info.rds"), compress = "xz")

        collate_script <- file.path(project_root, "tools", "collate_hessian_mfcl.R")
        collate_cmd <- paste("Rscript", shQuote(collate_script), shQuote(seed_dir_abs))
        collate_status <- suppressWarnings(system(collate_cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE))

        hessian_info_file <- file.path(hessian_dir, "hessian_info.rds")
        hessian_summary$info_file <- hessian_info_file
        if (!identical(collate_status, 0L) || !file.exists(hessian_info_file)) {
          hessian_summary$run_ok <- FALSE
          hessian_summary$error <- "Hessian collate failed or hessian_info.rds not produced."
        } else {
          hinfo <- suppressWarnings(tryCatch(readRDS(hessian_info_file), error = function(e) NULL))
          pdh_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$pdh$is_pdh), error = function(e) NA))
          spd_val <- suppressWarnings(tryCatch(as.logical(hinfo$diagnostics$summary$positivised_cov_is_spd), error = function(e) NA))
          n_neg <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_negative_eigenvalues), error = function(e) NA_integer_))
          n_tot <- suppressWarnings(tryCatch(as.integer(hinfo$eigen$n_total_eigenvalues), error = function(e) NA_integer_))
          h_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$hessian_status), error = function(e) NA_character_))
          rel_status <- suppressWarnings(tryCatch(as.character(hinfo$eigen$reliability), error = function(e) NA_character_))

          pdh_scalar <- if (length(pdh_val) > 0) pdh_val[[1]] else NA
          spd_scalar <- if (length(spd_val) > 0) spd_val[[1]] else NA

          hessian_summary$run_ok <- TRUE
          hessian_summary$is_pdh <- pdh_scalar
          hessian_summary$is_spd <- spd_scalar
          hessian_summary$hessian_ok <- if (!is.na(pdh_scalar)) {
            if (!is.na(spd_scalar)) isTRUE(pdh_scalar) && isTRUE(spd_scalar) else isTRUE(pdh_scalar)
          } else {
            NA
          }
          hessian_summary$n_negative_eigenvalues <- if (length(n_neg) > 0) n_neg[[1]] else NA_integer_
          hessian_summary$n_total_eigenvalues <- if (length(n_tot) > 0) n_tot[[1]] else NA_integer_
          hessian_summary$hessian_status <- if (length(h_status) > 0) h_status[[1]] else NA_character_
          hessian_summary$reliability <- if (length(rel_status) > 0) rel_status[[1]] else NA_character_
        }
      }
    }
  }
}

if (!is.null(base_par_obj) && !is.null(output_par_obj)) {
  fitted_parameter_changes <- compare_exact_jitter(
    base_par = base_par_obj,
    jittered_par = output_par_obj,
    indepvar_file = indepvar_in_seed,
    change_tol = 1e-14,
    output_prefix = FALSE
  )
  fitted_parameter_change_summary <- fitted_parameter_changes$summary
  fitted_parameter_change_overall <- data.frame(
    seed = jitter_seed,
    n = nrow(fitted_parameter_changes$labels),
    changed = sum(fitted_parameter_changes$labels$changed, na.rm = TRUE),
    changed_pct = 100 * mean(fitted_parameter_changes$labels$changed, na.rm = TRUE),
    mean_abs_delta = mean(abs(fitted_parameter_changes$labels$delta), na.rm = TRUE),
    median_abs_delta = stats::median(abs(fitted_parameter_changes$labels$delta), na.rm = TRUE),
    mean_abs_pct_change = mean(abs(100 * (fitted_parameter_changes$labels$delta / ifelse(abs(fitted_parameter_changes$labels$before) > .Machine$double.eps, abs(fitted_parameter_changes$labels$before), NA_real_))), na.rm = TRUE),
    median_abs_pct_change = stats::median(abs(100 * (fitted_parameter_changes$labels$delta / ifelse(abs(fitted_parameter_changes$labels$before) > .Machine$double.eps, abs(fitted_parameter_changes$labels$before), NA_real_))), na.rm = TRUE)
  )
}

# Save jitter run info
info_list <- list(
  jitter_seed   = jitter_seed,
  jitter_cv = jitter_cv,
  jitter_coverage = jitter_cv,
  jitter_amount = jitter_cv,
  frq_file      = frq_file,
  program_path  = program_path,
  model_dir     = model_dir,
  seed_dir      = seed_dir,
  seed_dir_abs  = seed_dir_abs,
  input_par     = base_par_label,
  input_00_par  = basename(base_00_par),
  input_ini     = ini_file,
  jittered_par  = jittered_par_name,
  parameter_changes = jitter_run$comparison,
  parameter_change_summary = jitter_run$comparison$summary,
  parameter_change_overall = data.frame(
    seed = jitter_seed,
    n = nrow(jitter_run$comparison$labels),
    changed = sum(jitter_run$comparison$labels$changed, na.rm = TRUE),
    changed_pct = 100 * mean(jitter_run$comparison$labels$changed, na.rm = TRUE),
    mean_abs_delta = mean(abs(jitter_run$comparison$labels$delta), na.rm = TRUE),
    median_abs_delta = stats::median(abs(jitter_run$comparison$labels$delta), na.rm = TRUE),
    mean_abs_pct_change = mean(abs(100 * (jitter_run$comparison$labels$delta / ifelse(abs(jitter_run$comparison$labels$before) > .Machine$double.eps, abs(jitter_run$comparison$labels$before), NA_real_))), na.rm = TRUE),
    median_abs_pct_change = stats::median(abs(100 * (jitter_run$comparison$labels$delta / ifelse(abs(jitter_run$comparison$labels$before) > .Machine$double.eps, abs(jitter_run$comparison$labels$before), NA_real_))), na.rm = TRUE)
  ),
  mfcl_run = list(
    command = mfcl_commands,
    run_ok = mfcl_run_ok,
    error = mfcl_error,
    output_par = output_par_name,
    output_par_exists = !is.null(final_par_path) && file.exists(final_par_path),
    log_file = file.path(seed_dir_abs, "mfcl_log.txt")
  ),
  fitted_parameter_changes = fitted_parameter_changes,
  fitted_parameter_change_summary = fitted_parameter_change_summary,
  fitted_parameter_change_overall = fitted_parameter_change_overall,
  hessian = hessian_summary
)

saveRDS(
  info_list,
  file = file.path(seed_dir_abs, "jitter_info.rds"),
  compress = "xz"
)

jitter_payload <- mp_build_jitter_payload(seed_dir_abs, jitter_seed)
saveRDS(
  jitter_payload,
  file = file.path(seed_dir_abs, "jitter_result.rds"),
  compress = "xz"
)

deleted_n <- mp_cleanup_files(
  seed_dir_abs,
  keep = c(
    "jitter_result.rds",
    "jitter_info.rds"
  ),
  recursive = TRUE
)
cat("Cleanup removed", deleted_n, "non-core files in", seed_dir, "\n")

cb_condor_keep_only_model_cleanup()

if (!isTRUE(mfcl_run_ok)) {
  stop(if (!is.null(mfcl_error) && nzchar(mfcl_error)) mfcl_error else paste0("Jitter MFCL run failed for seed ", jitter_seed))
}

cat("✅ Jitter run completed for seed", jitter_seed, "\n")
