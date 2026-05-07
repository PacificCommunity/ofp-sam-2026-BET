## ============================================================
## MFCL self-test / simulation-estimation runner
##
## Steps per replicate:
##   1. copy fitted input set and run MFCL pseudo-observation mode
##   2. rebuild a replicate input .frq from MFCL *_sim output
##   3. optionally refit from the operating-model .par start
##   4. write parameter and derived-quantity recovery tables
## ============================================================

library(FLR4MFCL)

source("tools/model_payload.R")
source("tools/selftest.R")
source("tools/input_recipe_runner.R")
source("tools/fitted_model_source.R")
source("tools/condor_archive_cleanup.R")

project_root <- getwd()
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) y else x

st_env <- function(name, default = "") {
  lower <- Sys.getenv(name, "")
  if (nzchar(lower)) return(lower)
  upper <- Sys.getenv(toupper(name), "")
  if (nzchar(upper)) return(upper)
  default
}

program_path <- st_env("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
program_path_abs <- st_resolve_path(program_path, project_root = project_root, must_work = TRUE)
Sys.setenv(PROGRAM_PATH = program_path_abs)

base_dir <- st_env("selftest_base_dir", st_env("base_dir", "mfcl/inputs/2023_4region"))
base_dir_abs <- ensure_input_dir_available(base_dir, project_root = project_root)
base_dir_abs <- st_resolve_path(base_dir_abs, project_root = project_root, must_work = TRUE)

model_dir_env <- st_env("selftest_model_dir", st_env("model_dir", ""))
model_dir_default <- file.path("model", basename(base_dir_abs))
model_dir <- if (nzchar(model_dir_env)) model_dir_env else model_dir_default
model_dir_abs <- st_resolve_path(model_dir, project_root = project_root, must_work = FALSE)
model_dir_available <- dir.exists(model_dir_abs)
auto_fitted_model_dir <- st_env("auto_fitted_model_dir", model_dir)
auto_fitted_model_dir_abs <- st_resolve_path(auto_fitted_model_dir, project_root = project_root, must_work = FALSE)
auto_fitted_model_available <- dir.exists(auto_fitted_model_dir_abs)

selftest_dir <- st_env("selftest_dir", file.path(model_dir, "selftest"))
selftest_dir_abs <- st_resolve_path(selftest_dir, project_root = project_root, must_work = FALSE)

fitted_source_active <- fms_truthy(st_env("fitted_model_source_enabled", "0")) ||
  nzchar(st_env("fitted_model_bundle", "")) ||
  nzchar(st_env("fitted_model_source_dir", "")) ||
  fms_truthy(st_env("auto_run_model_before_dependency", "0"))
if (isTRUE(fitted_source_active)) {
  base_dir_abs <- ensure_fitted_model_source(
    base_dir_abs = base_dir_abs,
    base_dir = base_dir,
    model_dir = model_dir,
    project_root = project_root
  )
}

source_mode <- tolower(trimws(st_env("selftest_source_mode", "last_par")))
if (!source_mode %in% c("last_par", "doitall")) {
  stop("Unsupported selftest_source_mode: ", source_mode, ". Use last_par or doitall.")
}

source_par_env <- st_env("selftest_source_par", "")
source_par <- NA_character_
if (identical(source_mode, "last_par")) {
  source_par <- if (nzchar(source_par_env)) {
    st_resolve_path(source_par_env, project_root = project_root, must_work = TRUE)
  } else {
    source_candidates <- c(
      if (isTRUE(model_dir_available)) st_latest_par(model_dir_abs) else NA_character_,
      if (isTRUE(auto_fitted_model_available)) st_latest_par(auto_fitted_model_dir_abs) else NA_character_,
      st_latest_par(base_dir_abs)
    )
    source_candidates <- source_candidates[!is.na(source_candidates) & file.exists(source_candidates)]
    if (length(source_candidates) > 0) source_candidates[[1]] else NA_character_
  }
  if (is.na(source_par) || !file.exists(source_par)) {
    stop(
      "No self-test source .par found. Set selftest_source_par, provide selftest_model_dir with a fitted .par, ",
      "or include a fitted .par in selftest_base_dir."
    )
  }
} else {
  if (!file.exists(file.path(base_dir_abs, "doitall.sh"))) {
    stop("selftest_source_mode=doitall requires doitall.sh in selftest_base_dir: ", base_dir_abs)
  }
}

frq_file <- st_first_file(base_dir_abs, "\\.frq$")
tag_sim_file <- st_env("selftest_tag_sim_file", "")

reps <- st_parse_numeric_tokens(st_env("selftest_reps", "1"))
reps <- reps[is.finite(reps)]
if (length(reps) == 0) reps <- 1
reps <- as.integer(reps)

seed_base <- suppressWarnings(as.integer(st_env("selftest_seed_base", "1000")))
if (!is.finite(seed_base)) seed_base <- 1000L

run_refit <- st_truthy(st_env("selftest_run_refit", "0"))
refit_mode <- tolower(trimws(st_env("selftest_refit_mode", "last_par")))
if (!refit_mode %in% c("last_par", "doitall")) {
  stop("Unsupported selftest_refit_mode: ", refit_mode, ". Use last_par or doitall.")
}
refit_fevals <- suppressWarnings(as.integer(st_env("selftest_refit_fevals", "500")))
if (!is.finite(refit_fevals) || refit_fevals < 1L) refit_fevals <- 500L
selftest_compact_cleanup <- st_truthy(
  st_env("selftest_compact_cleanup", if (isTRUE(run_refit)) "1" else "0"),
  default = isTRUE(run_refit)
)

update_catch_setting <- tolower(trimws(st_env("selftest_update_catch", "auto")))
update_effort_setting <- tolower(trimws(st_env("selftest_update_effort", "auto")))
catch_conditioned <- if (!is.na(source_par) && file.exists(source_par)) {
  st_catch_conditioned(source_par)
} else {
  FALSE
}
effort_conditioned <- if (!is.na(source_par) && file.exists(source_par)) {
  st_effort_conditioned(source_par)
} else {
  FALSE
}
update_catch <- if (identical(update_catch_setting, "auto")) {
  !isTRUE(catch_conditioned)
} else {
  st_truthy(update_catch_setting, default = !isTRUE(catch_conditioned))
}
update_effort <- if (identical(update_effort_setting, "auto")) {
  isTRUE(effort_conditioned)
} else {
  st_truthy(update_effort_setting, default = isTRUE(effort_conditioned))
}
update_lw <- st_truthy(st_env("selftest_update_lw", "1"), default = TRUE)
update_cpue <- st_truthy(st_env("selftest_update_cpue", "1"), default = TRUE)
update_tags <- st_truthy(st_env("selftest_update_tags", "1"), default = TRUE)
update_age_length <- st_truthy(st_env("selftest_update_age_length", "1"), default = TRUE)
require_native_tags <- st_truthy(st_env("selftest_require_native_tags", "1"), default = TRUE)
native_tag_projection_years <- suppressWarnings(as.integer(st_env("selftest_native_tag_projection_years", NA_character_)))
native_tag_nsims <- suppressWarnings(as.integer(st_env("selftest_native_tag_nsims", "1")))
if (!is.finite(native_tag_nsims) || native_tag_nsims < 1L) native_tag_nsims <- 1L
projection_average_years <- st_parse_numeric_tokens(st_env("selftest_projection_average_years", ""))
projection_average_years <- projection_average_years[is.finite(projection_average_years)]

sim_switch <- st_env("selftest_sim_switch", st_default_switch())
key_parameters <- strsplit(st_env(
  "selftest_key_parameters",
  "totpop,sv(21),vb_coff(1),vb_coff(2),vb_coff(3),var_coff(1),var_coff(2),age_pars(5)"
), "[,[:space:]]+")[[1]]
key_parameters <- key_parameters[nzchar(key_parameters)]

dir.create(selftest_dir_abs, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(selftest_dir_abs, "sim"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(selftest_dir_abs, "inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(selftest_dir_abs, "truth_eval"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(selftest_dir_abs, "refit"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(selftest_dir_abs, "recovery"), recursive = TRUE, showWarnings = FALSE)

cat("Running MFCL self-test\n")
cat("Base inputs :", base_dir_abs, "\n")
cat("Model source:", if (isTRUE(model_dir_available)) model_dir_abs else "<not found>", "\n")
cat("Auto source :", if (isTRUE(auto_fitted_model_available)) auto_fitted_model_dir_abs else "<not found>", "\n")
cat("Source mode :", source_mode, "\n")
cat("Source par  :", if (!is.na(source_par)) source_par else "<doitall.sh>", "\n")
cat("Output dir  :", selftest_dir_abs, "\n")
cat("Replicates  :", paste(reps, collapse = " "), "\n")
cat("Run refit   :", run_refit, "\n")
cat("Refit mode  :", refit_mode, "\n")
cat("Refit fevals:", refit_fevals, "\n")
cat("Native tags :", require_native_tags && update_tags, "\n")
cat("Compact cleanup:", selftest_compact_cleanup, "\n")
cat("Catch conditioned:", catch_conditioned, "\n")
cat("Effort conditioned:", effort_conditioned, "\n")
cat("Update data :", paste(
  c(
    if (update_catch) "catch",
    if (update_effort) "effort",
    if (update_lw) "length/weight",
    if (update_cpue) "cpue",
    if (update_tags) "tags",
    if (update_age_length) "age-length"
  ),
  collapse = ", "
), "\n")

run_rows <- list()

prepare_selftest_source_par <- function(sim_dir, source_par, rep_label) {
  frq_name <- basename(frq_file)
  if (identical(source_mode, "doitall")) {
    log_file <- file.path(sim_dir, "mfcl_selftest_source_doitall_log.txt")
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(sim_dir)
    Sys.chmod("doitall.sh", mode = "0755")
    cat("MFCL source command:\n ./doitall.sh\n")
    status <- system(paste("./doitall.sh", ">", shQuote(log_file), "2>&1"), intern = FALSE)
    if (!identical(as.integer(status), 0L)) {
      stop("Source doitall.sh failed for ", rep_label, " with status ", status, "; see ", log_file)
    }
    par_out <- st_latest_par(sim_dir)
    if (is.na(par_out) || !file.exists(par_out)) {
      stop("Source doitall.sh did not create a .par for ", rep_label, "; see ", log_file)
    }
    return(par_out)
  }

  sim_par <- file.path(sim_dir, basename(source_par))
  file.copy(source_par, sim_par, overwrite = TRUE)
  sim_par
}

st_prune_empty_dirs <- function(root) {
  if (!dir.exists(root)) return(invisible(0L))
  dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  dirs <- rev(dirs[nzchar(dirs)])
  removed <- 0L
  for (d in dirs) {
    entries <- list.files(d, all.files = TRUE, no.. = TRUE)
    if (length(entries) == 0) {
      unlink(d, recursive = TRUE, force = TRUE)
      removed <- removed + 1L
    }
  }
  invisible(removed)
}

st_save_truth_payload <- function(sim_dir, sim_info, native_tag_info, seeds, rep_id) {
  truth_par <- st_latest_par(sim_dir)
  info <- list(
    description = "MFCL self-test operating-model truth simulation",
    model_dir = sim_dir,
    replicate = rep_id,
    sim_info = sim_info,
    native_tag_info = native_tag_info,
    seeds = seeds,
    selftest = TRUE,
    truth = TRUE,
    obj_fun = if (!is.na(truth_par) && file.exists(truth_par)) mp_extract_par_obj_fun(truth_par) else NA_real_,
    max_grad = if (!is.na(truth_par) && file.exists(truth_par)) mp_extract_par_max_grad(truth_par) else NA_real_
  )
  saveRDS(info, file.path(sim_dir, "model_info.rds"), compress = "xz")
  payload <- tryCatch(
    mp_build_model_payload(sim_dir, tag_report_year1 = "auto"),
    error = function(e) {
      warning("Could not build self-test truth payload for ", basename(sim_dir), ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(payload)) {
    saveRDS(payload, file.path(sim_dir, "model_payload.rds"), compress = "xz")
  }
  invisible(!is.null(payload))
}

st_cleanup_selftest_rep <- function(sim_dir, input_dir, truth_eval_dir, refit_dir, recovery_dir, run_refit) {
  if (!isTRUE(selftest_compact_cleanup)) return(invisible(FALSE))

  sim_keep <- c(
    "model_payload.rds",
    "model_info.rds",
    "selftest_projection_info.rds",
    "selftest_native_tag_info.rds",
    "selftest_sim_info.rds"
  )
  input_keep <- c("selftest_input_info.rds", "data_simulation_summary.rds")
  truth_eval_keep <- c("model_payload.rds", "model_info.rds")
  refit_keep <- c("model_payload.rds", "model_info.rds")
  recovery_keep <- c("parameter_recovery.csv", "derived_recovery.csv")

  deleted <- c(
    sim = mp_cleanup_files(sim_dir, keep = sim_keep, recursive = TRUE),
    inputs = mp_cleanup_files(input_dir, keep = input_keep, recursive = TRUE),
    truth_eval = if (dir.exists(truth_eval_dir)) mp_cleanup_files(truth_eval_dir, keep = truth_eval_keep, recursive = TRUE) else 0L,
    refit = if (isTRUE(run_refit) && dir.exists(refit_dir)) mp_cleanup_files(refit_dir, keep = refit_keep, recursive = TRUE) else 0L,
    recovery = mp_cleanup_files(recovery_dir, keep = recovery_keep, recursive = TRUE)
  )
  st_prune_empty_dirs(sim_dir)
  st_prune_empty_dirs(input_dir)
  st_prune_empty_dirs(truth_eval_dir)
  if (isTRUE(run_refit)) st_prune_empty_dirs(refit_dir)
  st_prune_empty_dirs(recovery_dir)

  cat("Compact cleanup removed files:",
      paste(paste(names(deleted), deleted, sep = "="), collapse = ", "), "\n")
  invisible(TRUE)
}

for (rep_id in reps) {
  rep_label <- sprintf("rep_%03d", rep_id)
  cat("\n--- Self-test", rep_label, "---\n")

  sim_dir <- file.path(selftest_dir_abs, "sim", rep_label)
  input_dir <- file.path(selftest_dir_abs, "inputs", rep_label)
  truth_eval_dir <- file.path(selftest_dir_abs, "truth_eval", rep_label)
  refit_dir <- file.path(selftest_dir_abs, "refit", rep_label)
  recovery_dir <- file.path(selftest_dir_abs, "recovery", rep_label)
  dir.create(recovery_dir, recursive = TRUE, showWarnings = FALSE)

  st_copy_dir_contents(base_dir_abs, sim_dir)
  sim_par <- prepare_selftest_source_par(sim_dir, source_par, rep_label)

  seed_offset <- seed_base + rep_id * 10L
  seeds <- list(
    length = seed_offset + 1L,
    weight = seed_offset + 2L,
    catch = seed_offset + 3L,
    effort = seed_offset + 4L,
    cpue = seed_offset + 5L,
    tag = seed_offset + 6L,
    age_length = seed_offset + 7L
  )
  writeLines(as.character(seeds$tag), file.path(sim_dir, "simseed"))

  native_tag_info <- NULL
  if (isTRUE(update_tags) && isTRUE(require_native_tags)) {
    proj_info <- st_prepare_projection_input(
      sim_dir = sim_dir,
      program_path_abs = program_path_abs,
      frq_file = file.path(sim_dir, basename(frq_file)),
      par_file = sim_par,
      projection_years = native_tag_projection_years,
      average_years = projection_average_years,
      projection_root = "selftest_proj"
    )
    native_tag_info <- st_run_native_tag_simulation(
      sim_dir = sim_dir,
      program_path_abs = program_path_abs,
      projection_info = proj_info,
      seeds = seeds,
      nsims = native_tag_nsims,
      output_par = "selftest_native_tag.par"
    )
    cat(
      "Native realtag generated:", native_tag_info$report_realtag,
      "(MFCL helper projection only:",
      proj_info$first_projection_year, "-", proj_info$last_projection_year,
      "; pseudo-data estimation period remains the fitted input period)\n"
    )
  }

  if (nzchar(tag_sim_file)) {
    tag_sim_out <- sub("\\.frq$", ".tag_sim", file.path(sim_dir, basename(frq_file)))
    file.copy(st_resolve_path(tag_sim_file, project_root = project_root, must_work = TRUE), tag_sim_out, overwrite = TRUE)
  } else {
    tag_sim_out <- sub("\\.frq$", ".tag_sim", file.path(sim_dir, basename(frq_file)))
    st_write_minimal_tag_sim(tag_sim_out, file.path(sim_dir, basename(frq_file)))
  }

  sim_info <- st_run_mfcl_simulation(
    sim_dir = sim_dir,
    program_path_abs = program_path_abs,
    frq_file = file.path(sim_dir, basename(frq_file)),
    par_file = sim_par,
    output_par = "selftest_sim.par",
    switch_args = sim_switch,
    seeds = seeds
  )
  sim_info$native_tag_info <- native_tag_info

  required_sim <- c("test_lw_sim", "cpue_sim")
  if (isTRUE(update_catch) || isTRUE(effort_conditioned)) required_sim <- c(required_sim, "catch_sim")
  if (isTRUE(update_effort)) required_sim <- c(required_sim, "effort_sim")
  if (isTRUE(update_age_length)) required_sim <- c(required_sim, "agelengthresids.dat")
  if (isTRUE(update_tags) && isTRUE(require_native_tags)) required_sim <- c(required_sim, "report.realtag_1")
  missing_sim <- required_sim[!file.exists(file.path(sim_dir, required_sim))]
  if (length(missing_sim) > 0) {
    warning("Skipping pseudo input build for ", rep_label, "; missing simulation output: ", paste(missing_sim, collapse = ", "))
    run_rows[[length(run_rows) + 1L]] <- data.frame(
      replicate = rep_id,
      sim_status = sim_info$status,
      input_built = FALSE,
      truth_eval_status = NA_integer_,
      refit_status = NA_integer_,
      sim_dir = sim_dir,
      input_dir = input_dir,
      truth_eval_dir = truth_eval_dir,
      refit_dir = refit_dir,
      stringsAsFactors = FALSE
    )
    next
  }

  st_save_truth_payload(sim_dir, sim_info, native_tag_info, seeds, rep_id)

  input_info <- st_build_pseudo_input(
    base_dir = base_dir_abs,
    sim_dir = sim_dir,
    input_dir = input_dir,
    par_file = sim_par,
    update_catch = update_catch,
    update_effort = update_effort,
    update_lw = update_lw,
    update_cpue = update_cpue,
    update_tags = update_tags,
    update_age_length = update_age_length,
    require_native_tags = require_native_tags,
    seeds = seeds
  )
  cat("Pseudo input built:", input_dir, "\n")
  cat("  length rows:", input_info$length_replaced_rows %||% 0L,
      "weight rows:", input_info$weight_replaced_rows %||% 0L,
      "cpue rows:", input_info$cpue_replaced_rows %||% 0L, "\n")
  cat("  tags:", input_info$tag_source %||% "not requested",
      "age-length:", input_info$age_length_source %||% "not requested", "\n")
  if (!isTRUE(input_info$age_length_sample_sizes_matched %||% TRUE)) {
    stop("Age-length sample sizes did not match after pseudo-data generation for ", rep_label)
  }
  if ((input_info$length_sample_size_mismatch %||% 0L) > 0L ||
      (input_info$weight_sample_size_mismatch %||% 0L) > 0L) {
    stop("Length/weight simulated sample-size totals did not match MFCL sample sizes for ", rep_label)
  }

  truth_eval_info <- st_run_truth_on_pseudo(
    input_dir = input_dir,
    eval_dir = truth_eval_dir,
    program_path_abs = program_path_abs,
    tag_report_year1 = "auto"
  )
  cat("Truth-on-pseudo evaluation completed with status:", truth_eval_info$exit_status, "\n")

  refit_status <- NA_integer_
  if (isTRUE(run_refit)) {
    refit_info <- st_run_refit(
      input_dir = input_dir,
      refit_dir = refit_dir,
      program_path_abs = program_path_abs,
      fevals = refit_fevals,
      mode = refit_mode,
      tag_report_year1 = "auto"
    )
    refit_status <- refit_info$exit_status
    cat("Refit completed with status:", refit_status, "\n")

    truth_indepvar <- file.path(sim_dir, "indepvar.rpt")
    if (!file.exists(truth_indepvar)) truth_indepvar <- file.path(base_dir_abs, "indepvar.rpt")
    refit_indepvar <- file.path(refit_dir, "indepvar.rpt")
    st_parameter_recovery(
      truth_indepvar = truth_indepvar,
      refit_indepvar = refit_indepvar,
      out_file = file.path(recovery_dir, "parameter_recovery.csv"),
      key_parameters = key_parameters
    )
    st_derived_recovery(
      truth_dir = sim_dir,
      refit_dir = refit_dir,
      out_file = file.path(recovery_dir, "derived_recovery.csv")
    )
  }

  st_cleanup_selftest_rep(sim_dir, input_dir, truth_eval_dir, refit_dir, recovery_dir, run_refit)

  run_rows[[length(run_rows) + 1L]] <- data.frame(
    replicate = rep_id,
    sim_status = sim_info$status,
    input_built = TRUE,
    truth_eval_status = truth_eval_info$exit_status,
    refit_status = refit_status,
    sim_dir = sim_dir,
    input_dir = input_dir,
    truth_eval_dir = truth_eval_dir,
    refit_dir = refit_dir,
    stringsAsFactors = FALSE
  )
}

run_summary <- if (length(run_rows) > 0) do.call(rbind, run_rows) else data.frame()
utils::write.csv(run_summary, file.path(selftest_dir_abs, "selftest_runs.csv"), row.names = FALSE)
saveRDS(run_summary, file.path(selftest_dir_abs, "selftest_runs.rds"), compress = "xz")

top_keep_dir <- strsplit(gsub("^/+|/+$", "", selftest_dir), "/", fixed = TRUE)[[1]][[1]]
if (!nzchar(top_keep_dir)) top_keep_dir <- "selftest"
cb_condor_keep_only_model_cleanup(keep_dir = top_keep_dir)

cat("\nSelf-test runner finished. Summary:", file.path(selftest_dir_abs, "selftest_runs.csv"), "\n")
