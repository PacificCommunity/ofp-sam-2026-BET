## load libraries
library(FLR4MFCL)
library(CondorBox)

source("tools/ProfLike_utils.R")
source("tools/model_payload.R")
source("tools/post_hessian.R")
source("tools/condor_archive_cleanup.R")
source("tools/jitter.R")

## environment variables
program_path <- Sys.getenv("program_path", "mfcl/exe/mfclo64_2026_02_04_vsn2278")
Sys.setenv("PROGRAM_PATH" = paste0("../../", program_path))
base_dir <- Sys.getenv("base_dir", "mfcl/inputs/2023_rep")
model_dir <- Sys.getenv("model_dir", "model/base")

## Convert to absolute paths using getwd() (assumes script runs from project root)
project_root <- getwd()
program_path_abs <- file.path(project_root, program_path)
base_dir_abs <- file.path(project_root, base_dir)

## Profile likelihood settings
scalar <- as.numeric(Sys.getenv("scalar", "90"))
Reps <- as.integer(unlist(strsplit(Sys.getenv("Reps", "1 1 1 1 1 1"), "\\s+")))
names(Reps) <- paste0("Reps", 1:length(Reps))
# indepvar_reps: reps for indepvar fixed-parameter profile (no penalty ramp).
# Defaults to Reps4 from the Reps vector; set independently via env var if needed.
indepvar_reps <- suppressWarnings(as.integer(Sys.getenv("indepvar_reps", "")))
if (!is.finite(indepvar_reps) || indepvar_reps < 1L) indepvar_reps <- Reps["Reps4"]
QuantityType <- as.numeric(Sys.getenv("QuantityType", "2"))
AgeFlags <- c(
  Af172 = as.numeric(Sys.getenv("Af172", "0")),
  Af173 = as.numeric(Sys.getenv("Af173", "0")),
  Af174 = as.numeric(Sys.getenv("Af174", "0"))
)
init_par_override <- Sys.getenv("init_par_override", "")
init_from_scalar <- suppressWarnings(as.numeric(Sys.getenv("init_from_scalar", "")))
prof_init_map_rds <- Sys.getenv("prof_init_map_rds", "")
prof_hessian <- tolower(Sys.getenv("prof_hessian", Sys.getenv("likelihood_hessian", Sys.getenv("hessian", "0")))) %in% c("1", "true", "yes", "y")
prof_fix_indepvar <- Sys.getenv("prof_fix_indepvar", "")
prof_fix_values <- Sys.getenv("prof_fix_values", "")
prof_extra_switch <- trimws(Sys.getenv("prof_extra_switch", ""))
profile_set_name_env <- Sys.getenv("profile_set_name", "")
profile_set_label_env <- Sys.getenv("profile_set_label", "")
profile_set_tag_env <- Sys.getenv("profile_set_tag", "")
prof_use_quantity_penalty_raw <- trimws(Sys.getenv("prof_use_quantity_penalty", ""))
prof_use_quantity_penalty <- if (nzchar(prof_use_quantity_penalty_raw)) {
  tolower(prof_use_quantity_penalty_raw) %in% c("1", "true", "yes", "y")
} else {
  !nzchar(trimws(prof_fix_indepvar))
}

safe_read_scalar <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path) || !file.exists(path)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(read.table(path)))
}

detect_quantity_file <- function(path) {
  avg_bio_path <- file.path(path, "avg_bio")
  if (file.exists(avg_bio_path)) return(list(label = "avg_bio", path = avg_bio_path))
  rel_dep_path <- file.path(path, "relative_depletion")
  if (file.exists(rel_dep_path)) return(list(label = "relative_depletion", path = rel_dep_path))
  stop("No quantity file found in ", path, " (expected avg_bio or relative_depletion)")
}

detect_reference_quantity_file <- function(model_dir, scalar_dir) {
  model_try <- tryCatch(detect_quantity_file(model_dir), error = function(e) NULL)
  if (!is.null(model_try)) return(model_try)
  scalar_try <- tryCatch(detect_quantity_file(scalar_dir), error = function(e) NULL)
  if (!is.null(scalar_try)) return(scalar_try)
  stop("No reference quantity file found in either ", model_dir, " or ", scalar_dir)
}

quantity_label_from_type <- function(quantity_type) {
  if (isTRUE(all.equal(as.numeric(quantity_type), 1))) "relative_depletion" else "avg_bio"
}

resolve_prof_init_map_path <- function(prof_init_map_rds, project_root, base_dir_abs, scalar_dir) {
  if (!nzchar(prof_init_map_rds)) return(NA_character_)
  candidates <- unique(c(
    prof_init_map_rds,
    if (!grepl("^/", prof_init_map_rds)) file.path(project_root, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(base_dir_abs, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(scalar_dir, prof_init_map_rds) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(scalar_dir, basename(prof_init_map_rds)) else NA_character_,
    if (!grepl("^/", prof_init_map_rds)) file.path(base_dir_abs, basename(prof_init_map_rds)) else NA_character_
  ))
  candidates <- candidates[is.character(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

load_prof_init_map <- function(prof_init_map_rds, project_root, base_dir_abs, scalar_dir) {
  map_path <- resolve_prof_init_map_path(
    prof_init_map_rds = prof_init_map_rds,
    project_root = project_root,
    base_dir_abs = base_dir_abs,
    scalar_dir = scalar_dir
  )
  if (!is.character(map_path) || length(map_path) != 1 || !nzchar(map_path) || !file.exists(map_path)) {
    return(list(entries = NULL, path = NA_character_))
  }
  obj <- tryCatch(readRDS(map_path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj)) {
    warning("prof_init_map_rds could not be read as list: ", map_path)
    return(list(entries = NULL, path = map_path))
  }
  entries <- NULL
  if (!is.null(obj$entries) && is.list(obj$entries)) {
    entries <- obj$entries
  } else if (!is.null(obj$par_by_scalar) && is.list(obj$par_by_scalar)) {
    entries <- obj$par_by_scalar
  }
  if (is.null(entries) || !is.list(entries)) {
    warning("prof_init_map_rds has no usable entries/par_by_scalar: ", map_path)
    return(list(entries = NULL, path = map_path))
  }
  list(entries = entries, path = map_path)
}

extract_donor_par_lines <- function(entries, donor_scalar) {
  if (is.null(entries) || !is.list(entries) || !is.finite(donor_scalar)) return(character(0))
  key <- as.character(as.integer(round(donor_scalar)))
  val <- entries[[key]]
  if (is.null(val)) return(character(0))
  if (is.list(val) && !is.null(val$par_lines)) val <- val$par_lines
  if (is.null(val)) return(character(0))
  as.character(val)
}

resolve_init_par <- function(init_par_override, init_from_scalar, scalar_dir, prof_dir, fallback_par, init_map_entries = NULL) {
  # 1) Explicit override path/file
  if (nzchar(init_par_override)) {
    candidate <- if (grepl("^/", init_par_override)) init_par_override else file.path(scalar_dir, init_par_override)
    if (file.exists(candidate)) {
      out <- file.path(scalar_dir, "warm_init_override.par")
      file.copy(candidate, out, overwrite = TRUE)
      return(list(path = out, source = "override", donor = NA_integer_))
    }
    warning("init_par_override file not found: ", candidate, " -> fallback")
  }

  # 2) Donor scalar final par from model_dir/prof/scalar_<donor>
  if (is.finite(init_from_scalar)) {
    donor_scalar_label <- format(init_from_scalar, scientific = FALSE, trim = TRUE)
    donor_lines <- extract_donor_par_lines(init_map_entries, init_from_scalar)
    if (length(donor_lines) > 0) {
      out <- file.path(scalar_dir, paste0("warm_init_from_rds_", donor_scalar_label, ".par"))
      writeLines(donor_lines, con = out, useBytes = TRUE)
      return(list(path = out, source = "rds", donor = init_from_scalar))
    }

    donor_dir <- file.path(prof_dir, paste0("scalar_", donor_scalar_label))
    donor_payload <- file.path(donor_dir, "profile_payload.rds")
    if (file.exists(donor_payload)) {
      payload_obj <- tryCatch(readRDS(donor_payload), error = function(e) NULL)
      payload_lines <- NULL
      if (is.list(payload_obj) && !is.null(payload_obj$par_lines)) {
        payload_lines <- as.character(payload_obj$par_lines)
      }
      if (!is.null(payload_lines) && length(payload_lines) > 0) {
        out <- file.path(scalar_dir, paste0("warm_init_from_payload_", donor_scalar_label, ".par"))
        writeLines(payload_lines, con = out, useBytes = TRUE)
        return(list(path = out, source = "payload", donor = init_from_scalar))
      }
    }

    donor_par <- mp_final_par(donor_dir)
    if (!is.null(donor_par) && file.exists(donor_par)) {
      out <- file.path(scalar_dir, paste0("warm_init_from_scalar_", donor_scalar_label, ".par"))
      file.copy(donor_par, out, overwrite = TRUE)
      return(list(path = out, source = "neighbor", donor = init_from_scalar))
    }
    warning("init_from_scalar set but donor final par not found in: ", donor_dir, " -> fallback")
  }

  # 3) Default
  list(path = fallback_par, source = "default", donor = NA_integer_)
}

parse_tokens <- function(x) {
  txt <- paste(as.character(x), collapse = " ")
  if (!nzchar(trimws(txt))) return(character(0))
  toks <- unlist(strsplit(txt, "[[:space:],]+", perl = TRUE))
  toks <- trimws(toks)
  toks[nzchar(toks)]
}

sanitize_profile_set_key <- function(x, max_len = 80L) {
  txt <- paste(parse_tokens(x), collapse = "__")
  txt <- gsub("[^A-Za-z0-9]+", "_", txt)
  txt <- gsub("_+", "_", txt)
  txt <- gsub("^_|_$", "", txt)
  if (!nzchar(txt)) txt <- "indepvar"
  if (nchar(txt) > max_len) txt <- substr(txt, 1L, max_len)
  txt
}

first_nonempty_string <- function(...) {
  vals <- list(...)
  for (val in vals) {
    txt <- trimws(paste(as.character(val), collapse = " "))
    if (nzchar(txt)) return(txt)
  }
  ""
}

resolve_profile_storage <- function(model_dir, prof_fix_indepvar, profile_set_name = "", profile_set_label = "", profile_set_tag = "") {
  prof_fix_indepvar <- trimws(as.character(prof_fix_indepvar))
  if (!nzchar(prof_fix_indepvar)) {
    return(list(
      prof_subdir = "prof",
      prof_dir = file.path(model_dir, "prof"),
      profile_set_key = NA_character_,
      profile_set_label = NA_character_,
      profile_root_dir = file.path(model_dir, "prof")
    ))
  }

  profile_set_label <- first_nonempty_string(
    profile_set_label,
    profile_set_name,
    paste(parse_tokens(prof_fix_indepvar), collapse = ", ")
  )
  profile_set_key <- first_nonempty_string(
    profile_set_tag,
    sanitize_profile_set_key(profile_set_label),
    sanitize_profile_set_key(prof_fix_indepvar)
  )
  profile_root_dir <- file.path(model_dir, "prof_indepvar")
  prof_subdir <- file.path("prof_indepvar", profile_set_key)

  list(
    prof_subdir = prof_subdir,
    prof_dir = file.path(model_dir, prof_subdir),
    profile_set_key = profile_set_key,
    profile_set_label = profile_set_label,
    profile_root_dir = profile_root_dir
  )
}

run_system2_in_dir <- function(command, args = character(0), dir = ".", stdout = "", stderr = "") {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dir)
  system2(command = command, args = args, stdout = stdout, stderr = stderr)
}

append_extra_switch <- function(base_switch, extra_switch) {
  extra <- trimws(as.character(extra_switch))
  if (!nzchar(extra)) return(base_switch)

  extra_toks <- parse_tokens(extra)
  if (length(extra_toks) %% 3 != 0) {
    stop("prof_extra_switch must be triplets: type flag value")
  }

  m <- regexec("^(-switch[[:space:]]+)([0-9]+)([[:space:]].*)$", base_switch, perl = TRUE)
  mm <- regmatches(base_switch, m)[[1]]
  if (length(mm) < 4) {
    stop("Internal error: invalid base -switch format")
  }

  base_n <- suppressWarnings(as.integer(mm[3]))
  if (!is.finite(base_n)) {
    stop("Internal error: invalid base switch count")
  }

  new_n <- base_n + as.integer(length(extra_toks) / 3)
  paste0(mm[2], new_n, mm[4], " ", paste(extra_toks, collapse = " "))
}

resolve_indepvar_path <- function(scalar_dir, model_dir, base_dir_abs) {
  candidates <- c(
    file.path(scalar_dir, "indepvar.rpt"),
    file.path(model_dir, "indepvar.rpt"),
    file.path(base_dir_abs, "indepvar.rpt")
  )
  candidates <- unique(candidates[is.character(candidates) & nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

apply_indepvar_fix <- function(init_par_file,
                               scalar_dir,
                               model_dir,
                               base_dir_abs,
                               indepvar_select,
                               indepvar_values,
                               scalar_percent = NA_real_,
                               baseline_par_file = NA_character_) {
  tokens <- parse_tokens(indepvar_select)
  if (length(tokens) == 0) {
    return(list(
      applied = FALSE,
      reason = "no_selection",
      indepvar_file = NA_character_,
      par_file = init_par_file,
      details = NULL
    ))
  }

  indepvar_path <- resolve_indepvar_path(scalar_dir, model_dir, base_dir_abs)
  if (!is.character(indepvar_path) || length(indepvar_path) != 1 || !nzchar(indepvar_path) || !file.exists(indepvar_path)) {
    stop("prof_fix_indepvar is set but indepvar.rpt was not found in scalar/model/base directories.")
  }

  init_par_obj <- suppressWarnings(tryCatch(read.MFCLPar(init_par_file), error = function(e) NULL))
  if (is.null(init_par_obj)) {
    stop("Failed to read Initp .par for indepvar fixing: ", init_par_file)
  }

  indepvar_map <- build_indepvar_mapping(init_par_obj, indepvar_file = indepvar_path, tol = 1e-14)
  if (is.null(indepvar_map) || nrow(indepvar_map$mapping) == 0) {
    stop("Failed to build indepvar mapping from: ", indepvar_path)
  }

  report <- parse_indepvar_report(indepvar_path)
  if (is.null(report) || nrow(report) != nrow(indepvar_map$mapping)) {
    stop("Could not parse indepvar.rpt consistently for fixing: ", indepvar_path)
  }

  selected_rows <- integer(0)
  for (tok in tokens) {
    if (grepl("^[0-9]+$", tok)) {
      hit <- which(report$Index == as.integer(tok))
    } else {
      hit <- which(report$Var_name == tok)
    }
    if (length(hit) == 0) {
      stop("prof_fix_indepvar token not found in indepvar.rpt: ", tok)
    }
    selected_rows <- c(selected_rows, hit[[1]])
  }
  selected_rows <- unique(selected_rows)

  if (any(!indepvar_map$mapping$mapped[selected_rows])) {
    bad <- indepvar_map$mapping$Var_name[selected_rows][!indepvar_map$mapping$mapped[selected_rows]]
    stop("Selected indepvar parameters are not mapped to .par slots: ", paste(bad, collapse = ", "))
  }

  current_values <- extract_indepvar_values(init_par_obj, indepvar_map)

  value_tokens <- parse_tokens(indepvar_values)
  if (length(value_tokens) == 0) {
    scale_pct <- suppressWarnings(as.numeric(scalar_percent))
    baseline_values <- current_values

    if (is.finite(scale_pct) && is.character(baseline_par_file) && length(baseline_par_file) == 1 && nzchar(baseline_par_file) && file.exists(baseline_par_file)) {
      baseline_par_obj <- suppressWarnings(tryCatch(read.MFCLPar(baseline_par_file), error = function(e) NULL))
      if (!is.null(baseline_par_obj)) {
        baseline_values <- extract_indepvar_values(baseline_par_obj, indepvar_map)
      }
    }

    if (is.finite(scale_pct)) {
      target_values <- as.numeric(baseline_values[selected_rows]) * scale_pct / 100
    } else {
      target_values <- as.numeric(current_values[selected_rows])
    }
  } else {
    parsed_values <- suppressWarnings(as.numeric(value_tokens))
    if (any(!is.finite(parsed_values))) {
      stop("prof_fix_values includes non-numeric values.")
    }
    if (length(parsed_values) == 1L && length(selected_rows) > 1L) {
      target_values <- rep(parsed_values[[1]], length(selected_rows))
    } else if (length(parsed_values) == length(selected_rows)) {
      target_values <- parsed_values
    } else {
      stop("prof_fix_values length must be 1 or match number of selected indepvar parameters.")
    }
  }

  new_values <- current_values
  new_values[selected_rows] <- target_values

  fixed_par_obj <- inject_indepvar_values(init_par_obj, indepvar_map, new_values)
  out_par <- file.path(scalar_dir, "warm_init_indepvar_fixed.par")
  FLR4MFCL::write(fixed_par_obj, file = out_par)

  details <- data.frame(
    Index = report$Index[selected_rows],
    Var_name = report$Var_name[selected_rows],
    value_before = current_values[selected_rows],
    value_after = target_values,
    L_bound = suppressWarnings(as.numeric(report$L_bound[selected_rows])),
    U_bound = suppressWarnings(as.numeric(report$U_bound[selected_rows])),
    stringsAsFactors = FALSE
  )

  list(
    applied = TRUE,
    reason = "ok",
    indepvar_file = indepvar_path,
    par_file = out_par,
    details = details
  )
}

## Create scalar-specific directory inside prof folder
profile_storage <- resolve_profile_storage(
  model_dir = model_dir,
  prof_fix_indepvar = prof_fix_indepvar,
  profile_set_name = profile_set_name_env,
  profile_set_label = profile_set_label_env,
  profile_set_tag = profile_set_tag_env
)
prof_subdir <- profile_storage$prof_subdir
prof_dir <- profile_storage$prof_dir
scalar_dir <- file.path(prof_dir, paste0("scalar_", scalar))

cat("Running Profile Likelihood\n")
cat("Base directory:", base_dir_abs, "\n")
cat("Model directory:", model_dir, "\n")
cat("Prof directory:", prof_dir, "\n")
cat("Prof subdir:", prof_subdir, "\n")
cat("Profile set key:", ifelse(is.character(profile_storage$profile_set_key) && nzchar(profile_storage$profile_set_key), profile_storage$profile_set_key, "<none>"), "\n")
cat("Profile set label:", ifelse(is.character(profile_storage$profile_set_label) && nzchar(profile_storage$profile_set_label), profile_storage$profile_set_label, "<none>"), "\n")
cat("Scalar directory:", scalar_dir, "\n")
cat("Scalar:", scalar, "\n")
cat("Reps:", Reps, "\n")
cat("QuantityType:", QuantityType, "\n")
cat("AgeFlags:", AgeFlags, "\n")
cat("init_par_override:", ifelse(nzchar(init_par_override), init_par_override, "<none>"), "\n")
cat("init_from_scalar:", ifelse(is.finite(init_from_scalar), as.character(init_from_scalar), "<none>"), "\n")
cat("prof_init_map_rds:", ifelse(nzchar(prof_init_map_rds), prof_init_map_rds, "<none>"), "\n")
cat("prof_fix_indepvar:", ifelse(nzchar(prof_fix_indepvar), prof_fix_indepvar, "<none>"), "\n")
cat("prof_fix_values:", ifelse(nzchar(prof_fix_values), prof_fix_values, "<none>"), "\n")
cat("prof_extra_switch:", ifelse(nzchar(prof_extra_switch), prof_extra_switch, "<none>"), "\n")
cat("prof_use_quantity_penalty:", prof_use_quantity_penalty, "\n")
cat("Profile post-hessian:", prof_hessian, "\n")

## Create scalar directory and copy all files from base_dir (inputs)
dir.create(scalar_dir, recursive = TRUE, showWarnings = FALSE)
if (nzchar(trimws(prof_fix_indepvar))) {
  saveRDS(
    list(
      profile_source = "indepvar",
      profile_set_key = profile_storage$profile_set_key,
      profile_set_label = profile_storage$profile_set_label,
      prof_fix_indepvar = prof_fix_indepvar,
      prof_fix_values = prof_fix_values
    ),
    file = file.path(prof_dir, "profile_set_info.rds"),
    compress = "xz"
  )
}
files_to_copy <- list.files(base_dir_abs, full.names = TRUE)
file.copy(files_to_copy, to = scalar_dir, overwrite = TRUE, recursive = TRUE)

par_files <- list.files(scalar_dir, pattern = "\\.par$", full.names = TRUE)
frq_file <- list.files(scalar_dir, pattern = "\\.frq$", full.names = FALSE)
if (length(par_files) == 0) stop("No .par files found in directory: ", scalar_dir)

file_info <- file.info(par_files)
most_recent <- rownames(file_info)[which.max(file_info$mtime)]
cat("Most recent par file:", basename(most_recent), "\n")

init_map_obj <- load_prof_init_map(
  prof_init_map_rds = prof_init_map_rds,
  project_root = project_root,
  base_dir_abs = base_dir_abs,
  scalar_dir = scalar_dir
)
if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path) && file.exists(init_map_obj$path)) {
  cat("Loaded prof_init_map_rds:", init_map_obj$path, "\n")
}

init_info <- resolve_init_par(
  init_par_override = init_par_override,
  init_from_scalar = init_from_scalar,
  scalar_dir = scalar_dir,
  prof_dir = prof_dir,
  fallback_par = most_recent,
  init_map_entries = init_map_obj$entries
)
init_par_file <- init_info$path
init_source <- init_info$source
cat("Using Initp:", basename(init_par_file), "(source:", init_source, ")\n")

indepvar_fix_info <- list(
  applied = FALSE,
  reason = "disabled",
  indepvar_file = NA_character_,
  par_file = init_par_file,
  details = NULL,
  lock_rds = NA_character_
)
if (nzchar(prof_fix_indepvar)) {
  indepvar_fix_info <- apply_indepvar_fix(
    init_par_file = init_par_file,
    scalar_dir = scalar_dir,
    model_dir = model_dir,
    base_dir_abs = base_dir_abs,
    indepvar_select = prof_fix_indepvar,
    indepvar_values = prof_fix_values,
    scalar_percent = scalar,
    baseline_par_file = most_recent
  )
  init_par_file <- indepvar_fix_info$par_file
  init_source <- paste0(init_source, "+indepvar_fix")
  cat("Applied indepvar fixing using:", basename(indepvar_fix_info$indepvar_file), "\n")
  if (is.data.frame(indepvar_fix_info$details) && nrow(indepvar_fix_info$details) > 0) {
    indepvar_fix_info$lock_rds <- file.path(scalar_dir, "indepvar_lock_spec.rds")
    saveRDS(
      list(
        Index = indepvar_fix_info$details$Index,
        Var_name = indepvar_fix_info$details$Var_name,
        value_after = indepvar_fix_info$details$value_after,
        L_bound = indepvar_fix_info$details$L_bound,
        U_bound = indepvar_fix_info$details$U_bound
      ),
      file = indepvar_fix_info$lock_rds,
      compress = "xz"
    )
    cat("Fixed parameters:", paste(indepvar_fix_info$details$Var_name, collapse = ", "), "\n")
  }
  cat("Using fixed Initp:", basename(init_par_file), "(source:", init_source, ")\n")
}

quantity_label <- quantity_label_from_type(QuantityType)
reference_quantity <- NA_real_
target_quantity <- NA_real_
if (isTRUE(prof_use_quantity_penalty)) {
  reference_quantity_path <- file.path(scalar_dir, quantity_label)
  if (!file.exists(reference_quantity_path)) {
    cat("No reference quantity file found; refreshing from", basename(most_recent), "\n")
    unlink(reference_quantity_path, force = TRUE)
    ref_par <- file.path(scalar_dir, paste0("reference_", quantity_label, ".par"))
    ref_switch <- paste(
      "-switch 10 2 32 1 1 187 0 1 188 0 -999 55 0 1 1 1",
      "1 346", QuantityType,
      "1 347 0",
      "1 348 0",
      "2 172", AgeFlags["Af172"],
      "2 173", AgeFlags["Af173"],
      "2 174", AgeFlags["Af174"]
    )
    ref_switch <- append_extra_switch(ref_switch, prof_extra_switch)
    ref_args <- c(
      frq_file,
      basename(most_recent),
      basename(ref_par),
      strsplit(ref_switch, "\\s+", perl = TRUE)[[1]]
    )
    ref_args <- ref_args[nzchar(ref_args)]
    ref_status <- run_system2_in_dir(
      command = program_path_abs,
      args = ref_args,
      dir = scalar_dir,
      stdout = "",
      stderr = ""
    )
    # MFCL may emit non-zero status even when avg_bio/relative_depletion is produced.
    # Treat missing reference output as the true failure condition.
    if (!file.exists(reference_quantity_path)) {
      stop(
        "Reference quantity refresh did not create ",
        basename(reference_quantity_path),
        " (status=",
        ref_status,
        ")."
      )
    }
  }

  initial_quantity_info <- detect_reference_quantity_file(model_dir, scalar_dir)
  reference_quantity <- safe_read_scalar(initial_quantity_info$path)
  target_quantity <- reference_quantity * scalar / 100
} else {
  cat("Quantity penalty OFF for this profile run: skip avg_bio/relative_depletion switches.\n")
}

generate_proflike_script(
  Prog = program_path_abs,
  Reps = Reps,
  AgeFlags = AgeFlags,
  QuantityType = QuantityType,
  UseQuantityPenalty = prof_use_quantity_penalty,
  FixedMLE = if (isTRUE(prof_use_quantity_penalty)) reference_quantity else NA_real_,
  ExtraSwitch = prof_extra_switch,
  IndepvarReps = indepvar_reps,
  IndepvarLockRds = if (is.character(indepvar_fix_info$lock_rds) && nzchar(indepvar_fix_info$lock_rds)) indepvar_fix_info$lock_rds else "",
  IndepvarFile = if (is.character(indepvar_fix_info$indepvar_file) && nzchar(indepvar_fix_info$indepvar_file)) indepvar_fix_info$indepvar_file else "",
  LockScript = file.path(project_root, "runners", "apply_indepvar_lock.R"),
  Frq = frq_file,
  Mults = scalar,
  Initp = basename(init_par_file),
  filename = file.path(scalar_dir, "ProfLike.sh")
)

prof_status <- run_system2_in_dir(
  command = "bash",
  args = c("./ProfLike.sh"),
  dir = scalar_dir,
  stdout = "",
  stderr = ""
)
if (!is.numeric(prof_status) || length(prof_status) != 1 || is.na(prof_status) || as.integer(prof_status) != 0L) {
  stop("ProfLike.sh failed (status=", prof_status, ").")
}

final_profile_par <- mp_final_par(scalar_dir)
final_par_lines <- if (!is.null(final_profile_par) && file.exists(final_profile_par)) {
  tryCatch(readLines(final_profile_par), error = function(e) character(0))
} else {
  character(0)
}

hessian_summary <- mp_run_post_hessian(
  work_dir = scalar_dir,
  program_path_abs = program_path_abs,
  program_path = program_path,
  frq_file = frq_file,
  input_par = if (!is.null(final_profile_par)) basename(final_profile_par) else basename(init_par_file),
  project_root = project_root,
  requested = prof_hessian
)

if (isTRUE(prof_use_quantity_penalty)) {
  final_quantity_info <- detect_quantity_file(scalar_dir)
  actual_quantity <- safe_read_scalar(final_quantity_info$path)
  target_rel_err <- suppressWarnings(abs(actual_quantity - target_quantity) / pmax(abs(target_quantity), .Machine$double.eps))
  quantity_label_out <- final_quantity_info$label
} else {
  actual_quantity <- NA_real_
  target_rel_err <- NA_real_
  quantity_label_out <- quantity_label
}

info_list <- list(
  Reps = Reps,
  AgeFlags = AgeFlags,
  scalar = scalar,
  use_quantity_penalty = isTRUE(prof_use_quantity_penalty),
  quantity_label = quantity_label_out,
  reference_quantity = reference_quantity,
  target_quantity = target_quantity,
  actual_quantity = actual_quantity,
  target_rel_err = target_rel_err,
  frq_file = frq_file,
  program_path = program_path,
  model_dir = model_dir,
  scalar_dir = scalar_dir,
  init_source = init_source,
  init_par_used = basename(init_par_file),
  init_par_override = if (nzchar(init_par_override)) init_par_override else NA_character_,
  init_from_scalar = if (is.finite(init_info$donor)) init_info$donor else NA_integer_,
  indepvar_fix_applied = isTRUE(indepvar_fix_info$applied),
  indepvar_fix_file = if (is.character(indepvar_fix_info$indepvar_file) && nzchar(indepvar_fix_info$indepvar_file)) indepvar_fix_info$indepvar_file else NA_character_,
  indepvar_fix_details = indepvar_fix_info$details,
  indepvar_lock_rds = if (is.character(indepvar_fix_info$lock_rds) && nzchar(indepvar_fix_info$lock_rds)) indepvar_fix_info$lock_rds else NA_character_,
  profile_set_key = if (is.character(profile_storage$profile_set_key) && nzchar(profile_storage$profile_set_key)) profile_storage$profile_set_key else NA_character_,
  profile_set_label = if (is.character(profile_storage$profile_set_label) && nzchar(profile_storage$profile_set_label)) profile_storage$profile_set_label else NA_character_,
  prof_init_map_rds = if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path)) init_map_obj$path else NA_character_,
  final_par_lines = final_par_lines,
  hessian = hessian_summary
)

saveRDS(info_list, file = file.path(scalar_dir, "info.rds"), compress = "xz")

profile_payload <- mp_build_profile_payload(scalar_dir)
if (!is.null(profile_payload) && is.list(profile_payload)) {
  profile_payload$par_lines <- final_par_lines
  profile_payload$init_source <- init_source
  profile_payload$init_from_scalar <- if (is.finite(init_info$donor)) init_info$donor else NA_integer_
  profile_payload$indepvar_fix_applied <- isTRUE(indepvar_fix_info$applied)
  profile_payload$indepvar_fix_file <- if (is.character(indepvar_fix_info$indepvar_file) && nzchar(indepvar_fix_info$indepvar_file)) indepvar_fix_info$indepvar_file else NA_character_
  profile_payload$indepvar_fix_details <- indepvar_fix_info$details
  profile_payload$indepvar_lock_rds <- if (is.character(indepvar_fix_info$lock_rds) && nzchar(indepvar_fix_info$lock_rds)) indepvar_fix_info$lock_rds else NA_character_
  profile_payload$profile_set_key <- if (is.character(profile_storage$profile_set_key) && nzchar(profile_storage$profile_set_key)) profile_storage$profile_set_key else NA_character_
  profile_payload$profile_set_label <- if (is.character(profile_storage$profile_set_label) && nzchar(profile_storage$profile_set_label)) profile_storage$profile_set_label else NA_character_
  profile_payload$prof_init_map_rds <- if (is.character(init_map_obj$path) && length(init_map_obj$path) == 1 && nzchar(init_map_obj$path)) init_map_obj$path else NA_character_
}
saveRDS(profile_payload, file = file.path(scalar_dir, "profile_payload.rds"), compress = "xz")

deleted_n <- mp_cleanup_files(scalar_dir, keep = c("profile_payload.rds", "info.rds"), recursive = TRUE)
cat("Cleanup removed", deleted_n, "non-core files in", scalar_dir, "\n")

skip_archive_cleanup <- tolower(Sys.getenv("skip_condor_archive_cleanup", "0")) %in% c("1", "true", "yes", "y")
if (isTRUE(skip_archive_cleanup)) {
  cat("Skipping Condor archive cleanup (skip_condor_archive_cleanup=1)\n")
} else {
  cb_condor_keep_only_model_cleanup()
}
cat("✅ Profile likelihood completed for scalar", scalar, "\n")
