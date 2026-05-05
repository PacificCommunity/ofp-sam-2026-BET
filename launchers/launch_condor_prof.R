# Install the CondorBox package from GitHub (force reinstallation if needed)
# remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE)

remote_user <- "kyuhank"
remote_host <- Sys.getenv("NOU_CONDOR")
github_pat <- Sys.getenv("GIT_PAT")
github_username <- "kyuhank"
github_org <- "PacificCommunity"
github_repo <- "ofp-sam-2026-bet"
docker_image <- "ghcr.io/pacificcommunity/bet-2026:v1.9"
condor_memory <- "12GB"
condor_disk <- "10GB"
condor_cpus <- 2
branch <- "develop_lik"

setwd(here::here())
source("tools/condor_env.R")

config_file <- Sys.getenv("config_file", "configs/2023diag.R")
if (!file.exists(config_file)) stop("Config file not found: ", config_file)
source(config_file, chdir = TRUE)
if (!exists("models") || !is.list(models) || length(models) == 0) {
  stop("No models found in config: ", config_file)
}

run_dir <- Sys.getenv("prof_remote_dir", "develop/Feb_5_prof")
auto_refine_profile <- tolower(Sys.getenv("auto_refine_profile", "0")) %in% c("1", "true", "yes", "y")
auto_refine_k <- suppressWarnings(as.numeric(Sys.getenv("auto_refine_k", "4")))
if (!is.finite(auto_refine_k) || auto_refine_k <= 0) auto_refine_k <- 4
auto_refine_abs <- suppressWarnings(as.numeric(Sys.getenv("auto_refine_abs", "")))
if (!is.finite(auto_refine_abs) || auto_refine_abs < 0) auto_refine_abs <- NA_real_
auto_refine_max <- suppressWarnings(as.integer(Sys.getenv("auto_refine_max", "6")))
if (!is.finite(auto_refine_max) || auto_refine_max < 1) auto_refine_max <- 6L

parse_num_vec <- function(x) {
  vals <- suppressWarnings(as.numeric(unlist(strsplit(as.character(x), "\\s+"))))
  vals[is.finite(vals)]
}

parse_scalar_map <- function(txt) {
  out <- list()
  if (!nzchar(txt)) return(out)
  parts <- trimws(unlist(strsplit(txt, ",")))
  parts <- parts[nzchar(parts)]
  for (p in parts) {
    kv <- trimws(unlist(strsplit(p, ":", fixed = TRUE)))
    if (length(kv) != 2) next
    k <- suppressWarnings(as.integer(kv[1]))
    v <- suppressWarnings(as.integer(kv[2]))
    if (is.finite(k) && is.finite(v)) out[[as.character(k)]] <- as.character(v)
  }
  out
}

parse_target_map <- function(txt) {
  out <- list()
  if (!nzchar(txt)) return(out)
  parts <- trimws(unlist(strsplit(txt, ",")))
  parts <- parts[nzchar(parts)]
  for (p in parts) {
    kv <- trimws(unlist(strsplit(p, ":", fixed = TRUE)))
    if (length(kv) < 2) next
    k <- suppressWarnings(as.integer(kv[1]))
    v <- paste(kv[-1], collapse = ":")
    if (is.finite(k) && nzchar(v)) out[[as.character(k)]] <- v
  }
  out
}

target_scalars_from_map <- function(mp) {
  if (length(mp) == 0) return(integer(0))
  keys <- suppressWarnings(as.integer(names(mp)))
  sort(unique(keys[is.finite(keys)]))
}

resolve_model_scalars <- function(model_cfg, scalars_override, init_map, init_override_map) {
  base_scalars <- sort(unique(as.integer(round(parse_num_vec(model_cfg$scalars)))))
  mapped_targets <- sort(unique(c(
    target_scalars_from_map(init_map),
    target_scalars_from_map(init_override_map)
  )))
  scalars <- if (length(mapped_targets) > 0) mapped_targets else base_scalars
  if (length(scalars_override) > 0) {
    scalars <- intersect(scalars, as.integer(round(scalars_override)))
  }
  sort(unique(as.integer(round(scalars[is.finite(scalars)]))))
}

scalars_override_txt <- Sys.getenv("scalars_override", "")
scalars_override <- parse_num_vec(scalars_override_txt)
init_map <- parse_scalar_map(Sys.getenv("init_from_scalar_map", ""))
init_override_map <- parse_target_map(Sys.getenv("init_par_override_map", ""))

cat("Profile Condor launcher\n")
cat("- config_file:", config_file, "\n")
cat("- remote dir:", run_dir, "\n")
cat("- scalars_override:", ifelse(length(scalars_override) > 0, paste(scalars_override, collapse = " "), "<none>"), "\n")
cat("- init_from_scalar_map entries:", length(init_map), "\n")
cat("- init_par_override_map entries:", length(init_override_map), "\n")
cat("- auto_refine_profile:", auto_refine_profile, "\n")

for (model_name in names(models)) {
  model_scalars <- resolve_model_scalars(models[[model_name]], scalars_override, init_map, init_override_map)
  if (length(model_scalars) == 0) next

  for (sc in model_scalars) {
    prof_env <- models[[model_name]]
    prof_env$scalar <- as.character(sc)

    override_val <- init_override_map[[as.character(sc)]]
    if (!is.null(override_val) && nzchar(override_val)) {
      prof_env$init_par_override <- override_val
    } else {
      mapped <- init_map[[as.character(sc)]]
      if (!is.null(mapped) && nzchar(mapped)) {
        prof_env$init_from_scalar <- mapped
      }
    }

    CondorBox::CondorBox(
      make_options = "prof",
      remote_user = remote_user,
      remote_host = remote_host,
      remote_dir = paste0(github_repo, "/", run_dir, "/", model_name, "_sc", sc),
      github_pat = github_pat,
      github_username = github_username,
      github_org = github_org,
      github_repo = github_repo,
      docker_image = docker_image,
      condor_memory = condor_memory,
      condor_cpus = condor_cpus,
      condor_disk = condor_disk,
      stream_error = "TRUE",
      branch = branch,
      rmclone_script = "no",
      ghcr_login = TRUE,
      exclude_slots = c(
        "slot1@nouofpcand27",
        "slot1@nouofpcand28",
        "slot1@nouofpcand29",
        "slot1@nouofpcand30",
        "slot1_1@suvofpcand26.corp.spc.int",
        "slot1_2@suvofpcand26.corp.spc.int",
        "slot1_3@suvofpcand26.corp.spc.int"
      ),
      custom_batch_name = paste0(model_name, "-sc", sc, "-", format(Sys.time(), "%H:%M:%S_%D")),
      condor_environment = condor_git_safe_env(prof_env)
    )
  }
}

for (model_name in names(models)) {
  model_scalars <- resolve_model_scalars(models[[model_name]], scalars_override, init_map, init_override_map)
  if (length(model_scalars) == 0) next

  for (sc in model_scalars) {
    remote_dir <- paste0(github_repo, "/", run_dir, "/", model_name, "_sc", sc)
    CondorBox::BatchFileHandler(
      remote_user = remote_user,
      remote_host = remote_host,
      folder_name = remote_dir,
      action = "fetch",
      fetch_dir = "model",
      extract_archive = TRUE,
      direct_extract = TRUE,
      archive_name = "output_archive.tar.gz",
      extract_folder = paste0(github_repo, "/model")
    )
  }
}

if (auto_refine_profile) {
  parse_scalars_int <- function(x) sort(unique(as.integer(round(parse_num_vec(x)))))
  nearest_right_else_left <- function(target, scalars) {
    s <- sort(unique(as.integer(scalars)))
    right <- s[s > target]
    if (length(right) > 0) return(as.integer(right[1]))
    left <- s[s < target]
    if (length(left) > 0) return(as.integer(left[length(left)]))
    NA_integer_
  }
  detect_rough_scalars <- function(model_name, scalars) {
    model_dir <- as.character(models[[model_name]]$model_dir)
    rows <- lapply(scalars, function(sc) {
      pp <- file.path(model_dir, "prof", paste0("scalar_", sc), "profile_payload.rds")
      payload <- if (file.exists(pp)) tryCatch(readRDS(pp), error = function(e) NULL) else NULL
      obj <- if (!is.null(payload$obj_fun)) suppressWarnings(as.numeric(payload$obj_fun)) else NA_real_
      data.frame(scalar = as.integer(sc), obj_fun = obj, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, rows)
    df <- df[order(df$scalar), , drop = FALSE]
    if (nrow(df) < 3) return(integer(0))
    if (!any(is.finite(df$obj_fun))) return(integer(0))
    df$change <- df$obj_fun - min(df$obj_fun, na.rm = TRUE)
    rough <- rep(NA_real_, nrow(df))
    for (i in 2:(nrow(df) - 1)) {
      yl <- df$change[i - 1]; yc <- df$change[i]; yr <- df$change[i + 1]
      if (is.finite(yl) && is.finite(yc) && is.finite(yr)) rough[i] <- abs(yr - 2 * yc + yl)
    }
    finite_rough <- rough[is.finite(rough)]
    if (length(finite_rough) == 0) return(integer(0))
    med <- stats::median(finite_rough, na.rm = TRUE)
    madv <- stats::mad(finite_rough, center = med, constant = 1, na.rm = TRUE)
    thr <- med + auto_refine_k * madv
    if (is.finite(auto_refine_abs)) thr <- max(thr, auto_refine_abs)
    idx <- which(is.finite(rough) & rough > thr)
    if (length(idx) == 0) return(integer(0))
    flagged <- df$scalar[idx]
    flagged <- flagged[order(rough[idx], decreasing = TRUE)]
    unique(as.integer(head(flagged, auto_refine_max)))
  }

  rerun_plan <- list()
  for (model_name in names(models)) {
    model_scalars <- parse_scalars_int(models[[model_name]]$scalars)
    if (length(scalars_override) > 0) model_scalars <- intersect(model_scalars, as.integer(round(scalars_override)))
    if (length(model_scalars) < 3) next
    flagged <- detect_rough_scalars(model_name, model_scalars)
    if (length(flagged) == 0) next
    rerun_plan[[model_name]] <- flagged
    cat("[auto_refine]", model_name, "flagged scalars:", paste(flagged, collapse = " "), "\n")
  }

  for (model_name in names(rerun_plan)) {
    model_scalars <- parse_scalars_int(models[[model_name]]$scalars)
    flagged <- rerun_plan[[model_name]]
    for (sc in flagged) {
      prof_env <- models[[model_name]]
      prof_env$scalar <- as.character(sc)
      donor <- nearest_right_else_left(sc, model_scalars)
      if (is.finite(donor)) prof_env$init_from_scalar <- as.character(donor)
      CondorBox::CondorBox(
        make_options = "prof",
        remote_user = remote_user,
        remote_host = remote_host,
        remote_dir = paste0(github_repo, "/", run_dir, "/", model_name, "_sc", sc, "_refine"),
        github_pat = github_pat,
        github_username = github_username,
        github_org = github_org,
        github_repo = github_repo,
        docker_image = docker_image,
        condor_memory = condor_memory,
        condor_cpus = condor_cpus,
        condor_disk = condor_disk,
        stream_error = "TRUE",
        branch = branch,
        rmclone_script = "no",
        ghcr_login = TRUE,
        exclude_slots = c(
          "slot1@nouofpcand27",
          "slot1@nouofpcand28",
          "slot1@nouofpcand29",
          "slot1@nouofpcand30",
          "slot1_1@suvofpcand26.corp.spc.int",
          "slot1_2@suvofpcand26.corp.spc.int",
          "slot1_3@suvofpcand26.corp.spc.int"
        ),
        custom_batch_name = paste0(model_name, "-sc", sc, "-refine-", format(Sys.time(), "%H:%M:%S_%D")),
        condor_environment = condor_git_safe_env(prof_env)
      )
    }
  }

  for (model_name in names(rerun_plan)) {
    flagged <- rerun_plan[[model_name]]
    for (sc in flagged) {
      remote_dir <- paste0(github_repo, "/", run_dir, "/", model_name, "_sc", sc, "_refine")
      CondorBox::BatchFileHandler(
        remote_user = remote_user,
        remote_host = remote_host,
        folder_name = remote_dir,
        action = "fetch",
        fetch_dir = "model",
        extract_archive = TRUE,
        direct_extract = TRUE,
        archive_name = "output_archive.tar.gz",
        extract_folder = paste0(github_repo, "/model")
      )
      CondorBox::BatchFileHandler(
        remote_user = remote_user,
        remote_host = remote_host,
        folder_name = remote_dir,
        file_name = "clone_job.sh",
        action = "delete"
      )
    }
  }
}

for (model_name in names(models)) {
  model_scalars <- resolve_model_scalars(models[[model_name]], scalars_override, init_map, init_override_map)
  if (length(model_scalars) == 0) next

  for (sc in model_scalars) {
    CondorBox::BatchFileHandler(
      remote_user = remote_user,
      remote_host = remote_host,
      folder_name = paste0(github_repo, "/", run_dir, "/", model_name, "_sc", sc),
      file_name = "clone_job.sh",
      action = "delete"
    )
  }
}
