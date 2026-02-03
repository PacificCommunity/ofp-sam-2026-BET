#!/usr/bin/env Rscript
## Condor Job Launcher
## Simple and straightforward launcher based on original design

## =============================================================================
## LOAD CONFIGURATION
## =============================================================================

source("config.R")
library(CondorBox)

## =============================================================================
## PARSE ARGUMENTS
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  cat("\nUsage: Rscript launch.R <job_type>[,job_type,...] [options]\n\n")
  cat("Job types:\n")
  cat("  model     Run MFCL model\n")
  cat("  hessian   Calculate Hessian (parallel)\n")
  cat("  prof      Profile likelihood\n")
  cat("  jitter    Jitter analysis\n")
  cat("  all       Run all job types in sequence\n\n")
  cat("Options:\n")
  cat("  --local             Run locally instead of Condor\n")
  cat("  --model=<name>      Model name(s): 'all', 'base', 'base,M1' (default: all)\n")
  cat("  --dir=<path>        Remote directory name (default: auto from date+jobtype)\n")
  cat("  --nsplit=<n>        Hessian splits (default: ", HESSIAN_NSPLIT, ")\n", sep="")
  cat("  --scalers=<list>    Profile scalers (default: from config)\n")
  cat("  --njitter=<n>       Jitter runs (default: ", JITTER_NRUNS, ")\n\n", sep="")
  cat("Examples:\n")
  cat("  # Basic usage (auto folder: develop/Feb_03_2026_model)\n")
  cat("  Rscript launch.R model\n")
  cat("  Rscript launch.R hessian --nsplit=100\n\n")
  cat("  # Custom folder name\n")
  cat("  Rscript launch.R model --dir=develop/run01_base\n")
  cat("  Rscript launch.R model,hessian --dir=develop/test_steepness\n\n")
  cat("  # Multiple job types at once\n")
  cat("  Rscript launch.R model,hessian              # Auto: develop/Feb_03_2026_model+hessian\n")
  cat("  Rscript launch.R all --dir=develop/final    # Custom folder\n\n")
  cat("  # Select specific models\n")
  cat("  Rscript launch.R model --model=base --dir=develop/base_only\n")
  cat("  Rscript launch.R hessian,prof --model=base,M1 --dir=develop/sens_runs\n\n")
  cat("  # Local test (no Condor)\n")
  cat("  Rscript launch.R model --local\n\n")
  cat("NOTE: Remote directory is relative to: ", GITHUB_REPO, "/\n", sep="")
  cat("      Full path will be: ", CONDOR_USER, "@", CONDOR_HOST, ":", GITHUB_REPO, "/<your_dir>\n\n", sep="")
  quit(status = 1)
}

## Parse job types (can be comma-separated: model,hessian or 'all')
job_type_arg <- args[1]
valid_types <- c("model", "hessian", "prof", "jitter")

if(job_type_arg == "all") {
  job_types <- valid_types
} else {
  job_types <- strsplit(job_type_arg, ",")[[1]]
  job_types <- trimws(job_types)  # Remove whitespace
  invalid_types <- setdiff(job_types, valid_types)
  if(length(invalid_types) > 0) {
    stop("Invalid job type(s): ", paste(invalid_types, collapse = ", "),
         "\nValid types: ", paste(valid_types, collapse = ", "), ", all")
  }
}

## For directory naming, use first job type or 'all'
job_type <- if(length(job_types) > 1) "all" else job_types[1]

## Parse options
run_local <- if("--local" %in% args) TRUE else LAUNCH_LOCAL
remote_dir <- NULL
nsplit <- HESSIAN_NSPLIT
scalers_vec <- PROF_SCALERS
njitter <- JITTER_NRUNS
model_names <- if(!is.null(ACTIVE_MODELS)) ACTIVE_MODELS else names(MODELS)

for(arg in args[-1]) {
  if(grepl("^--dir=", arg)) {
    remote_dir <- sub("^--dir=", "", arg)
  } else if(grepl("^--model=", arg)) {
    model_str <- sub("^--model=", "", arg)
    if(model_str == "all") {
      model_names <- names(MODELS)  # All models explicitly
    } else if(model_str == "active") {
      model_names <- if(!is.null(ACTIVE_MODELS)) ACTIVE_MODELS else names(MODELS)
    } else {
      model_names <- strsplit(model_str, ",")[[1]]  # Specific models
      # Validate model names
      invalid <- setdiff(model_names, names(MODELS))
      if(length(invalid) > 0) {
        stop("Invalid model(s): ", paste(invalid, collapse = ", "), 
             "\nAvailable models: ", paste(names(MODELS), collapse = ", "))
      }
    }
  } else if(grepl("^--nsplit=", arg)) {
    nsplit <- as.integer(sub("^--nsplit=", "", arg))
  } else if(grepl("^--scalers=", arg)) {
    scalers_str <- sub("^--scalers=", "", arg)
    scalers_vec <- as.numeric(strsplit(scalers_str, ",")[[1]])
  } else if(grepl("^--njitter=", arg)) {
    njitter <- as.integer(sub("^--njitter=", "", arg))
  }
}

## Validate at least one model selected
if(length(model_names) == 0) {
  stop("No models selected. Use --model=<name> or --model=all")
}

## Set default remote directory
if(is.null(remote_dir)) {
  date_str <- format(Sys.Date(), "%b_%d_%Y", usetz = FALSE)
  if(length(job_types) > 1) {
    remote_dir <- paste0("develop/", date_str, "_", paste(job_types, collapse = "+"))
  } else {
    remote_dir <- paste0("develop/", date_str, "_", job_types[1])
  }
}

## =============================================================================
## RUN LOCALLY
## =============================================================================

if(run_local) {
  cat("==============================================\n")
  cat("Running locally:", toupper(paste(job_types, collapse = ", ")), "\n")
  cat("Model:", model_names[1], "(first model only for local test)\n")
  cat("==============================================\n\n")
  
  ## Use first model for local testing
  model_name <- model_names[1]
  Sys.setenv(MODEL_NAME = model_name)
  
  for(jt in job_types) {
    cat("\n--- Running", toupper(jt), "---\n")
    
    if(jt == "model") {
      Sys.setenv(EXEC_MODE = EXEC_MODE)
      system2("Rscript", "scripts/run_model.R")
    } else if(jt == "hessian") {
      cat("* Testing hessian part 1 of", nsplit, "...\n")
      Sys.setenv(hessian_part = "1", nsplit = as.character(nsplit))
      system2("Rscript", "scripts/run_hessian.R")
    } else if(jt == "prof") {
      cat("* Testing profile scaler", scalers_vec[1], "...\n")
      Sys.setenv(scaler = as.character(scalers_vec[1]),
                 PROF_START_YEAR = as.character(PROF_START_YEAR),
                 PROF_END_YEAR = as.character(PROF_END_YEAR))
      system2("Rscript", "scripts/run_prof.R")
    } else if(jt == "jitter") {
      cat("* Testing jitter seed 1...\n")
      Sys.setenv(jitter_seed = "1", jitter_cv = as.character(JITTER_CV))
      system2("Rscript", "scripts/run_jitter.R")
    }
  }
  
  cat("\n✓ Local test completed\n")
  quit(status = 0)
}

## =============================================================================
## CHECK PAR FILES AND DETERMINE IF MODEL RUN NEEDED
## =============================================================================

check_par_files <- function(model_name) {
  model_dir <- get_model_dir(model_name)
  par_files <- list.files(model_dir, pattern = "\\.par$", full.names = TRUE)
  return(length(par_files) > 0)
}

## Only check par files if running analysis jobs (not model)
analysis_jobs <- setdiff(job_types, "model")
models_needing_run <- c()
if(length(analysis_jobs) > 0) {
  for(model_name in model_names) {
    if(!check_par_files(model_name)) {
      models_needing_run <- c(models_needing_run, model_name)
    }
  }
  
  if(length(models_needing_run) > 0) {
    cat("==============================================\n")
    cat("WARNING: Par files not found\n")
    cat("==============================================\n")
    cat("Models missing .par files:", paste(models_needing_run, collapse = ", "), "\n\n")
    cat("These models need to be run first before:", paste(analysis_jobs, collapse = ", "), "\n\n")
    cat("To run missing models:\n")
    cat("  Rscript launch.R model --model=", paste(models_needing_run, collapse = ","), "\n\n", sep="")
    
    ## Remove models without par files
    model_names <- setdiff(model_names, models_needing_run)
    
    if(length(model_names) == 0) {
      stop("No models with .par files available. Run model first.")
    }
    
    cat("Proceeding with models:", paste(model_names, collapse = ", "), "\n\n")
  }
}

## =============================================================================
## SUBMIT TO CONDOR
## =============================================================================

cat("==============================================\n")
cat("Condor Submission:", toupper(paste(job_types, collapse = " + ")), "\n")
cat("==============================================\n")
cat("Models:", paste(model_names, collapse = ", "), "\n")
cat("Job types:", paste(job_types, collapse = ", "), "\n")
cat("Remote directory:", remote_dir, "\n")
cat("Docker image:", DOCKER_IMAGE, "\n")
cat("Branch:", GITHUB_BRANCH, "\n\n")

## Exclude slow slots
exclude_slots <- c("slot1@nouofpcand27", "slot1@nouofpcand28", 
                   "slot1@nouofpcand29", "slot1@nouofpcand30",
                   "slot1_1@suvofpcand26.corp.spc.int",
                   "slot1_2@suvofpcand26.corp.spc.int",
                   "slot1_3@suvofpcand26.corp.spc.int")

## =============================================================================
## MODEL
## =============================================================================

if("model" %in% job_types) {
  cat("Submitting model jobs for", length(model_names), "model(s)...\n\n")
  
  for(model_name in model_names) {
    
    cat("  - Submitting model:", model_name, "\n")
    
    ## Get model configuration
    model_config <- MODELS[[model_name]]
    
    ## Environment variables for this model
    model_env <- list(
      MODEL_NAME = model_name,
      base_dir = model_config$inputs_dir,
      model_dir = paste0("model/", model_name)
    )
    
    ## Save job metadata for tracking
    model_dir <- get_model_dir(model_name)
    if(!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
    job_info_file <- file.path(model_dir, "condor_job_info.txt")
    cat("==============================================\n", file = job_info_file)
    cat("CONDOR JOB INFORMATION\n", file = job_info_file, append = TRUE)
    cat("==============================================\n", file = job_info_file, append = TRUE)
    cat("Job Type: MODEL\n", file = job_info_file, append = TRUE)
    cat("Model Name:", model_name, "\n", file = job_info_file, append = TRUE)
    cat("Submitted:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = job_info_file, append = TRUE)
    cat("Branch:", GITHUB_BRANCH, "\n", file = job_info_file, append = TRUE)
    cat("Remote Dir:", paste0(GITHUB_REPO, "/", remote_dir, "/", model_name), "\n", file = job_info_file, append = TRUE)
    cat("\nEnvironment Variables:\n", file = job_info_file, append = TRUE)
    cat("  MODEL_NAME=", model_name, "\n", sep = "", file = job_info_file, append = TRUE)
    cat("  base_dir=", model_config$inputs_dir, "\n", sep = "", file = job_info_file, append = TRUE)
    cat("  model_dir=model/", model_name, "\n", sep = "", file = job_info_file, append = TRUE)
    cat("==============================================\n", file = job_info_file, append = TRUE)
    
    CondorBox::CondorBox(
      make_options = "model",
      remote_user = CONDOR_USER,
      remote_host = CONDOR_HOST,
      remote_dir = paste0(GITHUB_REPO, "/", remote_dir, "/", model_name),
      github_pat = GITHUB_PAT,
      github_username = GITHUB_USERNAME,
      github_org = GITHUB_ORG,
      github_repo = GITHUB_REPO,
      docker_image = DOCKER_IMAGE,
      condor_memory = CONDOR_MEMORY,
      condor_cpus = CONDOR_CPUS,
      condor_disk = CONDOR_DISK,
      stream_error = "TRUE",
      branch = GITHUB_BRANCH,
      rmclone_script = "no",
      ghcr_login = TRUE,
      exclude_slots = exclude_slots,
      custom_batch_name = paste0(model_name, "-", format(Sys.time(), "%H:%M:%S_%m%d")),
      condor_environment = model_env
    )
  }
  
  cat("\n✓ Submitted", length(model_names), "model job(s)\n\n")
}

## =============================================================================
## HESSIAN
## =============================================================================

if("hessian" %in% job_types) {
  cat("Submitting hessian jobs for", length(model_names), "model(s)...\n")
  cat("Parts per model:", nsplit, "\n\n")
  
  for(model_name in model_names) {
    
    cat("  Model:", model_name, "-", nsplit, "parts\n")
    
    for(part in 1:nsplit) {
      
      ## Environment variables for this part
      hessian_env <- list(
        MODEL_NAME = model_name,
        hessian_part = as.character(part),
        nsplit = as.character(nsplit)
      )
      
      ## Save job metadata for tracking
      hessian_dir <- get_hessian_dir(model_name)
      if(!dir.exists(hessian_dir)) dir.create(hessian_dir, recursive = TRUE, showWarnings = FALSE)
      job_info_file <- file.path(hessian_dir, paste0("condor_job_info_part", part, ".txt"))
      cat("==============================================\n", file = job_info_file)
      cat("CONDOR JOB INFORMATION\n", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      cat("Job Type: HESSIAN\n", file = job_info_file, append = TRUE)
      cat("Model Name:", model_name, "\n", file = job_info_file, append = TRUE)
      cat("Part:", part, "of", nsplit, "\n", file = job_info_file, append = TRUE)
      cat("Submitted:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = job_info_file, append = TRUE)
      cat("Branch:", GITHUB_BRANCH, "\n", file = job_info_file, append = TRUE)
      cat("Remote Dir:", paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_part", part), "\n", file = job_info_file, append = TRUE)
      cat("\nEnvironment Variables:\n", file = job_info_file, append = TRUE)
      cat("  MODEL_NAME=", model_name, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  hessian_part=", part, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  nsplit=", nsplit, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      
      CondorBox::CondorBox(
        make_options = paste0("hessian hessian_part=", part, " nsplit=", nsplit),
        remote_user = CONDOR_USER,
        remote_host = CONDOR_HOST,
        remote_dir = paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_part", part),
        github_pat = GITHUB_PAT,
        github_username = GITHUB_USERNAME,
        github_org = GITHUB_ORG,
        github_repo = GITHUB_REPO,
        docker_image = DOCKER_IMAGE,
        condor_memory = HESSIAN_MEMORY,
        condor_cpus = HESSIAN_CPUS,
        condor_disk = CONDOR_DISK,
        stream_error = "TRUE",
        branch = GITHUB_BRANCH,
        rmclone_script = "no",
        ghcr_login = TRUE,
        exclude_slots = exclude_slots,
        custom_batch_name = paste0(model_name, "-hess", part, "-", format(Sys.time(), "%H:%M:%S_%m%d")),
        condor_environment = hessian_env
      )
      
      if(part %% 10 == 0) {
        cat("    Submitted", part, "/", nsplit, "parts\n")
      }
    }
  }
  
  cat("\n✓ Submitted", length(model_names) * nsplit, "hessian job(s)\n\n")
  cat("After completion, run:\n")
  cat("  Rscript collate_hessian_mfcl.R\n")
  cat("  Rscript verify_hessian.R\n\n")
}

## =============================================================================
## PROFILE
## =============================================================================

if("prof" %in% job_types) {
  cat("Submitting profile jobs for", length(model_names), "model(s)...\n")
  cat("Scalers per model:", paste(scalers_vec, collapse = ", "), "\n\n")
  
  for(model_name in model_names) {
    
    cat("  Model:", model_name, "-", length(scalers_vec), "scalers\n")
    
    for(sc in scalers_vec) {
      
      ## Environment variables for this scaler
      prof_env <- list(
        MODEL_NAME = model_name,
        scaler = as.character(sc),
        PROF_START_YEAR = as.character(PROF_START_YEAR),
        PROF_END_YEAR = as.character(PROF_END_YEAR),
        PROF_REPS_OVERRIDE = paste(PROF_REPS, collapse = " "),
        PROF_PENALTIES_OVERRIDE = paste(PROF_PENALTIES, collapse = " ")
      )
      
      ## Save job metadata for tracking
      prof_dir <- get_prof_dir(model_name)
      if(!dir.exists(prof_dir)) dir.create(prof_dir, recursive = TRUE, showWarnings = FALSE)
      job_info_file <- file.path(prof_dir, paste0("condor_job_info_sc", sc, ".txt"))
      cat("==============================================\n", file = job_info_file)
      cat("CONDOR JOB INFORMATION\n", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      cat("Job Type: PROFILE\n", file = job_info_file, append = TRUE)
      cat("Model Name:", model_name, "\n", file = job_info_file, append = TRUE)
      cat("Scaler:", sc, "(biomass multiplier)", "\n", file = job_info_file, append = TRUE)
      cat("Submitted:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = job_info_file, append = TRUE)
      cat("Branch:", GITHUB_BRANCH, "\n", file = job_info_file, append = TRUE)
      cat("Remote Dir:", paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_sc", sc), "\n", file = job_info_file, append = TRUE)
      cat("\nEnvironment Variables:\n", file = job_info_file, append = TRUE)
      cat("  MODEL_NAME=", model_name, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  scaler=", sc, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  PROF_START_YEAR=", PROF_START_YEAR, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  PROF_END_YEAR=", PROF_END_YEAR, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      
      CondorBox::CondorBox(
        make_options = paste0("prof scaler=", sc),
        remote_user = CONDOR_USER,
        remote_host = CONDOR_HOST,
        remote_dir = paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_sc", sc),
        github_pat = GITHUB_PAT,
        github_username = GITHUB_USERNAME,
        github_org = GITHUB_ORG,
        github_repo = GITHUB_REPO,
        docker_image = DOCKER_IMAGE,
        condor_memory = PROF_MEMORY,
        condor_cpus = PROF_CPUS,
        condor_disk = CONDOR_DISK,
        stream_error = "TRUE",
        branch = GITHUB_BRANCH,
        rmclone_script = "no",
        ghcr_login = TRUE,
        exclude_slots = exclude_slots,
        custom_batch_name = paste0(model_name, "-sc", sc, "-", format(Sys.time(), "%H:%M:%S_%m%d")),
        condor_environment = prof_env
      )
    }
  }
  
  cat("\n✓ Submitted", length(model_names) * length(scalers_vec), "profile job(s)\n\n")
}

## =============================================================================
## JITTER
## =============================================================================

if("jitter" %in% job_types) {
  cat("Submitting jitter jobs for", length(model_names), "model(s)...\n")
  cat("Runs per model:", njitter, "\n\n")
  
  for(model_name in model_names) {
    
    cat("  Model:", model_name, "-", njitter, "runs\n")
    
    for(seed in 1:njitter) {
      
      ## Environment variables for this jitter run
      jitter_env <- list(
        MODEL_NAME = model_name,
        jitter_seed = as.character(seed),
        jitter_cv = as.character(JITTER_CV)
      )
      
      ## Save job metadata for tracking
      jitter_dir <- get_jitter_dir(model_name)
      if(!dir.exists(jitter_dir)) dir.create(jitter_dir, recursive = TRUE, showWarnings = FALSE)
      job_info_file <- file.path(jitter_dir, paste0("condor_job_info_seed", seed, ".txt"))
      cat("==============================================\n", file = job_info_file)
      cat("CONDOR JOB INFORMATION\n", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      cat("Job Type: JITTER\n", file = job_info_file, append = TRUE)
      cat("Model Name:", model_name, "\n", file = job_info_file, append = TRUE)
      cat("Seed:", seed, "of", njitter, "\n", file = job_info_file, append = TRUE)
      cat("Jitter CV:", JITTER_CV, "\n", file = job_info_file, append = TRUE)
      cat("Submitted:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = job_info_file, append = TRUE)
      cat("Branch:", GITHUB_BRANCH, "\n", file = job_info_file, append = TRUE)
      cat("Remote Dir:", paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_seed", seed), "\n", file = job_info_file, append = TRUE)
      cat("\nEnvironment Variables:\n", file = job_info_file, append = TRUE)
      cat("  MODEL_NAME=", model_name, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  jitter_seed=", seed, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("  jitter_cv=", JITTER_CV, "\n", sep = "", file = job_info_file, append = TRUE)
      cat("==============================================\n", file = job_info_file, append = TRUE)
      
      CondorBox::CondorBox(
        make_options = paste0("jitter jitter_seed=", seed, " jitter_cv=", JITTER_CV),
        remote_user = CONDOR_USER,
        remote_host = CONDOR_HOST,
        remote_dir = paste0(GITHUB_REPO, "/", remote_dir, "/", model_name, "_seed", seed),
        github_pat = GITHUB_PAT,
        github_username = GITHUB_USERNAME,
        github_org = GITHUB_ORG,
        github_repo = GITHUB_REPO,
        docker_image = DOCKER_IMAGE,
        condor_memory = JITTER_MEMORY,
        condor_cpus = JITTER_CPUS,
        condor_disk = CONDOR_DISK,
        stream_error = "TRUE",
        branch = GITHUB_BRANCH,
        rmclone_script = "no",
        ghcr_login = TRUE,
        exclude_slots = exclude_slots,
        custom_batch_name = paste0(model_name, "-jit", seed, "-", format(Sys.time(), "%H:%M:%S_%m%d")),
        condor_environment = jitter_env
      )
      
      if(seed %% 10 == 0) {
        cat("    Submitted", seed, "/", njitter, "runs\n")
      }
    }
  }
  
  cat("\n✓ Submitted", length(model_names) * njitter, "jitter job(s)\n\n")
}

## =============================================================================
## DOWNLOAD RESULTS
## =============================================================================

cat("\n==============================================\n")
cat("Job submission complete\n")
cat("==============================================\n\n")

cat("To check job status:\n")
cat("  ssh ", CONDOR_USER, "@", CONDOR_HOST, " 'condor_q'\n\n", sep="")

cat("To download results after completion:\n")
cat("  Rscript scripts/fetch_results.R ", remote_dir, "\n\n", sep="")

cat("Or fetch specific models:\n")
cat("  Rscript scripts/fetch_results.R ", remote_dir, " --model=base\n", sep="")
cat("  Rscript scripts/fetch_results.R ", remote_dir, " --model=base,M1 --local-dir=results/run01\n\n", sep="")

cat("Manual download with R:\n")
cat("  library(CondorBox)\n")
cat("  BatchFileHandler(\n")
cat("    remote_user = '", CONDOR_USER, "',\n", sep="")
cat("    remote_host = '", CONDOR_HOST, "',\n", sep="")
cat("    folder_name = '", GITHUB_REPO, "/", remote_dir, "/base',\n", sep="")
cat("    action = 'fetch',\n")
cat("    fetch_dir = 'model/base',\n")
cat("    extract_archive = TRUE\n")
cat("  )\n\n")
cat("  )\n")
