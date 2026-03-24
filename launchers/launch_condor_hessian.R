#!/usr/bin/env Rscript
# Install the CondorBox package from GitHub (force reinstallation if needed)
#remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

# ---------------------------------------------------------------------------------
# Set variables for the remote server and CondorBox job (ignore if running locally)
# ---------------------------------------------------------------------------------

remote_user <- "kyuhank"                                      # Remote server username (e.g., "kyuhank")
remote_host <- Sys.getenv("NOU_CONDOR")                       # Remote server address 
github_pat <- Sys.getenv("GIT_PAT")                           # GitHub Personal Access Token (e.g., ghp_....)
github_username <- "kyuhank"                                  # GitHub username (e.g., "kyuhank")
github_org <- "PacificCommunity"                              # GitHub organisation name (e.g., "PacificCommunity")
github_repo <- "ofp-sam-2026-bet"                             # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
docker_image <- "ghcr.io/pacificcommunity/bet-2026:v1.7"      # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
condor_memory <- "12GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "10GB"
condor_cpus <- 2                                              # CPU request for the Condor job ")(e.g., 4)
branch <- "develop_lik"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

setwd(here::here())

dir="develop/Feb_6_hessian"

source("configs/set_model.R", chdir = TRUE) 

all_split <- lapply(models, function(x) x$nsplit)

for(model_name in names(models)) {
  
  splits <- as.numeric(unlist(strsplit(all_split[[model_name]], "\\s+")))
  
  for(part in 1:splits) {
    
    ## Create environment for this specific hessian part
    hessian_env <- models[[model_name]]
    hessian_env$hessian_part <- as.character(part)
    
    ## run condor job
    CondorBox::CondorBox(
      make_options = "hessian",
      remote_user = remote_user,
      remote_host = remote_host,
      remote_dir = paste0(github_repo, "/", dir, "/", model_name, "_part", part), 
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
      ghcr_login = T,
      exclude_slots = c("slot1@nouofpcand27",
                        "slot1@nouofpcand28", 
                        "slot1@nouofpcand29",
                        "slot1@nouofpcand30",
                        "slot1_1@suvofpcand26.corp.spc.int",
                        "slot1_2@suvofpcand26.corp.spc.int",
                        "slot1_3@suvofpcand26.corp.spc.int"),   ## these slots are super slow..
      custom_batch_name = paste0(model_name, "-hess", part, "-", format(Sys.time(), "%H:%M:%S_%D")),
      condor_environment = hessian_env
    )
  }
}
  
# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------

output_dir=dir

setwd(here::here())

for(model_name in names(models)) {
  for(part in 1:nsplit) {
    
    remote_dir <- paste0(github_repo, "/", output_dir, "/", model_name, "_part", part)
    
    CondorBox::BatchFileHandler(
      remote_user   = remote_user,
      remote_host   = remote_host,
      folder_name   = remote_dir,
      action        = "fetch",
      fetch_dir     = "model",
      extract_archive = TRUE,
      direct_extract = TRUE,
      archive_name    = "output_archive.tar.gz",  # Archive file to extract
      extract_folder  = paste0(github_repo, "/model")
    )
  }
}


################################
## Delete file (clone_job.sh) ##
################################

for(model_name in names(models)) {
  for(part in 1:nsplit) {
    
    CondorBox::BatchFileHandler(
      remote_user   = remote_user,
      remote_host   = remote_host,
      folder_name   = paste0(github_repo, "/", dir, "/", model_name, "_part", part),
      file_name     = "clone_job.sh",
      action        = "delete"
    )
  }
}


# ----------------------------------------------------------
# Stitch Hessian components using MFCL
# ----------------------------------------------------------

cat("\n==============================================\n")
cat("Stitching Hessian components...\n")
cat("==============================================\n\n")

for(model_name in names(models)) {
  
  model_dir <- models[[model_name]]$model_dir
  
  cat("Stitching Hessian for model:", model_name, "\n")
  cat("Model directory:", model_dir, "\n")
  
  ## Run MFCL stitching
  tryCatch({
    source("collate_hessian_mfcl.R")
    cat("✅ Hessian stitching completed for", model_name, "\n\n")
  }, error = function(e) {
    cat("❌ Error stitching Hessian for", model_name, ":", e$message, "\n\n")
  })
}

cat("\n==============================================\n")
cat("✅ All Hessian calculations complete!\n")
cat("==============================================\n")
