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
docker_image <- "ghcr.io/pacificcommunity/bet-2026:v1.2"      # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
condor_memory <- "12GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "10GB"
condor_cpus <- 2                                              # CPU request for the Condor job ")(e.g., 4)
branch <- "develop_lik"                                              # Branch of git repository to use 

# ---------------------------------------
# Hessian calculation settings
# ---------------------------------------

nsplit <- 20                        # Number of parts to split Hessian calculation into

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

setwd(here::here())

dir="develop/Feb_2_hessian"

source("configs/test.R") 

for(model_name in names(models)) {
  for(part in 1:nsplit) {
    
    ## Create environment for this specific hessian part
    hessian_env <- models[[model_name]]
    hessian_env$hessian_part <- as.character(part)
    hessian_env$nsplit <- as.character(nsplit)
    
    ## run condor job
    CondorBox::CondorBox(
      make_options = paste0("hessian hessian_part=", part, " nsplit=", nsplit),
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
                        "slot1@nouofpcand30"),
      env_list = hessian_env
    )
  }
}
