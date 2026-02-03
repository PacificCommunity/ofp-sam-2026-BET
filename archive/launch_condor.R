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
condor_memory <- "10GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "10GB"
condor_cpus <- 2                                              # CPU request for the Condor job ")(e.g., 4)
branch <- "develop_lik"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

setwd(here::here())

#dir="develop/likelihood_test6" 
dir="develop/Jan_30_lik"

## Job type: "model", "prof", or "both"
job_type <- "prof"  # Change this to "model", "prof", or "both"

source("configs/test.R") 

if(job_type == "model") {
  ## Run model only
  make <- "model"
  
  for(model_name in names(models)) {
    CondorBox::CondorBox(
      make_options = make,
      remote_user = remote_user,
      remote_host = remote_host,
      remote_dir = paste0(github_repo, "/", dir, "/", model_name), 
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
                        "slot1_3@suvofpcand26.corp.spc.int"),
      custom_batch_name = paste0(model_name, "-", format(Sys.time(), "%H:%M:%S_%D")),
      condor_environment = models[[model_name]])
  }
  
} else if(job_type == "prof") {
  ## Run profile likelihood only (uses par from base_dir/inputs)
  ## Each scaler runs as independent parallel job
  scalers_vec <- as.numeric(unlist(strsplit(scalers, "\\s+")))
  
  for(model_name in names(models)) {
    for(sc in scalers_vec) {
      # Create environment for this specific scaler
      prof_env <- models[[model_name]]
      prof_env$scaler <- as.character(sc)
      
      CondorBox::CondorBox(
        make_options = paste0("prof scaler=", sc),
        remote_user = remote_user,
        remote_host = remote_host,
        remote_dir = paste0(github_repo, "/", dir, "/", model_name, "_sc", sc), 
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
                          "slot1_3@suvofpcand26.corp.spc.int"),
        custom_batch_name = paste0(model_name, "-sc", sc, "-", format(Sys.time(), "%H:%M:%S_%D")),
        condor_environment = prof_env)
    }
  }
  
} else if(job_type == "both") {
  ## Run both model and prof (original behavior)
  make <- "run"
  
  for(model_name in names(models)) {
    CondorBox::CondorBox(
      make_options = make,
      remote_user = remote_user,
      remote_host = remote_host,
      remote_dir = paste0(github_repo, "/", dir, "/", model_name), 
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
                        "slot1_3@suvofpcand26.corp.spc.int"),
      custom_batch_name = paste0(model_name, "-", format(Sys.time(), "%H:%M:%S_%D")),
      condor_environment = models[[model_name]])
  }
}
  
# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------

output_dir=dir

setwd(here::here())

if(job_type == "prof") {
  ## Fetch results for each scaler
  scalers_vec <- as.numeric(unlist(strsplit(scalers, "\\s+")))
  
  for(model_name in names(models)) {
    for(sc in scalers_vec) {
      remote_dir <- paste0(github_repo, "/", output_dir, "/", model_name, "_sc", sc)
      
      CondorBox::BatchFileHandler(
        remote_user   = remote_user,
        remote_host   = remote_host,
        folder_name   = remote_dir,
        action        = "fetch",
        fetch_dir     = "model",
        extract_archive = TRUE,
        direct_extract = TRUE,
        archive_name    = "output_archive.tar.gz",
        extract_folder  = paste0(github_repo, "/model")
      )
    }
  }
  
} else {
  ## Fetch results for model or both
  for(model_name in names(models)) {
    remote_dir <- paste0(github_repo, "/", output_dir, "/", model_name)
    
    CondorBox::BatchFileHandler(
      remote_user   = remote_user,
      remote_host   = remote_host,
      folder_name   = remote_dir,
      action        = "fetch",
      fetch_dir     = "model",
      extract_archive = TRUE,
      direct_extract = TRUE,
      archive_name    = "output_archive.tar.gz",
      extract_folder  = paste0(github_repo, "/model")
    )
  }
}


################################
## Delete file (clone_job.sh) ##
################################

if(job_type == "prof") {
  scalers_vec <- as.numeric(unlist(strsplit(scalers, "\\s+")))
  
  for(model_name in names(models)) {
    for(sc in scalers_vec) {
      CondorBox::BatchFileHandler(
        remote_user   = remote_user,
        remote_host   = remote_host,
        folder_name   = paste0(github_repo, "/", dir, "/", model_name, "_sc", sc),
        file_name     = "clone_job.sh",
        action        = "delete"
      )
    }
  }
  
} else {
  for(model_name in names(models)) {
    CondorBox::BatchFileHandler(
      remote_user   = remote_user,
      remote_host   = remote_host,
      folder_name   = paste0(github_repo, "/", dir, "/", model_name),
      file_name     = "clone_job.sh",
      action        = "delete"
    )
  }
}

