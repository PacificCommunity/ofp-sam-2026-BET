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
condor_memory <- "8GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "10GB"
condor_cpus <- 2                                              # CPU request for the Condor job ")(e.g., 4)
branch <- "main"                                              # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

setwd(here::here())

dir="10_oct_2025_sens" 
make="run plot"

source("configs/test.R") 

for(model_name in names(models)) {

## run condor job
CondorBox::CondorBox(
    make_options = make,
    remote_user = remote_user,
    remote_host = remote_host,
    remote_dir = paste0(github_repo, "/",dir,"/",model_name), 
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
    custom_batch_name = paste0(model_name,"-",format(Sys.time(), "%H:%M:%S_%D")),
    condor_environment = models[[model_name]] ) 

  }
  
# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------

output_dir="10_oct_2025_sens" 

setwd(here::here())

for(model_name in names(models))  {
  
  remote_dir <- paste0(github_repo,"/",output_dir,"/",model_name)
  
  CondorBox::BatchFileHandler(
    remote_user   = remote_user,
    remote_host   = remote_host,
    folder_name   = remote_dir,
    action        = "fetch",
    fetch_dir     =  "model",
    extract_archive = TRUE,
    direct_extract = TRUE,
    archive_name    = "output_archive.tar.gz",  # Archive file to extract
    extract_folder  = paste0(github_repo,"/model")
  )
  
}


