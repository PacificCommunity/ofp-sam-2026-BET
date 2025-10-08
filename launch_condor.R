# Install the CondorBox package from GitHub (force reinstallation if needed)
#remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

## Clean up the Docker resources interactively if needed
# CondorBox::clean_docker_resources_interactive()

# ---------------------------------------------------------------------------------
# Set variables for the remote server and CondorBox job (ignore if running locally)
# ---------------------------------------------------------------------------------

remote_user <- "kyuhank"                                      # Remote server username (e.g., "kyuhank")
remote_host <- "nouofpsubmit.corp.spc.int"                     # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
github_pat <- Sys.getenv("GIT_PAT")                           # GitHub Personal Access Token (e.g., ghp_....)
github_username <- "kyuhank"                                  # GitHub username (e.g., "kyuhank")
github_org <- "PacificCommunity"                              # GitHub organisation name (e.g., "PacificCommunity")
github_repo <- "ofp-sam-2026-bet"                       # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
docker_image <- "ghcr.io/pacificcommunity/bet-2026:v1.1"     # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
condor_memory <- "8GB"                                        # Memory request for the Condor job (e.g., "6GB")
condor_disk <- "10GB"
condor_cpus <- 2                                               # CPU request for the Condor job (e.g., 4)
branch <- "main"                                           # Branch of git repository to use 

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

dir="08Oct_2023_MFCL_doitall" 
make="run"
mfcl_commands="./doitall.sh"
mfcl_dir="mfcl/base"
input_dir="inputs/M1"
mfcl_commands="../../exe/mfclo64_2023 bet.frq 11.par 12.par"
program_path="../../exe/mfclo64_2023"

CondorBox::CondorBox(
    make_options = make,
    remote_user = remote_user,
    remote_host = remote_host,
    remote_dir = paste0(github_repo, "/",dir), 
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
    exclude_slots=c("slot1@nouofpcand27",
                    "slot1@nouofpcand28", 
                    "slot1@nouofpcand29",
                    "slot1@nouofpcand30",
                    "slot1_1@suvofpcand26.corp.spc.int",
                    "slot1_2@suvofpcand26.corp.spc.int",
                    "slot1_3@suvofpcand26.corp.spc.int"),   ## these slots are super slow..
    custom_batch_name = "trial3",
    condor_environment = list(mfcl_commands=mfcl_commands,
                              mfcl_dir=mfcl_dir) ) 


# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------


setwd(here::here())

for(i in 1:length(run_options)) {
  
  #remote_dir <- paste0("Surplus-SBC/M_", i)
  
  remote_dir <- paste0(github_repo,"/",TestTopic,"/",names(run_options[i]),"_nsims_",nsims,"_adapt_",adaptDelta)
  
  CondorBox::BatchFileHandler(
    remote_user   = remote_user,
    remote_host   = remote_host,
    folder_name   = remote_dir,
    action        = "fetch",
    fetch_dir     =  "results",
    extract_archive = TRUE,
    direct_extract = TRUE,
    archive_name    = "output_archive.tar.gz",  # Archive file to extract
    extract_folder  = paste0("Surplus-SBC/results")
  )
  
}


