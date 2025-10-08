# Install the CondorBox package from GitHub (force reinstallation if needed)
#remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

# ---------------------------------------------------------------------------------
# Getting inputs from 
# ---------------------------------------------------------------------------------

setwd(here::here())

CondorBox::BatchFileHandler(
  remote_user   = "kyuhank",
  remote_host   = Sys.getenv("PENGUIN"),
  folder_name   = ".",
  action        = "fetch",
  fetch_dir     =  "input",
  file_name     = "../shares/assessments/bet/2026/inputs/cpue.RData"
)
  

