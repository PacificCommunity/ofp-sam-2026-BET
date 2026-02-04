
retro.tag <- function(tag.obj, max_year) {
  # Keep only releases up to and including max_year
  keep_releases <- tag.obj@releases[tag.obj@releases$year <= max_year, ]
  keep_groups <- unique(keep_releases$rel.group)
  
  # Keep only recaptures from valid groups up to and including max_year
  keep_recaps <- tag.obj@recaptures[
    tag.obj@recaptures$rel.group %in% keep_groups & 
      tag.obj@recaptures$recap.year <= max_year,
  ]
  
  # Remap group numbers sequentially
  new_ids <- setNames(seq_along(keep_groups), keep_groups)
  keep_releases$rel.group <- new_ids[as.character(keep_releases$rel.group)]
  keep_recaps$rel.group <- new_ids[as.character(keep_recaps$rel.group)]
  
  # Update object slots
  tag.obj@release_groups <- length(keep_groups)
  tag.obj@releases <- keep_releases
  tag.obj@recaptures <- keep_recaps
  tag.obj@recoveries <- tabulate(keep_recaps$rel.group, length(keep_groups))
  tag.obj@range["maxyear"] <- max_year
  
  return(tag.obj)
}


retro.frq <- function(frq.obj, max_year, retro.tag.obj = NULL) {
  # Filter frequency data up to and including max_year
  filtered_freq <- frq.obj@freq[frq.obj@freq$year <= max_year, ]
  
  # Count unique datasets (year/month/week/fishery combinations)
  n_datasets <- nrow(unique(filtered_freq[, c("year", "month", "week", "fishery")]))
  
  frq.obj@freq <- filtered_freq
  frq.obj@range["maxyear"] <- max_year
  frq.obj@lf_range["Datasets"] <- n_datasets
  
  # Update tag group count if provided
  if (!is.null(retro.tag.obj)) {
    frq.obj@n_tag_groups <- retro.tag.obj@release_groups
  }
  
  return(frq.obj)
}



retro.age <- function(age.obj, max_year) {
  # Filter age-length key data up to and including max_year
  age.obj@ALK <- age.obj@ALK[age.obj@ALK$year <= max_year, ]
  
  # Update year range
  age.obj@range["maxyear"] <- max_year
  
  return(age.obj)
}


retro.ini <- function(ini.obj, tag.obj, max_year) {
  # Execute retro.tag to filter tag data up to max_year
  new_tag <- retro.tag(tag.obj, max_year)
  new_n_taggrps <- new_tag@release_groups
  
  # Identify original group numbers to keep (before remapping in retro.tag)
  keep_releases <- tag.obj@releases[tag.obj@releases$year <= max_year, ]
  keep_groups <- unique(keep_releases$rel.group)
  
  # Update tag group dimension in ini file
  ini.obj@dimensions["taggrps"] <- new_n_taggrps
  
  # Get the index of the last aggregate row
  n_rows_original <- nrow(ini.obj@tag_fish_rep_rate)
  aggregate_row <- n_rows_original
  
  # Combine valid groups with aggregate row
  keep_rows <- c(keep_groups, aggregate_row)
  
  # Update all tag-related slots (row dimension = tag groups + 1 aggregate)
  ini.obj@tag_fish_rep_rate <- ini.obj@tag_fish_rep_rate[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_grp <- ini.obj@tag_fish_rep_grp[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_flags <- ini.obj@tag_fish_rep_flags[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_target <- ini.obj@tag_fish_rep_target[keep_rows, , drop = FALSE]
  ini.obj@tag_fish_rep_pen <- ini.obj@tag_fish_rep_pen[keep_rows, , drop = FALSE]
  
  # Return both updated ini and tag objects
  return(list(ini = ini.obj, tag = new_tag))
}







read.MFCLIni<-function (inifile, nseasons = 4) 
{
  trim.leading <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x)
  splitter <- function(ff, tt, ll = 1, inst = 1) unlist(strsplit(trim.leading(ff[grep(tt, 
                                                                                      ff)[inst] + ll]), split = "[[:blank:]]+"))
  slotcopy <- function(from, to) {
    for (slotname in slotNames(from)) {
      slot(to, slotname) <- slot(from, slotname)
    }
    return(to)
  }
  res <- new("MFCLIni")
  par <- readLines(inifile)
  par <- par[nchar(par) >= 1]
  if (any(grepl("# ", par) & nchar(par) < 3)) 
    par <- par[-seq(1, length(par))[grepl("# ", par) & nchar(par) < 
                                      3]]
  slot(res, "ini_version") <- as.numeric(splitter(par, "# ini version number"))
  if (slot(res, "ini_version") == 1002) 
    stop("Sorry FLR4MFCL is not compatable with multispecies models at the moment")
  nages <- as.numeric(splitter(par, "# number of age classes"))
  nagestest <- length(splitter(par, "# maturity at age"))
  if (nages != nagestest) 
    warning("The number of age classes and length of maturity at age don't match up")
  nregions <- length(splitter(par, "# recruitment distribution by region"))
  dims_age <- dimnames(FLQuant(quant = "age"))
  dims_age$age <- as.character(0:((nages/nseasons) - 1))
  dims_age$season <- as.character(1:nseasons)
  if (any(grep("# tag fish rep", par))) 
    res <- slotcopy(read.MFCLTagRep(parfile, par), res)
  if (slot(res, "ini_version") >= 1004) 
    tag_shed_rate(res) <- as.numeric(splitter(par, "tag shed rate"))
  slot(res, "m") <- as.numeric(splitter(par, "# natural mortality"))
  slot(res, "mat") <- FLQuant(aperm(array(as.numeric(splitter(par, 
                                                              "# maturity at age")), dim = c(nseasons, nages/nseasons, 
                                                                                             1, 1, 1)), c(2, 3, 4, 1, 5)), dimnames = dims_age)
  if (slot(res, "ini_version") > 1002) 
    slot(res, "mat_at_length") <- as.numeric(splitter(par, 
                                                      "# maturity at length"))
  slot(res, "move_map") <- as.numeric(splitter(par, "# movement map"))
  slot(res, "diff_coffs") <- as.array(matrix(as.numeric(splitter(par, 
                                                                 "# diffusion coffs", 1:(max(c(length(slot(res, "move_map"))), 
                                                                                             1)))), nrow = length(slot(res, "move_map")), byrow = T))
  if (slot(res, "ini_version") > 1001) 
    slot(res, "region_flags") <- matrix(as.numeric(splitter(par, 
                                                            "# region")), ncol = nregions, nrow = 10, byrow = TRUE)
  slot(res, "age_pars") <- as.array(matrix(as.numeric(splitter(par, 
                                                               "# age_pars", 1:10)), nrow = 10, byrow = T))
  slot(res, "rec_dist") <- as.numeric(splitter(par, "# recruitment distribution"))
  slot(res, "growth") <- t(array(as.numeric(splitter(par, "# The von Bertalanffy", 
                                                     c(3, 5, 7))), dim = c(3, 3), dimnames = list(c("est", 
                                                                                                    "min", "max"), c("Lmin", "Lmax", "k"))))
  slot(res, "lw_params") <- as.numeric(splitter(par, "# Length-weight"))
  slot(res, "sv") <- as.numeric(splitter(par, "sv"))
  slot(res, "sd_length_at_age") <- as.numeric(splitter(par, 
                                                       "# Generic SD"))
  slot(res, "sd_length_dep") <- as.numeric(splitter(par, "# Length-dependent SD"))
  slot(res, "n_mean_constraints") <- as.numeric(splitter(par, 
                                                         "# The number of mean constraints"))
  slot(res, "dimensions") <- c(agecls = nages, years = NA, 
                               seasons = dim(slot(res, "mat"))[4], regions = nregions, 
                               fisheries = dim(slot(res, "tag_fish_rep_rate"))[2], taggrps = dim(slot(res, 
                                                                                                      "tag_fish_rep_rate"))[1] - 1)
  return(res)
}
