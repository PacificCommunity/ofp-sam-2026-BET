# Build the legacy default fishery map used by the current report.
# This keeps backward compatibility while allowing config override.
pm_default_fishery_map <- function() {
  fishery_map <- data.frame(
    fishery_name = c(
      "01.LL.ALL.1", "02.LL.ALL.2", "03.LL.US.2", "04.LL.ALL.3", "05.LL.OS.3",
      "06.LL.OS.7", "07.LL.ALL.7", "08.LL.ALL.8", "09.LL.ALL.4", "10.LL.AU.5",
      "11.LL.ALL.5", "12.LL.ALL.6", "13.PS.ASS.3", "14.PS.UNA.3", "15.PS.ASS.4",
      "16.PS.UNA.4", "17.MISC.PH.7", "18.HL.PHID.7", "19.PS.JP.1", "20.PL.JP.1",
      "21.PL.ALL.3", "22.PL.ALL.8", "23.MISC.ID.7", "24.PS.PHID.7", "25.PS.ASS.8",
      "26.PS.UNA.8", "27.LL.AU.9", "28.PL.ALL.7", "29.LL.ALL.9", "30.PS.ASS.7",
      "31.PS.UNA.7", "32.MISC.VN.7", "33.Index R1 (Fish 1)", "34.Index R2 (Fish 2)",
      "35.Index R3 (Fish 4)", "36.Index R4 (Fish 9)", "37.Index R5 (Fish 11)",
      "38.Index R6 (Fish 12)", "39.Index R7 (Fish 7)", "40.Index R8 (Fish 8)",
      "41.Index R9 (Fish 29)"
    ),
    stringsAsFactors = FALSE
  )
  
  fishery_map$fishery <- 1:nrow(fishery_map)
  fishery_map$region <- c(
    1, 2, 2, 3, 3, 7, 7, 8, 4, 5, 5, 6, 3, 3, 4, 4, 7, 7, 1, 1,
    3, 8, 7, 7, 8, 8, 9, 7, 9, 7, 7, 7, 1, 2, 3, 4, 5, 6, 7, 8, 9
  )
  
  fishery_map$group <- "Index"
  fishery_map$group[c(20, 21, 22, 28)] <- "PL"
  fishery_map$group[c(13, 14, 15, 16, 19, 24, 25, 26, 30, 31)] <- "PS"
  fishery_map$group[c(13, 15, 25, 30)] <- "PS ASS"
  fishery_map$group[c(14, 16, 26, 31)] <- "PS UNASS"
  fishery_map$group[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 27, 29)] <- "LL"
  fishery_map$group[c(17, 18, 23, 32)] <- "MISC"
  
  fishery_map$tag_recapture_group <- fishery_map$fishery
  fishery_map$tag_recapture_group[1:32] <- c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 13, 14, 14, 15, 15, 16, 17, 18, 19,
    15, 15, 20, 20, 21, 22, 23, 24, 24, 25
  )
  
  fishery_map$tag_recapture_name <- fishery_map$fishery_name
  fishery_map[fishery_map$tag_recapture_group == 13, "tag_recapture_name"] <- "PS.3"
  fishery_map[fishery_map$tag_recapture_group == 14, "tag_recapture_name"] <- "PS.4"
  fishery_map[fishery_map$tag_recapture_group == 15, "tag_recapture_name"] <- "MISC.7"
  fishery_map[fishery_map$tag_recapture_group == 20, "tag_recapture_name"] <- "PS.8"
  fishery_map[fishery_map$tag_recapture_group == 24, "tag_recapture_name"] <- "PS.7"
  
  pm_normalize_fishery_map(fishery_map)
}

# Normalize fishery map columns so downstream code can rely on a stable schema.
pm_normalize_fishery_map <- function(map_df) {
  if (!("fishery" %in% colnames(map_df))) {
    stop("fishery_map must include a 'fishery' column.")
  }
  
  out <- map_df
  if (!("fishery_name" %in% colnames(out))) out$fishery_name <- paste("Fishery", out$fishery)
  if (!("region" %in% colnames(out))) out$region <- NA_real_
  if (!("group" %in% colnames(out))) out$group <- "Unknown"
  if (!("tag_recapture_group" %in% colnames(out))) out$tag_recapture_group <- out$fishery
  if (!("tag_recapture_name" %in% colnames(out))) out$tag_recapture_name <- out$fishery_name
  
  out %>%
    dplyr::mutate(
      fishery = as.numeric(fishery),
      fishery_name = as.character(fishery_name),
      group = as.character(group),
      tag_recapture_group = as.numeric(tag_recapture_group),
      tag_recapture_name = as.character(tag_recapture_name)
    ) %>%
    dplyr::arrange(fishery)
}

# Load fishery map from external CSV when available; otherwise keep default map.
pm_load_or_build_fishery_map <- function(default_map,
                                         map_path = "config/fishery_map.csv",
                                         rep_list = NULL,
                                         len_list = NULL,
                                         wgt_list = NULL,
                                         tagtemp_list = NULL) {
  fishery_map <- default_map
  
  if (file.exists(map_path)) {
    fishery_map <- read.csv(map_path, stringsAsFactors = FALSE)
  }
  
  fishery_map <- pm_normalize_fishery_map(fishery_map)
  
  # When model outputs are available, auto-append any missing fishery IDs.
  if (!is.null(rep_list) && !is.null(len_list) && !is.null(wgt_list) && !is.null(tagtemp_list)) {
    fishery_map <- pm_augment_fishery_map(
      base_map = fishery_map,
      rep_list = rep_list,
      len_list = len_list,
      wgt_list = wgt_list,
      tagtemp_list = tagtemp_list
    )
  }
  
  fishery_map
}
