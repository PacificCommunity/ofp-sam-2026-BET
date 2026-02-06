# =============================================================================
# HELPER FUNCTIONS FOR SHINY APP
# =============================================================================

library(FLR4MFCL)
library(viridis)

# Global fishery names
GLOBAL_FISHERY_NAMES <- c(
  "1" = "LL-ALL-1", "2" = "LL-ALL-2", "3" = "LL-US-2", "4" = "LL-ALL-3",
  "5" = "LL-OS-3", "6" = "LL-OS-7", "7" = "LL-ALL-7", "8" = "LL-ALL-8",
  "9" = "LL-ALL-4", "10" = "LL-AU-5", "11" = "LL-ALL-5", "12" = "LL-ALL-6",
  "27" = "LL-AU-9", "29" = "LL-ALL-9",
  "13" = "PS-ASS-3", "14" = "PS-UNA-3", "15" = "PS-ASS-4", "16" = "PS-UNA-4",
  "19" = "PS-JP-1", "24" = "PS-PHID-7", "25" = "PS-ASS-8", "26" = "PS-UNA-8",
  "30" = "PS-ASS-7", "31" = "PS-UNA-7",
  "20" = "PL-JP-1", "21" = "PL-ALL-3", "22" = "PL-ALL-8", "28" = "PL-ALL-7",
  "17" = "MISC-PH-7", "18" = "HL-PHID-7", "23" = "MISC-ID-7", "32" = "MISC-VN-7",
  "33" = "LL-ALL-1i", "34" = "LL-ALL-2i", "35" = "LL-ALL-3i",
  "36" = "LL-ALL-4i", "37" = "LL-ALL-5i", "38" = "LL-ALL-6i",
  "39" = "LL-ALL-7i", "40" = "LL-ALL-8i", "41" = "LL-ALL-9i"
)

create_fishery_map <- function(par_obj, custom_map = NULL) {
  n_fisheries <- par_obj@dimensions["fisheries"]
  default_map <- setNames(paste0("Fishery-", 1:n_fisheries), as.character(1:n_fisheries))
  if (!is.null(custom_map)) {
    for (i in names(custom_map)) {
      if (i %in% names(default_map)) default_map[i] <- custom_map[i]
    }
  }
  return(default_map)
}

get_fishery_name <- function(fishery_num, mapping = NULL) {
  if (is.null(mapping)) return(paste0("Fishery-", fishery_num))
  key <- as.character(fishery_num)
  name <- mapping[key]
  if (is.na(name) || is.null(name)) paste0("Fishery-", fishery_num) else unname(name)
}

detect_index_fisheries <- function(fishery_map) {
  is_index <- grepl("i$|index", fishery_map, ignore.case = TRUE)
  names(fishery_map)[is_index]
}

safe_read <- function(path, reader = readLines) {
  if (file.exists(path)) reader(path) else NULL
}

parse_indepvar <- function(lines) {
  if (is.null(lines) || length(lines) < 2) return(NULL)
  data_list <- lapply(2:length(lines), function(i) {
    line <- lines[i]
    if (nchar(trimws(line)) == 0) return(NULL)
    hit_bound <- grepl("\\*{5,}", line)
    line_clean <- gsub("\\*+", "", line)
    parts <- strsplit(trimws(line_clean), "\\s+")[[1]]
    if (length(parts) >= 6) {
      data.frame(
        Index = as.integer(parts[1]),
        Var_name = parts[2],
        Estimate = as.numeric(parts[3]),
        L_bound = as.numeric(parts[4]),
        U_bound = as.numeric(parts[5]),
        Gradient = as.numeric(parts[6]),
        Hit_Bound = hit_bound,
        stringsAsFactors = FALSE
      )
    } else NULL
  })
  do.call(rbind, Filter(Negate(is.null), data_list))
}

get_scenario_colors <- function(scenarios, option = "D") {
  setNames(viridis(length(scenarios), option = option, direction = 1), scenarios)
}
