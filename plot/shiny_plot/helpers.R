# =============================================================================
# HELPER FUNCTIONS FOR SHINY APP
# =============================================================================

library(FLR4MFCL)
library(viridis)

resolve_existing_path <- function(candidates) {
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) existing[1] else candidates[1]
}

in_shiny_plot_dir <- function() {
  # app.R resides in shiny_plot/; when launched there, getwd() basename is shiny_plot.
  identical(basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE)), "shiny_plot")
}

# Reuse fishery-map logic from plots_refactored modules, robust to app cwd.
source_plot_module <- function(module_file) {
  candidates <- if (in_shiny_plot_dir()) {
    c(file.path("..", "R", module_file), file.path("R", module_file))
  } else {
    c(file.path("R", module_file), file.path("..", "R", module_file))
  }
  path <- resolve_existing_path(candidates)
  invisible(try(source(path), silent = TRUE))
}

source_plot_module("mod_general.R")
source_plot_module("mod_model_meta.R")
source_plot_module("mod_fishery.R")
source_plot_module("mod_tag.R")
source_plot_module("mod_cpue.R")

get_fishery_map_path <- function() {
  candidates <- if (in_shiny_plot_dir()) {
    c(file.path("..", "config", "fishery_map.csv"), file.path("config", "fishery_map.csv"))
  } else {
    c(file.path("config", "fishery_map.csv"), file.path("..", "config", "fishery_map.csv"))
  }
  resolve_existing_path(candidates)
}

as_named_fishery_lookup <- function(mapping) {
  if (is.data.frame(mapping) && all(c("fishery", "fishery_name") %in% names(mapping))) {
    return(with(mapping, setNames(as.character(fishery_name), as.character(fishery))))
  }
  if (is.null(mapping)) return(character(0))
  if (!is.null(names(mapping))) return(mapping)
  character(0)
}

build_model_fishery_map <- function(par_obj, base_map, rep_obj = NULL, len_obj = NULL, wgt_obj = NULL, tagtemp_obj = NULL) {
  n_fisheries <- suppressWarnings(as.integer(par_obj@dimensions["fisheries"]))
  if (!is.finite(n_fisheries) || n_fisheries <= 0) n_fisheries <- 1

  present_ids <- seq_len(n_fisheries)

  add_from <- function(x) {
    if (is.null(x)) return(numeric(0))
    unique(suppressWarnings(as.numeric(x)))
  }

  present_ids <- unique(c(
    present_ids,
    add_from(tryCatch(cpue_obs(rep_obj)$unit, error = function(e) NULL)),
    add_from(tryCatch(len_obj@lenfits$fishery, error = function(e) NULL)),
    add_from(tryCatch(wgt_obj@wgtfits$fishery, error = function(e) NULL)),
    add_from(tryCatch(tagtemp_obj$recap.fishery, error = function(e) NULL))
  ))
  present_ids <- sort(present_ids[is.finite(present_ids) & present_ids > 0])

  # Keep full base mapping (plots_refactored behavior), then append unseen IDs.
  map <- base_map

  missing_ids <- setdiff(present_ids, map$fishery)
  if (length(missing_ids) > 0) {
    add_df <- data.frame(
      fishery = missing_ids,
      fishery_name = paste("Fishery", missing_ids),
      region = NA_real_,
      group = "Unknown",
      tag_recapture_group = missing_ids,
      tag_recapture_name = paste("Fishery", missing_ids),
      stringsAsFactors = FALSE
    )
    map <- rbind(map, add_df)
  }

  map <- map[order(map$fishery), , drop = FALSE]
  rownames(map) <- NULL
  map
}

get_fishery_name <- function(fishery_num, mapping = NULL) {
  key <- as.character(fishery_num)

  if (is.data.frame(mapping) && all(c("fishery", "fishery_name") %in% names(mapping))) {
    idx <- which(as.character(mapping$fishery) == key)
    if (length(idx) > 0) return(as.character(mapping$fishery_name[idx[1]]))
    return(paste0("Fishery-", fishery_num))
  }

  if (is.null(mapping)) return(paste0("Fishery-", fishery_num))
  name <- mapping[key]
  if (is.na(name) || is.null(name)) paste0("Fishery-", fishery_num) else unname(name)
}

detect_index_fisheries <- function(fishery_map) {
  if (is.data.frame(fishery_map) && all(c("fishery", "group", "fishery_name") %in% names(fishery_map))) {
    idx <- fishery_map$group == "Index" | grepl("index", fishery_map$fishery_name, ignore.case = TRUE)
    return(as.character(fishery_map$fishery[idx]))
  }

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

check_lf_compatibility_global <- function(rv, base_model, compare_models) {
  if (is.null(rv$LengOut_list[[base_model]])) return(character(0))

  base_fisheries <- unique(rv$LengOut_list[[base_model]]@lenfits$fishery)
  base_years <- unique(rv$LengOut_list[[base_model]]@lenfits$year)
  base_map <- rv$FISHERY_MAPS[[base_model]]
  base_fishery_names <- sapply(base_fisheries, function(f) get_fishery_name(f, base_map))

  compatible <- sapply(compare_models, function(m) {
    if (is.null(rv$LengOut_list[[m]])) return(FALSE)
    m_fisheries <- unique(rv$LengOut_list[[m]]@lenfits$fishery)
    m_years <- unique(rv$LengOut_list[[m]]@lenfits$year)
    m_map <- rv$FISHERY_MAPS[[m]]
    m_fishery_names <- sapply(m_fisheries, function(f) get_fishery_name(f, m_map))

    setequal(base_fisheries, m_fisheries) &&
      identical(sort(base_fishery_names), sort(m_fishery_names)) &&
      setequal(base_years, m_years)
  })

  names(compatible)[compatible]
}

check_wf_compatibility_global <- function(rv, base_model, compare_models) {
  if (is.null(rv$WeightOut_list[[base_model]])) return(character(0))

  base_fisheries <- unique(rv$WeightOut_list[[base_model]]@wgtfits$fishery)
  base_years <- unique(rv$WeightOut_list[[base_model]]@wgtfits$year)
  base_map <- rv$FISHERY_MAPS[[base_model]]
  base_fishery_names <- sapply(base_fisheries, function(f) get_fishery_name(f, base_map))

  compatible <- sapply(compare_models, function(m) {
    if (is.null(rv$WeightOut_list[[m]])) return(FALSE)
    m_fisheries <- unique(rv$WeightOut_list[[m]]@wgtfits$fishery)
    m_years <- unique(rv$WeightOut_list[[m]]@wgtfits$year)
    m_map <- rv$FISHERY_MAPS[[m]]
    m_fishery_names <- sapply(m_fisheries, function(f) get_fishery_name(f, m_map))

    setequal(base_fisheries, m_fisheries) &&
      identical(sort(base_fishery_names), sort(m_fishery_names)) &&
      setequal(base_years, m_years)
  })

  names(compatible)[compatible]
}
