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

find_fishery_map_script <- function(model_path) {
  if (is.null(model_path) || !dir.exists(model_path)) return(NULL)
  cands <- list.files(
    model_path,
    pattern = "^fishery_map\\.[Rr]$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(cands) == 0) return(NULL)
  cands[1]
}

# Safely load fishery_map from an R script without executing unrelated side effects.
# Supported patterns are assignments to `fishery_map` and updates via `$<-`.
load_fishery_map_from_r <- function(map_r_path) {
  if (is.null(map_r_path) || !file.exists(map_r_path)) return(NULL)

  env <- new.env(parent = globalenv())
  env$save <- function(...) invisible(NULL)
  ok <- tryCatch({
    sys.source(map_r_path, envir = env, keep.source = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NULL)

  if (!exists("fishery_map", envir = env, inherits = FALSE)) return(NULL)
  map_df <- get("fishery_map", envir = env, inherits = FALSE)
  if (!is.data.frame(map_df)) return(NULL)

  required_cols <- c("fishery", "fishery_name", "region", "group", "tag_recapture_group", "tag_recapture_name")
  if (!all(required_cols %in% names(map_df))) return(NULL)

  out <- map_df[, required_cols, drop = FALSE]
  out$fishery <- suppressWarnings(as.numeric(out$fishery))
  out$fishery_name <- as.character(out$fishery_name)
  out$region <- suppressWarnings(as.numeric(out$region))
  out$group <- as.character(out$group)
  out$tag_recapture_group <- suppressWarnings(as.numeric(out$tag_recapture_group))
  out$tag_recapture_name <- as.character(out$tag_recapture_name)
  out <- out[is.finite(out$fishery), , drop = FALSE]
  out <- out[order(out$fishery), , drop = FALSE]
  rownames(out) <- NULL
  out
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
  if (is.null(base_map) || !is.data.frame(base_map)) return(NULL)
  map <- base_map[order(base_map$fishery), , drop = FALSE]
  rownames(map) <- NULL
  map
}

get_fishery_name <- function(fishery_num, mapping = NULL) {
  key <- as.character(fishery_num)

  if (is.data.frame(mapping) && all(c("fishery", "fishery_name") %in% names(mapping))) {
    idx <- which(as.character(mapping$fishery) == key)
    if (length(idx) > 0) return(as.character(mapping$fishery_name[idx[1]]))
    return(as.character(fishery_num))
  }

  if (is.null(mapping)) return(as.character(fishery_num))
  name <- mapping[key]
  if (is.na(name) || is.null(name)) as.character(fishery_num) else unname(name)
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

# Convert FLR-style arrays/FLQuant objects to data.frame defensively.
# Some model outputs carry malformed dimnames; this avoids as.data.frame() failures.
safe_array_to_df <- function(x, value_col = "data") {
  out <- tryCatch(as.data.frame(x), error = function(e) NULL)
  if (!is.null(out)) return(out)

  d <- dim(x)
  if (is.null(d) || length(d) == 0) {
    vals <- suppressWarnings(as.numeric(x))
    return(data.frame(data = vals))
  }

  dn <- tryCatch(dimnames(x), error = function(e) NULL)
  if (is.null(dn)) dn <- vector("list", length(d))
  if (length(dn) < length(d)) {
    dn <- c(dn, vector("list", length(d) - length(dn)))
  } else if (length(dn) > length(d)) {
    dn <- dn[seq_along(d)]
  }

  dim_cols <- names(dn)
  if (is.null(dim_cols) || length(dim_cols) != length(d) || any(!nzchar(dim_cols))) {
    dim_cols <- paste0("dim", seq_along(d))
  }

  idx <- lapply(d, seq_len)
  grid <- expand.grid(idx, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- dim_cols

  for (i in seq_along(d)) {
    labels <- dn[[i]]
    if (is.null(labels) || length(labels) != d[[i]]) {
      labels <- as.character(seq_len(d[[i]]))
    } else {
      labels <- as.character(labels)
    }
    grid[[dim_cols[[i]]]] <- labels[grid[[dim_cols[[i]]]]]
  }

  vals <- suppressWarnings(as.numeric(c(x)))
  expected_n <- prod(d)
  if (length(vals) != expected_n) {
    vals <- rep(NA_real_, expected_n)
  }
  grid[[value_col]] <- vals
  grid
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
