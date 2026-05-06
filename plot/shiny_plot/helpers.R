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

shiny_plot_repo_root <- function() {
  candidates <- unique(normalizePath(
    c(
      getwd(),
      file.path(getwd(), ".."),
      file.path(getwd(), "../..")
    ),
    winslash = "/",
    mustWork = FALSE
  ))
  hits <- candidates[
    file.exists(file.path(candidates, "tools", "input_change_metadata.R")) &
      dir.exists(file.path(candidates, "mfcl"))
  ]
  if (length(hits) > 0) hits[[1]] else candidates[[1]]
}

source_repo_tool <- function(tool_file) {
  candidates <- unique(c(
    file.path(shiny_plot_repo_root(), "tools", tool_file),
    file.path("tools", tool_file),
    file.path("..", "tools", tool_file),
    file.path("..", "..", "tools", tool_file)
  ))
  path <- candidates[file.exists(candidates)][1]
  if (!is.na(path) && nzchar(path)) {
    invisible(try(source(path), silent = TRUE))
  }
}

source_repo_tool("input_change_metadata.R")

sp_normalize_change_tokens <- function(x) {
  if (exists("normalize_input_change_tokens", mode = "function")) {
    x <- gsub("|", ",", paste(as.character(x), collapse = ","), fixed = TRUE)
    return(normalize_input_change_tokens(x))
  }
  if (is.null(x) || length(x) == 0) return(character(0))
  vals <- unlist(strsplit(paste(as.character(x), collapse = ","), "[|,;[:space:]]+", perl = TRUE), use.names = FALSE)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals) & !vals %in% c("None", "Changed", "base")]
  unique(vals)
}

sp_known_change_tokens <- function() {
  known <- c("fixM", "fixVB", "sel4", "cvH", "m23", "m123", "ExR9Index")
  catalog_path <- file.path(shiny_plot_repo_root(), "tools", "input_sensitivities", "sensitivity_catalog.R")
  if (file.exists(catalog_path)) {
    env <- new.env(parent = globalenv())
    ok <- tryCatch({
      sys.source(catalog_path, envir = env, keep.source = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (ok && exists("input_sensitivity_catalog", envir = env, inherits = FALSE)) {
      catalog <- tryCatch(env$input_sensitivity_catalog(), error = function(e) NULL)
      if (is.data.frame(catalog) && "token" %in% names(catalog)) {
        known <- unique(c(known, as.character(catalog$token)))
      }
    }
  }
  unique(known[nzchar(known)])
}

sp_legacy_change_tokens_from_name <- function(model_name) {
  nm <- basename(as.character(model_name[[1]]))
  if (!nzchar(nm) || !grepl("_", nm, fixed = TRUE)) return(character(0))

  known <- sp_known_change_tokens()
  tokens <- character(0)

  if (grepl("fixVB_M|fixVBM", nm, fixed = FALSE)) tokens <- c(tokens, "fixM", "fixVB")
  if (!grepl("fixVB_M|fixVBM", nm, fixed = FALSE) && grepl("fixVB", nm, fixed = TRUE)) tokens <- c(tokens, "fixVB")
  if (grepl("fixM", nm, fixed = TRUE) || grepl("_M($|_)", nm)) tokens <- c(tokens, "fixM")
  if (grepl("sel4|sel_spline4|sel[._-]?spline[._-]?4", nm, ignore.case = TRUE)) tokens <- c(tokens, "sel4")
  if (grepl("cvH|index_cv_half", nm, ignore.case = TRUE)) tokens <- c(tokens, "cvH")
  if (grepl("(^|_)m23($|_)", nm) || grepl("movement_R2_R3", nm, fixed = TRUE)) tokens <- c(tokens, "m23")
  if (grepl("(^|_)m123($|_)", nm) || grepl("movement_R1_R2_R1_R3_R2_R3", nm, fixed = TRUE)) tokens <- c(tokens, "m123")
  if (grepl("ExR9Index", nm, fixed = TRUE)) tokens <- c(tokens, "ExR9Index")

  parts <- strsplit(nm, "_", fixed = TRUE)[[1]]
  if (length(parts) > 1) {
    parts <- parts[-1]
    parts[parts == "M"] <- "fixM"
    drop <- grepl("^[0-9]+region$", parts) |
      parts %in% c("base", "rep", "movement", "sel", "spline4") |
      grepl("^R[0-9]+$", parts)
    parts <- parts[!drop]
    parts <- parts[parts %in% known]
    tokens <- c(tokens, parts)
  }

  unique(tokens[nzchar(tokens)])
}

sp_first_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  txt <- trimws(as.character(x[[1]]))
  if (is.na(txt) || !nzchar(txt)) default else txt
}

sp_read_input_change_meta_file <- function(input_dir) {
  if (is.null(input_dir) || length(input_dir) == 0 || is.na(input_dir[[1]])) return(NULL)
  input_dir <- as.character(input_dir[[1]])
  path <- file.path(input_dir, "input_change_metadata.rds")
  if (!file.exists(path)) return(NULL)
  meta <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(meta)) return(NULL)
  if (exists("update_input_change_summary_fields", mode = "function")) {
    meta <- update_input_change_summary_fields(meta)
  }
  meta
}

sp_resolve_input_dir <- function(input_dir, model_folder = NULL) {
  if (is.null(input_dir) || length(input_dir) == 0 || is.na(input_dir[[1]])) return(NA_character_)
  input_dir <- trimws(as.character(input_dir[[1]]))
  if (!nzchar(input_dir)) return(NA_character_)
  repo_root <- shiny_plot_repo_root()
  candidates <- if (grepl("^/", input_dir)) {
    input_dir
  } else {
    unique(c(
      file.path(repo_root, input_dir),
      file.path(getwd(), input_dir),
      if (!is.null(model_folder) && nzchar(model_folder)) file.path(dirname(model_folder), input_dir) else character(0),
      input_dir
    ))
  }
  meta_hits <- candidates[file.exists(file.path(candidates, "input_change_metadata.rds"))]
  if (length(meta_hits) > 0) return(meta_hits[[1]])
  dir_hits <- candidates[dir.exists(candidates)]
  if (length(dir_hits) > 0) dir_hits[[1]] else candidates[[1]]
}

sp_input_change_meta_from_info <- function(model_folder, info = NULL) {
  if (!is.list(info)) info <- list()
  candidate_dirs <- unique(c(
    model_folder,
    sp_first_text(info$base_dir),
    sp_first_text(info$display_base_dir),
    sp_first_text(info$input_recipe_output_dir)
  ))
  candidate_dirs <- candidate_dirs[nzchar(candidate_dirs)]
  for (candidate in candidate_dirs) {
    resolved <- sp_resolve_input_dir(candidate, model_folder = model_folder)
    meta <- sp_read_input_change_meta_file(resolved)
    if (is.list(meta)) {
      attr(meta, "resolved_input_dir") <- resolved
      return(meta)
    }
  }
  NULL
}

sp_model_change_metadata <- function(model_folder, model_name = basename(model_folder), info = NULL) {
  if (!is.list(info)) info <- list()

  tokens <- sp_normalize_change_tokens(info$change_tokens)
  source <- if (length(tokens) > 0) sp_first_text(info$change_token_source, "model_info") else NA_character_
  meta <- NULL

  if (length(tokens) == 0 && is.list(info$input_change_metadata)) {
    meta <- info$input_change_metadata
    tokens <- sp_normalize_change_tokens(meta$tokens)
    if (length(tokens) > 0) source <- "model_info input metadata"
  }

  if (length(tokens) == 0) {
    meta <- sp_input_change_meta_from_info(model_folder, info)
    tokens <- if (is.list(meta)) sp_normalize_change_tokens(meta$tokens) else character(0)
    if (length(tokens) > 0) source <- "input_change_metadata.rds"
  }

  if (length(tokens) == 0) {
    tokens <- sp_legacy_change_tokens_from_name(model_name)
    source <- if (length(tokens) > 0) "legacy known-token fallback" else "none"
  }

  label <- if (length(tokens) > 0) paste(tokens, collapse = " + ") else "None"
  description <- ""
  if (is.list(meta)) {
    if (exists("input_change_metadata_description", mode = "function")) {
      description <- input_change_metadata_description(meta)
    } else {
      description <- sp_first_text(meta$description)
    }
  }
  if (!nzchar(description)) description <- sp_first_text(info$input_change_description)
  if (!nzchar(description)) description <- sp_first_text(info$launcher_input_recipe_description)
  if (!nzchar(description) && length(tokens) > 0) description <- label

  list(
    tokens = tokens,
    label = label,
    description = description,
    source = source,
    metadata = meta
  )
}

sp_enrich_model_info <- function(model_folder, model_name = basename(model_folder), info = NULL) {
  if (!is.list(info)) info <- list()
  meta <- sp_model_change_metadata(model_folder, model_name, info)
  info$change_tokens <- meta$tokens
  info$change_token_label <- meta$label
  info$change_token_source <- meta$source
  info$input_change_description <- meta$description
  if (is.list(meta$metadata) && is.null(info$input_change_metadata)) {
    info$input_change_metadata <- meta$metadata
  }
  info
}

sp_model_folder_has_core_output <- function(model_folder) {
  if (!dir.exists(model_folder)) return(FALSE)
  if (file.exists(file.path(model_folder, "model_payload.rds"))) return(TRUE)
  has_par <- length(list.files(model_folder, pattern = "\\.par$", full.names = FALSE)) > 0
  has_rep <- length(list.files(model_folder, pattern = "\\.rep$", full.names = FALSE)) > 0
  has_par && has_rep
}

sp_model_choice_labels <- function(model_dir, model_names) {
  labels <- vapply(model_names, function(model_name) {
    folder <- file.path(model_dir, model_name)
    info <- tryCatch(readRDS(file.path(folder, "model_info.rds")), error = function(e) NULL)
    if (!is.list(info)) {
      payload_info <- tryCatch(readRDS(file.path(folder, "model_payload.rds"))$data$info, error = function(e) NULL)
      if (is.list(payload_info)) info <- payload_info
    }
    meta <- sp_model_change_metadata(folder, model_name, info)
    desc <- if (nzchar(meta$description) && !identical(meta$description, meta$label)) {
      paste0(" - ", meta$description)
    } else {
      ""
    }
    if (length(meta$tokens) > 0) {
      paste0(model_name, " [", meta$label, "]", desc)
    } else {
      paste0(model_name, " [base]")
    }
  }, character(1))
  stats::setNames(model_names, labels)
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

find_tag_rep_map_script <- function(model_path) {
  if (is.null(model_path) || !dir.exists(model_path)) return(NULL)
  cands <- list.files(
    model_path,
    pattern = "^tag_rep_map\\.[Rr]$",
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

# Safely load tag reporting labels from tag_rep_map.R.
# Expected object name is `tag_rep_map`.
# Accepted column names:
# - internal names: tag_recapture_group, tag_recapture_name
# - user-facing names: tag_reporting_group, tag_reporting_name
# - aliases from user files: tag_rep_group, tag_rep_name
load_tag_rep_map_from_r <- function(map_r_path) {
  if (is.null(map_r_path) || !file.exists(map_r_path)) return(NULL)

  env <- new.env(parent = globalenv())
  env$save <- function(...) invisible(NULL)
  ok <- tryCatch({
    sys.source(map_r_path, envir = env, keep.source = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(NULL)

  if (!exists("tag_rep_map", envir = env, inherits = FALSE)) return(NULL)
  map_df <- get("tag_rep_map", envir = env, inherits = FALSE)
  if (!is.data.frame(map_df)) return(NULL)

  nm <- names(map_df)
  grp_col <- if ("tag_recapture_group" %in% nm) {
    "tag_recapture_group"
  } else if ("tag_reporting_group" %in% nm) {
    "tag_reporting_group"
  } else if ("tag_rep_group" %in% nm) {
    "tag_rep_group"
  } else {
    NULL
  }
  name_col <- if ("tag_recapture_name" %in% nm) {
    "tag_recapture_name"
  } else if ("tag_reporting_name" %in% nm) {
    "tag_reporting_name"
  } else if ("tage_reporting_name" %in% nm) {
    "tage_reporting_name"
  } else if ("tag_rep_name" %in% nm) {
    "tag_rep_name"
  } else {
    NULL
  }
  if (is.null(grp_col) || is.null(name_col)) return(NULL)

  out <- map_df[, c(grp_col, name_col), drop = FALSE]
  names(out) <- c("tag_recapture_group", "tag_recapture_name")
  out$tag_recapture_group <- suppressWarnings(as.numeric(out$tag_recapture_group))
  out$tag_recapture_name <- as.character(out$tag_recapture_name)
  out <- out[is.finite(out$tag_recapture_group), , drop = FALSE]
  out <- out[!is.na(out$tag_recapture_name) & nzchar(out$tag_recapture_name), , drop = FALSE]
  out <- out[order(out$tag_recapture_group), , drop = FALSE]
  out <- out[!duplicated(out$tag_recapture_group), , drop = FALSE]
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

# Build a minimal fallback fishery map when fishery_map.R is missing/invalid.
# This keeps plots working with numeric/default labels.
build_fallback_fishery_map <- function(rep_obj = NULL, len_obj = NULL, wgt_obj = NULL, tagtemp_obj = NULL) {
  ids <- numeric(0)

  rep_sel_ids <- tryCatch({
    s <- as.data.frame(sel(rep_obj), drop = TRUE)
    suppressWarnings(as.numeric(as.character(s$unit)))
  }, error = function(e) numeric(0))
  ids <- c(ids, rep_sel_ids)

  rep_cpue_ids <- tryCatch({
    o <- as.data.frame(cpue_obs(rep_obj))
    suppressWarnings(as.numeric(as.character(o$unit)))
  }, error = function(e) numeric(0))
  ids <- c(ids, rep_cpue_ids)

  len_ids <- tryCatch(suppressWarnings(as.numeric(as.character(unique(len_obj@lenfits$fishery)))), error = function(e) numeric(0))
  ids <- c(ids, len_ids)

  wgt_ids <- tryCatch(suppressWarnings(as.numeric(as.character(unique(wgt_obj@wgtfits$fishery)))), error = function(e) numeric(0))
  ids <- c(ids, wgt_ids)

  tag_rel_ids <- tryCatch(suppressWarnings(as.numeric(tagtemp_obj$rel.fishery)), error = function(e) numeric(0))
  tag_recap_ids <- tryCatch(suppressWarnings(as.numeric(tagtemp_obj$recap.fishery)), error = function(e) numeric(0))
  ids <- c(ids, tag_rel_ids, tag_recap_ids)

  ids <- sort(unique(ids[is.finite(ids)]))
  if (length(ids) == 0) return(NULL)

  out <- data.frame(
    fishery = ids,
    fishery_name = as.character(ids),
    group = rep("Unknown", length(ids)),
    region = rep(NA_real_, length(ids)),
    tag_recapture_group = rep(NA_real_, length(ids)),
    tag_recapture_name = rep(NA_character_, length(ids)),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
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

ordered_fishery_label_levels <- function(ids, labels) {
  ids_num <- suppressWarnings(as.numeric(ids))
  labels_chr <- as.character(labels)
  df <- data.frame(
    fishery_id = ids_num,
    fishery_label = labels_chr,
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$fishery_label) & nzchar(df$fishery_label), , drop = FALSE]
  if (nrow(df) == 0) return(character(0))
  df <- unique(df)
  df$label_num <- suppressWarnings(as.numeric(df$fishery_label))
  df$label_is_num <- is.finite(df$label_num)
  df <- df[order(df$fishery_id, !df$label_is_num, df$label_num, df$fishery_label), , drop = FALSE]
  unique(df$fishery_label)
}

# For overlay fishery facets: keep a shared panel label when fishery names match
# across models, but prefix with "Model | " when names differ for the same ID.
build_overlay_fishery_panel_labels <- function(df, scenario_col = "Scenario", id_col = "fishery",
                                               label_col = "fishery_name", out_col = "fishery_panel") {
  if (!is.data.frame(df)) return(df)
  need <- c(scenario_col, id_col, label_col)
  if (!all(need %in% names(df))) return(df)

  out <- df
  out[[scenario_col]] <- as.character(out[[scenario_col]])
  out[[label_col]] <- as.character(out[[label_col]])
  out[[out_col]] <- out[[label_col]]

  if (length(unique(out[[scenario_col]])) > 1) {
    meta <- data.frame(
      fishery_id = suppressWarnings(as.numeric(as.character(out[[id_col]]))),
      scenario = out[[scenario_col]],
      label = out[[label_col]],
      stringsAsFactors = FALSE
    )
    meta <- unique(meta[is.finite(meta$fishery_id) & !is.na(meta$label) & nzchar(meta$label), , drop = FALSE])
    if (nrow(meta) > 0) {
      conflict_ids <- names(which(tapply(meta$label, meta$fishery_id, function(x) length(unique(x)) > 1)))
      if (length(conflict_ids) > 0) {
        mask <- as.character(out[[id_col]]) %in% conflict_ids
        out[[out_col]][mask] <- paste(out[[scenario_col]][mask], out[[label_col]][mask], sep = " | ")
      }
    }
  }

  out
}

# Build picker labels for fishery IDs across multiple models.
# If names differ by model, include model-prefixed labels in one combined choice label.
build_fishery_picker_choices <- function(fish_ids, model_names = NULL, fishery_maps = NULL) {
  ids_num <- suppressWarnings(as.numeric(fish_ids))
  ids_num <- sort(unique(ids_num[is.finite(ids_num)]))
  if (length(ids_num) == 0) return(setNames(character(0), character(0)))

  models <- unique(as.character(model_names))
  models <- models[!is.na(models) & nzchar(models)]
  if (length(models) == 0 || is.null(fishery_maps)) {
    labels <- as.character(ids_num)
    return(setNames(as.character(ids_num), labels))
  }

  if (length(models) == 1) {
    m <- models[1]
    map_obj <- fishery_maps[[m]]
    labels <- vapply(ids_num, function(fid) get_fishery_name(fid, map_obj), character(1))
    return(setNames(as.character(ids_num), labels))
  }

  meta <- do.call(rbind, lapply(models, function(m) {
    map_obj <- fishery_maps[[m]]
    data.frame(
      Model = m,
      fishery = ids_num,
      fishery_label = vapply(ids_num, function(fid) get_fishery_name(fid, map_obj), character(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(meta) || nrow(meta) == 0) {
    labels <- as.character(ids_num)
    return(setNames(as.character(ids_num), labels))
  }

  meta$fishery <- suppressWarnings(as.numeric(meta$fishery))
  meta <- meta[is.finite(meta$fishery), , drop = FALSE]

  meta$Model <- factor(meta$Model, levels = models)
  meta <- meta[order(meta$fishery, meta$Model, meta$fishery_label), , drop = FALSE]

  out_rows <- do.call(rbind, lapply(split(meta, meta$fishery), function(d) {
    d <- unique(d[, c("Model", "fishery", "fishery_label"), drop = FALSE])
    labels_u <- unique(as.character(d$fishery_label))
    fish_id <- suppressWarnings(as.numeric(d$fishery[1]))

    if (length(labels_u) <= 1) {
      data.frame(
        display = labels_u[1],
        value = as.character(fish_id),
        fishery = fish_id,
        ord = 0,
        stringsAsFactors = FALSE
      )
    } else {
      d$display <- paste(as.character(d$Model), as.character(d$fishery_label), sep = " | ")
      d$value <- paste(as.character(d$Model), as.character(d$fishery), sep = "::")
      data.frame(
        display = as.character(d$display),
        value = as.character(d$value),
        fishery = fish_id,
        ord = seq_len(nrow(d)),
        stringsAsFactors = FALSE
      )
    }
  }))

  if (is.null(out_rows) || nrow(out_rows) == 0) {
    labels <- as.character(ids_num)
    return(setNames(as.character(ids_num), labels))
  }
  out_rows <- out_rows[order(out_rows$fishery, out_rows$ord, out_rows$display), , drop = FALSE]
  out_rows <- unique(out_rows[, c("display", "value"), drop = FALSE])
  setNames(as.character(out_rows$value), as.character(out_rows$display))
}

parse_fishery_picker_values <- function(values) {
  vals <- as.character(values)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) {
    return(data.frame(Model = character(0), fishery = numeric(0), stringsAsFactors = FALSE))
  }

  has_model <- grepl("::", vals, fixed = TRUE)
  out <- data.frame(
    Model = rep(NA_character_, length(vals)),
    fishery = suppressWarnings(as.numeric(vals)),
    stringsAsFactors = FALSE
  )
  if (any(has_model)) {
    parts <- strsplit(vals[has_model], "::", fixed = TRUE)
    out$Model[has_model] <- vapply(parts, `[`, character(1), 1)
    out$fishery[has_model] <- suppressWarnings(as.numeric(vapply(parts, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))))
  }
  out <- out[is.finite(out$fishery), , drop = FALSE]
  unique(out)
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

  compatible <- sapply(compare_models, function(m) {
    if (is.null(rv$LengOut_list[[m]])) return(FALSE)
    m_fisheries <- unique(rv$LengOut_list[[m]]@lenfits$fishery)
    setequal(base_fisheries, m_fisheries)
  })

  names(compatible)[compatible]
}

check_wf_compatibility_global <- function(rv, base_model, compare_models) {
  if (is.null(rv$WeightOut_list[[base_model]])) return(character(0))

  base_fisheries <- unique(rv$WeightOut_list[[base_model]]@wgtfits$fishery)

  compatible <- sapply(compare_models, function(m) {
    if (is.null(rv$WeightOut_list[[m]])) return(FALSE)
    m_fisheries <- unique(rv$WeightOut_list[[m]]@wgtfits$fishery)
    setequal(base_fisheries, m_fisheries)
  })

  names(compatible)[compatible]
}
