# ============================================================
# Tag mixing sensitivity summary by release group (RG)
#
# Purpose:
# - Quantify how much tag information remains usable under different
#   tag mixing assumptions (n_mixing_periods)
# - Summarise retained releases and retained recaptures by release group
#   and release region
# - Save CSV summaries and a few quick diagnostic plots
#
# Typical use:
#   setwd("/Users/kyuhan_kim/Desktop/SPC/ofp-sam-2026-BET")
#   source("tools/input_sensitivities/diagnostics/tag_mixing_rg_usage.R")
#
# Notes:
# - "Usage" here is operational, not likelihood weight. It answers:
#   "Given a terminal year and a mixing-period assumption, how much of
#   each release group's release/recapture information is still inside
#   the usable window?"
# - Releases are summarised using tag@releases$lendist
# - Recaptures are summarised using tag@recaptures$recap.number
# - Additional TAL summaries treat n_mixing_periods as a minimum time-at-liberty
#   threshold (in quarters) for recaptures, which is often closer to the
#   biological question of how much tag information is truly usable.
# ============================================================

suppressPackageStartupMessages({
  library(FLR4MFCL)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(scales)
})

# -------------------------
# 1) Local settings
# -------------------------
base_dir <- "mfcl/inputs/2023_fixM"
out_dir <- "sensitivities/output/tag_mixing_rg_usage"

# Mixing assumptions to compare (quarters)
mixing_periods <- c(0, 1, 2, 3, 4, 6, 8)

# If NULL, use tag object's maxyear
terminal_year <- NULL

# Use fractional year based on release/recapture month.
# This is a little more informative than using the integer year only.
use_fractional_time <- TRUE

# Save plots/CSVs
write_outputs <- TRUE

# -------------------------
# 2) Helpers
# -------------------------
project_root <- getwd()
base_dir_abs <- file.path(project_root, base_dir)
out_dir_abs <- file.path(project_root, out_dir)

if (!dir.exists(base_dir_abs)) {
  stop("Base inputs directory does not exist: ", base_dir_abs)
}
if (write_outputs) dir.create(out_dir_abs, recursive = TRUE, showWarnings = FALSE)

detect_tag_file <- function(path) {
  files <- list.files(path, full.names = TRUE)
  bn <- basename(files)
  tag_file <- files[bn == "tag"]
  if (length(tag_file) == 0) tag_file <- files[endsWith(bn, ".tag")]
  if (length(tag_file) == 0) {
    stop("No tag/.tag file found in ", path)
  }
  tag_file[[1]]
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

weighted_median_safe <- function(x, w) {
  x <- safe_numeric(x)
  w <- safe_numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

build_time_index <- function(year, month, use_fractional_time = TRUE) {
  year <- safe_numeric(year)
  month <- safe_numeric(month)
  month[!is.finite(month)] <- 1
  if (!use_fractional_time) return(year)
  year + (pmax(month, 1) - 1) / 12
}

summarise_release_group_usage <- function(tag_obj,
                                          mixing_periods,
                                          terminal_year,
                                          use_fractional_time = TRUE) {
  rel <- as.data.frame(tag_obj@releases)
  rec <- as.data.frame(tag_obj@recaptures)

  rel$rel.group <- safe_numeric(rel$rel.group)
  rel$region <- safe_numeric(rel$region)
  rel$year <- safe_numeric(rel$year)
  rel$month <- safe_numeric(rel$month)
  rel$program <- as.character(rel$program)
  rel$lendist <- safe_numeric(rel$lendist)

  rec$rel.group <- safe_numeric(rec$rel.group)
  rec$region <- safe_numeric(rec$region)
  rec$year <- safe_numeric(rec$year)
  rec$month <- safe_numeric(rec$month)
  rec$program <- as.character(rec$program)
  rec$recap.year <- safe_numeric(rec$recap.year)
  rec$recap.month <- safe_numeric(rec$recap.month)
  rec$recap.fishery <- safe_numeric(rec$recap.fishery)
  rec$recap.number <- safe_numeric(rec$recap.number)

  rel <- rel %>%
    mutate(
      release_time = build_time_index(year, month, use_fractional_time = use_fractional_time),
      release_qty = replace_na(lendist, 0)
    )

  rec <- rec %>%
    mutate(
      release_time = build_time_index(year, month, use_fractional_time = use_fractional_time),
      recap_time = build_time_index(recap.year, recap.month, use_fractional_time = use_fractional_time),
      recap_qty = replace_na(recap.number, 0),
      tal_months = (recap.year - year) * 12 + (recap.month - month),
      tal_quarters = tal_months / 3
    )

  terminal_time <- if (use_fractional_time) terminal_year + 11 / 12 else terminal_year

  rg_meta <- rel %>%
    group_by(rel.group) %>%
    summarise(
      release_region = dplyr::first(region),
      release_program = dplyr::first(program),
      first_release_year = min(year, na.rm = TRUE),
      first_release_month = min(month[year == min(year, na.rm = TRUE)], na.rm = TRUE),
      last_release_year = max(year, na.rm = TRUE),
      total_release_qty = sum(release_qty, na.rm = TRUE),
      n_release_rows = dplyr::n(),
      .groups = "drop"
    )

  rg_total_recaps <- rec %>%
    group_by(rel.group) %>%
    summarise(
      total_recap_qty = sum(recap_qty, na.rm = TRUE),
      n_recap_rows = dplyr::n(),
      mean_tal_quarters = weighted.mean(tal_quarters, w = pmax(recap_qty, 0), na.rm = TRUE),
      median_tal_quarters = weighted_median_safe(tal_quarters, pmax(recap_qty, 0)),
      .groups = "drop"
    )

  rg_meta <- rg_meta %>%
    left_join(rg_total_recaps, by = "rel.group") %>%
    mutate(
      total_recap_qty = replace_na(total_recap_qty, 0),
      n_recap_rows = replace_na(n_recap_rows, 0)
    )

  by_mix <- lapply(mixing_periods, function(mp) {
    release_cutoff <- terminal_year - mp / 4
    keep_rel <- rel %>% filter(is.finite(release_time), release_time <= release_cutoff)
    keep_groups <- sort(unique(keep_rel$rel.group))
    keep_rec <- rec %>%
      filter(rel.group %in% keep_groups, is.finite(recap_time), recap_time <= terminal_time)

    rel_sum <- keep_rel %>%
      group_by(rel.group) %>%
      summarise(
        kept_release_qty = sum(release_qty, na.rm = TRUE),
        kept_release_rows = dplyr::n(),
        .groups = "drop"
      )

    rec_sum <- keep_rec %>%
      group_by(rel.group) %>%
      summarise(
        kept_recap_qty = sum(recap_qty, na.rm = TRUE),
        kept_recap_rows = dplyr::n(),
        kept_recap_fisheries = n_distinct(recap.fishery[is.finite(recap.fishery)]),
        last_recap_year = max(recap.year, na.rm = TRUE),
        .groups = "drop"
      )

    tal_sum <- rec %>%
      group_by(rel.group) %>%
      summarise(
        tal_kept_recap_qty = sum(recap_qty[tal_quarters >= mp], na.rm = TRUE),
        tal_lost_recap_qty = sum(recap_qty[tal_quarters < mp], na.rm = TRUE),
        tal_kept_rows = sum(tal_quarters >= mp, na.rm = TRUE),
        tal_lost_rows = sum(tal_quarters < mp, na.rm = TRUE),
        .groups = "drop"
      )

    rg_meta %>%
      left_join(rel_sum, by = "rel.group") %>%
      left_join(rec_sum, by = "rel.group") %>%
      left_join(tal_sum, by = "rel.group") %>%
      mutate(
        mixing_periods = mp,
        kept_release_qty = replace_na(kept_release_qty, 0),
        kept_release_rows = replace_na(kept_release_rows, 0),
        kept_recap_qty = replace_na(kept_recap_qty, 0),
        kept_recap_rows = replace_na(kept_recap_rows, 0),
        kept_recap_fisheries = replace_na(kept_recap_fisheries, 0),
        tal_kept_recap_qty = replace_na(tal_kept_recap_qty, 0),
        tal_lost_recap_qty = replace_na(tal_lost_recap_qty, 0),
        tal_kept_rows = replace_na(tal_kept_rows, 0),
        tal_lost_rows = replace_na(tal_lost_rows, 0),
        last_recap_year = ifelse(is.infinite(last_recap_year), NA_real_, last_recap_year),
        pct_release_kept = ifelse(total_release_qty > 0, 100 * kept_release_qty / total_release_qty, NA_real_),
        pct_recap_kept = ifelse(total_recap_qty > 0, 100 * kept_recap_qty / total_recap_qty, NA_real_),
        pct_recap_meeting_tal = ifelse(total_recap_qty > 0, 100 * tal_kept_recap_qty / total_recap_qty, NA_real_),
        share_of_kept_recaps = ifelse(sum(kept_recap_qty, na.rm = TRUE) > 0,
          100 * kept_recap_qty / sum(kept_recap_qty, na.rm = TRUE),
          0
        ),
        share_of_tal_kept_recaps = ifelse(sum(tal_kept_recap_qty, na.rm = TRUE) > 0,
          100 * tal_kept_recap_qty / sum(tal_kept_recap_qty, na.rm = TRUE),
          0
        )
      )
  })

  rg_summary <- bind_rows(by_mix) %>%
    arrange(mixing_periods, rel.group)

  region_summary <- rg_summary %>%
    group_by(mixing_periods, release_region) %>%
    summarise(
      total_release_qty = sum(total_release_qty, na.rm = TRUE),
      total_recap_qty = sum(total_recap_qty, na.rm = TRUE),
      kept_release_qty = sum(kept_release_qty, na.rm = TRUE),
      kept_recap_qty = sum(kept_recap_qty, na.rm = TRUE),
      tal_kept_recap_qty = sum(tal_kept_recap_qty, na.rm = TRUE),
      pct_release_kept = ifelse(sum(total_release_qty, na.rm = TRUE) > 0,
        100 * sum(kept_release_qty, na.rm = TRUE) / sum(total_release_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_kept = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_meeting_tal = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      share_of_kept_recaps = ifelse(sum(kept_recap_qty, na.rm = TRUE) > 0,
        100 * sum(kept_recap_qty, na.rm = TRUE) / sum(rg_summary$kept_recap_qty[rg_summary$mixing_periods == dplyr::first(mixing_periods)], na.rm = TRUE),
        0
      ),
      share_of_tal_kept_recaps = ifelse(sum(tal_kept_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(rg_summary$tal_kept_recap_qty[rg_summary$mixing_periods == dplyr::first(mixing_periods)], na.rm = TRUE),
        0
      ),
      n_release_groups = n_distinct(rel.group),
      .groups = "drop"
    ) %>%
    arrange(mixing_periods, release_region)

  program_summary <- rg_summary %>%
    group_by(mixing_periods, release_program) %>%
    summarise(
      total_release_qty = sum(total_release_qty, na.rm = TRUE),
      total_recap_qty = sum(total_recap_qty, na.rm = TRUE),
      kept_release_qty = sum(kept_release_qty, na.rm = TRUE),
      kept_recap_qty = sum(kept_recap_qty, na.rm = TRUE),
      tal_kept_recap_qty = sum(tal_kept_recap_qty, na.rm = TRUE),
      pct_release_kept = ifelse(sum(total_release_qty, na.rm = TRUE) > 0,
        100 * sum(kept_release_qty, na.rm = TRUE) / sum(total_release_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_kept = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_meeting_tal = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      share_of_tal_kept_recaps = ifelse(sum(tal_kept_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(rg_summary$tal_kept_recap_qty[rg_summary$mixing_periods == dplyr::first(mixing_periods)], na.rm = TRUE),
        0
      ),
      n_release_groups = n_distinct(rel.group),
      .groups = "drop"
    ) %>%
    arrange(mixing_periods, release_program)

  program_rg_summary <- rg_summary %>%
    group_by(mixing_periods, release_program, rel.group) %>%
    summarise(
      release_region = dplyr::first(release_region),
      total_recap_qty = sum(total_recap_qty, na.rm = TRUE),
      tal_kept_recap_qty = sum(tal_kept_recap_qty, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(mixing_periods, release_program) %>%
    mutate(
      total_program_tal = sum(tal_kept_recap_qty, na.rm = TRUE),
      share_within_program_tal = dplyr::if_else(
        total_program_tal > 0,
        100 * tal_kept_recap_qty / total_program_tal,
        0
      )
    ) %>%
    ungroup() %>%
    select(-total_program_tal) %>%
    arrange(release_program, mixing_periods, desc(share_within_program_tal), rel.group)

  program_region_summary <- rg_summary %>%
    group_by(mixing_periods, release_program, release_region) %>%
    summarise(
      total_recap_qty = sum(total_recap_qty, na.rm = TRUE),
      tal_kept_recap_qty = sum(tal_kept_recap_qty, na.rm = TRUE),
      pct_recap_meeting_tal = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    group_by(mixing_periods, release_program) %>%
    mutate(
      total_program_tal = sum(tal_kept_recap_qty, na.rm = TRUE),
      share_within_program_tal = dplyr::if_else(
        total_program_tal > 0,
        100 * tal_kept_recap_qty / total_program_tal,
        0
      )
    ) %>%
    ungroup() %>%
    select(-total_program_tal) %>%
    arrange(release_program, mixing_periods, release_region)

  program_top_rg_by_mixing <- program_rg_summary %>%
    group_by(mixing_periods, release_program) %>%
    slice_max(order_by = share_within_program_tal, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(release_program, mixing_periods, desc(share_within_program_tal), rel.group)

  program_region_top_rg <- rg_summary %>%
    group_by(mixing_periods, release_program, release_region) %>%
    mutate(
      total_program_region_tal = sum(tal_kept_recap_qty, na.rm = TRUE),
      share_within_program_region_tal = dplyr::if_else(
        total_program_region_tal > 0,
        100 * tal_kept_recap_qty / total_program_region_tal,
        0
      )
    ) %>%
    slice_max(order_by = tal_kept_recap_qty, n = 1, with_ties = TRUE) %>%
    ungroup() %>%
    select(-total_program_region_tal) %>%
    arrange(mixing_periods, release_program, release_region, desc(tal_kept_recap_qty), rel.group)

  overall_summary <- rg_summary %>%
    group_by(mixing_periods) %>%
    summarise(
      n_release_groups_with_data = sum(kept_recap_qty > 0 | kept_release_qty > 0, na.rm = TRUE),
      total_release_qty = sum(total_release_qty, na.rm = TRUE),
      total_recap_qty = sum(total_recap_qty, na.rm = TRUE),
      kept_release_qty = sum(kept_release_qty, na.rm = TRUE),
      kept_recap_qty = sum(kept_recap_qty, na.rm = TRUE),
      tal_kept_recap_qty = sum(tal_kept_recap_qty, na.rm = TRUE),
      pct_release_kept = ifelse(sum(total_release_qty, na.rm = TRUE) > 0,
        100 * sum(kept_release_qty, na.rm = TRUE) / sum(total_release_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_kept = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      pct_recap_meeting_tal = ifelse(sum(total_recap_qty, na.rm = TRUE) > 0,
        100 * sum(tal_kept_recap_qty, na.rm = TRUE) / sum(total_recap_qty, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    arrange(mixing_periods)

  list(
    overall_summary = overall_summary,
    rg_summary = rg_summary,
    region_summary = region_summary,
    program_summary = program_summary,
    program_rg_summary = program_rg_summary,
    program_top_rg_by_mixing = program_top_rg_by_mixing,
    program_region_summary = program_region_summary,
    program_region_top_rg = program_region_top_rg
  )
}

plot_top_rg_losses <- function(rg_summary, top_n = 12) {
  top_groups <- rg_summary %>%
    group_by(rel.group) %>%
    summarise(max_recap = max(total_recap_qty, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(max_recap), rel.group) %>%
    slice_head(n = top_n) %>%
    pull(rel.group)

  ggplot(
    rg_summary %>% filter(rel.group %in% top_groups),
    aes(x = mixing_periods, y = pct_recap_kept, group = rel.group, color = factor(rel.group))
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ release_region, scales = "free_y") +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Recapture retained (%)",
      color = "Release group",
      title = "Release-group recapture retention under tag mixing assumptions"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

plot_region_shares <- function(region_summary) {
  ggplot(region_summary, aes(x = factor(mixing_periods), y = share_of_tal_kept_recaps, fill = factor(release_region))) +
    geom_col(position = "stack", width = 0.8) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Share of TAL-qualified recaptures (%)",
      fill = "Release region",
      title = "How TAL-qualified tag information shifts across release regions"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

plot_tal_threshold_retention <- function(rg_summary, top_n = 12) {
  top_groups <- rg_summary %>%
    group_by(rel.group) %>%
    summarise(max_recap = max(total_recap_qty, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(max_recap), rel.group) %>%
    slice_head(n = top_n) %>%
    pull(rel.group)

  ggplot(
    rg_summary %>% filter(rel.group %in% top_groups),
    aes(x = mixing_periods, y = pct_recap_meeting_tal, group = rel.group, color = factor(rel.group))
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ release_region, scales = "free_y") +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Minimum time at liberty (quarters)",
      y = "Recaptures meeting TAL threshold (%)",
      color = "Release group",
      title = "Release-group TAL retention under tag mixing assumptions"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

plot_program_shares <- function(program_summary) {
  ggplot(program_summary, aes(x = factor(mixing_periods), y = share_of_tal_kept_recaps, fill = release_program)) +
    geom_col(position = "stack", width = 0.8) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Share of TAL-qualified recaptures (%)",
      fill = "Program",
      title = "How TAL-qualified tag information shifts across tagging programs"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

plot_program_region_shares <- function(program_region_summary) {
  ggplot(program_region_summary, aes(x = factor(mixing_periods), y = share_within_program_tal, fill = factor(release_region))) +
    geom_col(position = "stack", width = 0.8) +
    facet_wrap(~ release_program) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Release-region share within program TAL recaptures (%)",
      fill = "Release region",
      title = "How release-region composition changes within each tagging program"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

plot_program_region_top_rg <- function(program_region_top_rg) {
  plot_data <- program_region_top_rg %>%
    mutate(program_region = interaction(release_program, release_region, sep = "__", lex.order = TRUE))

  ggplot(
    plot_data,
    aes(x = factor(mixing_periods), y = program_region)
  ) +
    geom_tile(aes(fill = share_within_program_region_tal), color = "white", linewidth = 0.4) +
    geom_text(aes(label = paste0("RG", rel.group, "\n", round(share_within_program_region_tal), "%")), size = 3.1, fontface = "bold") +
    scale_fill_gradient(
      low = "#f3f7fb",
      high = "#0b6e4f",
      labels = label_percent(scale = 1)
    ) +
    scale_y_discrete(
      labels = function(x) {
        parts <- strsplit(x, "__", fixed = TRUE)
        vapply(parts, function(p) {
          if (length(p) >= 2) {
            paste0(p[1], " | R", p[2])
          } else {
            paste(p, collapse = "")
          }
        }, character(1))
      }
    ) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Program | Release region",
      fill = "Top-RG share",
      title = "Top release group within each program x release region",
      subtitle = "Tile color shows how dominant the top RG is; labels show RG id and its TAL-qualified recap share."
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      axis.text.y = element_text(face = "bold")
    )
}

plot_program_rg_dominance <- function(program_rg_summary, top_n = 8) {
  top_rg <- program_rg_summary %>%
    group_by(release_program, rel.group) %>%
    summarise(max_share = max(share_within_program_tal, na.rm = TRUE), .groups = "drop") %>%
    group_by(release_program) %>%
    slice_max(order_by = max_share, n = top_n, with_ties = FALSE) %>%
    ungroup()

  plot_data <- program_rg_summary %>%
    inner_join(top_rg %>% select(release_program, rel.group), by = c("release_program", "rel.group")) %>%
    mutate(
      rg_label = paste0("RG", rel.group, " (R", release_region, ")")
    )

  ggplot(
    plot_data,
    aes(x = factor(mixing_periods), y = reorder(rg_label, share_within_program_tal), fill = share_within_program_tal)
  ) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = round(share_within_program_tal)), size = 3, fontface = "bold") +
    facet_wrap(~ release_program, scales = "free_y") +
    scale_fill_gradient(
      low = "#f3f7fb",
      high = "#7f0000",
      labels = label_percent(scale = 1)
    ) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Release group",
      fill = "Share within program",
      title = "Which release groups become more dominant within each tagging program?",
      subtitle = "Cells show the share (%) of TAL-qualified recaptures contributed by each RG within a program."
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold")
    )
}

plot_program_top_rg_by_mixing <- function(program_top_rg_by_mixing) {
  ggplot(
    program_top_rg_by_mixing,
    aes(x = mixing_periods, y = share_within_program_tal, color = release_program, group = release_program)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.8) +
    geom_text(
      aes(label = paste0("RG", rel.group, " (R", release_region, ")")),
      nudge_y = 1.2,
      size = 3.2,
      show.legend = FALSE
    ) +
    facet_wrap(~ release_program, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(program_top_rg_by_mixing$mixing_periods))) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Top RG share within program (%)",
      color = "Program",
      title = "Most influential release group within each tagging program",
      subtitle = "At each mixing period, the label shows the RG contributing the largest share of TAL-qualified recaptures."
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

plot_program_rg_stacked <- function(program_rg_summary) {
  plot_data <- program_rg_summary %>%
    mutate(
      rg_label = paste0("RG", rel.group, " (R", release_region, ")")
    )

  ggplot(
    plot_data,
    aes(x = factor(mixing_periods), y = share_within_program_tal, fill = rg_label)
  ) +
    geom_col(width = 0.82, color = "white", linewidth = 0.15) +
    facet_wrap(~ release_program, ncol = 1) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.02))) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Share within program TAL-qualified recaptures (%)",
      fill = "Release group",
      title = "Release-group composition within each tagging program",
      subtitle = "Each bar sums to 100%, showing how TAL-qualified tag information is redistributed across RGs as the mixing period changes."
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

plot_program_rg_top2_others <- function(program_rg_summary) {
  plot_data <- program_rg_summary %>%
    group_by(release_program, mixing_periods) %>%
    arrange(desc(share_within_program_tal), rel.group, .by_group = TRUE) %>%
    mutate(rank_within_program = row_number()) %>%
    mutate(
      group_type = dplyr::case_when(
        rank_within_program == 1 ~ "Top 1",
        rank_within_program == 2 ~ "Top 2",
        TRUE ~ "Others"
      ),
      rg_text = dplyr::case_when(
        rank_within_program == 1 ~ paste0("RG", rel.group),
        rank_within_program == 2 ~ paste0("RG", rel.group),
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(release_program, mixing_periods, group_type) %>%
    summarise(
      share_within_program_tal = sum(share_within_program_tal, na.rm = TRUE),
      rg_text = dplyr::first(stats::na.omit(rg_text)),
      .groups = "drop"
    ) %>%
    mutate(
      group_rank = dplyr::case_when(
        group_type == "Top 1" ~ 1L,
        group_type == "Top 2" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    arrange(release_program, mixing_periods, group_rank, desc(share_within_program_tal)) %>%
    mutate(
      group_type = factor(group_type, levels = c("Top 1", "Top 2", "Others"))
    ) %>%
    select(-group_rank)

  label_data <- plot_data %>%
    group_by(release_program, mixing_periods) %>%
    arrange(group_type, .by_group = TRUE) %>%
    mutate(
      y_center = cumsum(share_within_program_tal) - share_within_program_tal / 2
    ) %>%
    ungroup() %>%
    filter(group_type != "Others", share_within_program_tal >= 6)

  ggplot(
    plot_data,
    aes(x = factor(mixing_periods), y = share_within_program_tal, fill = group_type)
  ) +
    geom_col(width = 0.82, color = "white", linewidth = 0.25) +
    geom_text(
      data = label_data,
      aes(x = factor(mixing_periods), y = y_center, label = rg_text),
      size = 3.1,
      fontface = "bold",
      color = "white",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ release_program, ncol = 1) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(
      values = c(
        "Top 1" = "#c73e1d",
        "Top 2" = "#f4a261",
        "Others" = "#d9d9d9"
      )
    ) +
    labs(
      x = "Mixing periods (quarters)",
      y = "Share within program TAL-qualified recaptures (%)",
      fill = "RG grouping",
      title = "Top 2 release groups and Others within each tagging program",
      subtitle = "Fixed colors: Top 1, Top 2, and Others. RG labels are shown inside the top segments when large enough."
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      strip.text = element_text(face = "bold")
    )
}

# -------------------------
# 3) Read tag input and run
# -------------------------
tag_file <- detect_tag_file(base_dir_abs)
tag_obj <- read.MFCLTag(tag_file)

if (is.null(terminal_year)) {
  terminal_year <- safe_numeric(tag_obj@range["maxyear"])
}
if (!is.finite(terminal_year)) {
  stop("Could not determine terminal_year from tag object range; set terminal_year manually.")
}

results <- summarise_release_group_usage(
  tag_obj = tag_obj,
  mixing_periods = mixing_periods,
  terminal_year = terminal_year,
  use_fractional_time = use_fractional_time
)

cat("Tag file:", tag_file, "\n")
cat("Terminal year:", terminal_year, "\n")
cat("Mixing periods:", paste(mixing_periods, collapse = ", "), "\n\n")

cat("Overall retained tag information summary:\n")
print(results$overall_summary)

cat("\nTop release groups by total recaptures:\n")
print(
  results$rg_summary %>%
    group_by(rel.group, release_region, release_program) %>%
    summarise(total_recap_qty = max(total_recap_qty, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_recap_qty), rel.group) %>%
    slice_head(n = 15)
)

cat("\nProgram-level TAL-qualified recap shares:\n")
print(results$program_summary)

cat("\nTop RG within each program at each mixing period:\n")
print(
  results$program_top_rg_by_mixing %>%
    select(
      mixing_periods,
      release_program,
      rel.group,
      release_region,
      tal_kept_recap_qty,
      share_within_program_tal
    )
)

cat("\nTop RGs within each program by TAL-qualified recap share:\n")
print(
  results$program_rg_summary %>%
    group_by(mixing_periods, release_program) %>%
    slice_max(order_by = share_within_program_tal, n = 5, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      mixing_periods,
      release_program,
      rel.group,
      release_region,
      tal_kept_recap_qty,
      share_within_program_tal
    )
)

cat("\nTop release group within each program x release region:\n")
print(
  results$program_region_top_rg %>%
    select(
      mixing_periods,
      release_program,
      release_region,
      rel.group,
      tal_kept_recap_qty,
      share_within_program_region_tal
    )
)

# -------------------------
# 4) Save outputs
# -------------------------
if (write_outputs) {
  readr::write_csv(results$overall_summary, file.path(out_dir_abs, "overall_summary.csv"))
  readr::write_csv(results$rg_summary, file.path(out_dir_abs, "release_group_summary.csv"))
  readr::write_csv(results$region_summary, file.path(out_dir_abs, "release_region_summary.csv"))
  readr::write_csv(results$program_summary, file.path(out_dir_abs, "program_summary.csv"))
  readr::write_csv(results$program_rg_summary, file.path(out_dir_abs, "program_release_group_summary.csv"))
  readr::write_csv(results$program_top_rg_by_mixing, file.path(out_dir_abs, "program_top_release_group_by_mixing.csv"))
  readr::write_csv(results$program_region_summary, file.path(out_dir_abs, "program_region_summary.csv"))
  readr::write_csv(results$program_region_top_rg, file.path(out_dir_abs, "program_region_top_release_group.csv"))

  p_rg <- plot_top_rg_losses(results$rg_summary)
  p_region <- plot_region_shares(results$region_summary)
  p_program <- plot_program_shares(results$program_summary)
  p_program_rg <- plot_program_rg_dominance(results$program_rg_summary)
  p_program_top_rg <- plot_program_top_rg_by_mixing(results$program_top_rg_by_mixing)
  p_program_rg_stacked <- plot_program_rg_stacked(results$program_rg_summary)
  p_program_rg_top2 <- plot_program_rg_top2_others(results$program_rg_summary)
  p_program_region <- plot_program_region_shares(results$program_region_summary)
  p_program_region_top_rg <- plot_program_region_top_rg(results$program_region_top_rg)

  ggsave(
    filename = file.path(out_dir_abs, "release_group_recap_retention.png"),
    plot = p_rg, width = 12, height = 8, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "release_region_recap_share.png"),
    plot = p_region, width = 10, height = 7, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_recap_share.png"),
    plot = p_program, width = 10, height = 7, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_release_group_dominance.png"),
    plot = p_program_rg, width = 12, height = 9, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_top_release_group_by_mixing.png"),
    plot = p_program_top_rg, width = 11, height = 7, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_release_group_stacked.png"),
    plot = p_program_rg_stacked, width = 14, height = 12, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_release_group_top2_others.png"),
    plot = p_program_rg_top2, width = 12, height = 10, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_release_region_share.png"),
    plot = p_program_region, width = 12, height = 8, dpi = 200
  )
  ggsave(
    filename = file.path(out_dir_abs, "program_region_top_release_group.png"),
    plot = p_program_region_top_rg, width = 14, height = 9, dpi = 200
  )

  saveRDS(
    list(
      settings = list(
        base_dir = base_dir,
        tag_file = tag_file,
        terminal_year = terminal_year,
        mixing_periods = mixing_periods,
        use_fractional_time = use_fractional_time
      ),
      results = results
    ),
    file = file.path(out_dir_abs, "tag_mixing_rg_usage.rds"),
    compress = "xz"
  )

  cat("\nSaved outputs to:\n", out_dir_abs, "\n", sep = "")
}

invisible(results)
