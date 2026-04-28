library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)

suppressMessages(sf::sf_use_s2(FALSE))

script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]),
  winslash = "/",
  mustWork = TRUE
)
REPO_ROOT <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
OUTPUT_DIR <- file.path(REPO_ROOT, "plot", "shiny_plot", "www", "quick_reference")
REGION_SHAPE_FILE <- file.path(REPO_ROOT, "regions.bet.RData")
SOURCE_REGION_IDS <- c("1", "2", "3", "4", "5")
PLOT_XLIM <- c(105, 214)
PLOT_YLIM <- c(-45, 55)
SOURCE_REGION_LABEL_MAP <- c(
  `1` = "1",
  `2` = "2",
  `3` = "3",
  `4` = "5",
  `5` = "4"
)
REGION4_MAP <- c(
  `1` = "1",
  `2` = "2",
  `3` = "3",
  `4` = "4",
  `5` = "3"
)

extract_region_ids <- function(x) {
  if (inherits(x, "SpatialPolygons")) {
    return(vapply(x@polygons, function(p) p@ID, character(1)))
  }

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    return(rownames(x@data))
  }

  if (!is.null(rownames(x))) {
    return(rownames(x))
  }

  rep(NA_character_, nrow(x))
}

make_label_df <- function(polys, id_col) {
  pts <- suppressWarnings(st_point_on_surface(polys))
  coords <- st_coordinates(pts)
  cbind(st_drop_geometry(polys), coords)[, c(id_col, "X", "Y")]
}

format_lon <- function(x) {
  ifelse(
    x > 180,
    sprintf("%s°W", 360 - x),
    ifelse(x < 0, sprintf("%s°W", abs(x)), sprintf("%s°E", x))
  )
}

format_lat <- function(y) {
  north_south <- ifelse(y < 0, "S", "N")
  sprintf("%s°%s", abs(y), north_south)
}

coords_from_sf <- function(polys, id_col) {
  pieces <- lapply(seq_len(nrow(polys)), function(i) {
    coords <- as.data.frame(st_coordinates(polys[i, ]))
    point_id <- seq_len(nrow(coords))
    data.frame(
      region = as.character(polys[[id_col]][i]),
      point_id = point_id,
      lon = coords$X,
      lat = coords$Y,
      ring = coords$L1,
      part = coords$L2,
      coord_label = sprintf("%s\n%s", format_lon(coords$X), format_lat(coords$Y))
    )
  })

  do.call(rbind, pieces)
}

drop_collinear_points <- function(point_df) {
  split_groups <- split(
    point_df,
    interaction(point_df$region, point_df$ring, point_df$part, drop = TRUE)
  )

  simplified <- lapply(split_groups, function(df) {
    df <- df[order(df$point_id), , drop = FALSE]
    closed <- nrow(df) >= 2L &&
      identical(df$lon[1], df$lon[nrow(df)]) &&
      identical(df$lat[1], df$lat[nrow(df)])

    core <- if (closed) df[-nrow(df), , drop = FALSE] else df
    keep <- rep(TRUE, nrow(core))

    if (nrow(core) >= 3L) {
      for (i in seq_len(nrow(core))) {
        prev_i <- if (i == 1L) nrow(core) else i - 1L
        next_i <- if (i == nrow(core)) 1L else i + 1L

        same_lon <- core$lon[prev_i] == core$lon[i] && core$lon[i] == core$lon[next_i]
        same_lat <- core$lat[prev_i] == core$lat[i] && core$lat[i] == core$lat[next_i]
        if (same_lon || same_lat) {
          keep[i] <- FALSE
        }
      }
    }

    core <- core[keep, , drop = FALSE]
    if (closed && nrow(core) > 0L) {
      core <- rbind(core, core[1, , drop = FALSE])
    }

    core$point_id <- seq_len(nrow(core))
    core
  })

  do.call(rbind, simplified)
}

prepare_point_labels <- function(point_df, xlim = PLOT_XLIM, ylim = PLOT_YLIM) {
  x_mid <- mean(xlim)
  y_mid <- mean(ylim)

  point_df %>%
    mutate(
      x_nudge = ifelse(lon >= x_mid, -2.2, 2.2),
      y_nudge = ifelse(lat >= y_mid, -1.4, 1.4),
      label_x = pmin(pmax(lon + x_nudge, xlim[1] + 3.2), xlim[2] - 3.2),
      label_y = pmin(pmax(lat + y_nudge, ylim[1] + 2.0), ylim[2] - 2.0),
      label_hjust = ifelse(x_nudge > 0, 0, 1),
      label_vjust = ifelse(y_nudge > 0, 0, 1)
    )
}

plot_region_map <- function(polys, label_df, point_df, id_col, title_text) {
  world <- ggplot2::map_data("world2")
  point_labels <- prepare_point_labels(point_df)

  ggplot() +
    geom_polygon(
      data = world,
      aes(x = long, y = lat, group = group),
      fill = "#efe7d0",
      colour = "grey55",
      linewidth = 0.2
    ) +
    geom_sf(
      data = polys,
      fill = NA,
      colour = "#24323d",
      linewidth = 0.95
    ) +
    geom_text(
      data = label_df,
      aes(x = X, y = Y, label = .data[[id_col]]),
      size = 6,
      fontface = "bold",
      colour = "#102027"
    ) +
    geom_point(
      data = point_df,
      aes(x = lon, y = lat),
      size = 2.1,
      colour = "#b91c1c"
    ) +
    geom_label(
      data = point_labels,
      aes(
        x = label_x,
        y = label_y,
        label = coord_label,
        hjust = label_hjust,
        vjust = label_vjust
      ),
      size = 2.6,
      linewidth = 0.15,
      label.padding = grid::unit(0.1, "lines"),
      label.r = grid::unit(0.08, "lines"),
      fill = grDevices::adjustcolor("white", alpha.f = 0.92),
      colour = "#7f1d1d",
      lineheight = 0.92
    ) +
    coord_sf(
      default_crs = st_crs(4326),
      xlim = PLOT_XLIM,
      ylim = PLOT_YLIM,
      expand = FALSE
    ) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(colour = "grey84", linewidth = 0.3),
      axis.text = element_text(colour = "grey20"),
      plot.title = element_text(face = "bold"),
      plot.margin = margin(10, 18, 10, 10)
    )
}

# ------------------------------------------------------------
# 1. Load region shapes
# ------------------------------------------------------------
loaded_names <- load(REGION_SHAPE_FILE)
cat("Loaded object name(s):\n")
print(loaded_names)

regions <- get(loaded_names[1])
cat("\nSelected object:\n")
print(loaded_names[1])

cat("\nClass of selected object:\n")
print(class(regions))

if (!inherits(regions, "sf")) {
  region_ids <- extract_region_ids(regions)
  regions <- st_as_sf(regions)
  if (!("ID" %in% names(regions))) {
    regions$ID <- region_ids
  }
}

if (!("ID" %in% names(regions))) {
  stop("Could not determine an ID column for the loaded regions object")
}

regions$ID <- sub("^r", "", tolower(as.character(regions$ID)))

cat("\nColumn names in the object:\n")
print(names(regions))

# ------------------------------------------------------------
# 2. Keep source regions used for 4-region merge
# ------------------------------------------------------------
regions_source <- regions %>%
  filter(ID %in% SOURCE_REGION_IDS) %>%
  mutate(
    raw_region = ID,
    source_region = unname(SOURCE_REGION_LABEL_MAP[ID]),
    source_region = factor(source_region, levels = c("1", "2", "3", "4", "5"))
  ) %>%
  arrange(source_region)

if (nrow(regions_source) != length(SOURCE_REGION_IDS)) {
  stop(
    sprintf(
      "Expected source regions %s, found %d features",
      paste(SOURCE_REGION_IDS, collapse = ", "),
      nrow(regions_source)
    )
  )
}

cat("\nSource regions used for merge:\n")
print(regions_source[, c("raw_region", "source_region")])

# ------------------------------------------------------------
# 3. Build 4-region polygons from the input shapes
# ------------------------------------------------------------
regions4 <- regions_source %>%
  mutate(region4 = unname(REGION4_MAP[as.character(source_region)])) %>%
  group_by(region4) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  mutate(region4 = factor(region4, levels = c("1", "2", "3", "4"))) %>%
  arrange(region4)

cat("\nRaw shape -> 5-region naming -> 4-region mapping used:\n")
print(
  data.frame(
    raw_region = SOURCE_REGION_IDS,
    source_region = unname(SOURCE_REGION_LABEL_MAP[SOURCE_REGION_IDS]),
    region4 = unname(REGION4_MAP[unname(SOURCE_REGION_LABEL_MAP[SOURCE_REGION_IDS])])
  )
)

cat("\nMerged 4-region polygons:\n")
print(regions4)

# ------------------------------------------------------------
# 4. Print merged polygon coordinates
# ------------------------------------------------------------
cat("\n================ 4-region polygon coordinate information ================\n")

for (i in seq_len(nrow(regions4))) {
  cat("\nRegion 4:", as.character(regions4$region4[i]), "\n")
  coords_i <- drop_collinear_points(coords_from_sf(regions4[i, ], "region4"))
  print(coords_i)
}

cat("\n================ Source 5-region polygon coordinate information ================\n")

for (i in seq_len(nrow(regions_source))) {
  cat("\nSource Region:", as.character(regions_source$source_region[i]), "\n")
  coords_i <- coords_from_sf(regions_source[i, ], "source_region")
  print(coords_i)
}

# ------------------------------------------------------------
# 5. Plot both the source shapes and merged 4-region shapes
# ------------------------------------------------------------
source_labels <- make_label_df(regions_source, "source_region")
region4_labels <- make_label_df(regions4, "region4")
source_coords <- do.call(
  rbind,
  lapply(seq_len(nrow(regions_source)), function(i) {
    out <- coords_from_sf(regions_source[i, ], "source_region")
    out$raw_region <- as.character(regions_source$raw_region[i])
    out
  })
)
region4_coords <- coords_from_sf(regions4, "region4")
region4_coords <- drop_collinear_points(region4_coords)

p_region4 <- plot_region_map(
  polys = regions4,
  label_df = region4_labels,
  point_df = region4_coords,
  id_col = "region4",
  title_text = "4-Region Shapes With Vertex Coordinates"
)

p_source <- plot_region_map(
  polys = regions_source,
  label_df = source_labels,
  point_df = source_coords,
  id_col = "source_region",
  title_text = "Alternative 5-Region Shapes With Vertex Coordinates"
)

final_plot <- p_region4 + p_source + plot_layout(widths = c(1, 1))

print(final_plot)

# ------------------------------------------------------------
# 6. Save outputs
# ------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

save(regions4, file = file.path(OUTPUT_DIR, "regions.bet.4region.RData"))
write.csv(source_coords, file.path(OUTPUT_DIR, "regions.bet.5region.coordinates.csv"), row.names = FALSE)
write.csv(region4_coords, file.path(OUTPUT_DIR, "regions.bet.4region.coordinates.csv"), row.names = FALSE)

ggsave(
  filename = file.path(OUTPUT_DIR, "region_map_with_4region_shape.png"),
  plot = final_plot,
  width = 15.5,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(OUTPUT_DIR, "region_map_4region_only.png"),
  plot = p_region4,
  width = 8,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(OUTPUT_DIR, "region_map_alternative_5region_only.png"),
  plot = p_source,
  width = 8,
  height = 7,
  dpi = 300
)
