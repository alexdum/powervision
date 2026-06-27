# global.R
# ==============================================================================
# Copernicus PECD v4.2 Visualization App — Initial Geographic Visualizer
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================
# This script initializes libraries, sets paths, and prepares global variables
# for the Shiny web application to display spatial aggregation boundaries.
# ==============================================================================

library(shiny)
library(mapgl)
library(sf)
library(dplyr)
library(bslib)
library(bsicons)
library(markdown)
library(arrow)
library(plotly)

# OpenFreeMap Style Endpoints (for vector tiles)
ofm_positron_style <- "https://tiles.openfreemap.org/styles/positron"
ofm_bright_style <- "https://tiles.openfreemap.org/styles/bright"

# Satellite imagery: EOX Sentinel-2 Cloudless (WGS84 compatible)
sentinel_url <- "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless-2023_3857/default/GoogleMapsCompatible/{z}/{y}/{x}.jpg"
sentinel_attribution <- '<a href="https://s2maps.eu" target="_blank">Sentinel-2 cloudless - by EOX IT Services GmbH</a>'

# Local GeoJSON repository folder (contains our pre-processed boundary files)
geojson_dir <- "www/data/geo"

# ==============================================================================
# PECD Boundary Version Configuration
# ==============================================================================
# The app uses GeoJSON files on the map, but those GeoJSONs were generated from
# two PECD shapefile vintages:
#
#   PECD v4.2 shapefile outputs:
#     pecd_P2ON.geojson, pecd_P2OF.geojson, pecd_SZON.geojson, pecd_SZOF.geojson
#
#   PECD v4.0 shapefile outputs:
#     pecd_P2ON40.geojson, pecd_P2OF40.geojson, pecd_SZON40.geojson,
#     pecd_SZOF40.geojson
#
# The April 2026 CSV files in new_csv match the boundary vintages differently by
# spatial tier:
#
#   P2ON / P2OF -> PECD v4.2 boundaries are the exact match
#   SZON / SZOF -> PECD v4.0 boundaries are the exact match
#
# For normal app use, keep the default "mixed" mode. For testing, set the
# PECD_GEOJSON_VERSION environment variable before starting Shiny:
#
#   PECD_GEOJSON_VERSION=mixed  -> P2ON/P2OF v4.2, SZON/SZOF v4.0
#   PECD_GEOJSON_VERSION=42     -> all PECD map layers from v4.2 shapefile output
#   PECD_GEOJSON_VERSION=40     -> all PECD map layers from v4.0 shapefile output
#
# NUTS 0 and NUTS 2 are Eurostat boundaries, so they are not affected by this
# PECD shapefile-version setting.
raw_pecd_geojson_version <- tolower(Sys.getenv(
  "PECD_GEOJSON_VERSION",
  unset = "mixed"
))

if (
  raw_pecd_geojson_version %in%
    c("42", "4.2", "v42", "v4.2", "pecd42", "pecd4.2")
) {
  pecd_geojson_mode <- "42"
} else if (
  raw_pecd_geojson_version %in%
    c("40", "4.0", "v40", "v4.0", "pecd40", "pecd4.0")
) {
  pecd_geojson_mode <- "40"
} else if (raw_pecd_geojson_version %in% c("mixed", "auto", "best")) {
  pecd_geojson_mode <- "mixed"
} else {
  warning(sprintf(
    "Unsupported PECD_GEOJSON_VERSION '%s'. Falling back to mixed PECD boundary mode.",
    raw_pecd_geojson_version
  ))
  pecd_geojson_mode <- "mixed"
}

# Start with the best-matching setup for the new CSV files.
pecd_boundary_source <- c(
  "P2ON" = "42",
  "P2OF" = "42",
  "SZON" = "40",
  "SZOF" = "40"
)

# Override the per-tier setup when a single shapefile vintage is requested.
if (pecd_geojson_mode == "42") {
  pecd_boundary_source[] <- "42"
} else if (pecd_geojson_mode == "40") {
  pecd_boundary_source[] <- "40"
}

pecd_boundary_suffix <- c(
  "42" = "",
  "40" = "40"
)

pecd_boundary_label <- c(
  "42" = "PECD v4.2",
  "40" = "PECD v4.0"
)

pecd_boundary_files <- c(
  "P2ON" = paste0(
    "pecd_P2ON",
    pecd_boundary_suffix[pecd_boundary_source["P2ON"]],
    ".geojson"
  ),
  "P2OF" = paste0(
    "pecd_P2OF",
    pecd_boundary_suffix[pecd_boundary_source["P2OF"]],
    ".geojson"
  ),
  "SZON" = paste0(
    "pecd_SZON",
    pecd_boundary_suffix[pecd_boundary_source["SZON"]],
    ".geojson"
  ),
  "SZOF" = paste0(
    "pecd_SZOF",
    pecd_boundary_suffix[pecd_boundary_source["SZOF"]],
    ".geojson"
  )
)

message(sprintf("Using PECD GeoJSON boundary mode: %s", pecd_geojson_mode))
message(sprintf(
  "  P2ON/P2OF source: %s / %s",
  pecd_boundary_label[pecd_boundary_source["P2ON"]],
  pecd_boundary_label[pecd_boundary_source["P2OF"]]
))
message(sprintf(
  "  SZON/SZOF source: %s / %s",
  pecd_boundary_label[pecd_boundary_source["SZON"]],
  pecd_boundary_label[pecd_boundary_source["SZOF"]]
))

# Spatial files metadata mapping
# Zone names follow the official PECD Product User Guide terminology:
#   - P2ON/P2OF = Pan-European Onshore/Offshore Nodes (finer, sub-divided zones)
#   - SZON/SZOF = Onshore/Offshore Study Zones (coarser, unified bidding zones)
spatial_levels <- list(
  "NUT0" = list(
    file = "pecd_NUT0.geojson",
    name = "NUTS 0 (National Boundaries)",
    description = "Official Eurostat 2021 country-level boundaries (~37 countries)."
  ),
  "NUT2" = list(
    file = "pecd_NUT2.geojson",
    name = "NUTS 2 (Provincial Boundaries)",
    description = "Official Eurostat 2021 province-level divisions (~334 regions)."
  ),
  "P2ON" = list(
    file = pecd_boundary_files["P2ON"],
    name = "P2ON (Pan-European Onshore Nodes)",
    description = paste0(
      "Pan-European onshore sub-zones — fine-grained resolution. ",
      "Boundary source: ",
      pecd_boundary_label[pecd_boundary_source["P2ON"]],
      "."
    )
  ),
  "P2OF" = list(
    file = pecd_boundary_files["P2OF"],
    name = "P2OF (Pan-European Offshore Nodes)",
    description = paste0(
      "Pan-European offshore sub-zones — fine-grained resolution. ",
      "Boundary source: ",
      pecd_boundary_label[pecd_boundary_source["P2OF"]],
      "."
    )
  ),
  "SZON" = list(
    file = pecd_boundary_files["SZON"],
    name = "SZON (Onshore Study Zones)",
    description = paste0(
      "ENTSO-E onshore study zones — dissolved bidding zones. ",
      "Boundary source: ",
      pecd_boundary_label[pecd_boundary_source["SZON"]],
      "."
    )
  ),
  "SZOF" = list(
    file = pecd_boundary_files["SZOF"],
    name = "SZOF (Offshore Study Zones)",
    description = paste0(
      "ENTSO-E offshore study zones — dissolved bidding zones. ",
      "Boundary source: ",
      pecd_boundary_label[pecd_boundary_source["SZOF"]],
      "."
    )
  )
)

# Coordinates for centering the map on Europe (lat/lon and WGS84 zoom)
europe_center_lon <- 15.0
europe_center_lat <- 50.0
europe_default_zoom <- 3.5

# Debug check to confirm files exist
message("Checking local spatial boundary layers...")
for (level_code in names(spatial_levels)) {
  file_path <- file.path(geojson_dir, spatial_levels[[level_code]]$file)
  if (file.exists(file_path)) {
    message(sprintf("  [OK] Found %s layer: %s", level_code, file_path))
  } else {
    warning(sprintf(
      "  [MISSING] Could not find file for %s at: %s",
      level_code,
      file_path
    ))
  }
}

# ==============================================================================
# Historical PECD Climate Data Configuration
# ==============================================================================

# Mapping between UI spatial level codes and the SpatialLevel column in Parquet.
# Now a direct 1:1 mapping after correcting the GeoJSON filenames to match the
# Copernicus CDS PECD v4.2 naming convention.
spatial_level_to_parquet <- c(
  "NUT0" = "nuts_0",
  "NUT2" = "nuts_2",
  "P2ON" = "p2on",
  "P2OF" = "p2of",
  "SZON" = "szon",
  "SZOF" = "szof"
)

# Configuration metadata for the PECD climate variables
# Includes clean display labels, scientific units, and harmonious color palettes
# for visualization (climatology/research analyst aesthetic)
climate_variables <- list(
  "2m_temperature" = list(
    label = "2m Temperature",
    unit = "°C",
    palette = c(
      "#2166ac",
      "#67a9cf",
      "#d1e5f0",
      "#fddbc7",
      "#ef8a62",
      "#b2182b"
    ) # Diverging Blue-to-Red
  ),
  "total_precipitation" = list(
    label = "Total Precipitation",
    unit = "mm",
    palette = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b") # Single-hue Blues
  ),
  "surface_solar_radiation_downwards" = list(
    label = "Solar Radiation",
    unit = "W/m²",
    palette = c("#1a1a2e", "#e67e22", "#f39c12", "#f1c40f", "#ffeaa7") # Dark navy to glowing orange/yellow
  ),
  "10m_wind_speed" = list(
    label = "10m Wind Speed",
    unit = "m/s",
    palette = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac") # Multi-hue Blue-Green
  ),
  "100m_wind_speed" = list(
    label = "100m Wind Speed",
    unit = "m/s",
    palette = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac") # Multi-hue Blue-Green
  )
)

# Open Hive-partitioned PECD datasets as lazy Arrow connections.
# No data is read into RAM at startup — Arrow only scans the folder structure.
# When the app filters by variable/spatial level/year, Arrow uses partition
# pruning to read only the exact parquet fragment needed (millisecond queries).
hist_annual_ds <- NULL
hist_seasonal_ds <- NULL
proj_annual_ds <- NULL
proj_seasonal_ds <- NULL

message("Connecting to PECD climate datasets (lazy Arrow connections)...")

hist_annual_path <- "www/data/pecd/historical/annual"
if (dir.exists(hist_annual_path)) {
  hist_annual_ds <- arrow::open_dataset(hist_annual_path)
  message(sprintf(
    "  [OK] Historical annual dataset connected (%d columns)",
    ncol(hist_annual_ds)
  ))
} else {
  warning(sprintf(
    "  [MISSING] Historical annual dataset not found at: %s",
    hist_annual_path
  ))
}

hist_seasonal_path <- "www/data/pecd/historical/seasonal"
if (dir.exists(hist_seasonal_path)) {
  hist_seasonal_ds <- arrow::open_dataset(hist_seasonal_path)
  message(sprintf(
    "  [OK] Historical seasonal dataset connected (%d columns)",
    ncol(hist_seasonal_ds)
  ))
} else {
  warning(sprintf(
    "  [MISSING] Historical seasonal dataset not found at: %s",
    hist_seasonal_path
  ))
}

proj_annual_path <- "www/data/pecd/projections/annual"
if (dir.exists(proj_annual_path)) {
  proj_annual_ds <- arrow::open_dataset(proj_annual_path)
  message(sprintf(
    "  [OK] Projection annual dataset connected (%d columns)",
    ncol(proj_annual_ds)
  ))
} else {
  message(
    "  [INFO] Projection annual dataset not yet available (will be created by process_pecd_projections.R)"
  )
}

proj_seasonal_path <- "www/data/pecd/projections/seasonal"
if (dir.exists(proj_seasonal_path)) {
  proj_seasonal_ds <- arrow::open_dataset(proj_seasonal_path)
  message(sprintf(
    "  [OK] Projection seasonal dataset connected (%d columns)",
    ncol(proj_seasonal_ds)
  ))
} else {
  message(
    "  [INFO] Projection seasonal dataset not yet available (will be created by process_pecd_projections.R)"
  )
}

# Pre-load all spatial boundary layers into a global list at startup to prevent disk I/O lag
spatial_boundary_cache <- list()
message("Pre-loading spatial boundary layers...")
for (level_code in names(spatial_levels)) {
  file_path <- file.path(geojson_dir, spatial_levels[[level_code]]$file)
  if (file.exists(file_path)) {
    sf_data <- sf::st_read(file_path, quiet = TRUE)
    # Safety check for correct coordinate reference system
    if (!is.na(sf::st_crs(sf_data)$epsg) && sf::st_crs(sf_data)$epsg != 4326) {
      message(sprintf(
        "  [Warning] CRS for %s is not WGS84 — reprojecting to EPSG:4326...",
        level_code
      ))
      sf_data <- sf::st_transform(sf_data, crs = 4326)
    }

    spatial_boundary_cache[[level_code]] <- sf_data
    message(sprintf("  [OK] Cached %s: %d features", level_code, nrow(sf_data)))
  }
}



# ==============================================================================
# Projection Data Configuration
# ==============================================================================
# Metadata used by server.R to conditionally show/hide the projection toggle
# and to label scenarios / models in the time-series chart overlay.
# ==============================================================================

# Variables that have projection data available in PECD v4.2
# (only temperature and precipitation for now — solar / wind not available)
projection_available_variables <- c("2m_temperature", "total_precipitation")

# Detect which spatial levels actually have projection data in the parquet store.
# This is computed at startup rather than hard-coded so that newly downloaded
# SZON/SZOF data is picked up automatically after reprocessing.
projection_available_spatial_levels <- character(0)
if (!is.null(proj_annual_ds)) {
  projection_available_spatial_levels <- proj_annual_ds |>
    dplyr::distinct(SpatialLevel) |>
    dplyr::collect() |>
    dplyr::pull(SpatialLevel)
  message(sprintf(
    "  [OK] Projection spatial levels detected: %s",
    paste(projection_available_spatial_levels, collapse = ", ")
  ))
}

# SSP emission scenario display labels — plain-language descriptions for
# non-climate-expert scientists. The key is the parquet column value;
# the value is the human-readable label shown in the UI dropdown.
ssp_scenario_labels <- c(
  "ssp1_2_6" = "SSP1-2.6 \u2014 Sustainability",
  "ssp2_4_5" = "SSP2-4.5 \u2014 Middle of the Road",
  "ssp3_7_0" = "SSP3-7.0 \u2014 Regional Rivalry",
  "ssp5_8_5" = "SSP5-8.5 \u2014 Fossil-fueled Development"
)

# IPCC-inspired color palette for each SSP scenario.
# These are used for the ensemble median line and the model spread envelope
# in the time-series chart. Colors follow the IPCC AR6 convention.
ssp_colors <- list(
  "ssp1_2_6" = list(line = "#2563eb", fill = "rgba(37, 99, 235, 0.15)"),
  "ssp2_4_5" = list(line = "#f59e0b", fill = "rgba(245, 158, 11, 0.15)"),
  "ssp3_7_0" = list(line = "#ef4444", fill = "rgba(239, 68, 68, 0.15)"),
  "ssp5_8_5" = list(line = "#7c3aed", fill = "rgba(124, 58, 237, 0.15)")
)

# CMIP6 model display names (for potential future individual-model toggle)
climate_model_labels <- c(
  "awi_cm_1_1_mr" = "AWI-CM-1.1-MR",
  "bcc_csm2_mr" = "BCC-CSM2-MR",
  "cmcc_cm2_sr5" = "CMCC-CM2-SR5",
  "ec_earth3" = "EC-Earth3",
  "mpi_esm1_2_hr" = "MPI-ESM1-2-HR",
  "mri_esm2_0" = "MRI-ESM2-0"
)

# ==============================================================================
# Diverging Anomaly Palettes (for map choropleth in anomaly mode)
# ==============================================================================
# These replace the sequential palettes when the display mode is "Anomaly".
# The palettes are centered on white/neutral for zero departure, with
# negative values (cooler/drier) on the left and positive (warmer/wetter) right.
# ==============================================================================

# Temperature anomaly: blue (cooler) → white (no change) → red (warmer)
anomaly_palette_temperature <- c(
  "#2166ac",
  "#67a9cf",
  "#d1e5f0",
  "#f7f7f7",
  "#fddbc7",
  "#ef8a62",
  "#b2182b"
)

# Precipitation anomaly: brown (drier) → white (no change) → teal (wetter)
anomaly_palette_precipitation <- c(
  "#8c510a",
  "#d8b365",
  "#f6e8c3",
  "#f5f5f5",
  "#c7eae5",
  "#5ab4ac",
  "#01665e"
)
