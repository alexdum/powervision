# pipeline/process_pecd_projections.R
# ==============================================================================
# Copernicus PECD v4.2 Climate Projections Data Processing Pipeline
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================
# This script processes raw CMIP6 climate projection CSV files (2015-2100)
# downloaded from the Copernicus CDS API and compiles them into clean,
# lightweight annual and seasonal syntheses.
#
# Unlike the historical pipeline (ERA5 reanalysis, single dataset), projections
# have two extra dimensions: climate model (6 CMIP6 GCMs) and emission scenario
# (4 SSPs). The Parquet output is Hive-partitioned by variable/scenario/model.
#
# Output format: Apache Parquet (.parquet)
#   - Hive-partitioned by variable, scenario, and model
#   - A "SpatialLevel" column identifies the zone type (nuts_0, nuts_2, etc.)
#   - Columnar storage with built-in snappy compression
#   - Extremely fast filtered reads via the R `arrow` package
#   - Cross-language compatible (R, Python, Julia, etc.)
#
# Output location: www/data/pecd/projections/
#   - annual/   — partitioned by variable/scenario/model
#   - seasonal/ — partitioned by variable/scenario/model
#
# Column schema:
#   Annual:   | SpatialLevel | Region | Year | Value |
#   Seasonal: | SpatialLevel | Region | Year | Season | Value |
#
# Note: CMIP6 projections do NOT include SZON/SZOF bidding zones — only
# nuts_0, nuts_2, p2on, and p2of spatial levels are available.
# ==============================================================================

library(data.table)
library(arrow)

# ==============================================================================
# Configuration
# ==============================================================================

# Input directory containing downloaded ZIP files
INPUT_DIR <- "raw/pecd_csv/projections"

# Output directories for processed syntheses
OUTPUT_ANNUAL_DIR <- "www/data/pecd/projections/annual"
OUTPUT_SEASONAL_DIR <- "www/data/pecd/projections/seasonal"

# Ensure output directories exist
dir.create(OUTPUT_ANNUAL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_SEASONAL_DIR, recursive = TRUE, showWarnings = FALSE)

# CMIP6 Models
CLIMATE_MODELS <- c(
  "awi_cm_1_1_mr",
  "bcc_csm2_mr",
  "cmcc_cm2_sr5",
  "ec_earth3",
  "mpi_esm1_2_hr",
  "mri_esm2_0"
)

# CMIP6 Scenarios
EMISSION_SCENARIOS <- c(
  "ssp1_2_6",
  "ssp2_4_5",
  "ssp3_7_0",
  "ssp5_8_5"
)

# Active climate variables to process
CLIMATE_VARIABLES <- c(
  "2m_temperature",
  "total_precipitation"
  # "surface_solar_radiation_downwards",
  # "10m_wind_speed",
  # "100m_wind_speed"
)

# Spatial levels to process.
#
# Projection data spatial levels (CMIP6 projections do NOT include bidding
# zones SZON/SZOF — only nuts_0, nuts_2, p2on, p2of are available).
SPATIAL_LEVELS <- data.frame(
  SpatialLevel = c(
    "nuts_0",
    "nuts_2",
    "p2on",
    "p2of"
  ),
  PrimaryToken = c(
    "nuts_0",
    "nuts_2",
    "p2on",
    "p2of"
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# Main Processing Loop
# ==============================================================================

message(strrep("=", 80))
message("Starting PECD v4.2 Projections Climate Aggregation Pipeline")
message("Output format: Apache Parquet (Hive partitioned by variable/scenario/model)")
message("Processing period: 2015 - 2100")
message(strrep("=", 80))

for (var_idx in seq_along(CLIMATE_VARIABLES)) {
  variable <- CLIMATE_VARIABLES[var_idx]

  message(sprintf("\n[%d/%d] Processing variable: '%s'...",
                  var_idx, length(CLIMATE_VARIABLES), variable))

  for (model_idx in seq_along(CLIMATE_MODELS)) {
    active_model <- CLIMATE_MODELS[model_idx]

    for (ssp_idx in seq_along(EMISSION_SCENARIOS)) {
      active_ssp <- EMISSION_SCENARIOS[ssp_idx]

      message(sprintf("\n  [Model %d/%d | Scenario %d/%d] %s / %s",
                      model_idx, length(CLIMATE_MODELS),
                      ssp_idx, length(EMISSION_SCENARIOS),
                      active_model, active_ssp))

      # Accumulators for this specific model+scenario combination
      combo_annual_chunks <- list()
      combo_seasonal_chunks <- list()

      for (level_row in seq_len(nrow(SPATIAL_LEVELS))) {
        spatial_level <- SPATIAL_LEVELS$SpatialLevel[level_row]
        primary_file_token <- SPATIAL_LEVELS$PrimaryToken[level_row]

        # 1. Locate all ZIP files for this variable, model, scenario, and spatial level.
        # Format: pecd42_proj_{model}_{ssp}_{variable}_{spatial_token}_{years}.zip
        zip_files <- list.files(
          path = INPUT_DIR,
          pattern = sprintf("^pecd42_proj_%s_%s_%s_%s_.*\\.zip$", active_model, active_ssp, variable, primary_file_token),
          full.names = TRUE
        )

    active_file_token <- primary_file_token

    if (length(zip_files) == 0) {
      next
    }

    message(sprintf(
      "    -> Level '%s': Found %d chunk ZIP files.",
      spatial_level,
      length(zip_files)
    ))

    # 2. Loop through each ZIP chunk file for this spatial level
    for (zip_path in zip_files) {
      message(sprintf("      Chunk: %s", basename(zip_path)))

      # Create a unique temporary directory to extract files safely
      temp_extract_dir <- tempfile("pe_extract_")
      dir.create(temp_extract_dir)

      # Unzip all files in the current zip file into our temp directory
      unzip(zip_path, exdir = temp_extract_dir)

      # List all extracted CSV files (one per year in this decade chunk)
      csv_files <- list.files(temp_extract_dir, pattern = "\\.csv$", full.names = TRUE)

      if (length(csv_files) == 0) {
        warning(sprintf("      No CSV files found inside ZIP: %s", basename(zip_path)))
        unlink(temp_extract_dir, recursive = TRUE)
        next
      }

      # 3. Read all yearly CSV files in this decade chunk together
      csv_list <- list()
      for (csv_path in csv_files) {
        chunk_df <- tryCatch({
          fread(csv_path, skip = "Date", header = TRUE)
        }, error = function(e) {
          message(sprintf("        [ERROR] Failed to read %s: %s",
                          basename(csv_path), e$message))
          NULL
        })
        if (!is.null(chunk_df) && nrow(chunk_df) > 0) {
          csv_list[[length(csv_list) + 1]] <- chunk_df
        }
      }

      if (length(csv_list) == 0) {
        unlink(temp_extract_dir, recursive = TRUE)
        next
      }

      # Bind all years in this decade chunk together into a single table
      raw_df <- rbindlist(csv_list, use.names = TRUE, fill = TRUE)

      # 4. Extract time parts using fast string slicing on raw WIDE table
      #    We do this BEFORE melting because the wide table has 300x fewer rows than the long table!
      #    For 300 regions, this reduces character string parsing operations from 26 million to 87,000.
      raw_df[, Year := as.integer(substr(Date, 1, 4))]
      raw_df[, Month := as.integer(substr(Date, 6, 7))]

      # Standard meteorological seasons:
      #   Winter (Dec, Jan, Feb), Spring (Mar, Apr, May),
      #   Summer (Jun, Jul, Aug), Autumn (Sep, Oct, Nov)
      raw_df[, Season := c(
        "Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
        "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter"
      )[Month]]

      # Meteorological winter convention: December of year Y belongs to
      # the winter of year Y+1 (e.g. Dec 1950 -> Winter 1951)
      raw_df[, MetYear := ifelse(Month == 12, Year + 1, Year)]

      # 5. Reshape from WIDE (one column per region) to LONG format
      #    We specify our pre-computed time columns as id.vars to preserve them.
      df_long <- melt(
        raw_df,
        id.vars = c("Date", "Year", "Month", "Season", "MetYear"),
        variable.name = "Region",
        value.name = "Value"
      )

      # 6. Annual aggregation — uses calendar year, all months included
      if (variable == "total_precipitation") {
        # Convert hourly meters to annual total in millimeters (mm)
        annual_agg <- df_long[, .(Value = sum(Value, na.rm = TRUE) * 1000),
                              by = .(Region, Year)]
      } else {
        annual_agg <- df_long[, .(Value = mean(Value, na.rm = TRUE)),
                              by = .(Region, Year)]
      }

      # 7. Seasonal aggregation — remove incomplete edge winters first
      #    Drop Dec 2100: no Jan/Feb 2101 exist to complete Winter 2101
      #    Drop Jan+Feb 2015: no Dec 2014 exists to complete Winter 2015
      df_seasonal <- df_long[!(Year == 2100 & Month == 12) &
                             !(Year == 2015 & Month %in% c(1, 2))]

      if (variable == "total_precipitation") {
        # Convert hourly meters to seasonal total in millimeters (mm)
        seasonal_agg <- df_seasonal[, .(Value = sum(Value, na.rm = TRUE) * 1000),
                                    by = .(Region, Year = MetYear, Season)]
      } else {
        seasonal_agg <- df_seasonal[, .(Value = mean(Value, na.rm = TRUE)),
                                    by = .(Region, Year = MetYear, Season)]
      }

      # Tag every row with the spatial level so we can filter later
      annual_agg[, SpatialLevel := spatial_level]
      seasonal_agg[, SpatialLevel := spatial_level]

      # Append to our model+scenario accumulator lists
      combo_annual_chunks[[length(combo_annual_chunks) + 1]] <- annual_agg
      combo_seasonal_chunks[[length(combo_seasonal_chunks) + 1]] <- seasonal_agg

      # Clean up extracted CSV files immediately to preserve disk space
      unlink(temp_extract_dir, recursive = TRUE)

      # Force memory cleanup of multi-gigabyte intermediate tables to prevent OOM
      rm(csv_list, raw_df, df_long, df_seasonal)
      gc()
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Combine all spatial levels into a single table for this model+scenario
  # ---------------------------------------------------------------------------
      if (length(combo_annual_chunks) == 0) {
        next
      }

      annual_dt <- rbindlist(combo_annual_chunks)
      seasonal_dt <- rbindlist(combo_seasonal_chunks)

      # Ensure Region and SpatialLevel are clean character columns (not factors)
      annual_dt[, Region := as.character(Region)]
      annual_dt[, SpatialLevel := as.character(SpatialLevel)]
      seasonal_dt[, Region := as.character(Region)]
      seasonal_dt[, SpatialLevel := as.character(SpatialLevel)]

      # ===========================================================================
      # METEOROLOGICAL WINTER CONVENTION AGGREGATION FIX (DUPLICATE REMOVAL)
      # ===========================================================================
      if (variable == "total_precipitation") {
        seasonal_dt <- seasonal_dt[, .(Value = sum(Value, na.rm = TRUE)),
                                    by = .(SpatialLevel, Region, Year, Season)]
      } else {
        seasonal_dt <- seasonal_dt[, .(Value = mean(Value, na.rm = TRUE)),
                                    by = .(SpatialLevel, Region, Year, Season)]
      }

      # Reorder columns for clarity
      setcolorder(annual_dt, c("SpatialLevel", "Region", "Year", "Value"))
      setcolorder(seasonal_dt, c("SpatialLevel", "Region", "Year", "Season", "Value"))

      # Sort for clean, structured data delivery
      setorder(annual_dt, SpatialLevel, Region, Year)
      setorder(seasonal_dt, SpatialLevel, Region, Year, Season)

      # ---------------------------------------------------------------------------
      # 8. Write Parquet Dataset using Hive Partitioning
      # ---------------------------------------------------------------------------
      # Add partition columns
      annual_dt[, variable := variable]
      annual_dt[, scenario := active_ssp]
      annual_dt[, model := active_model]

      seasonal_dt[, variable := variable]
      seasonal_dt[, scenario := active_ssp]
      seasonal_dt[, model := active_model]

      # Write partitioned datasets. We use `c("variable", "scenario", "model")`
      # so it nests exactly like that.
      write_dataset(annual_dt, path = OUTPUT_ANNUAL_DIR, partitioning = c("variable", "scenario", "model"))
      write_dataset(seasonal_dt, path = OUTPUT_SEASONAL_DIR, partitioning = c("variable", "scenario", "model"))
    }
  }
}

message(paste0("\n", strrep("=", 80)))
message("Projections Data Processing Complete!")
message("Processed partitioned datasets stored inside: www/data/pecd/projections/")
message(strrep("=", 80))
