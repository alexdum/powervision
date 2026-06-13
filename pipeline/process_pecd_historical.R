# pipeline/process_pecd_historical.R
# ==============================================================================
# Copernicus PECD v4.2 Historical Data Processing Pipeline
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================
# This script processes raw ERA5 historical climate variable CSV files (1950-2023)
# downloaded from the Copernicus CDS API and compiles them into clean,
# lightweight annual and seasonal syntheses.
#
# Output format: Apache Parquet (.parquet)
#   - One file per climate variable, with ALL spatial levels included
#   - A "SpatialLevel" column identifies the zone type (nuts_0, nuts_2, etc.)
#   - Columnar storage with built-in snappy compression
#   - Extremely fast filtered reads via the R `arrow` package
#   - Cross-language compatible (R, Python, Julia, etc.)
#
# Output location: www/data/pecd/historical/
#   - annual/   — one .parquet file per variable (all spatial levels inside)
#   - seasonal/ — one .parquet file per variable (all spatial levels inside)
#
# Column schema:
#   Annual:   | SpatialLevel | Region | Year | Value |
#   Seasonal: | SpatialLevel | Region | Year | Season | Value |
# ==============================================================================

library(data.table)
library(arrow)

# ==============================================================================
# Configuration
# ==============================================================================

# Input directory containing downloaded ZIP files
INPUT_DIR <- "raw/pecd_csv/historical"

# Output directories for processed syntheses
OUTPUT_ANNUAL_DIR <- "www/data/pecd/historical/annual"
OUTPUT_SEASONAL_DIR <- "www/data/pecd/historical/seasonal"

# Ensure output directories exist
dir.create(OUTPUT_ANNUAL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_SEASONAL_DIR, recursive = TRUE, showWarnings = FALSE)

# Active climate variables to process
CLIMATE_VARIABLES <- c(
  "2m_temperature",
  "total_precipitation",
  "surface_solar_radiation_downwards",
  "10m_wind_speed",
  "100m_wind_speed"
)

# Spatial levels to process.
#
# The app and parquet files use the stable SpatialLevel values below:
#   nuts_0, nuts_2, peon, peof, szon, szof
#
# The newest PECD v4.2 CSV files use P2ON and P2OF in the filename/header for
# the fine Pan-European node layers. We therefore read local ZIP files named
# with p2on/p2of, and tag the processed parquet rows directly as p2on/p2of.
SPATIAL_LEVELS <- data.frame(
  SpatialLevel = c(
    "nuts_0",
    "nuts_2",
    "p2on",
    "p2of",
    "szon",
    "szof"
  ),
  PrimaryToken = c(
    "nuts_0",
    "nuts_2",
    "p2on",
    "p2of",
    "szon",
    "szof"
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# Main Processing Loop — one iteration per climate variable
# ==============================================================================

message(strrep("=", 80))
message("Starting PECD v4.2 Historical Climate Aggregation Pipeline")
message("Output format: Apache Parquet (one file per variable, all zones inside)")
message("Processing period: 1950 - 2023")
message(strrep("=", 80))

for (var_idx in seq_along(CLIMATE_VARIABLES)) {
  variable <- CLIMATE_VARIABLES[var_idx]

  message(sprintf("\n[%d/%d] Processing variable: '%s'...",
                  var_idx, length(CLIMATE_VARIABLES), variable))

  # These lists will accumulate annual and seasonal aggregates
  # across ALL spatial levels for this variable
  var_annual_chunks <- list()
  var_seasonal_chunks <- list()

  # ---------------------------------------------------------------------------
  # Loop through every spatial level and collect aggregated data
  # ---------------------------------------------------------------------------
  for (level_row in seq_len(nrow(SPATIAL_LEVELS))) {
    spatial_level <- SPATIAL_LEVELS$SpatialLevel[level_row]
    primary_file_token <- SPATIAL_LEVELS$PrimaryToken[level_row]

    message(sprintf("  Level: '%s'", spatial_level))

    # 1. Locate all ZIP files for this variable and spatial level.
    zip_files <- list.files(
      path = INPUT_DIR,
      pattern = sprintf("^pecd42_hist_era5_%s_%s_.*\\.zip$", variable, primary_file_token),
      full.names = TRUE
    )

    active_file_token <- primary_file_token

    if (length(zip_files) == 0) {
      warning(sprintf("    [WARN] No ZIP files found for %s / %s. Skipping.",
                      variable, spatial_level))
      next
    }

    message(sprintf(
      "    Found %d decade-chunk ZIP files using token '%s'.",
      length(zip_files),
      active_file_token
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
      #    Drop Dec 2023: no Jan/Feb 2024 exist to complete Winter 2024
      #    Drop Jan+Feb 1950: no Dec 1949 exists to complete Winter 1950
      df_seasonal <- df_long[!(Year == 2023 & Month == 12) &
                             !(Year == 1950 & Month %in% c(1, 2))]

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

      # Append to our variable-level accumulator lists
      var_annual_chunks[[length(var_annual_chunks) + 1]] <- annual_agg
      var_seasonal_chunks[[length(var_seasonal_chunks) + 1]] <- seasonal_agg

      # Clean up extracted CSV files immediately to preserve disk space
      unlink(temp_extract_dir, recursive = TRUE)

      # Force memory cleanup of multi-gigabyte intermediate tables to prevent OOM
      rm(csv_list, raw_df, df_long, df_seasonal)
      gc()
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Combine all spatial levels into a single table for this variable
  # ---------------------------------------------------------------------------
  annual_dt <- rbindlist(var_annual_chunks)
  seasonal_dt <- rbindlist(var_seasonal_chunks)

  # Ensure Region and SpatialLevel are clean character columns (not factors)
  annual_dt[, Region := as.character(Region)]
  annual_dt[, SpatialLevel := as.character(SpatialLevel)]
  seasonal_dt[, Region := as.character(Region)]
  seasonal_dt[, SpatialLevel := as.character(SpatialLevel)]

  # ===========================================================================
  # METEOROLOGICAL WINTER CONVENTION AGGREGATION FIX (DUPLICATE REMOVAL)
  # ===========================================================================
  # Because each year's CSV is aggregated individually in the loops above,
  # the Winter season (which spans December of year Y and Jan/Feb of year Y+1)
  # is split across two rows in seasonal_dt:
  #   - Row 1: December of year Y (mapped to MetYear Y+1)
  #   - Row 2: Jan/Feb of year Y+1 (mapped to MetYear Y+1)
  # This results in duplicate (SpatialLevel, Region, Year, Season) rows.
  # We now perform a secondary aggregation to merge these two split parts.
  # ===========================================================================
  message("    Merging split meteorological winter seasons...")
  if (variable == "total_precipitation") {
    # Precipitation is a cumulative variable: we sum the two winter parts
    seasonal_dt <- seasonal_dt[, .(Value = sum(Value, na.rm = TRUE)),
                                by = .(SpatialLevel, Region, Year, Season)]
  } else {
    # Temperature, wind speed, and solar radiation are average variables:
    # we average the two winter parts (each representing the respective months)
    seasonal_dt <- seasonal_dt[, .(Value = mean(Value, na.rm = TRUE)),
                                by = .(SpatialLevel, Region, Year, Season)]
  }

  # Reorder columns for clarity: SpatialLevel first, then Region, Year, ...
  setcolorder(annual_dt, c("SpatialLevel", "Region", "Year", "Value"))
  setcolorder(seasonal_dt, c("SpatialLevel", "Region", "Year", "Season", "Value"))

  # Sort for clean, structured data delivery
  setorder(annual_dt, SpatialLevel, Region, Year)
  setorder(seasonal_dt, SpatialLevel, Region, Year, Season)

  # ---------------------------------------------------------------------------
  # 8. Write Parquet Dataset using Hive Partitioning
  # ---------------------------------------------------------------------------
  # Add the 'variable' column to support partitioning
  annual_dt[, variable := variable]
  seasonal_dt[, variable := variable]

  # Instead of writing a single flat file, write_dataset() fragments the output
  # into folders based on the 'variable' column (e.g. variable=2m_temperature/part-0.parquet)
  write_dataset(annual_dt, path = OUTPUT_ANNUAL_DIR, partitioning = "variable")
  write_dataset(seasonal_dt, path = OUTPUT_SEASONAL_DIR, partitioning = "variable")

  message(sprintf("  [OK] Annual dataset written: %d rows (variable=%s)", nrow(annual_dt), variable))
  message(sprintf("  [OK] Seasonal dataset written: %d rows (variable=%s)", nrow(seasonal_dt), variable))
}

message(paste0("\n", strrep("=", 80)))
message("Historical Data Processing Complete!")
message("Processed data stored inside: www/data/pecd/historical/")
message(strrep("=", 80))
