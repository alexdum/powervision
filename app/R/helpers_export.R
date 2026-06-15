# helpers_export.R
# ==============================================================================
# CSV Data Export Helper for Region Time-Series Download
# ==============================================================================
# Builds the combined historical + projection data.frame that is written to
# CSV when the user clicks the download button in the region stats drawer.
#
# This function:
#   1. Queries historical ERA5 data for the selected region via the
#      centralized query_arrow_dataset() helper
#   2. Optionally queries projection data (all 6 CMIP6 models) if projections
#      are toggled on and available for the variable/spatial combination
#   3. Tags each row with a Source label ("ERA5 Reanalysis" or
#      "CMIP6 Projection (ssp2_4_5)")
#   4. Selects clean export columns and combines both datasets
#   5. Sorts by Year and rounds Values for readability
#
# The exported CSV is designed for scientists who want to do their own
# analyses — it includes all 6 individual model runs (not just the median)
# for maximum scientific utility.
#
# Dependencies:
#   query_arrow_dataset()  - from R/helpers_climate_query.R
#
# Globals accessed from global.R:
#   hist_annual_ds, hist_seasonal_ds     - Lazy Arrow historical datasets
#   proj_annual_ds, proj_seasonal_ds     - Lazy Arrow projection datasets
#   projection_available_variables       - Variables with projection data
#   projection_available_spatial_levels  - Spatial levels with projection data
# ==============================================================================


# ------------------------------------------------------------------------------
# build_export_csv()
# ------------------------------------------------------------------------------
# Collects, cleans, and combines historical and projection data into a single
# data.frame ready to be written to CSV.
#
# Arguments:
#   var_name       - PECD variable key (e.g., "2m_temperature")
#   temp_mode      - "Annual" or season name (e.g., "Winter")
#   target_region  - Zone ID string (e.g., "AT", "DE00", "FRH1")
#   region_name    - Display name of the region (e.g., "Austria", "Bayern")
#   sp_level       - Spatial level in parquet format (e.g., "nuts_0", "p2on")
#   include_proj   - Logical: should projection data be included?
#   scenario_val   - SSP scenario key (e.g., "ssp2_4_5"). Only used when
#                    include_proj is TRUE.
#
# Returns:
#   A data.frame with columns:
#     Year, Value, Source, variable, Region, Region_Name, [Season], [model]
#   Sorted by Year, Values rounded to 1 decimal place.
# ------------------------------------------------------------------------------
build_export_csv <- function(var_name, temp_mode, target_region, region_name,
                             sp_level, include_proj, scenario_val) {

  message(sprintf("  CSV export: region=%s, var=%s, mode=%s", target_region, var_name, temp_mode))

  # ── Collect historical data using the centralized query helper ─────────────
  df_hist <- query_arrow_dataset(
    hist_annual_ds, hist_seasonal_ds, temp_mode,
    var_name, sp_level,
    target_region = target_region
  )

  # Convert to a plain data.frame (Arrow may return tibble/ArrowTabular)
  if (!is.null(df_hist)) {
    df_hist <- as.data.frame(df_hist)
  } else {
    # Create an empty data.frame with the expected columns so rbind works
    df_hist <- data.frame(
      Year = integer(0), Value = numeric(0),
      variable = character(0), Region = character(0),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(df_hist) > 0) {
    # Tag every historical row with its source for clarity in the CSV
    df_hist$Source <- "ERA5 Reanalysis"
    df_hist$Region_Name <- region_name

    # Keep only the columns scientists need — drop internal partition keys
    export_cols <- c("Year", "Value", "Source", "variable", "Region", "Region_Name")
    if ("Season" %in% names(df_hist)) export_cols <- c(export_cols, "Season")
    df_hist <- df_hist[, intersect(export_cols, names(df_hist))]
  }

  # ── Collect projection data if available and requested ─────────────────────
  df_proj_export <- NULL

  # Guard: only attempt projection query if the variable + spatial combination
  # actually has projection data in the parquet store
  proj_data_exists <- (var_name %in% projection_available_variables &&
                       sp_level %in% projection_available_spatial_levels)

  if (include_proj && proj_data_exists) {

    df_proj_raw <- query_arrow_dataset(
      proj_annual_ds, proj_seasonal_ds, temp_mode,
      var_name, sp_level,
      target_region = target_region, scenario_val = scenario_val
    )

    if (!is.null(df_proj_raw)) {
      df_proj_raw <- as.data.frame(df_proj_raw)

      if (nrow(df_proj_raw) > 0) {
        # Include all 6 individual model runs (not just ensemble median)
        # so scientists can do their own statistical analysis
        df_proj_raw$Source <- paste0("CMIP6 Projection (", scenario_val, ")")
        df_proj_raw$Region_Name <- region_name

        # Select export columns — includes "model" for individual model runs
        export_cols_proj <- c("Year", "Value", "Source", "variable",
                              "Region", "Region_Name", "model")
        if ("Season" %in% names(df_proj_raw)) {
          export_cols_proj <- c(export_cols_proj, "Season")
        }
        df_proj_export <- df_proj_raw[, intersect(export_cols_proj, names(df_proj_raw))]
      }
    }
  }

  # ── Combine historical and projection data ─────────────────────────────────
  if (!is.null(df_proj_export)) {
    # Add a model column to historical data for column alignment when rbinding
    if (!"model" %in% names(df_hist)) df_hist$model <- "ERA5"
    df_combined <- rbind(df_hist, df_proj_export)
  } else {
    df_combined <- df_hist
  }

  # Sort chronologically and round values for readability in the CSV
  df_combined <- df_combined[order(df_combined$Year), ]
  df_combined$Value <- round(df_combined$Value, 1)

  df_combined
}
