# helpers_ws_query.R
# ==============================================================================
# Weather Scenarios (WS) Query Helper
# ==============================================================================
# This file provides functions for querying the WS daily parquet dataset.
# It is used by server.R to fetch daily annual cycle data for the 36 ENTSO-E
# Weather Scenarios when a user clicks a region on the map.
#
# The WS daily dataset has the schema:
#   SpatialLevel | Region | WS | Model | DayOfYear | Month | Day | Value
# Partitioned by: variable
#
# Global objects used (initialized in global.R):
#   ws_daily_ds       — Lazy Arrow dataset connection
#   ws_mapping_table  — Data frame mapping WS codes to model/year
#   ws_model_lookup   — Named vector mapping short model codes to parquet keys
# ==============================================================================


# ------------------------------------------------------------------------------
# query_ws_daily()
# ------------------------------------------------------------------------------
# Fetches daily data for one or more Weather Scenarios for a specific region
# and variable. Returns a data.frame with columns:
#   WS | DayOfYear | Month | Day | Value
#
# This is the main function called by server.R when the user clicks a region
# and the stats drawer needs to render the annual cycle chart.
#
# Arguments:
#   var_name       - PECD variable name (e.g., "2m_temperature")
#   sp_level       - Spatial level in parquet format (e.g., "nuts_0", "p2on")
#   target_region  - Region/zone ID (e.g., "DE", "FR00")
#   ws_codes       - Character vector of WS codes to fetch (e.g., c("WS01", "WS03")).
#                    If NULL, fetches ALL 36 scenarios.
#
# Returns:
#   A data.frame with columns: WS, DayOfYear, Month, Day, Value
#   Returns NULL if the dataset is not available or query returns zero rows.
# ------------------------------------------------------------------------------
query_ws_daily <- function(var_name, sp_level, target_region,
                           ws_codes = NULL) {

  # Guard: exit early if the WS daily dataset is not loaded
  if (is.null(ws_daily_ds)) {
    message("[WS Query] Weather Scenarios daily dataset not available.")
    return(NULL)
  }

  # SZOF normalization: strip _OFF suffix from region IDs to match
  # the parquet convention (same rule as helpers_climate_query.R)
  if (sp_level == "szof") {
    target_region <- sub("_OFF$", "", target_region)
  }

  # Start with base filters: variable and spatial level + target region
  query <- ws_daily_ds |>
    dplyr::filter(
      variable == !!var_name,
      SpatialLevel == !!sp_level,
      Region == !!target_region
    )

  # Optional: filter to specific WS codes (if NULL, returns all 36)
  if (!is.null(ws_codes)) {
    query <- query |> dplyr::filter(WS %in% !!ws_codes)
  }

  # Select only the columns we need for the chart
  query <- query |>
    dplyr::select(WS, DayOfYear, Month, Day, Value)

  # Execute and collect
  df_result <- as.data.frame(dplyr::collect(query))

  # Return NULL for empty results
  if (nrow(df_result) == 0) return(NULL)

  # Sort by WS code and day of year for clean chart rendering
  df_result <- df_result[order(df_result$WS, df_result$DayOfYear), ,
                         drop = FALSE]

  df_result
}


# ------------------------------------------------------------------------------
# get_ws_display_label()
# ------------------------------------------------------------------------------
# Builds a human-readable label for a WS code to use in dropdown menus
# and chart legends. For example: "WS01 (AWCM — 2026)"
#
# Arguments:
#   ws_code - A single WS code string (e.g., "WS01")
#
# Returns:
#   A formatted display string, or the raw code if mapping is unavailable.
# ------------------------------------------------------------------------------
get_ws_display_label <- function(ws_code) {
  # Guard: if mapping table is not loaded, return the raw code
  if (is.null(ws_mapping_table)) return(ws_code)

  row <- ws_mapping_table[ws_mapping_table$CodeName == ws_code, ]

  if (nrow(row) == 0) return(ws_code)

  sprintf("%s (%s — %d)", ws_code, row$ClimateModel[1], row$ClimateYear[1])
}


# ------------------------------------------------------------------------------
# get_ws_dropdown_choices()
# ------------------------------------------------------------------------------
# Builds a named list suitable for Shiny selectInput() choices.
# The names are human-readable labels, the values are raw WS codes.
#
# Returns:
#   A named character vector where names = display labels, values = WS codes.
#   Example: c("WS01 (AWCM — 2026)" = "WS01", "WS02 (AWCM — 2030)" = "WS02", ...)
# ------------------------------------------------------------------------------
get_ws_dropdown_choices <- function() {
  if (is.null(ws_mapping_table)) {
    return(c("No WS data available" = ""))
  }

  ws_codes <- ws_mapping_table$CodeName
  labels <- sapply(ws_codes, get_ws_display_label)
  choices <- setNames(ws_codes, labels)

  choices
}


# ------------------------------------------------------------------------------
# query_ws_annual_for_map()
# ------------------------------------------------------------------------------
# Fetches the annual value for a single WS from the EXISTING projection
# annual dataset (proj_annual_ds). This is used to color the map when
# the user selects a WS — no new daily data is needed, just the pre-computed
# annual mean/sum from the standard projection parquet.
#
# Arguments:
#   var_name  - PECD variable name (e.g., "2m_temperature")
#   sp_level  - Spatial level in parquet format (e.g., "nuts_0")
#   ws_code   - A single WS code (e.g., "WS01")
#
# Returns:
#   A data.frame with columns: Region, Value
#   Returns NULL if the dataset or mapping is not available.
# ------------------------------------------------------------------------------
query_ws_annual_for_map <- function(var_name, sp_level, ws_code) {

  # Guard: check both the mapping table and the annual dataset exist
  if (is.null(ws_mapping_table) || !exists("proj_annual_ds")) {
    message("[WS Map Query] Required datasets not available.")
    return(NULL)
  }

  # Look up the model and year for this WS code
  ws_row <- ws_mapping_table[ws_mapping_table$CodeName == ws_code, ]
  if (nrow(ws_row) == 0) {
    message(sprintf("[WS Map Query] Unknown WS code: %s", ws_code))
    return(NULL)
  }

  target_model <- ws_row$ModelKey[1]
  target_year <- ws_row$ClimateYear[1]

  # SZOF normalization
  query_sp_level <- sp_level

  # Query the existing projection annual dataset
  # All WS are SSP2-4.5, so we hardcode the scenario
  query <- proj_annual_ds |>
    dplyr::filter(
      variable == !!var_name,
      SpatialLevel == !!query_sp_level,
      scenario == "ssp2_4_5",
      model == !!target_model,
      Year == !!target_year
    ) |>
    dplyr::select(Region, Value)

  df_result <- as.data.frame(dplyr::collect(query))

  if (nrow(df_result) == 0) return(NULL)

  df_result
}
