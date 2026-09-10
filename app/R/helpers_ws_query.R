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
#   solar_tech     - Optional solar technology subcode (e.g. "60", "63", "40")
#
# Returns:
#   A data.frame with columns: WS, DayOfYear, Month, Day, Value
#   Returns NULL if the dataset is not available or query returns zero rows.
# ------------------------------------------------------------------------------
query_ws_daily <- function(var_name, sp_level, target_region,
                           ws_codes = NULL, solar_tech = NULL) {

  # Guard: exit early if the WS daily dataset is not loaded
  if (is.null(ws_daily_ds)) {
    message("[WS Query] Weather Scenarios daily dataset not available.")
    return(NULL)
  }

  # Normalize spatial level case-insensitively
  sp_lower <- tolower(sp_level)
  lookup <- c(
    "nut0" = "nuts_0", "nuts_0" = "nuts_0",
    "nut2" = "nuts_2", "nuts_2" = "nuts_2",
    "p2on" = "p2on",   "p2of"   = "p2of",
    "szon" = "szon",   "szof"   = "szof"
  )
  if (sp_lower %in% names(lookup)) {
    query_sp_level <- lookup[[sp_lower]]
  } else if (exists("spatial_level_to_parquet") && sp_level %in% names(spatial_level_to_parquet)) {
    query_sp_level <- spatial_level_to_parquet[[sp_level]]
  } else {
    query_sp_level <- sp_level
  }

  # SZOF normalization: strip _OFF suffix from region IDs to match
  # the parquet convention (same rule as helpers_climate_query.R)
  if (query_sp_level == "szof") {
    target_region <- sub("_OFF$", "", target_region)
  }

  # Resolve partition variable for solar indicators
  if (var_name %in% c("solar_power_pv", "solar_power_csp")) {
    if (var_name == "solar_power_pv") {
      valid_tech <- !is.null(solar_tech) && nzchar(solar_tech) && as.character(solar_tech) %in% c("60", "61", "62", "63")
      subcode <- if (valid_tech) as.character(solar_tech) else "60"
      actual_var <- paste0("solar_power_pv_", subcode)
    } else {
      valid_tech <- !is.null(solar_tech) && nzchar(solar_tech) && as.character(solar_tech) %in% c("40", "41", "42", "43")
      subcode <- if (valid_tech) as.character(solar_tech) else "40"
      actual_var <- paste0("solar_power_csp_", subcode)
    }
  } else {
    actual_var <- var_name
  }

  # Start with base filters: variable and spatial level + target region
  query <- ws_daily_ds |>
    dplyr::filter(
      variable == !!actual_var,
      SpatialLevel == !!query_sp_level,
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

  # Fallback: if actual_var != var_name returned 0 rows, try var_name directly
  if (nrow(df_result) == 0 && actual_var != var_name) {
    query_fallback <- ws_daily_ds |>
      dplyr::filter(
        variable == !!var_name,
        SpatialLevel == !!query_sp_level,
        Region == !!target_region
      )
    if (!is.null(ws_codes)) {
      query_fallback <- query_fallback |> dplyr::filter(WS %in% !!ws_codes)
    }
    query_fallback <- query_fallback |> dplyr::select(WS, DayOfYear, Month, Day, Value)
    df_result <- as.data.frame(dplyr::collect(query_fallback))
  }

  # Return NULL for empty results
  if (nrow(df_result) == 0) return(NULL)

  # Sort by WS code and day of year for clean chart rendering
  df_result <- df_result[order(df_result$WS, df_result$DayOfYear), ,
                         drop = FALSE]

  df_result
}


# ------------------------------------------------------------------------------
# query_ws_daily_for_drawer()
# ------------------------------------------------------------------------------
# Dedicated helper wrapper for stats drawer daily queries, allowing explicit
# solar technology subcode specification.
# ------------------------------------------------------------------------------
query_ws_daily_for_drawer <- function(var_name, sp_level, target_region,
                                      ws_codes = NULL, solar_tech = NULL) {
  query_ws_daily(
    var_name = var_name,
    sp_level = sp_level,
    target_region = target_region,
    ws_codes = ws_codes,
    solar_tech = solar_tech
  )
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

  row <- ws_mapping_table[toupper(ws_mapping_table$CodeName) == toupper(ws_code), ]

  if (nrow(row) == 0) return(ws_code)

  sprintf("%s (%s — %d)", row$CodeName[1], row$ClimateModel[1], row$ClimateYear[1])
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
# ------------------------------------------------------------------------------
# query_ws_annual_for_map()
# ------------------------------------------------------------------------------
# Fetches the annual value for a single Weather Scenario (WS) from the EXISTING
# projection annual dataset (proj_annual_ds). This is used to color the map
# choropleth and display summary metric cards when the user selects a WS.
#
# Supports:
#   - Standard climate variables (2m_temperature, total_precipitation, etc.)
#   - Solar variables (solar_power_pv, solar_power_csp) via direct subcode lookup
#   - Wind power variables (wind_power_onshore, wind_power_offshore):
#       * Default: "fixed_2020" mode queries the pure raw existing fleet
#         (tech_30 for onshore, tech_20 for offshore) under SSP2-4.5
#       * Optional: "dynamic" mode runs dynamic technology mixing via
#         blend_wind_power_all_regions filtered to the scenario model and year
#
# Arguments:
#   var_name       - PECD variable name (e.g., "solar_power_pv", "wind_power_onshore")
#   sp_level       - Spatial level in parquet or UI format (e.g., "p2on", "P2ON", "nuts_0")
#   ws_code        - A single WS code (e.g., "WS01")
#   solar_tech     - (Optional) Active solar technology code (e.g., "60", "40")
#   tech_mix_mode  - (Optional) Wind tech mix mode; defaults to "fixed_2020"
#
# Returns:
#   A data.frame with columns: Region, Value
#   Returns NULL if the dataset or mapping is not available.
# ------------------------------------------------------------------------------
query_ws_annual_for_map <- function(var_name, sp_level, ws_code,
                                    solar_tech = NULL,
                                    tech_mix_mode = "fixed_2020") {

  # Guard: check both the mapping table and the annual dataset exist
  if (is.null(ws_mapping_table) || !exists("proj_annual_ds") || is.null(proj_annual_ds)) {
    message("[WS Map Query] Required datasets not available.")
    return(NULL)
  }

  # Look up the model and year for this WS code
  ws_row <- ws_mapping_table[toupper(ws_mapping_table$CodeName) == toupper(ws_code), ]
  if (nrow(ws_row) == 0) {
    message(sprintf("[WS Map Query] Unknown WS code: %s", ws_code))
    return(NULL)
  }

  target_model <- if ("ModelKey" %in% names(ws_row) && !is.na(ws_row$ModelKey[1])) {
    ws_row$ModelKey[1]
  } else if (exists("ws_model_lookup") && ws_row$ClimateModel[1] %in% names(ws_model_lookup)) {
    ws_model_lookup[[ws_row$ClimateModel[1]]]
  } else {
    ws_row$ClimateModel[1]
  }
  target_year <- ws_row$ClimateYear[1]

  # Normalize spatial level case-insensitively
  sp_lower <- tolower(sp_level)
  lookup <- c(
    "nut0" = "nuts_0", "nuts_0" = "nuts_0",
    "nut2" = "nuts_2", "nuts_2" = "nuts_2",
    "p2on" = "p2on",   "p2of"   = "p2of",
    "szon" = "szon",   "szof"   = "szof"
  )
  if (sp_lower %in% names(lookup)) {
    query_sp_level <- lookup[[sp_lower]]
  } else if (exists("spatial_level_to_parquet") && sp_level %in% names(spatial_level_to_parquet)) {
    query_sp_level <- spatial_level_to_parquet[[sp_level]]
  } else {
    query_sp_level <- sp_level
  }

  # ── Solar Power Indicators (Direct Technology Lookup) ──────────────────────
  if (var_name %in% c("solar_power_pv", "solar_power_csp")) {
    # Guard: Solar power projections only exist at P2ON and aggregated NUT0
    if (!(query_sp_level %in% c("p2on", "nuts_0"))) return(NULL)

    # Default subcodes if not provided or incompatible:
    # 60 = Industrial rooftop for PV (valid: 60, 61, 62, 63)
    # 40 = Pre-dispatch, no storage for CSP (valid: 40, 41, 42, 43)
    if (var_name == "solar_power_pv") {
      valid_tech <- !is.null(solar_tech) && nzchar(solar_tech) && as.character(solar_tech) %in% c("60", "61", "62", "63")
      subcode <- if (valid_tech) as.character(solar_tech) else "60"
      target_var <- paste0("solar_photovoltaic_", subcode)
    } else {
      valid_tech <- !is.null(solar_tech) && nzchar(solar_tech) && as.character(solar_tech) %in% c("40", "41", "42", "43")
      subcode <- if (valid_tech) as.character(solar_tech) else "40"
      target_var <- paste0("solar_concentrated_", subcode)
    }

    # Solar data in PECD projections is partitioned under p2on
    actual_sp_level <- "p2on"

    query <- proj_annual_ds |>
      dplyr::filter(
        variable == !!target_var,
        SpatialLevel == !!actual_sp_level,
        scenario == "ssp2_4_5",
        model == !!target_model,
        Year == !!target_year
      ) |>
      dplyr::select(Region, Value)

    df_result <- as.data.frame(dplyr::collect(query))
    if (nrow(df_result) == 0) return(NULL)

    # For NUT0 queries, aggregate granular P2ON regions up to country-level NUT0 codes
    if (query_sp_level == "nuts_0") {
      areas_df <- NULL
      if (exists("spatial_boundary_cache") && !is.null(spatial_boundary_cache[["P2ON"]])) {
        sf_p2on <- spatial_boundary_cache[["P2ON"]]
        if (inherits(sf_p2on, "sf") && requireNamespace("sf", quietly = TRUE)) {
          areas_df <- sf_p2on |>
            sf::st_drop_geometry() |>
            dplyr::select(zone_id, area_km2) |>
            dplyr::rename(Region = zone_id)
        } else if (is.data.frame(sf_p2on) && all(c("zone_id", "area_km2") %in% names(sf_p2on))) {
          areas_df <- sf_p2on[, c("zone_id", "area_km2")]
          names(areas_df) <- c("Region", "area_km2")
        }
      }

      # Fallback to reading geojson via jsonlite if spatial_boundary_cache not available
      if (is.null(areas_df)) {
        geojson_path <- if (file.exists("www/data/geo/pecd_P2ON.geojson")) {
          "www/data/geo/pecd_P2ON.geojson"
        } else if (file.exists("app/www/data/geo/pecd_P2ON.geojson")) {
          "app/www/data/geo/pecd_P2ON.geojson"
        } else NULL

        if (!is.null(geojson_path) && requireNamespace("jsonlite", quietly = TRUE)) {
          tryCatch({
            geo_json <- jsonlite::fromJSON(geojson_path)
            props <- geo_json$features$properties
            if (all(c("zone_id", "area_km2") %in% names(props))) {
              areas_df <- data.frame(Region = props$zone_id, area_km2 = props$area_km2, stringsAsFactors = FALSE)
            }
          }, error = function(e) NULL)
        }
      }

      if (!is.null(areas_df)) {
        areas_df$Region <- as.character(areas_df$Region)
        df_result$Region <- as.character(df_result$Region)
        df_result <- df_result |>
          dplyr::left_join(areas_df, by = "Region") |>
          dplyr::mutate(
            NUT0 = substr(Region, 1, 2),
            area_km2 = ifelse(is.na(area_km2), 1, area_km2)
          ) |>
          dplyr::group_by(NUT0) |>
          dplyr::summarize(
            Value = if (all(is.na(Value))) NA_real_ else sum(Value * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(Value)]),
            .groups = "drop"
          ) |>
          dplyr::rename(Region = NUT0)
      } else {
        # Fallback to unweighted mean
        df_result <- df_result |>
          dplyr::mutate(NUT0 = substr(Region, 1, 2)) |>
          dplyr::group_by(NUT0) |>
          dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
          dplyr::rename(Region = NUT0)
      }
      df_result <- as.data.frame(df_result)
    }

    return(df_result)
  }

  # ── Wind Power Indicators ──────────────────────────────────────────────────
  if (var_name %in% c("wind_power_onshore", "wind_power_offshore")) {
    wind_type <- if (var_name == "wind_power_onshore") "onshore" else "offshore"
    valid_levels <- if (wind_type == "onshore") c("p2on", "nuts_0") else c("p2of", "nuts_0")
    if (!(query_sp_level %in% valid_levels)) return(NULL)
    
    # Check if dynamic blend requested (default is fixed_2020: pure raw existing fleet)
    if (!is.null(tech_mix_mode) && tech_mix_mode == "dynamic") {
      df_blend <- blend_wind_power_all_regions(
        tech_mix_mode = "dynamic",
        wind_type = wind_type,
        ds_annual = proj_annual_ds,
        ds_seasonal = NULL,
        ds_monthly = NULL,
        temporal_mode = "Annual",
        sp_level = query_sp_level,
        year = target_year,
        scenario_val = "ssp2_4_5",
        model_val = target_model
      )
      if (is.null(df_blend) || nrow(df_blend) == 0) return(NULL)
      df_result <- df_blend |> dplyr::select(Region, Value)
      return(as.data.frame(df_result))
    } else {
      # Fixed existing technology (pure raw existing fleet: tech_30 for onshore, tech_20 for offshore)
      if (query_sp_level == "nuts_0") {
        # NUT0 requires area-weighted aggregation from granular tiers
        df_blend <- blend_wind_power_all_regions(
          tech_mix_mode = "fixed_2020",
          wind_type = wind_type,
          ds_annual = proj_annual_ds,
          ds_seasonal = NULL,
          ds_monthly = NULL,
          temporal_mode = "Annual",
          sp_level = "nuts_0",
          year = target_year,
          scenario_val = "ssp2_4_5",
          model_val = target_model
        )
        if (is.null(df_blend) || nrow(df_blend) == 0) return(NULL)
        df_result <- df_blend |> dplyr::select(Region, Value)
        return(as.data.frame(df_result))
      } else {
        # Direct lookup of existing fleet variable
        target_var <- if (wind_type == "onshore") "wind_onshore_30" else "wind_offshore_20"
        actual_sp_level <- if (wind_type == "onshore") "p2on" else "p2of"

        query <- proj_annual_ds |>
          dplyr::filter(
            variable == !!target_var,
            SpatialLevel == !!actual_sp_level,
            scenario == "ssp2_4_5",
            model == !!target_model,
            Year == !!target_year
          ) |>
          dplyr::select(Region, Value)

        df_result <- as.data.frame(dplyr::collect(query))
        if (nrow(df_result) == 0) return(NULL)
        return(df_result)
      }
    }
  }

  # ── Standard Climate Variables (temperature, precip, wind speed, etc.) ─────
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
