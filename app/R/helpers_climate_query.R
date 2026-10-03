# helpers_climate_query.R
# ==============================================================================
# Reusable Arrow Dataset Query Helper
# ==============================================================================
# This file contains a single, general-purpose function for querying the
# Hive-partitioned PECD climate datasets via Apache Arrow. It is used by
# multiple reactive blocks in server.R to fetch historical and projected
# climate data from the lazy Arrow connections initialized in global.R.
#
# By centralizing the query logic here, we avoid repeating the same
# filter-and-collect pattern across 7+ locations in server.R.
#
# Note: This file is auto-sourced by Shiny before global.R runs, but
# since it only contains function definitions (not top-level calls), all
# library dependencies (dplyr, arrow) are available by the time these
# functions are actually called from server.R.
# ==============================================================================


# Cached data frame of P2ON bidding zone areas (zone_id and area_km2)
.p2on_areas_cache <- NULL

#' Helper to fetch P2ON polygon areas for area-weighted spatial aggregation
#' Checks spatial_boundary_cache first, then falls back to pecd_P2ON.geojson
get_p2on_areas <- function() {
  if (!is.null(.p2on_areas_cache)) return(.p2on_areas_cache)

  areas_df <- NULL

  # 1. Try global boundary cache
  if (exists("spatial_boundary_cache", envir = .GlobalEnv) &&
      !is.null(.GlobalEnv$spatial_boundary_cache[["P2ON"]])) {
    sf_p2on <- .GlobalEnv$spatial_boundary_cache[["P2ON"]]
    if (inherits(sf_p2on, "sf") && requireNamespace("sf", quietly = TRUE)) {
      areas_df <- sf_p2on |>
        sf::st_drop_geometry() |>
        dplyr::select(zone_id, area_km2) |>
        dplyr::rename(Region = zone_id)
    } else if (is.data.frame(sf_p2on) && all(c("zone_id", "area_km2") %in% names(sf_p2on))) {
      areas_df <- sf_p2on[, c("zone_id", "area_km2")]
      names(areas_df) <- c("Region", "area_km2")
    }
  } else if (exists("spatial_boundary_cache") && !is.null(spatial_boundary_cache[["P2ON"]])) {
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

  # 2. Fallback to GeoJSON files
  if (is.null(areas_df)) {
    candidates <- c(
      "www/data/geo/pecd_P2ON.geojson",
      "app/www/data/geo/pecd_P2ON.geojson",
      "../www/data/geo/pecd_P2ON.geojson",
      "../../app/www/data/geo/pecd_P2ON.geojson"
    )
    for (cand in candidates) {
      if (file.exists(cand) && requireNamespace("jsonlite", quietly = TRUE)) {
        tryCatch({
          geo_json <- jsonlite::fromJSON(cand)
          props <- geo_json$features$properties
          if (all(c("zone_id", "area_km2") %in% names(props))) {
            areas_df <- data.frame(
              Region = as.character(props$zone_id),
              area_km2 = as.numeric(props$area_km2),
              stringsAsFactors = FALSE
            )
            break
          }
        }, error = function(e) NULL)
      }
    }
  }

  if (!is.null(areas_df)) {
    .p2on_areas_cache <<- areas_df
  }
  areas_df
}


# ------------------------------------------------------------------------------
# query_arrow_dataset()
# ------------------------------------------------------------------------------
# Builds and executes a filtered Arrow query against a pair of annual/seasonal
# datasets. Returns a collected data.frame, or NULL if the dataset is missing
# or the query returns zero rows.
#
# This function encapsulates the common query pattern:
#   1. Pick the annual or seasonal dataset based on temporal_mode
#   2. Apply required filters (variable, spatial level)
#   3. Apply optional filters (year, year range, region, scenario, season)
#   4. Collect results into a local data.frame
#
# Arguments:
#   ds_annual      - Lazy Arrow dataset for annual data (or NULL if unavailable)
#   ds_seasonal    - Lazy Arrow dataset for seasonal data (or NULL if unavailable)
#   temporal_mode  - "Annual" or one of "Winter", "Spring", "Summer", "Autumn".
#                    Determines which dataset to query AND whether to apply a
#                    Season filter.
#   var_name       - PECD variable name (e.g., "2m_temperature",
#                    "total_precipitation")
#   sp_level       - Spatial level in parquet column format (e.g., "nuts_0",
#                    "p2on", "szon"). Use spatial_level_to_parquet[] to convert
#                    from the UI-level codes.
#   year           - (Optional) Single year to filter by (integer). Use this
#                    for point-in-time queries (e.g., map choropleth for one year).
#   year_start     - (Optional) Start of year range, inclusive (integer). Use
#                    together with year_end for period queries.
#   year_end       - (Optional) End of year range, inclusive (integer).
#   target_region  - (Optional) Single region/zone ID to filter by (e.g., "AT",
#                    "DE00", "FRH1"). When NULL, returns data for all regions.
#   scenario_val   - (Optional) SSP scenario key for projection queries (e.g.,
#                    "ssp2_4_5"). Only relevant for projection datasets.
#   select_cols    - (Optional) Character vector of column names to return
#                    (e.g., c("Year", "Value", "Region")). When provided, Arrow
#                    pushes the column selection down to the parquet reader so
#                    unused columns are never read from disk — reducing I/O
#                    and memory. When NULL (default), all columns are returned.
#
# Returns:
#   A data.frame with the collected query results, or NULL if the dataset is
#   not available or the query matched zero rows.
# ------------------------------------------------------------------------------
query_arrow_dataset <- function(ds_annual, ds_seasonal, ds_monthly, temporal_mode,
                                var_name, sp_level,
                                year = NULL, year_start = NULL, year_end = NULL,
                                target_region = NULL, scenario_val = NULL,
                                select_cols = NULL, solar_tech = NULL,
                                model_val = NULL) {

  # If a solar variable is selected, resolve the technology subcode
  is_solar <- grepl("^solar_photovoltaic_", var_name) ||
              grepl("^solar_concentrated_", var_name) ||
              var_name %in% c("solar_power_pv", "solar_power_csp")

  if (var_name == "solar_power_csp") {
    tech_code <- if (!is.null(solar_tech) && solar_tech != "") solar_tech else "40"
    var_name <- paste0("solar_concentrated_", tech_code)
  } else if (var_name == "solar_power_pv") {
    tech_code <- if (!is.null(solar_tech) && solar_tech != "") solar_tech else "60"
    var_name <- paste0("solar_photovoltaic_", tech_code)
  }

  is_hydro <- grepl("^hydropower_", var_name)
  is_nut0 <- tolower(sp_level) %in% c("nuts_0", "nut0")

  # Pick the correct dataset based on temporal mode.
  ds <- if (temporal_mode == "Annual") {
    ds_annual
  } else if (temporal_mode %in% c("Winter", "Spring", "Summer", "Autumn")) {
    ds_seasonal
  } else {
    ds_monthly
  }

  # Guard: exit early if the dataset is not available (e.g., projections
  # haven't been downloaded yet, or the parquet directory is missing)
  if (is.null(ds)) return(NULL)

  # ── Dynamic NUTS 0 Aggregation Engine for Solar and Hydropower ──────────────
  if (is_nut0 && (is_solar || is_hydro)) {
    granular_sp_level <- if (is_solar) "p2on" else "szon"

    query <- ds |>
      dplyr::filter(variable == !!var_name, SpatialLevel == !!granular_sp_level)

    # Optional: filter to a single year
    if (!is.null(year)) {
      query <- query |> dplyr::filter(Year == !!year)
    }

    # Optional: filter to a year range
    if (!is.null(year_start) && !is.null(year_end)) {
      query <- query |> dplyr::filter(Year >= !!year_start, Year <= !!year_end)
    }

    # Optional: filter to constituent zones of target region
    if (!is.null(target_region)) {
      target_upper <- toupper(as.character(target_region))
      prefix_targets <- if (target_upper %in% c("EL", "GR")) c("GR", "EL") else target_upper
      query <- query |> dplyr::filter(substr(Region, 1, 2) %in% !!prefix_targets)
    }

    # Defensively exclude synthetic Southern Norway NOS0 for hydro to avoid double-counting
    if (is_hydro) {
      query <- query |> dplyr::filter(Region != "NOS0")
    }

    # Optional: filter by SSP scenario
    if (!is.null(scenario_val)) {
      query <- query |> dplyr::filter(scenario %in% !!scenario_val)
    }

    # Optional: filter by climate model
    if (!is.null(model_val)) {
      query <- query |> dplyr::filter(model %in% !!model_val)
    }

    # Seasonal or monthly filters
    if (temporal_mode %in% c("Winter", "Spring", "Summer", "Autumn")) {
      query <- query |> dplyr::filter(Season == !!temporal_mode)
    } else if (temporal_mode != "Annual") {
      month_int <- as.integer(temporal_mode)
      query <- query |> dplyr::filter(Month == !!month_int)
    }

    # Column selection: ensure Region, Value, and grouping columns are preserved
    safe_select <- unique(c(
      select_cols, "Region", "Value", "variable", "SpatialLevel",
      "Year", "Season", "Month", "scenario", "model"
    ))
    query <- query |> dplyr::select(dplyr::any_of(safe_select))

    df_result <- as.data.frame(dplyr::collect(query))
    if (nrow(df_result) == 0) return(NULL)

    # Secondary guard against NOS0 for hydro
    if (is_hydro) {
      df_result <- df_result[df_result$Region != "NOS0", , drop = FALSE]
      if (nrow(df_result) == 0) return(NULL)
    }

    # Solar: load P2ON areas and join
    if (is_solar) {
      areas_df <- get_p2on_areas()
      if (!is.null(areas_df)) {
        areas_df$Region <- as.character(areas_df$Region)
        df_result$Region <- as.character(df_result$Region)
        df_result <- dplyr::left_join(df_result, areas_df, by = "Region")
        df_result$area_km2 <- ifelse(is.na(df_result$area_km2) | df_result$area_km2 <= 0, 1, df_result$area_km2)
      } else {
        df_result$area_km2 <- 1
      }
    }

    # Map constituent granular zones to country code (normalizing GR -> EL)
    country_codes <- substr(as.character(df_result$Region), 1, 2)
    country_codes <- ifelse(country_codes == "GR", "EL", country_codes)
    df_result$Region <- country_codes

    # Filter to normalized target country if target_region was specified
    if (!is.null(target_region)) {
      target_upper <- toupper(as.character(target_region))
      exp_country <- if (target_upper %in% c("EL", "GR")) "EL" else target_upper
      df_result <- df_result[df_result$Region == exp_country, , drop = FALSE]
      if (nrow(df_result) == 0) return(NULL)
    }

    # Group by all available slice dimensions
    group_cols <- intersect(c("Region", "Year", "Season", "Month", "scenario", "model", "variable"), names(df_result))

    if (is_solar) {
      # Solar: area-weighted mean with active weight re-normalization (Rule 7)
      if (length(group_cols) > 0) {
        df_agg <- df_result |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
          dplyr::summarise(
            Value = if (all(is.na(Value))) NA_real_ else sum(Value * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(Value)]),
            .groups = "drop"
          )
      } else {
        df_agg <- df_result |>
          dplyr::summarise(
            Value = if (all(is.na(Value))) NA_real_ else sum(Value * area_km2, na.rm = TRUE) / sum(area_km2[!is.na(Value)]),
            .groups = "drop"
          )
      }
    } else {
      # Hydropower: volume summation (GWh). Return NA if all constituent values are NA.
      if (length(group_cols) > 0) {
        df_agg <- df_result |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
          dplyr::summarise(
            Value = if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        df_agg <- df_result |>
          dplyr::summarise(
            Value = if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }

    df_agg <- as.data.frame(df_agg)

    if ("SpatialLevel" %in% names(df_result)) {
      df_agg$SpatialLevel <- "nuts_0"
    }

    if (!is.null(select_cols)) {
      available_cols <- intersect(select_cols, names(df_agg))
      df_agg <- df_agg[, available_cols, drop = FALSE]
    }

    if (nrow(df_agg) == 0) return(NULL)

    if ("Year" %in% names(df_agg)) {
      df_agg <- df_agg[order(df_agg$Year), , drop = FALSE]
    }

    return(df_agg)
  }

  # Start with the two required base filters: variable name and spatial level.
  # These are present in every query throughout the app.
  # The !! (bang-bang) operator forces R to evaluate local variables BEFORE
  # Arrow sees the expression — necessary in Arrow 24.x where the dplyr
  # backend can't resolve function-scoped R variables inside filter().
  query <- ds |>
    dplyr::filter(variable == !!var_name, SpatialLevel == !!sp_level)

  # Optional: filter to a single year (used by map choropleth, single-year view)
  if (!is.null(year)) {
    query <- query |> dplyr::filter(Year == !!year)
  }

  # Optional: filter to a year range (used by baseline/period calculations)
  if (!is.null(year_start) && !is.null(year_end)) {
    query <- query |> dplyr::filter(Year >= !!year_start, Year <= !!year_end)
  }

  # Optional: filter to a single region (used by chart and baseline reactives).
  # SZOF normalization: the processing pipeline stripped the "_OFF" suffix from
  # offshore study zone region IDs in the parquet data (e.g., "AL00_OFF" became
  # "AL00"), but the GeoJSON zone_ids still include "_OFF". Strip it here so the
  # Arrow filter matches correctly.
  # Southern Norway (SZON) hydro normalization: the GeoJSON has a single dissolved
  # polygon "NOS0", but Copernicus parquet stores hydro under the 3 sub-bidding
  # zones "NOS1", "NOS2", "NOS3". When "NOS0" is targeted for hydro, query all three
  # and aggregate them after collection.
  is_nos0_hydro <- FALSE
  if (!is.null(target_region)) {
    if (sp_level == "szof") {
      target_region <- sub("_OFF$", "", target_region)
    }
    if (sp_level == "szon" && grepl("^hydropower_", var_name) && isTRUE(target_region == "NOS0")) {
      is_nos0_hydro <- TRUE
      query <- query |> dplyr::filter(Region %in% c("NOS1", "NOS2", "NOS3"))
    } else {
      query <- query |> dplyr::filter(Region == !!target_region)
    }
  }

  # Optional: filter by SSP scenario (only for projection datasets).
  if (!is.null(scenario_val)) {
    query <- query |> dplyr::filter(scenario %in% !!scenario_val)
  }

  # Optional: filter by climate model (e.g. for Weather Scenarios).
  if (!is.null(model_val)) {
    query <- query |> dplyr::filter(model %in% !!model_val)
  }

  # For seasonal/monthly modes, also filter by the active season/month.
  if (temporal_mode %in% c("Winter", "Spring", "Summer", "Autumn")) {
    query <- query |> dplyr::filter(Season == !!temporal_mode)
  } else if (temporal_mode != "Annual") {
    month_int <- as.integer(temporal_mode)
    query <- query |> dplyr::filter(Month == !!month_int)
  }

  # Optional: trim to only the columns the caller actually needs.
  # Doing this BEFORE collect() is critical for performance because it
  # tells Arrow's Parquet reader to only load those specific columns from disk.
  # NOTE: To prevent Arrow's lazy evaluation from breaking partition pruning,
  # we must ensure that any partition columns used in filter() are preserved in select().
  if (!is.null(select_cols)) {
    safe_select <- unique(c(select_cols, "variable", "SpatialLevel", "Year", "Season", "Month", "Region", "scenario", "model"))
    query <- query |> dplyr::select(dplyr::any_of(safe_select))
  }

  # Execute the query and pull the results into a local data.frame.
  # Arrow's partition pruning means only the relevant parquet fragments
  # are read from disk — this is fast even for large datasets.
  # Always convert to plain data.frame because Arrow's collect() returns
  # a data.table in this renv, and data.table's [,j] syntax is incompatible
  # with the base R column subsetting used throughout server.R.
  df_result <- as.data.frame(dplyr::collect(query))

  # Aggregate Southern Norway hydro sub-regions (NOS1, NOS2, NOS3) into single NOS0
  if (is_nos0_hydro && nrow(df_result) > 0) {
    group_cols <- setdiff(names(df_result), c("Region", "Value"))
    if (length(group_cols) > 0) {
      df_result <- df_result |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(Value = if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(Region = "NOS0")
    } else {
      df_result <- df_result |>
        dplyr::summarise(Value = if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(Region = "NOS0")
    }
  }

  # Trim the final result to exactly what the caller requested.
  # We included extra partition columns above to ensure Arrow partition pruning worked.
  if (!is.null(select_cols)) {
    available_cols <- intersect(select_cols, names(df_result))
    df_result <- df_result[, available_cols, drop = FALSE]
  }

  # Return NULL instead of an empty data.frame for cleaner downstream
  # handling. Most callers check `if (is.null(...)) return(NULL)` which
  # is more readable than `if (nrow(...) == 0)`.
  if (nrow(df_result) == 0) return(NULL)

  # Explicitly sort chronologically by Year to prevent zig-zag rendering in Plotly
  if ("Year" %in% names(df_result)) {
    df_result <- df_result[order(df_result$Year), , drop = FALSE]
  }

  df_result
}
