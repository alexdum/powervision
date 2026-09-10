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

  # If a solar variable is selected, append the technology number from the dropdown
  if (!is.null(solar_tech) && solar_tech != "") {
    if (var_name == "solar_power_csp") {
      var_name <- paste0("solar_concentrated_", solar_tech)
    } else if (var_name == "solar_power_pv") {
      var_name <- paste0("solar_photovoltaic_", solar_tech)
    }
  }

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
  if (!is.null(target_region)) {
    if (sp_level == "szof") {
      target_region <- sub("_OFF$", "", target_region)
    }
    query <- query |> dplyr::filter(Region == !!target_region)
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
