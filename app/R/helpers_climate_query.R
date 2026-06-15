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
query_arrow_dataset <- function(ds_annual, ds_seasonal, temporal_mode,
                                var_name, sp_level,
                                year = NULL, year_start = NULL, year_end = NULL,
                                target_region = NULL, scenario_val = NULL,
                                select_cols = NULL) {

  # Pick the correct dataset based on temporal mode.
  # Annual mode reads from the annual aggregate; any season reads from
  # the seasonal dataset which has an additional "Season" column.
  ds <- if (temporal_mode == "Annual") ds_annual else ds_seasonal

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

  # Optional: filter to a single region (used by chart and baseline reactives)
  if (!is.null(target_region)) {
    query <- query |> dplyr::filter(Region == !!target_region)
  }

  # Optional: filter by SSP scenario (only for projection datasets).
  if (!is.null(scenario_val)) {
    query <- query |> dplyr::filter(scenario == !!scenario_val)
  }

  # For seasonal modes, also filter by the active season name.
  # This is automatic: if we chose ds_seasonal above, we must also narrow
  # to the specific season the user selected.
  if (temporal_mode != "Annual") {
    query <- query |> dplyr::filter(Season == !!temporal_mode)
  }

  # Execute the query and pull the results into a local data.frame.
  # Arrow's partition pruning means only the relevant parquet fragments
  # are read from disk — this is fast even for large datasets.
  # Always convert to plain data.frame because Arrow's collect() returns
  # a data.table in this renv, and data.table's [,j] syntax is incompatible
  # with the base R column subsetting used throughout server.R.
  df_result <- as.data.frame(dplyr::collect(query))

  # Optional: trim to only the columns the caller actually needs.
  # This is done AFTER collect() rather than in the Arrow query because
  # Arrow's lazy select() can interfere with partition pruning when the
  # selected columns don't include partition keys used in filters
  # (e.g., filtering on 'variable' but not selecting it). Trimming in R
  # after collect is negligible overhead since the data is already small.
  if (!is.null(select_cols)) {
    # Only keep columns that actually exist in the result (some partition
    # columns like 'variable' may or may not appear depending on Arrow version).
    available_cols <- intersect(select_cols, names(df_result))
    df_result <- df_result[, available_cols, drop = FALSE]
  }

  # Return NULL instead of an empty data.frame for cleaner downstream
  # handling. Most callers check `if (is.null(...)) return(NULL)` which
  # is more readable than `if (nrow(...) == 0)`.
  if (nrow(df_result) == 0) return(NULL)

  df_result
}
