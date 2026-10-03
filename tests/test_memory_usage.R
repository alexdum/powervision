# test_memory_usage.R
# Script to rigorously measure memory usage (RAM / VmRSS) of the PowerVision app

get_mem_stats <- function(label = "") {
  # Read Linux /proc/self/status for accurate OS-level memory metrics
  status_lines <- readLines("/proc/self/status")
  vm_rss_line <- grep("^VmRSS:", status_lines, value = TRUE)
  vm_hwm_line <- grep("^VmHWM:", status_lines, value = TRUE)
  vm_size_line <- grep("^VmSize:", status_lines, value = TRUE)
  
  vm_rss_kb <- as.numeric(gsub("[^0-9]", "", vm_rss_line))
  vm_hwm_kb <- as.numeric(gsub("[^0-9]", "", vm_hwm_line))
  vm_size_kb <- as.numeric(gsub("[^0-9]", "", vm_size_line))
  
  # R internal garbage collector memory (used RAM)
  gc_res <- gc(verbose = FALSE)
  r_mem_mb <- sum(gc_res[, 2]) # Vcells + Ncells in MB (column 2 is '(Mb)')
  
  rss_mb <- round(vm_rss_kb / 1024, 1)
  hwm_mb <- round(vm_hwm_kb / 1024, 1)
  
  cat(sprintf("[%-35s] VmRSS (OS RAM): %6.1f MB | Peak (HWM): %6.1f MB | R heap: %5.1f MB\n",
              label, rss_mb, hwm_mb, r_mem_mb))
  
  invisible(list(rss_mb = rss_mb, hwm_mb = hwm_mb, r_mem_mb = r_mem_mb))
}

cat("================================================================================\n")
cat("          POWERVISION SHINY APP MEMORY USAGE (RAM) BENCHMARK\n")
cat("================================================================================\n\n")

# 1. Baseline
get_mem_stats("1. Baseline R + renv")

# 2. Individual library loads
library(shiny)
get_mem_stats("2a. After library(shiny)")

library(mapgl)
get_mem_stats("2b. After library(mapgl)")

library(sf)
get_mem_stats("2c. After library(sf)")

library(arrow)
get_mem_stats("2d. After library(arrow)")

library(plotly)
get_mem_stats("2e. After library(plotly)")

library(dplyr)
library(bslib)
library(bsicons)
library(markdown)
get_mem_stats("2f. After all libraries loaded")

# 3. Source global.R
cat("\n--- Loading global.R ---\n")
source("global.R")
get_mem_stats("3. After source('global.R')")

# Inspect sizes of global objects
cat("\n--- Breakdown of major global objects in memory ---\n")
objs <- ls(envir = .GlobalEnv)
sizes <- sapply(objs, function(x) object.size(get(x, envir = .GlobalEnv)))
sizes_mb <- sort(round(sizes / (1024 * 1024), 2), decreasing = TRUE)
print(head(sizes_mb[sizes_mb > 0.1], 15))

if (exists("spatial_boundary_cache")) {
  cat("\nSizes of individual spatial boundaries in cache:\n")
  for (nm in names(spatial_boundary_cache)) {
    sz <- round(as.numeric(object.size(spatial_boundary_cache[[nm]])) / (1024 * 1024), 2)
    cat(sprintf("  - %-6s: %5.2f MB (%d features)\n", nm, sz, nrow(spatial_boundary_cache[[nm]])))
  }
}

# 4. Source helpers
cat("\n--- Loading helper scripts ---\n")
source("R/helpers_climate_query.R")
source("R/helpers_wind_blend.R")
source("R/helpers_ws_query.R")
source("R/helpers_chart.R")
source("R/helpers_map.R")
source("R/helpers_legend.R")
source("R/helpers_ui_drawer.R")
source("R/helpers_export.R")
get_mem_stats("4. After loading all helpers")

# 5. Simulate queries / user actions
cat("\n--- Simulating App Operations & Queries ---\n")

# Query 1: Map Choropleth - Temperature Historical 2020 (NUT2 - 334 regions)
df_hist_map <- query_arrow_dataset(
  hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, "Annual",
  "2m_temperature", "nuts_2", year = 2020,
  select_cols = c("Region", "Value")
)
get_mem_stats("5a. After map query (temp, NUT2)")

# Query 2: Map Choropleth - Wind Onshore Blend 2025 across all P2ON regions
df_wind_blend <- blend_wind_power_all_regions(
  tech_mix_mode = "dynamic",
  wind_type = "onshore",
  ds_annual = proj_annual_ds,
  ds_seasonal = proj_seasonal_ds,
  ds_monthly = proj_monthly_ds,
  temporal_mode = "Annual",
  sp_level = "p2on",
  year = 2025,
  scenario_val = "ssp2_4_5"
)
get_mem_stats("5b. After wind blend query (P2ON)")

# Query 3: Time Series query for a clicked region (FR10 - all years)
df_ts <- query_arrow_dataset(
  hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, "Annual",
  "2m_temperature", "nuts_2", target_region = "FR10",
  select_cols = c("Year", "Value")
)
get_mem_stats("5c. After time series query (FR10)")

# Query 4: Weather scenarios daily query
if (!is.null(ws_daily_ds)) {
  df_ws <- query_ws_daily(
    var_name = "2m_temperature",
    sp_level = "nuts_2",
    target_region = "FR10"
  )
  get_mem_stats("5d. After WS daily query")
}

# 6. Stress test: Simulate active user session (20 sequential map and chart queries)
cat("\n--- Simulating 20 Sequential User Interactivity Steps ---\n")
vars <- c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards", "10m_wind_speed")
tiers <- c("nuts_0", "nuts_2", "p2on", "szon")
years <- c(1990, 2000, 2010, 2020, 2030, 2050)

for (i in 1:20) {
  v <- vars[(i %% length(vars)) + 1]
  t <- tiers[(i %% length(tiers)) + 1]
  y <- years[(i %% length(years)) + 1]
  
  if (y <= 2020) {
    res <- query_arrow_dataset(
      hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, "Annual",
      v, t, year = y, select_cols = c("Region", "Value")
    )
  } else {
    res <- query_arrow_dataset(
      proj_annual_ds, proj_seasonal_ds, proj_monthly_ds, "Annual",
      v, t, year = y, scenario_val = "ssp2_4_5", select_cols = c("Region", "Value")
    )
  }
}
get_mem_stats("6. After 20 user queries")

# 7. Post Garbage Collection
cat("\n--- Calling gc() ---\n")
gc(verbose = FALSE)
get_mem_stats("7. After gc()")

cat("\n================================================================================\n")
cat("                              CONCLUSION\n")
cat("================================================================================\n")
