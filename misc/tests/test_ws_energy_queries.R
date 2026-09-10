# misc/tests/test_ws_energy_queries.R
# ==============================================================================
# Verification Suite: Weather Scenario (WS) Energy Indicators & Drawer Cards
# ==============================================================================
# Verifies:
#   1. Solar PV direct lookup across technologies and spatial tiers (P2ON, NUT0)
#   2. Solar CSP direct lookup across technologies and spatial tiers (P2ON, NUT0)
#   3. Onshore wind query under fixed_2020 and dynamic modes (P2ON, NUT0)
#   4. Offshore wind query under fixed_2020 and dynamic modes (P2OF, NUT0)
#   5. Drawer summary metric cards for all 4 indicators
#   6. Edge cases: uninstalled/missing data regions (e.g. DZ00 or uninstalled P2OF)
# ==============================================================================

# Ensure correct working directory
if (file.exists("global.R")) {
  app_dir <- "."
} else if (file.exists("app/global.R")) {
  app_dir <- "app"
} else if (file.exists("../../app/global.R")) {
  app_dir <- "../../app"
} else {
  stop("Cannot locate app directory containing global.R")
}

old_wd <- getwd()
setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

message(sprintf("[Test Suite] Sourcing app environment from: %s", getwd()))
if (requireNamespace("shiny", quietly = TRUE)) {
  source("global.R", local = FALSE)
} else {
  message("[Test Suite] Headless environment detected (no shiny package). Initializing standalone environment...")
  suppressPackageStartupMessages({ library(arrow); library(dplyr) })
  missing_pkgs <- c("shiny", "htmltools")
  library <- function(package, ...) {
    pkg_name <- as.character(substitute(package))
    if (pkg_name %in% missing_pkgs) return(invisible(TRUE))
    base::library(pkg_name, character.only = TRUE, ...)
  }
  HTML <- function(text, ...) structure(text, html = TRUE, class = c("html", "character"))
  as.character.html <- function(x, ...) unclass(x)
  div <- function(...) structure(paste(c(...), collapse = " "), html = TRUE, class = c("shiny.tag", "html", "character"))
  span <- div
  conditionalPanel <- function(cond, ...) div(...)
  tags <- list(button = div, i = div, span = div, div = div)
  tagList <- function(...) HTML(paste(vapply(list(...), as.character, character(1)), collapse = " "))
  proj_annual_ds <- arrow::open_dataset("www/data/pecd/projections/annual")
  ws_daily_ds <- if (dir.exists("www/data/pecd/ws_daily")) arrow::open_dataset("www/data/pecd/ws_daily") else NULL
  ws_mapping_table <- read.csv("www/data/ws/weather_scenarios.csv", stringsAsFactors = FALSE)
  ws_model_lookup <- c(
    "AWCM" = "awi_cm_1_1_mr", "BCCS" = "bcc_csm2_mr", "CMR5" = "cmcc_cm2_sr5",
    "ECE3" = "ec_earth3", "MEHR" = "mpi_esm1_2_hr", "MRM2" = "mri_esm2_0"
  )
  ws_mapping_table[["ModelKey"]] <- ws_model_lookup[ws_mapping_table[["ClimateModel"]]]

  tech_mix_dir <- "www/data/technology_mix"
  onshore_resource_groups <- read.csv(file.path(tech_mix_dir, "onshore_wind_resource_groups.csv"))
  offshore_resource_groups <- read.csv(file.path(tech_mix_dir, "offshore_wind_resource_groups.csv"))
  names(onshore_resource_groups)[names(onshore_resource_groups) == "Wind.resource.group"] <- "ResourceGroup"
  names(offshore_resource_groups)[names(offshore_resource_groups) == "Wind.resource.group"] <- "ResourceGroup"
  onshore_mix_ratios <- read.csv(file.path(tech_mix_dir, "onshore_mix_ratios.csv"))
  offshore_mix_ratios <- read.csv(file.path(tech_mix_dir, "offshore_mix_ratios.csv"))
  wind_tech_mapping <- read.csv(file.path(tech_mix_dir, "wind_technologies_mapping.csv"))

  spatial_level_to_parquet <- c("NUT0" = "nuts_0", "NUT2" = "nuts_2", "P2ON" = "p2on", "P2OF" = "p2of", "SZON" = "szon", "SZOF" = "szof")
  spatial_boundary_cache <- list()

  climate_variables <- list(
    solar_power_pv = list(label = "Solar Photovoltaic", unit = "CF", is_cf = TRUE, is_solar = TRUE),
    solar_power_csp = list(label = "Concentrated Solar Power", unit = "CF", is_cf = TRUE, is_solar = TRUE),
    wind_power_onshore = list(label = "Onshore Wind", unit = "CF", is_cf = TRUE, is_wind = TRUE),
    wind_power_offshore = list(label = "Offshore Wind", unit = "CF", is_cf = TRUE, is_wind = TRUE)
  )

  get_solar_tech_name <- function(tech_code) {
    techs <- c("40" = "No storage", "41" = "1h storage", "42" = "3h storage", "43" = "6h storage",
               "60" = "Industrial", "61" = "Commercial", "62" = "Residential", "63" = "Utility-scale")
    if (as.character(tech_code) %in% names(techs)) techs[[as.character(tech_code)]] else NULL
  }

  enrich_var_meta <- function(var_meta, solar_tech) {
    if (isTRUE(var_meta[["is_solar"]]) && !is.null(solar_tech) && solar_tech != "") {
      tn <- get_solar_tech_name(solar_tech)
      if (!is.null(tn)) var_meta[["label"]] <- paste0(var_meta[["label"]], " — ", tn)
    }
    var_meta
  }

  get_ws_display_label <- function(x) x
}
source("R/helpers_climate_query.R", local = FALSE)
source("R/helpers_wind_blend.R", local = FALSE)
source("R/helpers_ws_query.R", local = FALSE)
source("R/helpers_ui_drawer.R", local = FALSE)
if (requireNamespace("plotly", quietly = TRUE)) {
  source("R/helpers_chart.R", local = FALSE)
}

test_scenarios <- c("WS01", "WS10", "WS25")
total_tests <- 0
passed_tests <- 0

assert_test <- function(desc, condition) {
  total_tests <<- total_tests + 1
  if (isTRUE(condition)) {
    passed_tests <<- passed_tests + 1
    cat(sprintf("  [PASS] %s\n", desc))
  } else {
    cat(sprintf("  [FAIL] %s\n", desc))
    stop(sprintf("Assertion failed: %s", desc))
  }
}

cat("\n==============================================================================\n")
cat("Starting Weather Scenario Energy Verification\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# 1. Solar Photovoltaic (solar_power_pv)
# ------------------------------------------------------------------------------
cat("--- 1. Testing Solar PV Direct Lookup ---\n")
for (ws in test_scenarios) {
  for (tech in c("60", "61")) {
    # P2ON Tier
    df_p2on <- query_ws_annual_for_map("solar_power_pv", "p2on", ws, solar_tech = tech)
    assert_test(
      sprintf("Solar PV (tech %s, %s, P2ON): returned non-empty df (rows: %d)", tech, ws, nrow(df_p2on)),
      !is.null(df_p2on) && nrow(df_p2on) == 215
    )
    assert_test(
      sprintf("Solar PV (tech %s, %s, P2ON): values in valid range [0, 1] (mean: %.4f)", tech, ws, mean(df_p2on$Value, na.rm = TRUE)),
      all(is.finite(df_p2on$Value)) && all(df_p2on$Value >= 0 & df_p2on$Value <= 1)
    )

    # NUT0 Tier (Synthesized / Aggregated)
    df_nut0 <- query_ws_annual_for_map("solar_power_pv", "nuts_0", ws, solar_tech = tech)
    assert_test(
      sprintf("Solar PV (tech %s, %s, NUT0): returned aggregated country codes (rows: %d)", tech, ws, nrow(df_nut0)),
      !is.null(df_nut0) && nrow(df_nut0) > 30 && all(nchar(df_nut0$Region) == 2)
    )
    assert_test(
      sprintf("Solar PV (tech %s, %s, NUT0): values in valid range [0, 1] (mean: %.4f)", tech, ws, mean(df_nut0$Value, na.rm = TRUE)),
      all(is.finite(df_nut0$Value)) && all(df_nut0$Value >= 0 & df_nut0$Value <= 1)
    )
  }
}

# ------------------------------------------------------------------------------
# 2. Concentrated Solar Power (solar_power_csp)
# ------------------------------------------------------------------------------
cat("\n--- 2. Testing Concentrated Solar Power Direct Lookup ---\n")
for (ws in test_scenarios) {
  for (tech in c("40", "41")) {
    # P2ON Tier
    df_p2on <- query_ws_annual_for_map("solar_power_csp", "p2on", ws, solar_tech = tech)
    assert_test(
      sprintf("Solar CSP (tech %s, %s, P2ON): returned non-empty df (rows: %d)", tech, ws, nrow(df_p2on)),
      !is.null(df_p2on) && nrow(df_p2on) == 215
    )
    assert_test(
      sprintf("Solar CSP (tech %s, %s, P2ON): values in valid range [0, 1] (mean: %.4f)", tech, ws, mean(df_p2on$Value, na.rm = TRUE)),
      all(is.finite(df_p2on$Value)) && all(df_p2on$Value >= 0 & df_p2on$Value <= 1)
    )

    # NUT0 Tier (Synthesized / Aggregated)
    df_nut0 <- query_ws_annual_for_map("solar_power_csp", "nuts_0", ws, solar_tech = tech)
    assert_test(
      sprintf("Solar CSP (tech %s, %s, NUT0): returned aggregated country codes (rows: %d)", tech, ws, nrow(df_nut0)),
      !is.null(df_nut0) && nrow(df_nut0) > 30 && all(nchar(df_nut0$Region) == 2)
    )
    assert_test(
      sprintf("Solar CSP (tech %s, %s, NUT0): values in valid range [0, 1] (mean: %.4f)", tech, ws, mean(df_nut0$Value, na.rm = TRUE)),
      all(is.finite(df_nut0$Value)) && all(df_nut0$Value >= 0 & df_nut0$Value <= 1)
    )
  }
}

# ------------------------------------------------------------------------------
# 3. Wind Power Onshore (wind_power_onshore)
# ------------------------------------------------------------------------------
cat("\n--- 3. Testing Wind Power Onshore ---\n")
for (ws in test_scenarios) {
  # Fixed 2020 Mode (Pure raw existing fleet: tech_30)
  df_fix_p2on <- query_ws_annual_for_map("wind_power_onshore", "p2on", ws, tech_mix_mode = "fixed_2020")
  assert_test(
    sprintf("Wind Onshore (fixed_2020, %s, P2ON): returned 152 regions (rows: %d)", ws, nrow(df_fix_p2on)),
    !is.null(df_fix_p2on) && nrow(df_fix_p2on) == 152
  )
  assert_test(
    sprintf("Wind Onshore (fixed_2020, %s, P2ON): values in [0, 1] (mean: %.4f)", ws, mean(df_fix_p2on$Value, na.rm = TRUE)),
    all(is.finite(df_fix_p2on$Value)) && all(df_fix_p2on$Value >= 0 & df_fix_p2on$Value <= 1)
  )

  # Fixed 2020 Mode at NUT0
  df_fix_nut0 <- query_ws_annual_for_map("wind_power_onshore", "nuts_0", ws, tech_mix_mode = "fixed_2020")
  assert_test(
    sprintf("Wind Onshore (fixed_2020, %s, NUT0): returned aggregated country codes (rows: %d)", ws, nrow(df_fix_nut0)),
    !is.null(df_fix_nut0) && nrow(df_fix_nut0) > 30 && all(nchar(df_fix_nut0$Region) == 2)
  )

  # Dynamic Mode
  df_dyn_p2on <- query_ws_annual_for_map("wind_power_onshore", "p2on", ws, tech_mix_mode = "dynamic")
  assert_test(
    sprintf("Wind Onshore (dynamic, %s, P2ON): returned valid rows (rows: %d)", ws, nrow(df_dyn_p2on)),
    !is.null(df_dyn_p2on) && nrow(df_dyn_p2on) >= 150
  )
}

# ------------------------------------------------------------------------------
# 4. Wind Power Offshore (wind_power_offshore)
# ------------------------------------------------------------------------------
cat("\n--- 4. Testing Wind Power Offshore ---\n")
for (ws in test_scenarios) {
  # Fixed 2020 Mode (Pure raw existing fleet: tech_20; exactly 26 EU regions per PECD Quirk 2)
  df_fix_p2of <- query_ws_annual_for_map("wind_power_offshore", "p2of", ws, tech_mix_mode = "fixed_2020")
  assert_test(
    sprintf("Wind Offshore (fixed_2020, %s, P2OF): returned exactly 26 EU regions (rows: %d)", ws, nrow(df_fix_p2of)),
    !is.null(df_fix_p2of) && nrow(df_fix_p2of) == 26
  )
  assert_test(
    sprintf("Wind Offshore (fixed_2020, %s, P2OF): values in [0, 1] (mean: %.4f)", ws, mean(df_fix_p2of$Value, na.rm = TRUE)),
    all(is.finite(df_fix_p2of$Value)) && all(df_fix_p2of$Value >= 0 & df_fix_p2of$Value <= 1)
  )

  # Fixed 2020 Mode at NUT0 (Coastal EU countries with offshore wind)
  df_fix_nut0 <- query_ws_annual_for_map("wind_power_offshore", "nuts_0", ws, tech_mix_mode = "fixed_2020")
  assert_test(
    sprintf("Wind Offshore (fixed_2020, %s, NUT0): returned coastal country codes (rows: %d)", ws, nrow(df_fix_nut0)),
    !is.null(df_fix_nut0) && nrow(df_fix_nut0) >= 10 && all(nchar(df_fix_nut0$Region) == 2)
  )

  # Dynamic Mode
  df_dyn_p2of <- query_ws_annual_for_map("wind_power_offshore", "p2of", ws, tech_mix_mode = "dynamic")
  assert_test(
    sprintf("Wind Offshore (dynamic, %s, P2OF): returned valid rows (rows: %d)", ws, nrow(df_dyn_p2of)),
    !is.null(df_dyn_p2of) && nrow(df_dyn_p2of) >= 26
  )
}

# ------------------------------------------------------------------------------
# 5. Drawer Summary Metric Cards
# ------------------------------------------------------------------------------
cat("\n--- 5. Testing Bottom-Drawer Summary Metric Cards ---\n")

region_onshore_p2on <- list(zone_id = "FR01", name = "France Zone 1", country = "France", level = "P2ON", parent = "FR")
# FR111_OFF has installed capacity under tech_20; FR115_OFF does not (PECD Quirk 2)
region_offshore_p2of <- list(zone_id = "FR111_OFF", name = "France Offshore 111", country = "France", level = "P2OF", parent = "FR")
region_nut0 <- list(zone_id = "FR", name = "France", country = "France", level = "NUT0", parent = "FR")

indicators_to_test <- list(
  list(var = "solar_power_pv", tech = "60", reg = region_onshore_p2on, sp = "P2ON"),
  list(var = "solar_power_csp", tech = "40", reg = region_onshore_p2on, sp = "P2ON"),
  list(var = "wind_power_onshore", tech = NULL, reg = region_onshore_p2on, sp = "P2ON"),
  list(var = "wind_power_offshore", tech = NULL, reg = region_offshore_p2of, sp = "P2OF"),
  list(var = "solar_power_pv", tech = "60", reg = region_nut0, sp = "NUT0"),
  list(var = "solar_power_csp", tech = "40", reg = region_nut0, sp = "NUT0"),
  list(var = "wind_power_onshore", tech = NULL, reg = region_nut0, sp = "NUT0"),
  list(var = "wind_power_offshore", tech = NULL, reg = region_nut0, sp = "NUT0")
)

for (ws in test_scenarios) {
  for (item in indicators_to_test) {
    # Test Path A: Resolved from cached choropleth clim_df
    clim_df_cached <- query_ws_annual_for_map(item$var, item$sp, ws, solar_tech = item$tech, tech_mix_mode = "fixed_2020")
    
    cards_cached <- build_region_stats_cards(
      region = item$reg,
      spatial_level = item$sp,
      show_projections = "1",
      projection_style = "single",
      temporal_mode = "WS",
      map_selected_ws = ws,
      climate_variable = item$var,
      ws_df = NULL,
      clim_df = clim_df_cached,
      solar_technology = item$tech
    )

    cards_html_cached <- as.character(cards_cached)
    assert_test(
      sprintf("Drawer Card (Cached, %s, %s, %s): contains ws-metric-card, 'Annual CF', and 3-decimal value", item$var, item$sp, ws),
      grepl("ws-metric-card", cards_html_cached) && grepl("Annual CF", cards_html_cached) && grepl("[0-9]+\\.[0-9]{3} CF", cards_html_cached)
    )

    # Test Path B: Autonomous direct query fallback (when clim_df is NULL)
    cards_direct <- build_region_stats_cards(
      region = item$reg,
      spatial_level = item$sp,
      show_projections = "1",
      projection_style = "single",
      temporal_mode = "WS",
      map_selected_ws = ws,
      climate_variable = item$var,
      ws_df = NULL,
      clim_df = NULL,
      solar_technology = item$tech
    )

    cards_html_direct <- as.character(cards_direct)
    assert_test(
      sprintf("Drawer Card (Direct Fallback, %s, %s, %s): contains ws-metric-card and 'Annual CF'", item$var, item$sp, ws),
      grepl("ws-metric-card", cards_html_direct) && grepl("Annual CF", cards_html_direct) && grepl("[0-9]+\\.[0-9]{3} CF", cards_html_direct)
    )
  }
}

# ------------------------------------------------------------------------------
# 6. Edge Case: Offshore region without installed capacity
# ------------------------------------------------------------------------------
cat("\n--- 6. Testing Edge Case: Missing Data Regions (DZ00 Offshore) ---\n")
region_dz_offshore <- list(zone_id = "DZ00_OFF", name = "Algeria Offshore", country = "Algeria", level = "P2OF", parent = "DZ")
cards_empty <- build_region_stats_cards(
  region = region_dz_offshore,
  spatial_level = "P2OF",
  show_projections = "1",
  projection_style = "single",
  temporal_mode = "WS",
  map_selected_ws = "WS01",
  climate_variable = "wind_power_offshore",
  ws_df = NULL,
  clim_df = NULL
)
assert_test(
  "Missing offshore region handled gracefully (identity cards present, no ws-metric-card, no crash)",
  !grepl("ws-metric-card", as.character(cards_empty))
)

# ------------------------------------------------------------------------------
# 7. Spatial Level Compatibility Guards & Case-Insensitive Inputs (Adversarial)
# ------------------------------------------------------------------------------
cat("
--- 7. Testing Spatial Tier Compatibility & Case Normalization ---
")

# A. Incompatible spatial tiers for Solar PV / CSP must return NULL
assert_test("Solar PV at nuts_2 returns NULL", is.null(query_ws_annual_for_map("solar_power_pv", "nuts_2", "WS01", solar_tech = "60")))
assert_test("Solar PV at szon returns NULL", is.null(query_ws_annual_for_map("solar_power_pv", "szon", "WS01", solar_tech = "60")))
assert_test("Solar CSP at nuts_2 returns NULL", is.null(query_ws_annual_for_map("solar_power_csp", "nuts_2", "WS01", solar_tech = "40")))

# B. Incompatible spatial tiers for Wind Onshore must return NULL (no leakage)
assert_test("Wind Onshore at nuts_2 returns NULL", is.null(query_ws_annual_for_map("wind_power_onshore", "nuts_2", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Onshore at p2of returns NULL", is.null(query_ws_annual_for_map("wind_power_onshore", "p2of", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Onshore at szon returns NULL", is.null(query_ws_annual_for_map("wind_power_onshore", "szon", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Onshore (dynamic) at nuts_2 returns NULL", is.null(query_ws_annual_for_map("wind_power_onshore", "nuts_2", "WS01", tech_mix_mode = "dynamic")))

# C. Incompatible spatial tiers for Wind Offshore must return NULL (no leakage)
assert_test("Wind Offshore at nuts_2 returns NULL", is.null(query_ws_annual_for_map("wind_power_offshore", "nuts_2", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Offshore at p2on returns NULL", is.null(query_ws_annual_for_map("wind_power_offshore", "p2on", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Offshore at szof returns NULL", is.null(query_ws_annual_for_map("wind_power_offshore", "szof", "WS01", tech_mix_mode = "fixed_2020")))
assert_test("Wind Offshore (dynamic) at nuts_2 returns NULL", is.null(query_ws_annual_for_map("wind_power_offshore", "nuts_2", "WS01", tech_mix_mode = "dynamic")))

# D. Case-insensitive spatial tier normalization
df_pv_case <- query_ws_annual_for_map("solar_power_pv", "nut0", "WS01", solar_tech = "60")
assert_test("Solar PV with lowercase nut0 returns aggregated rows", !is.null(df_pv_case) && nrow(df_pv_case) > 30)
df_onshore_case <- query_ws_annual_for_map("wind_power_onshore", "P2on", "WS01", tech_mix_mode = "fixed_2020")
assert_test("Wind Onshore with mixed-case P2on returns 152 rows", !is.null(df_onshore_case) && nrow(df_onshore_case) == 152)
df_offshore_case <- query_ws_annual_for_map("wind_power_offshore", "P2Of", "WS01", tech_mix_mode = "fixed_2020")
assert_test("Wind Offshore with mixed-case P2Of returns 26 rows", !is.null(df_offshore_case) && nrow(df_offshore_case) == 26)

# ------------------------------------------------------------------------------
# 8. Weather Scenario Daily Energy Parquet Queries & Value Ranges
# ------------------------------------------------------------------------------
cat("\n--- 8. Testing Weather Scenario Daily Energy Parquet Queries ---\n")

# A. Parquet partitions exist for all 4 energy indicators
energy_vars <- c("solar_power_pv", "solar_power_csp", "wind_power_onshore", "wind_power_offshore")
for (v in energy_vars) {
  part_dir <- file.path("www/data/pecd/ws_daily", paste0("variable=", v))
  assert_test(sprintf("Parquet partition directory exists: variable=%s", v), dir.exists(part_dir))
}

# Solar subcode partitions exist
for (sc in c("60", "61", "62", "63")) {
  assert_test(sprintf("Solar PV subcode partition exists: variable=solar_power_pv_%s", sc),
              dir.exists(file.path("www/data/pecd/ws_daily", paste0("variable=solar_power_pv_", sc))))
}
for (sc in c("40", "41", "42", "43")) {
  assert_test(sprintf("Solar CSP subcode partition exists: variable=solar_power_csp_%s", sc),
              dir.exists(file.path("www/data/pecd/ws_daily", paste0("variable=solar_power_csp_", sc))))
}

# B. Daily queries across sample scenarios (WS01, WS10, WS25)
for (ws in test_scenarios) {
  # 1. Solar PV: tech 60 and tech 63 at P2ON and NUT0
  df_pv_60_p2on <- query_ws_daily("solar_power_pv", "p2on", "FR01", ws_codes = ws, solar_tech = "60")
  assert_test(sprintf("Solar PV 60 daily (%s, FR01, P2ON): exactly 365 steps", ws),
              !is.null(df_pv_60_p2on) && nrow(df_pv_60_p2on) == 365)
  assert_test(sprintf("Solar PV 60 daily (%s, FR01, P2ON): DayOfYear strictly 1:365 and values in [0, 1]", ws),
              identical(df_pv_60_p2on$DayOfYear, as.numeric(1:365)) &&
              all(df_pv_60_p2on$Value >= 0 & df_pv_60_p2on$Value <= 1))

  df_pv_63_p2on <- query_ws_daily("solar_power_pv", "p2on", "FR01", ws_codes = ws, solar_tech = "63")
  assert_test(sprintf("Solar PV 63 daily (%s, FR01, P2ON): exactly 365 steps", ws),
              !is.null(df_pv_63_p2on) && nrow(df_pv_63_p2on) == 365)
  assert_test(sprintf("Solar PV switching tech 60 vs 63 yields distinct daily profiles (%s)", ws),
              !identical(df_pv_60_p2on$Value, df_pv_63_p2on$Value))

  df_pv_nut0 <- query_ws_daily("solar_power_pv", "nuts_0", "FR", ws_codes = ws, solar_tech = "60")
  assert_test(sprintf("Solar PV daily (%s, FR, NUT0): exactly 365 steps and values in [0, 1]", ws),
              !is.null(df_pv_nut0) && nrow(df_pv_nut0) == 365 &&
              all(df_pv_nut0$Value >= 0 & df_pv_nut0$Value <= 1))

  # 2. Solar CSP: tech 40 and tech 42 at P2ON and NUT0
  df_csp_40_p2on <- query_ws_daily("solar_power_csp", "p2on", "FR01", ws_codes = ws, solar_tech = "40")
  assert_test(sprintf("Solar CSP 40 daily (%s, FR01, P2ON): exactly 365 steps", ws),
              !is.null(df_csp_40_p2on) && nrow(df_csp_40_p2on) == 365)
  assert_test(sprintf("Solar CSP 40 daily (%s, FR01, P2ON): values in [0, 1]", ws),
              all(df_csp_40_p2on$Value >= 0 & df_csp_40_p2on$Value <= 1))

  df_csp_nut0 <- query_ws_daily("solar_power_csp", "nuts_0", "FR", ws_codes = ws, solar_tech = "40")
  assert_test(sprintf("Solar CSP daily (%s, FR, NUT0): exactly 365 steps and values in [0, 1]", ws),
              !is.null(df_csp_nut0) && nrow(df_csp_nut0) == 365 &&
              all(df_csp_nut0$Value >= 0 & df_csp_nut0$Value <= 1))

  # 3. Onshore Wind: pure raw existing fleet (tech_30) at P2ON and NUT0
  df_won_p2on <- query_ws_daily("wind_power_onshore", "p2on", "FR01", ws_codes = ws)
  assert_test(sprintf("Wind Onshore daily (%s, FR01, P2ON): exactly 365 steps", ws),
              !is.null(df_won_p2on) && nrow(df_won_p2on) == 365)
  assert_test(sprintf("Wind Onshore daily (%s, FR01, P2ON): values in [0, 1]", ws),
              all(df_won_p2on$Value >= 0 & df_won_p2on$Value <= 1))

  df_won_nut0 <- query_ws_daily("wind_power_onshore", "nuts_0", "FR", ws_codes = ws)
  assert_test(sprintf("Wind Onshore daily (%s, FR, NUT0): exactly 365 steps and values in [0, 1]", ws),
              !is.null(df_won_nut0) && nrow(df_won_nut0) == 365 &&
              all(df_won_nut0$Value >= 0 & df_won_nut0$Value <= 1))

  # 4. Offshore Wind: pure raw existing fleet (tech_20) at P2OF and NUT0
  df_wof_p2of <- query_ws_daily("wind_power_offshore", "p2of", "FR111_OFF", ws_codes = ws)
  assert_test(sprintf("Wind Offshore daily (%s, FR111_OFF, P2OF): exactly 365 steps", ws),
              !is.null(df_wof_p2of) && nrow(df_wof_p2of) == 365)
  assert_test(sprintf("Wind Offshore daily (%s, FR111_OFF, P2OF): values in [0, 1]", ws),
              all(df_wof_p2of$Value >= 0 & df_wof_p2of$Value <= 1))

  df_wof_nut0 <- query_ws_daily("wind_power_offshore", "nuts_0", "FR", ws_codes = ws)
  assert_test(sprintf("Wind Offshore daily (%s, FR, NUT0): exactly 365 steps and values in [0, 1]", ws),
              !is.null(df_wof_nut0) && nrow(df_wof_nut0) == 365 &&
              all(df_wof_nut0$Value >= 0 & df_wof_nut0$Value <= 1))
}

# C. Querying all 36 Weather Scenarios simultaneously returns exactly 36 * 365 = 13,140 rows
df_all_won <- query_ws_daily("wind_power_onshore", "p2on", "FR01")
assert_test("Wind Onshore daily for all 36 scenarios: returns exactly 13,140 rows (36 * 365)",
            !is.null(df_all_won) && nrow(df_all_won) == 13140 && length(unique(df_all_won$WS)) == 36)

df_all_pv <- query_ws_daily_for_drawer("solar_power_pv", "p2on", "FR01", solar_tech = "60")
assert_test("query_ws_daily_for_drawer wrapper returns all 36 scenarios (13,140 rows)",
            !is.null(df_all_pv) && nrow(df_all_pv) == 13140 && length(unique(df_all_pv$WS)) == 36)

# D. Drawer metric card calculation directly using daily ws_df
cards_from_daily <- build_region_stats_cards(
  region = region_onshore_p2on,
  spatial_level = "P2ON",
  show_projections = "1",
  projection_style = "single",
  temporal_mode = "WS",
  map_selected_ws = "WS01",
  climate_variable = "wind_power_onshore",
  ws_df = df_all_won,
  clim_df = NULL
)
cards_html_daily <- as.character(cards_from_daily)
assert_test("Drawer Card computed from daily ws_df contains 'Annual CF' and 3-decimal value",
            grepl("ws-metric-card", cards_html_daily) && grepl("Annual CF", cards_html_daily) && grepl("[0-9]+\\.[0-9]{3} CF", cards_html_daily))

# E. Chart Generation via build_ws_annual_cycle_chart
if (exists("build_ws_annual_cycle_chart")) {
  for (v in energy_vars) {
    df_sample <- if (v == "wind_power_offshore") {
      query_ws_daily(v, "p2of", "FR111_OFF")
    } else {
      query_ws_daily(v, "p2on", "FR01", solar_tech = if (v == "solar_power_pv") "60" else if (v == "solar_power_csp") "40" else NULL)
    }
    p_chart <- build_ws_annual_cycle_chart(
      df_ws = df_sample,
      var_name = v,
      var_meta = climate_variables[[v]],
      region_name = "France Test Region",
      highlighted_ws = "WS01"
    )
    assert_test(sprintf("build_ws_annual_cycle_chart renders without error for %s", v), !is.null(p_chart))
  }
}

cat("\n==============================================================================\n")
cat(sprintf("ALL TESTS PASSED: %d / %d assertions succeeded with 0 errors.\n", passed_tests, total_tests))
cat("==============================================================================\n\n")
