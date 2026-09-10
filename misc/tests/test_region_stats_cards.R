# ==============================================================================
# PowerClimate Vision Explorer — Region Stats Cards Regression Tests
# ==============================================================================
# Verifies build_region_stats_cards under varied combinations of:
#   - projection_view_mode: NULL, NA, "year", "period"
#   - show_projections: "0", "1", NULL
#   - temporal_mode: "Annual", "1", "DJF", "WS", NULL, NA, character(0)
#   - display_mode: "absolute", "anomaly"
# Prevents regression of:
#   'Error in if: missing value where TRUE/FALSE needed'
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

# Locate app directory containing global.R
if (file.exists("global.R")) {
  app_dir <- "."
} else if (file.exists("app/global.R")) {
  app_dir <- "app"
} else if (file.exists("../../app/global.R")) {
  app_dir <- "../../app"
} else if (dir.exists("/srv/shiny-server/powervision")) {
  app_dir <- "/srv/shiny-server/powervision"
} else {
  stop("Cannot locate app directory containing global.R")
}

old_wd <- getwd()
setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

cat(sprintf("=== Running Region Stats Cards Tests from: %s ===\n", getwd()))

# Source global environment and ui drawer helper
source("global.R", local = FALSE)
source("R/helpers_ui_drawer.R", local = FALSE)

# Mock test data
test_region <- list(
  name = "Romania",
  zone_id = "RO00",
  area_km2 = 238397,
  parent_zone = "RO",
  level = "NUT0"
)

test_clim_df <- data.frame(
  Region = "RO00",
  Value = 12.4,
  stringsAsFactors = FALSE
)

test_base_df <- data.frame(
  Region = "RO00",
  baseline_value = 10.8,
  stringsAsFactors = FALSE
)

test_ws_df <- data.frame(
  WS = "WS1",
  Value = 14.2,
  stringsAsFactors = FALSE
)

# Test parameter matrices required by specification
proj_view_modes <- list("NULL" = NULL, "NA" = NA, "year" = "year", "period" = "period")
show_proj_opts  <- list("0" = "0", "1" = "1", "NULL" = NULL)
temporal_modes  <- list(
  "Annual"       = "Annual",
  "1"            = "1",
  "DJF"          = "DJF",
  "WS"           = "WS",
  "NULL"         = NULL,
  "NA"           = NA,
  "character(0)" = character(0)
)

total_tests <- 0
passed_tests <- 0
failed_tests <- 0
failures <- list()

cat("\n--- Running Combinatorial Matrix Tests (4 x 3 x 7 = 84 cases) ---\n")

for (pvm_name in names(proj_view_modes)) {
  pvm_val <- proj_view_modes[[pvm_name]]
  
  for (sp_name in names(show_proj_opts)) {
    sp_val <- show_proj_opts[[sp_name]]
    
    for (tm_name in names(temporal_modes)) {
      tm_val <- temporal_modes[[tm_name]]
      
      total_tests <- total_tests + 1
      test_label <- sprintf(
        "pvm=%-6s | show_proj=%-4s | temp_mode=%-12s",
        pvm_name, sp_name, tm_name
      )
      
      err <- NULL
      res <- tryCatch({
        build_region_stats_cards(
          region = test_region,
          spatial_level = "nuts_0",
          show_projections = sp_val,
          projection_style = "band",
          temporal_mode = tm_val,
          map_selected_ws = if (identical(tm_val, "WS")) "WS1" else NULL,
          climate_variable = "2m_temperature",
          ws_df = if (identical(tm_val, "WS")) test_ws_df else NULL,
          clim_df = test_clim_df,
          base_df = test_base_df,
          selected_year = 2030,
          historical_period = "1991-2020",
          projection_period = "2021-2040",
          projection_view_mode = pvm_val,
          display_mode = "absolute",
          ssp_scenario = "ssp2_4_5"
        )
      }, error = function(e) {
        err <<- e
        NULL
      })
      
      if (!is.null(err)) {
        failed_tests <- failed_tests + 1
        failures <- c(failures, sprintf("[FAIL] %s -> Error: %s", test_label, conditionMessage(err)))
        cat(sprintf("  [FAIL] %s -> %s\n", test_label, conditionMessage(err)))
        next
      }
      
      if (is.null(res)) {
        failed_tests <- failed_tests + 1
        failures <- c(failures, sprintf("[FAIL] %s -> Result was unexpectedly NULL", test_label))
        cat(sprintf("  [FAIL] %s -> Result NULL\n", test_label))
        next
      }
      
      html_str <- as.character(res)
      
      # Determine expected header text
      expected_hdr <- if (identical(tm_val, "WS")) {
        "ws-metric-card"
      } else if (isTRUE(pvm_val == "period") && isTRUE(sp_val == "1")) {
        if (identical(tm_val, "1")) {
          "JANUARY (2021-2040) SUMMARY"
        } else if (identical(tm_val, "DJF")) {
          "WINTER (DJF) (2021-2040) SUMMARY"
        } else {
          "2021-2040 PERIOD SUMMARY"
        }
      } else {
        if (identical(tm_val, "1")) {
          "JANUARY 2030 SUMMARY"
        } else if (identical(tm_val, "DJF")) {
          "WINTER (DJF) 2030 SUMMARY"
        } else {
          "2030 ANNUAL SUMMARY"
        }
      }
      
      hdr_found <- grepl(expected_hdr, html_str, fixed = TRUE)
      if (!hdr_found) {
        failed_tests <- failed_tests + 1
        failures <- c(failures, sprintf("[FAIL] %s -> Expected header '%s' not found in HTML", test_label, expected_hdr))
        cat(sprintf("  [FAIL] %s -> Missing expected header '%s'\n", test_label, expected_hdr))
        next
      }
      
      # Verify ensemble toggle presence when show_projections == "1"
      if (isTRUE(sp_val == "1")) {
        if (!grepl("Ensemble View", html_str, fixed = TRUE)) {
          failed_tests <- failed_tests + 1
          failures <- c(failures, sprintf("[FAIL] %s -> Expected 'Ensemble View' card when show_projections='1'", test_label))
          cat(sprintf("  [FAIL] %s -> Missing Ensemble View card\n", test_label))
          next
        }
      }
      
      passed_tests <- passed_tests + 1
    }
  }
}

cat(sprintf("\nCombinatorial Matrix: %d / %d tests passed.\n", passed_tests, total_tests))

# --- Additional Edge Cases & Invariants ---
cat("\n--- Running Additional Edge Cases & Invariants ---\n")

edge_cases <- list(
  list(
    name = "NULL region returns NULL",
    fn = function() {
      res <- build_region_stats_cards(
        region = NULL,
        spatial_level = "nuts_0",
        show_projections = "1",
        projection_style = "band"
      )
      is.null(res)
    }
  ),
  list(
    name = "display_mode='anomaly' renders baseline anomaly comparison text",
    fn = function() {
      res <- build_region_stats_cards(
        region = test_region,
        spatial_level = "nuts_0",
        show_projections = "1",
        projection_style = "band",
        temporal_mode = "Annual",
        climate_variable = "2m_temperature",
        clim_df = test_clim_df,
        base_df = test_base_df,
        selected_year = 2030,
        historical_period = "1991-2020",
        projection_period = "2021-2040",
        projection_view_mode = "year",
        display_mode = "anomaly"
      )
      html_str <- as.character(res)
      grepl("vs 1991-2020 baseline", html_str, fixed = TRUE) &&
        grepl("+1.6", html_str, fixed = TRUE)
    }
  ),
  list(
    name = "empty string temporal_mode ('') sanitizes to Annual",
    fn = function() {
      res <- build_region_stats_cards(
        region = test_region,
        spatial_level = "nuts_0",
        show_projections = "0",
        projection_style = "band",
        temporal_mode = "",
        climate_variable = "2m_temperature",
        clim_df = test_clim_df,
        selected_year = 2020
      )
      html_str <- as.character(res)
      grepl("2020 ANNUAL SUMMARY", html_str, fixed = TRUE)
    }
  ),
  list(
    name = "NULL projection_style defaults to band without error",
    fn = function() {
      res <- build_region_stats_cards(
        region = test_region,
        spatial_level = "nuts_0",
        show_projections = "1",
        projection_style = NULL,
        temporal_mode = "Annual",
        climate_variable = "2m_temperature",
        clim_df = test_clim_df,
        projection_view_mode = "year"
      )
      !is.null(res) && grepl("Ensemble View", as.character(res), fixed = TRUE)
    }
  ),
  list(
    name = "NULL clim_df renders without error (summary card omitted)",
    fn = function() {
      res <- build_region_stats_cards(
        region = test_region,
        spatial_level = "nuts_0",
        show_projections = "1",
        projection_style = "band",
        temporal_mode = "Annual",
        climate_variable = "2m_temperature",
        clim_df = NULL,
        projection_view_mode = "period"
      )
      !is.null(res) && grepl("Region Name", as.character(res), fixed = TRUE)
    }
  ),
  list(
    name = "SZOF spatial level with _OFF stripped suffix matches region",
    fn = function() {
      szof_region <- list(name = "Offshore Zone", zone_id = "FR00_OFF", area_km2 = 5000, parent_zone = "FR", level = "SZOF")
      szof_clim_df <- data.frame(Region = "FR00", Value = 9.5, stringsAsFactors = FALSE)
      res <- build_region_stats_cards(
        region = szof_region,
        spatial_level = "SZOF",
        show_projections = "1",
        projection_style = "band",
        temporal_mode = "Annual",
        climate_variable = "wind_speed",
        clim_df = szof_clim_df,
        projection_view_mode = "period",
        projection_period = "2021-2040"
      )
      !is.null(res) && grepl("2021-2040 PERIOD SUMMARY", as.character(res), fixed = TRUE)
    }
  )
)

for (ec in edge_cases) {
  total_tests <- total_tests + 1
  err <- NULL
  ok <- tryCatch(ec$fn(), error = function(e) { err <<- e; FALSE })
  if (isTRUE(ok)) {
    passed_tests <- passed_tests + 1
    cat(sprintf("  [PASS] %s\n", ec$name))
  } else {
    failed_tests <- failed_tests + 1
    msg <- if (!is.null(err)) conditionMessage(err) else "Condition failed"
    failures <- c(failures, sprintf("[FAIL] %s -> %s", ec$name, msg))
    cat(sprintf("  [FAIL] %s -> %s\n", ec$name, msg))
  }
}

cat("\n==============================================================================\n")
cat(sprintf("TOTAL TESTS: %d | PASSED: %d | FAILED: %d\n", total_tests, passed_tests, failed_tests))
cat("==============================================================================\n")

if (failed_tests > 0) {
  cat("\nFailed Tests Summary:\n")
  for (f in failures) {
    cat(sprintf("  %s\n", f))
  }
  quit(status = 1)
} else {
  cat("\nAll region stats card regression tests PASSED successfully.\n")
  quit(status = 0)
}
