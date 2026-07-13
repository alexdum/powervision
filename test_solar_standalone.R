# test_solar_standalone.R
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

source('app/R/helpers_climate_query.R')

cat("Testing PV data fetching for PT01 P2ON with solar_tech = '63'...\n")

ds_annual_hist <- arrow::open_dataset("app/www/data/pecd/historical/annual")

res <- query_arrow_dataset(
  ds_annual = ds_annual_hist,
  ds_seasonal = NULL,
  ds_monthly = NULL,
  temporal_mode = "Annual",
  var_name = "solar_power_pv",
  sp_level = "p2on",
  target_region = "PT01",
  solar_tech = "63"
)

if (is.null(res)) {
  stop("Query returned NULL.")
} else if (nrow(res) == 0) {
  stop("Query returned 0 rows.")
} else {
  cat(sprintf("Success! Retrieved %d rows.\n", nrow(res)))
  print(head(res))
}

cat("Testing server.R parsing...\n")
tryCatch({
  parse("app/server.R")
  cat("server.R parsed successfully!\n")
}, error = function(e) {
  stop("Error parsing server.R: ", e$message)
})

cat("Testing global.R parsing...\n")
tryCatch({
  parse("app/global.R")
  cat("global.R parsed successfully!\n")
}, error = function(e) {
  stop("Error parsing global.R: ", e$message)
})

cat("ALL TESTS PASSED\n")
