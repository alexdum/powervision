setwd("app")
source('R/helpers_climate_query.R')
source('global.R')

cat("Testing PV data fetching for PT00 P2ON with solar_tech = '63'...\n")

res <- query_arrow_dataset(
  ds_annual = hist_annual_ds,
  ds_seasonal = hist_seasonal_ds,
  ds_monthly = hist_monthly_ds,
  temporal_mode = "Annual",
  var_name = "solar_power_pv",
  sp_level = "p2on",
  target_region = "PT00",
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
  parse("server.R")
  cat("server.R parsed successfully!\n")
}, error = function(e) {
  stop("Error parsing server.R: ", e$message)
})

cat("Testing global.R parsing...\n")
tryCatch({
  parse("global.R")
  cat("global.R parsed successfully!\n")
}, error = function(e) {
  stop("Error parsing global.R: ", e$message)
})

cat("ALL TESTS PASSED\n")
