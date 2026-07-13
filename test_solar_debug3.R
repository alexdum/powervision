# test_solar_debug3.R
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

ds_annual_hist <- arrow::open_dataset("app/www/data/pecd/historical/annual")

cat("Distinct Regions for p2on and solar_photovoltaic_63:\n")
res <- ds_annual_hist |>
  dplyr::filter(variable == "solar_photovoltaic_63", SpatialLevel == "p2on") |>
  dplyr::distinct(Region) |>
  dplyr::collect()
print(res$Region)
