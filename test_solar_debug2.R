# test_solar_debug2.R
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

ds_annual_hist <- arrow::open_dataset("app/www/data/pecd/historical/annual")

cat("Checking for PT00 with solar_photovoltaic_63 and p2on:\n")
res <- ds_annual_hist |>
  dplyr::filter(variable == "solar_photovoltaic_63", SpatialLevel == "p2on", Region == "PT00") |>
  dplyr::collect()
print(head(res))
print(nrow(res))
