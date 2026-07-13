# test_solar_debug.R
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

ds_annual_hist <- arrow::open_dataset("app/www/data/pecd/historical/annual")

cat("Distinct variables:\n")
print(
  ds_annual_hist |> dplyr::distinct(variable) |> dplyr::collect()
)

cat("\nDistinct spatial levels:\n")
print(
  ds_annual_hist |> dplyr::distinct(SpatialLevel) |> dplyr::collect()
)

cat("\nChecking for PT00 with solar_photovoltaic_63 and p2on:\n")
res <- ds_annual_hist |>
  dplyr::filter(variable == "solar_photovoltaic_63", SpatialLevel == "p2on") |>
  dplyr::collect()
print(head(res))
