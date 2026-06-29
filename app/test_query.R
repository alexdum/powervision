source("global.R")
source("R/helpers_climate_query.R")

sp_level_pq <- "p2of"
region_id <- "FR115_OFF"

df <- query_arrow_dataset(
  ds_annual = proj_annual_ds, ds_seasonal = proj_seasonal_ds, ds_monthly = proj_monthly_ds,
  temporal_mode = "Annual",
  var_name = "wind_offshore_20",
  sp_level = sp_level_pq,
  year_start = 2015,
  year_end = 2020,
  target_region = region_id
)
print(head(df))
print(summary(df$Value))
