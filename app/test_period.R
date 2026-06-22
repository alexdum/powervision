source("global.R")
source("R/helpers_climate_query.R")

cat("\n--- Benchmarking Projected Period ---\n")
t1 <- Sys.time()
df_proj <- query_arrow_dataset(
  proj_annual_ds, proj_seasonal_ds, "Annual",
  "2m_temperature", "nuts_2", 
  year_start = 2041, year_end = 2060, scenario_val = "ssp2_4_5",
  select_cols = c("Region", "Value", "model")
)
t2 <- Sys.time()
cat(sprintf("Projected period query took: %.3f seconds\n", as.numeric(difftime(t2, t1, units = "secs"))))
