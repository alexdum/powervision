source("global.R")
source("R/helpers_climate_query.R")

cat("\n--- Benchmarking Historical ---\n")
t1 <- Sys.time()
df_hist <- query_arrow_dataset(
  hist_annual_ds, hist_seasonal_ds, "Annual",
  "2m_temperature", "nuts_2", year = 2000,
  select_cols = c("Region", "Value")
)
t2 <- Sys.time()
cat(sprintf("Historical query took: %.3f seconds\n", as.numeric(difftime(t2, t1, units = "secs"))))

cat("\n--- Benchmarking Projections ---\n")
t1 <- Sys.time()
df_proj <- query_arrow_dataset(
  proj_annual_ds, proj_seasonal_ds, "Annual",
  "2m_temperature", "nuts_2", year = 2050, scenario_val = "ssp2_4_5",
  select_cols = c("Region", "Value")
)
t2 <- Sys.time()
cat(sprintf("Projections query took: %.3f seconds\n", as.numeric(difftime(t2, t1, units = "secs"))))

