library(data.table)
library(dplyr)
library(arrow)

# Configuration
raw_dir <- "/data/cds/raw/pecd_csv/energy/hydropower/historical"
out_dir <- "/data/powervision/app/www/data/pecd/historical"

# The 7 hydropower variables
hydro_vars <- c(
  "hydropower_run_of_river_generation",
  "hydropower_reservoir_inflow",
  "hydropower_open_loop_pumped_storage_inflow",
  "hydropower_reservoir_generation",
  "hydropower_run_of_river_inflow",
  "hydropower_run_of_river_with_pondage_generation",
  "hydropower_run_of_river_with_pondage_inflow"
)

# Helper function to assign seasons
get_season <- function(month) {
  case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5)  ~ "Spring",
    month %in% c(6, 7, 8)  ~ "Summer",
    month %in% c(9, 10, 11) ~ "Autumn"
  )
}

process_variable <- function(var_name) {
  cat(sprintf("Processing %s...\n", var_name))
  
  var_dir <- file.path(raw_dir, var_name)
  if (!dir.exists(var_dir)) {
    warning(paste("Directory not found:", var_dir))
    return(NULL)
  }
  
  zip_files <- list.files(var_dir, pattern = "\\.zip$", full.names = TRUE)
  
  # Read and combine all years for this variable
  dt_list <- lapply(zip_files, function(zfile) {
    cmd <- sprintf("unzip -p '%s'", zfile)
    # The CSV has 52 lines of header. We skip directly to 'Date'
    df <- fread(cmd = cmd, skip = "Date", header = TRUE)
    return(df)
  })
  
  dt_full <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  
  # Pivot to long format
  dt_long <- melt(dt_full, id.vars = "Date", variable.name = "Region", value.name = "Value")
  
  # Convert Date and extract time components
  dt_long[, Date := as.Date(Date)]
  dt_long[, Year := as.integer(format(Date, "%Y"))]
  dt_long[, Month := as.integer(format(Date, "%m"))]
  
  # Assign Season and Meteorological Year
  dt_long[, Season := get_season(Month)]
  dt_long[, MetYear := ifelse(Month == 12, Year + 1, Year)]
  
  dt_long[, SpatialLevel := "szon"]
  dt_long[, variable := var_name]
  
  # Ensure Region is character
  dt_long[, Region := as.character(Region)]
  
  # Convert MWh to GWh
  dt_long[, Value := Value / 1000]
  
  # Helper function for safe sum
  sum_safe <- function(x) {
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }

  # -- 1. Annual Aggregation --
  cat(sprintf("  Aggregating Annual for %s\n", var_name))
  dt_annual <- dt_long[, .(Value = if (.N < 52) NA_real_ else sum_safe(Value)), by = .(Year, Region, SpatialLevel, variable)]
  
  # -- 2. Monthly Aggregation --
  cat(sprintf("  Aggregating Monthly for %s\n", var_name))
  dt_monthly <- dt_long[, .(Value = if (.N < 4) NA_real_ else sum_safe(Value)), by = .(Year, Month, Region, SpatialLevel, variable)]
  
  # -- 3. Seasonal Aggregation --
  cat(sprintf("  Aggregating Seasonal for %s\n", var_name))
  dt_seasonal <- dt_long[, .(Value = if (.N < 12) NA_real_ else sum_safe(Value)), by = .(MetYear, Season, Region, SpatialLevel, variable)]
  setnames(dt_seasonal, "MetYear", "Year")
  
  # Truncate edge winters (first year and last year) since they are incomplete
  min_yr <- min(dt_seasonal$Year, na.rm = TRUE)
  max_yr <- max(dt_seasonal$Year, na.rm = TRUE)
  dt_seasonal <- dt_seasonal[!(Season == "Winter" & Year %in% c(min_yr, max_yr))]
  
  # Write Parquet datasets partitioned by variable
  cat(sprintf("  Writing to Parquet for %s\n", var_name))
  
  write_dataset(dt_annual, path = file.path(out_dir, "annual"), partitioning = "variable", format = "parquet", existing_data_behavior = "delete_matching")
  write_dataset(dt_monthly, path = file.path(out_dir, "monthly"), partitioning = "variable", format = "parquet", existing_data_behavior = "delete_matching")
  write_dataset(dt_seasonal, path = file.path(out_dir, "seasonal"), partitioning = "variable", format = "parquet", existing_data_behavior = "delete_matching")
}

# Run for all variables
for (v in hydro_vars) {
  process_variable(v)
}

cat("Hydropower processing complete!\n")
