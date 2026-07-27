# helpers_export.R
# ==============================================================================
# CSV Data Export Helper for Context-Aware (WYSIWYG) Downloads
# ==============================================================================

library(dplyr)

generate_wysiwyg_export_csv <- function(
  df_hist, df_proj, active_tab, display_mode, 
  historical_period, projection_period, var_name, region_id,
  is_relative_anomaly = FALSE
) {
  
  baseline <- NULL
  if (isTRUE(display_mode == "anomaly") && !is.null(df_hist) && !is.null(historical_period) && isTRUE(nchar(historical_period) > 0)) {
    ref_years <- as.integer(strsplit(historical_period, "-")[[1]])
    ref_vals <- df_hist$Value[df_hist$Year >= ref_years[1] & df_hist$Year <= ref_years[2]]
    if (length(ref_vals) > 0) baseline <- mean(ref_vals, na.rm = TRUE)
  }

  if (isTRUE(grepl("seasonality", active_tab))) {
    if (!is.null(df_hist) && !is.null(historical_period) && isTRUE(nchar(historical_period) > 0)) {
      ref_years <- as.integer(strsplit(historical_period, "-")[[1]])
      df_hist <- df_hist %>%
        filter(Year >= ref_years[1] & Year <= ref_years[2])
      
      if ("Month" %in% names(df_hist)) {
        df_hist <- df_hist %>%
          mutate(Source = "Historical (ERA5)", scenario = "Historical")
      }
    }
    if (!is.null(df_proj) && !is.null(projection_period) && isTRUE(nchar(projection_period) > 0)) {
      tgt_years <- as.integer(strsplit(projection_period, "-")[[1]])
      df_proj <- df_proj %>%
        filter(Year >= tgt_years[1] & Year <= tgt_years[2])
      
      if ("Month" %in% names(df_proj)) {
        df_proj <- df_proj %>%
          mutate(Source = "Projection")
      }
    }
  } else {
    if (isTRUE(display_mode == "anomaly") && !is.null(baseline)) {
      if (is_relative_anomaly) {
        if (!is.null(df_hist)) df_hist$Value <- (df_hist$Value - baseline) / baseline * 100
        if (!is.null(df_proj)) df_proj$Value <- (df_proj$Value - baseline) / baseline * 100
      } else {
        if (!is.null(df_hist)) df_hist$Value <- df_hist$Value - baseline
        if (!is.null(df_proj)) df_proj$Value <- df_proj$Value - baseline
      }
    }
    if (!is.null(df_hist)) {
      df_hist$Source <- "Historical (ERA5)"
      df_hist$scenario <- "Historical"
    }
    if (!is.null(df_proj)) {
      df_proj$Source <- "Projection"
    }
  }

  res_list <- list()
  if (!is.null(df_hist) && nrow(df_hist) > 0) {
    if (!"scenario" %in% names(df_hist)) df_hist$scenario <- "Historical"
    res_list[[1]] <- df_hist[, intersect(c("Year", "Month", "scenario", "Value", "Source"), names(df_hist))]
  }
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    if (!"scenario" %in% names(df_proj)) df_proj$scenario <- "Projection"
    res_list[[2]] <- df_proj[, intersect(c("Year", "Month", "scenario", "Value", "Source"), names(df_proj))]
  }

  df_combined <- bind_rows(res_list)
  if (nrow(df_combined) > 0) {
    df_combined$Region <- region_id
    df_combined$Variable <- var_name
    
    if (grepl("seasonality", active_tab) && "Month" %in% names(df_combined)) {
      df_combined <- df_combined[order(df_combined$Month), ]
    } else if ("Year" %in% names(df_combined)) {
      df_combined <- df_combined[order(df_combined$Year), ]
    }
    df_combined$Value <- round(df_combined$Value, 4)
  }

  df_combined
}
