# helpers_map.R
# ==============================================================================
# Map Rendering Helpers
# ==============================================================================
# This file contains the logic for updating the map choropleth (polygon colors)
# and generating the tooltip HTML.
# ==============================================================================

library(dplyr)

update_map_choropleth <- function(
  session, geom_data, clim_data, baseline_df,
  climate_variable, selected_year, display_mode, 
  show_projections, projection_period, historical_period, 
  technology_mix, spatial_level, polygon_opacity, view_mode,
  solar_tech = NULL, is_relative_anomaly = FALSE
) {
  var_meta <- enrich_var_meta(climate_variables[[climate_variable]], solar_tech)
  palette <- var_meta$palette
  var_label <- var_meta$label
  var_unit <- var_meta$unit

  # Use a lightweight dataframe for tooltip/color building
  df_build <- sf::st_drop_geometry(geom_data)
  
  if (!is.null(clim_data) && nrow(clim_data) > 0) {
    # SZOF normalization: The processing pipeline stripped the "_OFF" suffix from
    # offshore study zone region IDs in the parquet data, but the SZOF GeoJSON
    # still uses zone_ids with the "_OFF" suffix (e.g., "AL00_OFF", "FR00_OFF").
    # We create a temporary join key that strips "_OFF" so the left_join matches.
    if (spatial_level == "SZOF") {
      df_build$join_key <- sub("_OFF$", "", df_build$zone_id)
      df_build <- df_build %>%
        dplyr::left_join(clim_data, by = c("join_key" = "Region"))
      df_build$join_key <- NULL
    } else {
      df_build <- df_build %>%
        dplyr::left_join(clim_data, by = c("zone_id" = "Region"))
    }
  } else {
    df_build$Value <- NA_real_
  }

  use_anomaly_map <- (show_projections && isTRUE(display_mode == "anomaly"))
  if (isTRUE(var_meta$is_categorical)) use_anomaly_map <- FALSE
  is_precip <- (climate_variable == "total_precipitation")
  sel_year <- as.integer(selected_year)
  use_period <- isTRUE(view_mode == "period")

  is_wind <- climate_variable %in% c("wind_power_onshore", "wind_power_offshore")
  tech_mix_mode_val <- if (!is.null(technology_mix)) technology_mix else "dynamic"
  is_dynamic_wind <- (is_wind && tech_mix_mode_val == "dynamic")

  sp_level_pq <- spatial_level_to_parquet[spatial_level]
  hist_max_year <- get_historical_max_year(climate_variable, sp_level_pq)

  if (use_period && !is.null(projection_period) && nchar(projection_period) > 0) {
    period_end_year <- as.integer(strsplit(projection_period, "-")[[1]][2])
    is_projection_year <- (period_end_year > hist_max_year || is_dynamic_wind)
  } else {
    is_projection_year <- (sel_year > hist_max_year || is_dynamic_wind)
  }

  display_unit <- if (use_anomaly_map && is_relative_anomaly) "%" else var_unit

  if (use_anomaly_map) {
    if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
      # Same SZOF _OFF normalization as the clim_data join above
      if (spatial_level == "SZOF") {
        df_build$join_key <- sub("_OFF$", "", df_build$zone_id)
        df_build <- df_build %>%
          dplyr::left_join(baseline_df, by = c("join_key" = "Region"))
        df_build$join_key <- NULL
      } else {
        df_build <- df_build %>%
          dplyr::left_join(baseline_df, by = c("zone_id" = "Region"))
      }

      if (is_relative_anomaly) {
        df_build <- df_build %>%
          dplyr::mutate(Value = ifelse(
            is.na(baseline_value) | abs(baseline_value) < 0.001,
            NA_real_,
            pmin(pmax((Value - baseline_value) / baseline_value * 100, -200), 200)
          ))
      } else {
        df_build <- df_build %>%
          dplyr::mutate(Value = Value - baseline_value)
      }
    }
    is_any_wind <- grepl("Wind", var_label, ignore.case = TRUE)
    palette <- if (is_precip) anomaly_palette_precipitation else if (is_any_wind) anomaly_palette_wind else anomaly_palette_temperature
  }

  period_label <- if (use_period) paste0(projection_period, " period mean") else ""
  
  if (is_wind) {
    wind_type <- if (climate_variable == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(technology_mix)) technology_mix else "dynamic"
    
    target_data_year <- sel_year
    if (use_period && !is.null(projection_period) && nchar(projection_period) > 0) {
      pts <- as.integer(strsplit(projection_period, "-")[[1]])
      target_data_year <- floor((pts[1] + pts[2]) / 2)
    }
    tech_year <- resolve_tech_year(tech_mix_mode, target_data_year)
    
    # Detect fixed_2020 mode so the tooltip correctly shows "100% Existing Fleet"
    # instead of the interpolated 2025 technology blend
    is_existing_fleet_only <- (tech_mix_mode == "fixed_2020")
    
    df_build$wind_mix_html <- vapply(
      df_build$zone_id, 
      function(zid) get_wind_mix_tooltip(zid, wind_type, tech_year,
                                          use_existing_fleet_only = is_existing_fleet_only),
      FUN.VALUE = character(1), 
      USE.NAMES = FALSE
    )
  } else {
    df_build$wind_mix_html <- ""
  }
  
  time_title <- if (use_period) projection_period else as.character(sel_year)
  if (show_projections && is_projection_year) {
    time_title <- paste0(time_title, " (Proj)")
  }

  decimals <- if (var_unit == "CF" && display_unit != "%") 3 else 2

  if (use_anomaly_map) {
    ref_label <- historical_period
    df_build <- df_build %>%
      dplyr::mutate(
        tooltip_html = paste0(
          "<div class='map-tooltip-content' style='font-family: Inter, sans-serif; padding: 4px;'>",
          "  <div class='tooltip-title' style='font-weight: 600; color: #f8fafc; font-size: 0.85rem;'>", name, " (", zone_id, ") &bull; <span style='color: #cbd5e1; font-weight: 500;'>", time_title, "</span></div>",
          "  <div class='tooltip-metric' style='margin-top: 4px; font-size: 0.8rem;'>",
          "    <span class='tooltip-metric-label' style='color: #94a3b8;'>", var_label, " anomaly:</span> ",
          "    <span class='tooltip-metric-value' style='font-weight: 500; color: #38bdf8;'>",
                 ifelse(is.na(Value), "No Data",
                        paste0(ifelse(Value >= 0, "+", ""),
                               format(round(Value, decimals), nsmall = decimals, big.mark = ",", trim = TRUE), " ", display_unit)),
          "    </span>",
          "  </div>",
          "  <div style='margin-top: 2px; font-size: 0.7rem; color: #64748b;'>vs ", ref_label, " baseline</div>",
          wind_mix_html,
          "</div>"
        )
      )
  } else {
    is_categorical <- isTRUE(climate_variables[[climate_variable]]$is_categorical)

    tooltip_value_html <- if (is_categorical) {
      ifelse(is.na(df_build$Value) | df_build$Value == "", "No Data", as.character(df_build$Value))
    } else {
      ifelse(is.na(df_build$Value), "No Data", paste0(format(round(as.numeric(df_build$Value), decimals), nsmall = decimals, big.mark = ",", trim = TRUE), " ", var_unit))
    }

    df_build <- df_build |>
      dplyr::mutate(
        tooltip_html = paste0(
          "<div class='map-tooltip-content' style='font-family: Inter, sans-serif; padding: 4px;'>",
          "  <div class='tooltip-title' style='font-weight: 600; color: #f8fafc; font-size: 0.85rem;'>", name, " (", zone_id, ") &bull; <span style='color: #cbd5e1; font-weight: 500;'>", time_title, "</span></div>",
          "  <div class='tooltip-metric' style='margin-top: 4px; font-size: 0.8rem;'>",
          "    <span class='tooltip-metric-label' style='color: #94a3b8;'>", var_label, ":</span> ",
          "    <span class='tooltip-metric-value' style='font-weight: 500; color: #38bdf8;'>",
                 tooltip_value_html,
          "    </span>",
          "  </div>",
          wind_mix_html,
          "</div>"
        )
      )
  }

  vals <- df_build$Value
  colors <- rep("#33415533", nrow(df_build))

  is_categorical <- isTRUE(climate_variables[[climate_variable]]$is_categorical)

  if (is_categorical) {
    valid_mask <- !is.na(vals) & vals != ""
    if (any(valid_mask)) {
      cat_colors <- sapply(vals[valid_mask], function(v) {
        if (v %in% names(palette)) palette[[v]] else "#33415533"
      })
      colors[valid_mask] <- cat_colors
    }
  } else {
    finite_mask <- is.finite(as.numeric(vals))
    if (any(finite_mask)) {
      numeric_vals <- as.numeric(vals[finite_mask])
      # Tidy bounds
      if (var_unit == "CF") {
        min_val <- floor(min(numeric_vals) * 1000) / 1000
        max_val <- ceiling(max(numeric_vals) * 1000) / 1000
      } else {
        min_val <- floor(min(numeric_vals))
        max_val <- ceiling(max(numeric_vals))
      }

      if (use_anomaly_map || climate_variable == "2m_temperature") {
        abs_max <- max(abs(min_val), abs(max_val))
        # Apply a soft cap for very extreme outliers (e.g. AWI model desert greening)
        if (is_precip && abs_max > 200) abs_max <- 200
        # Wait, actually is_relative_anomaly should be capped at 200% instead of just precip
        if (is_relative_anomaly && abs_max > 200) abs_max <- 200
        min_val <- -abs_max
        max_val <- abs_max
      }

      if (min_val == max_val) {
        min_val <- min_val - 0.1
        max_val <- max_val + 0.1
      }

      color_fn <- grDevices::colorRampPalette(palette)
      n_colors <- 256
      color_lut <- color_fn(n_colors)

      if (grepl("^hydropower_", climate_variable)) {
        # Logarithmic breaks (pseudo-log) for highly skewed hydropower data
        pseudo_log <- function(x) sign(x) * log10(abs(x) + 1)
        log_vals <- pseudo_log(numeric_vals)
        log_min <- pseudo_log(min_val)
        log_max <- pseudo_log(max_val)
        if (log_min == log_max) {
          indices <- rep(1, length(log_vals))
        } else {
          indices <- round((log_vals - log_min) / (log_max - log_min) * (n_colors - 1)) + 1
        }
      } else {
        indices <- round((numeric_vals - min_val) / (max_val - min_val) * (n_colors - 1)) + 1
      }
      indices <- pmax(1, pmin(n_colors, indices))
      colors[finite_mask] <- color_lut[indices]
    }
  }

  tooltip_list <- as.list(df_build$tooltip_html)
  names(tooltip_list) <- df_build$zone_id
  session$sendCustomMessage("update_zone_tooltips", tooltip_list)

  zone_ids <- df_build$zone_id
  interleaved <- character(length(zone_ids) * 2)
  interleaved[seq(1, length(zone_ids) * 2, by = 2)] <- paste0('"', zone_ids, '"')
  interleaved[seq(2, length(zone_ids) * 2, by = 2)] <- paste0('"', colors, '"')
  fill_expr_json <- paste0(
    '["match",["get","zone_id"],',
    paste(interleaved, collapse = ","),
    ',"#33415533"]'
  )

  current_opacity <- if (is.null(polygon_opacity)) 0.65 else polygon_opacity

  session$sendCustomMessage("paint_zone_fills", list(
    fill_expr_json = fill_expr_json,
    opacity        = current_opacity
  ))
}
