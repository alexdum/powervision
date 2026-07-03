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
  technology_mix, spatial_level, polygon_opacity, view_mode
) {
  var_meta <- climate_variables[[climate_variable]]
  palette <- var_meta$palette
  var_label <- var_meta$label
  var_unit <- var_meta$unit

  # Use a lightweight dataframe for tooltip/color building
  df_build <- sf::st_drop_geometry(geom_data)
  
  if (!is.null(clim_data) && nrow(clim_data) > 0) {
    df_build <- df_build %>%
      dplyr::left_join(clim_data, by = c("zone_id" = "Region"))
  } else {
    df_build$Value <- NA_real_
  }

  use_anomaly_map <- (show_projections && isTRUE(display_mode == "anomaly"))
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

  display_unit <- if (use_anomaly_map && is_precip) "%" else var_unit

  if (use_anomaly_map) {
    if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
      df_build <- df_build %>%
        dplyr::left_join(baseline_df, by = c("zone_id" = "Region"))

      if (is_precip) {
        df_build <- df_build %>%
          dplyr::mutate(Value = ifelse(
            is.na(baseline_value) | abs(baseline_value) < 1.0,
            NA_real_,
            pmin(pmax((Value - baseline_value) / baseline_value * 100, -200), 200)
          ))
      } else {
        df_build <- df_build %>%
          dplyr::mutate(Value = Value - baseline_value)
      }
    }
    palette <- if (is_precip) anomaly_palette_precipitation else anomaly_palette_temperature
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
    
    df_build$wind_mix_html <- vapply(
      df_build$zone_id, 
      function(zid) get_wind_mix_tooltip(zid, wind_type, tech_year),
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
                               format(round(Value, 2), big.mark = ","), " ", display_unit)),
          "    </span>",
          "  </div>",
          "  <div style='margin-top: 2px; font-size: 0.7rem; color: #64748b;'>vs ", ref_label, " baseline</div>",
          wind_mix_html,
          "</div>"
        )
      )
  } else {
    df_build <- df_build %>%
      dplyr::mutate(
        tooltip_html = paste0(
          "<div class='map-tooltip-content' style='font-family: Inter, sans-serif; padding: 4px;'>",
          "  <div class='tooltip-title' style='font-weight: 600; color: #f8fafc; font-size: 0.85rem;'>", name, " (", zone_id, ") &bull; <span style='color: #cbd5e1; font-weight: 500;'>", time_title, "</span></div>",
          "  <div class='tooltip-metric' style='margin-top: 4px; font-size: 0.8rem;'>",
          "    <span class='tooltip-metric-label' style='color: #94a3b8;'>", var_label, ":</span> ",
          "    <span class='tooltip-metric-value' style='font-weight: 500; color: #38bdf8;'>",
                 ifelse(is.na(Value), "No Data", paste0(format(round(Value, 2), big.mark = ","), " ", var_unit)),
          "    </span>",
          "  </div>",
          wind_mix_html,
          "</div>"
        )
      )
  }

  vals <- df_build$Value
  finite_mask <- is.finite(vals)
  colors <- rep("#33415533", nrow(df_build))

  if (any(finite_mask)) {
    min_val <- min(vals[finite_mask])
    max_val <- max(vals[finite_mask])

    if (use_anomaly_map) {
      abs_max <- max(abs(min_val), abs(max_val))
      if (abs_max < 0.1) abs_max <- 0.1
      if (is_precip && abs_max > 200) abs_max <- 200
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

    indices <- round((vals[finite_mask] - min_val) / (max_val - min_val) * (n_colors - 1)) + 1
    indices <- pmax(1, pmin(n_colors, indices))
    colors[finite_mask] <- color_lut[indices]
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
