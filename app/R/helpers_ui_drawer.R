# helpers_ui_drawer.R
# ==============================================================================
# UI Drawer Helpers
# ==============================================================================
# This file contains functions to generate the HTML for the right-side sliding 
# stats drawer. 
# ==============================================================================

library(shiny)

# Helper to build a single metric card
metric_card <- function(label, value, accent_class = "") {
  safe_val <- if (is.null(value) || length(value) == 0) "\u2014" else value
  content <- if (inherits(safe_val, "shiny.tag") || inherits(safe_val, "html") || inherits(safe_val, "shiny.tag.list")) {
    safe_val
  } else {
    HTML(as.character(safe_val))
  }
  div(
    class = paste("metric-card", accent_class),
    div(class = "metric-label", label),
    div(class = "metric-value", content)
  )
}

# ------------------------------------------------------------------------------
# build_region_stats_cards()
# ------------------------------------------------------------------------------
# Generates the HTML for the region stats cards in the right drawer.
# ------------------------------------------------------------------------------
build_region_stats_cards <- function(region, spatial_level, show_projections, projection_style,
                                      temporal_mode = "Annual", map_selected_ws = NULL,
                                      climate_variable = NULL, ws_df = NULL,
                                      clim_df = NULL, base_df = NULL,
                                      selected_year = 2020, historical_period = "1991-2020",
                                      projection_period = "2021-2040", projection_view_mode = "year",
                                      display_mode = "absolute", ssp_scenario = "ssp2_4_5",
                                      solar_technology = NULL) {
  if (is.null(region)) return(NULL)

  # Defensive defaults for inputs that might be NULL, empty, NA, or invalid during initialization
  if (is.null(temporal_mode) || length(temporal_mode) == 0 || is.na(temporal_mode) || !nzchar(temporal_mode)) {
    temporal_mode <- "Annual"
  }
  if (is.null(projection_view_mode) || length(projection_view_mode) == 0 || is.na(projection_view_mode)) {
    projection_view_mode <- "year"
  }
  if (is.null(show_projections) || length(show_projections) == 0 || is.na(show_projections)) {
    show_projections <- "0"
  }
  if (is.null(projection_style) || length(projection_style) == 0 || is.na(projection_style)) {
    projection_style <- "band"
  }

  area_txt <- "N/A"
  if (!is.null(region$area_km2) && !is.na(region$area_km2)) {
    area_txt <- sprintf("%s km\u00b2", format(round(as.numeric(region$area_km2)), big.mark = ",", trim = TRUE))
  }

  parent_txt <- "\u2014"
  if (!is.null(region$parent_zone) && !is.na(region$parent_zone) && region$parent_zone != "null") {
    parent_txt <- region$parent_zone
  }

  # Translate Study Zones for UI clarity if they are bundled inside Bidding Zone maps
  display_level <- if (!is.null(region$level) && nzchar(region$level)) region$level else spatial_level
  if (spatial_level == "P2ON" && identical(region$level, "SZON")) {
    display_level <- "P2ON (Study Zone)"
  } else if (spatial_level == "P2OF" && identical(region$level, "SZOF")) {
    display_level <- "P2OF (Study Zone)"
  }

  cards <- list(
    metric_card("Region Name",    region$name),
    metric_card("Zone ID",        sprintf("<code>%s</code>", region$zone_id), "accent-danger"),
    metric_card("Parent Zone",    sprintf("<code>%s</code>", parent_txt)),
    metric_card("Spatial Tier",   display_level)
  )

  # Weather Scenario Metric Card (when in WS mode)
  if (isTRUE(temporal_mode == "WS") && !is.null(map_selected_ws) && map_selected_ws != "") {
    var_meta <- if (!is.null(climate_variable) && climate_variable %in% names(climate_variables)) {
      enrich_var_meta(climate_variables[[climate_variable]], solar_technology)
    } else NULL
    
    unit_str <- if (!is.null(var_meta)) var_meta$unit else ""
    is_extensive <- (climate_variable %in% c("total_precipitation") || grepl("^hydropower_", climate_variable))
    is_cf <- (!is.null(unit_str) && unit_str == "CF")
    
    calc_val <- NULL
    calc_type <- NULL

    # 1. Try daily ws_df if available (e.g., climate variables on ws_cycle tab)
    has_ws_df <- (!is.null(ws_df) && nrow(ws_df) > 0)
    ws_sub <- if (has_ws_df) ws_df[ws_df$WS == map_selected_ws, ] else NULL

    if (!is.null(ws_sub) && nrow(ws_sub) > 0) {
      if (is_extensive) {
        calc_val <- sum(ws_sub$Value, na.rm = TRUE)
        calc_type <- "Annual Sum"
      } else {
        calc_val <- mean(ws_sub$Value, na.rm = TRUE)
        calc_type <- if (is_cf) "Annual CF" else "Annual Mean"
      }
    } else if (!is.null(clim_df) && nrow(clim_df) > 0) {
      # 2. Fallback to annual choropleth data (filtered_climate_data) for wind, solar, or non-daily tabs
      target_id <- region$zone_id
      if (spatial_level %in% c("SZOF", "szof")) {
        target_id <- sub("_OFF$", "", target_id)
      }
      curr_row <- clim_df[clim_df$Region == target_id | clim_df$Region == region$zone_id, ]
      if (nrow(curr_row) > 0) {
        calc_val <- curr_row$Value[1]
        calc_type <- if (is_cf) "Annual CF" else if (is_extensive) "Annual Sum" else "Annual Mean"
      }
    }

    # 3. Direct query fallback via query_ws_annual_for_map if neither ws_df nor clim_df provided data
    if (is.null(calc_val) && exists("query_ws_annual_for_map") && !is.null(climate_variable)) {
      ws_annual_df <- tryCatch(
        query_ws_annual_for_map(
          var_name = climate_variable,
          sp_level = spatial_level,
          ws_code = map_selected_ws,
          solar_tech = solar_technology,
          tech_mix_mode = "fixed_2020"
        ),
        error = function(e) NULL
      )
      if (!is.null(ws_annual_df) && nrow(ws_annual_df) > 0) {
        target_id <- region$zone_id
        if (spatial_level %in% c("SZOF", "szof")) {
          target_id <- sub("_OFF$", "", target_id)
        }
        curr_row <- ws_annual_df[ws_annual_df$Region == target_id | ws_annual_df$Region == region$zone_id, ]
        if (nrow(curr_row) > 0) {
          calc_val <- curr_row$Value[1]
          calc_type <- if (is_cf) "Annual CF" else if (is_extensive) "Annual Sum" else "Annual Mean"
        }
      }
    }
    
    if (!is.null(calc_val) && is.finite(calc_val)) {
      dec <- if (is_cf) 3 else 1
      val_fmt <- format(round(calc_val, dec), nsmall = dec, big.mark = ",", trim = TRUE)
      val_display <- if (nzchar(unit_str)) sprintf("%s %s", val_fmt, unit_str) else val_fmt
      
      ws_label <- if (exists("get_ws_display_label")) get_ws_display_label(map_selected_ws) else map_selected_ws
      
      ws_content <- HTML(sprintf(
        "<div style='display: flex; align-items: baseline; justify-content: space-between; gap: 8px; flex-wrap: wrap;'><span style='color: #38bdf8; font-weight: 700; font-size: 1.05rem;'>%s</span><span style='font-size: 0.70rem; color: #94a3b8; font-weight: 500; text-transform: uppercase;'>%s</span></div>",
        val_display, calc_type
      ))
      
      cards <- c(cards, list(metric_card(ws_label, ws_content, "accent-warning ws-metric-card")))
    }
  } else if (!isTRUE(temporal_mode == "WS") && !is.null(clim_df) && nrow(clim_df) > 0 && !is.null(climate_variable)) {
    # Non-WS Summary Card for Annual, Monthly, and Seasonal modes
    target_id <- region$zone_id
    if (spatial_level %in% c("SZOF", "szof")) {
      target_id <- sub("_OFF$", "", target_id)
    }
    
    curr_row <- clim_df[clim_df$Region == target_id | clim_df$Region == region$zone_id, ]
    if (nrow(curr_row) > 0) {
      var_meta <- enrich_var_meta(climate_variables[[climate_variable]], solar_technology)
      unit_str <- var_meta$unit
      curr_val <- curr_row$Value[1]
      
      base_val <- NULL
      if (!is.null(base_df) && nrow(base_df) > 0) {
        b_row <- base_df[base_df$Region == target_id | base_df$Region == region$zone_id, ]
        if (nrow(b_row) > 0) base_val <- b_row$baseline_value[1]
      }
      
      time_hdr <- if (identical(temporal_mode, "Annual") || isTRUE(temporal_mode == "Annual")) {
        if (isTRUE(projection_view_mode == "period") && isTRUE(show_projections == "1")) {
          sprintf("%s Period", projection_period)
        } else {
          sprintf("%s Annual", selected_year)
        }
      } else if (temporal_mode %in% as.character(1:12)) {
        m_name <- month.name[as.integer(temporal_mode)]
        if (isTRUE(projection_view_mode == "period") && isTRUE(show_projections == "1")) {
          sprintf("%s (%s)", m_name, projection_period)
        } else {
          sprintf("%s %s", m_name, selected_year)
        }
      } else {
        season_names <- c("DJF" = "Winter (DJF)", "MAM" = "Spring (MAM)", "JJA" = "Summer (JJA)", "SON" = "Autumn (SON)")
        s_name <- if (temporal_mode %in% names(season_names)) season_names[[temporal_mode]] else temporal_mode
        if (isTRUE(projection_view_mode == "period") && isTRUE(show_projections == "1")) {
          sprintf("%s (%s)", s_name, projection_period)
        } else {
          sprintf("%s %s", s_name, selected_year)
        }
      }
      
      val_fmt <- if (is.finite(curr_val)) {
        format(round(curr_val, 1), nsmall = 1, big.mark = ",", trim = TRUE)
      } else "N/A"
      
      sub_info <- ""
      if (!is.null(base_val) && is.finite(base_val) && is.finite(curr_val)) {
        anom <- curr_val - base_val
        anom_sign <- if (anom >= 0) "+" else ""
        anom_fmt <- sprintf("%s%.1f %s", anom_sign, anom, unit_str)
        base_fmt <- sprintf("%.1f %s", base_val, unit_str)
        
        anom_color <- if (anom >= 0) "#38bdf8" else "#f43f5e"
        if (climate_variable == "total_precipitation") {
          anom_color <- if (anom >= 0) "#34d399" else "#f59e0b"
        }
        
        sub_info <- sprintf(
          "<div style='font-size: 0.72rem; color: #94a3b8; margin-top: 4px; border-top: 1px solid rgba(255,255,255,0.06); padding-top: 4px;'><span>vs %s baseline (%s): </span><span style='color: %s; font-weight: 600;'>%s</span></div>",
          historical_period, base_fmt, anom_color, anom_fmt
        )
      }
      
      card_label <- sprintf("%s SUMMARY", toupper(time_hdr))
      card_content <- HTML(sprintf(
        "<div><span style='color: #38bdf8; font-weight: 700; font-size: 1.05rem;'>%s %s</span>%s</div>",
        val_fmt, unit_str, sub_info
      ))
      
      cards <- c(cards, list(metric_card(card_label, card_content, "accent-info ws-metric-card")))
    }
  }

  cards <- c(cards, list(metric_card("Area", area_txt, "accent-success")))

  if (isTRUE(show_projections == "1")) {
    current_val <- if (is.null(projection_style)) "band" else projection_style
    
    proj_style_toggle <- div(
      class = paste0("proj-style-toggle", if (current_val == "spaghetti") " toggle-right" else ""),
      id = "projection-style-toggle-container",
      div(class = "proj-style-pill"),
      tags$button(
        type = "button",
        class = paste0("display-toggle-option", if (current_val == "band") " active" else ""),
        `data-value` = "band",
        "Band"
      ),
      tags$button(
        type = "button",
        class = paste0("display-toggle-option", if (current_val == "spaghetti") " active" else ""),
        `data-value` = "spaghetti",
        "Spaghetti"
      )
    )
    
    toggle_card <- metric_card("Ensemble View", proj_style_toggle, "accent-info")
    
    conditional_toggle <- conditionalPanel(
      condition = "input.drawer_tabs == 'trends'",
      toggle_card
    )
    
    cards <- c(cards, list(conditional_toggle))
  }

  do.call(tagList, cards)
}
