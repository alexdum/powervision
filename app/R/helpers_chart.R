# helpers_chart.R

# ==============================================================================
# Time-Series Chart Builder for the Region Stats Drawer
# ==============================================================================
# Constructs the interactive Plotly time-series chart shown in the bottom
# drawer when a region polygon is clicked on the map. This function handles:
#
#   1. Anomaly transformation (absolute or relative/%) of both historical
#      and projection data when anomaly display mode is active
#   2. Chart title construction with SSP scenario and reference period context
#   3. Plotly trace assembly: historical line, projection envelope + median
#   4. Annotation overlays: "Present Day" divider, baseline line, reference
#      period shaded band
#   5. Dark glassmorphism-compatible layout styling (transparent backgrounds,
#      Inter font, muted grid lines)
#
# This is a pure rendering function with NO Shiny reactivity (no input$,
# no reactive(), no session). It receives all data pre-computed from the
# renderPlotly() caller in server.R.
#
# Globals accessed from global.R:
#   ssp_colors           - IPCC-inspired color palette per SSP scenario
#   ssp_scenario_labels  - Human-readable SSP scenario display labels
# ==============================================================================

# ------------------------------------------------------------------------------
# build_region_timeseries_chart()
# ------------------------------------------------------------------------------
# Builds the complete interactive Plotly chart for a clicked region's climate
# time series, including optional projection overlay and anomaly transformation.
#
# Arguments:
#   df_region        - Historical ERA5 data.frame with at minimum Year and Value
#                      columns, pre-sorted chronologically by the caller.
#   var_name         - PECD variable key (e.g., "2m_temperature",
#                      "total_precipitation"). Used to determine whether
#                      anomalies should be relative (%) or absolute.
#   var_meta         - Metadata list for the variable, from climate_variables
#                      global. Must contain $label (display name), $unit
#                      (scientific unit string), and $palette (color vector).
#   accent_color     - Hex color string for the historical line trace (e.g.,
#                      "#b2182b"). Pre-computed by the caller from var_meta.
#   region_name      - Display name of the clicked region (e.g., "Austria",
#                      "Bayern"). Used in the chart title.
#   temp_mode        - "Annual" or season name (e.g., "Winter"). Shown in
#                      the chart title for context.
#   show_proj        - Logical flag: are climate projections toggled ON in
#                      the UI? Controls the "Present Day" divider annotation.
#   proj_data        - Projection ensemble statistics data.frame with columns
#                      Year, median_val, min_val, max_val. Or NULL if no
#                      projection data is available / toggled off.
#   baseline         - Numeric baseline mean value for anomaly calculation,
#                      computed from the selected reference period. Or NULL
#                      if anomaly mode is not active.
#   display_mode     - "absolute" or "anomaly". When "anomaly" and baseline
#                      is available, values are transformed to departures.
#   ssp_scenario     - SSP scenario key (e.g., "ssp2_4_5"). Used for chart
#                      title and to look up colors in ssp_colors global.
#   reference_period - Reference period string (e.g., "1981-2010"). Shown in
#                      chart title and as an annotation band.
#   reference_period - Reference period string (e.g., "1981-2010"). Shown in
#                      chart title and as an annotation band.
#   hide_historical_line - Logical flag to omit the historical trace entirely
#                          (useful when baseline is mathematically zeroed out).
#
# Returns:
#   A fully configured plotly object ready to render in the stats drawer.
#   The object has displayModeBar disabled for a clean interface.
# ------------------------------------------------------------------------------
build_region_timeseries_chart <- function(
  df_region,
  var_name,
  var_meta,
  accent_color,
  region_name,
  temp_mode,
  show_proj,
  proj_data,
  proj_models = NULL,
  projection_style = "band",
  baseline,
  display_mode,
  ssp_scenario,
  reference_period,
  hide_historical_line = FALSE
) {
  var_label <- var_meta$label
  var_unit <- var_meta$unit

  # --------------------------------------------------------------------------
  # Determine whether anomaly transformation is needed
  # --------------------------------------------------------------------------
  # Anomaly mode requires three conditions:
  #   1. Projections must be toggled ON (show_proj == TRUE)
  #   2. Display mode set to "anomaly" in the UI
  #   3. A valid, finite baseline value computed from the reference period
  use_anomaly <- (show_proj &&
    isTRUE(display_mode == "anomaly") &&
    !is.null(baseline) &&
    is.finite(baseline))

  # Precipitation uses relative (%) anomalies because a 10mm departure means
  # very different things in a desert vs a rainforest. Temperature and other
  # variables use absolute departures (same unit as the original).
  is_relative_anomaly <- (var_name == "total_precipitation")

  # --------------------------------------------------------------------------
  # Apply anomaly transformation if active
  # --------------------------------------------------------------------------
  # This modifies df_region$Value and proj_data in place, converting raw
  # values to departures from the baseline mean.
  if (use_anomaly) {
    if (is_relative_anomaly) {
      # Precipitation: percentage change from baseline
      # Guard against near-zero baseline to avoid division-by-zero
      if (abs(baseline) > 0.001) {
        df_region$Value <- (df_region$Value - baseline) / baseline * 100
        if (!is.null(proj_data) && nrow(proj_data) > 0) {
          proj_data$median_val <- (proj_data$median_val - baseline) /
            baseline *
            100
          proj_data$min_val <- (proj_data$min_val - baseline) / baseline * 100
          proj_data$max_val <- (proj_data$max_val - baseline) / baseline * 100
        }
        if (!is.null(proj_models) && nrow(proj_models) > 0 && "Value" %in% names(proj_models)) {
          proj_models$Value <- (proj_models$Value - baseline) / baseline * 100
        }
        anomaly_unit <- "%"
      } else {
        # Baseline is essentially zero — fall back to absolute difference
        df_region$Value <- df_region$Value - baseline
        if (!is.null(proj_data) && nrow(proj_data) > 0) {
          proj_data$median_val <- proj_data$median_val - baseline
          proj_data$min_val <- proj_data$min_val - baseline
          proj_data$max_val <- proj_data$max_val - baseline
        }
        if (!is.null(proj_models) && nrow(proj_models) > 0 && "Value" %in% names(proj_models)) {
          proj_models$Value <- proj_models$Value - baseline
        }
        anomaly_unit <- var_unit
      }
    } else {
      # Temperature and other variables: absolute departure from baseline
      df_region$Value <- df_region$Value - baseline
      if (!is.null(proj_data) && nrow(proj_data) > 0) {
        proj_data$median_val <- proj_data$median_val - baseline
        proj_data$min_val <- proj_data$min_val - baseline
        proj_data$max_val <- proj_data$max_val - baseline
      }
      if (!is.null(proj_models) && nrow(proj_models) > 0 && "Value" %in% names(proj_models)) {
        proj_models$Value <- proj_models$Value - baseline
      }
      anomaly_unit <- var_unit
    }

    # Y-axis label and hover tooltip unit for anomaly mode
    y_axis_label <- sprintf(
      "Change from %s (%s)",
      reference_period,
      anomaly_unit
    )
    hover_unit <- anomaly_unit
  } else {
    # Normal absolute mode — no transformation applied
    y_axis_label <- sprintf("%s (%s)", var_label, var_unit)
    hover_unit <- var_unit
  }

  # --------------------------------------------------------------------------
  # Chart title — varies by projection state and anomaly mode
  # --------------------------------------------------------------------------
  if (!is.null(proj_data) && nrow(proj_data) > 0) {
    # Projections overlay active: show SSP scenario and reference period
    scenario_label <- ssp_scenario_labels[ssp_scenario]
    chart_title <- sprintf(
      "%s: %s (%s) \u2014 %s vs Hist (%s)",
      var_label,
      region_name,
      temp_mode,
      scenario_label,
      reference_period
    )
  } else if (use_anomaly) {
    # Anomaly mode without projection data: show reference period only
    chart_title <- sprintf(
      "%s Anomaly: %s (%s) vs Hist (%s)",
      var_label,
      region_name,
      temp_mode,
      reference_period
    )
  } else {
    # Standard historical view — simplest title
    chart_title <- sprintf("Historical Record: %s (%s)", region_name, temp_mode)
  }

  # --------------------------------------------------------------------------
  # Build the Plotly chart — start with the historical ERA5 line trace
  # --------------------------------------------------------------------------
  p <- plot_ly(source = "timeseries")

  # --- 1. Historical Line (Solid Blue/Accent) ---
  # We only add this trace if it hasn't been explicitly hidden (e.g. for dynamic wind mode)
  if (!hide_historical_line && !is.null(df_region) && nrow(df_region) > 0) {
    p <- p %>%
      add_trace(
        data = df_region,
        x = ~Year,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical (ERA5)",
        line = list(color = "#94a3b8", width = 2),
        marker = list(color = "#94a3b8", size = 4),
        hovertemplate = paste0(
          "<b>Historical</b>: %{y:.2f} ",
          hover_unit,
          "<extra></extra>"
        )
      )
  }

  # --------------------------------------------------------------------------
  # Overlay projection ensemble data if available
  # --------------------------------------------------------------------------
  if (!is.null(proj_data) && nrow(proj_data) > 0) {
    # Get the SSP-specific color palette from global config
    ssp_key <- ssp_scenario
    proj_line_color <- ssp_colors[[ssp_key]]$line
    proj_fill_color <- ssp_colors[[ssp_key]]$fill

    # Clean up any Inf or -Inf values (caused by min/max on NA)
    proj_data$min_val[!is.finite(proj_data$min_val)] <- NA
    proj_data$max_val[!is.finite(proj_data$max_val)] <- NA
    proj_data$median_val[!is.finite(proj_data$median_val)] <- NA

    # Interpolate any missing years (like 2089 in ssp3_7_0) so the polygon doesn't break
    if (any(is.na(proj_data$min_val))) {
      proj_data$min_val <- approx(
        proj_data$Year,
        proj_data$min_val,
        xout = proj_data$Year,
        rule = 2
      )$y
      proj_data$max_val <- approx(
        proj_data$Year,
        proj_data$max_val,
        xout = proj_data$Year,
        rule = 2
      )$y
      proj_data$median_val <- approx(
        proj_data$Year,
        proj_data$median_val,
        xout = proj_data$Year,
        rule = 2
      )$y
    }

    # Plot Individual Models (Spaghetti) or Solid Band (Agreement)
    if (projection_style == "spaghetti" && !is.null(proj_models) && nrow(proj_models) > 0 && "model" %in% names(proj_models)) {
      models <- unique(proj_models$model)
      
      # Generate a color gradient (nuances) from the base projection color to a lighter version
      nuance_pal <- grDevices::colorRampPalette(c(proj_line_color, "#ffffff"))(length(models) + 4)
      nuance_colors <- nuance_pal[1:length(models)]
      
      for (i in seq_along(models)) {
        mdl <- models[i]
        
        # Dynamically tidy up raw CMIP6 IDs: 'awi_cm_1_1_mr' -> 'AWI-CM-1-1-MR'
        mdl_tidy <- toupper(gsub("_", "-", mdl))
        
        # Extremely short name for legend to keep it on one line (e.g., 'AWI')
        mdl_short <- toupper(strsplit(mdl, "[_-]")[[1]][1])
        if (mdl_short == "EC") mdl_short <- "EC-Earth"
        
        mdl_data <- proj_models[proj_models$model == mdl, ]
        p <- p %>% add_trace(
          data = mdl_data,
          x = ~Year,
          y = ~Value,
          type = 'scatter',
          mode = 'lines',
          name = mdl_short,
          legendgroup = mdl,
          line = list(color = nuance_colors[i], width = 1.5),
          opacity = 0.85,
          hovertemplate = paste0(
            "<b>", mdl_tidy, "</b>: %{y:.2f} ",
            hover_unit, "<extra></extra>"
          )
        )
      }

      # Ensemble median line (dashed) on top of spaghetti
      p <- p %>% add_trace(
        data = proj_data,
        x = ~Year,
        y = ~median_val,
        type = 'scatter',
        mode = 'lines',
        name = 'Projection Median',
        line = list(color = "#ffffff", width = 3, dash = 'dash'),
        text = ~ paste0(
          "Year: ", Year,
          "<br>Median projection: ", round(median_val, 2), " ", hover_unit
        ),
        hoverinfo = 'text'
      )
    } else {
      # This prevents SVG rendering gaps that occur with fill='tonexty' when bands are narrow.
      band_x <- c(proj_data$Year, rev(proj_data$Year))
      band_y <- c(proj_data$max_val, rev(proj_data$min_val))

      p <- p %>%
        add_trace(
          x = band_x,
          y = band_y,
          type = 'scatter',
          mode = 'lines',
          name = 'Model Agreement',
          fill = 'toself',
          fillcolor = proj_fill_color,
          line = list(color = 'transparent', width = 0),
          hoverinfo = 'skip'
        ) %>%
        # Ensemble median line (dashed, colored by SSP)
        add_trace(
          data = proj_data,
          x = ~Year,
          y = ~median_val,
          type = 'scatter',
          mode = 'lines',
          name = 'Projection Median',
          line = list(color = proj_line_color, width = 2.5, dash = 'dash'),
          text = ~ paste0(
            "<b>Projection Median</b>: ", round(median_val, 2), " ", hover_unit,
            "<br><span style='font-size:10px; color:#94a3b8;'>Range: ", round(min_val, 2), " \u2013 ", round(max_val, 2), "</span>"
          ),
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  }

  # --------------------------------------------------------------------------
  # Shapes and annotations for projection context overlays
  # --------------------------------------------------------------------------
  # These visual elements help scientists interpret the historical-to-projected
  # transition and the anomaly reference frame.
  chart_shapes <- list()
  chart_annotations <- list()

  if (show_proj) {
    if (!hide_historical_line) {
      # Vertical "Present Day" divider line at the end of observed data
      # (2021 for onshore tiers, 2023 for offshore tiers)
      divider_year <- if (!is.null(df_region) && nrow(df_region) > 0) max(df_region$Year, na.rm = TRUE) else 2023

      chart_shapes <- c(
        chart_shapes,
        list(
          list(
            type = "line",
            x0 = divider_year,
            x1 = divider_year,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(
              color = "rgba(255, 255, 255, 0.35)",
              width = 1.5,
              dash = "dot"
            )
          )
        )
      )

      # "Observed | Projected" label above the divider line
      chart_annotations <- c(
        chart_annotations,
        list(
          list(
            x = divider_year,
            y = 1.02,
            yref = "paper",
            text = "Observed | Projected",
            showarrow = FALSE,
            font = list(
              family = "Inter, sans-serif",
              size = 10,
              color = "rgba(255, 255, 255, 0.50)"
            ),
            xanchor = "center"
          )
        )
      )
    }

    # Anomaly-specific visual elements: baseline line, reference period band
    if (use_anomaly) {
      # Parse reference period years for the shaded band position
      ref_years <- as.integer(strsplit(reference_period, "-")[[1]])
      ref_start <- ref_years[1]
      ref_end <- ref_years[2]

      # Horizontal baseline line at y=0 — the "no change" reference level.
      # Scientists expect this visual anchor when reading anomaly plots.
      chart_shapes <- c(
        chart_shapes,
        list(
          list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 0,
            y1 = 0,
            line = list(
              color = "rgba(255, 255, 255, 0.40)",
              width = 1.5,
              dash = "dash"
            )
          )
        )
      )

      # Vertical shaded band highlighting the reference period on the x-axis.
      # This helps scientists see which years contributed to the baseline mean.
      chart_shapes <- c(
        chart_shapes,
        list(
          list(
            type = "rect",
            x0 = ref_start,
            x1 = ref_end,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            fillcolor = "rgba(56, 189, 248, 0.06)",
            line = list(
              color = "rgba(56, 189, 248, 0.20)",
              width = 1
            )
          )
        )
      )

      # "Baseline" label next to the y=0 line
      chart_annotations <- c(
        chart_annotations,
        list(
          list(
            x = 0.01,
            xref = "paper",
            y = 0,
            text = "Baseline",
            showarrow = FALSE,
            font = list(
              family = "Inter, sans-serif",
              size = 9,
              color = "rgba(255, 255, 255, 0.45)"
            ),
            xanchor = "left",
            yanchor = "bottom",
            yshift = 4
          )
        )
      )

      # Reference period label centered above the shaded band
      ref_band_midpoint <- (ref_start + ref_end) / 2
      chart_annotations <- c(
        chart_annotations,
        list(
          list(
            x = ref_band_midpoint,
            y = 0.98,
            yref = "paper",
            text = reference_period,
            showarrow = FALSE,
            font = list(
              family = "Inter, sans-serif",
              size = 9,
              color = "rgba(56, 189, 248, 0.50)"
            ),
            xanchor = "center"
          )
        )
      )
    }
  }

  # --------------------------------------------------------------------------
  # Apply the dark glassmorphism-compatible chart layout
  # --------------------------------------------------------------------------
  # Transparent backgrounds blend with the drawer's backdrop-filter blur.
  # Muted slate colors for axes and labels match the app's design language.
  p %>%
    layout(
      title = list(
        text = chart_title,
        font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
        x = 0.05
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 40, r = 20, b = 10, l = 50),
      xaxis = list(
        title = "",
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE,
        showspikes = FALSE
      ),
      yaxis = list(
        title = list(
          text = y_axis_label,
          font = list(family = "Inter, sans-serif", color = "#94a3b8")
        ),
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE,
        showspikes = FALSE
      ),
      shapes = chart_shapes,
      annotations = chart_annotations,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.12,
        font = list(
          family = "Inter, sans-serif",
          size = 10,
          color = "#94a3b8"
        ),
        bgcolor = "rgba(0,0,0,0)"
      ),
      # Unified tooltip is extremely helpful in spaghetti mode to compare all 6 models
      # simultaneously for a specific year without having to individually hover each line.
      hovermode = "x unified",
      hoverlabel = list(
        namelength = 0,
        bgcolor = "rgba(15, 23, 42, 0.90)",
        bordercolor = "rgba(255, 255, 255, 0.15)",
        font = list(
          family = "Inter, sans-serif",
          size = 12,
          color = "#e2e8f0"
        )
      )
    ) %>%
    config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
    ) %>%
    htmlwidgets::onRender("
      function(el) {
        var spikeId = 'custom-timeseries-spike';
        var spike = document.getElementById(spikeId);
        if (!spike) {
          spike = document.createElement('div');
          spike.id = spikeId;
          spike.style.position = 'absolute';
          spike.style.display = 'none';
          spike.style.zIndex = '9998';
          spike.style.width = '1px';
          spike.style.borderLeft = '1px dashed rgba(255, 255, 255, 0.4)';
          spike.style.pointerEvents = 'none';
          spike.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          document.body.appendChild(spike);
        }

        // Hide Plotly's native spike lines via CSS
        var style = document.getElementById('hide-native-spikes');
        if (!style) {
          style = document.createElement('style');
          style.id = 'hide-native-spikes';
          style.textContent = '.spikeline { display: none !important; }';
          document.head.appendChild(style);
        }

        el.on('plotly_hover', function(d) {
          if (!d.points || d.points.length === 0) return;
          var rect = el.getBoundingClientRect();
          var xAxis = d.points[0].xaxis;
          if (xAxis && d.points[0].x !== undefined) {
            var ml = el._fullLayout ? el._fullLayout.margin.l : 50;
            var mt = el._fullLayout ? el._fullLayout.margin.t : 50;
            var mb = el._fullLayout ? el._fullLayout.margin.b : 20;

            var leftOffset = xAxis.l2p(d.points[0].x);
            var absoluteLeft = window.scrollX + rect.left + ml + leftOffset;
            var absoluteTop = window.scrollY + rect.top + mt;
            var plotHeight = el._fullLayout ? (el._fullLayout.height - mt - mb) : (rect.height - 70);

            spike.style.display = 'block';
            spike.style.left = absoluteLeft + 'px';
            spike.style.top = absoluteTop + 'px';
            spike.style.height = plotHeight + 'px';
          }
        });

        el.on('plotly_unhover', function(d) {
          spike.style.display = 'none';
        });
      }
    ")
}

#' Build Seasonality Profile (Monthly Cycle) Chart
#' @param df_hist DataFrame with historical monthly data (Month, Value)
#' @param df_proj DataFrame with projected monthly data (Month, Value)
#' @param var_name The climate variable (for label)
#' @param region_name The selected region name
#' @param ssp_scenario The selected scenario string
#' @param reference_period Historical reference period string
#' @param target_period Projection target period string
#' @param accent_color Main color for the variable
#' @param hist_note Optional text to append to the hover tooltip for context
build_seasonality_plotly <- function(
  df_hist,
  df_proj,
  var_name,
  region_name,
  ssp_scenario,
  reference_period,
  target_period,
  accent_color,
  hist_note = ""
) {
  # Get axis label for variable from the global climate_variables list
  var_meta <- climate_variables[[var_name]]
  var_label <- var_meta$label
  hover_unit <- var_meta$unit

  # Title format matching the timeseries chart style
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    ssp_label <- ssp_scenario_labels[ssp_scenario]
    chart_title <- sprintf(
      "%s Seasonal Profile: %s \u2014 %s (%s) vs Hist (%s)",
      var_label,
      region_name,
      ssp_label,
      target_period,
      reference_period
    )
  } else {
    chart_title <- sprintf(
      "%s Seasonal Profile: %s \u2014 Historical (%s)",
      var_label,
      region_name,
      reference_period
    )
  }

  if (nchar(hist_note) > 0) {
    chart_title <- paste0(
      chart_title,
      "<br><sup style='color:#94a3b8;'><i>",
      hist_note,
      "</i></sup>"
    )
  }

  month_labels <- c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )

  # Guard: If no data is available, return a clean empty plot to avoid Plotly warnings
  if (
    (is.null(df_hist) || nrow(df_hist) == 0) &&
      (is.null(df_proj) || nrow(df_proj) == 0)
  ) {
    return(
      plot_ly() %>%
        layout(
          title = list(
            text = paste("No data available for", region_name),
            font = list(
              family = "Inter, sans-serif",
              size = 12,
              color = "#94a3b8"
            ),
            x = 0.05
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(t = 50, r = 20, b = 40, l = 50)
        )
    )
  }

  p <- plot_ly(source = "seasonality")
  
  # Historical Trace
  if (!is.null(df_hist) && nrow(df_hist) > 0) {
    p <- p %>%
      add_trace(
        data = df_hist,
        x = ~Month,
        y = ~Value,
        type = "box",
        name = paste("Historical", reference_period),
        marker = list(color = "#94a3b8"),
        line = list(color = "#94a3b8"),
        hoverinfo = "none"
      )
  }

  # Projection Trace
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    ssp_key <- ssp_scenario
    proj_line_color <- ssp_colors[[ssp_key]]$line
    ssp_label <- ssp_scenario_labels[ssp_scenario]

    p <- p %>%
      add_trace(
        data = df_proj,
        x = ~Month,
        y = ~Value,
        type = "box",
        name = paste(ssp_label, target_period),
        marker = list(color = proj_line_color),
        line = list(color = proj_line_color),
        hoverinfo = "none"
      )
  }

  p <- p %>%
    layout(
      title = list(
        text = chart_title,
        font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
        x = 0.05
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 1:12,
        ticktext = month_labels,
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE,
        showspikes = FALSE
      ),
      yaxis = list(
        title = paste0(var_label, " (", var_meta$unit, ")"),
        titlefont = list(
          family = "Inter, sans-serif",
          color = "#94a3b8",
          size = 12
        ),
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zerolinecolor = "rgba(255, 255, 255, 0.1)",
        showspikes = FALSE
      ),
      boxmode = "group",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 50, r = 20, b = 20, l = 50),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.10,
        xanchor = "center",
        font = list(family = "Inter, sans-serif", size = 11, color = "#94a3b8"),
        bgcolor = "rgba(0,0,0,0)"
      ),
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
    ) %>%
    htmlwidgets::onRender("
      function(el) {
        var tooltipId = 'custom-seasonality-tooltip';
        var spikeId = 'custom-seasonality-spike';
        
        var tooltip = document.getElementById(tooltipId);
        if (!tooltip) {
          tooltip = document.createElement('div');
          tooltip.id = tooltipId;
          tooltip.style.position = 'absolute';
          tooltip.style.display = 'none';
          tooltip.style.zIndex = '9999';
          tooltip.style.background = 'rgba(15, 23, 42, 0.95)';
          tooltip.style.border = '1px solid rgba(255, 255, 255, 0.15)';
          tooltip.style.borderRadius = '6px';
          tooltip.style.padding = '12px';
          tooltip.style.color = '#e2e8f0';
          tooltip.style.fontFamily = 'Inter, sans-serif';
          tooltip.style.fontSize = '12px';
          tooltip.style.pointerEvents = 'none';
          tooltip.style.boxShadow = '0 4px 6px -1px rgba(0, 0, 0, 0.5)';
          
          // Add a fast CSS transition so it glides smoothly between boxes
          tooltip.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1), top 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          
          document.body.appendChild(tooltip);
        }
        
        var spike = document.getElementById(spikeId);
        if (!spike) {
          spike = document.createElement('div');
          spike.id = spikeId;
          spike.style.position = 'absolute';
          spike.style.display = 'none';
          spike.style.zIndex = '9998'; // Just below tooltip
          spike.style.width = '1px';
          spike.style.borderLeft = '1px dashed rgba(255, 255, 255, 0.4)';
          spike.style.pointerEvents = 'none';
          spike.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          document.body.appendChild(spike);
        }

        el.on('plotly_hover', function(d) {
          var pts = d.points;
          if (!pts || pts.length === 0) return;
          
          // For box traces, pts[0].x is the categorical x value (month number 1-12)
          // pts[0].pointNumber is the raw data point index, NOT the box index
          var monthNum = pts[0].x;
          if (monthNum === undefined) return;
          
          // Convert 1-based month number to 0-based array index
          var idx = monthNum - 1;
          
          var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
          var monthName = (idx >= 0 && idx < 12) ? months[idx] : String(monthNum);
          
          var title = '<div style=\"margin-bottom: 8px; font-weight: 600; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 4px;\">' + monthName + '</div>';
          
          var cols = [];
          // Loop through all traces in calcdata to pull box stats for this month index
          if (el.calcdata) {
            for (var c = 0; c < el.calcdata.length; c++) {
              var traceData = el.calcdata[c];
              if (!traceData) continue;
              
              var calcPt = traceData[idx];
              if (!calcPt) continue;
              
              // Ensure we only process box plot traces
              if (calcPt.trace && calcPt.trace.type !== 'box') continue;
              
              var name = (calcPt.trace && calcPt.trace.name) ? calcPt.trace.name : 'Data';
              
              // Simplify trace names but PRESERVE periods
              // e.g. 'Historical 1991-2020' -> 'Hist 1991-2020'
              // e.g. 'SSP5-8.5 2041-2060' -> 'SSP5-8.5 2041-2060'
              var displayName = 'Data';
              if (name.indexOf('Historical') !== -1) {
                displayName = name.replace('Historical', 'Hist');
              } else if (name.indexOf('SSP') !== -1) {
                displayName = name; // Leave the SSP name and period intact
              } else {
                displayName = name;
              }
              
              var max = calcPt.max !== undefined ? calcPt.max.toFixed(2) : '-';
              var q3 = calcPt.q3 !== undefined ? calcPt.q3.toFixed(2) : '-';
              var med = calcPt.med !== undefined ? calcPt.med.toFixed(2) : '-';
              var q1 = calcPt.q1 !== undefined ? calcPt.q1.toFixed(2) : '-';
              var min = calcPt.min !== undefined ? calcPt.min.toFixed(2) : '-';
              
              if (max === '-' && med === '-' && min === '-') continue;
              
              cols.push({
                name: displayName,
                max: max,
                q3: q3,
                med: med,
                q1: q1,
                min: min
              });
            }
          }
          
          // Construct a single unified HTML table for the comparisons
          var tableHtml = '<table style=\"width: 100%; font-variant-numeric: tabular-nums; border-collapse: collapse;\">';
          
          // Header row with trace names
          tableHtml += '<tr><th style=\"text-align: left; padding-right: 16px; font-weight: normal; color: #94a3b8;\"></th>';
          for (var i = 0; i < cols.length; i++) {
            tableHtml += '<th style=\"text-align: right; padding-left: 20px; font-weight: 600; color: #fff;\">' + cols[i].name + '</th>';
          }
          tableHtml += '</tr>';
          
          // Data rows for each statistical boundary
          var rowConfigs = [
            { key: 'max', label: 'Max' },
            { key: 'q3', label: '75%' },
            { key: 'med', label: 'Median', bold: true },
            { key: 'q1', label: '25%' },
            { key: 'min', label: 'Min' }
          ];
          
          for (var r = 0; r < rowConfigs.length; r++) {
            var row = rowConfigs[r];
            var style = row.bold ? 'font-weight: 600; color: #fff;' : 'color: #e2e8f0;';
            var lblStyle = 'color: #94a3b8; padding-right: 16px;';
            if (row.bold) lblStyle += ' font-weight: 600;';
            
            tableHtml += '<tr style=\"' + style + '\"><td style=\"' + lblStyle + '\">' + row.label + '</td>';
            for (var i = 0; i < cols.length; i++) {
              tableHtml += '<td style=\"text-align: right; padding-left: 20px;\">' + cols[i][row.key] + '</td>';
            }
            tableHtml += '</tr>';
          }
          tableHtml += '</table>';
          
          tooltip.innerHTML = title + tableHtml;
          
          // Center the tooltip vertically relative to the plot container
          var rect = el.getBoundingClientRect();
          var centerY = window.scrollY + rect.top + (rect.height / 2);
          
          // Make visible to measure dimensions
          tooltip.style.display = 'block';
          var tooltipWidth = tooltip.offsetWidth || 200;
          var tooltipHeight = tooltip.offsetHeight || 150;
          
          var evt = d.event;
          if (evt) {
            var leftPos = evt.pageX + 15;
            // Check if it overflows the right side of the viewport
            if (evt.clientX + 15 + tooltipWidth > window.innerWidth) {
              // Flip to the left of the cursor
              leftPos = evt.pageX - tooltipWidth - 15;
            }
            tooltip.style.left = leftPos + 'px';
            
            // Position the custom spike line
            var xAxis = d.points[0].xaxis;
            if (xAxis && d.points[0].x !== undefined) {
              var ml = el._fullLayout ? el._fullLayout.margin.l : 50;
              var mt = el._fullLayout ? el._fullLayout.margin.t : 50;
              var mb = el._fullLayout ? el._fullLayout.margin.b : 20;
              
              var leftOffset = xAxis.l2p(d.points[0].x);
              var absoluteLeft = window.scrollX + rect.left + ml + leftOffset;
              var absoluteTop = window.scrollY + rect.top + mt;
              var plotHeight = el._fullLayout ? (el._fullLayout.height - mt - mb) : (rect.height - 70);
              
              spike.style.display = 'block';
              spike.style.left = absoluteLeft + 'px';
              spike.style.top = absoluteTop + 'px';
              spike.style.height = plotHeight + 'px';
            }
          }
          
          tooltip.style.top = (centerY - tooltipHeight / 2) + 'px';
        });
        
        el.on('plotly_unhover', function(d) {
          tooltip.style.display = 'none';
          spike.style.display = 'none';
        });
      }
    ")

  return(p)
}

# ==============================================================================
# All Scenarios Plotting Logic
# ==============================================================================

build_all_scenarios_timeseries_chart <- function(
  df_region,
  var_name,
  var_meta,
  accent_color,
  region_name,
  proj_ensemble,
  hide_historical_line = FALSE,
  reference_period = "",
  baseline = NULL,
  display_mode = "absolute"
) {
  var_label <- var_meta$label
  var_unit <- var_meta$unit

  # --------------------------------------------------------------------------
  # Apply anomaly transformation if active
  # --------------------------------------------------------------------------
  use_anomaly <- (isTRUE(display_mode == "anomaly") && !is.null(baseline) && is.finite(baseline))
  is_relative_anomaly <- (var_name == "total_precipitation")

  if (use_anomaly) {
    if (is_relative_anomaly) {
      if (abs(baseline) > 0.001) {
        if (!is.null(df_region)) df_region$Value <- (df_region$Value - baseline) / baseline * 100
        if (!is.null(proj_ensemble) && nrow(proj_ensemble) > 0) {
          proj_ensemble$median_val <- (proj_ensemble$median_val - baseline) / baseline * 100
        }
        var_unit <- "%"
      } else {
        if (!is.null(df_region)) df_region$Value <- df_region$Value - baseline
        if (!is.null(proj_ensemble) && nrow(proj_ensemble) > 0) {
          proj_ensemble$median_val <- proj_ensemble$median_val - baseline
        }
      }
    } else {
      if (!is.null(df_region)) df_region$Value <- df_region$Value - baseline
      if (!is.null(proj_ensemble) && nrow(proj_ensemble) > 0) {
        proj_ensemble$median_val <- proj_ensemble$median_val - baseline
      }
    }
  }

  if (use_anomaly) {
    y_axis_label <- sprintf("Change from %s (%s)", reference_period, var_unit)
  } else {
    y_axis_label <- sprintf("%s (%s)", var_label, var_unit)
  }

  hover_unit <- if (nchar(var_unit) > 0) paste0(" ", var_unit) else ""

  if (nchar(reference_period) > 0) {
    chart_title <- sprintf(
      "%s: %s \u2014 All Scenarios vs Hist (%s)",
      var_label,
      region_name,
      reference_period
    )
  } else {
    chart_title <- sprintf(
      "%s: %s \u2014 All Scenarios",
      var_label,
      region_name
    )
  }

  p <- plot_ly(source = "all_timeseries")

  # Historical Line
  if (!hide_historical_line && !is.null(df_region) && nrow(df_region) > 0) {
    p <- p %>%
      add_trace(
        data = df_region,
        x = ~Year,
        y = ~Value,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical (ERA5)",
        line = list(color = "#94a3b8", width = 2),
        marker = list(color = "#94a3b8", size = 4),
        hovertemplate = paste0(
          "<b>Historical</b>: %{y:.2f} ",
          hover_unit,
          "<extra></extra>"
        )
      )
  }

  # Projection Medians
  if (!is.null(proj_ensemble) && nrow(proj_ensemble) > 0) {
    scenarios <- unique(proj_ensemble$scenario)
    # Sort scenarios logically
    scenarios <- sort(scenarios)

    for (s in scenarios) {
      s_data <- proj_ensemble[proj_ensemble$scenario == s, ]
      s_color <- ssp_colors[[s]]$line
      s_label <- ssp_scenario_labels[[s]]

      p <- p %>%
        add_trace(
          data = s_data,
          x = ~Year,
          y = ~median_val,
          type = "scatter",
          mode = "lines",
          name = s_label,
          line = list(color = s_color, width = 2),
          hovertemplate = paste0(
            "<b>", s_label, "</b>: %{y:.2f} ",
            hover_unit,
            "<extra></extra>"
          )
        )
    }
  }

  # --------------------------------------------------------------------------
  # Shapes and annotations for projection context overlays
  # --------------------------------------------------------------------------
  chart_shapes <- list()
  chart_annotations <- list()

  if (!hide_historical_line && !is.null(proj_ensemble) && nrow(proj_ensemble) > 0) {
    divider_year <- if (!is.null(df_region) && nrow(df_region) > 0) max(df_region$Year, na.rm = TRUE) else 2023

    chart_shapes <- list(
      list(
        type = "line",
        x0 = divider_year,
        x1 = divider_year,
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(
          color = "rgba(255, 255, 255, 0.35)",
          width = 1.5,
          dash = "dot"
        )
      )
    )

    chart_annotations <- list(
      list(
        x = divider_year,
        y = 1.02,
        yref = "paper",
        text = "Observed | Projected",
        showarrow = FALSE,
        font = list(
          family = "Inter, sans-serif",
          size = 10,
          color = "rgba(255, 255, 255, 0.50)"
        ),
        xanchor = "center"
      )
    )
  }

  p <- p %>%
    layout(
      shapes = chart_shapes,
      annotations = chart_annotations,
      title = list(
        text = chart_title,
        font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
        x = 0.05,
        y = 0.95,
        xanchor = "left"
      ),
      hovermode = "x unified",
      hoverlabel = list(
        namelength = 0,
        bgcolor = "rgba(15, 23, 42, 0.90)",
        bordercolor = "rgba(255, 255, 255, 0.15)",
        font = list(
          family = "Inter, sans-serif",
          size = 12,
          color = "#e2e8f0"
        )
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.15,
        xanchor = "center",
        yanchor = "top",
        font = list(family = "Inter, sans-serif", color = "#cbd5e1", size = 11)
      ),
      margin = list(t = 40, r = 20, b = 10, l = 50),
      xaxis = list(
        title = "",
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE,
        showspikes = FALSE
      ),
      yaxis = list(
        title = list(
          text = y_axis_label,
          font = list(family = "Inter, sans-serif", color = "#cbd5e1", size = 12)
        ),
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zerolinecolor = "rgba(255, 255, 255, 0.2)"
      )
    ) %>%
    config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
    ) %>%
    htmlwidgets::onRender("
      function(el) {
        var spikeId = 'custom-all-timeseries-spike';
        var spike = document.getElementById(spikeId);
        if (!spike) {
          spike = document.createElement('div');
          spike.id = spikeId;
          spike.style.position = 'absolute';
          spike.style.display = 'none';
          spike.style.zIndex = '9998';
          spike.style.width = '1px';
          spike.style.borderLeft = '1px dashed rgba(255, 255, 255, 0.4)';
          spike.style.pointerEvents = 'none';
          spike.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          document.body.appendChild(spike);
        }

        // Hide Plotly's native spike lines via CSS
        var style = document.getElementById('hide-native-spikes');
        if (!style) {
          style = document.createElement('style');
          style.id = 'hide-native-spikes';
          style.textContent = '.spikeline { display: none !important; }';
          document.head.appendChild(style);
        }

        el.on('plotly_hover', function(d) {
          if (!d.points || d.points.length === 0) return;
          var rect = el.getBoundingClientRect();
          var xAxis = d.points[0].xaxis;
          if (xAxis && d.points[0].x !== undefined) {
            var ml = el._fullLayout ? el._fullLayout.margin.l : 50;
            var mt = el._fullLayout ? el._fullLayout.margin.t : 50;
            var mb = el._fullLayout ? el._fullLayout.margin.b : 20;

            var leftOffset = xAxis.l2p(d.points[0].x);
            var absoluteLeft = window.scrollX + rect.left + ml + leftOffset;
            var absoluteTop = window.scrollY + rect.top + mt;
            var plotHeight = el._fullLayout ? (el._fullLayout.height - mt - mb) : (rect.height - 70);

            spike.style.display = 'block';
            spike.style.left = absoluteLeft + 'px';
            spike.style.top = absoluteTop + 'px';
            spike.style.height = plotHeight + 'px';
          }
        });

        el.on('plotly_unhover', function(d) {
          spike.style.display = 'none';
        });
      }
    ")

  return(p)
}

build_all_scenarios_seasonality_chart <- function(
  df_hist,
  df_proj,
  var_name,
  var_meta,
  accent_color,
  region_name,
  reference_period = "",
  target_period = ""
) {
  var_label <- var_meta$label
  var_unit <- var_meta$unit

  if (nchar(target_period) > 0 && nchar(reference_period) > 0) {
    chart_title <- sprintf(
      "%s Seasonal Profile: %s \u2014 All Scenarios (%s) vs Hist (%s)",
      var_label,
      region_name,
      target_period,
      reference_period
    )
  } else if (nchar(reference_period) > 0) {
    chart_title <- sprintf(
      "%s Seasonal Profile: %s \u2014 Historical (%s)",
      var_label,
      region_name,
      reference_period
    )
  } else {
    chart_title <- sprintf(
      "%s Seasonal Profile: %s \u2014 All Scenarios",
      var_label,
      region_name
    )
  }

  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  # Guard
  if ((is.null(df_hist) || nrow(df_hist) == 0) && (is.null(df_proj) || nrow(df_proj) == 0)) {
    return(plot_ly() %>% layout(title = list(text = "No data available")))
  }

  p <- plot_ly(source = "all_seasonality")

  # Historical Boxplots
  if (!is.null(df_hist) && nrow(df_hist) > 0) {
    p <- p %>%
      add_trace(
        data = df_hist,
        x = ~Month,
        y = ~Value,
        type = "box",
        name = "Historical",
        marker = list(color = "#94a3b8"),
        line = list(color = "#94a3b8"),
        fillcolor = "rgba(0,0,0,0)",
        hoverinfo = "none"
      )
  }

  # Projection Boxplots
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    scenarios <- unique(df_proj$scenario)
    scenarios <- sort(scenarios)

    for (s in scenarios) {
      s_data <- df_proj[df_proj$scenario == s, ]
      s_color <- ssp_colors[[s]]$line
      s_label <- ssp_scenario_labels[[s]]

      p <- p %>%
        add_trace(
          data = s_data,
          x = ~Month,
          y = ~Value,
          type = "box",
          name = s_label,
          marker = list(color = s_color),
          line = list(color = s_color),
          fillcolor = "rgba(0,0,0,0)",
          hoverinfo = "none"
        )
    }
  }

  p <- p %>%
    layout(
      boxmode = "group",
      title = list(
        text = chart_title,
        font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
        x = 0.05,
        y = 0.95,
        xanchor = "left"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.15,
        xanchor = "center",
        yanchor = "top",
        font = list(family = "Inter, sans-serif", color = "#cbd5e1", size = 11)
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 1:12,
        ticktext = month_labels,
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE,
        showspikes = FALSE
      ),
      yaxis = list(
        title = list(
          text = var_unit,
          font = list(family = "Inter, sans-serif", color = "#cbd5e1", size = 12)
        ),
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)"
      ),
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
    ) %>%
    htmlwidgets::onRender("
      function(el) {
        var tooltipId = 'custom-all-seasonality-tooltip';
        var spikeId = 'custom-all-seasonality-spike';
        
        var tooltip = document.getElementById(tooltipId);
        if (!tooltip) {
          tooltip = document.createElement('div');
          tooltip.id = tooltipId;
          tooltip.style.position = 'absolute';
          tooltip.style.display = 'none';
          tooltip.style.zIndex = '9999';
          tooltip.style.background = 'rgba(15, 23, 42, 0.95)';
          tooltip.style.border = '1px solid rgba(255, 255, 255, 0.15)';
          tooltip.style.borderRadius = '6px';
          tooltip.style.padding = '12px';
          tooltip.style.color = '#e2e8f0';
          tooltip.style.fontFamily = 'Inter, sans-serif';
          tooltip.style.fontSize = '12px';
          tooltip.style.pointerEvents = 'none';
          tooltip.style.boxShadow = '0 4px 6px -1px rgba(0, 0, 0, 0.5)';
          
          // Add a fast CSS transition so it glides smoothly between boxes
          tooltip.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1), top 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          
          document.body.appendChild(tooltip);
        }
        
        var spike = document.getElementById(spikeId);
        if (!spike) {
          spike = document.createElement('div');
          spike.id = spikeId;
          spike.style.position = 'absolute';
          spike.style.display = 'none';
          spike.style.zIndex = '9998'; // Just below tooltip
          spike.style.width = '1px';
          spike.style.borderLeft = '1px dashed rgba(255, 255, 255, 0.4)';
          spike.style.pointerEvents = 'none';
          spike.style.transition = 'left 0.12s cubic-bezier(0.25, 1, 0.5, 1)';
          document.body.appendChild(spike);
        }

        el.on('plotly_hover', function(d) {
          var pts = d.points;
          if (!pts || pts.length === 0) return;
          
          // For box traces, pts[0].x is the categorical x value (month number 1-12)
          var monthNum = pts[0].x;
          if (monthNum === undefined) return;
          
          var idx = monthNum - 1;
          
          var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
          var monthName = (idx >= 0 && idx < 12) ? months[idx] : String(monthNum);
          
          var title = '<div style=\"margin-bottom: 8px; font-weight: 600; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 4px;\">' + monthName + '</div>';
          
          var cols = [];
          if (el.calcdata) {
            for (var c = 0; c < el.calcdata.length; c++) {
              var traceData = el.calcdata[c];
              if (!traceData) continue;
              
              var calcPt = traceData[idx];
              if (!calcPt) continue;
              
              if (calcPt.trace && calcPt.trace.type !== 'box') continue;
              
              var name = (calcPt.trace && calcPt.trace.name) ? calcPt.trace.name : 'Data';
              
              var displayName = 'Data';
              if (name.indexOf('Historical') !== -1) {
                displayName = 'Hist';
              } else if (name.indexOf('SSP') !== -1) {
                var sspIdx = name.indexOf('SSP');
                if (sspIdx !== -1) {
                  var chunk = name.substring(sspIdx, sspIdx + 9);
                  displayName = chunk.split(' ')[0];
                } else {
                  displayName = name.split(' ')[0];
                }
              } else {
                displayName = name;
              }
              
              var max = calcPt.max !== undefined ? calcPt.max.toFixed(2) : '-';
              var q3 = calcPt.q3 !== undefined ? calcPt.q3.toFixed(2) : '-';
              var med = calcPt.med !== undefined ? calcPt.med.toFixed(2) : '-';
              var q1 = calcPt.q1 !== undefined ? calcPt.q1.toFixed(2) : '-';
              var min = calcPt.min !== undefined ? calcPt.min.toFixed(2) : '-';
              
              if (max === '-' && med === '-' && min === '-') continue;
              
              cols.push({
                name: displayName,
                max: max,
                q3: q3,
                med: med,
                q1: q1,
                min: min
              });
            }
          }
          
          var tableHtml = '<table style=\"width: 100%; font-variant-numeric: tabular-nums; border-collapse: collapse;\">';
          tableHtml += '<tr><th style=\"text-align: left; padding-right: 16px; font-weight: normal; color: #94a3b8;\"></th>';
          for (var i = 0; i < cols.length; i++) {
            tableHtml += '<th style=\"text-align: right; padding-left: 20px; font-weight: 600; color: #fff;\">' + cols[i].name + '</th>';
          }
          tableHtml += '</tr>';
          
          var rowConfigs = [
            { key: 'max', label: 'Max' },
            { key: 'q3', label: '75%' },
            { key: 'med', label: 'Median', bold: true },
            { key: 'q1', label: '25%' },
            { key: 'min', label: 'Min' }
          ];
          
          for (var r = 0; r < rowConfigs.length; r++) {
            var row = rowConfigs[r];
            var style = row.bold ? 'font-weight: 600; color: #fff;' : 'color: #e2e8f0;';
            var lblStyle = 'color: #94a3b8; padding-right: 16px;';
            if (row.bold) lblStyle += ' font-weight: 600;';
            
            tableHtml += '<tr style=\"' + style + '\"><td style=\"' + lblStyle + '\">' + row.label + '</td>';
            for (var i = 0; i < cols.length; i++) {
              tableHtml += '<td style=\"text-align: right; padding-left: 20px;\">' + cols[i][row.key] + '</td>';
            }
            tableHtml += '</tr>';
          }
          tableHtml += '</table>';
          
          tooltip.innerHTML = title + tableHtml;
          
          var rect = el.getBoundingClientRect();
          var centerY = window.scrollY + rect.top + (rect.height / 2);
          
          tooltip.style.display = 'block';
          var tooltipWidth = tooltip.offsetWidth || 200;
          var tooltipHeight = tooltip.offsetHeight || 150;
          
          var evt = d.event;
          if (evt) {
            var leftPos = evt.pageX + 15;
            if (evt.clientX + 15 + tooltipWidth > window.innerWidth) {
              leftPos = evt.pageX - tooltipWidth - 15;
            }
            tooltip.style.left = leftPos + 'px';
            
            // Position the custom spike line
            var xAxis = d.points[0].xaxis;
            if (xAxis && d.points[0].x !== undefined) {
              var ml = el._fullLayout ? el._fullLayout.margin.l : 50;
              var mt = el._fullLayout ? el._fullLayout.margin.t : 50;
              var mb = el._fullLayout ? el._fullLayout.margin.b : 20;
              
              var leftOffset = xAxis.l2p(d.points[0].x);
              var absoluteLeft = window.scrollX + rect.left + ml + leftOffset;
              var absoluteTop = window.scrollY + rect.top + mt;
              var plotHeight = el._fullLayout ? (el._fullLayout.height - mt - mb) : (rect.height - 70);
              
              spike.style.display = 'block';
              spike.style.left = absoluteLeft + 'px';
              spike.style.top = absoluteTop + 'px';
              spike.style.height = plotHeight + 'px';
            }
          }
          
          tooltip.style.top = (centerY - tooltipHeight / 2) + 'px';
        });
        
        el.on('plotly_unhover', function(d) {
          tooltip.style.display = 'none';
          spike.style.display = 'none';
        });
      }
    ")

  return(p)
}
