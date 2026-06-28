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
build_region_timeseries_chart <- function(df_region, var_name, var_meta,
                                         accent_color, region_name,
                                         temp_mode, show_proj, proj_data,
                                         baseline, display_mode,
                                         ssp_scenario, reference_period,
                                         hide_historical_line = FALSE) {

  var_label <- var_meta$label
  var_unit  <- var_meta$unit

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
          proj_data$median_val <- (proj_data$median_val - baseline) / baseline * 100
          proj_data$min_val    <- (proj_data$min_val - baseline) / baseline * 100
          proj_data$max_val    <- (proj_data$max_val - baseline) / baseline * 100
        }
        anomaly_unit <- "%"
      } else {
        # Baseline is essentially zero — fall back to absolute difference
        df_region$Value <- df_region$Value - baseline
        if (!is.null(proj_data) && nrow(proj_data) > 0) {
          proj_data$median_val <- proj_data$median_val - baseline
          proj_data$min_val    <- proj_data$min_val - baseline
          proj_data$max_val    <- proj_data$max_val - baseline
        }
        anomaly_unit <- var_unit
      }
    } else {
      # Temperature and other variables: absolute departure from baseline
      df_region$Value <- df_region$Value - baseline
      if (!is.null(proj_data) && nrow(proj_data) > 0) {
        proj_data$median_val <- proj_data$median_val - baseline
        proj_data$min_val    <- proj_data$min_val - baseline
        proj_data$max_val    <- proj_data$max_val - baseline
      }
      anomaly_unit <- var_unit
    }

    # Y-axis label and hover tooltip unit for anomaly mode
    y_axis_label <- sprintf("Change from %s (%s)", reference_period, anomaly_unit)
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
    chart_title <- sprintf("%s: %s (%s) \u2014 %s vs %s",
                           var_label, region_name, temp_mode,
                           scenario_label, reference_period)
  } else if (use_anomaly) {
    # Anomaly mode without projection data: show reference period only
    chart_title <- sprintf("%s Anomaly: %s (%s) vs %s",
                           var_label, region_name, temp_mode, reference_period)
  } else {
    # Standard historical view — simplest title
    chart_title <- sprintf("Historical Record: %s (%s)", region_name, temp_mode)
  }

  # --------------------------------------------------------------------------
  # Build the Plotly chart — start with the historical ERA5 line trace
  # --------------------------------------------------------------------------
  p <- plot_ly()

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
        line = list(color = accent_color, width = 2),
        marker = list(color = accent_color, size = 4),
        hovertemplate = paste0("<b>Historical</b><br>Year: %{x}<br>Value: %{y:.2f} ", hover_unit, "<extra></extra>")
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

    # Add the model agreement envelope (min-max band).
    # Plotly's fill='tonexty' requires traces in a specific order:
    # first the bottom boundary (invisible line), then the top boundary
    # with fill referencing the previous trace.
    p <- p %>%
      # Bottom boundary of the envelope (invisible line)
      add_trace(
        data = proj_data,
        x = ~Year,
        y = ~min_val,
        type = 'scatter',
        mode = 'lines',
        name = 'Model Agreement (min)',
        line = list(color = 'transparent', width = 0),
        showlegend = FALSE,
        hoverinfo = 'skip'
      ) %>%
      # Top boundary of the envelope, filled down to the min trace
      add_trace(
        data = proj_data,
        x = ~Year,
        y = ~max_val,
        type = 'scatter',
        mode = 'lines',
        name = 'Model Agreement',
        fill = 'tonexty',
        fillcolor = proj_fill_color,
        line = list(color = 'transparent', width = 0),
        text = ~paste0("Year: ", Year,
                       "<br>Model range: ", round(min_val, 2),
                       " \u2013 ", round(max_val, 2), " ", hover_unit),
        hoverinfo = 'text'
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
        text = ~paste0("Year: ", Year,
                       "<br>Median projection: ", round(median_val, 2),
                       " ", hover_unit),
        hoverinfo = 'text'
      )
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
      # Vertical "Present Day" divider line at 2023 — marks the boundary
      # between observed ERA5 data and projected CMIP6 model data
      chart_shapes <- c(chart_shapes, list(
        list(
          type = "line",
          x0 = 2023, x1 = 2023,
          y0 = 0, y1 = 1,
          yref = "paper",
          line = list(
            color = "rgba(255, 255, 255, 0.35)",
            width = 1.5,
            dash = "dot"
          )
        )
      ))

      # "Observed | Projected" label above the divider line
      chart_annotations <- c(chart_annotations, list(
        list(
          x = 2023,
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
      ))
    }

    # Anomaly-specific visual elements: baseline line, reference period band
    if (use_anomaly) {

      # Parse reference period years for the shaded band position
      ref_years <- as.integer(strsplit(reference_period, "-")[[1]])
      ref_start <- ref_years[1]
      ref_end   <- ref_years[2]

      # Horizontal baseline line at y=0 — the "no change" reference level.
      # Scientists expect this visual anchor when reading anomaly plots.
      chart_shapes <- c(chart_shapes, list(
        list(
          type = "line",
          x0 = 0, x1 = 1,
          xref = "paper",
          y0 = 0, y1 = 0,
          line = list(
            color = "rgba(255, 255, 255, 0.40)",
            width = 1.5,
            dash = "dash"
          )
        )
      ))

      # Vertical shaded band highlighting the reference period on the x-axis.
      # This helps scientists see which years contributed to the baseline mean.
      chart_shapes <- c(chart_shapes, list(
        list(
          type = "rect",
          x0 = ref_start, x1 = ref_end,
          y0 = 0, y1 = 1,
          yref = "paper",
          fillcolor = "rgba(56, 189, 248, 0.06)",
          line = list(
            color = "rgba(56, 189, 248, 0.20)",
            width = 1
          )
        )
      ))

      # "Baseline" label next to the y=0 line
      chart_annotations <- c(chart_annotations, list(
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
      ))

      # Reference period label centered above the shaded band
      ref_band_midpoint <- (ref_start + ref_end) / 2
      chart_annotations <- c(chart_annotations, list(
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
      ))
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
      margin = list(t = 50, r = 20, b = 40, l = 50),
      xaxis = list(
        title = "",
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE
      ),
      yaxis = list(
        title = list(text = y_axis_label, font = list(family = "Inter, sans-serif", color = "#94a3b8")),
        tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
        gridcolor = "rgba(255, 255, 255, 0.05)",
        zeroline = FALSE
      ),
      shapes = chart_shapes,
      annotations = chart_annotations,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        font = list(
          family = "Inter, sans-serif",
          size = 11,
          color = "#94a3b8"
        ),
        bgcolor = "rgba(0,0,0,0)"
      ),
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = "rgba(15, 23, 42, 0.90)",
        bordercolor = "rgba(255, 255, 255, 0.15)",
        font = list(
          family = "Inter, sans-serif",
          size = 12,
          color = "#e2e8f0"
        )
      )
    ) %>%
    config(displayModeBar = FALSE)
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
build_seasonality_plotly <- function(
  df_hist, df_proj, var_name, region_name, 
  ssp_scenario, reference_period, target_period, accent_color
) {
  # Get axis label for variable from the global climate_variables list
  var_meta <- climate_variables[[var_name]]
  var_label <- var_meta$label
  hover_unit <- var_meta$unit
  
  # Title format matching the timeseries chart style
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    ssp_label <- ssp_scenario_labels[ssp_scenario]
    chart_title <- sprintf("%s Seasonal Profile: %s \u2014 %s vs %s", 
                           var_label, region_name, ssp_label, reference_period)
  } else {
    chart_title <- sprintf("%s Seasonal Profile: %s \u2014 Historical (%s)", 
                           var_label, region_name, reference_period)
  }
  
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Guard: If no data is available, return a clean empty plot to avoid Plotly warnings
  if ((is.null(df_hist) || nrow(df_hist) == 0) && (is.null(df_proj) || nrow(df_proj) == 0)) {
    return(
      plot_ly() %>% 
        layout(
          title = list(
            text = paste("No data available for", region_name),
            font = list(family = "Inter, sans-serif", size = 12, color = "#94a3b8"),
            x = 0.05
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(t = 50, r = 20, b = 40, l = 50)
        )
    )
  }
  
  p <- plot_ly()
  
  # Historical Trace
  if (!is.null(df_hist) && nrow(df_hist) > 0) {
    p <- p %>% add_trace(
      data = df_hist,
      x = ~Month,
      y = ~Value,
      type = "scatter", mode = "lines+markers",
      name = paste("Historical", reference_period),
      line = list(color = accent_color, width = 2),
      marker = list(color = accent_color, size = 4),
      hovertemplate = paste0("<b>Historical</b><br>%{x}: %{y:.2f} ", hover_unit, "<extra></extra>")
    )
  }
  
  # Projection Trace
  if (!is.null(df_proj) && nrow(df_proj) > 0) {
    ssp_key <- ssp_scenario
    proj_line_color <- ssp_colors[[ssp_key]]$line
    ssp_label <- ssp_scenario_labels[ssp_scenario]
    
    p <- p %>% add_trace(
      data = df_proj,
      x = ~Month,
      y = ~Value,
      type = "scatter", mode = "lines+markers",
      name = paste(ssp_label, target_period),
      line = list(color = proj_line_color, width = 2.5, dash = "dash"),
      marker = list(color = proj_line_color, size = 4),
      hovertemplate = paste0("<b>Projection</b><br>%{x}: %{y:.2f} ", hover_unit, "<extra></extra>")
    )
  }
  
  p <- p %>% layout(
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
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(text = paste0(var_label, " [", hover_unit, "]"), font = list(family = "Inter, sans-serif", color = "#94a3b8")),
      tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
      gridcolor = "rgba(255, 255, 255, 0.05)",
      zeroline = FALSE
    ),
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(t = 50, r = 20, b = 40, l = 50),
    legend = list(
      orientation = "h", x = 0.5, y = -0.15, xanchor = "center",
      font = list(family = "Inter, sans-serif", size = 11, color = "#94a3b8"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    hovermode = "x unified",
    hoverlabel = list(
      bgcolor = "rgba(15, 23, 42, 0.90)",
      bordercolor = "rgba(255, 255, 255, 0.15)",
      font = list(
        family = "Inter, sans-serif",
        size = 12,
        color = "#e2e8f0"
      )
    )
  ) %>%
  config(displayModeBar = FALSE)
  
  return(p)
}
