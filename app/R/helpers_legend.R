# helpers_legend.R
# ==============================================================================
# Choropleth Legend Builder for the Map Control Panel
# ==============================================================================
# Constructs the gradient legend UI shown at the bottom of the left control
# panel. The legend dynamically adapts to:
#
#   - Absolute mode: sequential palette with raw min/max labels
#   - Anomaly mode:  diverging palette with signed ±labels symmetric around 0
#   - Period mode:   uses period-averaged data range
#   - Projection:    includes SSP scenario label in the title
#
# This file contains two functions:
#
#   1. compute_legend_params() — pure computation that calculates the palette,
#      min/max labels, and title string. No Shiny UI tags involved.
#
#   2. build_legend_ui() — takes the params from above and wraps them in
#      Shiny div/span tags for rendering. Thin wrapper, kept separate so
#      the computation can be tested independently.
#
# Globals accessed from global.R:
#   anomaly_palette_temperature    - Blue→White→Red diverging palette
#   anomaly_palette_precipitation  - Brown→White→Teal diverging palette
#   ssp_scenario_labels            - Human-readable SSP scenario display names
# ==============================================================================


# ------------------------------------------------------------------------------
# compute_legend_params()
# ------------------------------------------------------------------------------
# Computes all the data needed to render the choropleth legend: palette colors,
# min/max labels, and the legend title string. This is pure computation with
# no Shiny dependencies.
#
# Arguments:
#   var_meta           - Metadata list for the climate variable (from
#                        climate_variables global). Must have $label, $unit,
#                        $palette.
#   is_precip          - Logical: is the variable "total_precipitation"?
#                        Affects anomaly palette choice and unit display.
#   use_anomaly_legend - Logical: should anomaly (diverging) legend be shown?
#   temporal_mode      - "Annual" or season name (e.g., "Winter"). Used in
#                        the legend title.
#   time_label         - Year or period string for the title (e.g., "2021" or
#                        "2041-2060").
#   is_projection_data - Logical: is the current data from CMIP6 projections?
#   use_period         - Logical: is the view in period-averaging mode?
#   ssp_scenario       - SSP scenario key (e.g., "ssp2_4_5"). Used for the
#                        title label when showing projections.
#   reference_period   - Reference period string (e.g., "1981-2010"). Shown
#                        in anomaly title.
#   clim_data          - Climate data.frame with a "Value" column. Used to
#                        compute the absolute data range.
#   baseline_df        - Baseline data.frame with columns "Region" and
#                        "baseline_value". Used to compute anomaly range.
#                        Can be NULL when not in anomaly mode.
#
# Returns:
#   A list with three elements:
#     $palette     - character vector of hex colors for the gradient
#     $label_min   - formatted string for the left (minimum) label
#     $label_max   - formatted string for the right (maximum) label
#     $title       - the legend title string
# ------------------------------------------------------------------------------
compute_legend_params <- function(var_meta, is_precip,
                                  use_anomaly_legend, temporal_mode,
                                  time_label, is_projection_data,
                                  use_period, ssp_scenario,
                                  reference_period, clim_data,
                                  baseline_df) {

  # Convert numeric month to month name for the legend title
  display_temporal <- temporal_mode
  if (temporal_mode %in% as.character(1:12)) {
    display_temporal <- month.name[as.integer(temporal_mode)]
  }

  # Extract raw finite values for range calculation
  vals <- clim_data$Value
  vals <- vals[is.finite(vals)]

  if (use_anomaly_legend) {
    # ── Anomaly legend: diverging palette, symmetric around 0 ──────────────
    is_any_wind <- grepl("Wind", var_meta$label, ignore.case = TRUE)
    palette <- if (is_precip) anomaly_palette_precipitation else if (is_any_wind) anomaly_palette_wind else anomaly_palette_temperature
    display_unit <- if (is_precip) "%" else var_meta$unit

    # Compute anomaly range by applying per-region baselines to the current data
    if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
      # Recompute anomalies for legend range
      df_with_baseline <- clim_data |>
        dplyr::left_join(baseline_df, by = "Region")

      if (is_precip) {
        # Precipitation: relative (%) anomaly — guard against near-zero baselines
        # (threshold 1.0 mm) and clamp to ±200% to match the map renderer
        anomaly_vals <- ifelse(
          is.na(df_with_baseline$baseline_value) | abs(df_with_baseline$baseline_value) < 1.0,
          NA_real_,
          pmin(pmax(
            (df_with_baseline$Value - df_with_baseline$baseline_value) / df_with_baseline$baseline_value * 100,
            -200), 200)
        )
      } else {
        # Temperature: absolute anomaly
        anomaly_vals <- df_with_baseline$Value - df_with_baseline$baseline_value
      }

      anomaly_vals <- anomaly_vals[is.finite(anomaly_vals)]
      if (length(anomaly_vals) > 0) {
        abs_max <- max(abs(anomaly_vals))
        if (abs_max < 0.1) abs_max <- 0.1  # prevent degenerate scale
      } else {
        abs_max <- 1
      }
    } else {
      # No baseline available — fall back to raw value range
      abs_max <- max(abs(vals))
      if (abs_max < 0.1) abs_max <- 0.1
    }

    # Signed min/max labels for the diverging scale
    label_min <- sprintf("-%s %s", format(round(abs_max, 1), big.mark = ","), display_unit)
    label_max <- sprintf("+%s %s", format(round(abs_max, 1), big.mark = ","), display_unit)

    # Build title with context — show SSP for projections, ERA5 for historical periods
    if (is_projection_data) {
      proj_note <- paste0(", ", ssp_scenario_labels[ssp_scenario])
    } else if (use_period) {
      proj_note <- ", ERA5 mean"
    } else {
      proj_note <- ""
    }
    legend_title <- sprintf("%s Anomaly (%s %s%s vs %s)",
                            var_meta$label, display_temporal, time_label,
                            proj_note, reference_period)

  } else {
    # ── Absolute legend: sequential palette ──────────────────────────────────
    palette <- var_meta$palette
    display_unit <- var_meta$unit
    min_val <- min(vals)
    max_val <- max(vals)
    label_min <- sprintf("%s %s", format(round(min_val, 1), big.mark = ","), display_unit)
    label_max <- sprintf("%s %s", format(round(max_val, 1), big.mark = ","), display_unit)

    # Build title — include SSP for projections, ERA5 for historical periods
    if (is_projection_data) {
      proj_note <- paste0(" ", ssp_scenario_labels[ssp_scenario], " proj.")
    } else if (use_period) {
      proj_note <- " ERA5 mean"
    } else {
      proj_note <- ""
    }
    legend_title <- sprintf("%s (%s %s%s)", var_meta$label, display_temporal, time_label, proj_note)
  }

  list(
    palette   = palette,
    label_min = label_min,
    label_max = label_max,
    title     = legend_title
  )
}


# ------------------------------------------------------------------------------
# build_legend_ui()
# ------------------------------------------------------------------------------
# Takes the computed legend params and wraps them in Shiny HTML tags for
# rendering inside the left control panel. This is a thin wrapper that only
# handles the visual layout — all data logic is in compute_legend_params().
#
# Arguments:
#   legend_params - List returned by compute_legend_params() with $palette,
#                   $label_min, $label_max, $title.
#
# Returns:
#   A Shiny tag (div) containing the gradient bar and labels, ready for
#   renderUI output.
# ------------------------------------------------------------------------------
build_legend_ui <- function(legend_params) {

  # Construct CSS linear gradient from the palette color vector
  gradient_css <- paste0(
    "linear-gradient(to right, ",
    paste(legend_params$palette, collapse = ", "),
    ")"
  )

  div(
    class = "choropleth-legend-container",
    div(
      class = "legend-title",
      style = "font-weight: 600; font-size: 0.8rem; color: #e2e8f0; margin-bottom: 6px; font-family: Inter, sans-serif;",
      legend_params$title
    ),
    div(
      class = "legend-gradient-bar",
      style = sprintf(
        "background: %s; height: 12px; border-radius: 4px; border: 1px solid rgba(255,255,255,0.15); margin: 6px 0 4px 0;",
        gradient_css
      )
    ),
    div(
      class = "legend-labels",
      style = "display: flex; justify-content: space-between; font-size: 0.75rem; color: #94a3b8; font-family: Inter, sans-serif;",
      span(legend_params$label_min),
      span(legend_params$label_max)
    )
  )
}
