# server.R
# ==============================================================================
# Copernicus PECD v4.2 Visualization App — Initial Geographic Visualizer
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================

server <- function(input, output, session) {

  # Reactive state variables
  clicked_region    <- reactiveVal(NULL)
  map_loaded        <- reactiveVal(FALSE)

  # Incrementing trigger used to force a re-render of zone layers after a basemap
  # style switch. Needed because set_style(..., preserve_layers = FALSE) wipes all
  # custom layers and we must redraw them once the new tiles have loaded.
  style_trigger     <- reactiveVal(0)

  # Track the source ID of any active satellite raster layer so we can remove it
  # cleanly before switching to a different basemap style.
  satellite_src_id  <- reactiveVal(NULL)

  # Dynamic layer stacking helper: determines which base map layer to insert
  # our custom polygon boundaries BEFORE. Setting this to "waterway_line_label"
  # ensures that vector drawing (roads, borders, waterways) sits BEHIND our
  # polygons, while only labels (places, city names, country names) overlay on top.
  target_before_id <- reactive({
    "waterway_line_label"
  })

  # ----------------------------------------------------------------------------
  # Reactive Filtered Climate Data snapshot
  # ----------------------------------------------------------------------------
  # This reactive filters the pre-loaded global historical dataset based on
  # the user's selected variable, temporal mode, year, and spatial level.
  # When projections are ON and the selected year is beyond the historical
  # range (>2023), it reads from the projection dataset instead, computing
  # the ensemble MEDIAN across 6 CMIP6 models for the selected SSP scenario.
  # Returns a simple data.frame with columns: Region, Value
  # ----------------------------------------------------------------------------
  filtered_climate_data <- reactive({
    req(input$climate_variable, input$temporal_mode, input$selected_year, input$spatial_level)

    var_name  <- input$climate_variable
    temp_mode <- input$temporal_mode
    sel_year  <- as.integer(input$selected_year)
    sp_level  <- spatial_level_to_parquet[input$spatial_level]

    # Determine whether to use projection data (year > 2023 with projections ON)
    show_proj <- isTRUE(input$show_projections == "1")
    proj_data_exists <- (var_name %in% projection_available_variables &&
                         sp_level %in% projection_available_spatial_levels)
    use_projection <- (show_proj && sel_year > 2023 && proj_data_exists)

    if (use_projection) {
      # ── Read from projection dataset ─────────────────────────────────────────
      req(input$ssp_scenario)
      scenario_val <- input$ssp_scenario

      if (temp_mode == "Annual") {
        ds <- proj_annual_ds
      } else {
        ds <- proj_seasonal_ds
      }

      if (is.null(ds)) return(NULL)

      # Query all 6 models for the selected scenario + year
      query <- ds |>
        dplyr::filter(variable == var_name,
                      SpatialLevel == sp_level,
                      Year == sel_year,
                      scenario == !!scenario_val)

      if (temp_mode != "Annual") {
        query <- query |> dplyr::filter(Season == temp_mode)
      }

      df_raw <- dplyr::collect(query)

      if (nrow(df_raw) == 0) return(NULL)

      # Compute ensemble median per region (across 6 climate models)
      df_out <- df_raw |>
        dplyr::group_by(Region) |>
        dplyr::summarise(Value = median(Value, na.rm = TRUE), .groups = "drop")

      # Carry forward the other columns needed downstream
      df_out$variable     <- var_name
      df_out$SpatialLevel <- sp_level
      df_out$Year         <- sel_year

      return(df_out)

    } else {
      # ── Read from historical dataset (existing path) ─────────────────────────
      if (temp_mode == "Annual") {
        ds <- hist_annual_ds
      } else {
        ds <- hist_seasonal_ds
      }

      if (is.null(ds)) return(NULL)

      query <- ds |>
        dplyr::filter(variable == var_name,
                      SpatialLevel == sp_level,
                      Year == sel_year)

      if (temp_mode != "Annual") {
        query <- query |> dplyr::filter(Season == temp_mode)
      }

      dplyr::collect(query)
    }
  })

  # ----------------------------------------------------------------------------
  # Dynamic Year Slider Bounds based on Spatial Tier + Projection State
  # ----------------------------------------------------------------------------
  # Onshore PECD spatial tiers (NUT0, NUT2, PEON, SZON) only contain historical
  # data up to 2021. Offshore tiers (PEOF, SZOF) go up to 2023.
  # When projections are toggled ON and the selected variable has projection
  # data, the slider extends to 2100 to allow browsing projected values.
  # ----------------------------------------------------------------------------
  observe({
    req(input$spatial_level, input$temporal_mode, input$climate_variable)

    # Determine the historical maximum year based on Onshore/Offshore tier
    is_offshore <- input$spatial_level %in% c("PEOF", "SZOF")
    hist_max_year <- if (is_offshore) 2023 else 2021

    # Check if projections should extend the slider
    show_proj <- isTRUE(input$show_projections == "1")
    var_name  <- input$climate_variable
    sp_level  <- spatial_level_to_parquet[input$spatial_level]
    proj_data_exists <- (var_name %in% projection_available_variables &&
                         sp_level %in% projection_available_spatial_levels)

    max_year <- if (show_proj && proj_data_exists) 2100 else hist_max_year

    # Winter season has incomplete 1950 data, so min year is 1951 for seasonal mode
    min_year <- if (input$temporal_mode == "Annual") 1950 else 1951

    # Adjust current year selection if it lies outside the valid range
    current_yr <- input$selected_year
    if (is.null(current_yr)) {
      current_yr <- hist_max_year
    }

    new_val <- current_yr
    if (new_val < min_year) {
      new_val <- min_year
    } else if (new_val > max_year) {
      new_val <- max_year
    }

    updateSliderInput(
      session = session,
      inputId = "selected_year",
      min = min_year,
      max = max_year,
      value = new_val
    )
  })

  # ----------------------------------------------------------------------------
  # Spatial Data Loader
  # ----------------------------------------------------------------------------
  # Fetches the spatial boundary layer from the global RAM cache initialized in 
  # global.R to completely bypass disk I/O read latency during user navigation.
  # The cache contains fully parsed sf features already transformed to EPSG:4326.
  # ----------------------------------------------------------------------------
  current_boundaries <- reactive({
    req(input$spatial_level)
    
    # Retrieve preloaded sf object from our global startup cache
    sf_data <- spatial_boundary_cache[[input$spatial_level]]
    req(sf_data)
    
    sf_data
  })


  # ----------------------------------------------------------------------------
  # MapLibre Initialisation
  # ----------------------------------------------------------------------------
  output$map <- renderMaplibre({
    message("Initialising MapLibre canvas...")
    maplibre(
      style      = ofm_positron_style,
      center     = c(europe_center_lon, europe_center_lat),
      zoom       = europe_default_zoom,
      projection = "globe"
      # Navigation control intentionally omitted — we use custom glassmorphism
      # zoom-in / zoom-out / home buttons defined in ui.R instead.
    )
  })

  # Mark map as ready once the zoom input becomes available
  observe({
    req(input$map_zoom)
    if (!map_loaded()) {
      map_loaded(TRUE)
      message("MapLibre canvas successfully loaded.")
    }
  })

  # ----------------------------------------------------------------------------
  # Custom Zoom Controls
  # ----------------------------------------------------------------------------
  # The built-in MapLibre navigation widget was removed because it overlapped
  # with our glassmorphism button column on the right side. These two observers
  # replicate zoom-in and zoom-out by reading the current zoom level from
  # input$map_zoom and applying a smooth +1 / -1 step via ease_to().
  # MapLibre enforces its own min/max zoom limits internally.
  # ----------------------------------------------------------------------------
  observeEvent(input$zoom_in, {
    req(map_loaded())
    current_zoom <- isolate(input$map_zoom)
    if (is.null(current_zoom)) current_zoom <- europe_default_zoom
    
    current_center <- isolate(input$map_center)
    if (is.null(current_center)) {
      center_coords <- c(europe_center_lon, europe_center_lat)
    } else {
      center_coords <- c(as.numeric(current_center[[1]]), as.numeric(current_center[[2]]))
    }
    
    maplibre_proxy("map") %>%
      ease_to(
        center   = center_coords,
        zoom     = current_zoom + 1,
        duration = 300
      )
  })

  observeEvent(input$zoom_out, {
    req(map_loaded())
    current_zoom <- isolate(input$map_zoom)
    if (is.null(current_zoom)) current_zoom <- europe_default_zoom
    
    current_center <- isolate(input$map_center)
    if (is.null(current_center)) {
      center_coords <- c(europe_center_lon, europe_center_lat)
    } else {
      center_coords <- c(as.numeric(current_center[[1]]), as.numeric(current_center[[2]]))
    }
    
    maplibre_proxy("map") %>%
      ease_to(
        center   = center_coords,
        zoom     = current_zoom - 1,
        duration = 300
      )
  })

  # ----------------------------------------------------------------------------
  # Baseline Map Data Reactive — per-region baseline for map anomaly mode
  # ----------------------------------------------------------------------------
  # Unlike baseline_mean_value() which computes the baseline for a single
  # clicked region, this reactive computes the mean Value for EVERY region
  # at the current spatial level over the selected reference period.
  # Returns a data.frame with columns: Region, baseline_value
  # Used by the map rendering observer to convert all polygon values to anomalies.
  # ----------------------------------------------------------------------------
  baseline_map_data <- reactive({
    req(input$show_projections == "1", input$display_mode == "anomaly")
    req(input$reference_period, input$climate_variable, input$temporal_mode, input$spatial_level)

    # Parse reference period
    ref_years <- as.integer(strsplit(input$reference_period, "-")[[1]])
    ref_start <- ref_years[1]
    ref_end   <- ref_years[2]

    var_name  <- input$climate_variable
    temp_mode <- input$temporal_mode
    sp_level  <- spatial_level_to_parquet[input$spatial_level]

    # Select the appropriate historical dataset
    if (temp_mode == "Annual") {
      ds <- hist_annual_ds
    } else {
      ds <- hist_seasonal_ds
    }

    if (is.null(ds)) return(NULL)

    # Query ALL regions for the reference period
    query <- ds |>
      dplyr::filter(variable == var_name,
                    SpatialLevel == sp_level,
                    Year >= ref_start,
                    Year <= ref_end)

    if (temp_mode != "Annual") {
      query <- query |> dplyr::filter(Season == temp_mode)
    }

    df_ref <- dplyr::collect(query)

    if (nrow(df_ref) == 0) return(NULL)

    # Compute per-region mean baseline
    baseline_df <- df_ref |>
      dplyr::group_by(Region) |>
      dplyr::summarise(baseline_value = mean(Value, na.rm = TRUE), .groups = "drop")

    baseline_df
  })

  # ----------------------------------------------------------------------------
  # Period-Averaged Climate Data for Map — historical & projected
  # ----------------------------------------------------------------------------
  # When the user selects "Period" view mode, this reactive computes the
  # period-averaged values for ALL regions on the map.
  #
  # For HISTORICAL periods (end year ≤ 2023):
  #   - Simple mean of ERA5 reanalysis values across all years in the period
  #
  # For PROJECTED periods (end year > 2023):
  #   - CMIP6/IPCC approach: per-model mean over the period → ensemble median
  #   - This gives "median of model means" (robust central estimate)
  #
  # Returns a data.frame with columns: Region, Value
  # (same shape as filtered_climate_data output)
  # ----------------------------------------------------------------------------
  period_averaged_climate_data <- reactive({
    req(input$projection_view_mode == "period")
    req(input$projection_period)
    req(input$climate_variable, input$temporal_mode, input$spatial_level)

    # Parse period (e.g., "2041-2060" or "1981-2010")
    period_years <- as.integer(strsplit(input$projection_period, "-")[[1]])
    period_start <- period_years[1]
    period_end   <- period_years[2]

    var_name  <- input$climate_variable
    temp_mode <- input$temporal_mode
    sp_level  <- spatial_level_to_parquet[input$spatial_level]

    # Decide whether this is a historical or projected period
    is_historical_period <- (period_end <= 2023)

    if (is_historical_period) {
      # ── Historical period: mean from ERA5 reanalysis ──────────────────────────
      if (temp_mode == "Annual") {
        ds <- hist_annual_ds
      } else {
        ds <- hist_seasonal_ds
      }

      if (is.null(ds)) return(NULL)

      query <- ds |>
        dplyr::filter(variable == var_name,
                      SpatialLevel == sp_level,
                      Year >= period_start,
                      Year <= period_end)

      if (temp_mode != "Annual") {
        query <- query |> dplyr::filter(Season == temp_mode)
      }

      df_raw <- dplyr::collect(query)

      if (nrow(df_raw) == 0) return(NULL)

      message(sprintf("  Historical period data: %d rows for %s, %d-%d",
                      nrow(df_raw), var_name, period_start, period_end))

      # Simple per-region mean across all years in the period
      df_out <- df_raw |>
        dplyr::group_by(Region) |>
        dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

    } else {
      # ── Projected period: CMIP6 ensemble (median of model means) ──────────────
      req(input$show_projections == "1", input$ssp_scenario)
      scenario_val <- input$ssp_scenario

      # Guard: exit early if no projection data for this combination
      if (!(var_name %in% projection_available_variables)) return(NULL)
      if (!(sp_level %in% projection_available_spatial_levels)) return(NULL)

      if (temp_mode == "Annual") {
        ds <- proj_annual_ds
      } else {
        ds <- proj_seasonal_ds
      }

      if (is.null(ds)) return(NULL)

      query <- ds |>
        dplyr::filter(variable == var_name,
                      SpatialLevel == sp_level,
                      Year >= period_start,
                      Year <= period_end,
                      scenario == !!scenario_val)

      if (temp_mode != "Annual") {
        query <- query |> dplyr::filter(Season == temp_mode)
      }

      df_raw <- dplyr::collect(query)

      if (nrow(df_raw) == 0) return(NULL)

      message(sprintf("  Projected period data: %d rows for %s, %s, %d-%d",
                      nrow(df_raw), var_name, scenario_val, period_start, period_end))

      # Step 1: Per-model period mean (mean over years for each model × region)
      model_means <- df_raw |>
        dplyr::group_by(Region, model) |>
        dplyr::summarise(model_mean = mean(Value, na.rm = TRUE), .groups = "drop")

      # Step 2: Ensemble median across models (robust central estimate per region)
      df_out <- model_means |>
        dplyr::group_by(Region) |>
        dplyr::summarise(Value = median(model_mean, na.rm = TRUE), .groups = "drop")
    }

    # Add metadata columns for compatibility with filtered_climate_data output
    df_out$variable     <- var_name
    df_out$SpatialLevel <- sp_level
    df_out$Year         <- as.integer(round((period_start + period_end) / 2))

    df_out
  })

  # ----------------------------------------------------------------------------
  # Dynamic Period Dropdown — add/remove projected periods when projections toggle
  # ----------------------------------------------------------------------------
  # When projections are toggled ON, projected period choices (2021-2100) are
  # appended to the dropdown. When toggled OFF, they are removed so only
  # historical periods remain.
  # ----------------------------------------------------------------------------
  observeEvent(input$projections_toggled, {
    if (input$projections_toggled == "on") {
      # Add projected periods to the dropdown
      updateSelectInput(session, "projection_period",
        choices = c(
          "1961\u20131990 (WMO Classic)"   = "1961-1990",
          "1971\u20132000 (WMO Previous)"  = "1971-2000",
          "1981\u20132010 (WMO Current)"   = "1981-2010",
          "2011\u20132023 (Recent)"        = "2011-2023",
          "2021\u20132040 (Near-term)"     = "2021-2040",
          "2041\u20132060 (Mid-term)"      = "2041-2060",
          "2061\u20132080 (Mid-late)"      = "2061-2080",
          "2081\u20132100 (Long-term)"     = "2081-2100"
        )
      )
    } else {
      # Remove projected periods, keep only historical
      updateSelectInput(session, "projection_period",
        choices = c(
          "1961\u20131990 (WMO Classic)"   = "1961-1990",
          "1971\u20132000 (WMO Previous)"  = "1971-2000",
          "1981\u20132010 (WMO Current)"   = "1981-2010",
          "2011\u20132023 (Recent)"        = "2011-2023"
        )
      )
    }
  })

  # ----------------------------------------------------------------------------
  # Central Zone Layer Renderer
  # ----------------------------------------------------------------------------
  # This single observe() block is responsible for ALL polygon drawing.
  # It fires when: (a) the spatial level changes, (b) the map first loads,
  # or (c) style_trigger is incremented after a basemap switch.
  # When projections are ON, it also applies anomaly transformations and
  # swaps to diverging color palettes based on the display mode.
  # ----------------------------------------------------------------------------
  observe({
    req(map_loaded())
    style_trigger()                   # take the dependency so we re-fire on style change

    geom_data <- current_boundaries()
    req(geom_data)

    # Explicit dependencies: ensure observer re-fires when these inputs change.
    # Reading them unconditionally registers them in Shiny's reactive graph,
    # even though we only use them conditionally below.
    view_mode <- input$projection_view_mode        # "year" or "period"
    proj_period <- input$projection_period          # e.g. "2041-2060"
    proj_scenario <- input$ssp_scenario             # e.g. "ssp2_4_5"
    display_mode_val <- input$display_mode           # "absolute" or "anomaly"

    # Reactive climate data — choose between single-year and period-averaged.
    # Period mode works for both historical and projected periods.
    show_proj <- isTRUE(input$show_projections == "1")
    use_period <- isTRUE(view_mode == "period")

    if (use_period) {
      clim_data <- period_averaged_climate_data()
    } else {
      clim_data <- filtered_climate_data()
    }

    message(sprintf("Rendering %d polygons to MapLibre...", nrow(geom_data)))

    # Get the selected variable metadata
    var_meta <- climate_variables[[input$climate_variable]]
    palette <- var_meta$palette
    var_label <- var_meta$label
    var_unit <- var_meta$unit

    # Left join the climate data onto the boundary geometries
    if (!is.null(clim_data) && nrow(clim_data) > 0) {
      joined_geom <- geom_data %>%
        left_join(clim_data, by = c("zone_id" = "Region"))
    } else {
      joined_geom <- geom_data
      joined_geom$Value <- NA_real_
    }

    # ── Check whether anomaly mode is active for the map ──────────────────────
    use_anomaly_map <- (show_proj && isTRUE(input$display_mode == "anomaly"))
    is_precip <- (input$climate_variable == "total_precipitation")
    sel_year <- as.integer(input$selected_year)
    # Determine if current data is from projections based on actual period/year
    if (use_period && !is.null(proj_period) && nchar(proj_period) > 0) {
      period_end_year <- as.integer(strsplit(proj_period, "-")[[1]][2])
      is_projection_year <- (period_end_year > 2023)
    } else {
      is_projection_year <- (sel_year > 2023)
    }

    # Determine the display unit for tooltips
    if (use_anomaly_map) {
      display_unit <- if (is_precip) "%" else var_unit
    } else {
      display_unit <- var_unit
    }

    # ── Apply anomaly transformation if active ────────────────────────────────
    if (use_anomaly_map) {
      baseline_df <- baseline_map_data()

      if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
        # Join baseline values onto the geometry
        joined_geom <- joined_geom %>%
          left_join(baseline_df, by = c("zone_id" = "Region"))

        # Transform values to anomalies
        if (is_precip) {
          # Precipitation: relative change (%), guard against zero baseline
          joined_geom <- joined_geom %>%
            mutate(Value = ifelse(
              is.na(baseline_value) | abs(baseline_value) < 0.001,
              NA_real_,
              (Value - baseline_value) / baseline_value * 100
            ))
        } else {
          # Temperature: absolute change
          joined_geom <- joined_geom %>%
            mutate(Value = Value - baseline_value)
        }
      }

      # Swap to diverging anomaly palette
      palette <- if (is_precip) anomaly_palette_precipitation else anomaly_palette_temperature
    }

    # ── Build tooltips ────────────────────────────────────────────────────────
    # Determine the projection annotation for tooltips
    period_label <- if (use_period) paste0(input$projection_period, " period mean") else ""
    if (use_anomaly_map) {
      # Anomaly tooltips with sign prefix and reference period
      ref_label <- input$reference_period
      proj_note <- if (use_period) paste0(" (", period_label, ")") else if (is_projection_year) " (projection median)" else ""
      joined_geom <- joined_geom %>%
        mutate(
          tooltip_html = paste0(
            "<div class='map-tooltip-content' style='font-family: Inter, sans-serif; padding: 4px;'>",
            "  <div class='tooltip-title' style='font-weight: 600; color: #f8fafc; font-size: 0.85rem;'>", name, " (", zone_id, ")</div>",
            "  <div class='tooltip-metric' style='margin-top: 4px; font-size: 0.8rem;'>",
            "    <span class='tooltip-metric-label' style='color: #94a3b8;'>", var_label, " anomaly:</span> ",
            "    <span class='tooltip-metric-value' style='font-weight: 500; color: #38bdf8;'>",
                   ifelse(is.na(Value), "No Data",
                          paste0(ifelse(Value >= 0, "+", ""),
                                 format(round(Value, 2), big.mark = ","), " ", display_unit)),
            "    </span>",
            "  </div>",
            "  <div style='margin-top: 2px; font-size: 0.7rem; color: #64748b;'>vs ", ref_label, proj_note, "</div>",
            "</div>"
          )
        )
    } else {
      # Standard absolute tooltips
      proj_note <- if (use_period) paste0(" (", period_label, ")") else if (show_proj && is_projection_year) " (projection median)" else ""
      joined_geom <- joined_geom %>%
        mutate(
          tooltip_html = paste0(
            "<div class='map-tooltip-content' style='font-family: Inter, sans-serif; padding: 4px;'>",
            "  <div class='tooltip-title' style='font-weight: 600; color: #f8fafc; font-size: 0.85rem;'>", name, " (", zone_id, ")</div>",
            "  <div class='tooltip-metric' style='margin-top: 4px; font-size: 0.8rem;'>",
            "    <span class='tooltip-metric-label' style='color: #94a3b8;'>", var_label, ":</span> ",
            "    <span class='tooltip-metric-value' style='font-weight: 500; color: #38bdf8;'>",
                   ifelse(is.na(Value), "No Data", paste0(format(round(Value, 2), big.mark = ","), " ", var_unit)),
            "    </span>",
            ifelse(proj_note != "", paste0("<span style='font-size: 0.7rem; color: #64748b;'>", proj_note, "</span>"), ""),
            "  </div>",
            "</div>"
          )
        )
    }

    # Calculate min and max values for interpolation stops, ignoring NAs
    vals <- joined_geom$Value
    vals <- vals[is.finite(vals)]

    if (length(vals) > 0) {
      min_val <- min(vals)
      max_val <- max(vals)

      # For anomaly mode, force symmetric scale around 0
      if (use_anomaly_map) {
        abs_max <- max(abs(min_val), abs(max_val))
        if (abs_max < 0.1) abs_max <- 0.1 # prevent degenerate scale
        min_val <- -abs_max
        max_val <- abs_max
      }

      if (min_val == max_val) {
        min_val <- min_val - 0.1
        max_val <- max_val + 0.1
      }
      interpolation_values <- seq(min_val, max_val, length.out = length(palette))

      fill_expr <- mapgl::interpolate(
        column = "Value",
        type = "linear",
        values = interpolation_values,
        stops = palette,
        na_color = "#33415533" # Subtle semi-transparent slate for areas with no data
      )
    } else {
      fill_expr <- "steelblue"
    }

    # NOTE: do NOT reset clicked_region() here — the user's selection must survive
    # a basemap style switch. We only clear it when the spatial tier itself changes
    # (handled by a separate observer below).

    proxy <- maplibre_proxy("map") %>%
      clear_layer("zone-highlight") %>%
      clear_layer("zone-borders") %>%
      clear_layer("zone-fills") %>%
      add_fill_layer(
        id                 = "zone-fills",
        source             = joined_geom,
        fill_color         = fill_expr,
        fill_opacity       = isolate(input$polygon_opacity),
        fill_outline_color = "#ffffff00",   # suppress the default hairline so our border layer controls it
        tooltip            = "tooltip_html",
        before_id          = target_before_id()
      ) %>%
      add_line_layer(
        id           = "zone-borders",
        source       = geom_data,
        line_color   = "darkslateblue",
        line_width   = 1.0,
        line_opacity = 0.8,
        before_id    = target_before_id()
      )

    # Custom study zone borders always remain visible to preserve shape definitions
    proxy %>% set_layout_property("zone-borders", "visibility", "visible")

    # Restore the crimson highlight if a region was selected before the style switch.
    # We use isolate() so this doesn't create a reactive dependency on clicked_region.
    selected <- isolate(clicked_region())
    if (!is.null(selected)) {
      highlight_geom <- geom_data %>% filter(zone_id == selected$zone_id)
      if (nrow(highlight_geom) > 0) {
        message(sprintf("Restoring highlight for: %s after basemap switch...", selected$name))
        maplibre_proxy("map") %>%
          add_line_layer(
            id           = "zone-highlight",
            source       = highlight_geom,
            line_color   = "#d9534f",
            line_width   = 3.0,
            line_opacity = 0.95,
            before_id    = target_before_id()
          )
      }
    }
  })

  # ----------------------------------------------------------------------------
  # Clear selection when the spatial tier changes (not on basemap switch)
  # ----------------------------------------------------------------------------
  observeEvent(input$spatial_level, {
    clicked_region(NULL)
    session$sendCustomMessage("toggle_stats_drawer", list(show = FALSE))
  }, ignoreInit = TRUE)

  # Close drawer when user clicks ✕ button (JS fires drawer_closed input)
  observeEvent(input$drawer_closed, {
    clicked_region(NULL)
    maplibre_proxy("map") %>% clear_layer("zone-highlight")
    
    # Zoom to the entire spatial extent of the active shapefile tier
    req(map_loaded(), current_boundaries())
    bbox <- sf::st_bbox(current_boundaries())
    message("Bottom stats drawer closed. Zooming out to fit full spatial tier extent.")
    maplibre_proxy("map") %>%
      fit_bounds(
        c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
        animate = TRUE,
        # Padding offsets for UI panels: 320px left padding keeps full tier map center-right,
        # away from control-panel overlay. top/bottom/right have minor breathing room.
        padding = list(top = 40, bottom = 40, left = 320, right = 40)
      )
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------------------------
  # Polygon Opacity Controller
  # ----------------------------------------------------------------------------
  # Dynamically updates the 'fill-opacity' paint property of the zone-fills
  # layer in real-time, bypassing the need to clear and recreate the layer.
  # ----------------------------------------------------------------------------
  observeEvent(input$polygon_opacity, {
    req(map_loaded())
    maplibre_proxy("map") %>%
      set_paint_property(
        layer_id = "zone-fills",
        name     = "fill-opacity",
        value    = input$polygon_opacity
      )
  })

  # ----------------------------------------------------------------------------
  # Boundary Visibility Controller
  # ----------------------------------------------------------------------------
  # Dynamically toggles custom and basemap boundaries on/off based on checkbox.
  # ----------------------------------------------------------------------------
  observeEvent(input$show_boundaries, {
    req(map_loaded())
    vis_value <- if (isTRUE(input$show_boundaries)) "visible" else "none"
    
    proxy <- maplibre_proxy("map")
    
    # Toggle base map background political and administrative boundaries
    basemap_borders <- c("boundary_2", "boundary_3", "boundary_disputed")
    for (layer_id in basemap_borders) {
      tryCatch(
        proxy %>% set_layout_property(layer_id, "visibility", vis_value),
        error = function(e) NULL
      )
    }
  })

  # ----------------------------------------------------------------------------
  # Map Projection Controller
  # ----------------------------------------------------------------------------
  # Switches the MapLibre projection between "globe" (3D sphere) and
  # "mercator" (flat 2D). The mapgl R package doesn't expose a proxy method
  # for setProjection(), so we send a custom message to JavaScript which calls
  # the MapLibre GL JS API directly on the underlying map instance.
  # ----------------------------------------------------------------------------
  observeEvent(input$map_projection, {
    req(map_loaded())
    session$sendCustomMessage("set_map_projection", list(projection = input$map_projection))

    # Re-fit the map bounds after the projection change so the zoom level
    # stays consistent. Globe and Mercator project extents differently,
    # so the same zoom number covers a different visual area.
    req(current_boundaries())

    selected <- clicked_region()
    if (!is.null(selected)) {
      # A region is selected — zoom to that polygon
      target_geom <- current_boundaries() %>% filter(zone_id == selected$zone_id)
      if (nrow(target_geom) > 0) {
        bbox <- sf::st_bbox(target_geom)
        maplibre_proxy("map") %>%
          fit_bounds(
            c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
            animate = TRUE,
            padding = list(top = 80, bottom = 220, left = 340, right = 80),
            maxZoom = 7.0
          )
        return()
      }
    }

    # No selection — zoom to the full spatial tier extent
    bbox <- sf::st_bbox(current_boundaries())
    maplibre_proxy("map") %>%
      fit_bounds(
        c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
        animate = TRUE,
        padding = list(top = 40, bottom = 40, left = 320, right = 40)
      )
  })

  # ----------------------------------------------------------------------------
  # Polygon Click / Highlight Handler
  # ----------------------------------------------------------------------------
  observeEvent(input$map_feature_click, {
    click <- input$map_feature_click
    if (is.null(click)) return()

    # Only react to clicks on our interactive fill layer
    layer_hit <- isTRUE(click$layer_id == "zone-fills") || isTRUE(click$layer == "zone-fills")
    if (!layer_hit) return()

    props <- click$properties
    message(sprintf("Polygon clicked: ID = %s | Name = %s", props$zone_id, props$name))
    clicked_region(props)

    # Slide up the stats drawer
    session$sendCustomMessage("toggle_stats_drawer", list(show = TRUE))

    # Draw a crimson highlight border around the selected zone
    highlight_geom <- current_boundaries() %>% filter(zone_id == props$zone_id)
    
    # Draw crimson highlight border AND smoothly pan to center the selected zone
    if (nrow(highlight_geom) > 0) {
      # Calculate the geographic centroid of the clicked polygon.
      # Some NUTS/PECD geometries (notably Norway) contain degenerate edges
      # (duplicate vertices) that cause sf::st_union() to crash under s2
      # spherical geometry. We handle this with a try-fallback chain:
      #   1. Try st_make_valid → st_union → st_centroid (s2-safe)
      #   2. If s2 still chokes, temporarily disable s2 and retry with planar ops
      #   3. Last resort: use the simple bounding-box center (always works)
      centroid_result <- tryCatch({
        valid_geom <- sf::st_make_valid(highlight_geom)
        sf::st_centroid(sf::st_union(valid_geom))
      }, error = function(e) {
        message(sprintf("  [WARN] s2 centroid failed for %s, falling back to planar: %s",
                        props$name, conditionMessage(e)))
        tryCatch({
          old_s2 <- sf::sf_use_s2()
          sf::sf_use_s2(FALSE)
          on.exit(sf::sf_use_s2(old_s2), add = TRUE)
          sf::st_centroid(sf::st_union(highlight_geom))
        }, error = function(e2) {
          NULL
        })
      })

      # Extract lon/lat from the centroid, or fall back to bbox center
      if (!is.null(centroid_result)) {
        centroid_coords <- sf::st_coordinates(centroid_result)
        centroid_lon <- centroid_coords[1, "X"]
        centroid_lat <- centroid_coords[1, "Y"]
      } else {
        # Bounding-box center — guaranteed to work for any geometry
        bbox <- sf::st_bbox(highlight_geom)
        centroid_lon <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
        centroid_lat <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
        message(sprintf("  [WARN] Using bbox center for %s", props$name))
      }


      # Use fit_bounds to zoom in to the polygon with generous breathing room.
      # The heavy bottom padding (~420px) is the key trick: since the stats drawer
      # covers ~40vh of the screen, we tell MapLibre to treat that area as dead space.
      # This causes the polygon to fit into the UPPER portion of the map, nicely
      # above the drawer. The large left padding (340px) avoids the control panel.
      # maxZoom = 7 prevents tiny polygons from zooming in too aggressively.
      bbox <- sf::st_bbox(highlight_geom)

      message(sprintf("  Zooming to clicked region: %s", props$name))

      maplibre_proxy("map") %>%
        clear_layer("zone-highlight") %>%
        add_line_layer(
          id           = "zone-highlight",
          source       = highlight_geom,
          line_color   = "#ef4444",
          line_width   = 3.0,
          line_opacity = 0.95,
          before_id    = target_before_id()
        ) %>%
        fit_bounds(
          c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
          animate = TRUE,
          # bottom = 420 pushes the polygon into the upper ~60% of the viewport,
          # keeping it visible above the 40vh stats drawer. left = 340 clears
          # the control panel. maxZoom = 7 keeps the view "one level out" for small regions.
          padding = list(top = 60, bottom = 420, left = 340, right = 60),
          maxZoom = 7.0
        )
    }
  })

  # ----------------------------------------------------------------------------
  # Basemap Style Controller
  # ----------------------------------------------------------------------------
  # Switches between the two vector tile basemaps and the Sentinel-2 satellite
  # hybrid. After calling set_style() (which wipes all custom layers), we wait
  # a short interval for the new tiles to settle and then increment style_trigger,
  # which causes the central renderer above to redraw the zone polygons cleanly.
  # ----------------------------------------------------------------------------
  observeEvent(input$basemap, {
    req(map_loaded())
    proxy  <- maplibre_proxy("map")
    choice <- input$basemap

    # --- Vector basemaps (Positron / Bright) ----------------------------------
    if (choice %in% c("ofm_positron", "ofm_bright")) {
      style_url <- switch(choice,
        "ofm_positron" = ofm_positron_style,
        "ofm_bright"   = ofm_bright_style
      )
      proxy %>% set_style(style_url, preserve_layers = FALSE)

      # Give the new vector tiles ~700 ms to load, then redraw zone polygons
      current_session <- shiny::getDefaultReactiveDomain()
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          style_trigger(isolate(style_trigger()) + 1)
        })
      }, delay = 0.7)

    # --- Sentinel-2 satellite hybrid ------------------------------------------
    } else if (choice == "sentinel") {
      # Use Positron as the label/road/symbol base, then insert the satellite
      # raster BELOW everything via before_id = "background".
      # Without the hide step below, the Positron fill layers (water, land, roads)
      # sit on top and completely obscure the imagery — which is the bug we fix here.
      proxy %>% set_style(ofm_positron_style, preserve_layers = FALSE)

      current_session <- shiny::getDefaultReactiveDomain()
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          # Race-condition guard: abort if user switched away in the meantime
          if (isolate(input$basemap) != "sentinel") return()

          # Fixed IDs are safe here because set_style(preserve_layers = FALSE)
          # already wiped the canvas — no collision risk on repeated switches.
          src_id <- "sentinel-src"
          lyr_id <- "sentinel-raster"

          # 1) Add Sentinel-2 imagery at the very bottom of the layer stack
          maplibre_proxy("map") %>%
            add_raster_source(
              id          = src_id,
              tiles       = sentinel_url,
              tileSize    = 256,
              attribution = sentinel_attribution
            ) %>%
            add_layer(
              id        = lyr_id,
              type      = "raster",
              source    = src_id,
              paint     = list("raster-opacity" = 1),
              before_id = "background"
            )

          # 2) Hide all Positron fill/polygon/road layers so the satellite shows
          #    through. Label and symbol layers are left visible. A try-catch
          #    silently ignores IDs that don't exist in this particular style.
          positron_fill_layers <- c(
            "background", "water", "waterway", "park",
            "landcover_wood", "landcover_grass", "landcover_ice_shelf", "landcover_glacier",
            "landuse_residential", "landuse_commercial", "landuse_industrial",
            "building",
            "tunnel_motorway_casing", "tunnel_motorway_inner",
            "highway_path", "highway_minor",
            "highway_major_casing", "highway_major_inner", "highway_major_subtle",
            "highway_motorway_casing", "highway_motorway_inner", "highway_motorway_subtle",
            "highway_motorway_bridge_casing", "highway_motorway_bridge_inner",
            "railway_transit", "railway_transit_dashline",
            "railway_service", "railway_service_dashline",
            "railway", "railway_dashline",
            "boundary_3", "boundary_2", "boundary_disputed",
            "aeroway-taxiway", "aeroway-runway-casing", "aeroway-area", "aeroway-runway",
            "road_area_pier", "road_pier"
          )
          for (lid in positron_fill_layers) {
            # Skip border layers if show_boundaries is enabled
            if (lid %in% c("boundary_2", "boundary_3", "boundary_disputed") && isTRUE(isolate(input$show_boundaries))) {
              next
            }
            tryCatch(
              maplibre_proxy("map") %>% set_layout_property(lid, "visibility", "none"),
              error = function(e) NULL   # layer absent in this style version — ignore
            )
          }

          satellite_src_id(src_id)

          # 3) Redraw the zone polygon overlays on top of the now-visible satellite
          style_trigger(isolate(style_trigger()) + 1)
        })
      }, delay = 0.7)
    }
  })

  # ----------------------------------------------------------------------------
  # Recenter / Fit-Bounds Handler
  # ----------------------------------------------------------------------------
  observeEvent(input$zoom_home, {
    req(map_loaded(), current_boundaries())
    
    selected <- clicked_region()
    if (!is.null(selected)) {
      # Zoom to the specific selected shapefile polygon
      target_geom <- current_boundaries() %>% filter(zone_id == selected$zone_id)
      if (nrow(target_geom) > 0) {
        bbox <- sf::st_bbox(target_geom)
        message(sprintf("Zooming view to spatial extent of selected region: %s...", selected$name))
        maplibre_proxy("map") %>%
          fit_bounds(
            c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
            animate = TRUE,
            # Maintain same padded offset zoom when homing on a selected polygon
            padding = list(top = 80, bottom = 220, left = 340, right = 80),
            maxZoom = 7.0
          )
        return()
      }
    }
    
    # Fallback: Zoom to the entire active geographical tier boundaries (Europe-wide)
    bbox <- sf::st_bbox(current_boundaries())
    message("Recentering view to spatial extent of entire current tier...")
    maplibre_proxy("map") %>%
      fit_bounds(
        c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
        animate = TRUE,
        # Default Europe zoom respects control panel overlay
        padding = list(top = 40, bottom = 40, left = 320, right = 40)
      )
  })

  # ----------------------------------------------------------------------------
  # Auto-Zoom on Tier Selection / Startup
  # ----------------------------------------------------------------------------
  # Automatically fits map bounds to the selected spatial tier's extent
  # when the tier is changed in the dropdown, or on initial app startup.
  # ----------------------------------------------------------------------------
  observe({
    req(map_loaded(), current_boundaries())
    
    # Take a dependency on the selected tier to trigger the zoom
    level_code <- input$spatial_level
    geom_data  <- current_boundaries()
    req(geom_data)
    
    bbox <- sf::st_bbox(geom_data)
    message(sprintf("Auto-zooming to spatial extent of tier: %s...", level_code))
    maplibre_proxy("map") %>%
      fit_bounds(
        c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
        animate = TRUE,
        # Default Europe/tier zoom respects left control panel overlay
        padding = list(top = 40, bottom = 40, left = 320, right = 40)
      )
  })

  # ----------------------------------------------------------------------------
  # Sidebar Metadata Renderers
  # ----------------------------------------------------------------------------

  # Active layer information card (inside dark left panel)
  output$layer_metadata_text <- renderUI({
    req(input$spatial_level)
    level_info <- spatial_levels[[input$spatial_level]]
    geom_data  <- current_boundaries()

    HTML(sprintf(
      "<p><b>Name:</b> %s</p>
       <p><b>Zones:</b> %d regions</p>
       <p>%s</p>",
      level_info$name,
      if (is.null(geom_data)) 0L else nrow(geom_data),
      level_info$description
    ))
  })

  # ----------------------------------------------------------------------------
  # Region Stats Drawer — metric cards rendered when a polygon is clicked
  # ----------------------------------------------------------------------------
  output$region_stats_cards <- renderUI({
    region <- clicked_region()
    if (is.null(region)) return(NULL)

    area_txt <- "N/A"
    if (!is.null(region$area_km2) && !is.na(region$area_km2)) {
      area_txt <- sprintf("%s km\u00b2", format(round(as.numeric(region$area_km2)), big.mark = ","))
    }

    parent_txt <- "\u2014"
    if (!is.null(region$parent_zone) && !is.na(region$parent_zone) && region$parent_zone != "null") {
      parent_txt <- region$parent_zone
    }

    # Helper to build a single metric card
    metric_card <- function(label, value, accent_class = "") {
      div(
        class = paste("metric-card", accent_class),
        div(class = "metric-label", label),
        div(class = "metric-value", HTML(value))
      )
    }

    tagList(
      metric_card("Region Name",    region$name),
      metric_card("Zone ID",        sprintf("<code>%s</code>", region$zone_id), "accent-danger"),
      metric_card("Parent Zone",    sprintf("<code>%s</code>", parent_txt)),
      metric_card("Spatial Tier",   region$level),
      metric_card("Area",           area_txt, "accent-success")
    )
  })

  # ----------------------------------------------------------------------------
  # Choropleth Custom Legend Renderer
  # ----------------------------------------------------------------------------
  # Computes the color scale gradient and min/max data range labels for the
  # active variable/year. Embedded at the bottom of the left control panel
  # so users can always interpret the choropleth without clicking a region.
  # When projections are ON in anomaly mode, shows diverging palette with
  # signed labels and reference period context.
  # ----------------------------------------------------------------------------
  output$choropleth_legend <- renderUI({
    # Take dependencies on all controls that affect the legend
    req(input$climate_variable, input$temporal_mode, input$selected_year)
    # Explicit dependencies for period mode
    view_mode <- input$projection_view_mode
    proj_period <- input$projection_period

    var_meta <- climate_variables[[input$climate_variable]]
    is_precip <- (input$climate_variable == "total_precipitation")

    # Check display mode
    show_proj <- isTRUE(input$show_projections == "1")
    use_anomaly_legend <- (show_proj && isTRUE(input$display_mode == "anomaly"))
    use_period <- isTRUE(view_mode == "period")
    sel_year <- as.integer(input$selected_year)

    # Determine if the current view shows projected data
    # For period mode: check the period end year (not just "period mode is on")
    # For year mode: check if the selected year is beyond historical range
    if (use_period && !is.null(proj_period) && nchar(proj_period) > 0) {
      period_end_year <- as.integer(strsplit(proj_period, "-")[[1]][2])
      is_projection_data <- (period_end_year > 2023)
    } else {
      period_end_year <- NA
      is_projection_data <- (sel_year > 2023)
    }

    # Build the year/period label for titles
    if (use_period) {
      time_label <- proj_period  # e.g. "2041-2060" or "1981-2010"
    } else {
      time_label <- as.character(input$selected_year)
    }

    # Get the current data range (use period data when in period mode)
    if (use_period) {
      clim_data <- period_averaged_climate_data()
    } else {
      clim_data <- filtered_climate_data()
    }
    if (is.null(clim_data) || nrow(clim_data) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }

    vals <- clim_data$Value
    vals <- vals[is.finite(vals)]

    if (length(vals) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }

    if (use_anomaly_legend) {
      # ── Anomaly legend: diverging palette, symmetric around 0 ──────────────
      palette <- if (is_precip) anomaly_palette_precipitation else anomaly_palette_temperature
      display_unit <- if (is_precip) "%" else var_meta$unit
      ref_label <- input$reference_period

      # Compute anomaly values for range (apply per-region baselines)
      baseline_df <- baseline_map_data()
      if (!is.null(baseline_df) && nrow(baseline_df) > 0) {
        # Recompute anomalies for legend range
        df_with_baseline <- clim_data |>
          dplyr::left_join(baseline_df, by = "Region")
        if (is_precip) {
          anomaly_vals <- ifelse(
            is.na(df_with_baseline$baseline_value) | abs(df_with_baseline$baseline_value) < 0.001,
            NA_real_,
            (df_with_baseline$Value - df_with_baseline$baseline_value) / df_with_baseline$baseline_value * 100
          )
        } else {
          anomaly_vals <- df_with_baseline$Value - df_with_baseline$baseline_value
        }
        anomaly_vals <- anomaly_vals[is.finite(anomaly_vals)]
        if (length(anomaly_vals) > 0) {
          abs_max <- max(abs(anomaly_vals))
          if (abs_max < 0.1) abs_max <- 0.1
        } else {
          abs_max <- 1
        }
      } else {
        abs_max <- max(abs(vals))
        if (abs_max < 0.1) abs_max <- 0.1
      }

      # Signed min/max labels
      label_min <- sprintf("-%s %s", format(round(abs_max, 1), big.mark = ","), display_unit)
      label_max <- sprintf("+%s %s", format(round(abs_max, 1), big.mark = ","), display_unit)

      # Build title with context
      # Only show SSP label for projected periods, show "ERA5 mean" for historical
      if (is_projection_data) {
        proj_note <- paste0(", ", ssp_scenario_labels[input$ssp_scenario])
      } else if (use_period) {
        proj_note <- ", ERA5 mean"
      } else {
        proj_note <- ""
      }
      legend_title <- sprintf("%s Anomaly (%s %s%s vs %s)",
                               var_meta$label, input$temporal_mode, time_label,
                               proj_note, ref_label)

    } else {
      # ── Absolute legend: sequential palette (unchanged) ────────────────────
      palette <- var_meta$palette
      display_unit <- var_meta$unit
      min_val <- min(vals)
      max_val <- max(vals)
      label_min <- sprintf("%s %s", format(round(min_val, 1), big.mark = ","), display_unit)
      label_max <- sprintf("%s %s", format(round(max_val, 1), big.mark = ","), display_unit)

      # Build title
      if (is_projection_data) {
        proj_note <- paste0(" ", ssp_scenario_labels[input$ssp_scenario], " proj.")
      } else if (use_period) {
        proj_note <- " ERA5 mean"
      } else {
        proj_note <- ""
      }
      legend_title <- sprintf("%s (%s %s%s)", var_meta$label, input$temporal_mode, time_label, proj_note)
    }

    # Construct CSS linear gradient from the palette colors
    gradient_css <- paste0("linear-gradient(to right, ", paste(palette, collapse = ", "), ")")

    div(
      class = "choropleth-legend-container",
      div(
        class = "legend-title",
        style = "font-weight: 600; font-size: 0.8rem; color: #e2e8f0; margin-bottom: 6px; font-family: Inter, sans-serif;",
        legend_title
      ),
      div(
        class = "legend-gradient-bar",
        style = sprintf("background: %s; height: 12px; border-radius: 4px; border: 1px solid rgba(255,255,255,0.15); margin: 6px 0 4px 0;", gradient_css)
      ),
      div(
        class = "legend-labels",
        style = "display: flex; justify-content: space-between; font-size: 0.75rem; color: #94a3b8; font-family: Inter, sans-serif;",
        span(label_min),
        span(label_max)
      )
    )
  })

  # ----------------------------------------------------------------------------
  # Projection Controls Availability Observer
  # ----------------------------------------------------------------------------
  # When the selected climate variable or spatial level changes, this observer
  # checks whether projection data exists for the combination and sends a JS
  # message to show or hide the projection controls in the left sidebar.
  # This prevents user confusion from disabled controls — the controls simply
  # disappear when not relevant and reappear when they are.
  # ----------------------------------------------------------------------------
  observe({
    req(input$climate_variable, input$spatial_level)

    var_name <- input$climate_variable
    sp_level <- spatial_level_to_parquet[input$spatial_level]

    # Check whether this variable + spatial level combination has projection data
    var_available <- var_name %in% projection_available_variables
    spatial_available <- sp_level %in% projection_available_spatial_levels
    projections_available <- var_available && spatial_available

    session$sendCustomMessage(
      "toggle_projection_controls",
      list(available = projections_available)
    )
  })

  # ----------------------------------------------------------------------------
  # Filtered Projection Data Reactive
  # ----------------------------------------------------------------------------
  # Only fires when the projection toggle is ON, a region is clicked, and the
  # selected variable + spatial level has projection data.
  # Reads all 6 CMIP6 models for the chosen SSP scenario and computes per-year
  # ensemble statistics: median, min, max (for the envelope / band).
  # Returns a data.frame with columns: Year, median_val, min_val, max_val
  # ----------------------------------------------------------------------------
  filtered_projection_data <- reactive({
    # Only compute if the toggle is on
    req(input$show_projections == "1")
    req(input$ssp_scenario)

    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode, input$spatial_level)

    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    target_region <- region$zone_id
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    scenario <- input$ssp_scenario

    # Guard: exit early if this combination has no projection data
    if (!(var_name %in% projection_available_variables)) return(NULL)
    if (!(sp_level %in% projection_available_spatial_levels)) return(NULL)

    # Select the correct lazy Arrow dataset (annual vs seasonal)
    if (temp_mode == "Annual") {
      ds <- proj_annual_ds
    } else {
      ds <- proj_seasonal_ds
    }

    if (is.null(ds)) return(NULL)

    # Build query: filter by variable, spatial level, region, and scenario.
    # This loads all 6 models for the chosen scenario in one query.
    query <- ds |>
      dplyr::filter(variable == var_name,
                    SpatialLevel == sp_level,
                    Region == target_region,
                    scenario == !!scenario)

    # If seasonal mode, filter to the active season
    if (temp_mode != "Annual") {
      query <- query |> dplyr::filter(Season == temp_mode)
    }

    df_proj <- dplyr::collect(query)

    if (nrow(df_proj) == 0) return(NULL)

    # Compute ensemble statistics grouped by Year:
    # - median_val: the central model estimate (robust to outliers)
    # - min_val: the lowest model value (bottom of the "model agreement" band)
    # - max_val: the highest model value (top of the band)
    ensemble_stats <- df_proj |>
      dplyr::group_by(Year) |>
      dplyr::summarise(
        median_val = median(Value, na.rm = TRUE),
        min_val    = min(Value, na.rm = TRUE),
        max_val    = max(Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(Year)

    ensemble_stats
  })

  # ----------------------------------------------------------------------------
  # Baseline Mean Reactive — reference period average for anomaly calculation
  # ----------------------------------------------------------------------------
  # Computes the mean historical value over the user-selected WMO reference
  # period (e.g., 1981-2010). This single number is subtracted from both the
  # historical and projection data to convert to anomalies.
  # For precipitation, anomalies are shown as % change instead of absolute.
  # Returns a single numeric value (the baseline average), or NULL if the
  # reference period has no data.
  # ----------------------------------------------------------------------------
  baseline_mean_value <- reactive({
    # Only compute when projections are toggled on
    req(input$show_projections == "1", input$reference_period)

    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode, input$spatial_level)

    # Parse the year range from the dropdown value (e.g., "1981-2010")
    ref_years <- as.integer(strsplit(input$reference_period, "-")[[1]])
    ref_start <- ref_years[1]
    ref_end   <- ref_years[2]

    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    target_region <- region$zone_id
    sp_level <- spatial_level_to_parquet[input$spatial_level]

    # Select the appropriate historical dataset
    if (temp_mode == "Annual") {
      ds <- hist_annual_ds
    } else {
      ds <- hist_seasonal_ds
    }

    if (is.null(ds)) return(NULL)

    # Query historical data for the reference period years only
    query <- ds |>
      dplyr::filter(variable == var_name,
                    SpatialLevel == sp_level,
                    Region == target_region,
                    Year >= ref_start,
                    Year <= ref_end)

    # Filter by season if in seasonal mode
    if (temp_mode != "Annual") {
      query <- query |> dplyr::filter(Season == temp_mode)
    }

    df_ref <- dplyr::collect(query)

    if (nrow(df_ref) == 0) return(NULL)

    # Compute the mean value over the reference period
    baseline <- mean(df_ref$Value, na.rm = TRUE)

    message(sprintf("  Baseline for %s (%s, %s): %.2f over %d-%d (%d years of data)",
                    var_name, target_region, temp_mode, baseline, ref_start, ref_end, nrow(df_ref)))

    baseline
  })

  # ----------------------------------------------------------------------------
  # Time-Series Plotly Chart Renderer — with Projection Overlay & Anomaly Mode
  # ----------------------------------------------------------------------------
  # When a region polygon is clicked, this block loads the full historical record
  # (1950-2023) for that specific region and selected climate variable, and plots
  # it as an interactive time series line chart using Plotly.
  #
  # When the projection toggle is ON, it overlays:
  #   - A dashed ensemble median line (colored by SSP scenario, IPCC convention)
  #   - A semi-transparent "model agreement" band (min-max of 6 CMIP6 models)
  #   - A vertical "Present Day" marker at 2023
  #   - A horizontal y=0 baseline reference line
  #   - A vertical shaded band highlighting the reference period
  #   - All values converted to anomalies (departures from the baseline mean)
  # ----------------------------------------------------------------------------
  output$region_timeseries <- renderPlotly({
    # Ensure a region is clicked and variable/mode are set
    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode)

    message(sprintf("  Chart render START: region=%s, var=%s, mode=%s, show_proj=%s, display_mode=%s",
                    region$zone_id, input$climate_variable, input$temporal_mode,
                    input$show_projections, input$display_mode))

    tryCatch({

    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    target_region <- region$zone_id

    # Load the full historical record from the lazy Arrow dataset
    if (temp_mode == "Annual") {
      ds <- hist_annual_ds
    } else {
      ds <- hist_seasonal_ds
    }

    req(ds)

    # Filter to only the clicked region, correct spatial level, and variable
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    query <- ds |>
      dplyr::filter(variable == var_name,
                    SpatialLevel == sp_level,
                    Region == target_region)

    # If seasonal mode, filter to the active season
    if (temp_mode != "Annual") {
      query <- query |> dplyr::filter(Season == temp_mode)
    }

    df_region <- dplyr::collect(query)

    # Order chronologically by Year
    df_region <- df_region[order(df_region$Year), ]

    if (nrow(df_region) == 0) {
      # Return an empty plotly object with a text message if no data exists
      return(
        plot_ly() %>%
          layout(
            title = list(text = "No historical record found for this region", font = list(color = "#ffffff")),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    # Extract label and unit for the chart
    var_meta <- climate_variables[[var_name]]
    var_label <- var_meta$label
    var_unit <- var_meta$unit

    # Use the variable's primary palette color for the chart line/fill (last color is usually strong/dark)
    accent_color <- tail(var_meta$palette, 1)
    # If the color is too bright or white, use a nice highlight color
    if (accent_color %in% c("#f7fbff", "#ffeaa7")) {
      accent_color <- "#38bdf8" # Sky blue accent
    }

    # Check whether projections should be overlaid
    show_proj <- isTRUE(input$show_projections == "1")
    proj_data <- NULL
    baseline <- NULL
    if (show_proj) {
      proj_data <- filtered_projection_data()
      baseline <- baseline_mean_value()
    }

    # Determine whether we are using anomaly mode (projections ON + baseline computed)
    use_anomaly <- show_proj && isTRUE(input$display_mode == "anomaly") && !is.null(baseline) && is.finite(baseline)

    # Determine whether this is a relative (%) anomaly variable (precipitation)
    # or an absolute anomaly variable (temperature)
    is_relative_anomaly <- (var_name == "total_precipitation")

    # ── Apply anomaly transformation if active ──────────────────────────────────
    if (use_anomaly) {
      if (is_relative_anomaly) {
        # Precipitation: relative change (%) from baseline
        # Guard against zero baseline (would cause division by zero)
        if (abs(baseline) > 0.001) {
          df_region$Value <- (df_region$Value - baseline) / baseline * 100
          if (!is.null(proj_data) && nrow(proj_data) > 0) {
            proj_data$median_val <- (proj_data$median_val - baseline) / baseline * 100
            proj_data$min_val    <- (proj_data$min_val - baseline) / baseline * 100
            proj_data$max_val    <- (proj_data$max_val - baseline) / baseline * 100
          }
          anomaly_unit <- "%"
        } else {
          # Baseline is essentially zero — fall back to absolute anomaly
          df_region$Value <- df_region$Value - baseline
          if (!is.null(proj_data) && nrow(proj_data) > 0) {
            proj_data$median_val <- proj_data$median_val - baseline
            proj_data$min_val    <- proj_data$min_val - baseline
            proj_data$max_val    <- proj_data$max_val - baseline
          }
          anomaly_unit <- var_unit
        }
      } else {
        # Temperature (and other variables): absolute change from baseline
        df_region$Value <- df_region$Value - baseline
        if (!is.null(proj_data) && nrow(proj_data) > 0) {
          proj_data$median_val <- proj_data$median_val - baseline
          proj_data$min_val    <- proj_data$min_val - baseline
          proj_data$max_val    <- proj_data$max_val - baseline
        }
        anomaly_unit <- var_unit
      }

      # Parse reference period label for the axis and title
      ref_period_label <- input$reference_period
      y_axis_label <- sprintf("Change from %s (%s)", ref_period_label, anomaly_unit)
      hover_unit <- anomaly_unit

    } else {
      # Normal absolute mode (no anomalies)
      y_axis_label <- sprintf("%s (%s)", var_label, var_unit)
      hover_unit <- var_unit
    }

    # ── Determine chart title ───────────────────────────────────────────────────
    if (!is.null(proj_data) && nrow(proj_data) > 0) {
      scenario_label <- ssp_scenario_labels[input$ssp_scenario]
      chart_title <- sprintf("%s: %s (%s) \u2014 %s vs %s",
                              var_label, region$name, temp_mode,
                              scenario_label, input$reference_period)
    } else if (use_anomaly) {
      chart_title <- sprintf("%s Anomaly: %s (%s) vs %s",
                              var_label, region$name, temp_mode, input$reference_period)
    } else {
      chart_title <- sprintf("Historical Record: %s (%s)", region$name, temp_mode)
    }

    # ── Build the Plotly chart ──────────────────────────────────────────────────

    # Start with the historical line trace
    p <- plot_ly() %>%
      add_trace(
        data = df_region,
        x = ~Year,
        y = ~Value,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Historical (ERA5)',
        line = list(color = accent_color, width = 2),
        marker = list(color = accent_color, size = 4),
        text = ~paste0("Year: ", Year, "<br>", var_label, ": ",
                       round(Value, 2), " ", hover_unit),
        hoverinfo = 'text'
      )

    # ── Overlay projection data if available ────────────────────────────────────
    if (!is.null(proj_data) && nrow(proj_data) > 0) {

      # Get the SSP-specific color palette
      ssp_key <- input$ssp_scenario
      proj_line_color <- ssp_colors[[ssp_key]]$line
      proj_fill_color <- ssp_colors[[ssp_key]]$fill

      # Add the model agreement envelope (min-max band)
      # Plotly fill='tonexty' requires the traces in order: bottom boundary,
      # then top boundary with fill referencing the previous trace.
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
        # Ensemble median line (dashed)
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

    # ── Add shapes and annotations when projections are ON ──────────────────────
    chart_shapes <- list()
    chart_annotations <- list()

    if (show_proj) {

      # Vertical "Present Day" line at 2023
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

      # If anomaly mode is active, add reference period visual elements
      if (use_anomaly) {

        # Parse reference period years for the shaded band
        ref_years <- as.integer(strsplit(input$reference_period, "-")[[1]])
        ref_start <- ref_years[1]
        ref_end   <- ref_years[2]

        # Horizontal baseline line at y=0 (the "no change" reference)
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

        # Vertical shaded band highlighting the reference period
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

        # Label for the baseline line
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

        # Label for the reference period band
        ref_band_midpoint <- (ref_start + ref_end) / 2
        chart_annotations <- c(chart_annotations, list(
          list(
            x = ref_band_midpoint,
            y = 0.98,
            yref = "paper",
            text = input$reference_period,
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

    # ── Apply common chart layout ───────────────────────────────────────────────
    p %>%
      layout(
        title = list(
          text = chart_title,
          font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
          x = 0.05
        ),
        paper_bgcolor = "rgba(0,0,0,0)", # Fully transparent to blend with glassmorphism drawer
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
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = FALSE) # Clean interface without cluttering toolbars

    }, error = function(e) {
      message(sprintf("  *** Chart render ERROR: %s", conditionMessage(e)))
      plot_ly() %>%
        layout(
          title = list(text = paste("Chart error:", conditionMessage(e)), font = list(color = "#ef4444")),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    })
  })
}
