# server.R
# ==============================================================================
# Copernicus PECD v4.2 Visualization App — Initial Geographic Visualizer
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================

server <- function(input, output, session) {

  # Reactive state variables
  clicked_region    <- reactiveVal(NULL)
  map_loaded        <- reactiveVal(FALSE)
  geom_ready        <- reactiveVal(FALSE)

  # Incrementing trigger used to force a re-render of zone layers after a basemap
  # style switch. Needed because set_style(..., preserve_layers = FALSE) wipes all
  # custom layers and we must redraw them once the new tiles have loaded.
  style_trigger     <- reactiveVal(0)

  # Track the last min/max/value sent to the year slider so we can skip no-op
  # updateSliderInput calls. Without this guard, toggling projections always
  # causes updateSliderInput to round-trip through the client, re-setting
  # input$selected_year and triggering a second map redraw.
  last_slider_min   <- reactiveVal(NULL)
  last_slider_max   <- reactiveVal(NULL)
  last_slider_val   <- reactiveVal(NULL)

  # Track the source ID of any active satellite raster layer so we can remove it
  # cleanly before switching to a different basemap style.
  satellite_src_id  <- reactiveVal(NULL)

  # Dynamic layer stacking helper: determines which base map layer to insert
  # our custom polygon boundaries BEFORE. Setting this to "boundary_2"
  # ensures that all reference boundaries (national, regional) and text labels
  # (places, city names, country names) overlay on top of our colored polygons.
  target_before_id <- reactive({
    "boundary_2"
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

    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    # Provide a default value for tech mix since it might not be initialized immediately
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    # Determine whether to use projection data.
    # Standard variables: year > hist_max_year with projections ON.
    # Dynamic wind: use projection data for future years, but historical years
    # can now use ERA5 blended with fixed_2025 technology (see Rule 9.20).
    show_proj <- isTRUE(input$show_projections == "1")
    proj_data_exists <- (var_name %in% projection_available_variables &&
                         sp_level %in% projection_available_spatial_levels)
    is_dynamic_wind <- (is_wind_power && tech_mix_mode == "dynamic")
    
    hist_max_year <- get_historical_max_year(var_name, sp_level)
    use_projection <- (show_proj && (sel_year > hist_max_year) && proj_data_exists)

    if (use_projection) {
      # ── Read from projection dataset ─────────────────────────────────────────
      req(input$ssp_scenario)

      if (is_wind_power) {
        df_raw <- blend_wind_power_all_regions(
          tech_mix_mode = tech_mix_mode,
          wind_type = wind_type,
          ds_annual = proj_annual_ds,
          ds_monthly = proj_monthly_ds,
          ds_seasonal = proj_seasonal_ds,
          temporal_mode = temp_mode,
          sp_level = sp_level,
          year = sel_year,
          scenario_val = input$ssp_scenario
        )
      } else {
        # Query all 6 models for the selected scenario + year.
        # Only read Region + Value — we only need these for the per-region median.
        df_raw <- query_arrow_dataset(
          proj_annual_ds, proj_seasonal_ds, proj_monthly_ds, temp_mode,
          var_name, sp_level,
          year = sel_year, scenario_val = input$ssp_scenario,
          select_cols = c("Region", "Value")
        )
      }

      if (is.null(df_raw)) return(NULL)

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
      # ── Read from historical dataset ─────────────────────────────────────────
      if (is_wind_power) {
        df_raw <- blend_wind_power_all_regions(
          tech_mix_mode = tech_mix_mode,
          wind_type = wind_type,
          ds_annual = hist_annual_ds,
          ds_monthly = hist_monthly_ds,
          ds_seasonal = hist_seasonal_ds,
          temporal_mode = temp_mode,
          sp_level = sp_level,
          year = sel_year
        )
        if (is.null(df_raw)) return(NULL)
        return(df_raw |> dplyr::select(Region, Value))
      } else {
        # Only read Region + Value — that's all the map choropleth needs.
        query_arrow_dataset(
          hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, temp_mode,
          var_name, sp_level,
          year = sel_year,
          select_cols = c("Region", "Value")
        )
      }
    }
  })

  # ----------------------------------------------------------------------------
  # Reset Filters Button
  # ----------------------------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "spatial_level", selected = "NUT0")
    updateSelectInput(session, "climate_variable", selected = "2m_temperature")
    updateSelectInput(session, "technology_mix", selected = "dynamic")
    updateSelectInput(session, "temporal_mode", selected = "Annual")
    updateSliderInput(session, "selected_year", value = 2021)
    updateSelectInput(session, "historical_period", selected = "1981-2010")
    updateSelectInput(session, "ssp_scenario", selected = "ssp2_4_5")
    updateSelectInput(session, "projection_period", selected = "2021-2040")
    updateSliderInput(session, "polygon_opacity", value = 0.75)
    
    # Reset basemap layers (popover settings)
    updateRadioButtons(session, "basemap", selected = "ofm_positron")
    updateCheckboxInput(session, "show_boundaries", value = TRUE)
    updateCheckboxInput(session, "show_labels", value = TRUE)
    session$sendCustomMessage("reset_custom_toggles", list())
    
    # Close drawer, clear selection, and zoom out
    clicked_region(NULL)
    session$sendCustomMessage("toggle_stats_drawer", list(show = FALSE))
    maplibre_proxy("map") %>% clear_layer("zone-highlight")
    
    if (map_loaded() && !is.null(current_boundaries())) {
      bbox <- sf::st_bbox(current_boundaries())
      maplibre_proxy("map") %>%
        fit_bounds(
          c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
          animate = TRUE,
          padding = list(top = 40, bottom = 40, left = 320, right = 40)
        )
    }
  })

  # ----------------------------------------------------------------------------
  # Wind Power UI Observers
  # ----------------------------------------------------------------------------
  observeEvent(input$climate_variable, {
    is_wind <- input$climate_variable %in% c("wind_power_onshore", "wind_power_offshore")
    session$sendCustomMessage("toggle_tech_mix_controls", list(show = is_wind))
    
    # NUTS 0 deprecation warning
    if (is_wind && !is.null(input$spatial_level) && input$spatial_level == "NUT0") {
      showNotification(
        "NUTS 0 wind power capacity factors are synthesized dynamically using area-weighting from granular spatial tiers. Use with caution for national capacity planning.",
        type = "warning", duration = 8, id = "nut0_wind_warning"
      )
    }
  }, ignoreInit = FALSE)
  
  # ----------------------------------------------------------------------------
  # Dynamic UI Updates for Climate Variable
  # ----------------------------------------------------------------------------
  # Wind power data is not available at the NUT2 level.
  # This observer updates the PECD Variable dropdown to hide Energy Indicators
  # when an incompatible spatial tier is selected.
  observeEvent(input$spatial_level, {
    sp <- input$spatial_level
    
    # Base choices available everywhere
    base_choices <- c(
      "2m Temperature" = "2m_temperature",
      "Total Precipitation" = "total_precipitation",
      "Solar Radiation" = "surface_solar_radiation_downwards",
      "10m Wind Speed" = "10m_wind_speed",
      "100m Wind Speed" = "100m_wind_speed"
    )
    
    # PECD v4.2 deprecates NUT0 aggregation for energy variables due to inaccuracy.
    # We also hide them for SZON/SZOF because the raw parquet data maps those regions into P2ON/P2OF.
    # Therefore, wind power is ONLY officially supported and shown on P2ON and P2OF.
    show_energy <- (sp %in% c("P2ON", "P2OF"))
    
    choices_list <- list("Climate Variables" = base_choices)
    if (show_energy) {
      energy_choices <- c(
        "Wind Power Onshore (CF)" = "wind_power_onshore",
        "Wind Power Offshore (CF)" = "wind_power_offshore"
      )
      
      # Optional polish: Only show onshore for onshore zones, offshore for offshore zones
      if (sp == "P2ON") {
        energy_choices <- energy_choices["Wind Power Onshore (CF)"]
      } else if (sp == "P2OF") {
        energy_choices <- energy_choices["Wind Power Offshore (CF)"]
      }
      
      choices_list[["Energy Indicators"]] <- energy_choices
    }
    
    # Preserve current selection if it's still available, else default to 2m temp
    curr_sel <- input$climate_variable
    all_valid_vals <- unname(unlist(choices_list))
    if (!(curr_sel %in% all_valid_vals)) {
      curr_sel <- "2m_temperature"
    }
    
    updateSelectInput(session, "climate_variable", choices = choices_list, selected = curr_sel)
  })

  # ----------------------------------------------------------------------------
  # Dynamic UI Updates for Anomaly Mode
  # ----------------------------------------------------------------------------
  # Anomalies are scientifically misleading for Wind Power when "Dynamic" is active,
  # because the projection uses different turbine heights/technologies than the baseline.
  observe({
    req(input$climate_variable)
    is_wind <- input$climate_variable %in% c("wind_power_onshore", "wind_power_offshore")
    tech_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"
    
    # Disable anomaly if Wind Power + Dynamic — the baseline uses fixed_2025 tech
    # while projections use a shifting mix, so anomalies would conflate technology
    # and climate signals.
    is_dynamic_wind <- is_wind && (tech_mode == "dynamic")
    session$sendCustomMessage("set_anomaly_disabled", list(disable = is_dynamic_wind))
    session$sendCustomMessage("set_projection_forced", list(force_on = FALSE))
  })

  # ----------------------------------------------------------------------------
  # Dynamic Year Slider Bounds based on Spatial Tier + Projection State
  # ----------------------------------------------------------------------------
  # Onshore PECD spatial tiers (NUT0, NUT2, P2ON, SZON) only contain historical
  # data up to 2021. Offshore tiers (P2OF, SZOF) go up to 2023.
  # When projections are toggled ON and the selected variable has projection
  # data, the slider extends to 2100 to allow browsing projected values.
  # ----------------------------------------------------------------------------
  observe({
    req(input$spatial_level, input$temporal_mode, input$climate_variable)

    # Determine the historical maximum year dynamically based on variable + spatial level
    sp_level_pq <- spatial_level_to_parquet[input$spatial_level]
    hist_max_year <- get_historical_max_year(input$climate_variable, sp_level_pq)

    # Check if projections should extend the slider
    show_proj <- isTRUE(input$show_projections == "1")
    var_name  <- input$climate_variable
    sp_level  <- spatial_level_to_parquet[input$spatial_level]
    proj_data_exists <- (var_name %in% projection_available_variables &&
                         sp_level %in% projection_available_spatial_levels)

    max_year <- if (show_proj && proj_data_exists) 2100 else hist_max_year

    # Winter season has incomplete 1950 data, so min year is 1951 for seasonal mode
    min_year <- if (input$temporal_mode == "Annual") 1950 else 1951
    
    # Note: Dynamic wind mode now shows blended historical data (fixed_2025 tech),
    # so no special min_year override is needed — users can browse all years.

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

    # Guard against no-op updates that cause Shiny to round-trip the slider
    # value through the client, which re-fires input$selected_year and
    # triggers a redundant second polygon redraw.
    #
    # Key insight: updateSliderInput with a `value` argument always causes
    # the client to emit a new input$selected_year event — even if the value
    # is the same. So we split into two cases:
    #   1. Value needs clamping (it fell outside new bounds) → pass value
    #   2. Only min/max changed, value stays the same → omit value param
    # Case 2 updates the slider visually without triggering a value round-trip.
    prev_min <- isolate(last_slider_min())
    prev_max <- isolate(last_slider_max())

    value_needs_clamping <- (new_val != current_yr)
    bounds_need_update   <- !identical(prev_min, min_year) ||
                            !identical(prev_max, max_year)

    if (value_needs_clamping) {
      # The value was clamped to fit new bounds — must send value to client
      last_slider_min(min_year)
      last_slider_max(max_year)
      last_slider_val(new_val)

      updateSliderInput(
        session = session,
        inputId = "selected_year",
        min = min_year,
        max = max_year,
        value = new_val
      )
    } else if (bounds_need_update) {
      # Only min/max changed, value stays the same — omit value param
      # to avoid Shiny round-tripping input$selected_year back to the server
      last_slider_min(min_year)
      last_slider_max(max_year)
      last_slider_val(new_val)

      updateSliderInput(
        session = session,
        inputId = "selected_year",
        min = min_year,
        max = max_year
      )
    }
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
  # Computes the mean Value for EVERY region at the current spatial level over
  # the selected WMO reference period (e.g., 1981-2010).
  # Returns a data.frame with columns: Region, baseline_value
  # Used by the map rendering observer to convert all polygon values to anomalies.
  # ----------------------------------------------------------------------------
  baseline_map_data <- reactive({
    req(input$show_projections == "1", input$display_mode == "anomaly")
    req(input$historical_period, input$climate_variable, input$temporal_mode, input$spatial_level)

    # Parse reference period
    ref_years <- as.integer(strsplit(input$historical_period, "-")[[1]])
    ref_start <- ref_years[1]
    ref_end   <- ref_years[2]

    var_name  <- input$climate_variable
    temp_mode <- input$temporal_mode
    sp_level  <- spatial_level_to_parquet[input$spatial_level]
    
    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    if (is_wind_power) {
      df_ref <- blend_wind_power_all_regions(
        tech_mix_mode = tech_mix_mode,
        wind_type = wind_type,
        ds_annual = hist_annual_ds,
          ds_monthly = hist_monthly_ds,
        ds_seasonal = hist_seasonal_ds,
        temporal_mode = temp_mode,
        sp_level = sp_level,
        year_start = ref_start,
        year_end = ref_end
      )
    } else {
      # Query ALL regions for the reference period using centralized helper
      # Only read Region + Value — we just need per-region means.
      df_ref <- query_arrow_dataset(
        hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, temp_mode,
        var_name, sp_level,
        year_start = ref_start, year_end = ref_end,
        select_cols = c("Region", "Value")
      )
    }

    if (is.null(df_ref)) return(NULL)

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
    show_proj <- isTRUE(input$show_projections == "1")
    map_period <- if (show_proj) input$projection_period else input$historical_period
    req(map_period)
    req(input$climate_variable, input$temporal_mode, input$spatial_level)

    # Parse period (e.g., "2041-2060" or "1981-2010")
    period_years <- as.integer(strsplit(map_period, "-")[[1]])
    period_start <- period_years[1]
    period_end   <- period_years[2]

    var_name  <- input$climate_variable
    temp_mode <- input$temporal_mode
    sp_level  <- spatial_level_to_parquet[input$spatial_level]
    
    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    # Decide whether this is a historical or projected period.
    # Dynamic wind now uses blended historical data (fixed_2025 tech via
    # backward clamping), so historical periods are valid for all modes.
    is_historical_period <- (period_end <= 2023)

    if (is_historical_period) {
      # ── Historical period: mean from ERA5 reanalysis ──────────────────────────
      if (is_wind_power) {
        df_raw <- blend_wind_power_all_regions(
          tech_mix_mode = tech_mix_mode,
          wind_type = wind_type,
          ds_annual = hist_annual_ds,
          ds_monthly = hist_monthly_ds,
          ds_seasonal = hist_seasonal_ds,
          temporal_mode = temp_mode,
          sp_level = sp_level,
          year_start = period_start,
          year_end = period_end
        )
      } else {
        # Only read Region + Value — we compute per-region mean over the period.
        df_raw <- query_arrow_dataset(
          hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, temp_mode,
          var_name, sp_level,
          year_start = period_start, year_end = period_end,
          select_cols = c("Region", "Value")
        )
      }

      if (is.null(df_raw)) return(NULL)

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

      if (is_wind_power) {
        df_raw <- blend_wind_power_all_regions(
          tech_mix_mode = tech_mix_mode,
          wind_type = wind_type,
          ds_annual = proj_annual_ds,
          ds_monthly = proj_monthly_ds,
          ds_seasonal = proj_seasonal_ds,
          temporal_mode = temp_mode,
          sp_level = sp_level,
          year_start = period_start,
          year_end = period_end,
          scenario_val = scenario_val
        )
      } else {
        # Need Region + Value + model — we group by model first, then take median.
        df_raw <- query_arrow_dataset(
          proj_annual_ds, proj_seasonal_ds, proj_monthly_ds, temp_mode,
          var_name, sp_level,
          year_start = period_start, year_end = period_end,
          scenario_val = scenario_val,
          select_cols = c("Region", "Value", "model")
        )
      }

      if (is.null(df_raw)) return(NULL)

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

  # NOTE: The period dropdown (projection_period) was previously updated here
  # via updateSelectInput() inside an observeEvent(input$projections_toggled).
  # This caused an async R → browser → R round-trip that triggered a SECOND
  # render of the map polygons every time projections were toggled.
  #
  # The dropdown is now updated entirely client-side via the selectize JS API
  # in app.js. All input changes (show_projections + projection_period) arrive
  # at the server in a single Shiny message batch → single render cycle.

  # ----------------------------------------------------------------------------
  # Geometry Observer — GeoJSON URL Source Swap
  # ----------------------------------------------------------------------------
  # This observer fires when the spatial tier or basemap style changes.
  # Instead of serializing sf objects and sending them over the websocket
  # (~2.5 MB), we send a GeoJSON file URL (~50 bytes). MapLibre fetches
  # the file directly from the static server — no R serialization needed.
  # ----------------------------------------------------------------------------
  observe({
    req(map_loaded())
    style_trigger()                   # re-fire on style change

    geom_data <- current_boundaries()
    req(geom_data)

    # Disable data painting while geometry is refreshing
    geom_ready(FALSE)

    # Show loading shimmer while the new geometry loads
    session$sendCustomMessage("map_loading_shimmer", list(show = TRUE))

    # Get the GeoJSON file URL for the current spatial tier.
    # Shiny serves www/ contents at the app root, so "data/geo/pecd_NUT0.geojson"
    # is accessible at http://host:port/data/geo/pecd_NUT0.geojson.
    # MapLibre loads this directly via HTTP — no R serialization needed.
    current_level <- input$spatial_level
    geojson_file  <- spatial_levels[[current_level]]$file
    geojson_url   <- paste0("data/geo/", geojson_file)

    message(sprintf("Geometry Observer: Swapping to GeoJSON URL for %s (%s)...",
                    current_level, geojson_url))

    # Send the swap command to JavaScript — this is ~50 bytes (just the URL string)
    # instead of serializing the full sf object over the websocket (~2.5 MB).
    # We use a semi-transparent thin white outline — it's neutral and looks
    # premium over any color palette (temp reds, hydro blues, solar yellows).
    session$sendCustomMessage("swap_tile_source", list(
      url            = geojson_url,
      border_color   = "#ffffff",
      border_width   = 0.5,
      border_opacity = 0.6
    ))

    # Restore the crimson highlight if a region was selected before the style switch.
    # We use isolate() so this doesn't create a reactive dependency on clicked_region.
    # This is the only place we still use sf geometry — for a single polygon, not the
    # entire tier. This is negligible payload.
    selected <- isolate(clicked_region())
    if (!is.null(selected)) {
      highlight_geom <- geom_data %>% dplyr::filter(zone_id == selected$zone_id)
      if (nrow(highlight_geom) > 0) {
        message(sprintf("Restoring highlight for: %s after tile swap...", selected$name))
        maplibre_proxy("map") %>%
          add_line_layer(
            id           = "zone-highlight",
            source       = highlight_geom,
            line_color   = "#d9534f",
            line_width   = 3.0,
            line_opacity = 0.95
          )
      }
    }

    # Hide the loading shimmer — geometry swap has been dispatched
    session$sendCustomMessage("map_loading_shimmer", list(show = FALSE))

    # Signal the Data/Color Observer that the fill layer now exists on the map
    geom_ready(TRUE)
  })

  # ----------------------------------------------------------------------------
  # Data & Color Observer
  # ----------------------------------------------------------------------------
  # This observer fires on climate data changes (year, variable, period, etc).
  # It calculates colors and tooltips in R, then sends lightweight JS commands 
  # to recolor the polygons and update the custom tooltips instantly.
  # ----------------------------------------------------------------------------
  observe({
    req(geom_ready())
    geom_data <- isolate(current_boundaries())
    req(geom_data)

    view_mode <- input$projection_view_mode            # "year" or "period"
    show_proj <- isolate(isTRUE(input$show_projections == "1"))
    display_mode_val <- input$display_mode
    use_period <- isTRUE(view_mode == "period")

    if (use_period) {
      clim_data <- period_averaged_climate_data()
    } else {
      clim_data <- filtered_climate_data()
    }

    baseline_df <- if (show_proj && isTRUE(display_mode_val == "anomaly")) baseline_map_data() else NULL

    update_map_choropleth(
      session = session,
      geom_data = geom_data,
      clim_data = clim_data,
      baseline_df = baseline_df,
      climate_variable = input$climate_variable,
      selected_year = input$selected_year,
      display_mode = display_mode_val,
      show_projections = show_proj,
      projection_period = isolate(input$projection_period),
      historical_period = isolate(input$historical_period),
      technology_mix = input$technology_mix,
      spatial_level = input$spatial_level,
      polygon_opacity = isolate(input$polygon_opacity),
      view_mode = view_mode
    )
  })

  # ----------------------------------------------------------------------------
  # Clear selection when the spatial tier changes (not on basemap switch)
  # ----------------------------------------------------------------------------
  observeEvent(input$spatial_level, {
    clicked_region(NULL)
    session$sendCustomMessage("toggle_stats_drawer", list(show = FALSE))
    # NOTE: The spatial tier dropdown loading spinner is shown immediately on
    # the client side via JS (on 'change' event), before this server observer
    # even fires. The central zone renderer sends the 'hide' message once
    # polygon rendering is complete.
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
  # Label Visibility Controller
  # --------------------------------------------------------------------------
  observeEvent(input$show_labels, {
    req(map_loaded())
    session$sendCustomMessage("toggle_labels", list(visible = isTRUE(input$show_labels)))
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
    
    # Check if the region has valid data before allowing selection
    view_mode <- isolate(input$projection_view_mode)
    clim_data <- isolate(if (isTRUE(view_mode == "period")) period_averaged_climate_data() else filtered_climate_data())
    
    if (!is.null(clim_data)) {
      # SZOF normalization: parquet Region values have _OFF stripped, but GeoJSON
      # zone_ids still include it. Use a normalized key for lookup.
      lookup_id <- props$zone_id
      if (isolate(input$spatial_level) == "SZOF") {
        lookup_id <- sub("_OFF$", "", lookup_id)
      }
      region_row <- clim_data[clim_data$Region == lookup_id, ]
      if (nrow(region_row) == 0 || all(is.na(region_row$Value))) {
        message(sprintf("Ignoring click on %s: No data available", props$name))
        return()
      }
    }
    
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

      # Calculate dynamic padding based on current window/drawer state
      # The stats drawer covers ~40vh (collapsed) or ~65vh (expanded).
      bottom_pad <- 420
      if (!is.null(input$drawer_state)) {
        vh <- if (!is.null(input$drawer_state$vh)) input$drawer_state$vh else 1000
        is_expanded <- if (!is.null(input$drawer_state$expanded)) input$drawer_state$expanded else FALSE
        
        bottom_pad <- if (is_expanded) (vh * 0.65) + 20 else (vh * 0.40) + 20
        bottom_pad <- min(bottom_pad, vh - 150)
        bottom_pad <- max(60, bottom_pad)
      }

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
          # dynamic bottom padding pushes the polygon into the upper viewport,
          # keeping it visible above the stats drawer. left = 340 clears
          # the control panel. maxZoom = 7 keeps the view "one level out" for small regions.
          padding = list(top = 60, bottom = bottom_pad, left = 340, right = 60),
          maxZoom = 7.0
        )
    }
  })

  # ----------------------------------------------------------------------------
  # Dynamic Map Zoom on Drawer Resize
  # ----------------------------------------------------------------------------
  # When the drawer is expanded, collapsed, or the window is resized, we must
  # re-fit the map bounds to keep the selected region visible in the remaining viewport.
  observeEvent(input$drawer_state, {
    req(map_loaded(), clicked_region())
    
    # We only care if the drawer is visible and we need to re-fit bounds.
    if (!isTRUE(input$drawer_state$visible)) return()
    
    props <- clicked_region()
    highlight_geom <- current_boundaries() %>% filter(zone_id == props$zone_id)
    if (nrow(highlight_geom) == 0) return()
    
    bbox <- sf::st_bbox(highlight_geom)
    
    maplibre_proxy("map") %>%
      fit_bounds(
        c(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]),
        animate = TRUE,
        padding = list(top = 60, bottom = input$drawer_state$bottom_padding, left = 340, right = 60),
        maxZoom = 7.0
      )
  }, ignoreInit = TRUE)

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
          session$sendCustomMessage("toggle_labels", list(visible = isTRUE(isolate(input$show_labels))))
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
          session$sendCustomMessage("toggle_labels", list(visible = isTRUE(isolate(input$show_labels))))
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
    build_region_stats_cards(
      region = clicked_region(),
      spatial_level = input$spatial_level,
      show_projections = input$show_projections,
      projection_style = isolate(input$projection_style)
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
    show_proj <- isTRUE(input$show_projections == "1")
    proj_period <- if (show_proj) input$projection_period else input$historical_period

    var_meta <- climate_variables[[input$climate_variable]]
    is_precip <- (input$climate_variable == "total_precipitation")

    # Check display mode
    show_proj <- isTRUE(input$show_projections == "1")
    use_anomaly_legend <- (show_proj && isTRUE(input$display_mode == "anomaly"))
    use_period <- isTRUE(view_mode == "period")
    sel_year <- as.integer(input$selected_year)

    # Determine if the current view shows projected data.
    # Dynamic wind now uses blended historical data (fixed_2025 tech), so the
    # standard year-based check applies to all modes.
    is_wind <- input$climate_variable %in% c("wind_power_onshore", "wind_power_offshore")
    tech_mix_val <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    sp_level_pq <- spatial_level_to_parquet[input$spatial_level]
    hist_max_year <- get_historical_max_year(input$climate_variable, sp_level_pq)

    if (use_period && !is.null(proj_period) && nchar(proj_period) > 0) {
      period_end_year <- as.integer(strsplit(proj_period, "-")[[1]][2])
      is_projection_data <- (period_end_year > hist_max_year)
    } else {
      is_projection_data <- (sel_year > hist_max_year)
    }

    # Build the year/period label for titles
    time_label <- if (use_period) proj_period else as.character(input$selected_year)

    # Get the current data range (use period data when in period mode)
    clim_data <- if (use_period) period_averaged_climate_data() else filtered_climate_data()
    if (is.null(clim_data) || nrow(clim_data) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }

    bounds <- current_boundaries()
    if (!is.null(bounds)) {
      active_zones <- bounds$zone_id
      if (input$spatial_level == "SZOF") {
        active_zones <- sub("_OFF$", "", active_zones)
      }
      clim_data <- clim_data %>% dplyr::filter(Region %in% active_zones)
    }

    vals <- clim_data$Value[is.finite(clim_data$Value)]
    if (length(vals) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }

    # Fetch baseline data for anomaly range calculation (NULL when not in anomaly mode)
    baseline_df <- if (use_anomaly_legend) baseline_map_data() else NULL

    # Compute all legend parameters using the helper function
    legend_params <- compute_legend_params(
      var_meta           = var_meta,
      is_precip          = is_precip,
      use_anomaly_legend = use_anomaly_legend,
      temporal_mode      = input$temporal_mode,
      time_label         = time_label,
      is_projection_data = is_projection_data,
      use_period         = use_period,
      ssp_scenario       = input$ssp_scenario,
      reference_period   = input$historical_period,
      clim_data          = clim_data,
      baseline_df        = baseline_df
    )

    # Build and return the legend UI tags
    build_legend_ui(legend_params)
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
  # ----------------------------------------------------------------------------
  # Centralized Data Reactives (Single Source of Truth)
  # ----------------------------------------------------------------------------
  
  # Historical Data for Trends (Year, Value)
  historical_trends_data <- reactive({
    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode)
    
    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    target_region <- region$zone_id
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    
    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    if (is_wind_power && tech_mix_mode == "dynamic") {
      # Dynamic mode: show historical ERA5 data blended with the 2025 technology
      # mix weights. This is consistent with the backward clamping rule (Rule 8):
      # all years <= 2025 use the 2025 mix, so blending ERA5 with fixed_2025
      # produces a scientifically valid historical baseline that connects
      # seamlessly to the dynamically-evolving projection line.
      df_region <- blend_wind_power_timeseries(
        region_id = target_region,
        tech_mix_mode = "fixed_2025",
        wind_type = wind_type,
        ds_annual = hist_annual_ds,
        ds_monthly = hist_monthly_ds,
        ds_seasonal = hist_seasonal_ds,
        temporal_mode = temp_mode,
        sp_level = sp_level
      )
      if (!is.null(df_region)) df_region <- df_region |> dplyr::select(Year, Value)
    } else if (is_wind_power) {
      df_region <- blend_wind_power_timeseries(
        region_id = target_region,
        tech_mix_mode = tech_mix_mode,
        wind_type = wind_type,
        ds_annual = hist_annual_ds,
        ds_monthly = hist_monthly_ds,
        ds_seasonal = hist_seasonal_ds,
        temporal_mode = temp_mode,
        sp_level = sp_level
      )
      if (!is.null(df_region)) df_region <- df_region |> dplyr::select(Year, Value)
    } else {
      df_region <- query_arrow_dataset(
        hist_annual_ds, hist_seasonal_ds, hist_monthly_ds, temp_mode,
        var_name, sp_level,
        target_region = target_region,
        select_cols = c("Year", "Value")
      )
    }
    
    if (!is.null(df_region)) df_region <- df_region[order(df_region$Year), ]
    df_region
  })

  # Helper to resolve Seasonality query parameters
  seasonality_query_params <- reactive({
    region_data <- clicked_region()
    req(region_data)
    
    var_name <- input$climate_variable
    query_var <- var_name
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"
    
    # Track whether this is a wind variable that needs blending for historical data.
    # Dynamic and fixed_2025 modes require the blending engine (weighted average of
    # multiple turbine technologies), not a single raw variable query.
    is_wind_power <- grepl("wind_power", var_name)
    wind_type <- ifelse(grepl("onshore", var_name), "onshore", "offshore")
    needs_hist_blending <- FALSE
    
    if (is_wind_power) {
      if (tech_mix_mode %in% c("dynamic", "fixed_2025")) {
        # Dynamic mode clamps to the 2025 technology mix for historical years
        # (all historical years are <= 2025 per Rule 8). fixed_2025 uses the
        # same 2025 weights. Both require the blending engine, not a single
        # raw tech variable, so we flag this for historical_seasonality_data().
        # For projection queries, the raw variable approach still works as a
        # reasonable fallback — but the blending engine is preferred.
        needs_hist_blending <- TRUE
        # Still set a fallback query_var for projection queries that don't use
        # the blending engine (e.g. all_scenarios_projection_seasonality_data)
        if (wind_type == "onshore") {
          query_var <- "wind_onshore_34"
        } else {
          query_var <- "wind_offshore_21"
        }
      } else {
        # Fixed technology modes: map directly to the single raw variable
        if (wind_type == "onshore") {
          query_var <- switch(tech_mix_mode,
            "fixed_2020" = "wind_onshore_30",
            "fixed_2030" = "wind_onshore_34",
            "fixed_2040" = "wind_onshore_34",
            "fixed_2050" = "wind_onshore_34",
            "wind_onshore_34"
          )
        } else {
          query_var <- switch(tech_mix_mode,
            "fixed_2020" = "wind_offshore_20",
            "fixed_2030" = "wind_offshore_21",
            "fixed_2040" = "wind_offshore_21",
            "fixed_2050" = "wind_offshore_21",
            "wind_offshore_21"
          )
        }
      }
    }
    
    view_mode <- if (!is.null(input$projection_view_mode)) input$projection_view_mode else "year"
    ref_period <- if (!is.null(input$historical_period)) input$historical_period else "1991-2020"
    
    if (view_mode == "period") {
      req(input$projection_period)
      target_period <- input$projection_period
      period_years <- as.integer(strsplit(target_period, "-")[[1]])
      proj_start <- period_years[1]
      proj_end   <- period_years[2]
    } else {
      map_year <- if (is.null(input$selected_year)) 2040 else as.numeric(input$selected_year)
      if (map_year <= 2020) {
        proj_start <- 2021; proj_end <- 2040
      } else {
        proj_start <- max(2021, map_year - 10)
        proj_end <- min(2100, map_year + 10)
      }
    }
    
    list(
      region_id = region_data[["zone_id"]],
      query_var = query_var,
      sp_level = spatial_level_to_parquet[input$spatial_level],
      ref_start = as.numeric(substr(ref_period, 1, 4)),
      ref_end = as.numeric(substr(ref_period, 6, 9)),
      proj_start = proj_start,
      proj_end = proj_end,
      ssp = input$ssp_scenario,
      # Wind blending context — used by historical_seasonality_data() to decide
      # whether it needs the full blending engine or a simple Arrow query
      needs_hist_blending = needs_hist_blending,
      wind_type = wind_type,
      is_wind_power = is_wind_power
    )
  })

  # Historical Data for Seasonality (Year, Month, Value)
  # When the dynamic or fixed_2025 technology mix is selected for wind power,
  # the historical boxplot must use the blending engine with fixed_2025 mode.
  # This ensures the 12-month seasonal shape is computed from a properly
  # weighted average of multiple turbine technologies at the 2025 anchor,
  # consistent with the dynamic engine's backward clamping rule (Rule 8:
  # all years <= 2025 use the 2025 technology mix).
  historical_seasonality_data <- reactive({
    params <- seasonality_query_params()
    
    if (params$needs_hist_blending) {
      # Use the blending engine with fixed_2025 mode to compute a weighted
      # average of multiple turbine technologies for this region's resource group
      df <- blend_wind_power_timeseries(
        region_id = params$region_id,
        tech_mix_mode = "fixed_2025",
        wind_type = params$wind_type,
        ds_annual = hist_monthly_ds,
        ds_monthly = hist_monthly_ds,
        ds_seasonal = hist_seasonal_ds,
        temporal_mode = "Annual",
        sp_level = params$sp_level
      )
      # blend_wind_power_timeseries returns all years in the dataset. Filter to
      # the user's selected reference period (e.g. 1991-2020) for the boxplot.
      if (!is.null(df) && nrow(df) > 0) {
        df <- df[df$Year >= params$ref_start & df$Year <= params$ref_end, ]
        if (nrow(df) == 0) df <- NULL
      }
    } else {
      # Standard path: query a single variable directly from Arrow
      df <- query_arrow_dataset(
        ds_annual = hist_monthly_ds, ds_seasonal = hist_seasonal_ds, ds_monthly = hist_monthly_ds,
        temporal_mode = "Annual",
        var_name = params$query_var,
        sp_level = params$sp_level,
        year_start = params$ref_start, 
        year_end = params$ref_end,
        target_region = params$region_id
      )
    }
    if (!is.null(df)) df <- as.data.frame(df)
    df
  })
  
  # Projection Data for Seasonality (Single Scenario)
  projection_seasonality_data <- reactive({
    req(input$show_projections == "1")
    params <- seasonality_query_params()
    df <- query_arrow_dataset(
      ds_annual = proj_monthly_ds, ds_seasonal = proj_seasonal_ds, ds_monthly = proj_monthly_ds,
      temporal_mode = "Annual",
      var_name = params$query_var,
      sp_level = params$sp_level,
      year_start = params$proj_start, 
      year_end = params$proj_end,
      target_region = params$region_id,
      scenario_val = params$ssp
    )
    if (!is.null(df)) df <- as.data.frame(df)
    df
  })
  
  # Projection Data for Seasonality (All Scenarios)
  all_scenarios_projection_seasonality_data <- reactive({
    req(input$show_projections == "1")
    params <- seasonality_query_params()
    df <- query_arrow_dataset(
      ds_annual = proj_monthly_ds, ds_seasonal = proj_seasonal_ds, ds_monthly = proj_monthly_ds,
      temporal_mode = "Annual",
      var_name = params$query_var,
      sp_level = params$sp_level,
      year_start = params$proj_start, 
      year_end = params$proj_end,
      target_region = params$region_id,
      scenario_val = NULL
    )
    if (!is.null(df)) df <- as.data.frame(df)
    df
  })
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

    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    # Guard: exit early if this combination has no projection data
    if (!(var_name %in% projection_available_variables)) return(NULL)
    if (!(sp_level %in% projection_available_spatial_levels)) return(NULL)

    if (is_wind_power) {
      df_proj <- blend_wind_power_timeseries(
        region_id = target_region,
        tech_mix_mode = tech_mix_mode,
        wind_type = wind_type,
        ds_annual = proj_annual_ds,
          ds_monthly = proj_monthly_ds,
        ds_seasonal = proj_seasonal_ds,
        temporal_mode = temp_mode,
        sp_level = sp_level,
        scenario_val = scenario
      )
    } else {
      # Query all 6 models for the chosen scenario using centralized helper.
      # Read Year, Value, and model for both ensemble stats and spaghetti plots.
      df_proj <- query_arrow_dataset(
        proj_annual_ds, proj_seasonal_ds, proj_monthly_ds, temp_mode,
        var_name, sp_level,
        target_region = target_region, scenario_val = scenario,
        select_cols = c("Year", "Value", "model")
      )
    }

    if (is.null(df_proj)) return(NULL)

    # Arrow dataset queries do not guarantee row order. We must explicitly sort 
    # the data chronologically so that Plotly draws the spaghetti lines cleanly 
    # from left to right instead of zig-zagging backward and forward in time.
    df_proj <- df_proj[order(df_proj$Year), ]

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

    list(
      ensemble = ensemble_stats,
      models = df_proj
    )
  })

  all_scenarios_projection_data <- reactive({
    req(input$show_projections == "1")
    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode, input$spatial_level)

    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    target_region <- region$zone_id
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    # Query all 4 scenarios at once
    scenarios <- c("ssp1_2_6", "ssp2_4_5", "ssp3_7_0", "ssp5_8_5")

    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    if (!(var_name %in% projection_available_variables)) return(NULL)
    if (!(sp_level %in% projection_available_spatial_levels)) return(NULL)

    if (is_wind_power) {
      df_proj <- blend_wind_power_timeseries(
        region_id = target_region,
        tech_mix_mode = tech_mix_mode,
        wind_type = wind_type,
        ds_annual = proj_annual_ds,
        ds_monthly = proj_monthly_ds,
        ds_seasonal = proj_seasonal_ds,
        temporal_mode = temp_mode,
        sp_level = sp_level,
        scenario_val = scenarios
      )
    } else {
      df_proj <- query_arrow_dataset(
        proj_annual_ds, proj_seasonal_ds, proj_monthly_ds, temp_mode,
        var_name, sp_level,
        target_region = target_region, scenario_val = scenarios,
        select_cols = c("Year", "Value", "scenario", "model")
      )
    }

    if (is.null(df_proj)) return(NULL)
    df_proj <- df_proj[order(df_proj$Year), ]

    # Compute ensemble median per Year per Scenario
    ensemble_stats <- df_proj |>
      dplyr::group_by(Year, scenario) |>
      dplyr::summarise(
        median_val = median(Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(Year, scenario)

    list(
      ensemble = ensemble_stats,
      models = df_proj
    )
  })

  # NOTE: baseline_mean_value() used to be a separate reactive that queried
  # the historical dataset a second time for the reference period. This was
  # redundant because renderPlotly already fetches the FULL historical record
  # (all years) for the clicked region. The baseline is now computed inline
  # in renderPlotly from that same df_region, eliminating one Arrow query
  # per region click. See the "Compute baseline inline" block below.

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
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    
    is_wind_power <- var_name %in% c("wind_power_onshore", "wind_power_offshore")
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    df_region <- historical_trends_data()

    # Dynamic wind now shows blended historical data (fixed_2025 tech),
    # so the historical line is always visible.
    hide_hist <- FALSE
    
    if (!hide_hist && (is.null(df_region) || nrow(df_region) == 0)) {
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

    # Get variable metadata and choose the chart accent color
    var_meta <- climate_variables[[var_name]]
    accent_color <- tail(var_meta$palette, 1)
    if (accent_color %in% c("#f7fbff", "#ffeaa7")) {
      accent_color <- "#38bdf8" # Sky blue accent
    }

    # Gather projection data and baseline if projections are toggled ON
    show_proj <- isTRUE(input$show_projections == "1")
    proj_data <- NULL
    proj_models <- NULL
    baseline <- NULL
    if (show_proj) {
      proj_data_list <- filtered_projection_data()
      # Guard: filtered_projection_data() returns NULL on early-exit paths
      # (e.g., variable has no projection data). Only extract list components
      # when we actually got a valid list back.
      if (!is.null(proj_data_list)) {
        proj_data <- proj_data_list$ensemble
        proj_models <- proj_data_list$models
      }

      # ── Compute baseline inline from df_region (no extra query needed) ──────
      # The full historical record is already in df_region (all years, 1950-2023).
      # We just subset to the reference period and compute the mean. This
      # eliminates the old baseline_mean_value() reactive which used to fire
      # a separate Arrow query for the same data.
      if (!is.null(input$historical_period) && nchar(input$historical_period) > 0) {
        ref_years <- as.integer(strsplit(input$historical_period, "-")[[1]])
        ref_start <- ref_years[1]
        ref_end   <- ref_years[2]

        # Subset the already-loaded historical data to the reference period
        ref_values <- df_region$Value[
          df_region$Year >= ref_start & df_region$Year <= ref_end
        ]

        if (length(ref_values) > 0) {
          baseline <- mean(ref_values, na.rm = TRUE)
          message(sprintf("  Baseline for %s (%s, %s): %.2f over %d-%d (%d years)",
                          var_name, target_region, temp_mode, baseline,
                          ref_start, ref_end, length(ref_values)))
        }
      }
    }

    # Build the complete time-series chart using the helper function.
    # This handles anomaly transformation, title generation, Plotly traces,
    # projection overlay, annotation shapes, and dark-theme layout.
    build_region_timeseries_chart(
      df_region        = df_region,
      var_name         = var_name,
      var_meta         = var_meta,
      accent_color     = accent_color,
      region_name      = region$name,
      temp_mode        = temp_mode,
      show_proj        = show_proj,
      proj_data        = proj_data,
      proj_models      = proj_models,
      projection_style = if(is.null(input$projection_style)) "band" else input$projection_style,
      baseline         = baseline,
      display_mode     = input$display_mode,
      ssp_scenario     = input$ssp_scenario,
      reference_period = input$historical_period,
      hide_historical_line = hide_hist
    )

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

  # -------------------------------------------------------------------------
  # All Scenarios Trends Plotly Chart
  # -------------------------------------------------------------------------
  output$all_region_timeseries <- renderPlotly({
    req(input$drawer_tabs == "all_trends")
    region_data <- clicked_region()
    req(region_data)

    var_name <- input$climate_variable
    var_meta <- climate_variables[[var_name]]
    temp_mode <- input$temporal_mode
    target_region <- region_data[["zone_id"]]
    sp_level <- spatial_level_to_parquet[input$spatial_level]

    is_wind_power <- grepl("wind_power", var_name)
    wind_type <- if(var_name == "wind_power_onshore") "onshore" else "offshore"
    tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"

    df_region <- historical_trends_data()

    # Dynamic wind now shows blended historical data (fixed_2025 tech),
    # so the historical line is always visible.
    hide_hist <- FALSE

    all_proj <- all_scenarios_projection_data()
    proj_ensemble <- if (!is.null(all_proj)) all_proj$ensemble else NULL

    accent_color <- tail(var_meta$palette, 1)
    if (accent_color %in% c("#f7fbff", "#ffeaa7")) accent_color <- "#38bdf8"

    # ── Compute baseline inline from df_region ──────
    baseline <- NULL
    if (!is.null(input$historical_period) && nchar(input$historical_period) > 0 && !is.null(df_region)) {
      ref_years <- as.integer(strsplit(input$historical_period, "-")[[1]])
      ref_start <- ref_years[1]
      ref_end   <- ref_years[2]
      ref_values <- df_region$Value[df_region$Year >= ref_start & df_region$Year <= ref_end]
      if (length(ref_values) > 0) {
        baseline <- mean(ref_values, na.rm = TRUE)
      }
    }

    build_all_scenarios_timeseries_chart(
      df_region = df_region,
      var_name = var_name,
      var_meta = var_meta,
      accent_color = accent_color,
      region_name = region_data[["name"]],
      proj_ensemble = proj_ensemble,
      hide_historical_line = hide_hist,
      reference_period = input$historical_period,
      baseline = baseline,
      display_mode = input$display_mode
    )
  })

  observeEvent(input$climate_variable, {
    if (grepl("wind", input$climate_variable)) {
      showTab(inputId = "drawer_tabs", target = "tech")
    } else {
      hideTab(inputId = "drawer_tabs", target = "tech")
    }
  }, ignoreInit = FALSE)

  observeEvent(input$show_projections, {
    if (isTRUE(input$show_projections == "1")) {
      showTab(inputId = "drawer_tabs", target = "all_trends")
      showTab(inputId = "drawer_tabs", target = "all_seasonality")
    } else {
      hideTab(inputId = "drawer_tabs", target = "all_trends")
      hideTab(inputId = "drawer_tabs", target = "all_seasonality")
      
      if (!is.null(input$drawer_tabs) && input$drawer_tabs %in% c("all_trends", "all_seasonality")) {
        updateTabsetPanel(session, "drawer_tabs", selected = "trends")
      }
    }
  }, ignoreInit = FALSE)

  # -------------------------------------------------------------------------
  # Cross-Filtering: Chart Click -> Map Year
  # -------------------------------------------------------------------------
  observeEvent(event_data("plotly_click", source = "timeseries"), {
    # Do not update the map if the user is in "Period" view mode (multiannual means)
    if (isTRUE(input$projection_view_mode == "period")) return()
    
    click_data <- event_data("plotly_click", source = "timeseries")
    if (!is.null(click_data) && "x" %in% names(click_data)) {
      clicked_year <- as.integer(round(click_data$x[[1]]))
      
      # Use dynamic bounds from the current UI state instead of hardcoded 1950-2100
      c_min <- isolate(last_slider_min())
      c_max <- isolate(last_slider_max())
      
      # Fallback defaults if state isn't initialized
      if (is.null(c_min)) c_min <- 1950
      if (is.null(c_max)) c_max <- 2100
      
      # Only update the map slider if the clicked year is within the current allowed bounds
      if (clicked_year >= c_min && clicked_year <= c_max) {
        updateSliderInput(session, "selected_year", value = clicked_year)
      }
    }
  })

  observeEvent(event_data("plotly_click", source = "all_timeseries"), {
    if (isTRUE(input$projection_view_mode == "period")) return()
    
    click_data <- event_data("plotly_click", source = "all_timeseries")
    if (!is.null(click_data) && "x" %in% names(click_data)) {
      clicked_year <- as.integer(round(click_data$x[[1]]))
      
      c_min <- isolate(last_slider_min())
      c_max <- isolate(last_slider_max())
      
      if (is.null(c_min)) c_min <- 1950
      if (is.null(c_max)) c_max <- 2100
      
      if (clicked_year >= c_min && clicked_year <= c_max) {
        updateSliderInput(session, "selected_year", value = clicked_year)
      }
    }
  })

  # -------------------------------------------------------------------------
  # Seasonality Profile (Monthly) Plotly Chart
  # -------------------------------------------------------------------------
  output$region_seasonality <- renderPlotly({
    # Lazy loading
    req(input$drawer_tabs == "seasonality")
    region_data <- clicked_region()
    req(region_data)
    region_id <- region_data[["zone_id"]]
    region_name <- region_data[["name"]]
    
    var_name <- input$climate_variable
    var_meta <- climate_variables[[var_name]]
    ssp <- input$ssp_scenario
    # ref_period is set below depending on view_mode
    
    # Technology note label for the chart subtitle — communicates to the user
    # which turbine technology is being used for the seasonal shape.
    tech_note <- ""
    if (grepl("wind_power", var_name)) {
      tech_mix_mode <- if (!is.null(input$technology_mix)) input$technology_mix else "dynamic"
      if (tech_mix_mode == "dynamic") {
        tech_note <- "Historical uses Blended 2025 Tech"
      } else if (tech_mix_mode == "fixed_2020") {
        tech_note <- "Computed with 2020 Tech"
      } else if (tech_mix_mode == "fixed_2025") {
        tech_note <- "Computed with 2025 Tech"
      } else if (tech_mix_mode == "fixed_2030") {
        tech_note <- "Computed with 2030 Tech"
      } else if (tech_mix_mode == "fixed_2040") {
        tech_note <- "Computed with 2040 Tech"
      } else if (tech_mix_mode == "fixed_2050") {
        tech_note <- "Computed with 2050 Tech"
      }
    }

    view_mode <- if (!is.null(input$projection_view_mode)) input$projection_view_mode else "year"
    
    # User requested: if no period selected, the anualcycle plot must use the most recent period 1991-2020
    ref_period <- if (!is.null(input$historical_period)) input$historical_period else "1991-2020"
    
    if (view_mode == "period") {
      req(input$projection_period)
      target_period <- input$projection_period
      period_years <- as.integer(strsplit(target_period, "-")[[1]])
      proj_start <- period_years[1]
      proj_end   <- period_years[2]
    } else {
      map_year <- if (is.null(input$selected_year)) 2040 else as.numeric(input$selected_year)
      if (map_year <= 2020) {
        proj_start <- 2021
        proj_end   <- 2040
      } else {
        proj_start <- max(2021, map_year - 10)
        proj_end   <- min(2100, map_year + 10)
      }
      target_period <- paste0(proj_start, "-", proj_end)
    }
    
    sp_level_pq <- spatial_level_to_parquet[input$spatial_level]
    
    df_hist <- historical_seasonality_data()
      
    # Query Projected Monthly
    show_proj <- isTRUE(input$show_projections == "1")
    df_proj <- NULL
    if (show_proj) {
      df_proj <- projection_seasonality_data()
    }

    build_seasonality_plotly(
      df_hist = df_hist,
      df_proj = df_proj,
      var_name = var_name,
      region_name = region_name,
      ssp_scenario = ssp,
      reference_period = ref_period,
      target_period = target_period,
      accent_color = tail(var_meta$palette, 1),
      hist_note = tech_note
    )
  })

  # -------------------------------------------------------------------------
  # All Scenarios Seasonality (Monthly) Plotly Chart
  # -------------------------------------------------------------------------
  output$all_region_seasonality <- renderPlotly({
    req(input$drawer_tabs == "all_seasonality")
    region_data <- clicked_region()
    req(region_data)
    region_id <- region_data[["zone_id"]]
    region_name <- region_data[["name"]]
    
    var_name <- input$climate_variable
    var_meta <- climate_variables[[var_name]]
    
    view_mode <- if (!is.null(input$projection_view_mode)) input$projection_view_mode else "year"
    ref_period <- if (!is.null(input$historical_period)) input$historical_period else "1991-2020"
    
    if (view_mode == "period") {
      req(input$projection_period)
      target_period <- input$projection_period
      period_years <- as.integer(strsplit(target_period, "-")[[1]])
      proj_start <- period_years[1]
      proj_end   <- period_years[2]
    } else {
      map_year <- if (is.null(input$selected_year)) 2040 else as.numeric(input$selected_year)
      if (map_year <= 2020) {
        proj_start <- 2021
        proj_end   <- 2040
      } else {
        proj_start <- max(2021, map_year - 10)
        proj_end   <- min(2100, map_year + 10)
      }
      target_period <- paste0(proj_start, "-", proj_end)
    }
    
    sp_level_pq <- spatial_level_to_parquet[input$spatial_level]
    
    df_hist <- historical_seasonality_data()
      
    show_proj <- isTRUE(input$show_projections == "1")
    df_proj <- NULL
    if (show_proj) {
      df_proj <- all_scenarios_projection_seasonality_data()
    }

    build_all_scenarios_seasonality_chart(
      df_hist = df_hist,
      df_proj = df_proj,
      var_name = var_name,
      var_meta = var_meta,
      accent_color = tail(var_meta$palette, 1),
      region_name = region_name,
      reference_period = ref_period,
      target_period = target_period
    )
  })


  # ----------------------------------------------------------------------------
  # CSV Download Handler — export chart data for the selected region
  # ----------------------------------------------------------------------------
  # Downloads the historical time series (and projections when ON) for the
  # currently clicked region as a CSV file. The exported file contains columns:
  #   Year, Value, Source (ERA5/Projection), Variable, Region, Season
  # Scientists can use this for their own analyses or for publication figures.
  # ----------------------------------------------------------------------------
  output$download_chart_csv <- downloadHandler(

    # Dynamic filename based on selected region and variable
    filename = function() {
      region <- clicked_region()
      var_name <- input$climate_variable
      temp_mode <- input$temporal_mode
      active_tab <- input$drawer_tabs
      
      region_id <- if (!is.null(region)) region[["zone_id"]] else "unknown"
      
      tab_name <- "timeseries"
      if (isTRUE(grepl("^all_seasonality", active_tab))) {
        tab_name <- "all_scenarios_seasonality"
      } else if (isTRUE(grepl("^all_trends", active_tab))) {
        tab_name <- "all_scenarios_timeseries"
      } else if (isTRUE(grepl("seasonality", active_tab))) {
        tab_name <- "seasonality"
      }
      
      paste0("powervision_", region_id, "_", var_name, "_", temp_mode, "_", tab_name, ".csv")
    },

    content = function(file) {
      region <- clicked_region()
      req(region, input$climate_variable, input$temporal_mode, input$spatial_level)

      df_hist <- NULL
      active_tab <- input$drawer_tabs
      show_proj <- isTRUE(input$show_projections == "1")

      df_hist <- NULL
      df_proj <- NULL

      if (grepl("seasonality", active_tab)) {
        df_hist <- historical_seasonality_data()
        if (show_proj) {
          if (grepl("^all_", active_tab)) {
            df_proj <- all_scenarios_projection_seasonality_data()
          } else {
            df_proj <- projection_seasonality_data()
          }
        }
      } else {
        df_hist <- historical_trends_data()
        if (show_proj) {
          if (grepl("^all_", active_tab)) {
            proj_list <- all_scenarios_projection_data()
            if (!is.null(proj_list)) {
              df_proj <- proj_list$ensemble
              if ("median_val" %in% names(df_proj)) df_proj$Value <- df_proj$median_val
            }
          } else {
            proj_list <- filtered_projection_data()
            if (!is.null(proj_list)) {
              df_proj <- proj_list$ensemble
              if ("median_val" %in% names(df_proj)) df_proj$Value <- df_proj$median_val
            }
          }
        }
      }

      df_combined <- generate_wysiwyg_export_csv(
        df_hist = df_hist,
        df_proj = df_proj,
        active_tab = active_tab,
        display_mode = input$display_mode,
        historical_period = input$historical_period,
        projection_period = input$projection_period,
        var_name = input$climate_variable,
        region_id = region[["zone_id"]]
      )

      write.csv(df_combined, file, row.names = FALSE)
    }
  )
}
