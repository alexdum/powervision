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
  # Returns a simple data.frame with columns: Region, Value
  # ----------------------------------------------------------------------------
  filtered_climate_data <- reactive({
    req(input$climate_variable, input$temporal_mode, input$selected_year, input$spatial_level)
    
    var_name <- input$climate_variable
    temp_mode <- input$temporal_mode
    sel_year <- as.integer(input$selected_year)
    sp_level <- spatial_level_to_parquet[input$spatial_level]
    
    # Select the appropriate lazy Arrow dataset (annual vs seasonal)
    if (temp_mode == "Annual") {
      ds <- hist_annual_ds
    } else {
      ds <- hist_seasonal_ds
    }
    
    if (is.null(ds)) {
      return(NULL)
    }
    
    # Build a lazy query with partition pruning on 'variable',
    # then filter in-file columns SpatialLevel and Year.
    query <- ds |>
      dplyr::filter(variable == var_name,
                    SpatialLevel == sp_level,
                    Year == sel_year)
    
    # Filter by season if we are in seasonal mode
    if (temp_mode != "Annual") {
      query <- query |> dplyr::filter(Season == temp_mode)
    }
    
    # collect() materializes only the matching rows into a data.frame
    dplyr::collect(query)
  })

  # ----------------------------------------------------------------------------
  # Dynamic Year Slider Bounds based on Onshore/Offshore Spatial Tier
  # ----------------------------------------------------------------------------
  # Onshore PECD spatial tiers (NUT0, NUT2, PEON, SZON) only contain historical
  # data up to 2021. Offshore tiers (PEOF, SZOF) go up to 2023.
  # This observer adjusts the year slider's bounds dynamically, protecting the
  # user from selecting blank periods and ensuring polygons are colored at startup.
  # ----------------------------------------------------------------------------
  observe({
    req(input$spatial_level, input$temporal_mode)
    
    # Determine the maximum year based on Onshore/Offshore tier
    is_offshore <- input$spatial_level %in% c("PEOF", "SZOF")
    max_year <- if (is_offshore) 2023 else 2021
    
    # Winter season has incomplete 1950 data, so min year is 1951 for seasonal mode
    min_year <- if (input$temporal_mode == "Annual") 1950 else 1951
    
    # Adjust current year selection if it lies outside the valid range
    current_yr <- input$selected_year
    if (is.null(current_yr)) {
      current_yr <- max_year
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
  # Central Zone Layer Renderer
  # ----------------------------------------------------------------------------
  # This single observe() block is responsible for ALL polygon drawing.
  # It fires when: (a) the spatial level changes, (b) the map first loads,
  # or (c) style_trigger is incremented after a basemap switch.
  # Keeping the draw logic here (rather than duplicating it in the basemap
  # observer) prevents the accumulation/overlap problem.
  # ----------------------------------------------------------------------------
  observe({
    req(map_loaded())
    style_trigger()                   # take the dependency so we re-fire on style change

    geom_data <- current_boundaries()
    req(geom_data)

    # Reactive climate data filter dependency
    clim_data <- filtered_climate_data()

    message(sprintf("Rendering %d polygons to MapLibre...", nrow(geom_data)))

    # Get the selected variable metadata
    var_meta <- climate_variables[[input$climate_variable]]
    palette <- var_meta$palette

    # Left join the climate data onto the boundary geometries
    if (!is.null(clim_data) && nrow(clim_data) > 0) {
      joined_geom <- geom_data %>%
        left_join(clim_data, by = c("zone_id" = "Region"))
    } else {
      joined_geom <- geom_data
      joined_geom$Value <- NA_real_
    }

    # Build clean HTML tooltips for hover popups
    var_label <- var_meta$label
    var_unit <- var_meta$unit
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
          "  </div>",
          "</div>"
        )
      )

    # Calculate min and max values for interpolation stops, ignoring NAs
    vals <- joined_geom$Value
    vals <- vals[is.finite(vals)]

    if (length(vals) > 0) {
      min_val <- min(vals)
      max_val <- max(vals)
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
  # ----------------------------------------------------------------------------
  output$choropleth_legend <- renderUI({
    # Take dependencies on controls
    req(input$climate_variable, input$temporal_mode, input$selected_year)
    
    var_meta <- climate_variables[[input$climate_variable]]
    palette <- var_meta$palette
    
    # Get the current data range
    clim_data <- filtered_climate_data()
    if (is.null(clim_data) || nrow(clim_data) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }
    
    vals <- clim_data$Value
    vals <- vals[is.finite(vals)]
    
    if (length(vals) == 0) {
      return(div(class = "legend-no-data", "No data available for legend"))
    }
    
    min_val <- min(vals)
    max_val <- max(vals)
    
    # Construct CSS linear gradient from the palette colors
    gradient_css <- paste0("linear-gradient(to right, ", paste(palette, collapse = ", "), ")")
    
    # Format min/max labels nicely
    label_min <- sprintf("%s %s", format(round(min_val, 1), big.mark = ","), var_meta$unit)
    label_max <- sprintf("%s %s", format(round(max_val, 1), big.mark = ","), var_meta$unit)
    
    div(
      class = "choropleth-legend-container",
      div(
        class = "legend-title",
        style = "font-weight: 600; font-size: 0.8rem; color: #e2e8f0; margin-bottom: 6px; font-family: Inter, sans-serif;",
        sprintf("%s (%s %s)", var_meta$label, input$temporal_mode, input$selected_year)
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
  # Time-Series Plotly Chart Renderer
  # ----------------------------------------------------------------------------
  # When a region polygon is clicked, this block loads the full historical record
  # (1950-2023) for that specific region and selected climate variable, and plots
  # it as an interactive time series line chart using Plotly.
  # ----------------------------------------------------------------------------
  output$region_timeseries <- renderPlotly({
    # Ensure a region is clicked and variable/mode are set
    region <- clicked_region()
    req(region, input$climate_variable, input$temporal_mode)
    
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
    
    # Build interactive Plotly chart with dark glassmorphism styling
    plot_ly(
      data = df_region,
      x = ~Year,
      y = ~Value,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = accent_color, width = 2),
      marker = list(color = accent_color, size = 5),
      text = ~paste0("Year: ", Year, "<br>", var_label, ": ", round(Value, 2), " ", var_unit),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = sprintf("Historical Record: %s (%s)", region$name, temp_mode),
          font = list(family = "Inter, sans-serif", size = 14, color = "#e2e8f0"),
          x = 0.05
        ),
        paper_bgcolor = "rgba(0,0,0,0)", # Fully transparent to blend with glassmorphism drawer
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 50, r = 20, b = 40, l = 50),
        xaxis = list(
          title = list(text = "Year", font = list(family = "Inter, sans-serif", color = "#94a3b8")),
          tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
          gridcolor = "rgba(255, 255, 255, 0.05)",
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(text = sprintf("%s (%s)", var_label, var_unit), font = list(family = "Inter, sans-serif", color = "#94a3b8")),
          tickfont = list(family = "Inter, sans-serif", color = "#94a3b8"),
          gridcolor = "rgba(255, 255, 255, 0.05)",
          zeroline = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE) # Clean interface without cluttering toolbars
  })
}
