// app.js
// ==============================================================================
// Copernicus PECD v4.2 — PowerClimate Vision Explorer
// Custom JavaScript — UI interactions and Shiny message handlers
// Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
// ==============================================================================

$(document).ready(function () {

  // --------------------------------------------------------------------------
  // PMTiles Protocol Registration
  // --------------------------------------------------------------------------
  // Register the pmtiles:// protocol so MapLibre can read .pmtiles files
  // directly as vector tile sources. This must run before any map loads.
  // --------------------------------------------------------------------------
  if (typeof pmtiles !== 'undefined') {
    var protocol = new pmtiles.Protocol();
    maplibregl.addProtocol('pmtiles', protocol.tile);
  }

  // --------------------------------------------------------------------------
  // PMTiles Source Swap Handler
  // --------------------------------------------------------------------------
  // R sends: session$sendCustomMessage("swap_tile_source", list(url = "...", ...))
  // This handler:
  //   1. Removes old zone layers (fills, borders, highlight)
  //   2. Removes the old tile source
  //   3. Adds a new vector tile source pointing to the new .pmtiles URL
  //   4. Adds fresh fill and border layers from the new source
  //
  // The Data/Color Observer then paints the correct colors via set_paint_property.
  // --------------------------------------------------------------------------
  // --------------------------------------------------------------------------
  // Shared Helper — Find the MapLibre map instance
  // --------------------------------------------------------------------------
  // mapgl stores the map instance on the widget's DOM element. We check
  // several known patterns to locate it reliably.
  // --------------------------------------------------------------------------
  function _getMapInstance() {
    var mapEl = document.getElementById('map');
    if (!mapEl) return null;

    // mapgl stores the instance as .map on the widget element
    if (mapEl.map) return mapEl.map;

    // Fallback: via HTMLWidgets binding
    if (typeof HTMLWidgets !== 'undefined') {
      var widget = HTMLWidgets.find('#map');
      if (widget && widget.getMap) return widget.getMap();
    }

    return null;
  }

  // --------------------------------------------------------------------------
  // GeoJSON Source Swap Handler
  // --------------------------------------------------------------------------
  // R sends: session$sendCustomMessage("swap_tile_source", list(url = "...", ...))
  // Instead of PMTiles (which needs HTTP Range Requests that Shiny doesn't
  // support), we load the GeoJSON files directly as static URLs. MapLibre
  // fetches the file from the Shiny static server — no R serialization needed.
  // --------------------------------------------------------------------------
  Shiny.addCustomMessageHandler('swap_tile_source', function (msg) {
    var map = _getMapInstance();
    if (!map) {
      console.warn('[GeoJSON] Could not find MapLibre map instance');
      return;
    }

    var sourceId     = 'zone-tiles';
    var fillLayerId  = 'zone-fills';
    var borderLayerId = 'zone-borders';
    var highlightLayerId = 'zone-highlight';
    var geojsonUrl   = msg.url;
    var borderColor  = msg.border_color || '#ffffff';
    var borderWidth  = msg.border_width || 0.5;
    var borderOpacity= msg.border_opacity || 0.8;

    // Step 1: Remove old layers (if they exist)
    if (map.getLayer(highlightLayerId)) map.removeLayer(highlightLayerId);
    if (map.getLayer(borderLayerId))    map.removeLayer(borderLayerId);
    if (map.getLayer(fillLayerId))      map.removeLayer(fillLayerId);

    // Step 2: Remove old source
    if (map.getSource(sourceId)) map.removeSource(sourceId);

    // Step 3: Add GeoJSON source directly from URL.
    // MapLibre fetches the file via HTTP — no websocket, no R serialization.
    map.addSource(sourceId, {
      type: 'geojson',
      data: geojsonUrl,
      promoteId: 'zone_id'    // use zone_id as feature ID for queryRenderedFeatures
    });

    // Step 4: Find the first symbol layer to insert our polygons underneath.
    // This ensures that city names, borders, and labels always render ON TOP
    // of the choropleth data.
    var layers = map.getStyle().layers;
    var firstSymbolId = null;
    for (var i = 0; i < layers.length; i++) {
      if (layers[i].type === 'symbol') {
        firstSymbolId = layers[i].id;
        break;
      }
    }

    // Step 5: Add fill layer — starts fully transparent
    map.addLayer({
      id: fillLayerId,
      type: 'fill',
      source: sourceId,
      paint: {
        'fill-color': '#00000000',
        'fill-opacity': 0,
        'fill-outline-color': '#ffffff00'
      }
    }, firstSymbolId);

    // Step 6: Add border layer
    map.addLayer({
      id: borderLayerId,
      type: 'line',
      source: sourceId,
      paint: {
        'line-color': borderColor,
        'line-width': borderWidth,
        'line-opacity': borderOpacity
      }
    }, firstSymbolId);

    console.log('[GeoJSON] Swapped source to:', geojsonUrl);
  });

  // --------------------------------------------------------------------------
  // Paint Zone Fills Handler
  // --------------------------------------------------------------------------
  // R sends: session$sendCustomMessage("paint_zone_fills", list(
  //   fill_expr = <match_expr result>,
  //   opacity   = 0.65
  // ))
  // This applies the fill-color expression and fill-opacity directly via
  // MapLibre's native setPaintProperty — bypassing mapgl's set_paint_property
  // which only works on layers it created itself.
  // --------------------------------------------------------------------------
  Shiny.addCustomMessageHandler('paint_zone_fills', function (msg) {
    var map = _getMapInstance();
    if (!map) {
      console.warn('[Paint] Map instance not found');
      return;
    }

    if (!map.getLayer('zone-fills')) {
      console.warn('[Paint] zone-fills layer not found, skipping paint');
      return;
    }

    // Parse the fill-color expression from the JSON string sent by R.
    // We use a JSON string to avoid Shiny's automatic serialization converting
    // R unnamed lists into JSON objects instead of arrays.
    if (msg.fill_expr_json) {
      try {
        var fillExpr = JSON.parse(msg.fill_expr_json);
        map.setPaintProperty('zone-fills', 'fill-color', fillExpr);
      } catch (e) {
        console.error('[Paint] Failed to parse fill expression:', e);
      }
    }

    // Apply the fill-opacity
    if (typeof msg.opacity === 'number') {
      map.setPaintProperty('zone-fills', 'fill-opacity', msg.opacity);
    }

    console.log('[Paint] Applied fill colors and opacity:', msg.opacity);
  });


  // --------------------------------------------------------------------------
  // Floating Layer Control — hover-expand / auto-collapse
  // --------------------------------------------------------------------------
  var $layerControl = $('.map-layer-control');

  $layerControl.on('mouseenter', function () {
    $(this).addClass('expanded');
  });

  $layerControl.on('mouseleave', function () {
    $(this).removeClass('expanded');
  });

  // Auto-collapse 400ms after a basemap radio is chosen
  $layerControl.on('change', 'input[type="radio"]', function () {
    setTimeout(function () {
      $layerControl.removeClass('expanded');
    }, 400);
  });

  // --------------------------------------------------------------------------
  // Stats Drawer — Shiny custom message handler
  // --------------------------------------------------------------------------
  // R calls: session$sendCustomMessage("toggle_stats_drawer", list(show = TRUE/FALSE))
  // This adds/removes the .is-visible class which drives the CSS translateY transition.
  // --------------------------------------------------------------------------
  Shiny.addCustomMessageHandler('toggle_stats_drawer', function (msg) {
    var $drawer = $('#stats-drawer');
    if (msg.show) {
      $drawer.addClass('is-visible');
    } else {
      $drawer.removeClass('is-visible');
    }
  });

  // Clicking the ✕ close button hides the drawer AND notifies Shiny so it can
  // reset clicked_region() — the server observer handles the reactive clean-up.
  $(document).on('click', '#drawer-close-btn', function () {
    $('#stats-drawer').removeClass('is-visible');
    Shiny.setInputValue('drawer_closed', Math.random()); // random ensures reactivity fires every time
  });

  // --------------------------------------------------------------------------
  // Map Loading Shimmer — Shiny custom message handlers
  // --------------------------------------------------------------------------
  // R calls: session$sendCustomMessage("map_loading_shimmer", list(show = TRUE))
  // Shows a subtle pulsing overlay on the map canvas while the choropleth is
  // re-rendering. The overlay is purely visual — pointer-events: none in CSS
  // ensures users can still interact with the map during the shimmer.
  //
  // When hiding, we add a brief 400ms delay to let MapLibre proxy commands
  // finish rendering on the GPU before removing the shimmer.
  // --------------------------------------------------------------------------
  var shimmerHideTimer = null;

  Shiny.addCustomMessageHandler('map_loading_shimmer', function (msg) {
    var $shimmer = $('#map-loading-shimmer');
    if (msg.show) {
      // Cancel any pending hide — a new render cycle started
      if (shimmerHideTimer) { clearTimeout(shimmerHideTimer); shimmerHideTimer = null; }
      $shimmer.addClass('is-active');
    } else {
      // Delay removal so the map has time to finish painting after proxy commands
      shimmerHideTimer = setTimeout(function () {
        $shimmer.removeClass('is-active');
        shimmerHideTimer = null;
      }, 400);
    }
  });

  // --------------------------------------------------------------------------
  // Custom Tooltip Engine — zone_id → tooltip HTML
  // --------------------------------------------------------------------------
  // The server sends a named list { zone_id: tooltip_html } via the
  // 'update_zone_tooltips' custom message. We store this in a global object
  // and use native MapLibre mousemove/mouseleave events to look up and display
  // the appropriate tooltip HTML in a Popup.
  //
  // This replaces mapgl's built-in tooltip which required the tooltip HTML to
  // be baked into the GeoJSON feature properties. Since we now decouple
  // geometry from data, the GeoJSON has no data columns — only zone_id.
  // --------------------------------------------------------------------------
  var _tooltipMap = {};       // { zone_id: "<div>...</div>" }
  var _tooltipPopup = null;   // Reusable maplibregl.Popup instance
  var _hoveredZoneId = null;  // Track last hovered zone to avoid redundant updates

  // Receive tooltip data from R
  Shiny.addCustomMessageHandler('update_zone_tooltips', function (msg) {
    _tooltipMap = msg;
  });

  // Attach native MapLibre listeners once the map instance is available
  function attachCustomTooltip() {
    var mapEl = document.getElementById('map');
    if (!mapEl || !mapEl.map) {
      // Map not ready yet — retry shortly
      setTimeout(attachCustomTooltip, 300);
      return;
    }
    var map = mapEl.map;

    // Create a single reusable popup (no close button, follows cursor)
    _tooltipPopup = new maplibregl.Popup({
      closeButton: false,
      closeOnClick: false,
      className: 'custom-zone-tooltip',
      maxWidth: '320px'
    });

    // On mousemove over zone-fills, show the tooltip for that zone
    map.on('mousemove', 'zone-fills', function (e) {
      if (!e.features || e.features.length === 0) return;

      var zoneId = e.features[0].properties.zone_id;
      if (!zoneId) return;

      // Skip redundant updates if still hovering the same zone
      if (zoneId === _hoveredZoneId) {
        // Just update position
        _tooltipPopup.setLngLat(e.lngLat);
        return;
      }
      _hoveredZoneId = zoneId;

      var html = _tooltipMap[zoneId];
      if (html) {
        _tooltipPopup.setLngLat(e.lngLat).setHTML(html).addTo(map);
      } else {
        _tooltipPopup.remove();
      }

      // Change cursor to pointer
      map.getCanvas().style.cursor = 'pointer';
    });

    // When the mouse leaves the zone-fills layer, hide the tooltip
    map.on('mouseleave', 'zone-fills', function () {
      _hoveredZoneId = null;
      _tooltipPopup.remove();
      map.getCanvas().style.cursor = '';
    });
  }

  // Start checking for the map instance
  attachCustomTooltip();

  // --------------------------------------------------------------------------
  // Projection Toggle — pill switch click handler
  // --------------------------------------------------------------------------
  // When a toggle-option button is clicked, we:
  //   1. Move the sliding pill by toggling .toggle-right on the container
  //   2. Swap the .active class to the clicked option
  //   3. Update the hidden Shiny input so the server observer fires
  // --------------------------------------------------------------------------
  $(document).on('click', '.projection-toggle .toggle-option', function () {
    var $btn       = $(this);
    var $container = $btn.closest('.projection-toggle');
    var newValue   = $btn.data('value');

    // Skip if this option is already active
    if ($btn.hasClass('active')) return;

    // Swap active class
    $container.find('.toggle-option').removeClass('active');
    $btn.addClass('active');

    // Slide the pill: second option = toggle-right, first = default (left)
    if (newValue === 'mercator') {
      $container.addClass('toggle-right');
    } else {
      $container.removeClass('toggle-right');
    }

    // Push the value into Shiny's input binding
    Shiny.setInputValue('map_projection', newValue);
  });

  // --------------------------------------------------------------------------
  // Map Projection — style.load auto-correction
  // --------------------------------------------------------------------------
  // Problem: the mapgl R package always re-applies the INITIAL projection
  // ("globe") on every style.load event. When the user has selected "flat"
  // (mercator) and then switches basemaps, the map briefly flashes as a globe
  // before our delayed R callback can correct it.
  //
  // Solution: hook into the MapLibre map's own style.load event and
  // immediately re-apply whatever the toggle currently shows. This fires
  // synchronously right after the mapgl handler, so no flash is visible.
  // --------------------------------------------------------------------------
  function attachProjectionGuard() {
    var mapEl = document.getElementById('map');
    if (!mapEl || !mapEl.map) {
      // Map not ready yet — retry shortly
      setTimeout(attachProjectionGuard, 300);
      return;
    }

    mapEl.map.on('style.load', function () {
      // Read the current toggle value from the active button
      var $active = $('.projection-toggle .toggle-option.active');
      var currentProj = $active.length ? $active.data('value') : 'globe';
      mapEl.map.setProjection({ type: currentProj });
    });
  }
  attachProjectionGuard();

  // Fallback: direct Shiny custom message handler for server-initiated changes
  Shiny.addCustomMessageHandler('set_map_projection', function (msg) {
    var mapEl = document.getElementById('map');
    if (mapEl && mapEl.map) {
      mapEl.map.setProjection({ type: msg.projection });
    }
  });

  // --------------------------------------------------------------------------
  // MapLibre canvas resize on window resize
  // --------------------------------------------------------------------------
  $(window).on('resize', function () {
    var mapEl = document.getElementById('map');
    if (mapEl && mapEl.map) {
      mapEl.map.resize();
    }
  });

  // --------------------------------------------------------------------------
  // About Modal — open / close handlers
  // --------------------------------------------------------------------------
  // The About button (#about_btn) toggles the .is-visible class on the overlay.
  // Dismissal happens via: close button, backdrop click, or Escape key.
  // --------------------------------------------------------------------------

  // Open the About modal when the info button is clicked
  $(document).on('click', '#about_btn', function () {
    $('#about-overlay').addClass('is-visible');
  });

  // Close when the ✕ button is clicked
  $(document).on('click', '#about-close-btn', function () {
    $('#about-overlay').removeClass('is-visible');
  });

  // Close when clicking the dark backdrop (outside the modal card)
  $(document).on('click', '#about-backdrop', function () {
    $('#about-overlay').removeClass('is-visible');
  });

  // Close on Escape key press
  $(document).on('keydown', function (event) {
    if (event.key === 'Escape' && $('#about-overlay').hasClass('is-visible')) {
      $('#about-overlay').removeClass('is-visible');
    }
  });
  // --------------------------------------------------------------------------
  // Projection Toggle — Show/Hide projections on time-series chart
  // --------------------------------------------------------------------------
  // When toggled ON ("1"), the scenario selector, display mode toggle, and
  // (conditionally) the reference period selector fade in.
  // When toggled OFF ("0"), all projection sub-controls hide.
  // --------------------------------------------------------------------------
  $(document).on('click', '.projection-show-toggle .proj-toggle-option', function () {
    var $btn       = $(this);
    var $container = $btn.closest('.projection-show-toggle');
    var newValue   = $btn.data('value');

    // Skip if this option is already active
    if ($btn.hasClass('active')) return;

    // Swap active class
    $container.find('.proj-toggle-option').removeClass('active');
    $btn.addClass('active');

    // Slide the pill: "1" (Projections) = toggle-right, "0" (Off) = default left
    if (newValue === '1' || newValue === 1) {
      $container.addClass('toggle-right');
      $('#scenario-selector-wrapper').addClass('is-visible');
      $('#display-mode-wrapper').addClass('is-visible');
      // Only show reference period if display mode is "anomaly"
      var currentMode = $('#display-mode-toggle .display-toggle-option.active').data('value');
      if (currentMode === 'anomaly') {
        $('#reference-period-wrapper').addClass('is-visible');
      }
      // Only show projection period if view mode is "period"
      var currentView = $('#view-mode-toggle .view-toggle-option.active').data('value');
      if (currentView === 'period') {
        $('#projection-period-wrapper').addClass('is-visible');
      }

      // ── Update the period dropdown directly via selectize API ──────────────
      // Previously this was done server-side via updateSelectInput(), which
      // caused an async round-trip (R → browser → R) and triggered a second
      // render cycle. By updating the dropdown client-side, all input changes
      // (show_projections + projection_period) arrive at the server in a
      // single Shiny message batch → single render.
      var $periodSelect = $('#projection_period');
      if ($periodSelect.length && $periodSelect[0].selectize) {
        var selectize = $periodSelect[0].selectize;
        selectize.clear(true);     // clear selection silently
        selectize.clearOptions();  // remove all existing options
        selectize.addOption([
          {value: '1961-1990', label: '1961\u20131990 (WMO Classic)'},
          {value: '1971-2000', label: '1971\u20132000 (WMO Historical)'},
          {value: '1981-2010', label: '1981\u20132010 (WMO Previous)'},
          {value: '1991-2020', label: '1991\u20132020 (WMO Current)'},
          {value: '2011-2023', label: '2011\u20132023 (Recent)'},
          {value: '2021-2040', label: '2021\u20132040 (Near-term)'},
          {value: '2041-2060', label: '2041\u20132060 (Mid-term)'},
          {value: '2061-2080', label: '2061\u20132080 (Mid-late)'},
          {value: '2081-2100', label: '2081\u20132100 (Long-term)'}
        ]);
        selectize.setValue('2041-2060', true); // select silently (no change event)
        // Push the new period value to Shiny in the same message batch
        Shiny.setInputValue('projection_period', '2041-2060');
      }
    } else {
      $container.removeClass('toggle-right');
      $('#scenario-selector-wrapper').removeClass('is-visible');
      $('#display-mode-wrapper').removeClass('is-visible');
      $('#reference-period-wrapper').removeClass('is-visible');

      // ── Restore historical-only periods via selectize API ──────────────────
      var $periodSelect = $('#projection_period');
      if ($periodSelect.length && $periodSelect[0].selectize) {
        var selectize = $periodSelect[0].selectize;
        selectize.clear(true);
        selectize.clearOptions();
        selectize.addOption([
          {value: '1961-1990', label: '1961\u20131990 (WMO Classic)'},
          {value: '1971-2000', label: '1971\u20132000 (WMO Historical)'},
          {value: '1981-2010', label: '1981\u20132010 (WMO Previous)'},
          {value: '1991-2020', label: '1991\u20132020 (WMO Current)'},
          {value: '2011-2023', label: '2011\u20132023 (Recent)'}
        ]);
        selectize.setValue('1981-2010', true);
        Shiny.setInputValue('projection_period', '1981-2010');
      }
    }

    // Push the value into Shiny's input binding
    Shiny.setInputValue('show_projections', String(newValue));
  });

  // --------------------------------------------------------------------------
  // Display Mode Toggle — Absolute vs Anomaly
  // --------------------------------------------------------------------------
  // Switches between absolute values and anomaly (departure from baseline)
  // for both the map choropleth and the time-series chart.
  // When "absolute" is selected, the reference period dropdown hides.
  // When "anomaly" is selected, the reference period dropdown appears.
  // --------------------------------------------------------------------------
  $(document).on('click', '.display-mode-toggle .display-toggle-option', function () {
    var $btn       = $(this);
    var $container = $btn.closest('.display-mode-toggle');
    var newValue   = $btn.data('value');

    // Skip if this option is already active
    if ($btn.hasClass('active')) return;

    // Swap active class
    $container.find('.display-toggle-option').removeClass('active');
    $btn.addClass('active');

    // Slide the pill: "anomaly" = toggle-right, "absolute" = default left
    if (newValue === 'anomaly') {
      $container.addClass('toggle-right');
      $('#reference-period-wrapper').addClass('is-visible');
    } else {
      $container.removeClass('toggle-right');
      $('#reference-period-wrapper').removeClass('is-visible');
    }

    // Push the value into Shiny's input binding
    Shiny.setInputValue('display_mode', newValue);
  });

  // --------------------------------------------------------------------------
  // View Mode Toggle — Year vs Period
  // --------------------------------------------------------------------------
  // Switches between single-year view (year slider) and period-averaged view
  // (IPCC 20-year windows). When "period" is selected, the projection period
  // dropdown appears. When "year" is selected, it hides.
  // --------------------------------------------------------------------------
  $(document).on('click', '.view-mode-toggle .view-toggle-option', function () {
    var $btn       = $(this);
    var $container = $btn.closest('.view-mode-toggle');
    var newValue   = $btn.data('value');

    // Skip if this option is already active
    if ($btn.hasClass('active')) return;

    // Swap active class
    $container.find('.view-toggle-option').removeClass('active');
    $btn.addClass('active');

    // Slide the pill: "period" = toggle-right, "year" = default left
    if (newValue === 'period') {
      $container.addClass('toggle-right');
      $('#projection-period-wrapper').addClass('is-visible');
      // Hide the year slider — it's replaced by the period dropdown
      $('#selected_year').closest('.form-group').slideUp(200);
    } else {
      $container.removeClass('toggle-right');
      $('#projection-period-wrapper').removeClass('is-visible');
      // Show the year slider again
      $('#selected_year').closest('.form-group').slideDown(200);
    }

    // Push the value into Shiny's input binding
    Shiny.setInputValue('projection_view_mode', newValue);
  });

  // --------------------------------------------------------------------------
  // Projection Controls — Enable / Disable from server
  // --------------------------------------------------------------------------
  // The server sends this message when the variable or spatial level changes.
  // If projection data is unavailable for the current combination, we hide
  // the entire projection controls group and reset all toggles.
  // --------------------------------------------------------------------------
  Shiny.addCustomMessageHandler('toggle_projection_controls', function (msg) {
    var $controls = $('#projection-controls');
    if (msg.available) {
      $controls.removeClass('is-disabled');
    } else {
      // Hide and reset to off
      $controls.addClass('is-disabled');
      // Reset the projection toggle to Off
      var $projToggle = $('#projection-show-toggle');
      $projToggle.find('.proj-toggle-option').removeClass('active');
      $projToggle.find('.proj-toggle-option[data-value="0"]').addClass('active');
      $projToggle.removeClass('toggle-right');
      // Hide all sub-controls
      $('#scenario-selector-wrapper').removeClass('is-visible');
      $('#display-mode-wrapper').removeClass('is-visible');
      $('#reference-period-wrapper').removeClass('is-visible');
      // Reset display mode to absolute (default)
      var $modeToggle = $('#display-mode-toggle');
      $modeToggle.find('.display-toggle-option').removeClass('active');
      $modeToggle.find('.display-toggle-option[data-value="absolute"]').addClass('active');
      $modeToggle.removeClass('toggle-right');
      // Reset view mode to year (default)
      var $viewToggle = $('#view-mode-toggle');
      $viewToggle.find('.view-toggle-option').removeClass('active');
      $viewToggle.find('.view-toggle-option[data-value="year"]').addClass('active');
      $viewToggle.removeClass('toggle-right');
      // Push reset values to Shiny
      Shiny.setInputValue('show_projections', '0');
      Shiny.setInputValue('display_mode', 'absolute');
      Shiny.setInputValue('projection_view_mode', 'year');
      // Restore year slider if it was hidden by period mode
      $('#selected_year').closest('.form-group').slideDown(200);

      // Restore historical-only periods via selectize API
      var $periodSelect = $('#projection_period');
      if ($periodSelect.length && $periodSelect[0].selectize) {
        var selectize = $periodSelect[0].selectize;
        selectize.clear(true);
        selectize.clearOptions();
        selectize.addOption([
          {value: '1961-1990', label: '1961\u20131990 (WMO Classic)'},
          {value: '1971-2000', label: '1971\u20132000 (WMO Historical)'},
          {value: '1981-2010', label: '1981\u20132010 (WMO Previous)'},
          {value: '1991-2020', label: '1991\u20132020 (WMO Current)'},
          {value: '2011-2023', label: '2011\u20132023 (Recent)'}
        ]);
        selectize.setValue('1981-2010', true);
        Shiny.setInputValue('projection_period', '1981-2010');
      }
    }
  });

});
