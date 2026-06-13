// app.js
// ==============================================================================
// Copernicus PECD v4.2 — PowerClimate Vision Explorer
// Custom JavaScript — UI interactions and Shiny message handlers
// Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
// ==============================================================================

$(document).ready(function () {

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

});
