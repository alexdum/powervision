# ui.R
# ==============================================================================
# Copernicus PECD v4.2 — PowerClimate Vision Explorer
# UI Redesign: Full-screen SPA with glassmorphism floating panels
# Author: Climate Research & Spatial Analysis Team (Code for Earth 2026)
# ==============================================================================

ui <- page_fillable(
  # Padding removed so map bleeds edge-to-edge
  padding = 0,

  # ── Head: fonts, meta tags, stylesheet, scripts ─────────────────────────────
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$meta(
      name = "description",
      content = "PowerClimate Vision Explorer — Copernicus PECD v4.2 Interactive Spatial Dashboard"
    ),
    tags$title("PowerClimate Vision Explorer"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),

  # ── Full-screen MapLibre canvas (z-index 0) ──────────────────────────────────
  maplibreOutput("map", height = "100vh", width = "100%"),



  # ── LEFT: Glassmorphism Control Panel ────────────────────────────────────────
  absolutePanel(
    id = "control-panel",
    top = 18,
    left = 18,
    width = 290,

    # Brand header
    div(
      class = "app-brand",
      div(class = "brand-icon", HTML("&#9889;")), # ⚡ lightning
      div(
        class = "brand-text",
        div(class = "brand-title", "PowerClimate Vision"),
        div(class = "brand-subtitle", "PECD v4.2 · Code for Earth 2026")
      )
    ),

    # ── Section: Geographical Tier ─────────────────────────────────────────────
    selectInput(
      inputId = "spatial_level",
      label = span(
        "Aggregation Level",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Select the spatial boundary layer used in PECD v4.2 or Eurostat NUTS 2021 classifications."
        )
      ),
      choices = c(
        "NUTS 0 — Countries"        = "NUT0",
        "SZON — Onshore Zones"      = "SZON",
        "SZOF — Offshore Zones"     = "SZOF",
        "NUTS 2 — Provinces"        = "NUT2",
        "PEON — Onshore Sub-zones"  = "PEON",
        "PEOF — Offshore Sub-zones" = "PEOF"
      ),
      selected = "NUT0"
    ),

    hr(class = "panel-divider"),

    # ── Section: Climate Variable ──────────────────────────────────────────────
    selectInput(
      inputId = "climate_variable",
      label = span(
        "PECD Variable",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Select the historical climate variable to visualize on the map."
        )
      ),
      choices = c(
        "2m Temperature" = "2m_temperature",
        "Total Precipitation" = "total_precipitation",
        "Solar Radiation" = "surface_solar_radiation_downwards",
        "10m Wind Speed" = "10m_wind_speed",
        "100m Wind Speed" = "100m_wind_speed"
      ),
      selected = "2m_temperature"
    ),

    selectInput(
      inputId = "temporal_mode",
      label = span(
        "Temporal Filter",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Select 'Annual' for full-year sum/mean, or choose a specific season."
        )
      ),
      choices = c(
        "Annual" = "Annual",
        "Winter" = "Winter",
        "Spring" = "Spring",
        "Summer" = "Summer",
        "Autumn" = "Autumn"
      ),
      selected = "Annual"
    ),

    sliderInput(
      inputId = "selected_year",
      label = span(
        "Selected Year",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Slide to select a year for the choropleth map."
        )
      ),
      min = 1950,
      max = 2023,
      value = 2021,
      step = 1,
      sep = "",
      ticks = FALSE
    ),

    hr(class = "panel-divider"),

    # ── Section: Display ────────────────────────────────────────────────────────
    sliderInput(
      inputId = "polygon_opacity",
      label = span(
        "Polygon Opacity",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "0 = fully transparent · 1 = solid fill"
        )
      ),
      min = 0.0,
      max = 1.0,
      value = 0.75,
      step = 0.05,
      ticks = FALSE
    ),

    # ── Projection Toggle — custom pill switch ──────────────────────────────────
    # A hidden text input carries the actual Shiny value ("globe" or "mercator").
    # The visible toggle is pure HTML; JavaScript handles click → slide → sync.
    div(
      class = "projection-toggle-wrapper",
      span(
        class = "control-label",
        "Map Projection",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Globe shows a 3D sphere · Mercator shows a flat 2D map"
        )
      ),
      # Hidden Shiny input — the toggle JS writes to this
      tags$input(
        type = "hidden",
        id = "map_projection",
        value = "globe",
        class = "shiny-bound-input"
      ),
      div(
        class = "projection-toggle",
        id = "projection-toggle",
        # Sliding highlight pill (positioned by CSS/JS)
        div(class = "toggle-pill"),
        # Two clickable label segments
        tags$button(
          type = "button",
          class = "toggle-option active",
          `data-value` = "globe",
          bsicons::bs_icon("globe2", size = "0.85em"),
          "Globe"
        ),
        tags$button(
          type = "button",
          class = "toggle-option",
          `data-value` = "mercator",
          bsicons::bs_icon("map", size = "0.85em"),
          "Flat"
        )
      )
    ),

    hr(class = "panel-divider"),

    # ── Section: Layer Info ─────────────────────────────────────────────────────
    div(class = "layer-info-text", htmlOutput("layer_metadata_text")),

    hr(class = "panel-divider"),

    # ── Choropleth Legend ─────────────────────────────────────────────────────
    # Always visible at the bottom of the control panel so users can interpret
    # the map color scale without needing to click a region.
    htmlOutput("choropleth_legend"),

    # Data attribution
    div(
      class = "data-attribution",
      "Copernicus Climate Data Store · PECD v4.2",
      tags$br(),
      "Eurostat GISCO NUTS 2021"
    )
  ),

  # ── RIGHT: Unified vertical button column ────────────────────────────────────
  # All right-side controls live in one absolutePanel to keep them perfectly
  # aligned and prevent any overlap with the now-removed MapLibre nav widget.
  absolutePanel(
    id = "right-btn-col",
    top = 18,
    right = 14,
    style = "z-index: 1000; display: flex; flex-direction: column; gap: 8px; align-items: flex-end;",

    # Zoom In
    actionButton(
      "zoom_in",
      bsicons::bs_icon("plus-lg"),
      class = "btn-map-ctrl",
      title = "Zoom in"
    ),

    # Zoom Out
    actionButton(
      "zoom_out",
      bsicons::bs_icon("dash-lg"),
      class = "btn-map-ctrl",
      title = "Zoom out"
    ),

    # Home / Fit Bounds
    actionButton(
      "zoom_home",
      bsicons::bs_icon("house-fill"),
      class = "btn-map-ctrl btn-map-ctrl--home",
      title = "Zoom to selected region or all of Europe"
    ),

    # Basemap / Boundary hover-expand selector
    div(
      class = "map-layer-control",

      div(class = "control-icon", icon("layer-group")),
      div(
        class = "control-content map-control-right",
        radioButtons(
          inputId = "basemap",
          label = "Basemap Style",
          choices = c(
            "Positron (Light)" = "ofm_positron",
            "Bright (Detailed)" = "ofm_bright",
            "Satellite (Sentinel-2)" = "sentinel"
          ),
          selected = "ofm_positron"
        ),
        hr(style = "margin: 8px 0;"),
        checkboxInput(
          inputId = "show_boundaries",
          label = "Show Reference Borders",
          value = TRUE
        )
      )
    ),

    # About / Info button — opens the About modal overlay
    actionButton(
      "about_btn",
      bsicons::bs_icon("info-circle-fill"),
      class = "btn-map-ctrl btn-map-ctrl--about",
      title = "About this application"
    )
  ),

  # ── ABOUT MODAL OVERLAY ───────────────────────────────────────────────────────
  # Hidden by default. The JS toggles .is-visible when the About button is
  # clicked. A backdrop click or the close button dismisses it.
  tags$div(
    id = "about-overlay",

    # Semi-transparent backdrop — clicking it also closes the modal
    tags$div(id = "about-backdrop"),

    # Modal content panel — glassmorphism card
    tags$div(
      id = "about-modal",

      # Close button
      tags$button(
        id = "about-close-btn",
        class = "about-close",
        type = "button",
        HTML("&times;")
      ),

      # Header
      div(
        class = "about-header",
        div(class = "about-icon", HTML("&#9889;")),
        div(
          class = "about-header-text",
          tags$h2("PowerClimate Vision Explorer"),
          tags$p(class = "about-version", "PECD v4.2 · Code for Earth 2026")
        )
      ),

      # Body content
      div(
        class = "about-body",

        includeMarkdown("www/about.md"),

        tags$div(
          class = "about-footer",
          tags$p(
            "Climate Data Store · ",
            tags$a(
              href = "https://cds.climate.copernicus.eu/datasets/sis-energy-pecd",
              target = "_blank",
              "cds.climate.copernicus.eu"
            )
          ),
          tags$p(
            class = "about-copyright",
            paste0("\u00A9 ", format(Sys.Date(), "%Y"))
          )
        )
      )
    )
  ),

  # ── BOTTOM: Slide-Up Region Stats Drawer ─────────────────────────────────────
  # Hidden by default (CSS transform: translateY(100%)). The server sends a
  # 'toggle_stats_drawer' message that adds/removes the .is-visible class.
  tags$div(
    id = "stats-drawer",

    # Header row: title + close button
    div(
      class = "drawer-header",
      div(
        class = "drawer-title",
        bsicons::bs_icon("geo-alt-fill", size = "0.9em"),
        "Selected Region Analysis"
      ),
      tags$button(
        id = "drawer-close-btn",
        class = "drawer-close",
        type = "button",
        HTML("&times;")
      )
    ),

    # Two-column layout: left column for metadata/legend, right column for timeseries chart
    div(
      class = "drawer-body-layout",
      div(
        class = "drawer-sidebar-column",
        div(class = "metric-grid", htmlOutput("region_stats_cards"))
      ),
      div(
        class = "drawer-chart-column",
        plotlyOutput("region_timeseries", height = "100%", width = "100%")
      )
    )
  )
)
