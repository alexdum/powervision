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
    tags$link(rel = "stylesheet", href = "styles.css?v=1.2"),
    tags$script(src = "app.js?v=1.3")
  ),

  # ── Full-screen MapLibre canvas (z-index 0) ──────────────────────────────────
  maplibreOutput("map", height = "100vh", width = "100%"),

  # ── Map Loading Shimmer ────────────────────────────────────────────────────────
  # A faint pulsing overlay shown during choropleth re-rendering.
  # pointer-events: none ensures it never blocks map interaction.
  # The server sends show/hide messages; CSS handles animation.
  tags$div(
    id = "map-loading-shimmer",
    tags$div(class = "map-spinner")
  ),

  # ── LEFT: Glassmorphism Control Panel ────────────────────────────────────────
  absolutePanel(
    id = "control-panel",
    top = 18,
    left = 18,
    width = 290,

    # ── Scrollable controls area ───────────────────────────────────────────────
    # All controls live in this scrollable zone. When projection controls expand,
    # only this area scrolls — the legend footer stays pinned at the bottom.
    div(
      class = "sidebar-scroll-area",

      # Brand header
      div(
      class = "app-brand",
      div(class = "brand-icon", HTML("&#9889;")), # ⚡ lightning
      div(
        class = "brand-text",
        div(class = "brand-title", "PowerClimate Vision"),
        div(class = "brand-subtitle", "PECD v4.2 · Code for Earth 2026")
      ),
      actionButton(
        "reset_filters",
        label = NULL,
        icon = bsicons::bs_icon("arrow-counterclockwise"),
        class = "btn-reset-filters",
        title = "Reset to Defaults"
      )
    ),

    # ── CARD 1: Core Data ──────────────────────────────────────────────────────
    div(
      class = "filter-card",
      div(class = "filter-card-header", bsicons::bs_icon("database-fill"), " Core Data"),

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
        "P2ON — Onshore Sub-zones"  = "P2ON",
        "P2OF — Offshore Sub-zones" = "P2OF"
      ),
      selected = "NUT0"
    ),

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
      choices = list(
        "Climate Variables" = c(
          "2m Temperature" = "2m_temperature",
          "Total Precipitation" = "total_precipitation",
          "Solar Radiation" = "surface_solar_radiation_downwards",
          "10m Wind Speed" = "10m_wind_speed",
          "100m Wind Speed" = "100m_wind_speed"
        ),
        "Energy Indicators" = c(
          "Wind Power Onshore (CF)" = "wind_power_onshore",
          "Wind Power Offshore (CF)" = "wind_power_offshore"
        )
      ),
      selected = "2m_temperature"
    ),

    # ── Section: Technology Mix (Only visible for Wind Power) ──────────────────
    div(
      id = "tech-mix-wrapper",
      class = "tech-mix-wrapper",
      style = "display: none;", # Hidden by default, toggled via JS
      selectInput(
        inputId = "technology_mix",
        label = span(
          "Technology Mix",
          tooltip(
            bsicons::bs_icon("info-circle", size = "0.85em"),
            "Controls which turbine technology assumptions are used for blending capacity factors."
          )
        ),
        choices = c(
          "Dynamic (Real-world progression)" = "dynamic",
          "Fixed Existing Technology (2020)" = "fixed_2020",
          "Fixed 2025 Technology"            = "fixed_2025",
          "Fixed 2030 Technology"            = "fixed_2030",
          "Fixed 2040 Technology"            = "fixed_2040",
          "Fixed 2050 Technology"            = "fixed_2050"
        ),
        selected = "dynamic",
        width = "100%"
      )
    ),
    ), # End Card 1

    # ── CARD 2: Time & Projections ─────────────────────────────────────────────
    div(
      class = "filter-card",
      div(class = "filter-card-header", bsicons::bs_icon("clock-history"), " Time & Scenarios"),

    selectInput(
      inputId = "temporal_mode",
      label = span(
        "Temporal Filter",
        tooltip(
          bsicons::bs_icon("info-circle", size = "0.85em"),
          "Select 'Annual' for full-year sum/mean, or choose a specific season or month."
        )
      ),
      choices = list(
        "Annual" = c("Annual" = "Annual"),
        "Seasons" = c(
          "Winter" = "Winter",
          "Spring" = "Spring",
          "Summer" = "Summer",
          "Autumn" = "Autumn"
        ),
        "Months" = c(
          "January" = "1",
          "February" = "2",
          "March" = "3",
          "April" = "4",
          "May" = "5",
          "June" = "6",
          "July" = "7",
          "August" = "8",
          "September" = "9",
          "October" = "10",
          "November" = "11",
          "December" = "12"
        )
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

    # View mode toggle — Year vs Period
    # Always available. Controls whether the map shows a single year (slider)
    # or a multi-year period average (dropdown with historical & projected periods).
    tags$input(
      type = "hidden",
      id = "projection_view_mode",
      value = "year",
      class = "shiny-bound-input"
    ),
    div(
      class = "view-mode-toggle",
      id = "view-mode-toggle",
      div(class = "view-toggle-pill"),
      tags$button(
        type = "button",
        class = "view-toggle-option active",
        `data-value` = "year",
        bsicons::bs_icon("calendar3", size = "0.8em"),
        "Year"
      ),
      tags$button(
        type = "button",
        class = "view-toggle-option",
        `data-value` = "period",
        bsicons::bs_icon("calendar-range", size = "0.8em"),
        "Period"
      )
    ),

    # Period dropdown — visible when Period mode selected
    # Includes both historical WMO baselines and IPCC projection windows.
    # Projected periods are added dynamically by server when projections are ON.
    div(
      id = "projection-period-wrapper",
      class = "projection-period-wrapper",
      selectInput(
        inputId = "projection_period",
        label = NULL,
        choices = c(
          "1961\u20131990 (WMO Classic)"   = "1961-1990",
          "1971\u20132000 (WMO Historical)"= "1971-2000",
          "1981\u20132010 (WMO Previous)"  = "1981-2010",
          "1991\u20132020 (WMO Current)"   = "1991-2020",
          "2011\u20132023 (Recent)"        = "2011-2023"
        ),
        selected = "1981-2010",
        width = "100%"
      )
    ),



    # ── Section: Climate Projections ────────────────────────────────────────────
    # These controls allow exploring projected climate data (CMIP6 models) on
    # the map without needing to click a region first. They affect both the
    # map choropleth and the time-series chart in the bottom drawer.
    # The entire group is hidden when projection data is not available for the
    # current variable/spatial level (controlled by server via JS message).
    div(
      id = "projection-controls",
      class = "sidebar-projection-controls",

      # Section label
      span(
        class = "control-label",
        "Climate Projections",
        tooltip(
          bsicons::bs_icon("question-circle", size = "0.85em"),
          paste0(
            "SSP (Shared Socioeconomic Pathway) scenarios represent different ",
            "plausible futures based on greenhouse gas emissions. ",
            "Lower numbers (SSP1) = strong climate policies; ",
            "higher numbers (SSP5) = continued fossil fuel reliance. ",
            "Projections show the median of 6 CMIP6 climate models."
          )
        )
      ),

      # Toggle switch — Off / Projections
      tags$input(
        type = "hidden",
        id = "show_projections",
        value = "0",
        class = "shiny-bound-input"
      ),
      div(
        class = "projection-show-toggle",
        id = "projection-show-toggle",
        div(class = "proj-toggle-pill"),
        tags$button(
          type = "button",
          class = "proj-toggle-option active",
          `data-value` = "0",
          bsicons::bs_icon("eye-slash", size = "0.8em"),
          "Off"
        ),
        tags$button(
          type = "button",
          class = "proj-toggle-option",
          `data-value` = "1",
          bsicons::bs_icon("graph-up-arrow", size = "0.8em"),
          "Projections"
        )
      ),

      # Scenario dropdown — only visible when toggle is on
      div(
        id = "scenario-selector-wrapper",
        class = "scenario-selector-wrapper",
        selectInput(
          inputId = "ssp_scenario",
          label = NULL,
          choices = c(
            "SSP1-2.6 \u2014 Sustainability"           = "ssp1_2_6",
            "SSP2-4.5 \u2014 Middle of the Road"        = "ssp2_4_5",
            "SSP3-7.0 \u2014 Regional Rivalry"           = "ssp3_7_0",
            "SSP5-8.5 \u2014 Fossil-fueled Development"  = "ssp5_8_5"
          ),
          selected = "ssp2_4_5",
          width = "100%"
        )
      ),

      # Display mode toggle — Absolute vs Anomaly
      div(
        id = "display-mode-wrapper",
        class = "display-mode-wrapper",
        tags$input(
          type = "hidden",
          id = "display_mode",
          value = "absolute",
          class = "shiny-bound-input"
        ),
        div(
          class = "display-mode-toggle",
          id = "display-mode-toggle",
          div(class = "display-toggle-pill"),
          tags$button(
            type = "button",
            class = "display-toggle-option active",
            `data-value` = "absolute",
            bsicons::bs_icon("thermometer-half", size = "0.8em"),
            "Absolute"
          ),
          tags$button(
            type = "button",
            class = "display-toggle-option",
            `data-value` = "anomaly",
            bsicons::bs_icon("plus-slash-minus", size = "0.8em"),
            "Anomaly"
          )
        )
      ),

      # Reference period dropdown — visible when Anomaly mode selected
      div(
        id = "reference-period-wrapper",
        class = "reference-period-wrapper",
        selectInput(
          inputId = "reference_period",
          label = NULL,
          choices = c(
            "Baseline: 1991\u20132020 (WMO Current)"  = "1991-2020",
            "Baseline: 1981\u20132010 (WMO Previous)" = "1981-2010",
            "Baseline: 1971\u20132000 (WMO Historical)"= "1971-2000",
            "Baseline: 1961\u20131990 (WMO Classic)"  = "1961-1990"
          ),
          selected = "1981-2010",
          width = "100%"
        )
      )
    ),
    ), # End Card 2

    # ── CARD 3: Display Settings ───────────────────────────────────────────────
    div(
      class = "filter-card",
      div(class = "filter-card-header", bsicons::bs_icon("palette-fill"), " Display Settings"),

    # ── Section: Layer Info ─────────────────────────────────────────────────────
    div(class = "layer-info-text", htmlOutput("layer_metadata_text")),

    # ── Section: Polygon Opacity ───────────────────────────────────────────────
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

    # ── Map Projection Toggle — custom pill switch ─────────────────────────────
    # Placed at the bottom of the sidebar since it's a less frequently changed
    # setting. A hidden text input carries the Shiny value ("globe" or "mercator").
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
    )
    ), # End Card 3
    ), # end sidebar-scroll-area

    # ── Pinned footer: legend + attribution ────────────────────────────────────
    # This section is always visible at the bottom of the sidebar, regardless
    # of scroll position. Scientists need the legend to interpret the map.
    div(
      class = "sidebar-footer",

      # Choropleth Legend
      htmlOutput("choropleth_legend"),

      # Data attribution
      div(
        class = "data-attribution",
        "Copernicus Climate Data Store · PECD v4.2",
        tags$br(),
        "Eurostat GISCO NUTS 2021"
      )
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

        includeMarkdown("text/about.md"),

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

    # Header row: region name + close button
    div(
      class = "drawer-header",

      # Left side: drawer title
      div(
        class = "drawer-title",
        bsicons::bs_icon("geo-alt-fill", size = "0.9em"),
        "Selected Region Analysis"
      ),

      # Right side: close button
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
        div(class = "metric-grid", htmlOutput("region_stats_cards")),

        # Download CSV button — exports the chart data (historical + projections)
        # for the currently selected region as a CSV file.
        div(
          class = "drawer-download-section",
          downloadButton(
            outputId = "download_chart_csv",
            label = "Download CSV",
            class = "drawer-download-btn",
            icon = shiny::icon("download")
          )
        )
      ),
      div(
        class = "drawer-chart-column",
        plotlyOutput("region_timeseries", height = "100%", width = "100%")
      )
    )
  )
)
