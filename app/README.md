# ⚡ PowerClimate Vision Explorer

**Interactive spatial dashboard for visualizing Copernicus PECD v4.2 climate-energy boundaries and multi-model projections across Europe.**

Built with [R Shiny](https://shiny.posit.co/), [Apache Arrow](https://arrow.apache.org/), and [MapLibre GL](https://maplibre.org/) as part of [Code for Earth 2026](https://codeforearth.ecmwf.int/).

🚀 **Live Application:** [https://adumitrescu-powervision.hf.space/](https://adumitrescu-powervision.hf.space/)

---

## Features

- 🌍 **Globe projection** with interactive MapLibre GL 3D globe & 2D Mercator map
- 🗺️ **Six spatial tiers**: NUTS 0, NUTS 2, ENTSO-E onshore/offshore bidding zones, fine-grained study zones
- 🎨 **Dark glassmorphism UI** with floating control panels and slide-up stats drawer
- ⚡ **Multi-vector energy suite**: Onshore/offshore wind (dynamic blending to 2050), solar PV & CSP, and 7 hydropower streams
- 📊 **Multi-model climate projections**: ERA5 historical baseline (1950–2023) + 6 CMIP6 models (2021–2100) across 4 SSP scenarios with calibrated IPCC uncertainty spread
- 📅 **36 ENTSO-E Weather Scenarios**: Daily annual cycle profiles (WS01–WS36)
- 🛰️ **Three basemaps**: Positron, Bright, Sentinel-2 satellite hybrid
- 🔍 Auto-zoom to clicked regions and full tier extents
- 💾 **Context-aware CSV export**: One-click RFC 4180 downloads for power system modeling

## Quick Start

```r
# Restore pinned dependencies via renv
renv::restore()

# Run the app from the repository root
shiny::runApp("app")
```

## Data Sources

| Source | Description |
|---|---|
| [Copernicus PECD v4.2](https://cds.climate.copernicus.eu/) | Pan-European Climate Database spatial boundaries & time-series |
| [Eurostat GISCO NUTS 2021](https://ec.europa.eu/eurostat/web/gisco) | Official country and regional administrative boundaries |
| [ENTSO-E](https://www.entsoe.eu/) | European electricity bidding zones & Weather Scenarios |
| [EOX Sentinel-2](https://s2maps.eu/) | Cloudless satellite imagery basemap |

## Project Structure

```text
├── global.R                # Dataset initialization, GeoJSON caching, constants
├── server.R                # Core reactive engine, MapLibre & Plotly observers
├── ui.R                    # Glassmorphism layout, sidebar controls, stats drawer
├── renv.lock               # Deterministic R package dependency pinning
├── R/                      # Modular backend query and UI helper modules
│   ├── helpers_climate_query.R # Apache Arrow Hive Parquet query engine
│   ├── helpers_wind_blend.R    # Dynamic wind turbine technology blending engine
│   ├── helpers_ws_query.R      # Weather Scenarios (WS01–WS36) daily query helper
│   ├── helpers_chart.R         # Plotly time-series, multi-scenario & cycle charts
│   ├── helpers_map.R           # MapLibre GL choropleth rendering & styling
│   ├── helpers_legend.R        # Dynamic legend colorbar slicing & intervals
│   ├── helpers_ui_drawer.R     # Detail stats drawer UI modules and tabs
│   └── helpers_export.R        # Context-aware RFC 4180 CSV export engine
├── text/
│   └── about.md            # In-app information modal and documentation
└── www/
    ├── styles.css          # Glassmorphism CSS design system & responsive rules
    ├── app.js              # JavaScript bridge for MapLibre, drawers & controls
    └── data/
        ├── geo/            # GeoJSON boundaries (NUTS 0/2, P2ON/P2OF, SZON/SZOF)
        ├── technology_mix/ # Turbine mix ratios, resource groups & mappings
        ├── ws/             # ENTSO-E Weather Scenarios mapping table
        └── pecd/           # Hive Parquet lakehouse (historical, projections, ws_daily)
            ├── historical/ # Annual, seasonal, and monthly ERA5 partitions
            ├── projections/# Annual, seasonal, and monthly CMIP6 partitions
            └── ws_daily/   # Daily annual cycle profiles for 36 Weather Scenarios
```

## License

Code for Earth 2026 — PowerClimate Vision Explorer, Meteo Romania Team

