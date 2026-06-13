# ⚡ PowerClimate Vision Explorer

**Interactive spatial dashboard for visualizing Copernicus PECD v4.2 climate-energy boundaries across Europe.**

Built with [R Shiny](https://shiny.posit.co/) and [MapLibre GL](https://maplibre.org/) as part of [Code for Earth 2026](https://codeforearth.ecmwf.int/).

---

## Features

- 🌍 **Globe projection** with interactive MapLibre GL map
- 🗺️ **Six spatial tiers**: NUTS 0, NUTS 2, ENTSO-E onshore/offshore bidding zones, fine-grained study zones
- 🎨 **Dark glassmorphism UI** with floating control panels and slide-up stats drawer
- 📊 Region metadata cards on polygon click (name, zone ID, parent, area)
- 🛰️ **Three basemaps**: Positron, Bright, Sentinel-2 satellite hybrid
- 🔍 Auto-zoom to clicked regions and full tier extents

## Quick Start

```r
# Install dependencies (if needed)
install.packages(c("shiny", "mapgl", "sf", "dplyr", "bslib", "bsicons"))

# Run the app
shiny::runApp(".")
```

## Data Sources

| Source | Description |
|---|---|
| [Copernicus PECD v4.2](https://cds.climate.copernicus.eu/) | Pan-European Climate Database spatial boundaries |
| [Eurostat GISCO NUTS 2021](https://ec.europa.eu/eurostat/web/gisco) | Official country and regional administrative boundaries |
| [ENTSO-E](https://www.entsoe.eu/) | European electricity bidding zones |
| [EOX Sentinel-2](https://s2maps.eu/) | Cloudless satellite imagery basemap |

## Project Structure

```
├── global.R          # Libraries, configuration, spatial level definitions
├── server.R          # All reactive logic, map rendering, event handlers
├── ui.R              # Page layout, floating panels, stats drawer
├── www/
│   ├── styles.css    # Dark glassmorphism design system
│   ├── app.js        # Drawer toggle, layer control interactions
│   └── data/geo/     # Pre-processed GeoJSON boundary files
├── AGENTS.md         # AI assistant development guidelines
└── DEVELOPMENT_GUIDELINES.md
```

## License

Code for Earth 2026 — Climate Research & Spatial Analysis Team
