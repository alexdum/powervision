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

The application is designed to be run inside a Docker container to ensure environment consistency.

### 🐳 Recommended: Using Docker (Production)

```bash
# Build and run the container in the background
docker compose up -d --build

# The app will be available at http://localhost:3838
```

### 💻 Local Development (R)

If you are developing locally and prefer to run the app directly in RStudio:

```r
# Restore exact package versions using renv
renv::restore()

# Or manually install core dependencies (if not using renv)
# install.packages(c("shiny", "mapgl", "sf", "dplyr", "bslib", "bsicons", "arrow", "plotly"))

# Run the app
shiny::runApp("app")
```

## Data Sources

| Source | Description |
|---|---|
| [Copernicus PECD v4.2](https://cds.climate.copernicus.eu/) | Pan-European Climate Database spatial boundaries |
| [Eurostat GISCO NUTS 2021](https://ec.europa.eu/eurostat/web/gisco) | Official country and regional administrative boundaries |
| [ENTSO-E](https://www.entsoe.eu/) | European electricity bidding zones |
| [EOX Sentinel-2](https://s2maps.eu/) | Cloudless satellite imagery basemap |

## PECD Data & GeoJSON Boundary Logic

### Spatial Tiers

The app displays six spatial aggregation tiers. Each tier maps to a specific
GeoJSON boundary file (for the map polygons) and a Parquet `SpatialLevel` value
(for the climate data):

| UI Tier | GeoJSON File | Parquet `SpatialLevel` | Source |
|---|---|---|---|
| NUT0 | `pecd_NUT0.geojson` | `nuts_0` | Eurostat GISCO NUTS 2021 |
| NUT2 | `pecd_NUT2.geojson` | `nuts_2` | Eurostat GISCO NUTS 2021 |
| PEON | `pecd_PEON.geojson` (v4.2) | `p2on` | Copernicus PECD v4.2 |
| PEOF | `pecd_PEOF.geojson` (v4.2) | `p2of` | Copernicus PECD v4.2 |
| SZON | `pecd_SZON40.geojson` (v4.0) | `szon` | Copernicus PECD v4.0 |
| SZOF | `pecd_SZOF40.geojson` (v4.0) | `szof` | Copernicus PECD v4.0 |

### GeoJSON Boundary Versions (v4.0 vs v4.2)

Two vintages of PECD shapefiles exist, each producing GeoJSON files with
**different zone ID schemes**:

| Version | Files | Example FR offshore IDs |
|---|---|---|
| **PECD v4.2** | `pecd_PEON.geojson`, `pecd_PEOF.geojson` | `FR021_OFF`, `FR081_OFF`, `FR082_OFF` (finer subdivisions) |
| **PECD v4.0** | `pecd_PEON40.geojson`, `pecd_PEOF40.geojson` | `FR02_OFF`, `FR08_OFF`, `FR13_OFF` (coarser zones) |

> **⚠️ Critical:** The GeoJSON zone IDs must match the Parquet `Region` column
> exactly, otherwise the `left_join` in `server.R` produces `NA` values and
> polygons appear with "No Data."

### CSV → Parquet Processing Pipeline

Raw CSV files are downloaded from the Copernicus CDS API and processed by
`/data/cds/scripts/process_pecd_historical.R` into Hive-partitioned Parquet
datasets stored in `www/data/pecd/historical/{annual,seasonal}/`.

The CDS API provides two sets of CSV files for the Pan-European node layers:

| CSV Token | Naming Convention | Zone ID Style | Matches GeoJSON |
|---|---|---|---|
| `p2on` / `p2of` | `pecd42_hist_era5_{var}_p2of_*.zip` | v4.2 IDs (`FR081_OFF`) | `pecd_PEOF.geojson` ✅ |
| `peon` / `peof` | `pecd42_hist_era5_{var}_peof_*.zip` | v4.0 IDs (`FR08_OFF`) | `pecd_PEOF40.geojson` ✅ |

The processing script reads **only the `p2on`/`p2of` ZIP files** (v4.2 data).
Therefore the Parquet data contains v4.2 zone IDs, and the app must use the
v4.2 GeoJSON files (`pecd_PEON.geojson`, `pecd_PEOF.geojson`) to match them.

For SZON/SZOF, the CSV files use the `szon`/`szof` token with v4.0-style zone
IDs, so these tiers use the v4.0 GeoJSON files (`pecd_SZON40.geojson`,
`pecd_SZOF40.geojson`).

### Boundary Version Configuration

The `PECD_GEOJSON_VERSION` environment variable controls which GeoJSON vintage
is loaded for each tier. Set it in `docker-compose.yml` or the Dockerfile:

| Value | PEON/PEOF | SZON/SZOF | Use Case |
|---|---|---|---|
| `mixed` (default) | v4.2 | v4.0 | **Production** — matches the processed Parquet data |
| `42` | v4.2 | v4.2 | Testing with all v4.2 boundaries |
| `40` | v4.0 | v4.0 | Testing with all v4.0 boundaries |

```bash
# Override at runtime (Docker)
docker run -e PECD_GEOJSON_VERSION=mixed ...

# Or set in docker-compose.yml
environment:
  - PECD_GEOJSON_VERSION=mixed
```

## Project Structure

```
├── global.R          # Libraries, configuration, spatial level definitions
├── server.R          # All reactive logic, map rendering, event handlers
├── ui.R              # Page layout, floating panels, stats drawer
├── www/
│   ├── styles.css    # Dark glassmorphism design system
│   ├── app.js        # Drawer toggle, layer control interactions
│   └── data/
│       ├── geo/      # Pre-processed GeoJSON boundary files (v4.0 and v4.2)
│       └── pecd/     # Hive-partitioned Parquet climate datasets
│           ├── historical/
│           │   ├── annual/    # One .parquet per variable (all spatial levels)
│           │   └── seasonal/  # One .parquet per variable (all spatial levels)
│           └── projections/   # Future climate projection data
```

## License

Code for Earth 2026 — PowerClimate Vision Explorer  Meteo-Romania Team
