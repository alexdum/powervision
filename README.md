# PowerClimate Vision Explorer

Interactive spatial dashboard for exploring Copernicus PECD v4.2 climate-energy boundaries across Europe. Built with R Shiny and MapLibre GL, developed as part of Code for Earth 2026.

The app lets you browse different spatial tiers (countries, NUTS2 regions, ENTSO-E bidding zones, study zones) on an interactive globe, click on regions to see metadata, and switch between basemaps.

## What it does

- Interactive MapLibre GL map with globe projection
- Six spatial tiers: NUTS 0, NUTS 2, ENTSO-E onshore/offshore bidding zones, and fine-grained study zones (onshore/offshore)
- Dark-themed UI with floating control panels and a slide-up stats drawer
- Click any polygon to see region info (name, zone ID, parent area, etc.)
- Three basemaps: Positron, Bright, and Sentinel-2 satellite hybrid
- Auto-zoom on click and on tier change

## Getting started

We run everything through Docker to keep the environment consistent across machines.

### Docker (recommended)

```bash
docker compose up -d --build

# App will be at http://localhost:3838
```

### Running locally in RStudio

If you prefer running outside Docker during development:

```r
# Restore packages (we use renv for version pinning)
renv::restore()

# Run the app
shiny::runApp("app")
```

If you don't want to use renv, you can install the dependencies manually:

```r
install.packages(c("shiny", "mapgl", "sf", "dplyr", "bslib", "bsicons", "arrow", "plotly"))
```

## Data sources

- [Copernicus PECD v4.2](https://cds.climate.copernicus.eu/) — Pan-European Climate Database spatial boundaries
- [Eurostat GISCO NUTS 2021](https://ec.europa.eu/eurostat/web/gisco) — country and regional administrative boundaries
- [ENTSO-E](https://www.entsoe.eu/) — European electricity bidding zones
- [EOX Sentinel-2](https://s2maps.eu/) — cloudless satellite imagery basemap

## Dynamic Wind Power Blending

Wind power in this dashboard is not plotted as static pre-blended files. Copernicus PECD v4.2 provides raw capacity factors for individual, highly-specific turbine types (e.g. `SP277 HH100`). 

To generate a realistic "Wind Power" metric for a given region, the app runs a **dynamic blending engine**:
1. Assigns the region to a Resource Group (e.g., High, Medium, Low).
2. Uses R's `approx(..., rule = 2)` to interpolate a realistic technology mix for the selected year.
   - *Note: If a "Period" (e.g. 2021-2040) is selected instead of a single year, the app mathematically calculates the midpoint of the period (2030) to use as the interpolation anchor.*
   - *Because our technology matrices only go up to 2050, the `rule = 2` ensures that any future midpoints (like 2070 or 2090) are safely capped out at the 2050 advanced fleet mix.*
3. Queries the Hive Parquet dataset for only the active turbines.
4. Safely blends them on the fly, correctly handling missing data gaps (re-normalizing weights if a climate model drops a turbine).

## Spatial tiers and data mapping

The app works with six spatial aggregation tiers. Each one maps to a GeoJSON boundary file (for the polygons you see on the map) and a `SpatialLevel` value in the Parquet data:

| Tier | GeoJSON file | Parquet `SpatialLevel` | Source |
|---|---|---|---|
| NUT0 | `pecd_NUT0.geojson` | `nuts_0` | Eurostat GISCO NUTS 2021 |
| NUT2 | `pecd_NUT2.geojson` | `nuts_2` | Eurostat GISCO NUTS 2021 |
| P2ON | `pecd_P2ON.geojson` | `p2on` | Copernicus PECD v4.2 |
| P2OF | `pecd_P2OF.geojson` | `p2of` | Copernicus PECD v4.2 |
| SZON | `pecd_SZON40.geojson` | `szon` | Copernicus PECD v4.0 |
| SZOF | `pecd_SZOF40.geojson` | `szof` | Copernicus PECD v4.0 |

### About the v4.0 vs v4.2 boundary files

There are two vintages of PECD shapefiles and they use different zone ID schemes. This matters because the GeoJSON zone IDs have to match the `Region` column in the Parquet files exactly — if they don't, you'll get `NA` values and polygons showing "No Data."

The v4.2 files (`pecd_P2ON.geojson`, `pecd_P2OF.geojson`) have finer subdivisions (e.g. `FR021_OFF`, `FR081_OFF` for France offshore), while the v4.0 files use coarser zones (`FR02_OFF`, `FR08_OFF`).

Our processing pipeline reads from the CDS API `p2on`/`p2of` CSV files which use v4.2 zone IDs. So P2ON/P2OF tiers need the v4.2 GeoJSON. For SZON/SZOF, the CSV files use v4.0-style IDs, so those tiers use the v4.0 GeoJSON files.

### Boundary version config

The `PECD_GEOJSON_VERSION` env variable controls which GeoJSON vintage gets loaded:

- `mixed` (default, used in production) — v4.2 for P2ON/P2OF, v4.0 for SZON/SZOF. This matches the processed Parquet data.
- `42` — all v4.2 boundaries (for testing)
- `40` — all v4.0 boundaries (for testing)

Set it in `docker-compose.yml` or pass it at runtime:

```bash
docker run -e PECD_GEOJSON_VERSION=mixed ...
```

## Project layout

```
├── global.R          # Libraries, config, spatial level definitions
├── server.R          # Reactive logic, map rendering, event handlers
├── ui.R              # Page layout, floating panels, stats drawer
├── www/
│   ├── styles.css    # Dark glassmorphism design system
│   ├── app.js        # Drawer toggle, layer control JS
│   └── data/
│       ├── geo/      # GeoJSON boundary files (v4.0 and v4.2)
│       └── pecd/     # Hive-partitioned Parquet climate datasets
│           ├── historical/
│           │   ├── annual/
│           │   └── seasonal/
│           └── projections/
```

## License

Code for Earth 2026 — PowerClimate Vision Explorer, Meteo-Romania Team
