An interactive spatial dashboard produced during the ECMWF Code for Earth 2026 <a href="https://github.com/ECMWFCode4Earth/Challenges_2026/issues/15" target="_blank">Challenge 14 - Visualising the impact of climate change for the European power system</a> for exploring the **Pan-European Climate Database (PECD v4.2)** (which is provided by the Copernicus Climate Change Service (C3S) in collaboration with ENTSO-E).

### Data Sources
* **Copernicus PECD v4.2** — Climate and energy variables from ERA5 reanalysis and CMIP6 projections (6 models, 4 SSP scenarios)
* **Eurostat GISCO** — NUTS 2021 administrative boundaries (Level 0 & Level 2)
* **ENTSO-E** — Bidding zone boundaries (PEON, PEOF)
* **Study Zones** — Fine-grained onshore and offshore zones (SZON, SZOF)

### Climate Variables
* 2m Air Temperature (TA)
* Population-Weighted Temperature (TAW)
* Total Precipitation (TP)
* Surface Solar Radiation Downwards (GHI)
* 10m & 100m Wind Speed (WS10, WS100)

### Basemap Layers
* **OpenFreeMap Positron & Bright** — Vector tile basemaps optimized for clean spatial visualization, powered by OpenStreetMap data.
* **EOX Sentinel-2 Cloudless** — High-resolution global satellite imagery composite (2023) provided by EOX IT Services GmbH.

### Technology
* Built with R/Shiny and MapLibre GL (globe projection)
* Spatial boundaries served as GeoJSON
