> ⚠️ **This application is under active development** and may be modified until the final version is delivered by the end of the project.

An interactive spatial dashboard produced during the ECMWF Code for Earth 2026 <a href="https://github.com/ECMWFCode4Earth/Challenges_2026/issues/15" target="_blank">Challenge 14 - Visualising the impact of climate change for the European power system</a> for exploring the **Pan-European Climate Database (PECD v4.2)** (which is provided by the Copernicus Climate Change Service (C3S) in collaboration with ENTSO-E).

### Data Sources
* **Copernicus PECD v4.2** — Climate and energy variables from ERA5 reanalysis and CMIP6 projections (6 models, 4 SSP scenarios)
* **Eurostat GISCO** — NUTS 2021 administrative boundaries (Level 0 & Level 2)
* **ENTSO-E** — Bidding zone boundaries (P2ON, P2OF)
* **Study Zones** — Fine-grained onshore and offshore zones (SZON, SZOF)

### Climate Variables
* 2m Air Temperature (TA)
* Population-Weighted Temperature (TAW)
* Total Precipitation (TP)
* Surface Solar Radiation Downwards (GHI)
* 10m & 100m Wind Speed (WS10, WS100)

### Energy Variables
* Wind Power Onshore (Capacity Factor)
* Wind Power Offshore (Capacity Factor)

### Basemap Layers
* **OpenFreeMap Positron & Bright** — Vector tile basemaps optimized for clean spatial visualization, powered by OpenStreetMap data.
* **EOX Sentinel-2 Cloudless** — High-resolution global satellite imagery composite (2023) provided by EOX IT Services GmbH.

### Technology
* Built with R/Shiny and MapLibre GL (globe projection)
* Spatial boundaries served as GeoJSON

### Wind Power Technologies & Dynamic Blending
Wind power projections are fundamentally intertwined with turbine technology evolution. This explorer provides two distinct ways to analyze wind power capacity factors:
* **Fixed Technology**: Simulates all data (historical and projected) through a single turbine model (e.g. *Fixed 2030* or *Fixed 2050*). This provides a pure, apples-to-apples view where any change in capacity factor is **100% driven by climate change**, isolating the meteorological signal without artificial bumps from upgrading technology. *Note: "Fixed 2020 (Existing)" data is only available for EU regions; non-EU study zones lack this historical baseline.*
* **Dynamic (Real-world progression)**: Simulates a realistic fleet where turbine models are continuously upgraded over time. Every region in Europe is assigned a **Wind Resource Group** (e.g., *Very High, High, Medium, Low* for onshore; *High, Low* for offshore). As time progresses from 2025 to 2050, the underlying mix of turbines is linearly interpolated year-by-year to reflect real-world technology improvements (e.g., replacing older turbines with taller, higher-swept-area models). 
  * *Tip: You can view the exact Resource Group and the percentage breakdown of turbine technologies used for any region's calculation by hovering over it on the map!*
  * *Note: Because dynamic blending constantly shifts over time, the **historical Annual Cycle** plot uses the 2025 technology as a consistent baseline proxy to draw the 12-month shape.*
