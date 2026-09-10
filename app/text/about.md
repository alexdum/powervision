> ⚠️ **This application is under active development** and may be modified until the final version is delivered by the end of the project.

An interactive spatial dashboard produced during the ECMWF Code for Earth 2026 <a href="https://github.com/ECMWFCode4Earth/Challenges_2026/issues/15" target="_blank">Challenge 14 - Visualising the impact of climate change for the European power system</a> for exploring the <a href="https://doi.org/10.24381/cds.f323c5ec" target="_blank">**Pan-European Climate Database (PECD v4.2)**</a> (provided by the Copernicus Climate Change Service (C3S) in collaboration with ENTSO-E).

### Climate Variables (Available across all map tiers)
* 2m Air Temperature (TA)
* Total Precipitation (TP)
* Surface Solar Radiation Downwards (GHI)
* 10m & 100m Wind Speed (WS10, WS100)

### Energy Variables (Available only at specific spatial tiers)
* **P2ON (Pan-European Onshore)**
  * Wind Power Onshore (Capacity Factor)
  * Concentrated Solar Power (Capacity Factor)
  * Solar Photovoltaic (Capacity Factor)
  * Wind Resource Group (Categorical Map)
* **P2OF (Pan-European Offshore)**
  * Wind Power Offshore (Capacity Factor)
  * Wind Resource Group (Categorical Map)
* **SZON (Study Zones Onshore)**
  * Hydropower Reservoir (Generation & Inflow)
  * Hydropower Run-of-River (Generation & Inflow)
  * Hydropower Run-of-River w/ Pondage (Generation & Inflow)
  * Hydropower Open-Loop Pumped Storage (Inflow)

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
  * *Note: For **historical years**, the dynamic mode applies the **blended 2025 technology mix** (the earliest anchor in the transition window). This means historical capacity factors on the time-series chart, map, and Annual Cycle boxplots are computed as a weighted average of multiple turbine technologies using the region-specific resource group weights defined for 2025 — consistent with the backward clamping rule that all years ≤ 2025 use the 2025 mix. This provides a scientifically consistent baseline that connects seamlessly to the dynamically-evolving projection data.*
* **Anomalies and Technology**: To calculate anomalies (e.g., how wind power changes compared to the 1991-2020 baseline), you must select a **Fixed Technology**. Anomalies are mathematically disabled when using the **Dynamic** progression. This is because comparing 2050's highly efficient turbines against 1990's baseline turbines would artificially inflate the difference, conflating technological advancement with true climate change. Fixing the technology ensures an "apples-to-apples" comparison that reveals only the climate signal.

### Weather Scenarios (WS)
In addition to historical reanalysis and future projections, the explorer includes **36 Weather Scenarios (WS)**. 
* **What they are**: These weather scenarios represent the champion set of 36 model-year combinations available for ENTSO-E-wide and national studies. They intend to be a representative subset of all six different models from the Coupled Model Intercomparison Project Phase 6 available in the PECD over the time period from 2026 to 2055 under SSP2-4.5.
* **How to find them**: In the left sidebar, change the **Temporal Filter** dropdown from *Annual* (or any season/month) to **Weather Scenarios (Daily)**. The map will update to show the selected scenario's value for each region. Click on any region to open the bottom statistics drawer, where you can explore the **WS Annual Cycle** tab. This chart displays the 15-day smoothed daily trajectories of all 36 scenarios simultaneously, highlighting the envelope of variability throughout the year. You can also explicitly highlight specific scenarios from the left sidebar.
* **Energy Indicators Daily Methodology**:
  * **Wind Power (2020 Existing Fleet Baseline)**: Onshore wind is evaluated using the pure raw existing fleet baseline (`tech_30`), and offshore wind is evaluated using the pure raw coastal existing fleet baseline (`tech_20`) across all 36 scenario-years under SSP2-4.5. This isolates meteorological impacts on the existing European wind turbine fleet without confounding dynamic technology upgrade assumptions.
  * **Solar Power (Direct Technology Lookups)**: Both Solar Photovoltaic (PV) and Concentrated Solar Power (CSP) provide direct lookups across available technology subcodes:
    * *Solar PV*: Subcode 60 (Industrial rooftop, default), Subcode 61 (Residential rooftop), Subcode 62 (Commercial rooftop), and Subcode 63 (Utility-scale ground-mounted).
    * *Solar CSP*: Subcode 40 (Pre-dispatch, no storage, default), Subcode 41 (Dispatched, no storage), Subcode 42 (Pre-dispatch, 7h storage), and Subcode 43 (Dispatched, 7h storage).
    * Switching solar technologies in the left sidebar dynamically updates the 36 daily annual cycle trajectories in real time.
  * **Country-Level Spatial Aggregation (NUT0)**: For national-level analysis (`NUT0`), daily capacity factor time series are synthesized via polygon area-weighted spatial aggregation (`area_km2`) of the underlying granular bidding zones (`P2ON` for onshore wind and solar; `P2OF` coastal zones for offshore wind).
  * **Calendar Harmonization**: To enable exact day-of-year comparisons across all 36 scenarios, all daily time series are harmonized to exactly 365 daily steps per scenario-year (leap day February 29 is excluded in leap years).


### Data Sources & Citation
* **Copernicus PECD v4.2** — Climate and energy related variables derived from reanalysis and climate projections (6 models, 4 SSP scenarios).
  * **Citation**: Copernicus Climate Change Service (2024): *Climate and energy related variables from the Pan-European Climate Database derived from reanalysis and climate projections*. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: <a href="https://doi.org/10.24381/cds.f323c5ec" target="_blank">10.24381/cds.f323c5ec</a>
* **Eurostat GISCO** — NUTS 2021 administrative boundaries (Level 0 & Level 2)
* **ENTSO-E** — Bidding zone boundaries (P2ON, P2OF)
* **Study Zones** — Fine-grained onshore and offshore zones (SZON, SZOF)
