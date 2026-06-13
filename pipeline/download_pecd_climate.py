"""
download_pecd_climate.py — Download PECD v4.2 area-averaged climate variable CSVs
from the Copernicus Climate Data Store (CDS).

This script downloads all 6 core climate variables for:
  - Historical period (ERA5 reanalysis, 1950–2023)
  - Future projections (6 CMIP6 models × 4 SSP scenarios, 2015–2100)

All data are downloaded as regionally aggregated CSV time series, covering
all spatial aggregation levels available (NUTS-0, NUTS-2, P2ON, P2OF, SZON, SZOF).

IMPORTANT — CDS API request size limits:
  The CDS enforces "cost limits" that restrict the number of years you can
  request in a single API call. This script splits downloads into 10-year
  chunks to stay within those limits. Each chunk produces one ZIP file.

Prerequisites:
  1. A CDS account: https://cds.climate.copernicus.eu
  2. Accept the dataset licence on the download page
  3. Configure your API key in ~/.cdsapirc:
         url: https://cds.climate.copernicus.eu/api
         key: <YOUR-API-KEY>
  4. Install the CDS API client:
         pip install "cdsapi>=0.7.7"

Usage:
  python pipeline/download_pecd_climate.py

Output:
  raw/pecd_csv/historical/  — ERA5-based ZIP files (one per variable × spatial level × decade)
  raw/pecd_csv/projections/ — GCM-based ZIP files (one per variable × model × scenario × spatial level × decade)
"""

import cdsapi
import os
import sys
import time
from pathlib import Path

# ==============================================================================
# Configuration
# ==============================================================================

# CDS dataset identifier for the Pan-European Climate Database
DATASET_ID = "sis-energy-pecd"

# PECD version — always use v4.2 (v4.1 is deprecated)
PECD_VERSION = "pecd4_2"

# File versions to try, in order of preference.
# fv2 is the latest corrected release; fv1 is the original release.
# Some variables (e.g., wind speed CSVs) are only available under fv1,
# so we try fv2 first and fall back to fv1 if the combination is invalid.
FILE_VERSIONS = ["fv2", "fv1"]

# The 6 core climate variables available in PECD v4.2
# These are the raw atmospheric/surface fields (NOT energy indicators)
CLIMATE_VARIABLES = [
    "2m_temperature",                     # TA  — 2-metre air temperature
    "total_precipitation",                # TP  — total precipitation
    "surface_solar_radiation_downwards",  # GHI — global horizontal irradiance
    "10m_wind_speed",                     # WS10 — 10-metre wind speed
    "100m_wind_speed",                    # WS100 — 100-metre wind speed
]

# Spatial aggregation levels for area-averaged CSV downloads.
#
# CDS uses P2ON/P2OF for the fine Pan-European node layers in the PECD v4.2 CSV
# files. The app now calls those levels P2ON/P2OF to match the CDS naming:
#   - app_level: the stable name used in parquet/global.R/server.R
#   - cds_spatial_resolution: the value sent to the CDS API
#   - file_token: the token written into local ZIP filenames
#
# The April 2026 CSV checks showed this correspondence:
#   P2ON/P2OF data -> PECD v4.2 P2ON/P2OF GeoJSONs
#   SZON/SZOF data -> PECD v4.0 SZON/SZOF GeoJSONs
SPATIAL_LEVELS = [
    {
        "app_level": "nuts_0",
        "cds_spatial_resolution": "nuts_0",
        "file_token": "nuts_0",
        "description": "Country-level (NUTS 2021 Level 0)",
    },
    {
        "app_level": "nuts_2",
        "cds_spatial_resolution": "nuts_2",
        "file_token": "nuts_2",
        "description": "Provincial-level (NUTS 2021 Level 2)",
    },
    {
        "app_level": "p2on",
        "cds_spatial_resolution": "p2on",
        "file_token": "p2on",
        "description": "Pan-European onshore nodes, matching pecd_P2ON.geojson",
    },
    {
        "app_level": "p2of",
        "cds_spatial_resolution": "p2of",
        "file_token": "p2of",
        "description": "Pan-European offshore nodes, matching pecd_P2OF.geojson",
    },
    {
        "app_level": "szon",
        "cds_spatial_resolution": "szon",
        "file_token": "szon",
        "description": "Onshore study zones, matching pecd_SZON40.geojson",
    },
    {
        "app_level": "szof",
        "cds_spatial_resolution": "szof",
        "file_token": "szof",
        "description": "Offshore study zones, matching pecd_SZOF40.geojson",
    },
]

# Scenario (projection) data spatial levels (skip szon/szof)
PROJECTION_SPATIAL_LEVELS = [
    SPATIAL_LEVELS[0],
    SPATIAL_LEVELS[1],
    SPATIAL_LEVELS[2],
    SPATIAL_LEVELS[3],
]

# The 6 CMIP6 global climate models available in PECD v4.2 projections
CLIMATE_MODELS = [
    "awi_cm_1_1_mr",
    "bcc_csm2_mr",
    "cmcc_cm2_sr5",
    "ec_earth3",
    "mpi_esm1_2_hr",
    "mri_esm2_0",
]

# The 4 SSP emission scenarios available
EMISSION_SCENARIOS = [
    "ssp1_2_6",   # SSP1-2.6 — sustainability pathway
    "ssp2_4_5",   # SSP2-4.5 — middle of the road
    "ssp3_7_0",   # SSP3-7.0 — regional rivalry
    "ssp5_8_5",   # SSP5-8.5 — fossil-fuelled development
]

# Year ranges for downloading
# NOTE: Historical ERA5 data in PECD v4.2 covers 1950 to ~2023.
#       Year 2024 may not be fully available yet (monthly updates).
#       We use 1950–2023 as the safe range.
HISTORICAL_YEARS = list(range(1950, 2024))    # 1950–2023
PROJECTION_YEARS = list(range(2015, 2101))    # 2015–2100

# How many years to include in a single CDS API request.
# The CDS imposes "cost limits" — requesting too many years at once
# triggers a 403 error. 10-year chunks are a safe default.
CHUNK_SIZE_YEARS = 10

# All 12 months
ALL_MONTHS = [f"{m:02d}" for m in range(1, 13)]

# Output base directory (relative to project root)
OUTPUT_BASE = "raw/pecd_csv"

# Number of retries for failed downloads (network errors, server timeouts)
MAX_RETRIES = 3

# Seconds to wait between retries (doubles each attempt)
RETRY_DELAY_SECONDS = 30


# ==============================================================================
# Helper Functions
# ==============================================================================

def ensure_output_dir(subdir):
    """Create the output directory if it doesn't exist."""
    project_root = Path(__file__).resolve().parent.parent
    output_dir = project_root / OUTPUT_BASE / subdir
    output_dir.mkdir(parents=True, exist_ok=True)
    return output_dir


def chunk_years(years, chunk_size):
    """
    Split a list of year integers into chunks of at most chunk_size.
    Returns a list of lists, e.g. [[1950,1951,...,1959], [1960,...,1969], ...]
    """
    chunks = []
    for i in range(0, len(years), chunk_size):
        chunks.append(years[i:i + chunk_size])
    return chunks


def download_file(client, request_params, output_path, label=""):
    """
    Submit a CDS API retrieve request and save the result.
    Skips the download if the output file already exists (resume-friendly).
    Retries on transient errors with exponential backoff.

    Returns:
        "ok"      — file was downloaded successfully (or already existed)
        "invalid" — the parameter combination is not valid on CDS
        "failed"  — all retry attempts exhausted (transient errors)
    """
    if output_path.exists():
        print(f"  [SKIP] Already exists: {output_path.name}")
        return "ok"

    for attempt in range(1, MAX_RETRIES + 1):
        print(f"  [DOWNLOADING] {output_path.name} (attempt {attempt}/{MAX_RETRIES}) ...")
        try:
            client.retrieve(DATASET_ID, request_params, str(output_path))
            print(f"  [OK] Saved: {output_path.name}")
            return "ok"
        except Exception as error:
            error_message = str(error)

            # If the combination is simply not valid (data doesn't exist),
            # don't retry — it will never succeed
            if "not produced a valid combination" in error_message:
                print(f"  [SKIP-INVALID] {output_path.name}: invalid parameter combination")
                print(f"                 {label}")
                return "invalid"

            # If cost limits are exceeded even for this chunk, skip it
            if "cost limits exceeded" in error_message:
                print(f"  [SKIP-COST] {output_path.name}: request still too large")
                print(f"              Try reducing CHUNK_SIZE_YEARS in the script")
                return "failed"

            # For transient errors (network, server), retry with backoff
            delay = RETRY_DELAY_SECONDS * (2 ** (attempt - 1))
            print(f"  [RETRY] {output_path.name}: {error}")
            if attempt < MAX_RETRIES:
                print(f"          Waiting {delay}s before retry...")
                time.sleep(delay)

    print(f"  [FAILED] {output_path.name}: all {MAX_RETRIES} attempts failed")
    return "failed"


def download_with_fv_fallback(client, request_params, output_path, label=""):
    """
    Try downloading with each file version in FILE_VERSIONS (fv2 first,
    then fv1). If the first version returns "invalid combination", retry
    with the next version. This handles variables like wind speed that
    are only available under fv1.

    Returns True if a file was downloaded, False otherwise.
    """
    for file_version in FILE_VERSIONS:
        # Update the file version in the request parameters
        request_params["file_version"] = [file_version]

        # Build a version-specific label for logging
        version_label = f"{label}, fv={file_version}"

        result = download_file(client, request_params, output_path, version_label)

        if result == "ok":
            return True

        # If the combination was invalid, try the next file version
        if result == "invalid":
            print(f"  [FV-FALLBACK] Trying next file version for: {output_path.name}")
            continue

        # If it failed for other reasons (cost, network), stop trying
        return False

    # All file versions exhausted — none worked
    print(f"  [SKIP-ALL-FV] {output_path.name}: no valid file version found")
    return False


# ==============================================================================
# Download: Historical ERA5 Reanalysis
# ==============================================================================

def download_historical(client):
    """
    Download all historical (ERA5 reanalysis) climate variable CSVs.
    Years are split into 10-year chunks to avoid CDS cost limits.
    One file per variable × spatial level × decade.
    """
    output_dir = ensure_output_dir("historical")
    year_chunks = chunk_years(HISTORICAL_YEARS, CHUNK_SIZE_YEARS)

    print("\n" + "=" * 70)
    print("HISTORICAL PERIOD (ERA5 reanalysis, 1950–2023)")
    print(f"  Split into {len(year_chunks)} chunks of up to {CHUNK_SIZE_YEARS} years each")
    print("=" * 70)

    success_count = 0
    skip_count = 0
    fail_count = 0

    for variable in CLIMATE_VARIABLES:
        for spatial_level_info in SPATIAL_LEVELS:
            app_level = spatial_level_info["app_level"]
            cds_spatial_resolution = spatial_level_info["cds_spatial_resolution"]
            file_token = spatial_level_info["file_token"]

            for year_chunk in year_chunks:
                # Build a descriptive filename that includes the year range
                first_year = year_chunk[0]
                last_year = year_chunk[-1]
                filename = f"pecd42_hist_era5_{variable}_{file_token}_{first_year}-{last_year}.zip"
                output_path = output_dir / filename

                # Convert years to strings for the API
                year_strings = [str(y) for y in year_chunk]

                # Build the request — file_version is set by the fallback wrapper
                request_params = {
                    "pecd_version":       PECD_VERSION,
                    "temporal_period":    ["historical"],
                    "origin":             ["era5_reanalysis"],
                    "variable":           [variable],
                    "spatial_resolution": [cds_spatial_resolution],
                    "year":               year_strings,
                    "month":              ALL_MONTHS,
                }

                label = (
                    f"var={variable}, app_level={app_level}, "
                    f"cds_spatial={cds_spatial_resolution}, years={first_year}-{last_year}"
                )
                result = download_with_fv_fallback(client, request_params, output_path, label)

                if result:
                    success_count += 1
                else:
                    fail_count += 1

    print(f"\n  Historical summary: {success_count} downloaded, {skip_count} skipped, {fail_count} failed")


# ==============================================================================
# Download: Future Climate Projections
# ==============================================================================

def download_projections(client):
    """
    Download all future projection climate variable CSVs.
    Years are split into 10-year chunks to avoid CDS cost limits.
    One file per variable × climate model × emission scenario × spatial level × decade.
    """
    output_dir = ensure_output_dir("projections")
    year_chunks = chunk_years(PROJECTION_YEARS, CHUNK_SIZE_YEARS)

    print("\n" + "=" * 70)
    print("FUTURE PROJECTIONS (CMIP6 models × SSP scenarios, 2015–2100)")
    print(f"  Split into {len(year_chunks)} chunks of up to {CHUNK_SIZE_YEARS} years each")
    print("=" * 70)

    success_count = 0
    skip_count = 0
    fail_count = 0

    for variable in CLIMATE_VARIABLES:
        for model in CLIMATE_MODELS:
            for scenario in EMISSION_SCENARIOS:
                for spatial_level_info in PROJECTION_SPATIAL_LEVELS:
                    app_level = spatial_level_info["app_level"]
                    cds_spatial_resolution = spatial_level_info["cds_spatial_resolution"]
                    file_token = spatial_level_info["file_token"]

                    for year_chunk in year_chunks:
                        # Build a descriptive filename with year range
                        first_year = year_chunk[0]
                        last_year = year_chunk[-1]
                        filename = (
                            f"pecd42_proj_{model}_{scenario}_{variable}"
                            f"_{file_token}_{first_year}-{last_year}.zip"
                        )
                        output_path = output_dir / filename

                        # Convert years to strings for the API
                        year_strings = [str(y) for y in year_chunk]

                        # Build the request — no "month" for projections (not supported),
                        # and file_version is set by the fallback wrapper
                        request_params = {
                            "pecd_version":       PECD_VERSION,
                            "temporal_period":    ["future_projections"],
                            "origin":             [model],
                            "emission_scenario":  [scenario],
                            "variable":           [variable],
                            "spatial_resolution": [cds_spatial_resolution],
                            "year":               year_strings,
                        }

                        label = (
                            f"var={variable}, model={model}, ssp={scenario}, "
                            f"app_level={app_level}, cds_spatial={cds_spatial_resolution}, "
                            f"years={first_year}-{last_year}"
                        )
                        result = download_with_fv_fallback(client, request_params, output_path, label)

                        if result:
                            success_count += 1
                        else:
                            fail_count += 1

    print(f"\n  Projections summary: {success_count} downloaded, {skip_count} skipped, {fail_count} failed")


# ==============================================================================
# Main Entry Point
# ==============================================================================

def main():
    print("=" * 70)
    print("PECD v4.2 Climate Variable Downloader")
    print("Dataset: sis-energy-pecd (Copernicus CDS)")
    print("=" * 70)

    # Initialise the CDS API client (reads credentials from ~/.cdsapirc)
    try:
        client = cdsapi.Client()
    except Exception as error:
        print(f"\n[FATAL] Could not initialise CDS API client: {error}")
        print("Make sure your ~/.cdsapirc file is configured with:")
        print("  url: https://cds.climate.copernicus.eu/api")
        print("  key: <YOUR-API-KEY>")
        print("\nGet your API key from: https://cds.climate.copernicus.eu/how-to-api")
        sys.exit(1)

    # Count total download jobs for progress display
    historical_year_chunks = len(chunk_years(HISTORICAL_YEARS, CHUNK_SIZE_YEARS))
    projection_year_chunks = len(chunk_years(PROJECTION_YEARS, CHUNK_SIZE_YEARS))

    historical_jobs = len(CLIMATE_VARIABLES) * len(SPATIAL_LEVELS) * historical_year_chunks
    projection_jobs = (
        len(CLIMATE_VARIABLES) * len(CLIMATE_MODELS)
        * len(EMISSION_SCENARIOS) * len(PROJECTION_SPATIAL_LEVELS) * projection_year_chunks
    )
    total_jobs = historical_jobs + projection_jobs

    print(f"\nPlanned downloads:")
    print(f"  Historical:  {historical_jobs} files "
          f"({len(CLIMATE_VARIABLES)} vars × {len(SPATIAL_LEVELS)} levels × {historical_year_chunks} decade-chunks)")
    print(f"  Projections: {projection_jobs} files "
          f"({len(CLIMATE_VARIABLES)} vars × {len(CLIMATE_MODELS)} models × "
          f"{len(EMISSION_SCENARIOS)} scenarios × {len(PROJECTION_SPATIAL_LEVELS)} levels × {projection_year_chunks} decade-chunks)")
    print(f"  Total:       {total_jobs} files")
    print(f"\nOutput directory: {OUTPUT_BASE}/")
    print(f"Year chunk size: {CHUNK_SIZE_YEARS} years per request")
    print(f"Existing files will be skipped (resume-friendly).\n")

    # Download both periods
    download_historical(client)
    download_projections(client)

    print("\n" + "=" * 70)
    print("All downloads complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()
