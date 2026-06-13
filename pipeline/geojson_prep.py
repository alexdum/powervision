#!/usr/bin/env python3
"""
geojson_prep.py — Convert PECD shapefiles into simplified GeoJSONs for Shiny app.

Reads:  PECD Zones/ShapeFiles/PECD42_v2_final.shp
Writes: www/data/geo/pecd_{PEON,PEOF,SZON,SZOF}.geojson

Optional PECD40 run:
Reads:  data/PECD40-Polygons_20230127/DTU-PECD40-Polygons_VF20230127.shp
Writes: www/data/geo/pecd_{PEON,PEOF,SZON,SZOF}40.geojson

Zone hierarchy in the shapefile:
  - Code       → fine-grained Pan-European Nodes (PEON onshore, PEOF offshore)
  - Study_Zone → coarser ENTSO-E Study/Bidding Zones (SZON onshore, SZOF offshore)
  - _OFF suffix distinguishes offshore from onshore

Naming follows the Copernicus CDS convention (sis-energy-pecd spatial_aggregation):
  PEON  = onshore polygons, one per Code (fine-grained nodes, no dissolve)
  PEOF  = offshore polygons, one per Code (fine-grained nodes, no dissolve)
  SZON  = onshore polygons dissolved by Study_Zone (coarser bidding zones)
  SZOF  = offshore polygons dissolved by Study_Zone (coarser bidding zones)
"""

import argparse
import geopandas as gpd
import json
import os
import sys
import warnings
from pathlib import Path


# --- Configuration -----------------------------------------------------------

# Relative to the project root.
PECD42_SHAPEFILE = "data/PECD Zones/ShapeFiles/PECD42_v2_final.shp"
PECD40_SHAPEFILE = "data/PECD40-Polygons_20230127/DTU-PECD40-Polygons_VF20230127.shp"
PECD40_STUDY_ZONE_SHAPEFILE = "data/PECD40-Polygons_20230127/DTU-PECD40-Polygons_SZ_VF20230127.shp"
OUTPUT_DIR = "www/data/geo"

# Simplification tolerance in degrees (~0.01° ≈ 1 km at mid-latitudes)
# Balances file size vs. visual fidelity for MapLibre rendering
SIMPLIFY_TOLERANCE = 0.005  # conservative: preserves coastal detail

# Coordinate precision in the GeoJSON output (decimal places)
COORD_PRECISION = 5  # ~1m precision, sufficient for dashboard maps


# --- Helper functions ---------------------------------------------------------

def classify_zones(gdf):
    """Split GeoDataFrame into onshore and offshore based on _OFF suffix."""
    is_offshore = gdf["Code"].str.contains("_OFF", na=False)
    onshore = gdf[~is_offshore].copy()
    offshore = gdf[is_offshore].copy()
    return onshore, offshore


def read_shapefile_with_repair(shp_path):
    """Read a shapefile and ask Shapely to repair fixable invalid rings.

    The PECD40 polygon shapefile has one unclosed ring. Without on_invalid="fix",
    the read fails before we can repair or replace the geometry.
    """
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", message="Non closed ring detected")
        gdf = gpd.read_file(shp_path, on_invalid="fix")

    return gdf


def standardize_column_names(gdf):
    """Use the PECD42 column names for all PECD versions.

    PECD42 uses Study_Zone. PECD40 uses Study Zone. The rest of the script keeps
    one spelling so the workflow stays the same for both vintages.
    """
    gdf = gdf.copy()

    if "Study Zone" in gdf.columns and "Study_Zone" not in gdf.columns:
        gdf = gdf.rename(columns={"Study Zone": "Study_Zone"})

    return gdf


def repair_invalid_geometries(gdf):
    """Repair invalid geometries that are present in the shapefile."""
    gdf = gdf.copy()

    has_geometry = ~gdf["geometry"].isna()
    invalid_geometry = has_geometry & ~gdf.is_valid
    invalid_count = invalid_geometry.sum()

    if invalid_count > 0:
        print(f"  Repairing {invalid_count} invalid geometries...")
        gdf.loc[invalid_geometry, "geometry"] = gdf.loc[invalid_geometry, "geometry"].make_valid()

    return gdf


def fill_missing_geometries_from_study_zones(gdf, study_zone_shp_path):
    """Fill missing PECD40 geometries from the matching study-zone shapefile.

    In the PECD40 fine-grained polygon shapefile, LY00_OFF is present as a row
    but its geometry cannot be read because of an unclosed ring. The companion
    Study Zone shapefile has a valid LY00_OFF polygon, so we use it when the
    Code and Study_Zone are the same zone.
    """
    missing_geometry = gdf["geometry"].isna()
    missing_count = missing_geometry.sum()

    if missing_count == 0:
        return gdf

    print(f"  Filling {missing_count} missing geometries from PECD40 Study Zone shapefile...")

    study_zones = read_shapefile_with_repair(study_zone_shp_path)
    study_zones = standardize_column_names(study_zones)
    study_zones = repair_invalid_geometries(study_zones)

    geometry_by_study_zone = {}
    for _, study_zone_row in study_zones.iterrows():
        study_zone_id = study_zone_row["Study_Zone"]
        geometry_by_study_zone[study_zone_id] = study_zone_row.geometry

    filled_count = 0
    for row_number, zone_row in gdf[missing_geometry].iterrows():
        code = zone_row["Code"]
        study_zone = zone_row["Study_Zone"]

        if code == study_zone and study_zone in geometry_by_study_zone:
            gdf.at[row_number, "geometry"] = geometry_by_study_zone[study_zone]
            filled_count = filled_count + 1

    remaining_missing_count = gdf["geometry"].isna().sum()
    print(f"  Filled geometries: {filled_count}")

    if remaining_missing_count > 0:
        missing_zone_ids = sorted(gdf.loc[gdf["geometry"].isna(), "Code"].astype(str).tolist())
        missing_zone_text = ", ".join(missing_zone_ids)
        raise ValueError(f"Could not fill geometries for: {missing_zone_text}")

    return gdf


def add_area_km2_if_missing(gdf):
    """Calculate area_km2 when a source shapefile does not provide it.

    PECD42 already includes Area_km2. PECD40 does not, so this calculates area
    in a global equal-area projection and stores it in the same column name.
    """
    if "Area_km2" in gdf.columns:
        return gdf

    print("  Calculating Area_km2 from geometry...")

    gdf = gdf.copy()
    original_crs = gdf.crs

    if original_crs is None:
        gdf = gdf.set_crs(epsg=4326)

    equal_area_gdf = gdf.to_crs(epsg=6933)
    gdf["Area_km2"] = equal_area_gdf.area / 1_000_000

    return gdf


def simplify_geometries(gdf, tolerance):
    """Simplify geometries preserving topology."""
    gdf = gdf.copy()
    gdf["geometry"] = gdf["geometry"].simplify(tolerance, preserve_topology=True)
    return gdf


def dissolve_to_bidding_zones(gdf, zone_col="Study_Zone"):
    """Dissolve fine-grained zones into bidding zones by Study_Zone.
    
    Aggregates Area_km2 (sum) and drops other numeric columns
    since they don't aggregate meaningfully after dissolve.
    """
    # Keep only the columns we need
    gdf_slim = gdf[["Study_Zone", "Area_km2", "geometry"]].copy()
    
    # Repair invalid geometries (fixes TopologyException during dissolve)
    gdf_slim = repair_invalid_geometries(gdf_slim)
    
    dissolved = gdf_slim.dissolve(
        by=zone_col,
        aggfunc={"Area_km2": "sum"}
    ).reset_index()
    
    # Rename Study_Zone to zone_id for consistency
    dissolved = dissolved.rename(columns={"Study_Zone": "zone_id", "Area_km2": "area_km2"})
    
    return dissolved


def prepare_study_zones(gdf):
    """Prepare fine-grained study zone GeoJSON.
    
    Keeps Code as the zone identifier and Study_Zone as the parent reference.
    """
    gdf_out = gdf[["Code", "Study_Zone", "Area_km2", "geometry"]].copy()
    gdf_out = gdf_out.rename(columns={
        "Code": "zone_id",
        "Study_Zone": "parent_zone",
        "Area_km2": "area_km2"
    })
    return gdf_out


def round_coordinates(geojson_dict, precision=5):
    """Round all coordinates in a GeoJSON dict to reduce file size."""
    def round_coords(coords):
        if isinstance(coords[0], (int, float)):
            return [round(c, precision) for c in coords]
        return [round_coords(c) for c in coords]
    
    for feature in geojson_dict.get("features", []):
        geom = feature.get("geometry", {})
        if "coordinates" in geom:
            geom["coordinates"] = round_coords(geom["coordinates"])
    
    return geojson_dict


def write_geojson(gdf, filepath, precision=5):
    """Write GeoDataFrame to GeoJSON with coordinate rounding."""
    # Ensure CRS is WGS84
    if gdf.crs and gdf.crs.to_epsg() != 4326:
        gdf = gdf.to_crs(epsg=4326)
    
    # Convert to GeoJSON dict, round, and write
    geojson_str = gdf.to_json()
    geojson_dict = json.loads(geojson_str)
    geojson_dict = round_coordinates(geojson_dict, precision)
    
    with open(filepath, "w") as f:
        json.dump(geojson_dict, f, separators=(",", ":"))  # compact output
    
    # Report file size
    size_mb = os.path.getsize(filepath) / (1024 * 1024)
    return size_mb


def build_output_path(out_dir, zone_type, filename_suffix):
    """Create the output path for one GeoJSON layer."""
    return out_dir / f"pecd_{zone_type}{filename_suffix}.geojson"


def prepare_source_data(shp_path, study_zone_shp_path=None):
    """Read, standardize, repair, and enrich the source polygon data."""
    gdf = read_shapefile_with_repair(shp_path)
    gdf = standardize_column_names(gdf)

    required_columns = {"Code", "Study_Zone", "geometry"}
    missing_columns = required_columns - set(gdf.columns)
    if missing_columns:
        missing_text = ", ".join(sorted(missing_columns))
        raise ValueError(f"Missing required columns: {missing_text}")

    if study_zone_shp_path is not None:
        gdf = fill_missing_geometries_from_study_zones(gdf, study_zone_shp_path)

    gdf = repair_invalid_geometries(gdf)
    gdf = add_area_km2_if_missing(gdf)

    return gdf


# --- Main --------------------------------------------------------------------

def write_layer(gdf, out_dir, zone_type, filename_suffix):
    """Write one GeoJSON layer and report its size."""
    filepath = build_output_path(out_dir, zone_type, filename_suffix)
    size = write_geojson(gdf, filepath, COORD_PRECISION)
    print(f"  Written: {filepath.name} ({size:.1f} MB)")


def run_geojson_prep(shapefile, output_suffix="", study_zone_shapefile=None, label="PECD42"):
    project_root = Path(__file__).resolve().parent.parent
    shp_path = project_root / shapefile
    out_dir = project_root / OUTPUT_DIR
    study_zone_shp_path = None

    if study_zone_shapefile is not None:
        study_zone_shp_path = project_root / study_zone_shapefile
    
    print(f"Preparing {label} GeoJSON files")
    print(f"Reading shapefile: {shp_path}")
    gdf = prepare_source_data(shp_path, study_zone_shp_path)
    print(f"  Total features: {len(gdf)}")
    print(f"  CRS: {gdf.crs}")
    print(f"  Columns: {list(gdf.columns)}")
    
    # --- Split into onshore / offshore ---
    onshore, offshore = classify_zones(gdf)
    print(f"\n  Onshore zones (no _OFF): {len(onshore)}")
    print(f"  Offshore zones (_OFF):   {len(offshore)}")
    
    # --- Simplify geometries ---
    print(f"\nSimplifying geometries (tolerance={SIMPLIFY_TOLERANCE}°)...")
    onshore_s = simplify_geometries(onshore, SIMPLIFY_TOLERANCE)
    offshore_s = simplify_geometries(offshore, SIMPLIFY_TOLERANCE)
    
    # --- Generate 4 GeoJSONs ---
    # Naming follows the Copernicus CDS convention for sis-energy-pecd:
    #   PEON/PEOF = fine-grained Pan-European Onshore/Offshore Nodes (per Code)
    #   SZON/SZOF = coarser Study/Bidding Zones (dissolved by Study_Zone)
    out_dir.mkdir(parents=True, exist_ok=True)
    
    # 1. PEON — onshore Pan-European Nodes (fine-grained, one per Code)
    print("\n--- PEON (onshore Pan-European Nodes, fine-grained) ---")
    peon = prepare_study_zones(onshore_s)
    print(f"  Zones: {len(peon)}")
    print(f"  Sample zone_ids: {sorted(peon['zone_id'].head(10).tolist())}")
    write_layer(peon, out_dir, "PEON", output_suffix)
    
    # 2. PEOF — offshore Pan-European Nodes (fine-grained, one per Code)
    print("\n--- PEOF (offshore Pan-European Nodes, fine-grained) ---")
    peof = prepare_study_zones(offshore_s)
    print(f"  Zones: {len(peof)}")
    print(f"  Sample zone_ids: {sorted(peof['zone_id'].head(10).tolist())}")
    write_layer(peof, out_dir, "PEOF", output_suffix)
    
    # 3. SZON — onshore Study/Bidding Zones (dissolved from PEON by Study_Zone)
    print("\n--- SZON (onshore Study/Bidding Zones, dissolved) ---")
    szon = dissolve_to_bidding_zones(onshore_s)
    print(f"  Zones: {len(szon)}")
    print(f"  Sample zone_ids: {sorted(szon['zone_id'].head(10).tolist())}")
    write_layer(szon, out_dir, "SZON", output_suffix)
    
    # 4. SZOF — offshore Study/Bidding Zones (dissolved from PEOF by Study_Zone)
    print("\n--- SZOF (offshore Study/Bidding Zones, dissolved) ---")
    szof = dissolve_to_bidding_zones(offshore_s)
    print(f"  Zones: {len(szof)}")
    print(f"  Sample zone_ids: {sorted(szof['zone_id'].head(10).tolist())}")
    write_layer(szof, out_dir, "SZOF", output_suffix)
    
    # --- Summary ---
    print("\n" + "=" * 60)
    print("Summary")
    print("=" * 60)
    total_size = 0
    for f in sorted(out_dir.glob(f"pecd_*{output_suffix}.geojson")):
        sz = f.stat().st_size / (1024 * 1024)
        total_size += sz
        print(f"  {f.name:25s}  {sz:6.2f} MB")
    print(f"  {'TOTAL':25s}  {total_size:6.2f} MB")
    print(f"\nOutput directory: {out_dir}")


def main():
    parser = argparse.ArgumentParser(
        description="Convert PECD shapefiles into simplified GeoJSONs for the Shiny app."
    )
    parser.add_argument(
        "--pecd40",
        action="store_true",
        help="Generate PECD40 GeoJSONs with 40 appended to each output filename.",
    )

    args = parser.parse_args()

    if args.pecd40:
        run_geojson_prep(
            shapefile=PECD40_SHAPEFILE,
            output_suffix="40",
            study_zone_shapefile=PECD40_STUDY_ZONE_SHAPEFILE,
            label="PECD40",
        )
    else:
        run_geojson_prep(
            shapefile=PECD42_SHAPEFILE,
            output_suffix="",
            study_zone_shapefile=None,
            label="PECD42",
        )


if __name__ == "__main__":
    main()
