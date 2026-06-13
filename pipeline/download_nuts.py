#!/usr/bin/env python3
"""
download_nuts.py — Download official NUTS 2021 boundaries from Eurostat GISCO,
process them to align with our Shiny app zone format, and save them.

Reads from Eurostat GISCO REST API:
  - NUTS 2021 Level 0 (Country)
  - NUTS 2021 Level 2 (Provinces/Regions)

Saves to:
  - www/data/geo/pecd_NUT0.geojson
  - www/data/geo/pecd_NUT2.geojson
"""

import json
import urllib.request
import os
from pathlib import Path

# URL for official Eurostat GISCO NUTS 2021 GeoJSON at 03M scale (1:3 million) and WGS84 CRS (4326)
# This provides highly detailed and sharp boundaries compared to the 20M (1:20 million) scale.
NUTS_LEVL_0_URL = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_03M_2021_4326_LEVL_0.geojson"
NUTS_LEVL_2_URL = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_03M_2021_4326_LEVL_2.geojson"

OUTPUT_DIR = "www/data/geo"
COORD_PRECISION = 5

def round_coordinates(geojson_dict, precision=5):
    """Round coordinates in the GeoJSON to reduce file size."""
    def round_coords(coords):
        if isinstance(coords[0], (int, float)):
            return [round(c, precision) for c in coords]
        return [round_coords(c) for c in coords]
    
    for feature in geojson_dict.get("features", []):
        geom = feature.get("geometry", {})
        if "coordinates" in geom:
            geom["coordinates"] = round_coords(geom["coordinates"])
    
    return geojson_dict

def process_nuts_geojson(url, output_path, level_name):
    print(f"Downloading official NUTS {level_name} from: {url}")
    try:
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode('utf-8'))
    except Exception as e:
        print(f"Error downloading {level_name}: {e}")
        return False
        
    print(f"Processing NUTS {level_name}...")
    
    # Process features: standardize property names to match our schema (zone_id, name)
    processed_features = []
    for feature in data.get("features", []):
        props = feature.get("properties", {})
        
        # Check standard Eurostat property names
        nuts_id = props.get("NUTS_ID") or props.get("id")
        nuts_name = props.get("NUTS_NAME") or props.get("NAME_LATN")
        
        if not nuts_id:
            continue
            
        new_props = {
            "zone_id": nuts_id,
            "name": nuts_name,
            "country_code": props.get("CNTR_CODE"),
            "level": props.get("LEVL_CODE")
        }
        
        # Maintain other metadata if helpful
        feature["properties"] = new_props
        processed_features.append(feature)
        
    data["features"] = processed_features
    
    # Round coordinates to minimize footprint
    data = round_coordinates(data, COORD_PRECISION)
    
    print(f"Saving processed NUTS {level_name} to: {output_path}")
    with open(output_path, "w") as f:
        json.dump(data, f, separators=(",", ":"))
        
    size_mb = os.path.getsize(output_path) / (1024 * 1024)
    print(f"  Successfully wrote {output_path.name} ({size_mb:.2f} MB, {len(processed_features)} regions)")
    return True

def main():
    project_root = Path(__file__).resolve().parent.parent
    out_dir = project_root / OUTPUT_DIR
    out_dir.mkdir(parents=True, exist_ok=True)
    
    success_0 = process_nuts_geojson(NUTS_LEVL_0_URL, out_dir / "pecd_NUT0.geojson", "Level 0 (NUT0)")
    success_2 = process_nuts_geojson(NUTS_LEVL_2_URL, out_dir / "pecd_NUT2.geojson", "Level 2 (NUT2)")
    
    if success_0 and success_2:
        print("\nAll NUTS files successfully sourced and placed!")
        print("Total files in geo directory:")
        for f in sorted(out_dir.glob("*.geojson")):
            sz = f.stat().st_size / (1024 * 1024)
            print(f"  {f.name:25s}  {sz:6.2f} MB")
    else:
        print("\nSome NUTS files failed to process.")

if __name__ == "__main__":
    main()
