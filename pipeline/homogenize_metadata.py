#!/usr/bin/env python3
"""
homogenize_metadata.py — Homogenize the metadata (properties) across all 6 GeoJSON layers
for the PowerClimate Vision Shiny app:
  - pecd_PEON.geojson
  - pecd_PEOF.geojson
  - pecd_SZON.geojson
  - pecd_SZOF.geojson
  - pecd_NUT0.geojson
  - pecd_NUT2.geojson

Ensures all 6 files share a unified schema:
  1. zone_id (string)      — Primary identifier (e.g., FR, CZ05, FR00, FR01_OFF)
  2. name (string)         — High-quality, human-readable display name
  3. parent_zone (string)  — Parent identifier (e.g., country code or bidding zone, or null)
  4. level (string)        — Level tag (NUT0, NUT2, PEON, PEOF, SZON, SZOF)
  5. area_km2 (float)      — Area in sq. km (retains PECD values; calculated/null for NUTS)
"""

import argparse
import json
from pathlib import Path

# Country prefix to name mappings for European and Mediterranean countries in PECD v4.2
COUNTRY_NAMES = {
    'AL': 'Albania', 'AT': 'Austria', 'BA': 'Bosnia and Herzegovina', 'BE': 'Belgium', 'BG': 'Bulgaria',
    'CH': 'Switzerland', 'CY': 'Cyprus', 'CZ': 'Czechia', 'DE': 'Germany', 'DK': 'Denmark',
    'DZ': 'Algeria', 'EE': 'Estonia', 'EG': 'Egypt', 'ES': 'Spain', 'FI': 'Finland',
    'FR': 'France', 'GR': 'Greece', 'HR': 'Croatia', 'HU': 'Hungary', 'IE': 'Ireland',
    'IL': 'Israel', 'IS': 'Iceland', 'IT': 'Italy', 'JO': 'Jordan', 'LB': 'Lebanon',
    'LT': 'Lithuania', 'LU': 'Luxembourg', 'LV': 'Latvia', 'LY': 'Libya', 'MA': 'Morocco',
    'MD': 'Moldova', 'ME': 'Montenegro', 'MK': 'North Macedonia', 'MT': 'Malta', 'NL': 'Netherlands',
    'NO': 'Norway', 'PL': 'Poland', 'PS': 'Palestine', 'PT': 'Portugal', 'RO': 'Romania',
    'RS': 'Serbia', 'SE': 'Sweden', 'SI': 'Slovenia', 'SK': 'Slovakia', 'SY': 'Syria',
    'TN': 'Tunisia', 'TR': 'Turkey', 'UA': 'Ukraine', 'UK': 'United Kingdom'
}

SPECIAL_ZONES = {
    # Denmark
    'DKE1': 'Denmark East (Sjælland)',
    'DKW1': 'Denmark West (Jylland/Fyn)',
    # Italy
    'ITCA': 'Italy Calabria',
    'ITCN': 'Italy Centre-North',
    'ITCS': 'Italy Centre-South',
    'ITN1': 'Italy North',
    'ITS1': 'Italy South',
    'ITSA': 'Italy Sardinia',
    'ITSI': 'Italy Sicily',
    # Norway
    'NOM1': 'Norway Midt-Norge (NO3)',
    'NON1': 'Norway Nord-Norge (NO4)',
    'NOS1': 'Norway Sør-Norge (NO1)',
    'NOS2': 'Norway Sør-Norge (NO2)',
    'NOS3': 'Norway Sør-Norge (NO5)',
    # Sweden
    'SE01': 'Sweden Luleå (SE1)',
    'SE02': 'Sweden Sundsvall (SE2)',
    'SE03': 'Sweden Stockholm (SE3)',
    'SE04': 'Sweden Malmö (SE4)',
    # Ukraine
    'UA01': 'Ukraine Integrated Power System',
    'UA02': 'Ukraine Burshtyn Island',
    # UK
    'UK00': 'United Kingdom Great Britain',
    'UKNI': 'United Kingdom Northern Ireland'
}

def filter_geometry_by_bbox(geom, min_lon, min_lat, max_lon, max_lat):
    """
    Filters a GeoJSON geometry (Polygon or MultiPolygon) to only keep
    polygons/sub-polygons that fall entirely or mostly within the bounding box.
    """
    if not geom:
        return None
    geom_type = geom.get("type")
    coords = geom.get("coordinates", [])
    
    if geom_type == "Polygon":
        if not coords:
            return geom
        ext_ring = coords[0]
        lons = [pt[0] for pt in ext_ring]
        lats = [pt[1] for pt in ext_ring]
        if not lons or not lats:
            return geom
        mean_lon = sum(lons) / len(lons)
        mean_lat = sum(lats) / len(lats)
        if min_lon <= mean_lon <= max_lon and min_lat <= mean_lat <= max_lat:
            return geom
        else:
            return None
            
    elif geom_type == "MultiPolygon":
        new_polygons = []
        for poly in coords:
            if not poly:
                continue
            ext_ring = poly[0]
            lons = [pt[0] for pt in ext_ring]
            lats = [pt[1] for pt in ext_ring]
            if not lons or not lats:
                continue
            mean_lon = sum(lons) / len(lons)
            mean_lat = sum(lats) / len(lats)
            if min_lon <= mean_lon <= max_lon and min_lat <= mean_lat <= max_lat:
                new_polygons.append(poly)
        if new_polygons:
            geom["coordinates"] = new_polygons
            return geom
        else:
            return None
            
    return geom

def get_country_name(zone_id):
    """Resolve a high-quality country or region name based on code prefix."""
    # Clean OFF suffix first
    base_id = zone_id.replace("_OFF", "")
    
    # Check special bidding zones first
    if base_id in SPECIAL_ZONES:
        return SPECIAL_ZONES[base_id]
        
    prefix = base_id[:2]
    return COUNTRY_NAMES.get(prefix, prefix)

def generate_display_name(zone_id, level):
    """Generate a highly professional, human-readable name for any level code."""
    country_name = get_country_name(zone_id)
    
    if level == "NUT0":
        return country_name
        
    elif level == "NUT2":
        # Keep the official Eurostat name, will be set from source data
        return None
        
    elif level == "PEON":
        # Onshore Pan-European Nodes (fine-grained, per Code)
        # e.g., FR01 -> "France Node 01"
        region_part = zone_id[2:]
        return f"{country_name} Node {region_part}"
        
    elif level == "PEOF":
        # Offshore Pan-European Nodes (fine-grained, per Code)
        # e.g., FR01_OFF -> "France Offshore Node 01"
        base_id = zone_id.replace("_OFF", "")
        region_part = base_id[2:]
        return f"{country_name} Offshore Node {region_part}"
        
    elif level == "SZON":
        # Onshore Study/Bidding Zones (coarse, dissolved by Study_Zone)
        if zone_id.endswith("00"):
            return f"{country_name} Bidding Zone"
        # Special bidding zone names are already resolved in country_name
        if zone_id in SPECIAL_ZONES:
            return f"{country_name} Bidding Zone"
        return f"{country_name} Bidding Zone ({zone_id})"
        
    elif level == "SZOF":
        # Offshore Study/Bidding Zones (coarse, dissolved by Study_Zone)
        base_id = zone_id.replace("_OFF", "")
        if base_id.endswith("00"):
            return f"{country_name} Offshore Bidding Zone"
        if base_id in SPECIAL_ZONES:
            return f"{country_name} Offshore Bidding Zone"
        return f"{country_name} Offshore Bidding Zone ({base_id})"
        
    return zone_id

def homogenize_file(filepath, level):
    print(f"\nProcessing {filepath.name} ({level})...")
    if not filepath.exists():
        print(f"  Warning: File does not exist at {filepath}")
        return False
        
    with open(filepath, "r") as f:
        data = json.load(f)
        
    features = data.get("features", [])
    homogenized_features = []
    
    for feature in features:
        props = feature.get("properties", {})
        
        # 1. zone_id (primary key)
        zone_id = props.get("zone_id") or props.get("Code") or props.get("Study_Zone") or props.get("NUTS_ID") or props.get("id")
        if not zone_id:
            continue
            
        # Filter out overseas regions that are not addressed in the PECD dataset
        # This includes French DOMs (FRY), Canary Islands (ES7), Azores (PT2), Madeira (PT3), Svalbard (NO0B)
        if zone_id.startswith(('FRY', 'ES7', 'PT2', 'PT3', 'NO0B')):
            print(f"  Skipping overseas/unsupported zone: {zone_id}")
            continue
            
        # Geometrical filtering for parent countries in NUT0 to exclude overseas territories
        if level == "NUT0" and zone_id in ["FR", "ES", "PT", "NO"]:
            bboxes = {
                "FR": (-10, 41, 12, 52),      # France (mainland + Corsica)
                "ES": (-11, 34, 5, 44),       # Spain (mainland + Balearics)
                "PT": (-11, 36, -6, 43),      # Portugal (mainland)
                "NO": (-10, 50, 35, 72)       # Norway (mainland)
            }
            min_lon, min_lat, max_lon, max_lat = bboxes[zone_id]
            filtered_geom = filter_geometry_by_bbox(feature.get("geometry"), min_lon, min_lat, max_lon, max_lat)
            if filtered_geom:
                feature["geometry"] = filtered_geom
            else:
                print(f"  Warning: Entire geometry for {zone_id} was filtered out!")
            
        # 2. name
        # If NUT2 or NUT0, preserve official name if available
        name = props.get("name") or props.get("NUTS_NAME") or props.get("NAME_LATN")
        if not name or level not in ["NUT0", "NUT2"]:
            name = generate_display_name(zone_id, level)
            
        # 3. parent_zone
        parent_zone = props.get("parent_zone") or props.get("parent") or props.get("Study_Zone")
        if not parent_zone:
            if level == "NUT2":
                # For NUTS2, parent is the country code (first 2 chars of ID)
                parent_zone = zone_id[:2]
            elif level in ["PEON", "PEOF"]:
                # For fine-grained nodes, parent is the Study_Zone (bidding zone) from initial prep
                parent_zone = props.get("parent_zone")
                if not parent_zone:
                    suffix = "_OFF" if "_OFF" in zone_id else ""
                    parent_zone = f"{zone_id[:2]}00{suffix}"
            else:
                parent_zone = None
                
        # 4. area_km2 (float)
        area_km2 = props.get("area_km2") or props.get("Area_km2")
        if area_km2 is not None:
            try:
                area_km2 = round(float(area_km2), 2)
            except ValueError:
                area_km2 = None
                
        # Construct unified property schema
        new_props = {
            "zone_id": zone_id,
            "name": name,
            "parent_zone": parent_zone,
            "level": level,
            "area_km2": area_km2
        }
        
        feature["properties"] = new_props
        homogenized_features.append(feature)
        
    data["features"] = homogenized_features
    
    with open(filepath, "w") as f:
        json.dump(data, f, separators=(",", ":"))
        
    print(f"  Successfully homogenized {len(homogenized_features)} features.")
    return True

def main():
    parser = argparse.ArgumentParser(
        description="Homogenize GeoJSON feature properties for the Shiny app."
    )
    parser.add_argument(
        "--suffix",
        default="",
        help="Optional filename suffix before .geojson, for example 40 for pecd_PEON40.geojson.",
    )

    args = parser.parse_args()

    project_root = Path(__file__).resolve().parent.parent
    geo_dir = project_root / "www/data/geo"

    suffix = args.suffix
    
    layers = [
        ("pecd_NUT0.geojson", "NUT0"),
        ("pecd_NUT2.geojson", "NUT2"),
        (f"pecd_PEON{suffix}.geojson", "PEON"),
        (f"pecd_PEOF{suffix}.geojson", "PEOF"),
        (f"pecd_SZON{suffix}.geojson", "SZON"),
        (f"pecd_SZOF{suffix}.geojson", "SZOF")
    ]

    if suffix:
        layers = layers[2:]
    
    success_count = 0
    for filename, level in layers:
        filepath = geo_dir / filename
        if homogenize_file(filepath, level):
            success_count += 1
            
    print(f"\n==========================================")
    print(f"Metadata Homogenization Complete: {success_count}/{len(layers)} files")
    print(f"==========================================")

if __name__ == "__main__":
    main()
