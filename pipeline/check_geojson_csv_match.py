import json
import glob

def get_csv_zones(filename):
    with open(filename, 'r') as f:
        for line in f:
            if not line.startswith('#'):
                parts = line.strip().split(',')
                # Return the columns from index 1 (skip 'Date')
                return set(parts[1:])
    return set()

def get_geo_zones(filename):
    try:
        with open(filename, 'r') as f:
            data = json.load(f)
            zones = set()
            for feat in data['features']:
                prop = feat['properties']
                # Try common keys for zone
                if 'zone_id' in prop:
                    zones.add(prop['zone_id'])
                elif 'BIDDING_ZO' in prop:
                    zones.add(prop['BIDDING_ZO'])
                elif 'id' in prop:
                    zones.add(prop['id'])
                elif 'name' in prop:
                    zones.add(prop['name'])
            return zones
    except Exception as e:
        print(f"Error reading {filename}: {e}")
        return set()

csvs = glob.glob('new_csv/*.csv')
for csv_f in csvs:
    print(f"\n--- CSV: {csv_f} ---")
    csv_zones = get_csv_zones(csv_f)
    print(f"Number of zones in CSV: {len(csv_zones)}")
    
    tier = None
    if 'P2ON' in csv_f: tier = 'PEON'
    elif 'P2OF' in csv_f: tier = 'PEOF'
    elif 'SZON' in csv_f: tier = 'SZON'
    elif 'SZOF' in csv_f: tier = 'SZOF'
    
    if not tier:
        print("Could not determine tier")
        continue
        
    # check 4.2
    geo_42 = f'www/data/geo/pecd_{tier}.geojson'
    geo_42_zones = get_geo_zones(geo_42)
    match_42 = (csv_zones == geo_42_zones)
    
    print(f"GeoJSON 4.2 ({geo_42}): {len(geo_42_zones)} zones -> EXACT MATCH: {match_42}")
    if not match_42:
        diff1 = csv_zones - geo_42_zones
        diff2 = geo_42_zones - csv_zones
        if diff1: print(f"  In CSV but not in Geo 4.2: {sorted(list(diff1))[:10]}")
        if diff2: print(f"  In Geo 4.2 but not in CSV: {sorted(list(diff2))[:10]}")
        
    # check 4.0
    geo_40 = f'www/data/geo/pecd_{tier}40.geojson'
    geo_40_zones = get_geo_zones(geo_40)
    match_40 = (csv_zones == geo_40_zones)
    
    print(f"GeoJSON 4.0 ({geo_40}): {len(geo_40_zones)} zones -> EXACT MATCH: {match_40}")
    if not match_40:
        diff1 = csv_zones - geo_40_zones
        diff2 = geo_40_zones - csv_zones
        if diff1: print(f"  In CSV but not in Geo 4.0: {sorted(list(diff1))[:10]}")
        if diff2: print(f"  In Geo 4.0 but not in CSV: {sorted(list(diff2))[:10]}")
