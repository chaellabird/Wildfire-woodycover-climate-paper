import arcpy
from pathlib import *
scriptDir = Path.cwd()
parent = Path.cwd().parent
arcpy.env.workspace = str(parent / "Data"/ "Chapter2_revisions.gdb")
arcpy.env.overwriteOutput = True
grandparent = parent.parent

sf = str(parent / "Output"/ "L1_points_with_multiple_dates_hay_crop_filtered.shp") #this is the shapefile we got from python script 4.
for field in arcpy.ListFields(sf):
    print(field.name, field.type)
dates_key = str(parent/ "Output"/ "Dates_precip_filtered_rdm_keep_crophayfiltered.csv")
csv_file= dates_key

import csv
from collections import Counter

def _normalize(val):

    if isinstance(val, int):
        return val

    if isinstance(val, float):
        # keep the float if it really has a fraction; otherwise truncate
        return int(val) if val.is_integer() else val

    if isinstance(val, str):
        s = val.strip()
        if not s:
            return s  # empty string stays empty

        try:
            # Try int first (covers plain numbers)
            return int(s)
        except ValueError:
            # Fallback to float
            try:
                return float(s)
            except ValueError:
                return s  # non‑numeric string

    # Fallback – everything else is converted to a string
    return str(val)


# ---------- 2.  Helper: normalise an Event_ID ---------------------------
def _norm_id(val):
    """Convert an arbitrary Event_ID to a single, comparable form."""
    # If it's already an int or long, keep it
    if isinstance(val, (int,)):
        return val

    # If it's a float that comes from a field type that doesn't store decimals,
    # round to int before returning
    if isinstance(val, float):
        return int(round(val))

    # If it's a string, strip whitespace and try int(); fall back to the raw string
    if isinstance(val, str):
        stripped = val.strip()
        try:
            # works for "005" -> 5 and "123.0" -> 123
            return int(float(stripped))
        except ValueError:
            # non‑numeric string stays as string
            return stripped

    # Fallback: any other type is converted to string
    return str(val)

# ---------- 3.  Read & normalise from shapefile ------------------------
shp_ids = Counter()
with arcpy.da.SearchCursor(sf, ["Event_ID"]) as cursor:
    for (event_id,) in cursor:
        shp_ids[_norm_id(event_id)] += 1

# ---------- 4.  Read & normalise from CSV -----------------------------
csv_ids = Counter()
with open(csv_file, newline='', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    for row in reader:
        csv_ids[_norm_id(row["Event_ID"])] += 1

# ---------- 5.  Analyze --------------------------------------------------

unique_shp  = set(shp_ids.keys())
unique_csv  = set(csv_ids.keys())
common_ids  = unique_shp & unique_csv

print(f"Unique IDs in shapefile      : {len(unique_shp)}")
print(f"Unique IDs in CSV             : {len(unique_csv)}")
print(f"IDs that appear in BOTH sets : {len(common_ids)}")

print("\n--- Example of matching / non‑matching IDs ---")
print(f"Example shapefile‑only ID  : {next(iter(unique_shp - common_ids), None)}")
print(f"Example CSV‑only ID        : {next(iter(unique_csv - common_ids), None)}")
print(f"Example common ID          : {next(iter(common_ids), None)}")

# If you want to know how many *records* match (not just unique IDs), you can intersect the counters:
matched_records = sum(shp_ids[id_] * csv_ids[id_] for id_ in common_ids)
print(f"\nTotal matched rows (shapefile × CSV for each ID): {matched_records}")

#I now know that they have the same event ids, just in different datatypes



print("creating a dictionary from the dates key")
dates_dict = {}
with arcpy.da.SearchCursor(dates_key, ["Event_ID","ignition_id"]) as cursor:
    for row in cursor:
        dates_dict[row[0]] = row[1]
date_pairs = {(eid, iid) for eid, iid in dates_dict.items()}
print("some samples of the date pairs")
print(date_pairs)

pivoted_sf= "pivoted_fc_rdmpoints_haycropfiltered"
#arcpy.management.CopyFeatures(sf, pivoted_sf)
#rather than copying, create a NEW blank FEATURE.


spatial_reference = arcpy.Describe(sf).spatialReference

# Create the empty feature class
arcpy.management.CreateFeatureclass(arcpy.env.workspace, pivoted_sf, "POLYGON",
                                    template=None, has_m="DISABLED", has_z="DISABLED",
                                    spatial_reference=spatial_reference)


print("Pivoting the sf data to be more compatible with my knowledge")
# 2. Add the two new fields that will hold the pivoted data
     # the unpivoted date
arcpy.management.AddField(pivoted_sf, "NA_L3NAME", "TEXT")
arcpy.management.AddField(pivoted_sf, "Event_ID", "LONG")
arcpy.management.AddField(pivoted_sf, "area_ha", "FLOAT")
arcpy.management.AddField(pivoted_sf, "ignition_id", "SHORT")   # 1–5
arcpy.management.AddField(pivoted_sf, "Ig_Date", "DATE")
# --------------------------------------------------------------------
# 3. Prepare the list of fields that we will read from the source
date_fields = [f"Ig_Date_{i}" for i in range(1, 6)]          # Ig_Date_1 … Ig_Date_5
in_fields = ["SHAPE@","Event_ID","NA_L3NAME", "area_ha", *date_fields]                        # geometry +Event_ID+ 5 dates

# 4. Open a search cursor on the source and an insert cursor on the copy
with arcpy.da.SearchCursor(sf, in_fields) as sc, \
     arcpy.da.InsertCursor(pivoted_sf, ["SHAPE@","NA_L3NAME","area_ha","Event_ID", "ignition_id", "Ig_Date"]) as ic:

    for row in sc:
        shape = row[0]          # geometry stays the same
        event_id = row[1]
        NA_L3NAME = row[2]
        area_ha = row[3]
        for idx, date in enumerate(row[4:], start=1):   # idx = 1..5
            if date:                        # skip empty dates
                ic.insertRow([shape,NA_L3NAME, area_ha, event_id, idx, date])

print("Pivoting complete.")
for field in arcpy.ListFields(pivoted_sf):
    print(field.name)

counter = 0
for row in arcpy.da.SearchCursor(pivoted_sf, ["Event_ID","ignition_id"]):
    print(row)
    counter +=1
    if counter >10:
        break

#at this point it looks good.

with arcpy.da.UpdateCursor(pivoted_sf, ["Event_ID", "ignition_id"]) as cur:
    for row in cur:
        event_id, ign_id = row
        print(type(event_id), type(ign_id))
        break

print("preparing to filter out any Event_ID-idx combinations not found in the key")
output = str(parent/"Output"/ "Shapefiles"/ "Randompoints_restricted_dates_nlcd_gotime_2.shp")

counter = 0
with arcpy.da.UpdateCursor(pivoted_sf, ["Event_ID","ignition_id"]) as cursor:
    for row in cursor:
        event_id, ign_id = row #cool that I can assign everything at once.
        norm_event_id = _normalize(event_id)
        norm_ign_id = _normalize(ign_id)
        if not (norm_event_id, norm_ign_id) in date_pairs:
            cursor.deleteRow()
        else:
            # The pair is missing – maybe clear the field or flag it
            counter +=1

for field in arcpy.ListFields(pivoted_sf):
    print(field.name)
counter_takealook = 0
for row in arcpy.da.SearchCursor(pivoted_sf,"*"):
    print(row)
    counter_takealook +=1
    if counter_takealook >10:
        break


print(f"we need 2277 pairs of dates and ignitions and we have {counter} pairs of dates and ignitions")

print("exporting the filtered shapefile to Output...")


arcpy.management.CopyFeatures(in_features = pivoted_sf, out_feature_class= output)
print("Script Finished. Go extract gridmet and woody cover data from earth engine.")
print("Then to go R script 3. Create Presence Absence dataframe.")

