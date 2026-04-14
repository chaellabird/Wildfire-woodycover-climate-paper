#load modules
from os.path import exists
from pathlib import *
import arcpy
import os
import random

import csv
import numpy as np

np.random.seed(42)  # Optional: for reproducible results
#set working directory
scriptDir = Path.cwd()
parent = Path.cwd().parent
arcpy.env.workspace = str(parent / "Data"/ "Chapter2_revisions.gdb")
arcpy.env.overwriteOutput = True
grandparent = parent.parent
mtbs = str(grandparent / "Pre-process Spatial Data"/"Output"/ "Unaltered_MTBS_ETF_Limited.shp")
#for field in arcpy.ListFields(mtbs):
#    print(field.name)
#make sure that this only contains wildfires
unique_types = sorted(list(set([row[0] for row in arcpy.da.SearchCursor(mtbs, ["Incid_Type"]) if row[0] is not None])))

print("Unique Types:", unique_types)

#dissolve the mtbs by event id
mtbs_layer_dissolved = "mtbs_layer_dissolved"
if not arcpy.Exists(mtbs_layer_dissolved):
    arcpy.management.Dissolve(mtbs, mtbs_layer_dissolved, "Event_ID")

print('\n' *1)
print("1. Calculate fire size")


print(f"Calculating geometry attributes for {mtbs}...")
if "Area_ha" not in arcpy.ListFields(mtbs):
    arcpy.management.AddField(mtbs, "Area_ha", "FLOAT")
    arcpy.management.CalculateGeometryAttributes(
            in_features=mtbs,
            geometry_property=[["Area_ha", "AREA_GEODESIC"]],
            area_unit="HECTARES")
    print(f"Geometry attributes calculated for {mtbs}.")
else:
    print("area_ha field already exists.")

#save these areas in a dictionary for later.
event_id_area_dict = {}
with arcpy.da.SearchCursor(mtbs,["Event_ID", "Area_ha"]) as cursor:
    for row in cursor:
        event_id = row[0]
        event_id_area_dict[event_id] = row[1]
#this should look like this
print("it should look like: 'FL1117372 : 527,' ")
# Print the first entry (which will be a tuple of (key, value))
print(next(iter(event_id_area_dict.items())))

print("checking the size of the area dict. It should be the same length as the number of fires")
print(len(event_id_area_dict))
print("checking for nas...")
na_zero_counter = 0
def has_nan_or_zero(d):
    for k, v in d.items():
        if v is None:
            na_zero_counter += 1
            print(f"missing value for {k}")
            return True


        if isinstance(v, float) and np.isnan(v):
            na_zero_counter += 1
            print(f"na detected for {k}")
            return True

       # if isinstance(v, pd.NAType):
        #    return True
        if v == 0:
            na_zero_counter += 1
            print(f"0 size detected for {k}")
            return True

    return False
has_nan_or_zero(event_id_area_dict)
print(f"total number of nas and 0s = {na_zero_counter}")

#now intersect with the etf layer.
ETF = str(grandparent / "Pre-Process Spatial Data"/ "Data"/"us_eco_l3"/ "us_eco_l3.shp")

#create a nice layer to work with in the gdb
where_clause = "NA_L1CODE = '8'"
etf_layer = "etf_layer"
arcpy.management.MakeFeatureLayer(ETF, etf_layer, where_clause= where_clause)
region_key_dict= {}
with arcpy.da.SearchCursor(etf_layer,["NA_L1CODE","NA_L1NAME"]) as cursor:
    for row in cursor:
        if row[0] not in region_key_dict:
            region_key_dict[row[0]] = row[1]
print(region_key_dict)
#intersect with the etf layer





mtbs_layer = "mtbs_layer"
if not arcpy.Exists(mtbs_layer):
    arcpy.analysis.Intersect([mtbs_layer_dissolved, etf_layer], mtbs_layer)
else:
    print("intersected mtbs layer already exists")
#print("Fields in MTBS Layer")
#for field in arcpy.ListFields(mtbs_layer):
        #print(field.name)


#count the number of fields in both to make sure we correctly duplicated the ones that crossed boundaries.
mtbs_count = 0
mtbs_layer_count = 0
with arcpy.da.SearchCursor(mtbs, "*") as cursor:
    for row in cursor:
        mtbs_count += 1
with arcpy.da.SearchCursor(mtbs_layer,"*") as cursor:
    for row in cursor:
        mtbs_layer_count += 1
print(f"The original fire layer had {mtbs_count} rows and the intersected one has {mtbs_layer_count}")



region_event_id_dict = {}
with arcpy.da.SearchCursor(mtbs_layer, ["US_L3NAME", "Event_ID"]) as cursor:
    for row in cursor:
        region = row[0]
        event_id = row[1]
        #area = row[1]
        if region not in region_event_id_dict:
           region_event_id_dict[region] = [] #with the dict create an empty list for event ids.
        region_event_id_dict[region].append(event_id)
#it should look like this now:
#Southern Coastal Plain: [fl183838,AL838438]




#make a dictionary with the regions and the sizes.
area_dict = {
    region: [event_id_area_dict[e] for e in event_ids]
    for region, event_ids in region_event_id_dict.items()
}

#this should produce a dictionary that looks like this:
# Southern Coastal Plain: [571, 43.1, 75]
print("It should now look like 'Southern Coastal Plain: [571, 43.1, 75]' ")
print(next(iter(area_dict.items())))

#check for nas and missing values
print("checking for bad values in the area dict")
na_zero_counter = 0
has_nan_or_zero(area_dict)
print(f"total number of nas and 0s = {na_zero_counter}")


print('\n' * 1)
print("3 generate random points within the ETF")

counter =0
with arcpy.da.SearchCursor(etf_layer, "*") as cursor:
    for row in cursor:
        counter +=1
print(f"there are {counter} features in the etf layer. there should be 30")
#for field in arcpy.ListFields(etf_layer):
 #   print(field.name)
if not arcpy.Exists("etf_layer_dissolved"):
    if counter > 33:
        print("too many features. Dissolving first")
        arcpy.management.Dissolve(etf_layer, "etf_layer_dissolved", "NA_L3NAME")
        etf_layer = "etf_layer_dissolved"
        loop_counter = 0
        with arcpy.da.SearchCursor(etf_layer,"*") as cursor:
            for row in cursor:
                loop_counter += 1
        print(f"But now there are {loop_counter} features")
    else:
        print("its okay")
else:
    print("dissolve layer already exists")
    etf_layer = "etf_layer_dissolved"
#print("examine which regions we are looking at")
#with arcpy.da.SearchCursor(etf_layer,"*") as cursor:
#    for row in cursor:
#        print(row)
# create random points
# Check current workspace

print(f"Current workspace: {arcpy.env.workspace}")
print(f"Workspace exists: {arcpy.Exists(arcpy.env.workspace)}")

out_name = "random_points_revised"


# --- Parameters ----------------------------------------------------

# --- 1. Create a point per polygon --------------------------------
# Loop through polygons and create a fixed‑count of points in each
print("fields in the dissolved layer")
for field in arcpy.ListFields(etf_layer):
    print(field.name)

counter = 0
temp_pt_fcs = []
merged_fc = "merged_points"
if not arcpy.Exists(merged_fc):
    with arcpy.da.SearchCursor(etf_layer, ['NA_L3NAME', 'SHAPE@']) as cur:
        for oid, geom in cur:
            print(f"creating random points for the {oid} ")
            # name the temporary point feature class so we can keep them separate
            tmp_pt_fc = f"tmp_pt_{counter}"
            temp_pt_fcs.append(tmp_pt_fc)
            # Generate `num_pts_each` points inside `geom`
            arcpy.management.CreateRandomPoints(
                out_path            = arcpy.env.workspace,
                out_name            = tmp_pt_fc,
                constraining_feature_class     = geom,          # this is a Geometry object
                number_of_points_or_field    = 3000,
                minimum_allowed_distance= "5 meters"
            )
            print("points created. About to add field")
            # Add a field that records the parent polygon OID
            arcpy.AddField_management(tmp_pt_fc, "NA_L3NAME", "TEXT")
            with arcpy.da.UpdateCursor(tmp_pt_fc, ["NA_L3NAME"]) as uc:
                for row in uc:
                    row[0] = oid
                    uc.updateRow(row)
            counter +=1
            print(f"Finished polygon # {counter}")

    arcpy.Merge_management(temp_pt_fcs, merged_fc)

    print(f"All temporary point FCs merged into {merged_fc}")
else:
    print("merged fc already exists")
if not arcpy.Exists(out_name):
    arcpy.management.CopyFeatures(merged_fc, out_name)
else:
    print("points layer ready for assigning sizes")


print('\n' * 1)
print("4. Sample with replacement from the actual size distribution")
#create field for assigned area
print("Assigning areas to random points...")
fields = [f.name for f in arcpy.ListFields(out_name)]
print(f"Existing fields: {fields}")

if "area_ha" not in fields:
    arcpy.management.AddField(out_name, "area_ha", "FLOAT")
    print("Field added successfully")
else:
    print("Field 'area_ha' already exists")


#sample with replacement from values
counter =1
count_result = arcpy.management.GetCount(out_name)
total_number = int(count_result[0])   # GetCount returns a result object whose first element is the row count
print(f"The number of rows in {out_name} is {total_number}")
with arcpy.da.UpdateCursor(out_name,["area_ha", "NA_L3NAME"]) as cursor:
    for row in cursor:
        region = row[1]
        values = np.array(area_dict.get(region, None))
        if values.size == 1 and values.item() is None:
            print(f"for datapoint {counter} no available values")
            random_area = 0
            row[0] = random_area
            cursor.updateRow(row)
            counter += 1
            continue
        random_area = random.choice(values) #there is an error here.
        row[0] = random_area
        cursor.updateRow(row)
        counter +=1
    print("First five rows of Random Points:")

counter = 0
region_dict = {}

with arcpy.da.SearchCursor(out_name, ["OBJECTID", "NA_L3NAME", "area_ha"]) as cursor:
    for row in cursor:
        event_id = row[0]
        region   = row[1]
        size     = row[2]

        region_dict[event_id] = {
            "region": region,
            "area_ha": size
        }

        counter += 1
        if counter <= 5:           # print only first five rows
            print(row)




#save a key saying which event ids belong in which region
def save_to_csv(region_dict, output_file):
    # Check if there's data to save
    if not region_dict:
        print("No data to save")
        return

    # Create a flat list of dictionaries for CSV writing
    rows = []
    for event_id, object_data in region_dict.items():
            row = {
                'Event_ID' : event_id,
                'NA_L3NAME': object_data['region'],
                'area_ha': object_data['area_ha']
            }
            rows.append(row)

    # Write to CSV file
    with open(output_file, 'w', newline='') as csvfile:
        fieldnames = ['Event_ID', 'NA_L3NAME', 'area_ha']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        writer.writeheader()
        for row in rows:
            writer.writerow(row)

    print(f"Metrics saved to {output_file}")


output_path = str(parent/"Output"/ "Rdm_points_region_key.csv")
save_to_csv(region_dict, output_path)







print("Script Finished. Move on to script 2 Buffer random points")
#For one region examine the mean and median of this compared to the og
#it's okay that we have missing values. Those random points fell in L3 regions without large wildfires.

"""
pseudo_areas_swr = [] #create empty list.
with arcpy.da.SearchCursor(out_name,["area_ha"]) as cursor:
    for row in cursor:
        pseudo_areas_swr.append(row[0])
pseudo_areas_swr_1 = np.array(pseudo_areas_swr)
print(f"the psuedo areas list is of type: {type(pseudo_areas_swr_1)}")
print("this should maintain the shape of the data")
print(f"Range: {pseudo_areas_swr_1.min():.0f} to {pseudo_areas_swr_1.max():.0f} sqm")
print(f"Mean original: {mean_size_ha:.0f} ha")
print(f"Mean psuedo: {pseudo_areas_swr_1.mean():.0f} ha")
print(f"Median original: {median_size_ha} ha")
print(f"Median psuedo: {np.median(pseudo_areas_swr_1)} ha")

"""