
#write_csv(randlev1_matched, here("Output", "nlcd_prp_randlev1_CROPS_filtered.csv"))
#in this script I need to bring in the crops filtered file, get year as its own thing, and then select three dates from that year

#load modules
from pathlib import *
import arcpy
#import math
#import numpy as np
#import pandas as pd
import random
from datetime import datetime, timedelta
scriptDir = Path.cwd()
parent = Path.cwd().parent
arcpy.env.workspace = str(parent / "Data"/ "Chapter2_revisions.gdb")
arcpy.env.overwriteOutput = True
grandparent = parent.parent

#set seed for reproducibility
random.seed(42)


#check what format it expects the dates to be in by looking at the mtbs
mtbs = str(grandparent / "Pre-process Spatial Data"/"Output"/ "Unaltered_MTBS_ETF_Limited.shp")
for field in arcpy.ListFields(mtbs):
    if field.name == "Ig_Date":
        print(f"field {field.name}, is of type {field.type}")

points_filter_key = str(parent/ "Output"/ "nlcd_prp_randlev1_cropshayfiltered.csv") #this tells us which to keep based on proportions of crop and hay
print("creating a set that contains all the 'good' ids")
keep_ids = set()
with arcpy.da.SearchCursor(points_filter_key, ["event_id"]) as cur:
    for row in cur:
        try:
            keep_ids.add(int(row[0]))
        except (ValueError, TypeError):
            # skip non‑numeric or missing values
            continue



print("filtering out the points that failed the nlcd test...")
print("but first, creating a feature layer so we don't mess with the old one...")
points_og = str(parent / "Output"/ "L1_points_with_dates.shp") #this is a shapefile.
points = "points_lyr"
arcpy.management.MakeFeatureLayer(points_og, points)
print("layer created")

for field in arcpy.ListFields(points):
    print(field.name)



in_ids = ", ".join(str(i) for i in keep_ids)
where_clause = f"Event_ID IN ({in_ids})"
arcpy.SelectLayerByAttribute_management(points, "NEW_SELECTION",where_clause)
#look at the field names in points.

print(f"the original points creation was {arcpy.management.GetCount(points_og)} rows long "
      f"the filtering list is {arcpy.management.GetCount(points_filter_key)} rows long "
      f" the points feature layer is {arcpy.management.GetCount(points)} rows long.")






print("Adding year field to the filtered buffered points...")
if "rdm_year" in arcpy.ListFields(points):
    print("rdm year field already found")
else:
    arcpy.management.AddField(points,"rdm_year","TEXT")
    print("rdm year field added to filtered buffered points")

#create date field for the points
print("Adding 1st random date field to the filtered buffered points...")
if "Ig_date_1" in arcpy.ListFields(points):
    print("date field already found")
else:
    arcpy.management.AddField(points,"Ig_Date_1","DATE")
    print("rdm Date field 1 added to buffered points")

print("Adding 2nd random date field to the filtered buffered points...")
if "Ig_date_2" in arcpy.ListFields(points):
    print("rdm date field 2 already found")
else:
    arcpy.management.AddField(points,"Ig_Date_2","DATE")
    print("Date field 2 added to buffered points")

print("Adding 3rd rdm date field to the filtered buffered points...")

if "Ig_date_3" in arcpy.ListFields(points):
    print("rdm date field 3 already found")
else:
    arcpy.management.AddField(points,"Ig_Date_3","DATE")
    print("rdm Date field 3 added to buffered points")

if "Ig_date_4" in arcpy.ListFields(points):
    print("rdm date field 4 already found")
else:
    arcpy.management.AddField(points,"Ig_Date_4","DATE")
    print("rdm Date field 4 added to buffered points")

if "Ig_date_5" in arcpy.ListFields(points):
    print("rdm date field 5 already found")
else:
    arcpy.management.AddField(points,"Ig_Date_5","DATE")
    print("rdm Date field 5 added to buffered points")

print("checking to see if the date fields were properly added...")
#make sure they have been added
if "Ig_date_1" in arcpy.ListFields(points):
    print("rdm date field 1  found")
if "Ig_date_2" in arcpy.ListFields(points):
    print("rdm date field 2  found")
if "Ig_date_3" in arcpy.ListFields(points):
    print("rdm date field 3 found")


print("extracting the year into its own field")
counter = 0
with arcpy.da.UpdateCursor(points, ["Ig_Date", "rdm_year"]) as cursor:
    for row in cursor:
        #random_date = random.choice(available_dates)
        year_string = row[0].strftime("%Y")# Convert to date object
        row[1] = year_string
        counter += 1
        if counter < 5:
            print(year_string)
        cursor.updateRow(row)

#now I need to populate the three date fields using a random choice between year
print("Populating the five date fields within the year constraint.")
counter = 0
with arcpy.da.UpdateCursor(points, ["rdm_year", "Ig_date_1", "Ig_date_2", "Ig_date_3", "Ig_date_4", "Ig_date_5"]) as cursor:
    for row in cursor:
        year = int(row[0])
        start_date = datetime(year, 1, 1)
        end_date = datetime(year, 12, 31)
        days_span = (end_date - start_date).days
        #pick 5 offset days

        offsets = random.sample(range(days_span + 1), 5)
        random_dates = [start_date + timedelta(days=off) for off in offsets]

        # Assign them to the 5 fields
        row[1:6] = random_dates
        counter += 1
        if counter <7:
            print(row)
        cursor.updateRow(row)

print("Random dates loaded.  Check the field type if you want to be extra sure:")
for fld in arcpy.ListFields(points):
    print(f"Field {fld.name} has type {fld.type}")


#check on the output
counter =0
with arcpy.da.SearchCursor(points, "*") as cursor:
    for row in cursor:
        counter +=1
        print(row)
        if counter == 5:
            break


print("exporting shapefile...")

#export the points feature class as a shapefile that can be uploaded to google earth engine
out_feature = str(parent / "Output"/"Shapefiles"/ "L1_points_with_multiple_dates_hay_crop_filtered.shp")
arcpy.management.CopyFeatures(in_features = points,out_feature_class= out_feature)

print("go on to extracting precipitation data in google earth engine.")
print("After that go to R script 2.")