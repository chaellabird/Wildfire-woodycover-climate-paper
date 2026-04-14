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

#check what format it expects the dates to be in by looking at the mtbs
mtbs = str(grandparent / "Pre-process Spatial Data"/"Output"/ "Unaltered_MTBS_ETF_Limited.shp")
for field in arcpy.ListFields(mtbs):
    if field.name == "Ig_Date":
        print(f"field {field.name}, is of type {field.type}")

points = "buffered_points_L1_revised"


start_date = datetime(1991, 1, 1)
end_date = datetime(2021,12,31)
days_span  = (end_date - start_date).days



#create date field for the points
print("Adding date field to the buffered points...")
if "Ig_date" in arcpy.ListFields(points):
    print("date field already found")
else:
    arcpy.management.AddField(points,"Ig_Date","DATE")
    print("Date field added to buffered points")
'''
with arcpy.da.UpdateCursor(points, ["Ig_Date"]) as cursor:
    for row in cursor:
        random_date = random.choice(available_dates)
        row[0] = random_date.date()  # Convert to date object
        cursor.updateRow(row)
'''
with arcpy.da.UpdateCursor(points, ["Ig_Date"]) as cur:
    for row in cur:
        # pick a random number of days and add to the start date
        row[0] = start_date + timedelta(days=random.randint(0, days_span))
        cur.updateRow(row)

print("Random dates loaded.  Check the field type if you want to be extra sure:")
for fld in arcpy.ListFields(points, "Ig_Date"):
    print(f"Field {fld.name} has type {fld.type}")


#check on the output
counter =0
with arcpy.da.SearchCursor(points, "*") as cursor:
    for row in cursor:
        counter +=1
        print(row)
        if counter == 5:
            break




#export the points feature class as a shapefile that can be uploaded to google earth engine
out_feature = str(parent / "Output"/ "L1_points_with_dates.shp")
arcpy.management.CopyFeatures(in_features = points,out_feature_class= out_feature)

print("Script Finished. Go to Google Earth Engine to extract NLCD Cover Data.")
print("After that go to R script 1")