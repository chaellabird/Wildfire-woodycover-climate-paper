



#load modules
from pathlib import *
import arcpy
import math
import numpy as np


np.random.seed(42)  # Optional: for reproducible results
#set working directory
scriptDir = Path.cwd()
parent = Path.cwd().parent
arcpy.env.workspace = str(parent / "Data"/ "Chapter2_revisions.gdb")
arcpy.env.overwriteOutput = True
grandparent = parent.parent

points = "random_points_revised"
points_lyr = arcpy.management.MakeFeatureLayer(points)


with arcpy.da.UpdateCursor(points_lyr, ["area_ha"]) as cursor:
    for row in cursor:
        if row[0]== 0:
            cursor.deleteRow()
            print(f"deleted row with area_m {row[0]}")

print("\n" * 1)
print("Buffer random points based on area_ha column")
#determine the linear units of the spatial reference
desc = arcpy.Describe(points)
spatial_ref = desc.spatialReference

# Get linear unit information
linear_unit_name = spatial_ref.linearUnitName
linear_unit_code = spatial_ref.linearUnitCode

print(f"Linear unit name: {linear_unit_name}")
print(f"Linear unit code: {linear_unit_code}")
print(f"Coordinate system: {spatial_ref.name}")

#create a column with the radius to buffer
arcpy.management.AddField(points_lyr, "radius_m","FLOAT")

#populate field
with arcpy.da.UpdateCursor(points_lyr, ["radius_m", "area_ha"]) as cursor:
    for row in cursor:
        area_ha = row[1]
        area_m = area_ha * 10000
        radius_m = math.sqrt(area_m/math.pi)
        row[0] = radius_m
        cursor.updateRow(row)



out_name = "buffered_points_L1_revised"
arcpy.analysis.Buffer(points_lyr, out_name, "radius_m")

#change name of ORIG_FID to Event_ID
arcpy.management.AlterField(out_name, "ORIG_FID", "Event_ID")

print("Script Finished. Go to Script 3. Assign Dates to Random Points.")