#5/1/2024 
#Purpose: Assigning dates to random polygons
#Data used: random polygons

#load libraries
  library(pacman)
  pacman::p_load(tidyverse,sf,tidyterra,terra)
#Load datasets
  random<-st_read("Total_buffer/Total_buffer.shp") %>% 
    select(TARGET_FID,NA_L3NAME)

# Convert start_date and end_date to Date objects
start_date <- as.Date("1991-01-01")
end_date <- as.Date("2021-12-31")

# Generate 30,000 random dates
random_dates <- sample(seq(start_date, end_date, by = "day"), 30000, replace = TRUE)

# Show the first few random dates
head(random_dates)
random$Ig_Dates<-random_dates

#Save polygons

#I need to split this into several files, to avoid crashing GEE.

random_split <- split(random, rep(1:6, each = 5000))

# Loop through each chunk and write as a shapefile
for (i in seq_along(random_split)) {
  folder_path<-"Randompolygonshps"
  dir.create(folder_path,recursive =TRUE)
  current_shp <- random_split[[i]]
  st_write(current_shp, file.path(folder_path, paste0("random", i, ".shp")))
}


