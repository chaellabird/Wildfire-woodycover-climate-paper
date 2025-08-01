library(pacman)
pacman::p_load(tidyverse,ggplot2,readxl,sf,tidyterra,terra)
rois_names<-c("Ouachita Mountains",
              "Arkansas Valley",
              "South Central Plains",
              "Southeastern Plains",
              "Ridge and Valley",
              "Southern Coastal Plain",
              "Central Appalachians")
rois_names<-c('Blue Ridge',
                         "Middle Atlantic Coastal Plain",
                         "Ozark Highlands",
                         "Southwestern Appalachians")


fires<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
  mutate(day_of_year = yday(Ig_Date)) %>% 
  filter(NA_L3NAME %in% rois_names)

rois_codes<-data.frame(rois_names) %>%
  rename(NA_L3NAME = "rois_names") %>% 
  inner_join(fires %>% 
               select(NA_L3NAME,US_L3CODE) %>% 
               unique())


all_dates <- seq.Date(from = as.Date("1991-01-01"),
                      to = as.Date("2021-12-31"),
                      by = "day")

random_points<-st_read('C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Pre-Process Data/Outputs/unsplit_buffered_points.shp') %>% 
  filter(US_L3CODE %in% rois_codes$US_L3CODE) %>% 
  rename(Event_ID = "TARGET_FID") %>% 
  mutate(Ig_Date = sample(all_dates, n(), replace = TRUE))

st_write(random_points,'Output/Data/Seven_regions_random_points_with_dates.shp')

st_write(random_points,'Output/Data/Four_regions_random_points_with_dates.shp')



####L1 Eastern Temperate Forests Scale ####
random_points_L1<-st_read("C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Pre-Process Data/Outputs/L1sampledpoints_1000.shp") %>% 
  mutate(Ig_Date = sample(all_dates, n(), replace = TRUE))



st_write(random_points_L1, "Output/Data/L1_rdm_points_with_dates.shp")

####L2 Eastern Temperate Forest Scale####
random_points_L2<-st_read("C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Pre-Process Data/Outputs/L2sampledpoints.shp") %>% 
  mutate(Ig_Date = sample(all_dates, n(), replace = TRUE)) %>% 
  filter(!NA_L2NAME %in% c("CENTRAL USA PLAINS","MIXED WOOD PLAINS"))



st_write(random_points_L2, "Output/Data/L2_rdm_points_with_dates.shp")


