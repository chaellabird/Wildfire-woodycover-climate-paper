library(pacman)
pacman::p_load(tidyverse,sf,terra,tidyterra)

region_key<-read_csv("Output/NLCD_proportion_pivoted_wildfires.csv") %>% 
  select(NA_L3NAME,event_id) %>% 
  unique() %>% 
  mutate(Event_ID = event_id)

fires_polygons<-read_sf("../Pre-process Spatial Data/Output/Unaltered_MTBS_ETF_Limited.shp") %>% 
  mutate(PA = "1") %>% 
  st_drop_geometry() %>% 
  rename(area_ha = Area_ha) %>% 
  full_join(region_key) %>% 
  select(NA_L3NAME,Event_ID, area_ha,PA)

#needs the NA_L3NAME 

filtered_polygons<-read_sf("Output/Shapefiles/Randompoints_restricted_dates_nlcd_gotime_2.shp") %>% 
  mutate(PA = "0",
         Event_ID = as.character(Event_ID)) %>% 
  st_drop_geometry() %>% 
  select(NA_L3NAME,Event_ID, area_ha,PA)
counts_keep<-fires_polygons %>% 
  group_by(NA_L3NAME) %>% 
  mutate(n= n()) %>% 
  select(NA_L3NAME, n) %>% 
  unique() %>% 
  filter(n > 15)

together <-bind_rows(filtered_polygons,fires_polygons) %>% 
  filter(NA_L3NAME %in% counts_keep$NA_L3NAME)
library(ragg)
agg_png(
  filename = "Output/figures/Comparison of sizes of final points and wildfires.png",
  width = 16,
  height = 16,
  units = "in",
  pointsize = 12,
  res = 72,
  scaling = 1.5,
)
ggplot(data = together, aes(x = area_ha, fill =PA)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 40)+
  facet_wrap(~NA_L3NAME, scales = "free")+
  theme_bw()
dev.off()
