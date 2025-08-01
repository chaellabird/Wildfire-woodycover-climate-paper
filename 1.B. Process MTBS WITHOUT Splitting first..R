####Load packages####
pacman::p_load(tidyverse, sf, terra,
               tidyterra, ggspatial,
               lwgeom,writexl)
sf_use_s2(FALSE)

#I need to create a file that has info on which regions touch which Event_IDs. 
#Then, I need to filter the MTBS base shpfile by it. 


####Load Data####
mtbs <-
  read_sf("Data/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  mutate(year = year(Ig_Date), 
         area_ha = as.numeric(st_area(.) / 10000)) %>%
  group_by(Event_ID) %>%
  mutate(area_ha = sum(area_ha)) %>% ungroup %>%
  filter(Incid_Type == "Wildfire",
         area_ha >=200, #filter out all fires smaller than 200 ha
         year >= 1991 & year <= 2021) %>% 
  select(Event_ID,Ig_Date,geometry,area_ha)%>% vect()


etf <-
  st_read("Data/us_eco_l3/us_eco_l3.shp") %>%
  filter(NA_L1CODE == 8) %>% vect()



####Intersect MTBS with ETF####
if (!identical(crs(etf), crs(mtbs))) {
  mtbs <- project(mtbs, crs(etf))
}

# Intersect mtbs with etf
intersection_result <- intersect(mtbs, etf)

####Save these as new polygons####
mtbs<-st_as_sf(intersection_result) %>% 
  #mutate(burned_area = as.numeric(st_area(.) / 10000)) %>% 
  #filter(burned_area >=100) %>% 
  select(-c("L2_KEY","L1_KEY",
            "Shape_Leng","Shape_Area",
           # "NA_L2CODE",  "NA_L2NAME",
            "NA_L1CODE","NA_L1NAME","L3_KEY","US_L3NAME"))


east_temp_fire<-mtbs %>% 
  st_drop_geometry() 
write_xlsx(east_temp_fire,"Output/fires_Eastern_temperate_forest.xlsx")

#Bring in the unaltered MTBS file
MTBS<-st_read("Data/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  filter(Event_ID %in% east_temp_fire$Event_ID)


st_write(MTBS,"Output/Unaltered_MTBS_ETF_Limited.shp")


####Package the ETF away for future mapping ####
just_etf<-st_as_sf(etf) %>%
  select(-c("L2_KEY","L1_KEY",
            "Shape_Leng","Shape_Area",
            #"NA_L2CODE",  "NA_L2NAME",
            "NA_L1CODE","NA_L1NAME")) %>% 
  mutate(poly_id = row_number()) %>%
  group_by(NA_L2CODE,NA_L2NAME, NA_L3NAME) %>% 
  #group_by(NA_L3NAME,US_L3CODE,NA_L3CODE,L3_KEY) %>% 
  summarize(geometry = st_union(geometry))


st_write(just_etf,'Output/us_l3_ecoregions_ETF.shp')  

st_write(just_etf,"Output/us_l3_ecoregions_ETF_UNsimplifiedgeometry.shp")

st_write()


l3_ecoregions<-just_etf %>% 
  st_drop_geometry()

write_xlsx(l3_ecoregions, 'Output/l3_ecoregions.xlsx')

ggplot() +
  geom_spatvector(etf, mapping = aes(), fill = "white") +
  geom_spatvector(mtbs, mapping = aes(), color = "red") +
  theme_bw()

