library(pacman)
pacman::p_load(tidyverse, sf, ggrepel)

# Load the shapefile
eco <- st_read("Data/us_eco_l3/us_eco_l3.shp", quiet = TRUE)
states<-st_read('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp', quiet = TRUE) %>% 
  filter(NA_L1CODE == 8) %>% 
  group_by(STATE_NAME) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

bbox <- st_bbox(states)
bbox["ymin"]<-bbox["ymin"]-100000
bbox["xmin"]<-bbox["xmin"]-120000
clip_square <- st_as_sf(st_sfc(st_polygon(list(matrix(
  c(bbox["xmin"], bbox["ymin"],
    bbox["xmax"], bbox["ymin"],
    bbox["xmax"], bbox["ymax"],
    bbox["xmin"], bbox["ymax"],
    bbox["xmin"], bbox["ymin"]),
  ncol=2, byrow=TRUE))), crs=st_crs(states)))



states_east <- st_intersection(st_read('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp', quiet = TRUE), clip_square) %>% 
  group_by(NA_L1NAME) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")






# Filter for the Eastern Temperate Forests
etf <- eco %>%
  filter(L1_KEY == "8  EASTERN TEMPERATE FORESTS")

# Aggregate geometries by L1_KEY
L1_shp <- st_read('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>% 
  filter(NA_L1CODE == 8) %>% 
  #group_by(STATE_NAME) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>% 
  mutate(NA_L1NAME = "L1 Eastern Temperate Forests")

# Check dimensions
#print(st_geometry(etf_merge))

# Aggregate geometries by L2_KEY
# l3eco_l2 <- etf %>%
#   group_by(L2_KEY, NA_L2NAME) %>%
#   summarize(geometry = st_union(geometry), .groups = "drop")

# Check dimensions
#print(st_geometry(l3eco_l2))

# Aggregate geometries by L3_KEY for regions not used
l3s_not_used <- etf %>%
  filter(L2_KEY %in%
           c("8.3  SOUTHEASTERN USA PLAINS",
             "8.4  OZARK/OUACHITA-APPALACHIAN FORESTS",
             "8.5  MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS")) %>%
  group_by(NA_L3NAME) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# l2s_not_used_test <- etf %>%
#   filter(L2_KEY %in%
#            c("8.3  SOUTHEASTERN USA PLAINS",
#              "8.4  OZARK/OUACHITA-APPALACHIAN FORESTS",
#              "8.5  MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS")) %>%
#   group_by(NA_L2NAME, NA_L3NAME) %>%
#   summarize(geometry = st_union(geometry), .groups = "drop")
# # Check dimensions
# print(st_geometry(l3s_not_used))

# Filter and aggregate geometries for regions used
l3s_used <- etf %>%
  filter(NA_L3NAME %in% c("Central Appalachians",
                          "Middle Atlantic Coastal Plain",
                          "Ouachita Mountains",
                          "Ozark Highlands",
                          "Ridge and Valley",
                          "South Central Plains",
                          "Southeastern Plains",
                          "Southern Coastal Plain")) %>%
  group_by(NA_L3NAME,NA_L2NAME) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  arrange(NA_L3NAME) %>% 
  mutate(label = row_number(),
         label_name = paste(label, NA_L3NAME, sep = " ")) %>%   # Concatenate number with NA_L3NAME
  rename("Ecoregion" = label_name)
# Check dimensions
print(st_geometry(l3s_used))

# Create numeric labels for used regions
l3s_used_labels <- l3s_used %>%
  mutate(geometry = st_centroid(geometry))


#plot L1 with ggplot
ggplot() +
  geom_sf(data = L1_shp, fill = "grey80", color = "grey20") +
  theme_void() 

    ggsave(file = "Output/graphsandfigures/study_region_l1.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)


#L1 with states
ggplot() +
  geom_sf(data = L1_shp, fill = "grey80", color = "grey20") +
  geom_sf(data = states, fille = NA, color = "grey20")+
  theme_void() 

ggsave(file = "Output/graphsandfigures/l1_with_states.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)



# #Plot L2
# ggplot() +
#   geom_sf(data = L1_shp, fill = "grey80", color = NA) +
#   geom_sf(data = (l3eco_l2 %>% 
#                     filter(L2_KEY %in%
#                              c("8.3  SOUTHEASTERN USA PLAINS",
#                                "8.4  OZARK/OUACHITA-APPALACHIAN FORESTS",
#                                "8.5  MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS"))),
#           aes(fill = NA_L2NAME))+
#   geom_sf(data = states, fill = NA, color = "grey60")+
#   scale_fill_manual(values = c("#b48ac0","#1b6f9f","#f5b739"),
#                     labels = c("Mississippi Alluvial and \nSoutheast USA Coastal Plains",
#                                "Ozark-Ouachita-\nAppalachian Forests",
#                                "Southeastern USA Plains"),
#                     name = "L2 Ecoregion")+
#   theme_void()+
#   theme(theme(legend.text = element_text(size = 20)))
#   ggsave(file = "Output/graphsandfigures/study_region_l2.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)





# Plot full map with ggplot
ggplot() +
  geom_sf(data = states_east, fill = "white", color = "darkgrey", legend = FALSE)+
  geom_sf(data = L1_shp, fill = "grey90", aes(color = NA_L1NAME), show.legend = TRUE) +
  scale_color_manual(values = "black",name = "L1 Ecoregion")+
  geom_sf(data = l3s_used, aes(fill = Ecoregion), color = "black", show.legend = TRUE) +
  scale_fill_brewer(palette = "Set2", name = "L3 Ecoregion")+
  # geom_sf(data = l3s_not_used, fill = "white", show.legend = FALSE) + # you need this
  #  geom_sf(data = l3s_not_used, aes(fill = NA_L3NAME), alpha = 0.1, show.legend = FALSE) +
#  geom_sf(data = dummy, aes(fill = label_name), alpha = 0, show.legend = TRUE)+
  geom_sf(data = states, fill = NA,color = "grey60", show.legend = FALSE)+
  theme_void() +
  ggrepel::geom_text_repel(data = l3s_used,
                           aes(label = label, geometry = geometry),
                           stat = "sf_coordinates",
                           fun.geometry = st_point_on_surface,
                           force = 0.4,
                           force_pull = 0.8,
                           box.padding = 0.1,
                           size = 6,
                           color = "black",
                           bg.color = "white",
                          # nudge_x = ifelse(l3s_used_labels$NA_L3NAME == "Middle Atlantic Coastal Plain", 4, 
                           #                 ifelse(l3s_used_labels$NA_L3NAME == "Southeastern Plains", -14,0)),
                          #nudge_y = ifelse(l3s_used_labels$NA_L3NAME == "Southeastern Plains", -22,4),#,
                          #                 ifelse(l3s_used_labels$NA_L3NAME == "Ouachita Mountain", 300, 0)),
                           bg.r = 0.3,
                           max.overlaps = 5)#

ggsave(file = "Output/graphsandfigures/Study_L1.jpg",
       height = 3.5,
       width = 3.5,
       dpi = 350)


ggsave(file = "Output/graphsandfigures/Studyregion_forppt.jpg",
       #height = 3.5,
       #width = 3.5,
       dpi = 350)


  scale_fill_manual(name = "Ecoregion",
    values = c(
    # Southeastern USA Plains (Shades of Orange)
    "East Central Texas Plains" = "#f6b26b",
    "Interior Plateau" = "#f6b26b",
    "Interior River Valleys and Hills" = "#f6b26b",
    "Mississippi Valley Loess Plains" = "#f6b26b",
    "Northern Piedmont" = "#f6b26b",
    "Piedmont" = "#f6b26b",
    "South Central Plains" = "#f5b739",
    "Southeastern Plains" = "#f5b739",
    # Mississippi Alluvial and Southeast USA Coastal Plains (Shades of Purple)
    "Mississippi Alluvial Plain" = "#c4a3d7",
    "Southern Coastal Plain" = "#b48ac0",
    "Middle Atlantic Coastal Plain" = "#b48ac0",
    # Ozark/Ouachita-Appalachian Forests (Shades of Blue)
    "Arkansas Valley" = "#1b6f9f",
    "Blue Ridge" = "#1b6f9f",
    "Boston Mountains" = "#9ecae1",
    "Central Appalachians" = "#1b6f9f",
    "Ouachita Mountains" = "#1b6f9f",
    "Ozark Highlands" = "#1b6f9f",
    "Ridge and Valley" = "#1b6f9f",
    "Southwestern Appalachians" = "#1b6f9f",
    "Western Allegheny Plateau" = "#9ecae1"))


ggsave(file = "Output/graphsandfigures/study_region_l3.jpg", 
       width = 230, height = 190, 
       units = "mm", dpi = 600, scale = 1.2)


#need to create dummy layer for text to the side like in figure 1






####Map individual regions for powerpoint visualization####

ggplot() + 
  geom_sf(data = (l3s_used %>% 
                    filter(NA_L3NAME == "Southern Coastal Plain")),
                  fill = "#1b6f9f",
                  size = 1.5, color = "black") + 
            theme_void()+
            coord_sf()
          
          
          
      ggsave(file = "Output/graphsandfigures/single_region/southcoast_and_middle_atlantic.png",
                 width = 230, height = 190, 
                 units = "mm", dpi = 600, scale = 1.2)


      ggsave(file = "Output/graphsandfigures/single_region/plains.png",
             width = 230, height = 190, 
             units = "mm", dpi = 600, scale = 1.2)

      ggsave(file = "Output/graphsandfigures/single_region/mountains.png",
             width = 230, height = 190, 
             units = "mm", dpi = 600, scale = 1.2)
      

    ggplot() + 
        geom_sf(data = (l3eco_l2 %>% 
                          filter(NA_L2NAME == "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS")),
                fill = "#b48ac0",
                size = 1.5, color = "black") + 
        theme_void()+
        coord_sf()
      ggsave(file = "Output/graphsandfigures/single_region/L2_mountains.png",
             width = 230, height = 190, 
             units = "mm", dpi = 600, scale = 1.2)
      
      
      
      ggsave(file = "Output/graphsandfigures/single_region/L2_plains.png",
             width = 230, height = 190, 
             units = "mm", dpi = 600, scale = 1.2)

      ggsave(file = "Output/graphsandfigures/single_region/L2_coast.png",
             width = 230, height = 190, 
             units = "mm", dpi = 600, scale = 1.2)
      



