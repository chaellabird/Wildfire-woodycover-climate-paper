library(pacman)
pacman::p_load(tidyverse, sf,terra,tidyterra,ggspatial,scales)






etf <- st_read("Data/us_eco_l3/us_eco_l3.shp", quiet = TRUE) 


#united states outline

us <- st_crop(etf, c(xmin = -70, xmax = -100, ymin = 25, ymax = 55))


# Aggregate geometries by US_L1CODE
l3eco_l1 <- etf %>%
  group_by(L1_KEY) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
etf_only<-l3eco_l1 %>% 
  filter(L1_KEY=="8  EASTERN TEMPERATE FORESTS")
# Plot and save Level 1
ggplot() +
  geom_sf(data = l3eco_l1, aes(fill = L1_KEY), fill = "gray",
          color = "grey30", linewidth = 0.25) +
  geom_sf(data = etf_only, aes(fill =L1_KEY), fill = "white",
          color = "grey30", linewidth = 0.25) +
  theme_void()

ggsave(file = "study_region_l1.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)

# # Aggregate geometries by US_L2CODE
# l3eco_l2 <- etf %>%
#   filter(L1_KEY=="8  EASTERN TEMPERATE FORESTS") %>% 
#   group_by(L2_KEY,NA_L2NAME) %>%
#   summarize(geometry = st_union(geometry), .groups = "drop")
# eco_l2_select<-l3eco_l2 %>% 
#   filter(L2_KEY %in% c("8.3  SOUTHEASTERN USA PLAINS",
#                        "8.4  OZARK/OUACHITA-APPALACHIAN FORESTS",
#                        "8.5  MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS")) %>% 
#   mutate(color = c("blue","orange","lightgreen"))

# Plot and save Level 2
# ggplot() +
#   geom_sf(data = l3eco_l2, aes(fill = NA_L2NAME), fill = "white",
#           color = "grey30", linewidth = 0.25) +
#   geom_sf(data = eco_l2_select, aes(fill = NA_L2NAME), fill = c("cyan","pink","orange"),
#           color = "grey30", linewidth = 0.25)
#   theme_void()
# 
# ggsave(file = "study_region_l2.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)

# Aggregate geometries by US_L3CODE
l3eco_l3 <- etf %>%
  group_by(NA_L3NAME,NA_L2NAME) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>% 
  filter(NA_L3NAME %in% c(
                "Central Appalachians",
                "Middle Atlantic Coastal Plain",
                "Ouachita Mountains",
                "Ozark Highlands",
                "Ridge and Valley",
                "South Central Plains",
                "Southeastern Plains",
                "Southern Coastal Plain")) #%>% 
  # mutate(numlabel = row_number(),
  #        NA_L3NAME = paste(numlabel,NA_L3NAME, sep = " "),
  #        color = c( "blue","blue","blue","lightgreen","blue","blue","blue",
  #           "orange","orange","lightgreen","blue"))


# Plot and save Level 3
ggplot() +
  geom_sf(data = etf_only, fill = "gray", color = "gray", linewidth = 0.25) +
  geom_sf(data = eco_l2_select, aes(fill = color),
          alpha =0.15,
          color = "grey30", linewidth = 0.25)+
  scale_fill_identity() +
  geom_sf(data = l3eco_l3, aes(fill = color), color = "black", linewidth = 0.25) +
  ggrepel::geom_text_repel(data = l3eco_l3,
                           aes(label = numlabel,
                               geometry = geometry),
                           stat = "sf_coordinates",
                           fun.geometry = st_point_on_surface,
                           point.size = NA,
                           min.segment.length = Inf,
                           size = 3.5, #6-8 point text required by the journal. What would it look like? 3.5 default
                           color = "black",
                           bg.color = "white",
                           bg.r = 0.3,
                           max.overlaps = 5)+
  theme(legend.text = element_text(size = 3.5))+
  theme_void()


ggsave(file = "Output/study_region_l3.jpg", width = 230, height = 190, units = "mm", dpi = 600, scale = 1.2)

states_and_regions<-read_sf('Data/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp') %>% 
  filter(NA_L1CODE == 8) %>% 
  group_by(STATE_NAME) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")


#plot all layers
earth_tone_colors <- c(
  "#964B00",  # Brown
  "#4CAF50", # Green
  "#553C88",  # Mauve
  "#4C5136",  # Taupe
  "#F7DC6F",  # Sandy Yellow
  "#C9E4CA",  # Pale Mustard
  "#7A288A",  # Mulberry
  "#C2B280")

ggplot()+
  theme_minimal()+
  # annotation_scale(location = ('bl'), 
  #                  width_hint = 0.4,
  #                  pad_x = unit(0.3, "in"),
  #                  pad_y = unit(0.1, "in"),
  #                  text_cex = 1)+
  # annotation_north_arrow(location = "br", width = unit(1.5, "cm"),
  #                        which_north = "true",
  #                        pad_x = unit(0.4, "in"),
  #                        pad_y = unit( 0.27, "in"),
  #                        style = north_arrow_minimal())+
  geom_sf(data = etf_only, aes(fill =L1_KEY), fill = "white",
          color = "lightgrey", linewidth = 0.25) +
  geom_sf(data = l3eco_l3, aes(fill = NA_L3NAME),
         color = "black", linewidth = 0.5)+
  scale_fill_manual(name = "L3 Region", values = earth_tone_colors)+
  geom_sf(data = states_and_regions, fill = "transparent",color = "darkgrey")+
  theme(legend.key.size = unit(0.1,"cm"),
        axis.title       = element_blank(),
        panel.background = element_rect(color = "white"), #element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 7))

ggsave(filename = "Output/graphsandfigures/Studyregion.jpg",
       dpi = 350,
       height =3.25,
       width = 3.25)



#plot just one region at a time
library(ragg)

ragg::agg_png("../Word Documents/screenshots and images/OMandscp.png",
              height =3.25 , 
              width =3.25 , 
              units = "in",
              bg = "transparent",
              res = 300 ,
              )

ggplot()+
  theme_minimal()+
  # annotation_scale(location = ('bl'), 
  #                  width_hint = 0.4,
  #                  pad_x = unit(0.3, "in"),
  #                  pad_y = unit(0.1, "in"),
  #                  text_cex = 1)+
  # annotation_north_arrow(location = "br", width = unit(1.5, "cm"),
  #                        which_north = "true",
  #                        pad_x = unit(0.4, "in"),
  #                        pad_y = unit( 0.27, "in"),
  #                        style = north_arrow_minimal())+
  geom_sf(data = etf_only, aes(fill =L1_KEY), fill = "white",
          color = "lightgrey", linewidth = 0.25) +
   geom_sf(data = l3eco_l3 %>% 
             filter(NA_L3NAME %in% c("Ouachita Mountains","South Central Plains")),
           color = "black", linewidth = 0.5,
          # fill ="#7A288A")+
           aes(fill = NA_L3NAME))+
   scale_fill_manual(name = "L3 Region", values = c("skyblue","#F7DC6F"))+
   #scale_fill_manual(name = "L3 Region", values = earth_tone_colors)+
  geom_sf(data = states_and_regions, fill = "transparent",color = "darkgrey")+
  theme(legend.key.size = unit(0.1,"cm"),
        axis.title       = element_blank(),
        axis.text = element_blank(),
        #panel.background = element_rect(fill = "transparent"), #element_blank(),
        #plot.background = element_rect(fill = "transparent"),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 7))

#save it
dev.off()

ggsave(filename = "C:/Users/micha/OneDrive - University of Florida/Grad Courses/AI Modeling in Soil Science/Class Project/Studyregion.jpg",
       dpi = 350,
       height =3.25,
       width = 3.25)
