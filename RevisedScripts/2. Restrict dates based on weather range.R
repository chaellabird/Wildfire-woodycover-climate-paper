library(pacman)
pacman::p_load(tidyverse,ragg,readxl)
library(patchwork)


set.seed(127)

#Read in data 

fire_precip_unsummarized<-read_csv("Data/Fire Data/ETF_fireweather_MTBS.csv") %>%
  full_join(read_excel("Data/Fire Data/fires_Eastern_temperate_forest.xlsx"), by = "Event_ID") %>% 
  mutate(diff_days = floor(as.numeric(difftime(date, fire_date, units = "days")))) 



rdm_precip_unsummarized<-read_csv("Data/Random Points Data/ETF_fireweather_multiple_dates_L1_haycropfiltered.csv") %>% 
  left_join(read_csv("Output/Rdm_points_region_key.csv")) %>% 
  select(NA_L3NAME,ignition_id, Event_ID, pr, diff_days,ignition_date)

counts_rdm<- rdm_precip_unsummarized %>% 
  select(Event_ID) %>% 
  unique() %>% 
  summarize(actual_count = n()) #2277 is the right number!!


combined <- rdm_precip_unsummarized %>%
  mutate(set = "Random",
         Event_ID = as.character(Event_ID)) %>%                # tag each row
  bind_rows(
    fire_precip_unsummarized %>% mutate(set = "Fire",
                                        ignition_id = 4)    # tag the others
  ) %>% 
  filter(diff_days <1 & diff_days > -1) %>% 
  group_by(NA_L3NAME,ignition_id, Event_ID) %>% 
  mutate(accumulated_precip =sum(pr)) %>% 
  select(where(~!any(is.na(.)))) %>% 
  select(-c(diff_days,pr))%>% 
  unique()

plot_before<-ggplot(combined, aes(x = accumulated_precip, fill = set, color = set)) +
  geom_density(alpha = 0.4, adjust = 1) +
 # facet_wrap(~NA_L3NAME)+
  scale_fill_manual(name = "Set",
                    values = c("Random" = "#1f77b4", "Fire" = "#ff7f0e")) +
  scale_colour_manual(name = "Set",
                      values = c("Random" = "#1f77b4", "Fire" = "#ff7f0e")) +
  labs(subtitle = "Before",
       # title = "Accumulated Precipitation before restricting",
       x = NULL, y = NULL
       #x     = "Accumulated Precipitation (mm)",
       #y     = "Density"
  ) +
  theme_minimal()
ggsave("Output/figures/precip_density_BEFORE.jpg")


####Exclude points that fall outside the 3 day sum max####
fire_precip<-fire_precip_unsummarized %>% 
  filter(diff_days < 1 & diff_days > -1) %>% 
  group_by(Event_ID, NA_L3NAME) %>% 
  summarise(accumulated_precip = sum(pr, na.rm = TRUE), .groups = "drop")

rdm_precip<- rdm_precip_unsummarized %>% 
  filter(diff_days < 1 & diff_days > -1) %>% 
  group_by(Event_ID,ignition_id, NA_L3NAME) %>% 
  summarise(accumulated_precip = sum(pr, na.rm = TRUE), .groups = "drop")



split<-rdm_precip %>% 
  group_by(NA_L3NAME) %>%
  group_split()

#create a dictionary on the max accumulated precipitation in the wildfires
max_values_dict<- fire_precip %>% 
  group_by(NA_L3NAME) %>% 
  summarize(across(where(is.numeric), max, .names = "{col}")) %>% 
  rename("max_precip_fire"= "accumulated_precip") %>% 
  #mutate(max_precip_fire = case_when(max_precip_fire <1 ~ max_precip_fire +0.001,
  #                                   max_precip_fire >1 ~ max_precip_fire)) %>% 
  left_join(rdm_precip %>% 
  group_by(NA_L3NAME) %>% 
  summarize(across(where(is.numeric), max, .names = "{col}")) %>% 
  rename("max_precip_rdm" = "accumulated_precip") %>% 
  
  select(-Event_ID)
)


#exclude values outside the max values for each region
precip_filtered<-lapply(split, function (df){
  region <- unique(df$NA_L3NAME)
  print(region)
  limit<-max_values_dict$max_precip_fire[max_values_dict$NA_L3NAME== region]
  print(limit)
  df %>% 
    filter(df$accumulated_precip <= limit)%>% 
    group_by(Event_ID) %>% 
    sample_n(1) 
  
}) %>% 
  bind_rows()






#see if we have enough points. 
precip_filtered %>% 
  select(NA_L3NAME, Event_ID) %>% 
  unique() %>% 
  summarize(actual_count = n()) 
#We have 2277!!!!


#examine
look<-precip_filtered %>% 
  filter(NA_L3NAME %in% c("Southeastern Wisconsin Till Plains","Northern Allegheny Plateau","Northeastern Coastal Zone","Central Corn Belt Plains"))



#we have an adequate number of points
#but what does the distribution look like now?

combined_filtered<-precip_filtered %>% 
  group_by(NA_L3NAME,Event_ID) %>% 
  sample_n(1) %>%
  select(-ignition_id) %>% 
  mutate(set = "Random",
         Event_ID = as.character(Event_ID)) %>% 
  bind_rows(fire_precip %>% 
              mutate(set = "Fire")) %>% 
  filter(NA_L3NAME != "Southern Coastal Plain")
  

plot_after<-ggplot(combined_filtered, aes(x = accumulated_precip, fill = set, color = set))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(name = "Set",
                    values = c("Random" = "#1f77b4", "Fire" = "#ff7f0e")) +
  scale_colour_manual(name = "Set",
                      values = c("Random" = "#1f77b4", "Fire" = "#ff7f0e")) +
  labs(subtitle = "After", x = NULL, y = NULL)+
  theme_bw()
    #x = "Precipitation accumulated over past 3 days (mm)")#, tag = "after restriction")
ggsave("Output/figures/Precipitation after restriction.jpg", plot)

together<-plot_before + plot_after +plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom") &
  labs(x = "Accumulated precipitation (mm)", y = "Density", fill = "Type")
ggsave("Output/figures/Precipitation before_after.jpg", together)



#save the ids and their dates as a csv to save time in getting the other data

precip_filtered_key<-precip_filtered %>% 
  select(Event_ID,ignition_id) #the date with its matching event id. I need to pivot it. 

write_csv(precip_filtered_key,"Output/Dates_precip_filtered_rdm_keep_crophayfiltered.csv")

#the script is finished. Go to python script 5.


