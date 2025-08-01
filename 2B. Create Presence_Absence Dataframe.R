#The purpose of this data is to prepare presence/absence data, like in 
 #chapter one.
####Load packages####
library(pacman)
pacman::p_load(tidyverse,readxl,sf,writexl)




####L3 Level####
#Import the fire data
NA_L3NAME<-c("Central Appalachians",
              "Middle Atlantic Coastal Plain",
              "Ouachita Mountains",
              "Ozark Highlands",
              "Ridge and Valley",
              "South Central Plains",
              "Southeastern Plains",
              "Southern Coastal Plain")
US_L3CODE<-c(69,63,36,39,67,35,65,75)
key<-data.frame(cbind(NA_L3NAME,US_L3CODE)) %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE))#this step is necessary
fires<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Objective 3 Fire Size by Climate and Woody Cover/Outputs/Output Data/Fire_IDs_with_weather_indices_and_vegetation_EXTRAindices.xlsx') %>% 
#fires<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Objective 3 Fire Size by Climate and Woody Cover/Outputs/Output Data/Fire_IDs_with_weather_indices_and_vegetation.xlsx') %>% 
  filter(US_L3CODE %in% key$US_L3CODE) %>% 
  select(-c(fire_size)) %>%
  na.omit() %>% #remove the na soil moisture values
  mutate(percent_cover = percent_cover*100,
         US_L3CODE = as.numeric(US_L3CODE),
         PA = 1) %>% 
  group_by(US_L3CODE) %>% 
  mutate(count = n_distinct(Event_ID))


#Bring in random points data
rdmpts_weather<-read_csv('Data/Year_of_fire_weather_rdm_pts.csv') %>% 
  bind_rows(read_csv('Data/Year_of_fire_weather_rdm_pts_L3_ADDED_REGIONS.csv')) %>% 
  select(where(~!any(is.na(.))))
extraindices<-read_csv("Data/L3_buffers_fireweather_extra (1).csv") %>% 
  bind_rows(read_csv("Data/L3_buffers_fireweather_extraB.csv"))%>% 
  select(where(~!any(is.na(.))))
 
all_rdmpts_weather <- left_join(rdmpts_weather, extraindices, by = c("fire_date", "date", "Event_ID", "pr")) %>% 
  na.omit() %>% 
  mutate(diff_days = as.numeric(difftime(as.Date(date), as.Date(fire_date), units = "days"))) 
summarized_weather<-all_rdmpts_weather%>% 
  group_by(Event_ID, US_L3CODE) %>% 
  mutate(vpd_10.5.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         vpd_10.1.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         vpd_5.5.window_mean = mean(vpd[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         vpd_1.1.window_mean = mean(vpd[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         vs_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         vs_10.1.window_mean = mean(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         vs_5.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         vs_1.1.window_mean = mean(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         vs_10.1.window_max = max(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         vs_5.1.window_max = max(vs[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
         vs_1.1.window_max = max(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         fm100.10.5.window_mean = mean(fm100[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         fm100.10.1.window_mean = mean(fm100[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         fm100.5.5.window_mean = mean(fm100[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         fm100.1.1.window_mean = mean(fm100[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         fm1000.10.5.window_mean = mean(fm1000[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         fm1000.10.1.window_mean = mean(fm1000[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         fm1000.5.5.window_mean = mean(fm1000[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         fm1000.1.1.window_mean = mean(fm1000[diff_days <= 1 & diff_days >= -1], na.rm = TRUE))%>% 
  select(-c(vpd,vs,date,pr,fm100,fm1000)) %>% #ditch the old columns
  unique()#remove duplicate entries
  


rdm_points<-read_csv('Data/year_of_fire_drought_stats_rdm_pts.csv') %>%
  bind_rows(read_csv('Data/year_of_fire_drought_stats_rdm_pts_L3_ADDED_REGIONS.csv')) %>% 
  mutate(diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
  filter(diff_days <= 5 & diff_days >=-5) %>%
  group_by(Event_ID,US_L3CODE) %>% 
  summarize(spei30 = mean(spei30d),
            spei14 = mean(spei14d),
            pdsi = mean(pdsi)) %>%
  left_join(summarized_weather) %>% 
  left_join(read_csv('Data/L3_Seven_regions_rdmpts_woody_Ha.csv') %>%
              bind_rows(read_csv('Data/Four_regions_rdmpts_woody_Ha.csv')) %>% 
              left_join(read_csv('Data/L3_Seven_regions_rdmpts_evaluated_area_ha.csv') %>% 
                          bind_rows(read_csv('Data/Four_regions_rdmpts_evaluated_area_ha.csv'))) %>% 
              mutate(percent_cover = (woody_hectares/Evaluated_Hectares)*100) %>%
              filter(Year == as.numeric(year(as_datetime(Ig_Date)))-1)) %>% 
  right_join(key) %>% 
  mutate(PA = 0) %>%  
    na.omit() %>%  #this gets rid of sm NA values
  #filter(Evaluated_Hectares >200) %>% #this gets rid of random points placed in the wrong spots
  select(-c(Evaluated_Hectares,woody_hectares,Year))

set.seed(seed = 50)#make selection repeatable


#Create a function to sample data based on fire counts
  sample_targets <- function(data) {
    sample_size <- unique(data$count) 
    sample_n(data, sample_size)
  }

  
  
  sampled_rdm_points<-rdm_points %>%
    left_join(fires %>% 
                select(US_L3CODE,count) %>% 
                unique() %>% 
                mutate(US_L3CODE = as.numeric(US_L3CODE))) %>% 
    filter(!is.na(count)) %>% 
    group_by(US_L3CODE) %>%
    group_modify(~ sample_targets(.x)) %>%
    ungroup() %>% 
    mutate(Event_ID = as.character(Event_ID)) 
colnames(sampled_rdm_points)
colnames(fires)

Presence_Absence<-rbind(fires,sampled_rdm_points) 
  write_xlsx(Presence_Absence,'Output/Data/L3_Presence_Absence_Eight_RegionsEXTRAINDICES.xlsx')

unique(Presence_Absence$NA_L3NAME)

  
####L1 Eastern Temperate Forests ####
  L1_fires<-read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Objective 3 Fire Size by Climate and Woody Cover/Outputs/Output Data/Fire_IDs_with_weather_indices_and_vegetation_EXTRAindices.xlsx') %>% 
    select(-c(fire_size)) %>% 
    na.omit() %>% 
    mutate(percent_cover = percent_cover*100,
           count = n_distinct(Event_ID), 
           PA = 1) %>%
    select(-c(NA_L3NAME,US_L3CODE)) %>% 
    unique()



  L1_rdmpts_weather<-read_csv('Data/Year_of_fire_weather_rdm_pts_L1.csv')#had to rerun with the correct event_ids. 
  
  L1extra_weather<-read_csv("Data/L1buffers_fireweather_for_defense.csv")
  
 ##
  L1_rdmpts_weather<-read_csv('Data/Year_of_fire_weather_rdm_pts_L1.csv') %>% 
    select(where(~!any(is.na(.))))
  L1_extraindices<-read_csv("Data/L1_buffers_extraweather.csv")%>% 
    select(where(~!any(is.na(.))))

  L1_all_rdmpts_weather <- left_join(L1_rdmpts_weather, L1_extraindices, by = c("fire_date", "date", "Event_ID","pr")) %>% 
    na.omit() %>% 
    mutate(diff_days = as.numeric(difftime(as.Date(date), as.Date(fire_date), units = "days"))) 
  L1_summarized_weather<-L1_all_rdmpts_weather%>% 
    group_by(Event_ID) %>% 
    mutate(vpd_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
           vpd_10.1.window_mean = mean(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
           vpd_5.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
           vpd_1.1.window_mean = mean(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
           vs_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
           vs_10.1.window_mean = mean(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
           vs_5.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
           vs_1.1.window_mean = mean(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
           vs_10.1.window_max = max(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
           vs_5.1.window_max = max(vs[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
           vs_1.1.window_max = max(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
           fm100.10.5.window_mean = mean(fm100[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
           fm100.10.1.window_mean = mean(fm100[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
           fm100.5.5.window_mean = mean(fm100[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
           fm100.1.1.window_mean = mean(fm100[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
           fm1000.10.5.window_mean = mean(fm1000[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
           fm1000.10.1.window_mean = mean(fm1000[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
           fm1000.5.5.window_mean = mean(fm1000[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
           fm1000.1.1.window_mean = mean(fm1000[diff_days <= 1 & diff_days >= -1], na.rm = TRUE))%>% 
    select(-c(vpd,vs,date,pr,fm100,fm1000)) %>% #ditch the old columns
    unique()#remove duplicate entries 
  
 ## 
  count(unique(L1_rdmpts_weather))
   
 
 # rdm_points_L1<-read_csv('Data/year_of_fire_drought_stats_rdm_pts_L1.csv') %>% 
  rdm_points_L1<-read_csv('Data/L1buffers_drought_rerun_6_12_25.csv') %>% 
    mutate(diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
    filter(diff_days <= 5 & diff_days >=-5) %>%
    group_by(Event_ID) %>% 
    summarize(spei30 = mean(spei30d),
              spei14 = mean(spei14d),
              pdsi = mean(pdsi)) %>%
    left_join(L1_summarized_weather) %>% 
    left_join(read_csv('Data/L1_rdmpts_woody_ha.csv') %>% #I need rap data for these buffer points
                left_join(read_csv('Data/L1_rdmpts_evaluated_area_ha.csv')) %>%
                select(-US_L3CODE) %>% 
                mutate(percent_cover = (woody_hectares/Evaluated_Hectares)*100) %>%
                filter(Year == as.numeric(year(as_datetime(Ig_Date)))-1)) %>% #good up to here
 #   left_join(read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/ESA Soil Moisure Extraction/Output/Soil_moisture_rdm_pts_L1.csv') %>% 
   #             mutate(diff_days = as.numeric(difftime(ymd(date), ymd(Ig_Date), units = "days"))) %>% 
   #             filter(diff_days < 30 & diff_days >-30) %>% 
    #            group_by(Event_ID) %>% 
   #             summarize(sm = mean(sm))) %>% 
                mutate(PA = 0) %>% 
                na.omit() %>%  #this gets rid of sm NA values
                filter(Evaluated_Hectares > 200) %>% #this gets rid of random points placed in the wrong spots
                select(-c(Evaluated_Hectares,woody_hectares,Year,diff_days,fire_date)) %>% 
    unique() 
  
length(rdm_points_L1$percent_cover[rdm_points_L1$percent_cover <1])/9965


  #Create a function to sample data based on fire counts
  sample_targets <- function(data) {
    sample_size <-L1_fires$count[1] 
    sample_n(data, sample_size)
  }
  
  sampled_rdm_points_L1<-sample_targets(rdm_points_L1)
  colnames(sampled_rdm_points_L1)
  colnames(L1_fires)

  
 
  
  
Presence_Absence_L1<-rbind(L1_fires %>% 
                             select(-count)
                           ,sampled_rdm_points_L1)
  write_xlsx(Presence_Absence_L1,'Output/Data/Presence_Absence_L1_EXTRAINDICES.xlsx') 
  

  

  
  

