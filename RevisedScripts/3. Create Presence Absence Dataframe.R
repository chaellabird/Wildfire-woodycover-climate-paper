#The purpose of this data is to prepare presence/absence data, like in 
#chapter one.
####Load packages####
library(pacman)
pacman::p_load(tidyverse,readxl,sf,writexl)



####L1 Eastern Temperate Forests ####

#fire ids
fire_ids<-read_excel("Data/Fire Data/fires_Eastern_temperate_forest.xlsx") %>% 
  filter(Ig_Date >= ymd("1991-01-01") & Ig_Date <= ("2022-01-01")) %>% 
  select(-c(L3_KEY,NA_L3CODE,US_L3NAME)) %>% 
  group_by(NA_L3NAME) %>% 
  mutate(count = n())

fire_ids%>% 
  select(Event_ID) %>% 
  unique() %>% 
  nrow()#there are 2277 unique fire ids. 
#but there are 
fire_ids %>% 
  select(Event_ID,NA_L3NAME) %>% 
  unique() %>% 
  nrow() #there are 2227 fire region combinations


#woody cover
woody_cover<-read_csv("Data/Fire Data/Woody_Hectares_Fires_Revised.csv") %>%
  select(-US_L3CODE) %>% 
  full_join(read_csv("Data/Fire Data/Evaluated_Hectares_Fires_Revised.csv") %>% 
              select(-US_L3CODE),
            by = c("Event_ID", "Year")) %>% 
  full_join(read_excel("Data/Fire Data/fires_Eastern_temperate_forest.xlsx")%>% 
              mutate(US_L3CODE = as.character(US_L3CODE))) %>% 
  filter(year(Ig_Date)-1 == Year) %>% 
  mutate(PA = "1",
         percent_woody = woody_hectares/Evaluated_Hectares) %>% 
  select(-c(L3_KEY, NA_L3CODE, US_L3NAME,area_ha)) %>% 
  filter(Ig_Date >= ymd("1991-01-01") & Ig_Date <= ymd("2022-01-01"))




drought<-read_csv("Data/Fire Data/REVISED_mtbs_drought_stats.csv") %>% 
  mutate(diff_days_raw = as.numeric(difftime(date, fire_date, units = "days")),

# floor the positive values, round the negative ones “up” (to the less negative)
  diff_days = if_else(
    diff_days_raw > 0,
    floor(diff_days_raw),            # positive → floor
    ceiling(diff_days_raw)           # negative → go up (ceiling)
  )) %>% 
    # filter(diff_days <= 5 & diff_days >=-5,
    #        ) %>%
    group_by(Event_ID) %>% 
    # summarize(spei30 = mean(spei30d),
    #           spei14 = mean(spei14d),
    #           pdsi = mean(pdsi)) %>% 
 mutate(spei.10.10.window_mean= mean(spei14d[diff_days <= 10 & diff_days >= -10], na.rm = TRUE),
        spei.10.5.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -10], na.rm = TRUE),
        spei.5.5.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
        spei.5.1.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
        spei.3.3.window_mean= mean(spei14d[diff_days <= 3 & diff_days >= -3], na.rm = TRUE),
        spei.3.1.window_mean= mean(spei14d[diff_days <= 3 & diff_days >= -1], na.rm = TRUE),
        spei.1.1.window_mean= mean(spei14d[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
        pdsi.10.10.window_mean= mean(pdsi[diff_days <= 10 & diff_days >= -10], na.rm = TRUE),
        pdsi.10.5.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -10], na.rm = TRUE),
        pdsi.5.5.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
        pdsi.5.1.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
        pdsi.3.3.window_mean= mean(pdsi[diff_days <= 3 & diff_days >= -3], na.rm = TRUE),
        pdsi.3.1.window_mean= mean(pdsi[diff_days <= 3 & diff_days >= -1], na.rm = TRUE),
        pdsi.1.1.window_mean= mean(pdsi[diff_days <= 1 & diff_days >= -1], na.rm = TRUE)
        ) %>% 
  select(-c(date,fire_date,day_of_year,US_L3CODE, diff_days,diff_days_raw,spei14d,spei30d,pdsi)) %>% 
  unique() %>% 
 
          full_join(fire_ids, by = "Event_ID") %>% 
  filter(Ig_Date >= ymd("1991-01-01") & Ig_Date <= ("2022-01-01"))

weather<-read_csv("Data/Fire Data/ETF_fireweather_MTBS.csv") %>% 
  filter(fire_date >= ymd("1991-01-01") & fire_date <= ymd("2022-01-01")) %>% 
  mutate(diff_days = as.numeric(difftime(as.Date(date), as.Date(fire_date), units = "days"))) %>%
  group_by(Event_ID) %>% 
  mutate(vpd_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
       vpd_10.1.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
       vpd_10.5.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
       vpd_5.5.window_mean = mean(vpd[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
       vpd_1.1.window_mean = mean(vpd[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
       vpd_1.5.window_mean = mean(vpd[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
       vs_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
       vs_10.1.window_mean = mean(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
       vs_5.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
       vs_1.1.window_mean = mean(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
       vs_1.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -1], na.rm = TRUE)) %>% 
  select(-c(vpd,vs,date,fire_date,day_of_year,US_L3CODE,FID,diff_days,pr,fm100,fm1000)) %>% #ditch the old columns
  unique() %>% #at this point we have 2184 unique fires.
  full_join(fire_ids, by = "Event_ID") #we now have 2276. which isn't right. 
  

P<-full_join(woody_cover, drought) %>% 
  full_join(weather) %>% 
  mutate(PA = "1")


rm(weather, woody_cover,drought)




####Clean the random points ####
rdm_point_ids_filtered<-read_csv("Output/nlcd_prp_randlev1_cropshayfiltered.csv") %>% 
   select(NA_L3NAME, event_id) %>% 
   rename(Event_ID = "event_id")


L1_woody_cover<-read_csv("Data/Random Points Data/New Points Crop and Hay filtered/REVISED_2_rdmpoints_evaluated_area_ha1.csv") %>% 
  left_join(read_csv("Data/Random Points Data/New Points Crop and Hay filtered/REVISED_2_rdmpoints_woody_Ha1.csv")) %>% 
  filter(year(Ig_Date)-1 == Year) %>% 
  mutate(PA = "0",
         percent_woody = woody_hectares/Evaluated_Hectares) %>% 
  select(-c(US_L3CODE,FID)) %>% 
  filter(Ig_Date >= ymd("1990-01-01") & Ig_Date <= ymd("2021-01-01"),
         Event_ID %in% rdm_point_ids_filtered$Event_ID) #these have not been filtered for their nlcd



L1_rdmpts_drought<-read_csv("Data/Random Points Data/drought and weather less than max values in fires/REVISED_3_rdmpoints_drought_stats.csv") %>% 
 
  mutate(diff_days_raw = as.numeric(difftime(date, fire_date, units = "days")),
         
         # floor the positive values, round the negative ones “up” (to the less negative)
         diff_days = if_else(
           diff_days_raw > 0,
           floor(diff_days_raw),            # positive → floor
           ceiling(diff_days_raw)           # negative → go up (ceiling)
         )) %>% 
  group_by(Event_ID, fire_date) %>% 
  mutate(spei.10.10.window_mean= mean(spei14d[diff_days <= 10 & diff_days >= -10], na.rm = TRUE),
         spei.10.5.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -10], na.rm = TRUE),
         spei.5.5.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         spei.5.1.window_mean= mean(spei14d[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
         spei.3.3.window_mean= mean(spei14d[diff_days <= 3 & diff_days >= -3], na.rm = TRUE),
         spei.3.1.window_mean= mean(spei14d[diff_days <= 3 & diff_days >= -1], na.rm = TRUE),
         spei.1.1.window_mean= mean(spei14d[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         pdsi.10.10.window_mean= mean(pdsi[diff_days <= 10 & diff_days >= -10], na.rm = TRUE),
         pdsi.10.5.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -10], na.rm = TRUE),
         pdsi.5.5.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         pdsi.5.1.window_mean= mean(pdsi[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
         pdsi.3.3.window_mean= mean(pdsi[diff_days <= 3 & diff_days >= -3], na.rm = TRUE),
         pdsi.3.1.window_mean= mean(pdsi[diff_days <= 3 & diff_days >= -1], na.rm = TRUE),
         pdsi.1.1.window_mean= mean(pdsi[diff_days <= 1 & diff_days >= -1], na.rm = TRUE)
  ) %>% 
  select(-c(date,fire_date,day_of_year,US_L3CODE, diff_days,diff_days_raw,spei14d,pdsi)) %>% 
  unique()
 

#create a key for the dates of the random events
key<-L1_rdmpts_drought %>% 
  select(Event_ID, fire_date)



L1_rdmpts_weather<-read_csv('Data/Random Points Data/drought and weather less than max values in fires/REVISED_3_ETF_fireweather_rdmpoints.csv') %>% 
  left_join(key) %>% 
 select(-c(Ig_date,US_L3CODE,FID,day_of_year)) %>% 
  left_join(L1_rdmpts_drought %>% 
             select(Event_ID,fire_date)) %>% 
  mutate(diff_days = as.numeric(difftime(as.Date(date), as.Date(fire_date), units = "days"))) %>% 
  group_by(Event_ID) %>% 
  mutate(vpd_10.5.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         vpd_10.1.window_mean = mean(vpd[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         vpd_5.5.window_mean = mean(vpd[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         vpd_1.1.window_mean = mean(vpd[diff_days <= 1 & diff_days >= -1], na.rm = TRUE),
         vpd_1.5.window_mean = mean(vpd[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
         vs_10.5.window_mean = mean(vs[diff_days <= 10 & diff_days >= -5], na.rm = TRUE),
         vs_10.1.window_mean = mean(vs[diff_days <= 10 & diff_days >= -1], na.rm = TRUE),
         vs_1.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -1], na.rm = TRUE),
         vs_5.5.window_mean = mean(vs[diff_days <= 5 & diff_days >= -5], na.rm = TRUE),
         vs_1.1.window_mean = mean(vs[diff_days <= 1 & diff_days >= -1], na.rm = TRUE)) %>% #,
         
  select(-c(vpd,vs,date,diff_days,pr,fire_date)) %>% 
  unique()#remove duplicate entries 



A<- L1_rdmpts_drought %>% 
  full_join(L1_rdmpts_weather) %>% 
  full_join(L1_woody_cover) %>% 
  left_join(rdm_point_ids_filtered) %>% #this is necessary for the na_l3name. 
  mutate(PA = 0) %>% 
  ungroup() %>% 
  select(-c(Evaluated_Hectares,woody_hectares,Year,fire_date,FID,fire_day_of_year)) %>% 
  unique() 

#check colnames
col_A <-colnames(A)
col_P <-colnames (P)
col_P[col_P %in% col_A]

P<-P %>% 
  select(col_P[col_P %in% col_A])

Presence_Absence_L3<-rbind(P,A)

#wait to write the file till we decide which window we want. 


set.seed(23)

duplications <- Presence_Absence_L3 %>% 
  group_by(Event_ID) %>%   # split by Event_ID
  filter(n() > 1)          # keep only groups with >1 row

regionswithduplicates<-duplications %>% 
  select(NA_L3NAME) %>% 
  unique()
keep<-duplications %>% 
  group_by(Event_ID) %>% 
  slice_sample(n = 1)
not_keep<-anti_join(duplications,keep)
discard_number<- not_keep %>% 
  group_by(NA_L3NAME) %>% 
  summarize(n_trash = n())

Presence_Absence_L1<-Presence_Absence_L3 %>% 
  anti_join(not_keep) %>% 
  group_by(NA_L3NAME) %>% 
  group_split()

Presence_Absence_L1 <- lapply(Presence_Absence_L1, function(region){
    region_name = unique(region$NA_L3NAME)
    print(region_name)
    region_fire_reserve<-region %>% 
      filter(PA == 1)
    trash_number = discard_number %>% 
      filter(NA_L3NAME == region_name) %>% 
      pull(n_trash)
    print(trash_number)
    if (length(trash_number) > 0){
      region %>%
        filter(PA == 0) %>% 
        slice_sample(n = -trash_number) %>% 
        bind_rows(region_fire_reserve)#exclude this many rows
      
    }else {
      region
    }
    
    }) %>% 
  bind_rows() 


length(Presence_Absence_L1 %>% 
         filter(PA == 1) %>% 
         pull(NA_L3NAME))


#we now have 2184 unique values for presence and 2184 for absence. 


####Test different averaging windows####

#we should decide which averaging windows for the weather variables are the best. 

#run some models!!!

rm(rdm_point_ids_filtered, regionswithduplicates,not_keep, L1_woody_cover,L1_rdmpts_weather,L1_rdmpts_drought,keep,fire_ids,duplications,A,P,discard_number)


####L1 scale #### Just gonna go with this one. 
p_load(glmmTMB, lme4, MuMIn)
df_l1 <-Presence_Absence_L1 %>% 
  mutate(PA = as.factor(PA))

#VPD #

vpd_cols <- grep("vpd", colnames(Presence_Absence_L1), value = TRUE)
vpd_cols

model_10.5<-glmmTMB(PA ~ vpd_10.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_10.1<-glmmTMB(PA ~ vpd_10.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.5<-glmmTMB(PA ~ vpd_5.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_1.1<-glmmTMB(PA ~ vpd_1.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_1.5<-glmmTMB(PA ~ vpd_1.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
AICc(model_10.5,
     model_10.1,
     model_5.5,
     model_1.1,
     model_1.5)

#okay so for vpd 1.1 is the best by far. 

#VS #



vs_cols <- grep("vs", colnames(Presence_Absence_L1), value = TRUE)
vs_cols

model_10.5_vs<-glmmTMB(PA ~ vs_10.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_10.1_vs<-glmmTMB(PA ~ vs_10.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.5_vs<-glmmTMB(PA ~ vs_5.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_1.1_vs<-glmmTMB(PA ~ vs_1.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_1.5_vs<-glmmTMB(PA ~ vs_1.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
summary(model_1.1_vs)
AICc(model_10.5_vs,
     model_10.1_vs,
     model_5.5_vs,
     model_1.1_vs,
     model_1.5_vs)

#14 day spei
spei_cols <- grep("spei", colnames(Presence_Absence_L1), value = TRUE)
spei_cols
model_10.10_spei<-glmmTMB(PA ~ spei.10.10.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_10.5_spei<-glmmTMB(PA ~ spei.10.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.5_spei<-glmmTMB(PA ~ spei.5.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.1_spei<-glmmTMB(PA ~ spei.5.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_3.3_spei<-glmmTMB(PA ~ spei.3.3.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_3.1_spei<-glmmTMB(PA ~ spei.3.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
summary(model_5.5_spei)
summary(model_5.1_spei)
summary(model_3.3_spei)
summary(model_3.1_spei)

AICc(model_10.10_spei,
     model_10.5_spei,
    model_5.5_spei,
     model_5.1_spei,
     model_3.3_spei,
     model_3.1_spei)

summary(df_l1$spei.10.10.window_mean)
summary(df_l1$spei.10.5.window_mean)
summary(df_l1$spei.5.5.window_mean)
summary(df_l1$spei.5.1.window_mean)
summary(df_l1$spei.3.3.window_mean)
summary(df_l1$spei.3.1.window_mean)

#there should no NAs...

pdsi_cols <- grep("pdsi", colnames(Presence_Absence_L1), value = TRUE)
pdsi_cols

model_10.10_pdsi<-glmmTMB(PA ~ pdsi.10.10.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_10.5_pdsi<-glmmTMB(PA ~ pdsi.10.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.5_pdsi<-glmmTMB(PA ~ pdsi.5.5.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_5.1_pdsi<-glmmTMB(PA ~ pdsi.5.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_3.3_pdsi<-glmmTMB(PA ~ pdsi.3.3.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
model_3.1_pdsi<-glmmTMB(PA ~ pdsi.3.1.window_mean + (1|NA_L3NAME), data = df_l1, family = binomial)
summary(model_5.5_pdsi)
summary(model_5.1_pdsi)
summary(model_3.3_pdsi)
summary(model_3.1_pdsi)

AICc(model_10.10_pdsi,
     model_10.5_pdsi,
     model_5.5_pdsi,
     model_5.1_pdsi,
     model_3.3_pdsi,
     model_3.1_pdsi)




#for pdsi 5.1 is the best. 
nas_in_spei<-df_l1[is.na(df_l1$spei.5.1.window_mean)==TRUE,]
nas_in_pdsi<-df_l1[is.na(df_l1$pdsi.5.1.window_mean)==TRUE,]


rm(list = ls(pattern = "model"))



Presence_Absence_L3_restricted<-Presence_Absence_L3 %>% 
select(-c(vpd_10.5.window_mean, vpd_10.1.window_mean, vpd_5.5.window_mean,
          vs_10.5.window_mean, vs_10.1.window_mean, vs_5.5.window_mean, 
          spei.10.5.window_mean, spei.5.5.window_mean, spei.3.3.window_mean, spei.3.1.window_mean, spei.1.1.window_mean,
          pdsi.10.5.window_mean, pdsi.5.5.window_mean, pdsi.3.3.window_mean, pdsi.3.1.window_mean, pdsi.1.1.window_mean
))

write_xlsx(Presence_Absence_L3_restricted,'Output/Revised_Presence_Absence_L3_crops_hay_filtered.xlsx') 

Presence_Absence_L1_restricted<-Presence_Absence_L1 %>% 
  select(-c(vpd_10.5.window_mean, vpd_10.1.window_mean, vpd_5.5.window_mean,
  vs_10.5.window_mean, vs_10.1.window_mean, vs_5.5.window_mean,
  spei.10.5.window_mean, spei.5.5.window_mean, spei.3.3.window_mean, spei.3.1.window_mean, spei.1.1.window_mean,
  pdsi.10.5.window_mean, pdsi.5.5.window_mean, pdsi.3.3.window_mean, pdsi.3.1.window_mean, pdsi.1.1.window_mean
  ))


write_xlsx(Presence_Absence_L1_restricted,'Output/Revised_Presence_Absence_L1_crops_hay_filtered.xlsx') 
