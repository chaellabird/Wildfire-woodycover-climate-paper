library(pacman)
pacman::p_load(tidyverse,readxl)

l3_key<-read_excel('Output/l3_ecoregions.xlsx') %>% 
  select(NA_L3NAME,US_L3CODE)


old_rap_shrubs<-read_csv('C:/Users/micha/Thesis Data/GEE Evaluated cover/L3ecoregion_evaluted/L3_Ecoregions_SHR.csv')
old_rap_trees<-read_csv('C:/Users/micha/Thesis Data/GEE Evaluated cover/L3ecoregion_evaluted/L3_Evaluated_Hectares_Trees.csv')
old_rap_area<-read_csv('C:/Users/micha/Thesis Data/GEE Evaluated cover/L3ecoregion_evaluted/L3_Ecoregion_evaluated.csv')

old_woody_cover<-left_join(old_rap_shrubs,old_rap_trees) %>%
  left_join(old_rap_area) %>% 
  mutate(woody_cover = SHR_Hectares + Tree_Hectares,
         percent_cover = woody_cover/Evaluated_Hectares) %>% 
  left_join(l3_key) %>% 
  filter(Year > 1989,
         NA_L3NAME != "Lake Erie Lowland") %>% 
  select(-c(SHR_Hectares,Tree_Hectares)) %>% 
  mutate(US_L3CODE = as.numeric(US_L3CODE))


new_tree<-read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Obj 1 Trends in Woody Cover Over Time/Data/RAP Data/L3_Evaluated_Hectares_Trees.csv')
new_shrub<-read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Obj 1 Trends in Woody Cover Over Time/Data/RAP Data/L3_Evaluated_Hectares_Shrubs.csv')
new_area<-read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 1 Data Analysis/Obj 1 Trends in Woody Cover Over Time/Data/RAP Data/L3_evaluated_total_hectares.csv')

new_woody_cover<-left_join(new_tree,new_shrub) %>%
  left_join(new_area) %>% 
  mutate(new_woody_cover = Tree_Hectares + SHR_Hectares,
         new_percent_cover = new_woody_cover/Evaluated_Hectares) %>% 
  select(-c(Tree_Hectares,SHR_Hectares)) %>% 
  filter(Year <2021) %>% 
  rename(US_L3CODE = L3CODE,
         new_evaluated_Hectares = Evaluated_Hectares) 

rm(old_rap_shrubs,old_rap_trees,new_shrub,new_tree)


combined<-left_join(new_woody_cover,old_woody_cover)
  colnames(combined)
mismatched<-combined %>%
  mutate(difference_woody_cover = abs(new_woody_cover -woody_cover),
         diff_eval_ha = abs(new_evaluated_Hectares-Evaluated_Hectares),
         diff_percent_cover = abs(new_percent_cover - percent_cover)) %>% 
  filter(difference_woody_cover >2) %>% 
  mutate(percent_difference = difference_woody_cover/new_evaluated_Hectares)
max(mismatched$diff_percent_cover)

#this is a big problem. What shall we do about it?