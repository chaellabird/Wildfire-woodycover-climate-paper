# install.packages("pacman") # if not already installed


#This script restricts NLCD for the selected YEAR. I will go on to the next script (in python) (tbw)
#to assign three dates to each point. 


library(pacman)
pacman::p_load(data.table, googledrive, tidyverse, FedData,
               snakecase, here, patchwork, RColorBrewer,readxl,purrr) #always load data.table first. 


nlcd <- nlcd_colors() %>% as_tibble() %>% #this is a key for the class names
  mutate(class = to_snake_case(Class)) #add a cute little _ between the words.

#it's a key for names and codes.
l3_eco <-read_excel("../Pre-process Spatial Data/Output/l3_ecoregions_key.xlsx")

#bring in keys for fires, points, and regions
rdm_region_key<-read_csv("Output/Rdm_points_region_key.csv")%>% 
  rename(event_id = "Event_ID")
fire_region_key<-read_excel("Data/Fire Data/fires_Eastern_temperate_forest.xlsx"
) %>% 
  rename(event_id = "Event_ID") %>% 
  filter(Ig_Date >= ymd("1990-01-01") & Ig_Date <= "2022-01-01") #2224 now. That's the number of points we need to end with. 

  

wildfire1 <- read_csv("Data/Fire Data/nlcd_prp_wildfire_ETF.csv") %>%
  left_join(nlcd, by = c("nlcd_class" = "ID")) %>%
  full_join(fire_region_key, by = "event_id", relationship = "many-to-many") %>% #2277 here. correct number
  select(-nlcd_class) %>%
  select(event_id,NA_L3NAME, nlcd_class = class, nlcd_prop) %>%
  pivot_wider(names_from = nlcd_class, values_from = nlcd_prop) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
   #mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(Ig_Date = ymd(str_extract(event_id, "\\d{8}$"))) %>%
  # filter Ig_Date to be between 1990 and 2020
  filter(Ig_Date >= ymd("1991-01-01") & Ig_Date <= "2022-01-01") #okay it turns out that its a fine number
#we just filtered out the fires outside of our time period. 

#save the pivoted file for later


write_csv(wildfire1, "Output/NLCD_proportion_pivoted_wildfires.csv")


na_rows <- wildfire1 %>% filter(if_any(everything(), is.na))

wildfire <- wildfire1 %>%
  group_by(NA_L3NAME) %>% 
  rowwise() %>%
  mutate(grass = grassland_herbaceous,
         woody = sum(c_across(c(evergreen_forest, deciduous_forest,
                                mixed_forest, shrub_scrub))),
         crop = cultivated_crops, hay = pasture_hay,
         developed = sum(c_across(c(developed_open_space,
                                    developed_low_intensity,
                                    developed_medium_intensity,
                                    developed_high_intensity)))) %>%
  ungroup() %>%
  select(NA_L3NAME, Event_ID = event_id, grass, woody, crop, hay, developed) %>%
  mutate(across(everything(), ~ replace_na(., 0)))


wildfire %>% 
  select(NA_L3NAME,Event_ID) %>% 
  unique() %>% 
  nrow()
#at this point we have 2277. 


fire_sum <- wildfire %>%
  group_by(NA_L3NAME) %>%                                # keep the group column intact
  summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE))) %>%
  pivot_longer(-NA_L3NAME,   # exclude the group column from the pivot
               names_to = "nlcd_class",
               values_to = "mean_nlcd_prop")

 p1_l1 <- 
  ggplot(fire_sum, aes(x = "", y = mean_nlcd_prop,
                       fill = nlcd_class)) +
  facet_wrap(~NA_L3NAME)+
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +
  labs(fill = "NLCD Class", title = "Wildfire")

p1_l1

# Read in the random level 1 data -----------------------------------



randlev1a <- read_csv("Data/Random Points Data/nlcd_prp_rdm_points_REVISED1_ETF.csv") %>% #bring in my random data as intersected with NLCD.
  left_join(nlcd, by = c("nlcd_class" = "ID")) %>%
  full_join(rdm_region_key, by = "event_id", relationship = "many-to-many") %>% 
  select(-nlcd_class) %>%
  select(NA_L3NAME, event_id, nlcd_class = class, nlcd_prop) %>%
  pivot_wider(names_from = nlcd_class, values_from = nlcd_prop) %>%
  mutate(across(everything(), ~ replace_na(., 0)))
  
write_csv(randlev1a, "Output/NLCD_proportion_pivoted_randompoints.csv")
set.seed(123)

max_values_dict<- wildfire1 %>% 
  group_by(NA_L3NAME) %>% 
  summarize(across(where(is.numeric), max, .names = "{col}")) 

#####exclude points that are outside the thresholds for water and development #####

l1_ids <- randlev1a %>%
  group_by(NA_L3NAME) %>% 
  group_split() 

l1_ids_filt<- map(l1_ids, function(df){
  key <- unique(df$NA_L3NAME)
  limit<-max_values_dict %>% 
    filter(NA_L3NAME == key) 
  
  df %>% filter(open_water > limit$open_water,
           developed_open_space > limit$developed_open_space |
           developed_low_intensity > limit$developed_low_intensity |
           developed_medium_intensity > limit$developed_medium_intensity |
           developed_high_intensity > limit$developed_high_intensity) %>%
  select(unique_id = event_id) %>%
  mutate(unique_id = as.character(unique_id))
}) %>% 
  bind_rows()


randlev1_filtered <- randlev1a %>%
  filter(!event_id %in% l1_ids_filt$unique_id) 

rm(max_values_dict, fire_region_key,rdm_region_key)



counts_after_filteringstep1<-randlev1_filtered %>% 
  group_by(NA_L3NAME) %>% 
 mutate(counts_leftover = n()) %>% 
  select(NA_L3NAME,counts_leftover) %>% 
  unique()


####Bin and cut based on cultivated crops#### 


# wildfire bin proportions (ensure character bins)
wildfire_bins_crops <- wildfire1 %>%
  group_by(NA_L3NAME) %>% 
   mutate(bin = cut(cultivated_crops,
                    breaks = seq(0, 1, 0.05), #5 % bin from 0 to 1.
                    include.lowest = TRUE)) %>%
  count(bin, name = "wf_n") %>% #count the number of things in the bin and calling it wildfire number.
  mutate(prop = wf_n / sum(wf_n), #proportion of the wildfires
         bin = as.character(bin)) #making this an id column. 


# bin randoms the same way
randlev1_binned <- randlev1_filtered %>%
  group_by(NA_L3NAME) %>% 
  mutate(bin = cut(cultivated_crops,
                   breaks = seq(0, 1, 0.05),
                   include.lowest = TRUE)) %>%
 # count(bin, name = "rdm_points_n") %>%
  mutate(bin = as.character(bin))


# join bin PROPORTIONS to randoms
#we will be keeping stuff based on the proportions
randlev1_joined <- randlev1_binned %>%
  left_join(wildfire_bins_crops 
            #%>% select(NA_L3NAME, bin, prop)
            , by = c("bin","NA_L3NAME")) %>%
  mutate(prop = replace_na(prop, 0),
         wf_n = replace_na(wf_n, 0))



# determine how many total randoms to keep (same as wildfire count)
total_keep <-wildfire1 %>% 
  group_by(NA_L3NAME) %>% 
  summarise(total_keep = n()*5, .groups = "drop") %>% 
  mutate(padded = case_when(total_keep <90 ~ total_keep +150,
                            TRUE ~ total_keep+5)) #make sure we have at least 15 points, to avoid rounding issues.

#edit total keep to get some more padding for the smaller regions

  



# compute per-bin sample sizes normalized to total_keep

bin_keep_crops <- randlev1_joined %>%
  left_join(total_keep, by = "NA_L3NAME") %>% 
  group_by(NA_L3NAME) %>% 
  distinct(bin, prop, padded) %>% #only keep distinct bins and proportions. This step reduces it to 500 samples
  mutate(n_keep = round(prop / sum(prop) * padded)) %>%  #per bin, keep the number of samples to keep per bin. 
  ungroup()
# sample from each bin without replacement



#this is just a random sampling loop
randlev1_matched <- map_dfr(seq_len(nrow(bin_keep_crops)), \(i) {
  n_l3   <- bin_keep_crops$NA_L3NAME[i]
  b <- bin_keep_crops$bin[i] #bin value for this row
  n_keep <- bin_keep_crops$n_keep[i] #how many values we want to keep for this bin
  #if (n_keep < 20){
  #  n_keep =n_keep+5
  #  print("not enough points being kept for later, adding a 10 point padding")
  #  }
  
  #here is the error. We want to be selecting actual IDS. 
  subset <- randlev1_binned %>%  filter(NA_L3NAME == n_l3, bin == b) #all rows that belong to bin b
  if (nrow(subset) == 0) return(NULL) #no data for this bin. Skip
  slice_sample(subset, n = min(n_keep, nrow(subset)), replace = FALSE) #randomly pick rows fom this bin
}) #%>%
#  select(-bin, -prop)

#Separate out the ones that we are not using.
removed_event_ids_after_crop_removal <- anti_join(randlev1_binned, randlev1_matched,
                               by = "event_id") %>%
  mutate(unique_id = as.character(event_id)) %>% 
  select(c(NA_L3NAME,unique_id))

#at this point randlev1matched is the dataframe that is moving forward into the next step.

#save randlev1matched for use in creating a presence absence dataframe. 

counts_after_filteringstep2<-randlev1_matched%>% 
  group_by(NA_L3NAME) %>% 
  mutate(counts_leftover = n()) %>% 
  select(NA_L3NAME,counts_leftover) %>% 
  unique() %>% 
  full_join(wildfire %>% 
              group_by(NA_L3NAME) %>% 
              mutate(counts_fires = n()) %>% 
              select(NA_L3NAME,counts_fires) %>% 
              unique()) %>% 
  mutate(excess = counts_leftover - counts_fires) %>% 
  left_join(total_keep)


set.seed(123)

# compute medians
#  Medians before sampling -----------------------------------------------
med_before_tbl <- bind_rows(
  wildfire1  %>%
    group_by(NA_L3NAME)                %>%
    summarise(med_before = median(cultivated_crops, na.rm = TRUE)) %>%
    mutate(type = "Wildfire") %>% select(NA_L3NAME, type, med_before),
  
  randlev1a  %>%
    group_by(NA_L3NAME)                %>%
    summarise(med_before = median(cultivated_crops, na.rm = TRUE)) %>%
    mutate(type = "Random")   %>% select(NA_L3NAME, type, med_before)
)

# Median values *after* sampling ------------------------------------------
med_after_tbl <- bind_rows(
  wildfire1        %>%
    group_by(NA_L3NAME)                %>%
    summarise(med_after = median(cultivated_crops, na.rm = TRUE)) %>%
    mutate(type = "Wildfire") %>% select(NA_L3NAME, type, med_after),
  
  randlev1_matched %>%
    group_by(NA_L3NAME)                %>%
    summarise(med_after = median(cultivated_crops, na.rm = TRUE)) %>%
    mutate(type = "Random")   %>% select(NA_L3NAME, type, med_after)
)

#  Merge both tables ➜ one tidy tibble --------------------------------------
med_before_after <- full_join(med_before_tbl, med_after_tbl,
                              by = c("NA_L3NAME", "type"))




write_csv(randlev1_matched, here("Output", "nlcd_prp_randlev1_CROPS_filtered.csv"))


####Run the binning and excluding again for hay ####
wildfire_bins_hay <- wildfire1 %>%
  group_by(NA_L3NAME) %>% 
  mutate(bin = cut(pasture_hay,
                   breaks = seq(0, 1, 0.1), #10 % bin from 0 to 1.
                   include.lowest = TRUE)) %>%
  count(bin, name = "wf_n") %>% #count the number of things in the bin and calling it wildfire number.
  mutate(prop = wf_n / sum(wf_n), #proportion of the wildfires
         bin = as.character(bin)) #making this an id column. 


# bin randoms the same way
randlev1_binned <- randlev1_matched %>% #start with the output from the last step
  group_by(NA_L3NAME) %>% 
  mutate(bin = cut(pasture_hay,
                   breaks = seq(0, 1, 0.1),
                   include.lowest = TRUE)) %>%
 # count(bin, name = "rdm_points_n") %>%
  mutate(bin = as.character(bin))

colnames(randlev1_binned)
colnames(wildfire_bins_crops)
# join bin PROPORTIONS to randoms
#we will be keeping stuff based on the proportions
randlev1_joined <- randlev1_binned %>%
  left_join(wildfire_bins_hay 
            #%>% select(NA_L3NAME, bin, prop)
            , by = c("bin","NA_L3NAME")) %>%
  mutate(prop = replace_na(prop, 0),
         wf_n = replace_na(wf_n, 0))



# determine how many total randoms to keep (same as wildfire count)
total_keep <-wildfire1 %>% 
  group_by(NA_L3NAME) %>% 
  #summarise(total_keep = n()*10, .groups = "drop") 
  summarise(total_keep = n()*1,.groups = "drop") 
# compute per-bin sample sizes normalized to total_keep
sum(total_keep$total_keep) #this number is correct
#somewhere along the line we are not getting enough points.




bin_keep_hay <- randlev1_joined %>% #should be the same number of rows as randlev1matched
  left_join(total_keep, by = "NA_L3NAME") %>% 
  group_by(NA_L3NAME) %>% 
  distinct(bin, prop, total_keep) %>% #only keep distinct bins and proportions. This step reduces it to 500 samples
 # mutate(n_keep = round(prop / sum(prop) * total_keep)) %>%  #per bin, keep the number of samples to keep per bin. 
  mutate(n_keep = prop / sum(prop) * total_keep) %>%
  ungroup() %>% 
  mutate(n_keep = replace_na(n_keep, 0))

sum(bin_keep_hay$n_keep)  #2275 when we need 2277. 
#getting rid of the round means we are only missing one.(2276) 

#now check by region the numbers
check_count_desired<-bin_keep_hay %>% 
  group_by(NA_L3NAME) %>% 
  mutate(keep_count = sum(n_keep)) %>% 
  left_join(counts_after_filteringstep2 %>% 
              select(NA_L3NAME, counts_fires)) %>% 
  mutate(selection_excess = keep_count - counts_fires)


# sample from each bin without replacement

#sample for the ones we are actually going to model with. 
####Left off here####

#this is just a random sampling loop for hay
randlev1_matched_hay <- map_dfr(seq_len(nrow(bin_keep_hay)), \(i) {
  n_l3   <- bin_keep_hay$NA_L3NAME[i]
  b <- bin_keep_hay$bin[i] #bin value for this row
  n_keep <- floor(bin_keep_hay$n_keep[i]) #how many values we want to keep for this bin
 #create a step to keep at least one if the total keep for the region is one. 
  
   print(paste0("working with ", n_l3, "for bin: ", b, "to get ", n_keep, "values"))
  #here is the error. We want to be selecting actual IDS. 
  subset <- randlev1_binned  %>%
    filter(NA_L3NAME == n_l3, bin == b)#all rows that belong to bin b
  if (nrow(subset) == 0) {
    message("no data in this subset")   # or cat("this bin empty\n")
    return(NULL)
  } #no data for this bin. Skip
  slice_sample(subset, n = min(n_keep, nrow(subset)), replace = FALSE) #randomly pick rows from this bin
}) #%>%
#  select(-bin, -prop)




counts_after_filteringstep3<-randlev1_matched_hay%>% 
  group_by(NA_L3NAME) %>% 
  mutate(counts_leftover = n()) %>% 
  select(NA_L3NAME,counts_leftover) %>% 
  unique() %>% 
  full_join(wildfire %>% 
              group_by(NA_L3NAME) %>% 
              mutate(counts_fires = n()) %>% 
              select(NA_L3NAME,counts_fires) %>% 
              unique()) %>% 
  mutate(excess = counts_leftover - counts_fires)

# we are short a point in the interior river valleys and hills. It's supposed to have 4. It looks like
#there just aren't any points in those bins. That's a problem. 

#if that's the case, then increasing it won't change the missing value. 
interior<-randlev1_binned %>% 
  filter(NA_L3NAME == "Interior River Valleys and Hills")
interior_matched <-randlev1_matched_hay %>% 
  filter(NA_L3NAME == "Interior River Valleys and Hills")


#Interior River Valleys and Hills is missing a value in the (0.8,0.9 hay bin range)
#i'm going to add a point from the (0.7,0.8) hay bin range. 
randlev1_matched_hay <- randlev1_matched_hay %>% 
  bind_rows(
    slice_sample(
      randlev1_binned %>% filter(NA_L3NAME == "Interior River Valleys and Hills",
                                 bin == "(0.7,0.8]"),
      n = 1
    )
  )

#this is a little non-kosher, but its the closest I can get. 



#Separate out the ones that we are not using.
removed_event_ids_cropsandhay <- anti_join(randlev1_binned, randlev1_matched_hay,
                               by = "event_id") %>%
  mutate(unique_id = as.character(event_id))


set.seed(123)

# compute medians
med_before <- tibble(type = c("Wildfire", "Random"),
                     med = c(median(wildfire1$pasture_hay, na.rm = TRUE),
                             median(randlev1_matched$pasture_hay, na.rm = TRUE)))

med_after <- tibble(type = c("Wildfire", "Random"),
                    med = c(median(wildfire1$pasture_hay, na.rm = TRUE),
                            median(randlev1_matched_hay$pasture_hay, na.rm = TRUE)))

mean_before <-tibble(type = c("Wildfire", "Random"),
                     mean = c(mean(wildfire1$pasture_hay, na.rm = TRUE),
                              mean(randlev1_matched$pasture_hay, na.rm = TRUE)))
mean_after<- tibble(type = c("Wildfire", "Random"),
                    mean = c(mean(wildfire1$pasture_hay, na.rm = TRUE),
                             mean(randlev1_matched_hay$pasture_hay, na.rm = TRUE)))
med_before
med_after
mean_before
mean_after




####now to save the data####
 

#save all the stuff for later.
write_csv(randlev1_matched_hay, here("Output", "nlcd_prp_randlev1_cropshayfiltered.csv"))

####GO ON TO PYTHON SCRIPT 4 ASSIGN MULTIPLE DATES TO RANDOM POINTS####



#look at the distribution. For the curious. 
# ---- before matching ----
 before <- ggplot() +
   facet_wrap(~NA_L3NAME)+
   geom_histogram(data = wildfire1, aes(x = pasture_hay),
                  fill = "red", alpha = 0.4, bins = 20) +
   geom_histogram(data = randlev1a %>%
                    slice_sample(n = nrow(wildfire1)),
                  aes(x = pasture_hay),
                  fill = "blue", alpha = 0.4, bins = 20) +
   #geom_vline(data = med_before %>% filter(type == "Wildfire"),
    #          aes(xintercept = med), color = "red", linetype = "dashed") +
   #geom_vline(data = med_before %>% filter(type == "Random"),
   #           aes(xintercept = med), color = "blue", linetype = "dashed") +
   theme_classic() +
   labs(title = "Before Matching",
        x = "Cultivated Crops", y = "Count")
 ggsave(here("Output","figures","nlcd_pasture", "Haymatch before.jpg"),
        width = 16, height = 8, dpi = 600) 
 # ---- after matching ----
 after <- ggplot() +
   facet_wrap(~NA_L3NAME)+
   geom_histogram(data = wildfire1 %>% 
                    filter(NA_L3NAME %in% counts_keep$NA_L3NAME), aes(x = pasture_hay),
                  fill = "red", alpha = 0.4, bins = 20) +
   geom_histogram(data = randlev1_matched_hay %>% 
                    filter(NA_L3NAME %in% counts_keep$NA_L3NAME), aes(x = pasture_hay),
                  fill = "blue", alpha = 0.4, bins = 20) +
   labs(caption= "Blue is random points and red is wildfire")+
   theme_classic() +
   labs(title = "After Matching",
        x = "Pasture/Hay", y = "Count")
 ggsave(here("Output","figures","nlcd_pasture", "Haymatch after.jpg"),
        width = 16, height = 8, dpi = 600) 

 
#look at distribution of forests. 
 
forest_random<- read_csv(here("Output", "nlcd_prp_randlev1_filtered.csv"))%>% 
  mutate(forest = rowSums(across(c(deciduous_forest, mixed_forest, evergreen_forest)), na.rm = TRUE)) %>% 
  mutate(event_id = as.character(event_id),
         PA = "0")

forest_fires<-read_csv("Output/NLCD_proportion_pivoted_wildfires.csv") %>% 
  mutate(forest = rowSums(across(c(deciduous_forest, mixed_forest, evergreen_forest)), na.rm = TRUE),
         PA = "1")
forest_together<-bind_rows(forest_random,forest_fires)


counts_keep<-forest_together %>% 
  filter(PA == 1) %>% 
  group_by(NA_L3NAME) %>% 
  mutate(n= n()) %>% 
  select(NA_L3NAME, n) %>% 
  unique() %>% 
  filter(n > 80)
ggplot(data = forest_together %>% 
         filter(NA_L3NAME %in% counts_keep$NA_L3NAME), aes(x = forest, fill = PA))+
  geom_histogram(position = "identity", alpha = 0.5)+
  facet_wrap(~NA_L3NAME, scales = "free")
ggsave("Output/figures/ForestProportionPostFiltering.jpg")
 
 
 dpi   <- 300               
scaling <- dpi / 96
library(ragg)
 agg_png("Output/figures/nlcd_Pasture/Hay_match.jpg", 
         width = 9,
         height = 13,
         units = "in",
         scaling = 1)

 before # before + after + plot_layout(ncol = 2, guides = "collect") &
 #   theme(legend.position = "bottom") &
 #   labs(x = "Pasture/Hay", y = "Count", fill = "Type")
 dev.off()






