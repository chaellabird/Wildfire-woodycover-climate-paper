library(pacman)
pacman::p_load(tidyverse,readxl,gt,flextable,writexl,questionr)

#Read in data
data<-read_excel('Output/Data/Binomial_GLM_Model_selection_table.xlsx') %>% 
  mutate(logLik = round(logLik,2),
         AICc = round(AICc,2),
         delta = round(delta,2),
         weight = round(weight,2),
         Ecoregion = case_when(Ecoregion == "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" ~ "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                               Ecoregion == "OZARK/OUACHITA-APPALACHIAN FORESTS" ~ "L2 Ozark-Ouchita-Appalachian Forests",
                               Ecoregion == "SOUTHEASTERN USA PLAINS" ~"L2 Southeastern USA Plains",
                               TRUE ~ Ecoregion), 
         Ecoregion = factor(Ecoregion,
                            levels =c("L1 Eastern Temperate Forests",
                                      "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                                      "L2 Ozark-Ouchita-Appalachian Forests",
                                      "L2 Southeastern USA Plains",
                                      "Arkansas Valley",
                                      "Blue Ridge",
                                      "Central Appalachians",
                                      "Middle Atlantic Coastal Plain",
                                      "Ouachita Mountains",
                                      "Ozark Highlands",
                                      "Ridge and Valley",
                                      "South Central Plains",
                                      "Southeastern Plains",
                                      "Southern Coastal Plain",
                                      "Southwestern Appalachians")))%>% 
  flextable() %>%
  set_header_labels(.,
                    Ecoregion           = "Ecoregion",
                    Model_Terms         = "Model",
                    df                  = "DF",
                    logLik              = "LogL",
                    AICc                = "AICc",
                    delta               = "ΔAICc",
                    weight              = "Weight") %>%
  #set_table_properties(layout = "autofit") %>%
  width(width = c(1.6, 2.35, 0.4,0.75, 0.75, 0.6, 0.65)) %>%
  align(align = "left", part = "all") %>%
  align(j = "Ecoregion", align = "center", part = "all") %>%  
  bold(j = "Model_Terms", bold = TRUE, part = "body") %>% 
  bold(part = "header")  

save_as_docx(file = data,
             path = "Output/Graphs and Figures/Binomial_GLM_AIC_table.docx")


####Create a table for coefficients####
#I should get the 2.5% and 95% confidence intervals, instead
#of z and se. 
#as well as odds ratio. 



coefficients<-read_excel('Output/Data/Binomial_GLM_Model_coefficients.xlsx') %>% 
  # rename(std.error = "Std. Error",
  #        adjusted.se = "Adjusted SE",
  #        z.value = "z value",
  #        p.z     =  "Pr(>|z|)") %>% 
  mutate(coefficient = round(coefficient,3),
         se = round(se,3),
         z_value = round(z_value,3),
         Ecoregion = case_when(Ecoregion == "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" ~ "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                               Ecoregion == "OZARK/OUACHITA-APPALACHIAN FORESTS" ~ "L2 Ozark-Ouchita-Appalachian Forests",
                               Ecoregion == "SOUTHEASTERN USA PLAINS" ~"L2 Southeastern USA Plains",
                               TRUE ~ Ecoregion), 
         Ecoregion = factor(Ecoregion,
                            levels =c("L1 Eastern Temperate Forests",
                                      "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                                      "L2 Ozark-Ouchita-Appalachian Forests",
                                      "L2 Southeastern USA Plains",
                                      "Arkansas Valley",
                                      "Blue Ridge",
                                      "Central Appalachians",
                                      "Middle Atlantic Coastal Plain",
                                      "Ouachita Mountains",
                                      "Ozark Highlands",
                                      "Ridge and Valley",
                                      "South Central Plains",
                                      "Southeastern Plains",
                                      "Southern Coastal Plain",
                                      "Southwestern Appalachians")))%>% 
  flextable() %>% 
  set_header_labels(.,
                    Ecoregion           = "Ecoregion",
                    Predictor           = "Predictor",
                    Estimate            = "Estimate",
                    std.error           = "Std. Error",
                    adjusted.se         = "Adjusted SE",
                    z.value             = "z value",
                    p.z                 = "Pr(>|z|)")%>%
  width(width = c(1.6, 1.75, 0.8,0.8, 0.8, 0.8, 0.8)) %>%
  align(align = "center", part = "all") %>%
  align(j = "Predictor", align = "left", part = "all") %>%  
  bold(j = "Predictor", bold = TRUE, part = "body") %>% 
  bold(part = "header")  


save_as_docx(file = averaged,
             path = "Output/Graphs and Figures/Binomial_Averaged_coefficients_table.docx")

#####Single model coefficients table####
single<-read_excel("Output/Data/Binomial_GLM_Non_averaged_coefficients.xlsx") %>% 
  rename(std.error = "Std. Error",
         z.value = "z value",
         p.z     =  "Pr(>|z|)") %>% 
  mutate(Estimate = round(Estimate,3),
         std.error = round(std.error,3),
         z.value = round(z.value,3),
         p.z = round(p.z,3),
         Ecoregion = case_when(Ecoregion == "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" ~ "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                               Ecoregion == "OZARK/OUACHITA-APPALACHIAN FORESTS" ~ "L2 Ozark-Ouchita-Appalachian Forests",
                               Ecoregion == "SOUTHEASTERN USA PLAINS" ~"L2 Southeastern USA Plains",
                               TRUE ~ Ecoregion), 
         Ecoregion = factor(Ecoregion,
                            levels =c("L1 Eastern Temperate Forests",
                                      "L2 Mississippi Alluvial and Southeast US Coastal Plains",
                                      "L2 Ozark-Ouchita-Appalachian Forests",
                                      "L2 Southeastern USA Plains",
                                      "Arkansas Valley",
                                      "Blue Ridge",
                                      "Central Appalachians",
                                      "Middle Atlantic Coastal Plain",
                                      "Ouachita Mountains",
                                      "Ozark Highlands",
                                      "Ridge and Valley",
                                      "South Central Plains",
                                      "Southeastern Plains",
                                      "Southern Coastal Plain",
                                      "Southwestern Appalachians"))) %>% 
  flextable() %>% 
  set_header_labels(.,
                    Ecoregion           = "Ecoregion",
                    Predictor           = "Predictor",
                    Estimate            = "Estimate",
                    std.error           = "Std. Error",
                    z.value             = "t value",
                    p.z                 = "Pr(>|z|)")%>%
  width(width = c(1.6, 1.75, 0.8,0.8, 0.8, 0.8)) %>%
  align(align = "center", part = "all") %>%
  align(j = "Predictor", align = "left", part = "all") %>%  
  bold(j = "Predictor", bold = TRUE, part = "body") %>% 
  bold(part = "header")  

save_as_docx(file = single,
             path = "Output/Graphs and Figures/Binomial_GLM_Single_model_coefficients_table.docx")




