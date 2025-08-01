


#write functions
fit_models_and_average_coefficients <-function(data) {
  models<-list(
    model1=glm(fire_size~ vpd + vs + sm + spei30*percent_cover + pdsi,
               data = data, family = Gamma(link = "identity")),
    model2=glm(fire_size ~ vpd + vs + sm + pdsi*percent_cover + spei30, 
               data = data, family = Gamma(link = "identity")),
    model3=glm(fire_size ~ spei30*percent_cover, 
               data  = data,
               family = Gamma(link = "identity")),
    model4 =glm(fire_size ~ pdsi*percent_cover, 
                data  = data,
                family = Gamma(link = "identity")),
    model5 =glm(fire_size ~ vpd + vs + sm + percent_cover, 
                data  = data,
                family = Gamma(link = "identity")),
    model6 =glm(fire_size ~ vpd + vs + sm,  
                data  = data,
                family = Gamma(link = "identity")),
    model7 =glm(fire_size ~ vpd + vs + sm + spei30 + pdsi,
                data  = data,
                family = Gamma(link = "identity")),
    model8 =glm(fire_size ~ pdsi, 
                data  = data,
                family = Gamma(link = "identity")),
    model9 =glm(fire_size ~ spei30, 
                data  = data,
                family = Gamma(link = "identity")),
    model10 =glm(fire_size ~ percent_cover, 
                 data  = data,
                 family = Gamma(link = "identity"))
  )
  aic_values<- lapply(models, AICc)  
  aic_df <- bind_rows(lapply(names(models), function(name) {
    data.frame(model_name = name,
               AICc = aic_values[[name]])
  })) %>%  arrange(AICc,) %>% 
    mutate(deltaAIC = AICc-AICc[1]) %>% 
    filter(deltaAIC <=2)
  if (length(models[aic_df$model_name]) > 1) {
    models<-models[aic_df$model_name]
    #vif <- lapply(models, car::vif())
    #  summary(model.avg(models[aic_df$model_name]))
  } else {
    #vif<- NULL
    models<-NULL
    #loglik <- summary(model.avg(models)) } #if only one, include them all.
  return(models)
  }
}







rois_names<-c("Arkansas Valley",
              'Blue Ridge',
              "Central Appalachians",
              "Middle Atlantic Coastal Plain",
              "Ouachita Mountains",
              "Ozark Highlands",
              "Ridge and Valley",
              "South Central Plains",
              "Southeastern Plains",
              "Southern Coastal Plain",
              "Southwestern Appalachians")
L3_data<-read_excel('Outputs/Output Data/Fire_IDs_with_weather_indices_and_vegetation.xlsx') %>% 
  filter(NA_L3NAME %in% rois_names) %>% 
  mutate(fire_size = log10(fire_size)) %>% 
  na.omit() %>% 
  group_by(NA_L3NAME) %>% 
  group_split()

names(L3_data) <- lapply(L3_data, function(x) unique(x$NA_L3NAME))

L3_models_list<-lapply(L3_data,fit_models_and_average_coefficients)



# Assuming L3_models_list is a list of lists with models in it
vif_results <- lapply(L3_models_list, function(region_models) {
  lapply(region_models, car::vif)
})
