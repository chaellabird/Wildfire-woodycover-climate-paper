
library(pacman)
pacman::p_load(hnp,readxl,gridExtra,vcdExtra,performance,
               broom,tidyverse,boot,MuMIn,bestNormalize,
               ggplot2,MASS)

output_folder<-"Output/Graphs and Figures/Diagnostic Plots/"

####import data####

list<-read_excel('Output/Data/Presence_Absence_L1.xlsx') %>%
  mutate(region = "L1 Eastern Temperate Forests") %>% 
  # bind_rows(read_excel('Output/Data/Presence_Absence_L2.xlsx') %>% 
  #             rename(region = "NA_L2NAME") %>% 
  #             mutate(region = case_when(region == "OZARK/OUACHITA-APPALACHIAN FORESTS" ~ "Ozark-Ouachita-Appalachian Forests",
  #                                       TRUE ~ region))) %>% 
  bind_rows(read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx') %>% 
              rename(region = "NA_L3NAME")) %>% 
  filter(!region %in% c("Arkansas Valley","Blue Ridge","Southwestern Appalachians") ) %>% 
  group_by(region) %>% 
  group_split()
names(list)<-lapply(list, function(x) unique(x$region))


####Write functions####
fit_models <-function(data) {
  models<-list(
    model1=glm(PA ~ vpd + vs + sm + spei30*percent_cover + pdsi, 
               data = data,
               family = binomial(link = "logit")),
    model2=glm(PA ~ vpd + vs + sm + pdsi*percent_cover + spei30, 
               data = data,
               family = binomial(link = "logit")),
    model3=glm(PA ~ spei30*percent_cover, 
               data  = data,
               family = binomial(link = "logit")),
    model4 =glm(PA ~ pdsi*percent_cover, 
                data  = data,
                family = binomial(link = "logit")),
    model5 =glm(PA ~ vpd + vs + sm + percent_cover, #df = 4
                data  = data,
                family = binomial(link = "logit")),
    model6 =glm(PA ~ vpd + vs + sm, # df =3 
                data  = data,
                family = binomial(link = "logit")),
    model7 =glm(PA ~ vpd + vs + sm + spei30 + pdsi, #df = 5
                data  = data,
                family = binomial(link = "logit")),
    model8 =glm(PA ~ pdsi, #df = 1
                data  = data,
                family = binomial(link = "logit")),
    model9 =glm(PA ~ spei30, #df = 1
                data  = data,
                family = binomial(link = "logit")),
    model10 =glm(PA ~ percent_cover, #df = 1
                 data  = data,
                 family = binomial(link = "logit"))
  )
  aic_values<- lapply(models, AICc)  
  aic_df <- bind_rows(lapply(names(models), function(name) {
    data.frame(model_name = name,
               AICc = aic_values[[name]])
  })) %>%  arrange(AICc,) %>% 
    mutate(deltaAIC = AICc-AICc[1]) %>% 
    filter(deltaAIC <=2)
 filtered_models<-models[aic_df$model_name]

  return(filtered_models)
}


plot_diagnostics<-function(model, model_index){
  region <-model$data$region
  model_name <- model$call
  output_file <- paste0(output_folder,
                        "diagnostic_plot_",
                        gsub(" ", "_", region),
                        "_", model_index,
                        ".jpg")
  # Open a PNG device
  png(filename = output_file, 
      width = 6, height = 6, 
      units = "in", res = 500)
  par(mfrow = c(3, 2))
  
  plot(x = model,
       which = 1)#residuals by fitted
  mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 2)#QQplot
  mtext(model_name, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 3)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 4)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 5)
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = model,
       which = 6)#Cook's leverage
  #mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 1.2)
  
  dev.off()
}


####Apply functions####

fitted_models<-lapply(list,fit_models)
flattened_models <- map(fitted_models, ~ .x) %>% flatten()
flattened_models <- set_names(flattened_models, seq_along(flattened_models))

lapply(names(flattened_models), function(name) {
  plot_diagnostics(flattened_models[[name]], name)
})

####Just for the global model####
output_folder <- "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/"
region_list <- lapply(list, function(data){
  region <- unique(data$region)

      output_model =glm(PA ~ vpd + vs + spei30*percent_cover+vpd*percent_cover+percent_cover*pdsi, 
                 data = data,
                 family = binomial(link = "logit"))

      cooks_d <- cooks.distance(output_model)
      
      high_leverage_index <- which.max(cooks_d)
      high_leverage_point <- data[high_leverage_index, ]
      #Define a Cook's distance threshold (common choice: 4/n)
  threshold <- 4 / nrow(data)
  influential_points <- which(cooks_d > threshold)
  
  # Only print if there are influential points
  if (length(influential_points) > 0) {
    cat(paste0("\nRegion: ", region, "\n"))
    cat(paste0("Number of influential points: ", length(influential_points), "\n"))
    cat(paste0("Highest Cook's distance index: ", high_leverage_index, "\n"))
    cat("Data for highest leverage point:\n")
    print(high_leverage_point)
  }
      
  # # select the model with the lowest AIC
  # models_aic <- sapply(models, AIC)
  # 
  # best_model_index <- which.min(models_aic)
  # 
  # output_model <- models[[names(models)[best_model_index]]]
  
  model_terms = deparse(output_model$terms)
  
  output_file <- paste0(output_folder,
                        "diagnostic_plot_",
                        gsub(" ", "_", region),
                        ".jpg")  
  # Open a PNG device
  png(filename = output_file, width = 6, height = 6, units = "in", res = 500)
  par(mfrow = c(3, 2))
  
  plot(x = output_model,
       which = 1)#residuals by fitted
  mtext(region, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  mtext(model_terms,side = 3, line = 2, adj = -0.5, col = "blue",cex = 0.8)
  plot(x = output_model,
       which = 2)#QQplot
  mtext("Binomial,logit", side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
  plot(x = output_model,
       which = 3)
  plot(x = output_model,
       which = 4)
  plot(x = output_model,
       which = 5)
  plot(x = output_model,
       which = 6)#Cook's leverage
  

  
  dev.off()
  return(list(model = output_model, diagnostic_plot = output_file))
  
})



