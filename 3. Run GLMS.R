
library(pacman)
pacman::p_load(jtools,tidyverse,readxl,
               MuMIn,writexl,prettyglm,
               ggeffects,sjPlot,
               sjmisc,sjlabelled,
               flextable,patchwork,officer)
#devtools::install_github("strengejacke/strengejacke")

####Write Functions and Keys####
rois_names<-c("Central Appalachians",
                    "Middle Atlantic Coastal Plain",
                    "Ouachita Mountains",
                    "Ozark Highlands",
                    "Ridge and Valley",
                    "South Central Plains",
                    "Southeastern Plains",
                    "Southern Coastal Plain")
fit_models <- function(data) {
  region_name = unique(data$NA_L3NAME)
  models<-list(
    model1=glm(PA ~ 1, 
               data = data,
               family = binomial(link = "logit")),
      # global=glm(PA ~ vs+ vpd + pdsi*percent_cover + spei14*percent_cover, 
      #            data = data,
      #            family = binomial(link = "logit")),
    #all terms but no interactions.
      model2 = glm(PA ~ vpd + vs  + pdsi+percent_cover + spei14, 
                   data = data,
                   family = binomial(link = "logit")),
    #individual interactions
     # model3=glm(PA ~ vpd + vs  + percent_cover*pdsi + spei14, 
     #            data = data,
     #            family = binomial(link = "logit")),
     # model4=glm(PA ~ percent_cover*vpd + vs  + pdsi + spei14, 
     #            data = data,
     #            family = binomial(link = "logit")),
      # model5 = glm(PA ~ vpd + vs  + pdsi + percent_cover*spei14, 
      #              data = data,
      #              family = binomial(link = "logit")),
#two interactions
         # model6 = glm(PA ~spei14 + vs  + pdsi*percent_cover+ percent_cover*vpd, 
         #          data = data,
         #          family = binomial(link = "logit")),
   #some terms. 
     model7=glm(PA ~ spei14+percent_cover, 
                data  = data,
                family = binomial(link = "logit")),
      # model8 = glm(PA ~ spei14*percent_cover, 
      #              data  = data,
      #              family = binomial(link = "logit")),
    
      model9=glm(PA ~ pdsi+percent_cover, 
                 data  = data,
                 family = binomial(link = "logit")),
     # model10= glm(PA ~ pdsi*percent_cover, 
     #              data  = data,
     #              family = binomial(link = "logit")),
    
     model11 =glm(PA ~ vpd + vs  + percent_cover, 
                  data  = data,
                  family = binomial(link = "logit")),
     # model12 = glm(PA ~ vpd*percent_cover + vs, 
     #               data  = data,
     #               family = binomial(link = "logit")),
    model13 =glm(PA ~ vpd + vs , # df =3 
                 data  = data,
                 family = binomial(link = "logit")),
    model14 =glm(PA ~ vpd + vs  + spei14 + pdsi, #df = 5
                 data  = data,
                 family = binomial(link = "logit")),
    
    model15 =glm(PA ~ percent_cover, 
                 data  = data,
                 family = binomial(link = "logit")),
    model16 =glm(PA ~ spei14, 
                 data  = data,
                 family = binomial(link = "logit")),
    model17=glm(PA ~ pdsi, 
                data  = data,
                family = binomial(link = "logit"))
  )
 
  
   
#get rid of models with insignificant interaction term. 
#    models<-lapply(models, function(model){
#      #ditch the models with insignificant interaction terms  
#      coef_summary <- summary(model)$coefficients
#      coefficients_df <- data.frame(
#        variable = rownames(coef_summary),
#        coefficient = coef_summary[, 1],
#        se = coef_summary[, 2],
#        z_value = coef_summary[, 3],
#        p_value = coef_summary[, 4]
#      )
#      #print("just about to check if interaction term is significant")
#      
#      if(any(grepl(":", coefficients_df$variable))) {
#        # If a colon is found, get the variable name and p-value
#        value_name <- coefficients_df$variable[grep(":", coefficients_df$variable)]
#        value <- coefficients_df$p_value[coefficients_df$variable == value_name]
#        #print(value)
#        if (any(value >= 0.05)){
#        #  print("null")
#          return(NULL) 
#        }
#        #end function to return null if p value >0.05
#      } 
#    return(model) 
#    })
# 
# #drop null from the list
# models<- models[-which(sapply(models, is.null))]
# 
# 





#calculate AIC Values, determine best models
    aic_values<- lapply(models, AICc)
 ####figure out the wierd odds ratio ####
     odds<-lapply(models, function(model){
       logodds<-summary(model)$coefficients[,1]
       OR<-exp(coef(model))
     print(OR)
       })

    summaries<-lapply(models,summary)
  
  # Extract the log-likelihood
  logLikelihoods <- lapply(models,logLik)

  # Get the number of parameters
  numParams <- lapply(models, function(m) length(coef(m)))
  
  
  
  aic_df <- bind_rows(lapply(names(models), function(name) {
    summary<- summaries[[name]]
    loglik<-logLikelihoods[[name]]
    k <-numParams[[name]]
    predictor_vars <- attr(summary$terms, "term.labels")
    formula <- paste("PA ~", paste(predictor_vars, collapse = " + "))
    data.frame(model_name = name,
               loglik,
               k,
               AICc = aic_values[[name]],
               terms = formula)
  })) %>%  
    arrange(AICc) %>% 
    mutate(
      deltaAIC = AICc - first(AICc),
      rel_likelihood = exp(-0.5 * deltaAIC),
      weight = round(rel_likelihood / sum(rel_likelihood),3)
     ) #%>%
    # select(-rel_likelihood) 
  
  aic_df_reduced<-aic_df %>% 
    filter(deltaAIC <= 2)
  aic_df_best<-aic_df %>% 
    filter(deltaAIC <= 2)
  n_models = nrow(aic_df_reduced)
  
  cat("Summaries for models with deltaAIC <= 2:\n")
  print(region_name)
  print(paste0("Number of candidate models : ", n_models))
  print(aic_df)
  
  
  
  #Create nice names for the terms
  pl <- c(`(Intercept)` = "Intercept",
          "vpd" = "Vapor Pressure Deficit (kPa)",
          "vs" = "Wind Speed (m/s)",
          "sm" = "Soil Moisture (% water in 1 m3)",
          "pdsi" = "PDSI",
          "spei14" = "14 Day SPEI",
          "percent_cover" = "Woody Cover (%)",
          ":" = " X ",
          "~  " = "~1"
          # "pdsi:percent_cover" = "Woody Cover X PDSI",
          # "percent_cover:vpd" = "Woody Cover X Vapor Pressure Deficit",
          # "percent_cover:spei14"= "Woody Cover X 14 Day SPEI"
  )
####Make AIC Table ####  
  
#create AIC Table for Publication, for each region
aic_df<-aic_df %>% 
  dplyr::select(terms,AICc,deltaAIC,loglik,k,weight) %>%
  mutate(terms = gsub("PA", "", terms)) %>% 
  mutate(terms = str_replace_all(terms, pl)) %>% 
  rename("Predictor Terms" = terms,
         "Delta AICc" = deltaAIC) 


final_row_number<-nrow(aic_df)
#print(final_row_number)
  
  folder_aic<-"Output/Graphs and Figures/AIC tables/"
filepath_aic<-file.path(folder_aic,paste0(
    gsub(" ", "_", region_name),
    "aic_table.docx"))

border_style <- fp_border(color = "black", width = 1.2)

flex<-flextable(aic_df) %>% 
    theme_vanilla() %>%
  set_formatter("AICc" = function(x){
    formatC(x,format = "f", digits = 2)},
    "Delta AICc" = function(x){
      formatC(x,format = "f", digits = 2)}) %>% 
#  highlight(i = aic_df[[3]] <= 2, color = 'wheat') %>% 
  border_remove() %>% 
  bold(part= "header") %>% 
   width(j = 1:5, 
         width = c(3.5,
                   0.8,
                   1.1,
                   1,
                   0.3)) %>% 
  add_header_row(values = region_name, colwidths = ncol(aic_df)) %>% 
  border(part = "header",
         border.top = border_style) %>% 
  border(part = "header", i = 2, border.bottom = border_style) %>% 
  border(part = "body", i = final_row_number, border.bottom = border_style) %>% 
   
  
  #flex<- add_header_lines(flex, values = region_name) %>% 
    save_as_docx(path = filepath_aic)  
flex

  

models_restricted <-models[names(models) %in% aic_df_best$model_name]
  

  print(models_restricted)
 
   lapply(aic_df_reduced$model_name, function(model_name) {
    model <- models[[model_name]]
    
    print(summary(model))})
  
#option to create plots  
  user_input <- as.numeric(readline(prompt = "Enter 1 to create diagnostic and output plots or 0 to skip: "))
  
 # Check if the user input is valid (1 or 0)
  if (user_input == 1) {
    # Code to create diagnostic plots
    print("Creating diagnostic and output plots...")

  #create output plots
  output_folder<-"Output/Graphs and Figures/probabilites/"
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  } #end function to create output folder if doesn't exist
  
  #analyze the individual models
  print("But meanwhile....here's the summaries of the top models")
  lapply(aic_df_reduced$model_name, function(model_name) {
    model <- models[[model_name]]
    
    print(summary(model))
    
    #check if the interaction term is significant
    coef_summary <- summary(model)$coefficients
    coefficients_df <- data.frame(
      variable = rownames(coef_summary),
      coefficient = coef_summary[, 1],
      se = coef_summary[, 2],
      z_value = coef_summary[, 3],
      p_value = coef_summary[, 4]
    )
    print("just about to check if interaction term is significant")
   
     if(any(grepl(":", coefficients_df$variable))) {
      # If a colon is found, get the variable name and p-value
       interaction_rows <- coefficients_df[grepl(":", coefficients_df$variable), ]
       
       # Print p-values for all interaction terms
      # print(interaction_rows$p_value)
       # If all interaction terms have p >= 0.05, return NULL
       if(any(interaction_rows$p_value >= 0.05)) {
       #  print("All interaction terms are not significant")
         return(NULL)
       }
     } #end function to remove models with insignificant interaction term from plotting
    
    predictor_vars <- colnames(model.matrix(model))
    print("removing intercep and : from predictor vars")
    
    # Remove intercept term if present
    if ("(Intercept)" %in% predictor_vars) {
      coeff_names <- predictor_vars[predictor_vars != "(Intercept)"]
    }
    if(any(grepl(":.*", coeff_names))) {
      coeff_names <- coeff_names[!grepl(":.*", coeff_names)]
    } else {
      coeff_names <- coeff_names
    }
print(coeff_names)
    print("creating an output file location")

   output_file <- file.path(output_folder,paste0(
     gsub(" ", "_", region_name),
     model_name,
     "prob_plot",
     ".jpg"))
   
   column_key <- c("vpd" = "Vapor Pressure Deficit (kPa)",
                   "vs" = "Wind Speed (m/s)",
                  # "sm" = "Soil Moisture (m3/m-3)",
                   "pdsi" = "PDSI",
                   "spei14" = "14 Day SPEI",
                   "percent_cover" = "Woody Cover (%)")
  
    plts = lapply(coeff_names, function(i) {
      pretty_name <- column_key[i]
      
#make plots
     plot<-plot(ggpredict(model, i),
                show_title = FALSE)+
        ylab(str_wrap("Predicted Fire Probability",width = 17))+
       theme(text = element_text(size = 7))+
       theme_bw()+
       xlab(str_wrap(pretty_name,width = 15))
     return(plot)
   })
   print("we made at least one plot")
   wrap_plots(plts,
              ncol= 3,
              byrow = FALSE,
              axis_titles = "collect")
   ggsave(output_file, wrap_plots(plts), width = 6, height = 6, units = "in")
print("we made all non-interaction plots")
   
   #If there is an interaction term, create an additional plot
   interaction_term <- predictor_vars[grepl(":", predictor_vars)]
   
   if (length(interaction_term) > 0){
     print("This region has a significant interaction term")
     interaction_vars <- unlist(strsplit(interaction_term, ":"))
     interaction_vars <- c(interaction_vars[1], interaction_vars[2])
     
     # Create the interaction plot
     interaction_plt <- plot(ggpredict(model, interaction_vars))
  
   #if ("pdsi:percent_cover" %in% predictor_vars){
   #  print("This region has a significant interaction term")
    # interaction_plt<-plot(ggpredict(model,c("pdsi","percent_cover")))
    
     filename = file.path(output_folder,paste0(
    gsub(" ", "_", region_name),
    model_name,
    "prob_plot_interaction",
    ".jpg"))
   ggsave(filename,plot = interaction_plt,
          dpi = 300,
          height = 5,
          width = 5)
   } #end interaction plot function
   
   output_folder <-"Output/Graphs and Figures/Diagnostic Plots/"
#create diagnostic plot 
   output_file <- file.path(paste0(output_folder,
                         "diagnostic_plot_",
                         gsub(" ", "_", region_name),
                         "_", model_name,
                         ".jpg"))
# Open a PNG device
   png(filename = output_file, 
       width = 6, height = 6, 
       units = "in", res = 500)
   par(mfrow = c(3, 2))
   
   plot(x = model,
        which = 1)
   mtext(region_name, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
   plot(x = model,
        which = 2)
   mtext(model_name, side = 3, line = 3, adj = 0.5, col = "blue", cex = 0.8)
   plot(x = model,
        which = 3)
   plot(x = model,
        which = 4)
   plot(x = model,
        which = 5)
   plot(x = model,
        which = 6)
 
   dev.off()
   
    
   })#end loop through models
  
  } else if (user_input == 0) {
    # Code to skip creating diagnostic plots
    print("Diagnostic and output plots skipped.")
    return(models_restricted)
  } else {
    # Handle invalid user input
    print("Invalid input. Please enter either 1 or 0.")
  }
  
  return(models_restricted)

}


#now make some table. 
pl <- c(`(Intercept)` = "Intercept",
        "vpd" = "Vapor Pressure Deficit (kPa)",
        "vs" = "Wind Speed (m/s)",
        "sm" = "Soil Moisture (% water in 1 m3)",
        "pdsi" = "PDSI",
        "spei14" = "14 Day SPEI",
        "percent_cover" = "Woody Cover (%)",
        "pdsi:percent_cover" = "PDIS X Woody Cover",
        "~  " = "~1")




####L1 Eastern Temperate Forests####

data_L1<-read_excel('Output/Data/Presence_Absence_L1.xlsx') %>% 
  mutate(NA_L3NAME= "L1 Eastern Temperate Forests")
L1_model_results<-fit_models(data_L1) 

tab_model(L1_model_results, 
          pred.labels = pl,
          collapse.ci = TRUE,
          dv.labels = "Best Model",
          wrap.labels = 15,
          CSS = list(css.centeralign = 'text-align: center;'),
          p.style = "stars",
          file = "Output/Graphs and Figures/Odds ratio tables/L1_oddsratio.html")

####L3 Eight Selected Regions####
L3_data<-read_excel('Output/Data/L3_Presence_Absence_Eight_Regions.xlsx')%>%
  group_by(NA_L3NAME) %>% 
  filter(NA_L3NAME == "Middle Atlantic Coastal Plain") %>% 
  group_split() 
#summary(L3_data$sm)



names(L3_data) <- lapply(L3_data, function(x) unique(x$NA_L3NAME))
L3_model_results <- lapply(L3_data, fit_models) %>% 
  unlist(recursive = FALSE)


#L3 Table
tab_model(L3_model_results, 
          pred.labels = pl,
          collapse.ci = TRUE,
          #dv.labels = rois_names,
          dv.labels = names(L3_model_results),
          p.style = "stars",
          wrap.labels = 15,
          CSS = list(css.centeralign = 'text-align: center;'),
          file = "Output/Graphs and Figures/Odds ratio tables/L3_odds_ratiotable.html")



L3_model_results_filtered<-L3_model_results[-c(2,6,7,10:12,14,17,18,20,21)]
tab_model(L3_model_results_filtered, 
          pred.labels = pl,
          collapse.ci = TRUE,
          #dv.labels = rois_names,
          dv.labels = names(L3_model_results_filtered),
          p.style = "stars",
          wrap.labels = 15,
          CSS = list(css.centeralign = 'text-align: center;'),
          file = "Output/Graphs and Figures/Odds ratio tables/L3_odds_ratiotable_filtered.html")


####Write dataframes to become pretty tables later####
#turns out I need these after all
 # term_codes_df <- data.frame(
 #   Term = c("intercept","pdsi","pdsi:percent_cover", "percent_cover", "sm", "spei14", "vpd", 
 #            "vs", ),
 #   Code = c(1, 2, 3, 4, 5, 6, 7, 8)
 # ) %>% 
 #   mutate(Code = as.character(Code))

####Extract odds ratio to make cool maps####
library(questionr)

OR_L1<-odds.ratio(L1_model_results$model1.interaction_spei14,
                  level = 0.95)
rownames<- rownames(OR_L1)
OR_L1 = as_tibble(OR_L1) %>% 
  mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
      predictors = rownames)
  
  
#this is the "best" model, once models with
#insignificant hanger-on variables are removed.





  new_names <- sapply(names(L3_model_results), function(x) {
    str_split(x, "\\.")[[1]][1]
  })
  
  L3_model_results <- setNames(L3_model_results, new_names)


  
  
  
  
Or_L3<-map(L3_model_results, odds.ratio) 
 Or_L3_delisted<- lapply(Or_L3, function(x){
   predictors<- rownames(x)
   tb<-as_tibble(x) %>% 
     mutate(predictors = predictors)
 return(tb)
   })
df<- bind_rows(Or_L3_delisted,.id= "id") %>% 
  rename(NA_L3NAME = "id")

all_model_results<-bind_rows(df,OR_L1)


write_xlsx(all_model_results,"Output/Data/Binomial_GLM_Model_coefficients.xlsx")
colnames(all_model_results)



####Test three way interaction####
p_load(interactions)
#middle atlantic
glm_test<-glm(PA ~ vs + pdsi*percent_cover + percent_cover*vpd + spei14, 
              data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
              family = binomial(link = "logit"))
summary(glm_test)

glm_test_pdsi<-glm(PA ~ vs + pdsi*percent_cover + vpd + spei14, 
                   data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                     filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
                   family = binomial(link = "logit"))

glm_test_vpd<-glm(PA ~ vs + pdsi+percent_cover*vpd + spei14, 
                  data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                    filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
                  family = binomial(link = "logit"))

AICc(glm_test,glm_test_pdsi,glm_test_vpd)


plot(ggpredict(glm_test, c("percent_cover","vpd")))
plot(ggpredict(glm_test, c("percent_cover","pdsi")))





tab_model(glm_test, 
          #pred.labels = pl,
          collapse.ci = TRUE,
          dv.labels = "Best Model",
          wrap.labels = 15,
          CSS = list(css.centeralign = 'text-align: center;'),
          p.style = "stars",
          file = "Output/Graphs and Figures/Odds ratio tables/MAextrainteraction.html")

interact_plot(glm_test, pred = percent_cover, modx = pdsi,
              plot.points = TRUE, jitter =0.05,
              interval= TRUE, int.width = 0.9)
#this replicates ggpredict.

interact_plot(glm_test, pred = percent_cover, modx = pdsi,
              plot.points = TRUE, linearity.check = TRUE)


#Southern Coastal Plain
glm_test<-glm(PA ~ vs + percent_cover*pdsi + percent_cover*vpd + spei14, 
              data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                filter(NA_L3NAME == "Southern Coastal Plain"),
              family = binomial(link = "logit"))
summary(glm_test)

glm_test_pdsi<-glm(PA ~ vs + pdsi*percent_cover + vpd + spei14, 
                   data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                     filter(NA_L3NAME == "Southern Coastal Plain"),
                   family = binomial(link = "logit"))
glm_test_vpd<-glm(PA ~ vs + pdsi+percent_cover*vpd + spei14, 
                  data = read_excel('Output/Data/L3_Presence_Absence_Eleven_Regions.xlsx')%>%
                    filter(NA_L3NAME == "Southern Coastal Plain"),
                  family = binomial(link = "logit"))

AICc(glm_test,glm_test_pdsi,glm_test_vpd)


plot(ggpredict(glm_test, c("percent_cover","vpd")))
plot(ggpredict(glm_test, c("percent_cover","pdsi[-8,-4,0,4]")))

#determine what levels of vpd are found there maybe?



tab_model(glm_test, 
          #pred.labels = pl,
          collapse.ci = TRUE,
          dv.labels = "Best Model",
          wrap.labels = 15,
          CSS = list(css.centeralign = 'text-align: center;'),
          p.style = "stars",
          file = "Output/Graphs and Figures/Odds ratio tables/extrainteraction.html")


####test other windows for averaging####
data<-read_excel("Output/Data/L3_Presence_Absence_Eight_RegionsTEST.xlsx")


vpd<-glm(PA ~ vs + pdsi+percent_cover+vpd + spei14, 
         data = data %>% 
           filter(NA_L3NAME == "Ouachita Mountains"),
         family = binomial(link = "logit"))
vpd_1day<-glm(PA ~ vs + pdsi+percent_cover+vpd_1day + spei14, 
              data = data %>% 
                filter(NA_L3NAME == "Ouachita Mountains"),
              family = binomial(link = "logit"))
vpd_5day<-glm(PA ~ vs + pdsi+percent_cover+vpd_5day + spei14, 
              data = data %>% 
                filter(NA_L3NAME == "Ouachita Mountains"),
              family = binomial(link = "logit"))

AICc(vpd,vpd_1day,vpd_5day)


vs<-glm(PA ~ vs + pdsi+percent_cover+vpd + spei14, 
         data = data %>% 
           filter(NA_L3NAME == "Ouachita Mountains"),
         family = binomial(link = "logit"))
vs_1day<-glm(PA ~ vs_1day + pdsi+percent_cover+vpd + spei14, 
              data = data %>% 
                filter(NA_L3NAME == "Ouachita Mountains"),
              family = binomial(link = "logit"))
vs_5day<-glm(PA ~ vs_5day + pdsi+percent_cover+vpd + spei14, 
              data = data %>% 
                filter(NA_L3NAME == "Ouachita Mountains"),
              family = binomial(link = "logit"))

AICc(vs,vs_1day,vs_5day)




