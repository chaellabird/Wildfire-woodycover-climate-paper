####Load Packages####
library(pacman)
pacman::p_load(lme4,nlme, tidyverse,readxl,glmmTMB,DHARMa,regclass,performance,effectsize,ggeffects,flextable)

p_load(marginaleffects,performance, insight, sjPlot, ggeffects,patchwork)

####Load Data and manipulate####

column_key <- c("vpd_1.1.window_mean" = "Vapor Pressure Deficit (kPa)",
                "vs_1.1.window_mean" = "Wind Speed (m/s)",
                "pdsi.5.1.window_mean" = "PDSI",
                "spei.5.1.window_mean" = "14 Day SPEI",
                "year" = "Year",
                "percent_woody" = "Woody Cover (%)",
                "percent_woody:pdsi.5.1.window_mean" = "Woody Cover X PDSI",
                "percent_woody:vpd_1.1.window_mean" = "Woody Cover X Vapor Pressure Deficit")


nlcd<-read_csv("Output/NLCD_proportion_pivoted_randompoints.csv") %>%
  mutate(event_id = as.character(event_id)) %>% 
  bind_rows(read_csv("Output/NLCD_proportion_pivoted_wildfires.csv")) %>% 
  mutate(forest = rowSums(across(c(deciduous_forest, mixed_forest, evergreen_forest)), na.rm = TRUE)) %>% 
  rename(Event_ID = event_id) %>% 
  select(Event_ID, forest,pasture_hay,shrub_scrub) %>% 
  unique()

df_L3<-read_excel("Output/Revised_Presence_Absence_L3_crops_hay_filtered.xlsx") %>% 
  mutate(PA = as.factor(PA),
         year = as.numeric(year(Ig_Date)),#-1990, # change the year to start at 0 
         percent_woody = percent_woody*100, 
         decade = as.factor(case_when(year <= 10 ~ 1,
                                      year > 10 & year < 20 ~ 2,
                                      year >=20 ~ 3)
         )) %>% 
  left_join(nlcd)


counts_keep<-df_L3 %>% 
  filter(PA == 1) %>% 
  group_by(NA_L3NAME) %>% 
  mutate(n= n()) %>% 
  select(NA_L3NAME, n) %>% 
  unique() %>% 
  filter(n > 70)

df_L3_excluded<-df_L3 %>% 
  filter(NA_L3NAME != "Middle Atlantic Coastal Plain") %>% 
  filter(NA_L3NAME != "Ouachita Mountains") %>%
  filter(NA_L3NAME %in% counts_keep$NA_L3NAME) %>% 
  group_by(NA_L3NAME) %>% 
  group_split()


####Run Models####
L3_models_no_interaction<-invisible(lapply(df_L3_excluded, function(df){
  region = unique(df$NA_L3NAME)
  n_fires = nrow(df)/2
  model = glm(PA ~ percent_woody + spei.5.1.window_mean+vpd_1.1.window_mean + vs_1.1.window_mean
                  + pdsi.5.1.window_mean +year,
                  data = df, family = binomial(link = "logit"))
  print(region)
  
  print(summary(model))
  return(model)
}))
names(L3_models_no_interaction) <- sapply(df_L3_excluded, function(df) unique(df$NA_L3NAME))

model_MA<- glm(PA ~ percent_woody + spei.5.1.window_mean+vpd_1.1.window_mean*percent_woody+  vpd_1.1.window_mean + vs_1.1.window_mean
                          + pdsi.5.1.window_mean +year,
                          data = df_L3 %>% 
                            filter(NA_L3NAME  == "Middle Atlantic Coastal Plain"), family = binomial(link = "logit"))
  
model_Ouachita<- glm(PA ~ percent_woody + spei.5.1.window_mean+pdsi.5.1.window_mean*percent_woody+  vpd_1.1.window_mean + vs_1.1.window_mean
                                + pdsi.5.1.window_mean +year,
                                data = df_L3 %>% 
                                  filter(NA_L3NAME == "Ouachita Mountains"), family = binomial(link = "logit"))

models_interactions<-list(model_MA,model_Ouachita)
names(models_interactions)<-c("Middle Atlantic Coastal Plain","Ouachita Mountains")
all_models<-append(L3_models_no_interaction, models_interactions)


####Export Outputs as csv for use in map creation ####

L3_models<-lapply(names(all_models), function(name){
  region<-name
  model<-all_models[[name]]
 print(region)
   #extract p values
  model_summary<-summary(model)
  #print(model_summary)
  #pvals <- model_summary$coefficients$cond[ , "Pr(>|z|)"]
  pvals <- model_summary$coefficients[ , "Pr(>|z|)"]
  pvals_df <- setNames(as.data.frame(t(pvals)), names(pvals)) %>%
    pivot_longer(cols = everything(),names_to = "Term", values_to = "p_value")
 # print(pvals_df)
  #create a df that can be built upon
  effects <- model_summary$coefficients
 # print(effects)
  #fixed<-fixef(model) #this step is screwy 
  #oddsratio<-exp(unlist(fixed[1]))
 
  #print("odds ratios")
  #print(oddsratio)
  oddsratio_df <- as.data.frame(effects) %>% 
    rownames_to_column(var = "Term") %>% 
    mutate(OR = exp(Estimate))
  #print("examine odds ratio df")
 
 # colnames(oddsratio_df) =  sub("^cond\\.", "", colnames(oddsratio_df))
  
  oddsratio_df<-oddsratio_df %>%
  #  pivot_longer(everything(), names_to = "Term", values_to = "OR") %>%
    mutate(NA_L3NAME = region) %>% 
    #select(Term,Estimate, NA_L3NAME,p_value) %>% 
    left_join(pvals_df, by = "Term") # Add explicit join column
 # print(oddsratio_df)
  #add the confidence interval
  df_confint<-as.data.frame(confint(model) )%>%
    rownames_to_column(var = "Term") #%>%
   # rename(Log_odds = "Estimate")
  #print(df_confint)
  oddsratio_df<-oddsratio_df %>%
    left_join(df_confint) %>%
    rename(int_2.5 = "2.5 %",
           int_97.5 = "97.5 %") %>% 
    mutate(OR2.5 = exp(int_2.5),
            OR97.5 = exp(int_97.5)) #exponentiating it to get it in terms of odds ratio.
# print("Examine final joined df")
  # print(oddsratio_df)
  return(oddsratio_df)
})%>% 
  bind_rows() %>% 
  select(NA_L3NAME, Term,Estimate,int_2.5,int_97.5,OR,OR2.5, OR97.5,p_value) 



#export a csv for making cool maps
write_csv(L3_models, "Output/L3_models_output_redoneAGAIN.csv")





####Check Variance inflation factor ####
runvif <- lapply(names(all_models), function(name){
  model <- all_models[[name]]
  plot <- check_collinearity(model) %>% 
    plot() + 
    annotate("text", x = Inf, y = Inf, label = name, 
             hjust = 1.1, vjust = 1.1, size = 4, fontface = "bold")
  print(plot)
})


#####Make Diagnostic plots for all models####

lapply(names(all_models), function(name){
#create diagnostic plots
output_folder<-"Output/figures/Diagnostic Plots/"
  region<-name
model<-all_models[[name]]
model_name <- model$call
output_file <- paste0(output_folder,
                      "diagnostic_plot_logit_glm",
                      gsub(" ", "_", region),
                      "_",
                      ".png")
# Open a PNG device
png(filename = output_file, 
    width = 8, height = 6, 
    units = "in", res = 500)
par(mfrow = c(1, 2))
resid<-simulateResiduals(model)
plot(resid)

dev.off()

})



####Make Display plots####


makeplots<-function(model){
  
  predictor_vars <- colnames(model.matrix(model))
  print("removing intercept and : from predictor vars")
  
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
  
  
  plts = lapply(seq_along(coeff_names), function(idx) {
    i <- coeff_names[idx]
    pretty_name <- column_key[i]
    
    #make plots
    plot<-plot(ggpredict(model, i),
               show_title = FALSE)+
      #ylab(str_wrap("Predicted Fire Probability",width = 17))+
      #ylab("Predicted Fire Probability")+
      ylab("")+
      theme_bw(base_size = 25)+
      theme(text = element_text(size = 20,
                                color = "black"))+
      xlab(str_wrap(pretty_name,width = 15))+
      scale_y_continuous(
        limits = c(0, 1),          
        labels = scales::percent   
      )
    #if (idx <= 3) {
    if (idx <= 5) {
      plot <- plot + ylab(str_wrap("Predicted Fire Probability",width = 17))
    }
    
    return(plot)
  }) 
  
  
  names(plts) <- coeff_names 
  return(plts) 
  
}

lapply(names(models_interactions), function(name){
  region <-name
  n_fires<- counts_keep$n[counts_keep$NA_L3NAME == region]
  model<-all_models[[name]]

  plots_fixed<-makeplots(model)

  
  #write whole if statement for if it has an : in the terms. 
  predictor_vars <- colnames(model.matrix(model))
  print("Selecting only : from predictor vars")
  
  if(any(grepl(":.*", predictor_vars))) {
    coeff_names <- predictor_vars[grepl(":.*", predictor_vars)]
    print(coeff_names)
    #there should only be one for each
    #make that plot dynamically to use the term found. 
    if("percent_woody:vpd_1.1.window_mean" %in% coeff_names){
      #remove both from the old plots fixed
      plots_fixed[c("percent_woody", "vpd_1.1.window_mean")] <- NULL
      
      int_df <- ggpredict(model, terms = c("percent_woody", "vpd_1.1.window_mean[0.5,1,1.5]")) 
      plot_int<- ggplot(int_df, aes(x = x, y = predicted, color = group)) +
      geom_line(size = 1) +  # Line for predicted values
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
      
      labs(
        x = "Woody Cover (%)",
        y = "",
        color = "vpd_1.1.window_mean",
        fill = "vpd_1.1.window_mean",
      ) +
      theme_bw(base_size = 25)+
      theme(text = element_text(size = 20))+
      scale_y_continuous(
        limits = c(0, 1),          
        labels = scales::percent   
      )
    }else{
      plots_fixed[c("percent_woody", "pdsi.5.1.window_mean")] <- NULL
      int_df <- ggpredict(model, terms = c("percent_woody", "pdsi.5.1.window_mean[-5,0,5]")) 
      plot_int<- ggplot(int_df, aes(x = x, y = predicted, color = group)) +
        geom_line(size = 1) +  # Line for predicted values
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
        
        labs(
          x = "Woody Cover (%)",
          y = "",
          color = "pdsi.5.1.window_mean",
          fill = "pdsi.5.1.window_mean",
        ) +
        theme_bw(base_size = 25)+
        theme(text = element_text(size = 20))+
        scale_y_continuous(
          limits = c(0, 1),          
          labels = scales::percent   
        )
    }
    
    
     plot_list<-append(plots_fixed,list(plot_int))
  } else{
    plot_list<-plots_fixed
  }
  
  
  
  plots_wrapped<-wrap_plots(plot_list,
                          byrow = FALSE,
                          nrow = 3,
                          axis_titles = "collect")+
  plot_annotation(title = region,
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))
output_folder = "Output/figures/Prediction plots from log odds/"
output_file <- paste0(output_folder,
                      "Predictedprobs",
                      gsub(" ", "_", region),
                      "_",
                      ".png")

ggsave(output_file, plots_wrapped, width = 11, height = 11, units = "in")


})





####Make standardized importance plots####


lapply(names(all_models), function(name){
  output_folder<-"Output/figures/Standardized Effect Plots/"
region_name<-name
model<-all_models[[name]]
model_summary<-summary(model)
output_file <- paste0(output_folder,
                      "standardized_odds_ratio_",
                      gsub(" ", "_", region_name),
                      ".png")
print(output_file)
p<- model_summary$coefficients[ , "Pr(>|z|)"]
p_values <- setNames(as.data.frame(t(p)), names(p)) %>% #where is pvals coming from?
  pivot_longer(cols = everything(),names_to = "Parameter", values_to = "p.value")%>% 
  dplyr::select(c(Parameter,p.value))

factor_order <- c("(Intercept)",
                  "year", 
                  "vpd_1.1.window_mean",
                  "vs_1.1.window_mean", 
                  "pdsi.5.1.window_mean",
                  "spei.5.1.window_mean",  
                  "percent_woody",
                  "percent_woody:pdsi.5.1.window_mean",
                  "percent_woody:vpd_1.1.window_mean")


standardized<-as.data.frame(standardize_parameters(
  model,
  ci = 0.95,
  exp = TRUE))#
standardized<-as_tibble(standardized%>%
                          left_join(p_values) %>% 
                          mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% #this step is messing up. 
                          arrange(Parameter) %>% 
                          mutate(y_numeric = as.numeric(Parameter)))
ggplot_std<-ggplot(standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_bw() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = region_name
  )+
  scale_y_discrete(labels = column_key)+
  theme(
    panel.grid.major.x = element_line(color = "gray80"),  # keep vertical lines
    panel.grid.major.y = element_blank(),                # remove horizontal lines
    panel.grid.minor = element_blank(),
    # Make axis lines black
    axis.line = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    
    panel.border = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )+
  geom_segment(aes(x = Std_Odds_Ratio, xend = Std_Odds_Ratio,
                   y = Parameter, yend = y_numeric + 0.3),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "gray40") +
  # Label above each point
  geom_label(aes(x = Std_Odds_Ratio + 0.1,
                 y = y_numeric + 0.4,
                 label = paste0("p = ", signif(p.value, 2), "\nOR = ", signif(Std_Odds_Ratio, 2))
  ),
  fill = "white", color = "black", size = 3,
  label.size = 0.2, label.r = unit(0.05, "lines"))

ggsave(output_file,ggplot_std,device = "png")

})




####Compile Outputs into a Flextable ####







#Create nice names for the terms
pl <- c(`(Intercept)` = "Intercept",
        "vpd_1.1.window_mean" = "Vapor Pressure Deficit (kPa)",
        "vs_1.1.window_mean" = "Wind Speed (m/s)",
        "pdsi.5.1.window_mean" = "PDSI",
        "spei.5.1.window_mean" = "14 Day SPEI",
        "year" = "Year",
        "percent_woody" = "Woody Cover (%)",
        "percent_woody:pdsi.5.1.window_mean"= "Woody Cover X PDSI",
        "percent_woody:vpd_1.1.window_mean" = "Woody Cover X Vapor Pressure Deficit")



odds_ratio<- L3_models %>%  
  arrange(NA_L3NAME,Term) %>% 
  mutate(Predictor = recode(Term,!!!pl),
         OR = round(OR,3),
         OR2.5 = round(OR2.5,3),
         OR97.5 = round(OR97.5,3)) %>% 
  mutate(p.new = case_when(p_value< 0.05 & p_value > 0.01 ~ "<0.05",#paste0(as_character(round(OR,3)),"*"),
                           p_value <0.01 & p_value >0.001 ~ "<0.01",#paste0(as_character(round(OR,3)),"**"),
                           p_value <0.001 ~ "<0.001",#paste0(as_character(round(OR,3)),"***"),
                           p_value >0.05 ~ as.character(round(p_value,3)))) %>% 
  dplyr::select("NA_L3NAME","Predictor",'OR', "OR2.5","OR97.5","p.new") %>% 
  rename(Ecoregion = NA_L3NAME,
         "Odds Ratio" = OR,
         "p value" = p.new,
         "2.5 %" = OR2.5,
         "97.5 %" = OR97.5) 





ft<-flextable(odds_ratio)
ft
border<-fp_border_default()


ft<- ft %>% 
  padding(padding = 1, part = "body") %>% 
  set_formatter(
    ## so we wrap it in back‑ticks.
    `Odds Ratio` = function(x) {
      # x is a numeric vector (one element per row)
      ifelse(
        x > 10,                    # the condition
        formatC(x, format = "e",   # scientific notation
                digits = 2,        # e.g. 1.23e+01
                big.mark = ""),             
        formatC(x, format = "f",   # normal fixed‑point
                digits = 3)        # 1.234
      )
    },
    
    `2.5 %` = function(x) {
      # x is a numeric vector (one element per row)
      ifelse(
        x > 10,                    # the condition
        formatC(x, format = "e",   # scientific notation
                digits = 2,        # e.g. 1.23e+01
                big.mark = ""),             
        formatC(x, format = "f",   # normal fixed‑point
                digits = 3)        # 1.234
      )
    },
    `97.5 %` = function(x) {  # x is a numeric vector (one element per row)
      ifelse(
        x > 10,                    # the condition
        formatC(x, format = "e",   # scientific notation
                digits = 2,        # e.g. 1.23e+01
                big.mark = ""),             
        formatC(x, format = "f",   # normal fixed‑point
                digits = 3)        # 1.234
      )
    }
  ) %>% 
  # set_table_properties(j = c("Ecoregion", "Predictors", "OR", "2.5 %", "97.5"),layout = "autofit", width = 0.8) %>% 
  width(j = c("Ecoregion","Predictor","Odds Ratio", "2.5 %","97.5 %", "p value"), width = c(2.1,
                                                                                            2.2,
                                                                                            0.8,
                                                                                            0.8, #2.5 %
                                                                                            0.8, #97.5 %
                                                                                            0.7)) %>% 
  # add_footer_lines(values = "* p<0.05   ** p<0.01   *** p<0.001") %>% 
  fontsize(size =10, part = "footer") %>% 
  fontsize(size = 12, part  = "body") %>% 
  align(j = "p value",
        align = "right",
        part = "body")
ft

save_as_docx(
  "OR_ft" = ft, 
  path = "Output/Figures/Oddsratio_flextableREDONEWITHINTERACTIONS.docx")







