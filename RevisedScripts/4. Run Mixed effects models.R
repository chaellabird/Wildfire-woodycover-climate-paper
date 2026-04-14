
library(pacman)
pacman::p_load(lme4,nlme, tidyverse,readxl,glmmTMB,DHARMa,regclass,performance,effectsize,ggeffects,flextable)

p_load(marginaleffects,performance, insight, sjPlot, ggeffects,patchwork)

# install.packages("Matrix", type = "binary")
# find.package("Matrix")        # path to the package
# packageVersion("Matrix")      # should report 1.6.5
# 
# # Does the required symbol exist ?
# exists("cholmod_factor_ldetA", mode = "function")

#we are missing the cholmod_factor_ldeta

nlcd<-read_csv("Output/NLCD_proportion_pivoted_randompoints.csv") %>%
  mutate(event_id = as.character(event_id)) %>% 
  bind_rows(read_csv("Output/NLCD_proportion_pivoted_wildfires.csv")) %>% 
  mutate(forest = rowSums(across(c(deciduous_forest, mixed_forest, evergreen_forest)), na.rm = TRUE)) %>% 
  rename(Event_ID = event_id) %>% 
  select(Event_ID, forest,pasture_hay,shrub_scrub) %>% 
  unique()



####Run the mixed effect model for L1 Scale ####

df_L1<-read_excel("Output/Revised_Presence_Absence_L1_crops_hay_filtered.xlsx") %>%  
  mutate(#PA = as.factor(PA),
       PA = as.numeric(PA),
    year = as.numeric(year(Ig_Date))-1990, # change the year to start at 0 
       percent_woody = percent_woody*100, 
       ) %>% 
  left_join(nlcd)
colnames(df_L1)
head(df_L1)
range(df_L1$vpd_1.1.window_mean)

model<-glmmTMB(PA ~ percent_woody + spei.5.1.window_mean
       + pdsi.5.1.window_mean+ vpd_1.1.window_mean + spei.5.1.window_mean*percent_woody+ vs_1.1.window_mean + (1|NA_L3NAME)+
         (1|year), data = df_L1, family = binomial)


summary(model)
#STOP THE PRESS WE HAVE A SIGNIFICANT INTERACTION

residuals_L1 <- simulateResiduals(model)

plot(residuals_L1)

print(summary)
df_confint<-as.data.frame(confint(model) )%>% 
  rownames_to_column(var = "Term") %>% 
  rename(Log_odds = "Estimate") %>% 
  mutate(Odds_Ratio = exp(Log_odds),
          OR2.5 = exp(`2.5 %`),
          OR97.5 = exp(`97.5 %`))

print(round(df_confint$OR97.5,3))
fixed<-fixef(model)
oddsratio<-exp(unlist(fixed[1]))
print("odds ratios")
print(oddsratio)


column_key<- c("vpd_1.1.window_mean" = "Vapor Pressure Deficit (kPa)",
                            "vs_1.1.window_mean" = "Wind Speed (m/s)",
                            "pdsi.5.1.window_mean" = "PDSI",
                            "spei.5.1.window_mean" = "14 Day SPEI",
                            "year"= "Year",
                            "percent_woody" = "Woody Cover (%)",
                            "percent_woody:spei.5.1.window_mean " = "Woody Cover X 14 Day SPEI")
  
#Visualize
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
  
  
  
  return(plts) 
  
}



plots_fixed<-makeplots(model)


int_L1 <- ggpredict(model, terms = c("percent_woody", "spei.5.1.window_mean[-2,-1,0,1]"))

# Create a ggplot
int_L1<- ggplot(int_L1, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +  # Line for predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
  
  labs(
    x = "Woody Cover (%)",
    y = "",
    color = "SPEI14",
    fill = "SPEI14",
  ) +
  theme_bw(base_size = 25)+
  theme(text = element_text(size = 20))+
  scale_y_continuous(
    limits = c(0, 1),          
    labels = scales::percent   
  )

#super cool. 



rr_df <- as.data.frame(ranef(model, condVar = TRUE)) 
#split into two dataframes so I can have two factor systems. 

rr_df_NA_L3NAME<-rr_df %>% 
  filter(grpvar == "NA_L3NAME") %>% 
  mutate(grp = factor(grp, 
                        levels = c("Acadian Plains and Hills",
                                   "Arkansas Valley",
                                   "Atlantic Coastal Pine Barrens",
                                   "Blue Ridge",
                                   "Boston Mountains",
                                   "Central Appalachians",
                                   "Central Corn Belt Plains",
                                   "Driftless Area",
                                   "East Central Texas Plains",
                                   "Eastern Corn Belt Plains",
                                   "Eastern Great Lakes Lowlands",
                                   "Interior Plateau",
                                   "Interior River Valleys and Hills",
                                   "Middle Atlantic Coastal Plain",
                                   "Mississippi Alluvial Plain",
                                   "Mississippi Valley Loess Plains",
                                   "North Central Hardwood Forests",
                                   "Northeastern Coastal Zone",
                                   "Northern Allegheny Plateau",
                                   "Northern Piedmont",
                                   "Ouachita Mountains",
                                   "Ozark Highlands",
                                   "Piedmont",
                                   "Ridge and Valley",
                                   "South Central Plains",
                                   "Southeastern Plains",
                                   "Southeastern Wisconsin Till Plains",
                                   "Southern Coastal Plain",
                                   "Southwestern Appalachians",
                                   "Western Allegheny Plateau")))
random_region<-ggplot(rr_df_NA_L3NAME, aes(x = condval, y = grp)) +
  geom_point() +
  geom_errorbarh(aes(xmin = condval - 1.96 * condsd, xmax = condval + 1.96 * condsd), height = 0.2) +
  labs(x = "Conditional mode (intercept)", y = "L3 Ecoregion") +
  theme_bw()+
  scale_y_discrete(limits = rev)
ggsave("Output/figures/ecoregion_random_intercepts.jpg")

rr_df_year<-rr_df %>% 
  filter(grpvar == "year") %>%
  mutate(year = as.numeric(as.character(grp)) + 1990) %>% 
  mutate(year = factor(year, levels = c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,
                                       2001,2002,2003,2004,2005,2006,2007,2008,2009,
                                       2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)))
random_year<-ggplot(rr_df_year, aes(x = condval, y = year)) +
  geom_point() +
  geom_errorbarh(aes(xmin = condval - 1.96 * condsd, xmax = condval + 1.96 * condsd), height = 0.2) +
  labs(x = "Conditional mode (intercept)", y = "Year") +
  theme_bw()

ggsave("Output/figures/Year_random_intercepts.jpg")



plots_L1_all<-append(plots_fixed, list(int_L1)) #but we don't want spei and woody cover term. 


#remove spei and woody cover
plots_L1_all<-plots_L1_all[c(3:6)]


plots_L1_wrapped<-wrap_plots(plots_L1_all,
                             byrow = FALSE,
                             nrow = 2,
                             axis_titles = "collect")+
  plot_annotation(title = "L1 Eastern Temperate Forests",
                 # subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))

plots_random_wrapped<-wrap_plots(list(random_year,random_region),
                                 byrow = FALSE,
                                 nrow = 2,
                                 axis_titles = "collect")+
  plot_annotation(title = "L1 Eastern Temperate Forests",
                  # subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))


ragg::agg_png("Output/figures/L1_fixedeffects_plots.png", 
              width = 5, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_L1_wrapped
dev.off()

ragg::agg_png("Output/figures/L1_randomintercepts_plots.png", 
              width = 3.5, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_random_wrapped
dev.off()

summary<-summary(model)
#calculate the standardized odds ratios
p_values <-as.data.frame(summary$coefficients$cond[,4]) %>% 
  rownames_to_column() %>% 
   rename(Parameter = "rowname",
          p.value = "summary$coefficients$cond[, 4]") %>% 
   dplyr::select(c(Parameter,p.value))

factor_order<- c("(Intercept)",
                 "vpd_1.1.window_mean",
                 "vs_1.1.window_mean",
                 "pdsi.5.1.window_mean",
                 "percent_woody:spei.5.1.window_mean")
column_key <- c("vpd_1.1.window_mean" = "Vapor Pressure Deficit (kPa)",
                "vs_1.1.window_mean" = "Wind Speed (m/s)",
                "pdsi.5.1.window_mean" = "PDSI",
                "spei.5.1.window_mean" = "14 Day SPEI",
                "percent_woody" = "Woody Cover (%)",
                "percent_woody:spei.5.1.window_mean" = "Woody Cover % X 14 Day SPEI")

L1_standardized <- standardize_parameters(
  model,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(p_values) %>% 
  filter(Parameter != "percent_woody") %>% 
  filter(Parameter != "spei.5.1.window_mean") %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 
  


ragg::agg_png("Output/figures/L1_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)

ggplot(L1_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "L1 Eastern Temperate Forests"
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
    
    # Optional: turn off top/right border (can be useful for minimal theme)
    panel.border = element_blank()
  ) +# Add axis lines manually since theme_minimal doesn't show them by default
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
   geom_label(aes(x = Std_Odds_Ratio+ 0.1,
                  y = y_numeric + 0.4,
                  label = paste0("p = ", signif(p.value, 2), "\nOR = ", signif(Std_Odds_Ratio, 2))
  ),
  #label = paste0("OR = ", signif(Std_Odds_Ratio, 2))),
  fill = "white", color = "black", size = 3,
  label.size = 0.2, label.r = unit(0.05, "lines")
  )
#now I need the p value.
dev.off()  





####L3 Level####

column_key<- c("vpd_1.1.window_mean" = "Vapor Pressure Deficit (kPa)",
               "vs_1.1.window_mean" = "Wind Speed (m/s)",
               "pdsi.5.1.window_mean" = "PDSI",
               "spei.5.1.window_mean" = "14 Day SPEI",
               "year"= "Year",
               "percent_woody" = "Woody Cover (%)",
               "percent_woody:spei.5.1.window_mean " = "Woody Cover X 14 Day SPEI")

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

df_L3<-df_L3 %>%
  filter(NA_L3NAME %in% counts_keep$NA_L3NAME) %>% 
  group_by(NA_L3NAME) %>% 
  group_split()
  
#run L3 models, using the global model. 

####verify that there are no significant interactions. ####

L3_models<-invisible(lapply(df_L3, function(df){
  region = unique(df$NA_L3NAME)
  n_fires = nrow(df)/2
  model = glm(PA ~ percent_woody + spei.5.1.window_mean+  vpd_1.1.window_mean + vs_1.1.window_mean
                  + pdsi.5.1.window_mean +year+vpd_1.1.window_mean*percent_woody+
                spei.5.1.window_mean*percent_woody,
                  data = df, family = binomial(link = "logit"))
  print(region)
  
  print(summary(model))
}))


#Pdsi in middle atlantic Coastal Plain
#pdsi in ouachita mountains



L3_models<-invisible(lapply(df_L3, function(df){
  region = unique(df$NA_L3NAME)
  n_fires = nrow(df)/2
  model = glmmTMB(PA ~ percent_woody + spei.5.1.window_mean+vpd_1.1.window_mean*percent_woody+  vpd_1.1.window_mean + vs_1.1.window_mean
                  + pdsi.5.1.window_mean +year,
                  data = df, family = binomial(link = "logit"))
  print(region)
  
  print(summary(model))
}))

#and the middle atlantic coastal plain does here. 


L3_models<-invisible(lapply(df_L3, function(df){
  region = unique(df$NA_L3NAME)
  n_fires = nrow(df)/2
  model = glmmTMB(PA ~ percent_woody + spei.5.1.window_mean+vpd_1.1.window_mean*percent_woody+ pdsi.5.1.window_mean*percent_woody + vpd_1.1.window_mean + vs_1.1.window_mean
                  + pdsi.5.1.window_mean +year,
                  data = df, family = binomial(link = "logit"))
  print(region)
  
  print(summary(model))
}))

#here we have vpd interaction in middle atlantic coastal plain
#pdsi interaction in ouachita mountains


#
#I say run Ouachita Mountains separately with its pdsi interaction

#And...Middle Atlantic Coastal Plain with vpd. 






