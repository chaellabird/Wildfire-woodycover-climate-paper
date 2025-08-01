#Make nice plots specific to each region, and wrap them together.
#at some point I'll need to fix the old code to get AIC tables for each region 
#that will probably end up in the supplemental.
####Load packages ####
library(pacman)
pacman::p_load(jtools,tidyverse,readxl,
               MuMIn,writexl,prettyglm,
               ggeffects,sjPlot,
               sjmisc,sjlabelled,
               flextable,patchwork,ragg,questionr,arm,
               effectsize,performance)

output_folder<-"Output/Graphs and Figures/probabilites/"
column_key <- c("vpd" = "Vapor Pressure Deficit (kPa)",
                "vs" = "Wind Speed (m/s)",
                "pdsi" = "PDSI",
                "spei14" = "14 Day SPEI",
                "percent_cover" = "Woody Cover (%)",
                "percent_cover:vpd" = "Woody Cover X VPD",
                "percent_cover:pdsi" = "Woody Cover X PDSI",
                "percent_cover:spei14" = "Woody Cover X SPEI",
                "pdsi:percent_cover" = "Woody Cover X PDSI")

factor_order<- c("(Intercept)",
  "vpd",
"vs",
"pdsi",
"spei14",
"percent_cover",
"percent_cover:vpd",
"percent_cover:pdsi",
"percent_cover:spei14",
"pdsi:percent_cover")


factor_order_new_names <- c("Vapor Pressure Deficit (kPa)",
                  "Wind Speed (m/s)",
                  "PDSI",
                  "14 Day SPEI",
                  "Woody Cover (%)",
                  "Woody Cover X VPD",
                  "Woody Cover X PDSI",
                  "Woody Cover X SPEI",
                  "Woody Cover X PDSI")


makeplots<-function(model){
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
    if (idx <= 3) {
      plot <- plot + ylab(str_wrap("Predicted Fire Probability",width = 17))
    }
    
    return(plot)
  }) 

  
  
  return(plts) 
  
}



####L1 Eastern Temperate Forests####
  L1_data<-read_excel('Output/Data/Presence_Absence_L1.xlsx')
  n_fires = L1_data %>% 
    filter(PA == 1) %>%
    pull(Event_ID) %>% 
    n_distinct()
    
#####Fit model#####
  model_L1 = glm(PA ~ vpd + vs  + pdsi + percent_cover*spei14, 
                                  data = L1_data,
                                  family = binomial(link = "logit"))

  
#####compute standardized effects#####  
 #extract p value from model
  p_value<-as.data.frame(summary(model_L1)$coefficients) %>% 
    rownames_to_column() %>% 
    rename(Parameter = "rowname",
           p.value = "Pr(>|z|)") %>% 
    dplyr::select(c(Parameter,p.value))
  
  
  L1_standardized <- standardize_parameters(
    model_L1,
    ci = 0.95,
    exp = TRUE
  )%>%
    left_join(p_value) %>% 
    mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
    arrange(Parameter) %>% 
    mutate(y_numeric = as.numeric(Parameter)) 
  
  ragg::agg_png("Output/Graphs and Figures/importance/L1_standardized effect.png", 
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
  
####Model diagnostics####  
  ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/L1 Eastern.png", 
                width = 4, height = 4, units = "in", 
                res = 300, scaling = 0.4)
  
  check_model(model_L1, plot = TRUE)
  dev.off()
  
  
  
#####examine binned residuals#####  
  ragg::agg_png("Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/L1Binnedresiduals.png", 
                width = 4, height = 4, units = "in", 
                res = 300, scaling = 0.8)
 binnedplot(model_L1$fitted.values ,model_L1$residuals, nclass=NULL, 
             xlab="Expected Values", ylab="Average residual", 
             main="Binned residual plot", 
             cex.pts=0.8, col.pts=1, col.int="gray")
 dev.off()
  
#####Make the plots#####
  non_int_plots_L1<- makeplots(model_L1)

#Interaction Plot
  int_L1 <- ggpredict(model_L1, terms = c("percent_cover", "spei14[-2,-1,0,1]"))
  
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
  
  
  
  plots_L1_all<-append(non_int_plots_L1,list(int_L1))
#remove spei and woody cover
  plots_L1<-plots_L1_all[c(1:3,6)]
 
plots_L1_wrapped<-wrap_plots(plots_L1,
           byrow = FALSE,
           nrow = 2,
           axis_titles = "collect")+
  plot_annotation(title = "L1 Eastern Temperate Forests",
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))



ragg::agg_png("Output/Graphs and Figures/probabilites/L1_plots_probabilities.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_L1_wrapped
dev.off()






# ggsave(plots_L1_wrapped,
#        file = "Output/Graphs and Figures/probabilites/L1_plots_probabilities.jpg",
#        height = 2.25,
#        width = 2.25,
#        dpi = 300)
####Load L3 Data#### 
  L3_data<-read_excel('Output/Data/L3_Presence_Absence_Eight_Regions.xlsx')

####Central Appalachians####
  model_CA=glm(PA ~ vpd + vs  + pdsi+percent_cover + spei14, 
             data = L3_data %>% 
               filter(NA_L3NAME == "Central Appalachians"),
             family = binomial(link = "logit"))
####Model diagnostics####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/CA Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_CA, plot = TRUE)
dev.off()
#####examine binned residuals #####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/CentralAppalachians.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_CA$fitted.values ,model_CA$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Central Appalachians", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

  n_fires = L3_data %>%
    filter(NA_L3NAME == "Central Appalachians",
    PA == 1) %>% 
    pull(Event_ID) %>% 
    n_distinct()
  

#####Compute standardized effects#####  
  
  CA_standardized <- standardize_parameters(
    model_CA,
    ci = 0.95,
    exp = TRUE
  )%>%
    left_join(as.data.frame(summary(model_CA)$coefficients) %>% 
                rownames_to_column() %>% 
                rename(Parameter = "rowname",
                       p.value = "Pr(>|z|)") %>% 
                dplyr::select(c(Parameter,p.value))) %>% 
    mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
    arrange(Parameter) %>% 
    mutate(y_numeric = as.numeric(Parameter)) 
  
  ragg::agg_png("Output/Graphs and Figures/importance/CA_standardized effect.png", 
                width = 4.5, height = 4, units = "in", 
                res = 300, scaling = 0.8)
  ggplot(CA_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
    theme_minimal() +
    labs(
      y = NULL,
      x = "Standardized Odds Ratios",
      subtitle = "Central Appalachians"
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
      axis.title = element_text(color = "black"),
      plot.margin = margin(20, 20, 20, 20) 
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

  dev.off()  
  
  
#####make the partial plots#####  
plots_CA<-makeplots(model_CA)
wrapped_CA<-wrap_plots(plots_CA,
                       byrow = FALSE,
                       nrow = 3,
                     axis_titles = "collect")+
  plot_annotation(title = "Central Appalachians",
                  subtitle =paste0("Number of fires: ",n_fires),
                                   theme = theme(plot.title = element_text(size = 25),
                                                 plot.subtitle = element_text(size = 20)))

ragg::agg_png(file = "Output/Graphs and Figures/probabilites/CentralAppalachians_probabilities.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
wrapped_CA
dev.off()


# ggsave(wrapped_CA,
#        file = "Output/Graphs and Figures/probabilites/CentralAppalachians_probabilities.jpg",
#        width = 6,
#        height = 8,
#        dpi = 300)  




  
####Middle Atlantic####
model_MA1 = glm(PA ~spei14 + vs  + pdsi+ percent_cover + vpd, 
                               data = L3_data %>% 
                   filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
                               family = binomial(link = "logit"))
model_MA2 = glm(PA ~spei14 + percent_cover, 
                data = L3_data %>% 
                  filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
                family = binomial(link = "logit"))
model_MA = model.avg(model_MA1,model_MA2)
model_MA$coefficients[1,]
full_averaged_coefficients<-coefTable(model_MA, full = TRUE)
summary(model_MA)

#only use full averaged coefficients


####Model diagnostics####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/MA Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_MA, plot = TRUE)
dev.off()
#####examine binned residuals#####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Middle Atlantic.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_MA$fitted.values ,model_MA$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Middle Atlantic Coastal Plain", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

n_fires = L3_data %>%
  filter(NA_L3NAME == "Middle Atlantic Coastal Plain") %>% 
  filter(PA == 1) %>%
  dplyr::select(Event_ID) %>% 
  n_distinct()

#####Standardize effect sizes#####
std_coefs <- std.coef(model_MA, full = TRUE, partial.sd = FALSE) 
Parameter<- rownames(std_coefs)


MA_standardized<-as_tibble(std_coefs) %>% 
  rename(Estimate= "Estimate*") %>% 
  mutate(Parameter = Parameter,
         #CI = exp(confint(model_MA, full = TRUE)), # I need the STANDARDIZED confidence interval
        # CI_low = CI[,1],
        # CI_high = CI[,2],
         Std_Odds_Ratio = as.numeric(exp(Estimate))) %>% 
 # dplyr::select(-c(df,CI,Estimate)) %>%
  

  left_join(as.data.frame(summary(model_MA)$coefmat.full) %>% #this is the tricky step.
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 

ragg::agg_png("Output/Graphs and Figures/importance/MA_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(MA_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  #geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "Middle Atlantic Coastal Plain"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  



#####Make the plots#####
  non_int_plots_MA<- makeplots(model_MA)
  
#Interaction Plot
 # int_MA_vpd <- ggpredict(model_MA, terms = c("percent_cover", "vpd[0.5,1,1.5]"))
  #int_MA_pdsi<-ggpredict(model_MA, terms = c("percent_cover", "pdsi[-4,-2,0,4]"))
# Create a ggplot
  # int_MA_vpd<- ggplot(int_MA_vpd, aes(x = x, y = predicted, color = group)) +
  #   geom_line(size = 1) +  # Line for predicted values
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
  #   labs(
  #     x = "Woody Cover (%)",
  #     y = "",
  #     color = "VPD",
  #     fill = "VPD",
  #   ) +
  #   theme_bw(base_size = 25)+
  #   theme(text = element_text(size = 20))+
  #   scale_y_continuous(
  #     limits = c(0, 1),          
  #     labels = scales::percent   
  #   ) 
  # 
 
  
  
  #plots_MA_all<-append(non_int_plots_MA,list(int_MA_vpd))
  plots_MA<-non_int_plots_MA
  plots_MA_wrapped<-wrap_plots(plots_MA,
                               byrow = FALSE,
                               nrow = 2,
                               axis_titles = "collect")+
    plot_annotation("Middle Atlantic Coastal Plain",
                    subtitle =paste0("Number of fires: ",n_fires),
                    theme = theme(plot.title = element_text(size = 25),
                                  plot.subtitle = element_text(size = 20)))
  
#   #need to change size of the interaction plots to be bigger. 
# design <- "
# AAADDDGGG
# AAADDDGGG
# BBBEEEGGG
# BBBEEEHHH
# CCCFFFHHH
# CCCFFFHHH
# "
#   
#   # Combine the plots using the design
#   plots_MA_wrapped <- wrap_plots(
#     A = plots_MA[[1]], B = plots_MA[[2]],
#     C = plots_MA[[3]], D = plots_MA[[4]],
#     E = plots_MA[[5]], F = plots_MA[[6]],
#     G = plots_MA[[7]], H = plots_MA[[8]],
#     design = design
#   )+
#     plot_annotation("Middle Atlantic Coastal Plain",
#                     subtitle =paste0("Number of fires: ",n_fires))
#   
#   
  ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Middle Atlantic_probability.png", 
                width = 4, height = 4, units = "in", 
                res = 300, scaling = 0.4)
  plots_MA_wrapped
  dev.off()
  
  
 #  
 # ggsave(plots_MA_wrapped,
 #        file = "Output/Graphs and Figures/probabilites/Middle Atlantic_probability.jpg",
 #        dpi = 300) 
 #  
  
####Ouachita Mountain####
  model_OM =glm(PA ~ vpd + vs  + spei14 + pdsi, 
              data  = L3_data %>% 
                filter(NA_L3NAME == "Ouachita Mountains",
                       Event_ID !=2459),
              family = binomial(link = "logit"))
####Model diagnostics####  
  ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/OM Eastern.png", 
                width = 4, height = 4, units = "in", 
                res = 300, scaling = 0.4)
  
  check_model(model_OM, plot = TRUE)
  dev.off()
#####Examine binned residuals#####
    ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Ouachita.png", 
                width = 4, height = 4, units = "in", 
                res = 300, scaling = 0.6)
  binnedplot(model_OM$fitted.values ,model_OM$residuals, 
             xlab="Expected Values", ylab="Average residual", 
             main="Binned residual plot Ouachita Mountains", 
             cex.pts=0.8, col.pts=1, col.int="gray")
  dev.off()
 
 #when we remove the point with high leverage there is a clear top model
 
 n_fires = L3_data %>%
   filter(NA_L3NAME == "Ouachita Mountains") %>% 
   filter(PA == 1) %>%
   select(Event_ID) %>% 
   n_distinct()
#####standardized effect sizes#####
 OM_standardized <- standardize_parameters(
   model_OM,
   ci = 0.95,
   exp = TRUE
 )%>%
   left_join(as.data.frame(summary(model_OM)$coefficients) %>% 
               rownames_to_column() %>% 
               rename(Parameter = "rowname",
                      p.value = "Pr(>|z|)") %>% 
               dplyr::select(c(Parameter,p.value))) %>% 
   mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
   arrange(Parameter) %>% 
   mutate(y_numeric = as.numeric(Parameter)) 
 
 ragg::agg_png("Output/Graphs and Figures/importance/OM_standardized effect.png", 
               width = 4.5, height = 4, units = "in", 
               res = 300, scaling = 0.8)
 ggplot(OM_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
   geom_point(size = 3, color = "blue") +
   geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
   geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
   theme_minimal() +
   labs(
     y = NULL,
     x = "Standardized Odds Ratios",
     subtitle = "Ouachita Mountains"
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
     axis.title = element_text(color = "black"),
     plot.margin = margin(20, 20, 20, 20) 
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
 
 dev.off()  
 
 
 
 
 #####make plots##### 
plots_OM<- makeplots(model_OM) 
plots_OM_wrapped<-wrap_plots(plots_OM,
                             byrow = FALSE,
                             nrow = 2,
                             axis_titles = "collect")+
  plot_annotation("Ouachita Mountains",
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))


ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Ouachita_Mountains_probabilities.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_OM_wrapped
dev.off()



 
####Ozark Highlands####
model_OH = glm(PA ~ spei14 + vs  + pdsi + percent_cover+ vpd, 
                             data = L3_data %>% 
                 filter(NA_L3NAME == "Ozark Highlands"),
                             family = binomial(link = "logit"))
####Model diagnostics####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/OH Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_OH, plot = TRUE)
dev.off()
#####examine binned residuals #####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Ozark Highlands.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_OH$fitted.values ,model_OH$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Ozark Highlands", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()
#####standardize effects#####
OH_standardized <- standardize_parameters(
  model_OH,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(as.data.frame(summary(model_OH)$coefficients) %>% 
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 
ragg::agg_png("Output/Graphs and Figures/importance/OH_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(OH_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "Ozark Highlands"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  



#####make plots#####
n_fires = L3_data %>%
  filter(NA_L3NAME == "Ozark Highlands") %>% 
  filter(PA == 1) %>%
  select(Event_ID) %>% 
  n_distinct()
non_int_plots_OH<- makeplots(model_OH)


plots_OH_wrapped<-wrap_plots(non_int_plots_OH,
                             byrow = FALSE,
                             nrow = 3,
                             axis_titles = "collect")+
  plot_annotation(title = "Ozark Highlands",
                  subtitle = paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))
ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Ozark_highlands_probabilities.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_OH_wrapped
dev.off()



####Ridge and valley####
data_RV<- L3_data %>% 
  filter(NA_L3NAME == "Ridge and Valley")
model_RV<-glm(formula = PA ~ spei14 + percent_cover+vpd+pdsi+vs, 
              family = binomial(link = "logit"), 
              data = L3_data %>% 
                filter(NA_L3NAME == "Ridge and Valley"))
#####Model diagnostics#####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/RV Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_RV, plot = TRUE)
dev.off()
#
#####examine binned residuals#####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Ridge and Valley.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_RV$fitted.values ,model_RV$residuals, 
                          xlab="Expected Values", ylab="Average residual", 
                          main="Binned residual plot RV", 
                          cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

#####calculate standardized effect size #####

RV_standardized <- standardize_parameters(
  model_RV,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(as.data.frame(summary(model_RV)$coefficients) %>% 
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 

ragg::agg_png("Output/Graphs and Figures/importance/RV_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(RV_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "Ridge and Valley"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  



#####Make plots#####
odds.ratio(model_RV)
n_fires = L3_data %>%
  filter(NA_L3NAME == "Ridge and Valley") %>% 
  filter(PA == 1) %>%
  pull(Event_ID) %>% 
  n_distinct()
plots_RH<-makeplots(model_RH)

plots_RH_wrapped<-wrap_plots(plots_RH,
                             byrow = FALSE,
                             nrow = 3,
                             axis_titles = "collect")+
  plot_annotation("Ridge and Valley",
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))
ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Ridge and Valley_probabilities.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_RH_wrapped
dev.off()



#### South Central Plains####
model_SCeP<- glm(formula = PA ~ vpd + vs + pdsi + percent_cover + spei14, 
               family = binomial(link = "logit"),
               data = L3_data %>% 
                filter(NA_L3NAME == "South Central Plains"))
#####Model diagnostics#####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/SCP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SCeP, plot = TRUE)
dev.off()#
#####examine binned residuals#####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/South Central.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_SCeP$fitted.values ,model_SCeP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot South Central Plains", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

n_fires = L3_data %>%
  filter(NA_L3NAME == "South Central Plains") %>% 
  filter(PA == 1) %>%
  select(Event_ID) %>% 
  n_distinct()
#####standardize effect size#####

SCeP_standardized <- standardize_parameters(
  model_SCeP,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(as.data.frame(summary(model_SCeP)$coefficients) %>% 
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 

ragg::agg_png("Output/Graphs and Figures/importance/SCeP_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(SCeP_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "South Central Plains"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  



#####make plots#####
plots_SCep<-makeplots(model_SCeP)
plots_SCep_wrapped <-wrap_plots(plots_SCep,
                                byrow = FALSE,
                                nrow = 3,
                                axis_titles = "collect")+
  plot_annotation(title = "South Central Plains",
                  subtitle = paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))
ragg::agg_png(file = "Output/Graphs and Figures/probabilites/South Central Plain.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_SCep_wrapped
dev.off()

# ggsave(plots_SCep_wrapped,
#        file = "Output/Graphs and Figures/probabilites/SouthCentralPlains.jpg",
#        dpi = 300)


####Southeastern Plains####
model_SP<-glm(formula = PA ~ vpd + vs + pdsi + percent_cover + spei14, 
            family = binomial(link = "logit"),
            data = L3_data %>% 
              filter(NA_L3NAME == "Southeastern Plains"))
#####Model diagnostics#####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/SP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SP, plot = TRUE)
dev.off()
#####Make binned residuals#####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Southeastern Plains.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_SP$fitted.values ,model_SP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Southeastern Plains", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()
#####standardize residuals#####
SP_standardized <- standardize_parameters(
  model_SP,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(as.data.frame(summary(model_SP)$coefficients) %>% 
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
        mutate(y_numeric = as.numeric(Parameter)) 


ragg::agg_png("Output/Graphs and Figures/importance/SP_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(SP_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "Southeastern Plains"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  

OR_VPD <-(exp(1.885599)-1)/10
OR_vs <-exp(0.381908)
OR_SPEI <-exp(  -0.868270)
OR_woody<-exp(0.017889)
#####make plots#####
n_fires = L3_data %>%
  filter(NA_L3NAME == "Southeastern Plains") %>% 
  filter(PA == 1) %>%
  select(Event_ID) %>% 
  n_distinct()
plots_SP<-makeplots(model_SP)
plots_SP_wrapped <-wrap_plots(plots_SP,
                              byrow = FALSE,
                              nrow = 3,
                              axis_titles = "collect")+
  plot_annotation(title = "Southeastern Plains",
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))

ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Southeastern_plains.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
plots_SP_wrapped
dev.off()





####Southern Coastal Plain####
model_SCoP<-glm(formula = PA ~ vs + pdsi * percent_cover + 
      vpd+spei14*percent_cover, 
              family = binomial(link = "logit"),
              data = L3_data %>% 
        filter(NA_L3NAME == "Southern Coastal Plain"))
summary(model_SCoP)
#####Model diagnostics#####  
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/check_model/SCoP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SCoP, plot = TRUE)
dev.off()



#####examine binned residuals#####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Top Model Diagnostics/binnedresiduals/Southern Coastal.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.6)
binnedplot(model_SCoP$fitted.values ,model_SCoP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Southern Coastal Plain", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()
#####standardize estimates#####
SCoP_standardized <- standardize_parameters(
  model_SCoP,
  ci = 0.95,
  exp = TRUE
)%>%
  left_join(as.data.frame(summary(model_SCoP)$coefficients) %>% 
              rownames_to_column() %>% 
              rename(Parameter = "rowname",
                     p.value = "Pr(>|z|)") %>% 
              dplyr::select(c(Parameter,p.value))) %>% 
  mutate(Parameter = factor(Parameter, levels = factor_order[factor_order %in% Parameter])) %>% 
  arrange(Parameter) %>% 
  mutate(y_numeric = as.numeric(Parameter)) 
ragg::agg_png("Output/Graphs and Figures/importance/SCoP_standardized effect.png", 
              width = 4.5, height = 4, units = "in", 
              res = 300, scaling = 0.8)
ggplot(SCoP_standardized, aes(y = Parameter, x = Std_Odds_Ratio)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "longdash", color = "black") +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Standardized Odds Ratios",
    subtitle = "Southern Coastal Plain"
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
    axis.title = element_text(color = "black"),
    plot.margin = margin(20, 20, 20, 20) 
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

dev.off()  

#####make plots#####
n_fires = L3_data %>%
  filter(NA_L3NAME == "Southern Coastal Plain") %>% 
  filter(PA == 1) %>%
  dplyr::select(Event_ID) %>% 
  n_distinct()





        
non_int_plots_SCoP<- makeplots(model_SCoP)

#Interaction Plot
#int_SCoP_vpd <- ggpredict(model_SCoP, terms = c("percent_cover", "vpd[0.5,1,1.5]"))
int_SCoP_pdsi<-ggpredict(model_SCoP, terms = c("percent_cover", "pdsi[-4,-2,0,4]"))
intSCoP_spei <-ggpredict(model_SCoP,terms = c("percent_cover", "spei14[-2,-1,0,1]"))
# Create a ggplot
 # int_SCoP_vpd<- ggplot(int_SCoP_vpd, aes(x = x, y = predicted, color = group)) +
 #   geom_line(size = 1) +  # Line for predicted values
 #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
 #   labs(
 #     x = "Woody Cover (%)",
 #     y = "",
 #     color = "VPD",
 #     fill = "VPD",
 #   ) +
 #   theme_bw(base_size = 25)+
 #   theme(text = element_text(size = 20),
 #         plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
 #   scale_y_continuous(
 #     limits = c(0, 1),          
 #     labels = scales::percent   
 #   ) 



int_SCoP_pdsi<- ggplot(int_SCoP_pdsi, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +  # Line for predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
  labs(
    x = "Woody Cover (%)",
    y = "",
    color = "PDSI",
    fill = "PDSI",
  ) +
  theme_bw(base_size = 25)+
  
  theme(text = element_text(size = 20),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  scale_y_continuous(
    limits = c(0, 1),          
    labels = scales::percent   
  ) 

int_SCoP_spei<- ggplot(intSCoP_spei, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +  # Line for predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +  # Confidence interval
  labs(
    x = "Woody Cover (%)",
    y = "",
    color = "14-Day SPEI",
    fill = "14-Day SPEI",
  ) +
  theme_bw(base_size = 25)+
  
  theme(text = element_text(size = 20),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
  scale_y_continuous(
    limits = c(0, 1),          
    labels = scales::percent   
  ) 




plots_SCoP_all<-append(non_int_plots_SCoP,list(plot_spacer(),int_SCoP_pdsi,int_SCoP_spei))




plots_SCoP_wrapped<-wrap_plots(plots_SCoP_all,
                                  byrow = FALSE,
                                  nrow = 3,
                                  #widths = c(1.,1.2),
                                  axis_titles = "collect")+
  
  plot_annotation("Southern Coastal Plain",
                  subtitle =paste0("Number of fires: ",n_fires),
                  theme = theme(plot.title = element_text(size = 25),
                                plot.subtitle = element_text(size = 20)))



ragg::agg_png(file = "Output/Graphs and Figures/probabilites/Southern_coastal_plains.png", 
              width = 5, height = 4.75, units = "in", 
              res = 300, scaling = 0.5)
plots_SCoP_wrapped
dev.off()




# ggsave(plots_SCoP_wrapped,
#        file = "Output/Graphs and Figures/probabilites/Southern Coastal Plain.jpg")
# 
# 





















####Create a table with the odds ratios####

#collect all best models into a list
  #all <- mget(ls(), envir = globalenv())
 # all <-all[grep("model", names(all))] 
  
  all<- list(model_L1,
             model_CA,
             #model_MA,
             model_OH,
             model_OM,
             model_RV,
             model_SCeP,
             model_SCoP,
             model_SP)          

#get nicer names
  names<- list(
  "model_L1" = "L1 Eastern Temperate Forests",
  "model_CA"  ="Central Appalachians",                
  #"model_MA"  = "Middle Atlantic Coastal Plain",               
  "model_OH" = "Ozark Highlands",
  "model_OM" = "Ouachita Mountains",
  "model_RV" = "Ridge and Valley",
  #"model_RH_2" = "Ridge and Valley candidate model 2",
  "model_SCeP" = "South Central Plains",                
  "model_SCoP" = "Southern Coastal Plain",
  "model_SP" = "Southeastern Plains")
  names(all) = names
  
  #Create nice names for the terms
pl <- c(`(Intercept)` = "Intercept",
        "vpd" = "Vapor Pressure Deficit (kPa)",
        "vs" = "Wind Speed (m/s)",
        "sm" = "Soil Moisture (% water in 1 m3)",
        "pdsi" = "PDSI",
        "spei14" = "14 Day SPEI",
        "percent_cover" = "Woody Cover (%)",
        "pdsi:percent_cover" = "Woody Cover X PDSI",
        "percent_cover:vpd" = "Woody Cover X Vapor Pressure Deficit",
        "percent_cover:spei14"= "Woody Cover X 14 Day SPEI"
          )
#create table and export
# tab_model(all, 
#           pred.labels = pl,
#           collapse.ci = TRUE,
#           digits = 2,
#           dv.labels = names,
#           wrap.labels = 35,
#           CSS = list(css.centeralign = 'text-align: center;'),
#           p.style = "stars",
#           file = "Output/Graphs and Figures/Odds ratio tables/All_oddsratio.html")

####Make table with flextable####

all<-list("L1 Eastern Temperate Forests" = model_L1,
         "Central Appalachians" = model_CA,
         #"Middle Atlantic Coastal Plain"= model_MA_averaged,
         "Ouachita Mountains"= model_OM,
         "Ozark Highlands"= model_OH,
          "Ridge and Valley" = model_RV,
          "South Central Plains" = model_SCeP,
          "Southeastern Plains" = model_SP,
          "Southern Coastal Plain" = model_SCoP)

odds_ratio<-lapply(all, function(model){
  or <-  odds.ratio(model, level = 0.95)
  rownames<-rownames(or)
  OR_L1 = as_tibble(or) %>% 
  mutate(predictors = rownames)
  })%>% 
  bind_rows(.id = "NA_L3NAME") %>% 
  mutate(predictors = recode(predictors,!!!column_key),
         OR = round(OR,3)) %>% 
   mutate(p.new = case_when(p< 0.05 & p > 0.01 ~ "<0.05",#paste0(as_character(round(OR,3)),"*"),
                            p <0.01 & p >0.001 ~ "<0.01",#paste0(as_character(round(OR,3)),"**"),
                            p <0.001 ~ "<0.001",#paste0(as_character(round(OR,3)),"***"),
                            p >0.05 ~as_character(round(p,3)))) %>% 
  dplyr::select("NA_L3NAME","predictors",'OR',"2.5 %","97.5 %","p.new") %>% 
  rename(Ecoregion = NA_L3NAME,
        Predictors = predictors,
        "Odds Ratio" = OR,
        "p value" = p.new) 





ft<-flextable(odds_ratio)

border<-fp_border_default()


  ft<- ft %>% #add_header_row(values = c("","Confidence Intervals",""), colwidths = c(3,2,1)) %>% 
    #merge_v(j= ~Ecoregion+ Predictors) %>% 
  #vline(j = c("Predictors"), border = border, part = "all") %>% #separate predictors from the stats
  # hline(i= ~ Ecoregion == Ecoregion,j = c("Ecoregion", "Predictors"), border = border) %>%#separate every region
    padding(padding = 1, part = "body") %>% 
    set_formatter(#p = function(x){
                  #    formatC(x,format = "e", digits = 2)},
                #  OR =function(x){
                 #     formatC(x,format = "f", digits = 3)},
                  "2.5 %" =function(x){
                    formatC(x,format = "f", digits = 3)},
                  "97.5 %" =function(x){
                    formatC(x,format = "f", digits = 3)}) %>% 
   # set_table_properties(j = c("Ecoregion", "Predictors", "OR", "2.5 %", "97.5"),layout = "autofit", width = 0.8) %>% 
    width(j = c("Ecoregion","Predictors","Odds Ratio", "2.5 %","p value"), width = c(2,
                                                                               1.2,
                                                                               1,
                                                                               0.8,
                                                                               1.1)) %>% 
   # add_footer_lines(values = "* p<0.05   ** p<0.01   *** p<0.001") %>% 
    fontsize(size =10, part = "footer") %>% 
    fontsize(size = 12, part  = "body")

ft

  save_as_docx(
    "OR_ft" = ft, 
    path = "Output/Graphs and Figures/Odds ratio tables/Oddsratio_flextable.docx")

model_MA
  summary(model_MA)
  coefficients_full<-exp(model_MA$coefficients[1,])
 exp(confint(model_MA, full = TRUE)) 
 