#5/23/24
#Michaella Ivey
#Purpose:  
####Load packages####
library(pacman)
pacman::p_load(tidyverse,stringr,readxl,patchwork,ggpubr,gridExtra,grid,ragg)

####Histogram of drought conditions####

drought<-read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Objective 3 Fire Size by Climate and Woody Cover/Data/Year_Of_Fire_drought_stats.csv') 





Drought_distribution<-drought%>% 
  left_join(read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Spatial Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
              group_by(NA_L3NAME) %>% 
              mutate(n_fires =n_distinct(Event_ID))) %>% 
  mutate(date = date(date),
         fire_date = date(fire_date),
    diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
  filter(diff_days <= 5,
          diff_days >= -5,
         n_fires >=10, 
          NA_L3NAME %in% c(            "Central Appalachians",
                                       "Middle Atlantic Coastal Plain",
                                       "Ouachita Mountains",
                                       "Ozark Highlands",
                                       "Ridge and Valley",
                                       "South Central Plains",
                                       "Southeastern Plains",
                                       "Southern Coastal Plain")) %>% 
  na.omit() %>% 
  group_by(NA_L3NAME,US_L3CODE,Event_ID,fire_date) %>%
  summarize(spei14d = mean(spei14d),
            pdsi = mean(pdsi)) %>% 
#add in the L1 fires
   bind_rows(drought %>%
  mutate(date = date(date),
         fire_date = date(fire_date),
         diff_days = abs(as.numeric(difftime(date, fire_date, units = "days")))) %>% 
  filter(diff_days <= 5) %>% 
  na.omit() %>%
  group_by(Event_ID, fire_date) %>% 
  summarize(spei14d = mean(spei14d),
            spei30d = mean(spei30d),
            pdsi = mean(pdsi)) %>% 
  mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
        US_L3CODE = "0")) %>% 
  mutate(NA_L3NAME = factor(NA_L3NAME,
                             levels =c("L1 Eastern Temperate Forests",
                                       "Central Appalachians",
                                       "Middle Atlantic Coastal Plain",
                                       "Ouachita Mountains",
                                       "Ozark Highlands",
                                       "Ridge and Valley",
                                       "South Central Plains",
                                       "Southeastern Plains",
                                       "Southern Coastal Plain")))
                            # levels = c("L1 Eastern Temperate Forests",
                            #            "Arkansas Valley",
                            #            "Atlantic Coastal Pine Barrens",
                            #            "Blue Ridge",
                            #            "Boston Mountains",
                            #            "Central Appalachians",
                            #            "East Central Texas Plains",
                            #            "Interior Plateau",
                            #            "Middle Atlantic Coastal Plain",
                            #            "Mississippi Valley Loess Plains",
                            #            "North Central Hardwood Forests",
                            #            "Ouachita Mountains",
                            #            "Ozark Highlands",
                            #            "Piedmont",
                            #            "Ridge and Valley",
                            #            "South Central Plains",
                            #            "Southeastern Plains",
                            #            "Southern Coastal Plain",
                            #            "Southwestern Appalachians",
                            #            "Western Allegheny Plateau"))) 
  

  
#Create the Dummy layer
legend <- c("5.0 or more (extremely wet)",
            "4.0 to 4.99 (very wet)",
            "3.0 to 3.99 (moderately wet)",
            "2.0 to 2.99 (slightly wet)",
            "1.0 to 1.99 (incipient wet spell)",
            "-0.99 to 0.99(near normal)",
            "-1.99 to -1.00 (incipient dry spell)",
            "-2.99 to -2.00 (mild drought)",
            "-3.99 to -3.00 (moderate drought)",
            "-4.99 to -4.00 (severe drought)",
            "-5.0 or less (extreme drought)")

regions <- c("Central Appalachians", "Middle Atlantic Coastal Plain", "Ouachita Mountains", 
             "Ozark Highlands", "Ridge and Valley", "South Central Plains", 
             "Southeastern Plains", "Southern Coastal Plain", "L1 Eastern Temperate Forests")


# regions<-c("L1 Eastern Temperate Forests",
#            "Arkansas Valley",
#            "Atlantic Coastal Pine Barrens",
#            "Blue Ridge",
#            "Boston Mountains",
#            "Central Appalachians",
#            "East Central Texas Plains",
#            "Interior Plateau",
#            "Middle Atlantic Coastal Plain",
#            "Mississippi Valley Loess Plains",
#            "North Central Hardwood Forests",
#            "Ouachita Mountains",
#            "Ozark Highlands",
#            "Piedmont",
#            "Ridge and Valley",
#            "South Central Plains",
#            "Southeastern Plains",
#            "Southern Coastal Plain",
#            "Southwestern Appalachians",
#            "Western Allegheny Plateau")


df <- data.frame(region = rep(regions, each = 11),
                 legend = rep(legend, length(regions)),
                 x = rep(0, length(regions) * 11),
                 y = rep(0, length(regions) * 11)) 

df<-as_tibble(df) %>% 
  mutate(legend = factor(legend, levels =c("5.0 or more (extremely wet)",
                                           "4.0 to 4.99 (very wet)",
                                           "3.0 to 3.99 (moderately wet)",
                                           "2.0 to 2.99 (slightly wet)",
                                           "1.0 to 1.99 (incipient wet spell)",
                                           "-0.99 to 0.99 (near normal)",
                                           "-1.99 to -1.00 (incipient dry spell)",
                                           "-2.99 to -2.00 (mild drought)",
                                           "-3.99 to -3.00 (moderate drought)",
                                           "-4.99 to -4.00 (severe drought)",
                                           "-5.0 or less (extreme drought)"))) %>% 
  drop_na()



#PDSI

pdsi_density<-ggplot()+
geom_density(data = Drought_distribution %>% 
               filter(NA_L3NAME == "L1 Eastern Temperate Forests"), aes(x =pdsi))+
  geom_point(data = df, 
             aes(x = x, y =y, color =legend), alpha = 0)+
  labs(x = "PDSI",
       y = "Density",
       color = "PDSI",
       tag  = "A")+
  theme_minimal()+
  facet_wrap(~NA_L3NAME, 
             labeller = label_wrap_gen(width = 18))+
  guides(color = guide_legend(ncol = 2, title.position = "top")) +
  theme(strip.text = element_text(size = 10),
        strip.background =element_rect(fill="white"),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10))#,
        plot.tag = element_text(face = "bold", size = 20)


ggsave(pdsi_density,
       file = "Output/Graphs and Figures/pdsi_density.jpg",
       dpi = 350,
       height =5,
      width =8)  











#   


legend_spei <- c("2.0 or more (extremely wet)",
            "1.6 to 1.99 (very wet)",
            "1.3 to 1.59 (moderately wet)",
            "0.8 to 1.29 (slightly wet)",
            "0.5 to 0.79 (incipient wet spell)",
            "-0.49 to 0.49 (near normal)",
            "-0.79 to -0.5 (incipient dry spell)",
            "-1.29 to -0.8 (mild drought)",
            "-1.59 to -1.3 (moderate drought)",
            "-1.99 to -1.6 (severe drought)",
            "-2.0 or less (extreme drought)")


df_spei <- data.frame(region = rep(regions, each = 11),
                 legend = rep(legend_spei, length(regions)),
                 x = rep(0, length(regions) * 11),
                 y = rep(0, length(regions) * 11)) 





#Spei 14d
spei14d_density<-ggplot(data = Drought_distribution%>% 
                          filter(NA_L3NAME == "L1 Eastern Temperate Forests"), aes(x = spei14d))+
  geom_density()+
  geom_point(data = df_spei, 
             aes(x = x, y =y, color =legend), alpha = 0)+
  labs(x = "14 Day SPEI",
       y = "Density",
       color = "14 Day SPEI",
      tag = "B")+
  theme_minimal()+
  facet_wrap(~NA_L3NAME, 
             labeller = label_wrap_gen(width = 20))+
  guides(color = guide_legend(ncol = 2, title.position = "top")) +
  theme(strip.text = element_text(size = 10),
        legend.position = "bottom",
        plot.margin = margin(1,1.5,1,0, "cm"),
        strip.background =element_rect(fill="white"),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        plot.tag = element_text(face = "bold", size = 20)) 





ragg::agg_png("Output/Graphs and Figures/spei14d_density.png", 
              width = 5.5, height = 5, units = "in", 
              res = 300, scaling = 0.6)
spei14d_density
dev.off()




ggsave(spei14d_density,
       file = "Output/Graphs and Figures/spei14d_density.jpg",
       dpi = 350,
       height = 5,
       width = 8) 






list= list(pdsi_density,plot_spacer(),spei14d_density)
wrapped<-wrap_plots(list,
                    widths = c(1,0.1,1),
                    ncol = 3,
           axis_titles = "collect_y")

# ragg::agg_png("Output/Graphs and Figures/Figure2Density_plots.png", 
#               width = 6, height = 4, units = "in", 
#               res = 300, scaling = 0.55)
ragg::agg_png("../Word Documents/screenshots and images/density_plots_L1.png", 
                             width = 6, height = 4, units = "in", 
                             res = 300, scaling = 0.6)
wrapped
dev.off()


# ggsave(wrapped,
#        file = "Output/Graphs and Figures/Figure2Density_plots.jpg",
#        dpi=350,
#        height =10,
#        width = 8)

####Seasonal stuff####
library(ggplot2)
library(ggridges)
library(lubridate)
library(colorspace)  # For better color control

# Add month columns
Drought_distribution$month <- factor(month(Drought_distribution$fire_date, label = TRUE, abbr = TRUE))
Drought_distribution$month_num <- month(Drought_distribution$fire_date)
# Use a perceptually uniform color scale that wraps smoothly
#cyclic_colors <- hcl.colors(12, "Dark 3")  # Alternative: "Set 3"

seasonal_pdsi_density<-ggplot(Drought_distribution, aes(x = pdsi, y = month, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(limits = c(min(Drought_distribution$pdsi), max(Drought_distribution$pdsi))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  #guides(fill = guide_legend(nrow = 2, title.position = "top")) +
  facet_wrap(~NA_L3NAME,
             labeller = label_wrap_gen(width = 20))+
  scale_fill_gradientn(colors = c("darkred", "yellow", "white", "deepskyblue", "navy"),
                       name = "PDSI",
                       breaks = c(-5, 0, 5),
                       labels = c("<-5 Extreme Drought", "0 Near Normal", ">5 Very Wet")) +
  labs(x = "PDSI", y = "Month", tag = "a") +
  theme_bw(base_size = 14)+
  theme(plot.tag = element_text(face = "bold", size = 20))


ragg::agg_png("Output/Graphs and Figures/SeasonalDensity.png", 
              width = 3.25, height = 3.25, units = "in", 
              res = 300, scaling = 0.45)
seasonal_pdsi_density
dev.off()

#now with spei

seasonal_spei14_density<-ggplot(Drought_distribution, aes(x = spei14d, y = month, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(limits = c(min(Drought_distribution$spei14d), max(Drought_distribution$spei14d))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  
  #guides(fill = guide_legend(nrow = 2, title.position = "top")) +
  facet_wrap(~NA_L3NAME,
             labeller = label_wrap_gen(width = 20))+
  scale_fill_gradientn(colors = c("darkred", "yellow", "white", "deepskyblue", "navy"),
                       name = "14-Day SPEI",
                       breaks = c(-2, 0, 2),
                       labels = c("< -2 Extreme Drought", "0 Near Normal", "> 2 Very Wet")) +
  labs(x = "14-Day SPEI", y = "Month", tag = "b") +
  theme_bw(base_size = 14)+
  theme(plot.tag = element_text(face = "bold", size = 20))



ragg::agg_png("Output/Graphs and Figures/SeasonalDensityspei14.png", 
              width = 3.25, height = 3.25, units = "in", 
              res = 300, scaling = 0.45)
seasonal_spei14_density
dev.off()



#set pdsi seasonal density and spei seasonal density next to each other. 

wrapped_seasonality<-wrap_plots(seasonal_pdsi_density,seasonal_spei14_density,
                                ncol = 1)


ragg::agg_png("Output/Graphs and Figures/wrappedSeasonalDensity.png", 
              width = 5, height = 8, units = "in", 
              res = 300, scaling = 0.6)
wrapped_seasonality
dev.off()


