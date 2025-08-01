library(pacman)
pacman::p_load(tidyverse,readxl,patchwork, lemon, ragg)

drought<-read_csv('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Objective 3 Fire Size by Climate and Woody Cover/Data/Year_Of_Fire_drought_stats.csv') 


drought2<-drought %>% 
  left_join(read_excel('C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Spatial Data/Output/fires_Eastern_temperate_forest.xlsx') %>% 
              group_by(NA_L3NAME) %>% 
              mutate(n_fires =n_distinct(Event_ID))) %>% 
  mutate(date = date(date),
         fire_date = date(fire_date),
         diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
  #filter(n_fires >=85) %>% 
     filter(NA_L3NAME %in% c("Central Appalachians",
                           "Middle Atlantic Coastal Plain",
                           "Ouachita Mountains",
                           "Ozark Highlands",
                           "Ridge and Valley",
                           "South Central Plains",
                           "Southeastern Plains",
                           "Southern Coastal Plain")) %>%
  na.omit() %>% 
  bind_rows(drought %>% #add L1
              mutate(date = date(date),
                     fire_date = date(fire_date),
                     diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
              mutate(NA_L3NAME = "L1 Eastern Temperate Forests",
                     US_L3CODE = "0") %>% 
              na.omit()) %>% 
   mutate(NA_L3NAME = factor(NA_L3NAME,
                             levels =c("L1 Eastern Temperate Forests",
                                       "Central Appalachians",
                                       "Middle Atlantic Coastal Plain",
                                       "Ouachita Mountains",
                                       "Ozark Highlands",
                                       "Ridge and Valley",
                                       "South Central Plains",
                                       "Southeastern Plains",
                                       "Southern Coastal Plain"))) %>% 
  filter(diff_days <= 100 & diff_days >=-100) %>% 
  mutate(PDSI_zone = case_when(
    pdsi <= -4 ~ "Extreme Drought",
    pdsi < 0 & pdsi > -4 ~ "Drought",
    pdsi >= 0 ~ " wet"
  ))


unique(drought2$NA_L3NAME)
####Geom_smoothed plots####
# Define the colors for each zone
# Define the colors for each relevant zone
zone_colors <- c("darkred",
                 "red",
                 "orange",
                 "yellow",
                 "lemonchiffon2", 
                 "white",
                 "lightblue",
                "deepskyblue",
                 "dodgerblue3",
                "navy")
#continouous palette
zone_colors <- c("darkred", "yellow", "white", "deepskyblue", "navy")

# Create a continuous palette function
zone_palette <- colorRampPalette(zone_colors)
# Generate, say, 100 gradient colors
gradient_colors <- zone_palette(100)
# Optional: visualize the palette
scales::show_col(gradient_colors)




# Define the y-axis ranges for each relevant zone
zone_ranges1 <- data.frame(
  ymin = c(-Inf,-1.99,-1.59, -1.29, -0.79, -0.49, 0.5, 0.8,1.3,1.6),
  ymax = c(-2.0,-1.6,-1.3, -0.8, -0.5, 0.49, 0.79, 1.29,1.59,Inf),
  #color = zone_colors,
  key = factor(c("Extreme Drought","Severe Drought","Moderate Drought","Mild Drought","Incipient Dry Spell", 
                 "Near Normal","Incipient Wet Spell","Slightly Wet","Moderately Wet","Very to extremely Wet"),
          levels = c("Extreme Drought","Severe Drought","Moderate Drought","Mild Drought",
                     "Incipient Dry Spell", "Near Normal",
                     "Incipient Wet Spell","Slightly Wet","Moderately Wet","Very to extremely Wet")))
zone_ranges <- zone_ranges1[rev(rownames(zone_ranges1)), ]
zone_ranges$key <- factor(zone_ranges$key, levels = zone_ranges$key)




# 
# #SPEI 14 days plot
# spei14<-ggplot(data = drought2,
#        aes(x = diff_days, y = spei14d))+
#   theme_minimal()+
#   theme_bw(10)+
#   labs(x = "Days till/since fire",
#        y = "14 Day SPEI")+
#   facet_wrap(~PDSI_zone) + 
#   #facet_wrap(~NA_L3NAME, 
#    #          labeller = label_wrap_gen(width = 25))+
# #format the facet labels
#   theme(strip.text = element_text(size = 8),
#         strip.background =element_rect(fill="white"),
#         panel.grid.minor = element_blank()
#   )+
#   geom_rect(data = zone_ranges,
#             aes(ymin = ymin, 
#                 ymax = ymax, 
#                 xmin = -Inf, 
#                 xmax = Inf, 
#                 fill = key),
# 
#             alpha = 0.3, inherit.aes = FALSE) +
# #change the colors for the blocks
#   #scale_fill_manual(values = setNames(zone_ranges$color, zone_ranges$key)) + 
# 
# #ditch the legend for the colors
#   #guides(fill = FALSE,alpha = FALSE)+
# #add a vertical line at the day of fire.
#   geom_vline(xintercept = 0, color = "grey70")+
#   geom_quantile(method = "rqss",
#                 aes(color = factor(..quantile..)),
#                 quantiles = c(0.1,0.5,0.9),
#                 lambda = 0.01, size = 0.5)+
# #change the aesthetics of the lines
#   scale_color_manual(values = c("0.1" = "black", "0.5" = "darkblue", "0.9" = "grey40"),
#                      labels = c("0.1 Quantile", "0.5 Quantile", "0.9 Quantile"),
#                      name = "Quantiles") +  
#   guides(fill = guide_legend(title = "SPEI Range"))
# 
# 
# 
# ggsave(file = 'Output/Graphs and Figures/spei14d_smoothed.jpg',
#        height =4,
#        width =7,
#        dpi = 500)



# Background fill data
bg <- data.frame(
  y = seq(-2.2, 2.2, by = 0.01),   # full y-range you want to cover
  x = 0,
  fill_value = seq(-2.2, 2.2, by = 0.01)
)

# Expand x so tiles are wide enough to cover the panel
bg$xmin <- -100
bg$xmax <- 100

p_load(ggfx)
# SPEI 14 days plot with continuous background
contin<-ggplot(data = drought2,
       aes(x = diff_days, y = spei14d)) +
  theme_minimal() +
  theme_bw(10) +
  ylim(-2,2)+
  labs(x = "Days till/since fire",
       y = "14 Day SPEI") +
  scale_x_continuous(expand = c(0, 0))+
  facet_wrap(~NA_L3NAME + PDSI_zone,
             labeller = label_wrap_gen(width = 25)) +
  #facet_wrap(~PDSI_zone)+
  theme(strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  # Continuous vertical background
  geom_tile(data = bg,
            aes(x = x, y = y, fill = fill_value,
                width = xmax - xmin, height = 0.01),
            inherit.aes = FALSE,
            alpha = 0.7) +
  
  # Color scale approximating your 10-level discrete palette
  scale_fill_gradientn(
    colours = zone_colors,
    limits = c(-2.2, 2.2),  # or wider range
    breaks = c(-2, 0, 2),
    labels = c("<-2 Extreme Drought", "0 Near Normal", ">2 Very Wet"),
    name = "SPEI Range"
  ) +
  
  geom_vline(xintercept = 0, color = "grey70") +
  
  with_outer_glow(geom_quantile(method = "rqss",
                aes(color = factor(..quantile..)),
                quantiles = c(0.1, 0.5, 0.9),
                lambda = 0.01, size = 0.3),
                color = "white",
                sigma = 10) +
  
  scale_color_manual(values = c("0.1" = "black", "0.5" = "darkblue", "0.9" = "grey40"),
                     labels = c("0.1 Quantile", "0.5 Quantile", "0.9 Quantile"),
                     name = "Quantiles")+
  guides(color = guide_legend(reverse = TRUE))
  
ragg::agg_png("Output/Graphs and Figures/spei14continuous.png", 
              width = 7, height = 4, units = "in", 
              res = 300, scaling = 0.8)
contin
dev.off()
####working here####





# temperature<-read_csv("Data/ETF_fireweather_for_defense.csv") %>% 
#   mutate(tmmx_celsius = tmmx-273.15,
#          tmmn_celsius = tmmn - 273.15,
#          date = date(date),
#         fire_date = date(fire_date),
#         diff_days = as.numeric(difftime(date, fire_date, units = "days"))) %>% 
#   left_join(codes)
# 
# ggplot(data = temperature,
#        aes(x = diff_days, y = tmmn_celsius))+
#   theme_minimal()+
#   # geom_rect(data = zone_ranges,
#   #           aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, 
#   #               fill = key),
#   #           alpha = 0.3, inherit.aes = FALSE) +
#   facet_wrap(~NA_L3NAME, 
#              #scales = "free_y",
#              labeller = label_wrap_gen(width = 25))+
#   geom_vline(xintercept = 0, color = "grey70")+
#   # geom_hline(yintercept = 0, color = "grey70")+
#   geom_quantile(method = "rqss",
#                 aes(color = factor(..quantile..)),
#                 quantiles = c(0.1,0.5,0.9),
#                 lambda = 0.001, size = 0.5)+
#   guides(fill = FALSE,alpha = FALSE)+
#   
#   theme_bw(10)+
#   labs(x = "Days till/since fire",
#        y = "temperature")+
#   theme(strip.text = element_text(size = 8),
#         strip.background =element_rect(fill="white"),
#         panel.grid.minor = element_blank()
#   )#+
#   # scale_fill_manual(values = setNames(zone_ranges$color, zone_ranges$key)) +
#   # scale_color_manual(values = c("0.1" = "black", "0.5" = "darkblue", "0.9" = "grey40"),
#   #                    labels = c("0.1 Quantile", "0.5 Quantile", "0.9 Quantile"),
#   #                    name = "Quantiles") +
#   # guides(fill = guide_legend(title = "SPEI Range"))
# 








# 
# #PDSI plot
# zone_colors <- c("darkred","red", "orange", "yellow","lemonchiffon2", "white","lightblue", "deepskyblue","dodgerblue3","navy")
# 
# 
# 
# pdsi_ranges <- data.frame(
#   ymin = c(4.0,3.0,2.0,1.0, -0.99, -1.99, -2.99, -3.99, -4.99,-Inf),
#   ymax = c(Inf,3.99,2.99,1.99, 0.99, -1.00, -1.99, -2.99, -4.00,-5),
#   color = c("navy","dodgerblue3","deepskyblue","lightblue", "white","lemonchiffon2","yellow", "orange", "red", "darkred"),
#   key = factor(c("Very to Extremely Wet","Moderately Wet","Slightly Wet", "Incipient Wet Spell", "Near Normal", "Incipient Dry Spell", "Mild Drought", "Moderate Drought", "Severe Drought","Extreme Drought"),
#                levels = c("Very to Extremely Wet","Moderately Wet","Slightly Wet","Incipient Wet Spell", "Near Normal", "Incipient Dry Spell", "Mild Drought", "Moderate Drought", "Severe Drought","Extreme Drought"))
# )
# 
# pdsi_ranges
# 
# pdsi<-ggplot(data = drought2,
#        aes(x = diff_days, y = pdsi))+
#   facet_wrap(~NA_L3NAME, 
#              #scales = "free_y",
#              labeller = label_wrap_gen(width = 25))+
#   geom_rect(data = pdsi_ranges,
#             aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, 
#                 fill = key),
#             alpha = 0.3, inherit.aes = FALSE) +
# 
#   geom_quantile(method = "rqss",
#                 aes(color = factor(..quantile..)),
#                 quantiles = c(0.1,0.5,0.9),
#                 lambda = 0.1, size = 0.5)+
#   guides(fill = FALSE,alpha = FALSE)+
#   geom_vline(xintercept = 0, color = "grey70")+
#   # geom_hline(yintercept = 0, color = "grey70")+
#   theme_bw(10)+
#   labs(x = "Days till/since fire",
#        y = "PDSI")+
#   theme(strip.text = element_text(size = 8),
#         strip.background =element_rect(fill="white"),
#         panel.grid.minor = element_blank()
#   )+
#   scale_color_manual(values = c("0.1" = "black", "0.5" = "darkblue", "0.9" = "grey40"),
#                      labels = c("0.1 Quantile", "0.5 Quantile", "0.9 Quantile"),
#                      name = "Quantiles") +
#   scale_fill_manual(values = setNames(pdsi_ranges$color, pdsi_ranges$key)) +
#   guides(fill = guide_legend(title = "PDSI Range"))
# 
# 
# ggsave(file = 'Output/Graphs and Figures/pdsi_smoothed.jpg',
#        height =4,
#        width =7,
#        dpi = 500)
# 
# list= list(pdsi,spei14)
# wrap_plots(list,ncol = 1,
#            axis_titles = "collect_x")
# ggsave("Output/Graphs and Figures/Figure3_daystillfirelambda01.jpg", 
# width = 7, height = 8, units = "in")
# 
# 





