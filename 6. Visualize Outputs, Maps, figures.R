
#Load packages
library(pacman)
pacman::p_load(readxl,tidyverse,sf,ggrepel,
               terra,tidyterra,cowplot,RColorBrewer,
               classInt,ggspatial)



####Import Data and Wrangle####
East_temp<-st_read("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Pre-process Spatial Data/Output/us_l3_ecoregions_ETF_UNsimplifiedgeometry.shp") %>% 
  group_by(NA_L3NAME) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")


plot(East_temp) 


#bring in Odds Ratio Data. 
odds<- read_csv("Output/L3_models_output_redoneAGAIN.csv") %>% 
  arrange(NA_L3NAME)%>%
  select(NA_L3NAME,Term,OR,p_value) %>% 
  mutate(OR = round(OR, 3)) %>% 
  filter(Term != "(Intercept)") %>% 
  mutate(Term = case_when(Term == "pdsi.5.1.window_mean" ~ "pdsi",
                           Term == "spei.5.1.window_mean" ~ "spei",
                           Term == "vpd_1.1.window_mean" ~ "vpd",
                           Term == "vs_1.1.window_mean" ~ "vs",
                          TRUE ~ Term
                           
))



####PDSI####
justsig %>% 
   filter(Term == "pdsi")
 


breakpoints_pdsi<-c( 
                    0.75,0.85,0.9)
breaks_pdsi <-c(
                   "0.75-0.8",
                 ">0.85") 
sig_pdsi<-justsig %>% 
  filter(Term =="pdsi") %>% 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_pdsi, labels = breaks_pdsi))
####Percent woody ####
justsig %>% 
  filter(Term == "percent_woody") %>% 
  arrange(OR)
 
breakpoints_woody <-c(0.9,1,1.05)
breaks_woody <-c("<1.00",
                  "1.00-1.05") 
sig_woody<-justsig %>% 
  filter(Term =="percent_woody") %>% 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_woody, labels = breaks_woody)) 
  
####14 Day SPEI####
justsig %>% 
  filter(Term == "spei") %>% 
  arrange(OR)


breakpoints_spei<-c(0.2,0.3,0.4,0.5,0.8)
breaks_spei <-c("0.2 - 0.3",
               "0.3 - 0.4",
               "0.4 - 0.5",
               ">0.5") 
sig_spei<- justsig %>% 
  filter(Term =="spei") %>% 
  arrange(OR) %>% 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_spei, labels = breaks_spei))

 
####VPD####

justsig %>% 
  filter(Term == "vpd") %>% 
  arrange(OR)


breakpoints_vpd<-c(3,
                   5,
                   6,
                   15)
breaks_vpd <-c("3 - 4",
               "5 - 6.0",
               ">10")
sig_vpd<-  justsig %>% 
  filter(Term =="vpd") %>% 
  arrange(OR) %>% 
 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_vpd, labels = breaks_vpd))
####Year ####
 justsig %>% 
  filter(Term == "year")

breakpoints_year<-c(0.8,1,1.3)
breaks_year <-c("<1",
                ">1"
)
sig_year<-justsig %>% 
  filter(Term =="year") %>% 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_year, labels = breaks_year)) %>% 
  mutate()

####VS#####
justsig %>% 
  filter(Term == "vs") %>% 
  arrange(OR)

breakpoints_vs<-c(1.1,1.4,1.5,1.6,1.7,3)
breaks_vs <-c("1.2 - 1.3",
              "1.4 - 1.5",
              "1.5 - 1.6",
              "1.6 - 1.7",
              "1.7 - 2.0") 
sig_vs<-justsig %>% 
  filter(Term == "vs") %>% 
  arrange(OR) %>% 
  mutate(Bins = cut(OR, include.lowest=TRUE, breaks = breakpoints_vs, labels = breaks_vs))
 


justsig<-bind_rows(sig_vs,sig_year,sig_vpd,sig_woody,sig_pdsi,sig_spei)
rm(sig_vs,sig_year,sig_vpd,sig_woody,sig_pdsi,sig_spei,
   list = ls(pattern = "break"))

odds_for_map<-rbind(justsig,notsig) %>% 
  mutate(Bins = as.character(Bins)) %>% 
  select(-p_value)

#Add NAs for the excluded regions. 

excluded_regions <- East_temp$NA_L3NAME[!(East_temp$NA_L3NAME %in% odds$NA_L3NAME)] %>% 
  rep(6)
empty_terms_column<- odds %>% 
  filter(Term != "percent_woody:vpd_1.1.window_mean") %>% 
  filter(Term != "percent_woody:pdsi.5.1.window_mean") %>% 
  pull(Term) %>% 
  unique() %>% 
  rep(22) 
 
excluded_regions <-as.data.frame(excluded_regions) %>%
  mutate(OR = NA,
         Bins = "Excluded")%>%
  rename(NA_L3NAME = excluded_regions) %>% 
  select(NA_L3NAME,OR,Bins) %>% 
  arrange(NA_L3NAME) %>% 
  mutate(Term = empty_terms_column)

odds_map_joined <-rbind(odds_for_map,excluded_regions) %>% 
  mutate(Bins = case_when(NA_L3NAME== "Middle Atlantic Coastal Plain" & Term %in% c("percent_woody","vpd") ~ "Involved in Interaction",
                          NA_L3NAME == "Ouachita Mountains" & Term %in% c("percent_woody","pdsi")~ "Involved in Interaction",
                          TRUE ~ Bins))


oddsmap_sf<-East_temp %>% 
  left_join(odds_map_joined) 

####Create palettes####


palette.red.2<-c("lightblue","#FEE0D2") #this one has negative effects and positive effects. 
palette.red.3<-brewer.pal(3,"Reds") 
c("#FEE0D2", "#FC9272", "#DE2D26")
palette.red.4<-brewer.pal(4,"Reds")
c("#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D")
palette.red.5<-brewer.pal(5,"Reds")
c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15")

palette.blue.4<-brewer.pal(4,"Blues")
c("#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5")

palette.blue.2<-c("#BDD7E7", "#2171B5")
####Plot Odds Ratio####

#I want to plot each term separately and then patch them all together. 

####Year first####
plot.year<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
                 filter(Term =="year") %>% 
            mutate(Bins = factor(Bins, levels = c("<1",">1",
                                                  "Insignificant",
                                                  "Excluded"))),
               aes(fill = Bins),
               size = 1.5, color = "black") + 
         scale_fill_manual(name = "Odds ratio of wildfire \n relative to year",
                           values = c(palette.red.2,'darkgray',"white"), #got dark grey and white for insignificant and excluded. 
                           labels = c("<1",
                                      ">1",
                                      "Insignificant",
                                      "Excluded")) +
         theme_void()+
         coord_sf()+
         
         theme(axis.title = element_blank(),
               #axis.text = element_text(size = 6),
               axis.text =element_blank(),
               panel.background = element_blank(),
               panel.grid = element_blank(),
               legend.spacing.y = unit(0.3, 'cm'),
               legend.position = "right",
               legend.title = element_text(hjust = 0.5, size = 10),
               legend.text = element_text(size = 10),
               legend.key.width = unit(0.5, "cm"), 
               legend.key.height = unit(0.5, "cm"),
               legend.background = element_blank(),
               plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  # plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))+  # top, right, bottom, left)
         guides(fill = guide_legend(ncol = 1)) +   
        labs(tag = "A") + 
        theme(plot.tag = element_text(face = "bold", size = 15),
              plot.tag.position = c(0.04,0.93))        
  
odds_map_joined %>% 
  filter(Term =="vpd") %>% 
  select(Bins) %>% 
  unique()

####Plot VPD####
plot.vpd<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
            filter(Term =="vpd") %>% 
            mutate(Bins = factor(Bins,
                                    levels = c(   
                                    "3 - 4",   
                                    #"4.5-6",
                                    "5 - 6.0",    
                                    ">10", 
                                    "Involved in Interaction",
                                    "Insignificant",
                                    "Excluded"))),
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  scale_fill_manual(name = "Odds ratio of wildfire relative to \n  Vapor Pressure Deficit (kPa)",
                    values = c(palette.red.4[1], palette.red.4[2], palette.red.4[3]
                              , "lightgrey", "darkgray", "white"))+
 
  theme_void()+
  coord_sf()+
  
  theme(axis.title = element_blank(),

        axis.text =element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.5, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+

  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "B") + 
  theme(plot.tag = element_text(face = "bold", size = 15),
        plot.tag.position = c(0.04,0.93))   



####Plot VS####
odds_map_joined %>% 
  filter(Term =="vs") %>% 
  select(Bins) %>% 
  unique()

plot.vs<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
            filter(Term =="vs") %>% 
            mutate(Bins = factor(Bins,
                                 levels = c("1.2 - 1.3",      
                                            "1.4 - 1.5",   
                                            "1.5 - 1.6",
                                            "1.6 - 1.7",    
                                            "1.7 - 2.0",       
                                            "Insignificant",
                                            "Excluded"))),
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  
   scale_fill_manual(name = "Odds ratio of wildfire \n relative to Wind Speed (m/s)",
                     values =  c(palette.red.5,'darkgray',"white"),
                       labels = c("1.2 - 1.3",      
                                "1.4 - 1.5",   
                                "1.5 - 1.6",
                                "1.6 - 1.7",    
                                "1.7 - 2.0",
                                "Insignificant",
                                "Excluded")) +
  theme_void()+
  coord_sf()+
  
  theme(axis.title = element_blank(),

        axis.text =element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.5, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
 
  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "C") + 
  theme(plot.tag = element_text(face = "bold", size = 15),
        plot.tag.position = c(0.04,0.93)) 




####Plot Woody Cover ####
odds_map_joined %>% 
  filter(Term =="percent_woody") %>% 
  select(Bins) %>% 
  unique()

plot.woody<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
            filter(Term =="percent_woody") %>% 
            mutate(Bins = factor(Bins,
                                 levels = c("<1.00", 
                                            "1.00-1.05",
                                            "Involved in Interaction",
                                            "Insignificant",
                                            "Excluded"))),
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  scale_fill_manual(name = "Odds ratio of wildfire \n relative to Woody Cover (%)",
                    values =  c(palette.red.2,"lightgrey",'darkgray',"white"),
                    labels = c("<1.0", 
                               "1.00-1.05",
                               "Involved in Interaction",
                               "Insignificant",
                               "Excluded")) +
  theme_void()+
  coord_sf()+
  
  theme(axis.title = element_blank(),
        #axis.text = element_text(size = 6),
        axis.text =element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.5, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  # plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))+  # top, right, bottom, left)
  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "D") + 
  theme(plot.tag = element_text(face = "bold", size = 15),
        plot.tag.position = c(0.04,0.93))    



####Plot PDSI####

odds_map_joined %>% 
  filter(Term =="pdsi") %>% 
  select(Bins) %>% 
  unique()

plot.pdsi<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
            filter(Term =="pdsi") %>% 
            mutate(Bins = factor(Bins,
                                 levels = c(">0.85",
                                            "0.75-0.8",
                                            "Involved in Interaction",
                                            "Insignificant",
                                            "Excluded"))),
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  scale_fill_manual(name = "Odds ratio of wildfire \n relative to PDSI",
                    values =  c(palette.blue.2,"lightgrey",'darkgray',"white"),
                    labels = c(">0.85",
                               "0.75-0.8",
                               "Involved in Interaction",
                               "Insignificant",
                               "Excluded")) +
  theme_void()+
  coord_sf()+
  
  theme(axis.title = element_blank(),
        #axis.text = element_text(size = 6),
        axis.text =element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.5, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
       # plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))+  # top, right, bottom, left)
  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "E") + 
  theme(plot.tag = element_text(face = "bold", size = 15),
        plot.tag.position = c(0.04,0.93)) 

####Plot SPEI####
odds_map_joined %>% 
  filter(Term =="spei") %>% 
  select(Bins) %>% 
  unique()

plot.spei<-ggplot()+
  geom_sf(data = oddsmap_sf %>% 
            filter(Term =="spei") %>% 
            mutate(Bins = factor(Bins,
                                 levels = c(">0.5",
                                            "0.4 - 0.5",
                                            "0.3 - 0.4",
                                            "0.2 - 0.3",
                                            "Excluded"))),
          aes(fill = Bins),
          size = 1.5, color = "black") + 
  scale_fill_manual(name = "Odds ratio of wildfire \n relative to 14 Day SPEI",
                    values =  c(palette.blue.4,"white"),
                    labels = c(">0.5",
                               "0.4 - 0.5",
                               "0.3 - 0.4",
                               "0.2 - 0.3",
                               "Excluded")) +
  theme_void()+
  coord_sf()+
  
  theme(axis.title = element_blank(),
        #axis.text = element_text(size = 6),
        axis.text =element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.position = "right",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(0, 0, 0, -20),
        legend.key.width = unit(0.5, "cm"), 
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))+
        #plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))+  # top, right, bottom, left)
  guides(fill = guide_legend(ncol = 1)) +   
  labs(tag = "F") + 
  theme(plot.tag = element_text(face = "bold", size = 15),
        plot.tag.position = c(0.04,0.93)) 



####Patch them all together####
patched.plot<- plot_grid(plot.year,
                         plot.vpd,
                         plot.vs,
                         plot.woody,
                         plot.pdsi,
                         plot.spei,
                                  nrow = 3,
                                  align = 'v')
ragg::agg_png("C:/Users/micha/OneDrive - University of Florida/Chapter 2 Data Analysis/Mixed Models Revision/Output/figures/Oddsratio_maps.png",
              height =9,
              width =9 , 
              units = "in",
              scaling = 1,
              res = 300)
patched.plot
dev.off()










