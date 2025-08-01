####Load Packages####
library(pacman)
pacman::p_load(jtools,tidyverse,readxl,
             prettyglm,ragg,questionr,arm, performance)
####Load initial data####
#L1_data<-read_excel('Output/Data/Presence_Absence_L1_EXTRAINDICES.xlsx')
L1_data<-read_excel("Output/Data/Presence_Absence_L1.xlsx")
L3_data<-read_excel('Output/Data/L3_Presence_Absence_Eight_Regions.xlsx')


####L1 Eastern Temperate Forests####
model_L1= glm(PA ~ vs+vpd+
              + pdsi*percent_cover 
             + spei14*percent_cover, 
           data = L1_data,
           family = binomial(link = "logit"))
summary(model_L1)
#PDSI insignificant

cor(L1_data$vpd_10.5.window_mean,L1_data$fm100.1.1.window_mean)
library(corrplot)
corrplot(cor(L1_data %>% 
           dplyr::select(c("pdsi","spei14",vs_10.1.window_mean,vpd_10.5.window_mean,
                           fm100.1.1.window_mean,fm1000.10.1.window_mean))),
         method = "number")


ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/L1 Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_L1, plot = TRUE)
dev.off()

summary(model_L1)

ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/L1 Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_L1$fitted.values ,model_L1$residuals, nclass=NULL, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot", 
           cex.pts=0.8, col.pts=1, col.int="gray")

dev.off()

####Central Appalachians####
model_CA=glm(PA ~ vs+ 
               vpd+ 
               pdsi +#*percent_cover + 
               spei14 ,#*percent_cover, 
             data = L3_data %>% 
               filter(NA_L3NAME == "Central Appalachians"),
             family = binomial(link = "logit"))
summary(model_CA)
#neither significant

####Diagnostic Plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/CA Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_CA, plot = TRUE)
dev.off()



####Binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/CentralAppalachians.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_CA$fitted.values ,model_CA$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Central Appalachians", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

####Middle Atlantic Coastal Plain####
 model_MA = glm(PA ~vs+ vpd+
                  pdsi*percent_cover + 
                  spei14*percent_cover, 
                               data = L3_data %>% 
                   filter(NA_L3NAME == "Middle Atlantic Coastal Plain"),
                               family = binomial(link = "logit"))
summary(model_MA)
#neither significant
####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/MA Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_MA, plot = TRUE)
dev.off()


####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Middle Atlantic.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_MA$fitted.values ,model_MA$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Middle Atlantic Coastal Plain", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

####Ouachita Mountains ####
model_OM =glm(PA ~ vs+ vpd + 
                pdsi*percent_cover + 
                spei14*percent_cover, 
              data  = L3_data %>% 
                filter(NA_L3NAME == "Ouachita Mountains",
                       Event_ID !=2459), #eliminate super wierd outlier
              family = binomial(link = "logit"))

summary(model_OM)

####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/OM Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_OM, plot = TRUE)
dev.off()


####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Ouachita.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_OM$fitted.values ,model_OM$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Ouachita Mountains", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

####Ozark Highlands####
model_OH = glm(PA ~ vs+ vpd +
                 pdsi*percent_cover + 
                 spei14*percent_cover, 
               data = L3_data %>% 
                 filter(NA_L3NAME == "Ozark Highlands"),
               family = binomial(link = "logit"))
summary(model_OH)
####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/OH Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_OH, plot = TRUE)
dev.off()

####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Ozark Highlands.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_OH$fitted.values ,model_OH$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Ozark Highlands", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()


####Ridge and Valley
data_RV<- L3_data %>% 
  filter(NA_L3NAME == "Ridge and Valley")
model_RV<-glm(formula = PA ~ vs+ vpd +
                pdsi*percent_cover + 
                spei14*percent_cover, 
              family = binomial(link = "logit"), 
              data = L3_data %>% 
                filter(NA_L3NAME == "Ridge and Valley"))
summary(model_RV)



####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/RV Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_RV, plot = TRUE)
dev.off()

####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Ridge and Valey.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_RV$fitted.values ,model_RV$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot RV", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

####South Central Plains####
model_SCP<- glm(formula = PA ~ vs+ vpd+ 
                  pdsi*percent_cover + 
                  spei14*percent_cover, 
                 family = binomial(link = "logit"),
                 data = L3_data %>% 
                   filter(NA_L3NAME == "South Central Plains"))
summary(model_SCP)
####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/SCP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SCP, plot = TRUE)
dev.off()

####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/South Central.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_SCeP$fitted.values ,model_SCeP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot South Central Plains", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()


####Southeastern Plains####
model_SP<-glm(formula = PA ~ vs+ vpd + pdsi*percent_cover + spei14*percent_cover, 
              family = binomial(link = "logit"),
              data = L3_data %>% 
                filter(NA_L3NAME == "Southeastern Plains"))
summary(model_SP)
####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/SP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SP, plot = TRUE)
dev.off()

####binned residuals####
summary(model_SP)
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Southeastern Plains.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_SP$fitted.values ,model_SP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Southeastern Plains", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()


####Southern Coastal Plain####
model_SCoP<-glm(formula = PA ~ vs+ vpd+
                  pdsi*percent_cover + 
                  spei14*percent_cover, 
                family = binomial(link = "logit"),
                data = L3_data %>% 
                  filter(NA_L3NAME == "Southern Coastal Plain"))


summary(model_SCoP)


library(MuMIn)
AICc(model_SCoP,model_SCoP2,model_SCoP3)

####diagnostic plots####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/check_model/SCoP Eastern.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)

check_model(model_SCoP, plot = TRUE)
dev.off()

####binned residuals####
ragg::agg_png(file = "Output/Graphs and Figures/Diagnostic Plots/Global Model Diagnostics/binnedresiduals/Southern Coastal.png", 
              width = 4, height = 4, units = "in", 
              res = 300, scaling = 0.4)
binnedplot(model_SCoP$fitted.values ,model_SCoP$residuals, 
           xlab="Expected Values", ylab="Average residual", 
           main="Binned residual plot Southern Coastal Plain", 
           cex.pts=0.8, col.pts=1, col.int="gray")
dev.off()

