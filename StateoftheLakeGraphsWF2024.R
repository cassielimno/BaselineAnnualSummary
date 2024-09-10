#making summary stats for baseline 2023 report
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(grid) 
library(tayloRswift)
library(cowplot)
library(patchwork)
detach(package:plyr)
library("plotrix")

#if you are going to use any DO graphs make sure data and se is correct ##### 

#load theme
mlc_theme <- theme(
  axis.title.x=element_text(size=14, face="bold", colour = "black"),
  axis.title.y=element_text(size=14, face="bold", colour = "black"),
  axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1, colour = "black"),
  axis.text.y = element_text(size=12, face="bold", colour = "black"),
  legend.text = element_text(colour="black", size = 11, face = "bold"),
  legend.title = element_text(colour="black", size=11, face="bold"),
  legend.position= "right", 
  axis.line.x = element_line(color="black", linewidth  = 0.3),
  axis.line.y = element_line(color="black", linewidth  = 0.3),
  panel.border = element_rect(colour = "black", fill=NA, size=0.3),
  title = element_text(size = 12, face = "bold"),
  panel.background = element_blank(),
  panel.grid.major = element_line(color="grey", linewidth  = 0.3), 
  panel.grid.minor = element_blank())
#THIS IS THE COMPLETED DATA for 2023
data<- read.csv("HydroShareFinalALL2023.csv")

wf<- data %>% filter(Station_ID == "WF-LK-IP1" | Station_ID == "WF-LK-IP2")


glimpse(wf)

#turn date to date


wf$Activity_Start_Date<- as.POSIXct(wf$Activity_Start_Date)



#make year and month cols
wf<-wf %>% mutate(year = year(Activity_Start_Date), 
                        month = month(Activity_Start_Date), day = yday(Activity_Start_Date))





#make mean for each year and max and min
wfsummer<- wf  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

glimpse(wfsummer)

wfsummer$year<- as.character(wfsummer$year)

#cut this data to just bottom for ip1 oxygen 
deepdo<- wfsummer %>% filter(Result_Depth_Height_Measure > 50, Station_ID == "WF-LK-IP1", Characteristic_ID == "DO-SAT") %>% 
  group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value), n = length(Result_Value))

mean(deepdo$Result_Value)


#find mean for individual things #####
secchimeanip1<-wfsummer %>% filter(Characteristic_ID == "DEPTH-SECCHI", Station_ID == "WF-LK-IP1")
mean(secchimeanip1$Result_Value)

chlmeanip1<- wfsummer %>% filter(Characteristic_ID == "CHL-A-CP", Station_ID == "WF-LK-IP1")
mean(chlmeanip1$Result_Value, na.rm = TRUE)

tnmeanip1<- wfsummer %>% filter(Characteristic_ID == "TN", Station_ID == "WF-LK-IP1")
mean(tnmeanip1$Result_Value, na.rm = TRUE)

tpmeanip1<- wfsummer %>% filter(Characteristic_ID == "TP", Station_ID == "WF-LK-IP1")
mean(tpmeanip1$Result_Value, na.rm = TRUE)



#make summmer secchi depth graph thats just like flbs one ####
ggplot(data = wfsummer %>% filter(Characteristic_ID == "DEPTH-SECCHI", Station_ID == "WF-LK-IP1"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 23.28, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 40)+
  mlc_theme+
  ylab("Secchi Depth ft, (+/- s.e.)")+
  xlab("Year")

summary(lm(Result_Value~Activity_Start_Date, data = wfsummer %>% filter(Characteristic_ID == "DEPTH-SECCHI", Station_ID == "WF-LK-IP1")))

#in meters
ggplot(data = wfsummer %>% filter(Characteristic_ID == "DEPTH-SECCHI", Station_ID == "WF-LK-IP1"),
       aes(x = year, y = mean*0.3048))+
  geom_point()+
  geom_errorbar(aes(ymin = mean*0.3048-se*0.3048, ymax = mean*0.3048+se*0.3048), width = 0.3)+
  #scale_x_date(date_breaks = "year", date_labels = "%Y")+
  ylim(0,20)+
  xlab("Year")+
  mlc_theme+
  ylab("Secchi Depth m, (+/- s.e.)")



#same graph for summer chl-a ####
tiff("Tripfdom.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummer %>% filter(Characteristic_ID == "CHL-A-CP", Station_ID == "WF-LK-IP1"),
       aes(x = year, y = mean))+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  geom_hline(yintercept = 1.78, linetype = "dashed", color = "black", alpha = 0.5)+
  #ylim(0, 40)+
  #scale_x_continuous(expand = c(0.1, 0.1), breaks = scales :: pretty_breaks(n = 10), limits = c(2014, 2024))+
  mlc_theme+
  #ylim(0,4, expand = c(0.1, 0.1))+
  xlab("Year")+
  scale_y_continuous(expand = c(0.15, 0.1))+
  ylab("Summer Chlorophyll, (ug/L, +/- s.e.)")

dev.off()


#DO graph
ggplot(data = deepdo,
       aes(x = year, y = mean))+
  geom_hline(yintercept = 77.88, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  #geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.4)+
  geom_linerange(aes(x = year, ymin = mean-se, ymax = mean+se))+
  mlc_theme+
 ylim(65,100)+
  ylab(bquote(bold("Summer"~O[2]~"(% Saturation, +/- s.e.)")))+
  xlab("Year")

summary(lm(mean~Activity_Start_Date, data = deepdo))

#nitrogen
ggplot(data = wfsummer %>% filter(Characteristic_ID == "TN", Station_ID == "WF-LK-IP1"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 89.61, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Nitrogen, (ug/L, +/- s.e.)")+
  xlab("Year")

#phosphorus
ggplot(data = wfsummer %>% filter(Characteristic_ID == "TP", Station_ID == "WF-LK-IP1"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 4.29, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")

#tn trend
summary(lm(mean~Activity_Start_Date, data = wfsummer %>% filter(Characteristic_ID == "TN", Station_ID == "WF-LK-IP1")))

domean<- wf %>% filter(Characteristic_ID == "DO-SAT") %>% mutate(depthround = round(Result_Depth_Height_Measure, digits = 0))
  
domean<- domean %>%  group_by(depthround, Station_ID, month) %>% mutate(meando = mean(Result_Value))



#make average graphs
ggplot()+
  geom_point(data = wf %>% filter(Characteristic_ID == "DO", 
                                  month == 10, year == 2023, Station_ID == "WF-LK-IP1"), 
             aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
             size = 3, shape = 23)+
  scale_fill_manual(name = "This Year", values = c("lawngreen"))+
  theme_bw()+
  mlc_theme

ggplot()+
  geom_point(data = domean %>% filter(month == 10, Station_ID == "WF-LK-IP1"),
             aes(x = meando, y = -1*depthround), size = 3, shape = 21, fill = "grey")+
  geom_point(data = wf %>% filter(Characteristic_ID == "DO-SAT", 
                                  month == 10, year == 2023, Station_ID == "WF-LK-IP1"), 
             aes(x= Result_Value, y = -1*Result_Depth_Height_Measure,), 
             size = 3, fill = "lawngreen", shape = 21)+
 theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"))+
  mlc_theme+
  ggtitle( "Whitefish Lake IP1 \nOctober Dissolved oxygen")+
  ylab("Depth (m)")+
  xlab("Dissolved Oxygen Saturation")



ggplot(data = wf %>% filter(Characteristic_ID == "DO-SAT", month == 10, year < 2023, Station_ID == "WF-LK-IP1"))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure), alpha = 0.5, size = 2.5, fill = "lightgrey", shape = 21)+
  theme_bw()+
  geom_point(data = wf %>% filter(Characteristic_ID == "DO-SAT", month == 10,  year == 2023, Station_ID == "WF-LK-IP1"), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
  ggtitle( "Whitefish Lake IP1 \nOctober Dissolved oxygen")+
  ylab("Depth (m)")+
  xlab("Dissolved Oxygen Saturation")+
  scale_y_continuous(expand = c(.01,.01))+
  scale_x_continuous(expand = c(.01,.01))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"))  


#whithout the background
ggplot(data = wf %>% filter(Characteristic_ID == "DO-SAT", month == 10, year < 2023, Station_ID == "WF-LK-IP1"))+
  geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure), alpha = 0.5, size = 2.5, fill = "grey", shape = 21)+
  theme_bw()+
  geom_point(data = wf %>% filter(Characteristic_ID == "DO-SAT", month == 10,  year == 2023, Station_ID == "WF-LK-IP1"), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
  ggtitle( "Whitefish Lake IP1 \nOctober Dissolved oxygen")+
  ylab("Depth (m)")+
  xlab("Dissolved Oxygen saturation")+
  scale_y_continuous(expand = c(.01,.01))+
  scale_x_continuous(expand = c(.01,.01))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold"))  


#make all graphs again for ip2 ####
#see if the trends change compared to site (I think they don't)
#make simple profile graphs with all background grey and averaged and this year in color
#make a mean 
#make temperature graphs ####
#make elevation graphs ####




#make all graphs with sites combined ####
#make mean for each year and max and min
unique(wf$Station_ID)
wfsummerboth<- wf  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

glimpse(wfsummerboth)


wfsummerboth$year<- as.character(wfsummerboth$year)

#cut this data to just bottom for ip1 oxygen 
#FIX THIS ####
deepdo<- wfsummerboth %>% filter(Result_Depth_Height_Measure > 50, Characteristic_ID == "DO-SAT") %>% 
  group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value), n = length(Result_Value))

mean(deepdo$Result_Value)


#find mean for individual things #####
secchimean<-wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")
mean(secchimean$Result_Value)

chlmean<- wfsummerboth %>% filter(Characteristic_ID == "CHL-A-CP")
mean(chlmean$Result_Value, na.rm = TRUE)

tnmean<- wfsummerboth %>% filter(Characteristic_ID == "TN")
mean(tnmean$Result_Value, na.rm = TRUE)

tpmean<- wfsummerboth %>% filter(Characteristic_ID == "TP")
mean(tpmean$Result_Value, na.rm = TRUE)

nnmean<- wfsummerboth %>% filter(Characteristic_ID == "NN")
mean(nnmean$Result_Value, na.rm = TRUE)

#make summmer secchi depth graph thats just like flbs one ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/Whitefish data and code/StateoftheLake")

tiff("Secchi2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 22.65, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 42)+
  ggtitle("Whitefish Secchi")+
  mlc_theme+
  ylab("Secchi Depth ft, (+/- s.e.)")+
  xlab("Year")

dev.off()


summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")))
wfsummerboth$year<-as.numeric(wfsummerboth$year)
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")))
#in meters
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI"),
       aes(x = year, y = mean*0.3048))+
  geom_point()+
  geom_errorbar(aes(ymin = mean*0.3048-se*0.3048, ymax = mean*0.3048+se*0.3048), width = 0.3)+
  #scale_x_date(date_breaks = "year", date_labels = "%Y")+
  ylim(0,20)+
  xlab("Year")+
  mlc_theme+
  ylab("Secchi Depth m, (+/- s.e.)")



#same graph for summer chl-a ####
tiff("chla2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "CHL-A-CP"),
       aes(x = year, y = mean))+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  geom_hline(yintercept = 1.56, linetype = "dashed", color = "black", alpha = 0.5)+
  #ylim(0, 40)+
  #scale_x_continuous(expand = c(0.1, 0.1), breaks = scales :: pretty_breaks(n = 10), limits = c(2014, 2024))+
  mlc_theme+
  #ylim(0,4, expand = c(0.1, 0.1))+
  xlab("Year")+
  scale_y_continuous(expand = c(0.15, 0.1))+
  ylab("Summer Chlorophyll, (ug/L, +/- s.e.)")+
  ggtitle("Whitefish CHL")


dev.off()

summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "CHL-A-CP")))
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "CHL-A-CP")))
#nitrogen
tiff("TN2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "TN"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 91.49, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Nitrogen, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TN")

dev.off()

#tn trend
summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "TN")))
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "TN")))
#phosphorus
tiff("TP2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "TP"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 4.21, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TP")

dev.off()

#tp trend
summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "TP")))
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "TP")))
#do nn as well
tiff("NN2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "NN"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 5.03, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Nitrate, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish NN")

dev.off()

#nn trend
summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "NN")))
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "NN")))

#make a top 5 meters temp graphs
 wfsummertemp <- wfsummerboth %>% filter(Result_Depth_Height_Measure < 5.5, Characteristic_ID == "TEMP-W") %>% 
          group_by(year) %>% mutate(meantemp = mean(Result_Value), setemp = std.error(Result_Value),
                                    ntemp = length(Result_Value))
 
 #graph for mean temp
 tiff("Temp2023.tiff", units="in", width=7, height=5, res=300)
 ggplot(data = wfsummertemp,
        aes(x = year, y = meantemp))+
   geom_hline(yintercept = 62.35, linetype = "dashed", color = "black", alpha = 0.5)+
   geom_point(size = 2.5)+
   geom_errorbar(aes(ymin = meantemp-setemp, ymax = meantemp+setemp), width = 0.3)+
   ylim(30, 68)+
   mlc_theme+
   ylab("Summer Surface Temperature, (F, +/- s.e.)")+
   xlab("Year")
 
 dev.off()
 
 mean(wfsummertemp$meantemp, na.rm = TRUE)
 
 summary(lm(Result_Value~Activity_Start_Date, data = wfsummertemp))
 summary(lm(meantemp~Activity_Start_Date, data = wfsummertemp))
 
 #summer bottom temp graphs ####
wfsummertemplow<- wfsummerboth %>% filter(Characteristic_ID == "TEMP-W") %>% 
  filter(case_when(Station_ID=="WF-LK-IP1" ~ Result_Depth_Height_Measure > 55, 
                   Station_ID == "WF-LK-IP2" ~ Result_Depth_Height_Measure > 15)) %>% 
   group_by(year) %>% 
   mutate(meanlow = mean(Result_Value), lowse = std.error(meanlow), nlow = length(meanlow))

 tiff("Templow2023.tiff", units="in", width=7, height=5, res=300)
 ggplot(data = wfsummertemplow,
        aes(x = year, y = meanlow))+
   geom_hline(yintercept = 41.62, linetype = "dashed", color = "black", alpha = 0.5)+
   geom_point(size = 2.5)+
   geom_errorbar(aes(ymin = meanlow-lowse, ymax = meanlow+lowse), width = 0.3)+
   ylim(30, 68)+
   mlc_theme+
   ylab("Summer Deep Lake Temperature, (F, +/- s.e.)")+
   xlab("Year")
 
 dev.off()
 
 tempmeanlow<- wfsummertemplow
 mean(wfsummertemplow$meanlow, na.rm = TRUE)

summary(lm(Result_Value~Activity_Start_Date, data = wfsummertemplow))
summary(lm(meanlow~year, data = wfsummertemplow))
 
 

#make correct data for deep do for both lakes combined
wfsummerdolow<- wfsummerboth %>% filter(Characteristic_ID == "DO-SAT") %>% 
  filter(case_when(Station_ID=="WF-LK-IP1" ~ Result_Depth_Height_Measure > 55, 
                   Station_ID == "WF-LK-IP2" ~ Result_Depth_Height_Measure > 15)) %>% 
  group_by(year) %>% 
  mutate(meanlow = mean(Result_Value), lowse = std.error(meanlow), nlow = length(meanlow))
 
 
 
 #graph for low do sat in both lakes
tiff("DOlow2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerdolow,
       aes(x = year, y = meanlow))+
  geom_hline(yintercept = 84.45, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = meanlow-lowse, ymax = meanlow+lowse), width = 0.3)+
  ylim(0, 100)+
  # mlc_theme+
 # theme(axis.text=element_text(size=1),
        # axis.title=element_text(size=1,face="bold"))+
  ylab("Summer Deep Lake Dissolved Oxygen Saturation, (%, +/- s.e.)")+
  xlab("Year")+
  theme(
    axis.title.x=element_text(size=10, face="bold", colour = "black"),
    axis.title.y=element_text(size=10, face="bold", colour = "black"),
    axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1, colour = "black"),
    axis.text.y = element_text(size=12, face="bold", colour = "black"),
    legend.text = element_text(colour="black", size = 11, face = "bold"),
    legend.title = element_text(colour="black", size=11, face="bold"),
    legend.position= "right", 
    axis.line.x = element_line(color="black", linewidth  = 0.3),
    axis.line.y = element_line(color="black", linewidth  = 0.3),
    panel.border = element_rect(colour = "black", fill=NA, size=0.3),
    title = element_text(size = 12, face = "bold"),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="grey", linewidth  = 0.3), 
    panel.grid.minor = element_blank())


dev.off()

tempmeanlow<- wfsummertemplow
mean(wfsummerdolow$meanlow, na.rm = TRUE)

summary(lm(Result_Value~Activity_Start_Date, data = wfsummerdolow))
summary(lm(meanlow~year, data = wfsummerdolow)) 
 

#make all the same graphs while making non-detects zeros or mid value  ####


 
wfsummerbothnd<- wf %>% filter(month == 6 | month == 7 | month == 8)

wfsummerbothnd<- wfsummerbothnd %>% 
  mutate(result_nd = case_when(Result_Detection_Condition == "Not Detected" ~ wfsummerbothnd$Method_Detection_Limit_Value,
                               Result_Detection_Condition == "" ~ wfsummerbothnd$Result_Value,
                               is.na(Result_Detection_Condition) ~ wfsummerbothnd$Result_Value)) 

wfsummerbothnd <- wfsummerbothnd %>% group_by(year, Characteristic_ID) %>% 
  mutate(mean = mean(result_nd, na.rm = TRUE), max = max(result_nd), min = min(result_nd),
         se = std.error(result_nd))

wfsummerbothnd$year<- as.character(wfsummerbothnd$year)
 
 
#lets try graphing TN TP  NN and CHL since those are the only ones with NDs
#see if trends are different than other graphs
#find mean for individual things #####
secchimeannd<-wfsummerbothnd %>% filter(Characteristic_ID == "DEPTH-SECCHI")
mean(secchimeannd$result_nd)

chlmeannd<- wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP")
mean(chlmeannd$result_nd, na.rm = TRUE)

tnmeannd<- wfsummerbothnd %>% filter(Characteristic_ID == "TN")
mean(tnmeannd$result_nd, na.rm = TRUE)

tpmeannd<- wfsummerbothnd %>% filter(Characteristic_ID == "TP")
mean(tpmeannd$result_nd, na.rm = TRUE)

nnmeannd<- wfsummerbothnd %>% filter(Characteristic_ID == "NN")
mean(nnmeannd$result_nd, na.rm = TRUE)

#same graph for summer chl-a ####
tiff("chlaND2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP"),
       aes(x = year, y = mean))+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  geom_hline(yintercept = 1.45, linetype = "dashed", color = "black", alpha = 0.5)+
  #ylim(0, 40)+
  #scale_x_continuous(expand = c(0.1, 0.1), breaks = scales :: pretty_breaks(n = 10), limits = c(2014, 2024))+
  mlc_theme+
  #ylim(0,4, expand = c(0.1, 0.1))+
  xlab("Year")+
  scale_y_continuous(expand = c(0.15, 0.1))+
  ylab("Summer Chlorophyll, (ug/L, +/- s.e.)")+
  ggtitle("Whitefish CHL")

dev.off()


summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP")))

summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP")))

wfsummerbothnd$year<-as.numeric(wfsummerbothnd$year)

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP")))

wfsummerbothnd$year<-as.character(wfsummerbothnd$year)


#nitrogen
tiff("TN_ND2023.tiff", units="in", width=7, height=5, res=300)
#filtering out 2009 because there is only one day of data and it is all nondetect
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009), #NOTE HERE 2009 IS FILTERED OUT
       aes(x = year, y = mean))+
  geom_hline(yintercept = 79.05, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 130)+
  mlc_theme+
  ylab("Summer Total Nitrogen, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TN")

dev.off()

#tn trend
summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009)))

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009)))
#phosphorus
tiff("TP_ND2023.tiff", units="in", width=7, height=5, res=300)

ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TP"), 
       aes(x = year, y = mean))+
  geom_hline(yintercept = 3.59, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TP")

dev.off()

#tp trend
summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "TP")))

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "TP")))

#do nn as well

ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "NN"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 2.91, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Nitrate, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish NN")

dev.off()

#trend nn
summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "NN")))

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "NN")))
































































 
 
 
 #figure out a good option for trib graphs

#now trib graphs #####
unique(data$Station_ID)
tribs<- data %>% filter(Station_Type == "River/Stream")
unique(tribs$Station_ID)

tribs$Activity_Start_Date<- as.POSIXct(tribs$Activity_Start_Date)



#make year and month cols
tribs<-tribs %>% mutate(year = year(Activity_Start_Date), 
                  month = month(Activity_Start_Date), day = yday(Activity_Start_Date))





#make mean for each year and max and min
tribssummer<- tribs  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

tribssummer$year<- as.character(tribssummer$year)

glimpse(tribssummer)
glimpse(wfsummer)

#means
beavtpmean<- tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "BEAV-CRK-RR")
mean(beavtpmean$Result_Value)
#make simple graphs for this 
ggplot(data = tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "BEAV-CRK-RR"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 13.15, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.4)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")

#do for cow
cowtpmean<- tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "COW-CRK-PA")
mean(cowtpmean$Result_Value)
#make simple graphs for this 
ggplot(data = tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "COW-CRK-PA"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 13.15, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.4)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")


#try other cow sight
cowrtpmean<- tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "COW-CRK-RR")
mean(cowrtpmean$Result_Value)
#make simple graphs for this 
ggplot(data = tribssummer %>% filter(Characteristic_ID == "TP", Station_ID == "COW-CRK-RR"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 13.15, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.4)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")



