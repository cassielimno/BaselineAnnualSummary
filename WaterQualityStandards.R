#figuring out lake water quality standards 
#based off of state of the lake means etc
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/Whitefish data and code/SummaryStats/BaselineAnnualSummary")
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
library(rkt)

#THIS IS THE COMPLETED DATA for 2023
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
data<- read.csv("HydroShareFinalALL2023.csv")

wf<- data %>% filter(Station_ID == "WF-LK-IP1" | Station_ID == "WF-LK-IP2")


glimpse(wf)

#turn date to date


wf$Activity_Start_Date<- as.POSIXct(wf$Activity_Start_Date)



#make year and month cols
wf<-wf %>% mutate(year = year(Activity_Start_Date), 
                  month = month(Activity_Start_Date), day = yday(Activity_Start_Date))



#make mean for each year and max and min
unique(wf$Station_ID)
wfsummerboth<- wf  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

glimpse(wfsummerboth)


wfsummerboth$year<- as.character(wfsummerboth$year)

#also do this for them seperate
#make mean for each year and max and min
wfsummer<- wf  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

glimpse(wfsummer)

wfsummer$year<- as.character(wfsummer$year)

#for both together
unique(wf$Station_ID)
wfsummerboth<- wf  %>% filter(month == 6 | month == 7 | month == 8) %>% group_by(year, Characteristic_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE), max = max(Result_Value), min = min(Result_Value),
         se = std.error(Result_Value))

glimpse(wfsummerboth)


wfsummerboth$year<- as.character(wfsummerboth$year)



#calculating secchi crital threshhold#####
#long term summer mean established in state of the lake is 22.65
#lets see what one standard deviation off of that is

#finding long term mean
secchimean<-wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")
mean(secchimean$Result_Value)


#intermission to check trends lol #####
#doing linear regression analysis
summary(lm(Result_Value~Activity_Start_Date, data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")))
wfsummerboth$year<-as.numeric(wfsummerboth$year)
summary(lm(mean~year, data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")))


#doing mann kendall analysis
#taking out 2011 since there is only one data point
secchi<- wfsummer %>% group_by(Station_ID, year, Characteristic_ID) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE),
         se = std.error(Result_Value), n = length(Result_Value))
glimpse(secchi)
secchi$year<- as.numeric(secchi$year)
#perform mann kendall
secchimk1dat<-  secchi %>% filter(Station_ID == "WF-LK-IP1", Characteristic_ID == "DEPTH-SECCHI")
secchimk1<- rkt(secchimk1dat$year, secchimk1dat$Result_Value, secchimk1dat$month, correct = TRUE, rep = "a") %>% 
print(secchimk1)


#secchi ip 2
secchimk2dat<-  secchi %>% filter(Station_ID == "WF-LK-IP2", Characteristic_ID == "DEPTH-SECCHI")
secchimk2<- rkt(secchimk2dat$year, secchimk2dat$Result_Value, secchimk2dat$month, correct = TRUE, rep = "a") %>% 
print(secchimk2)

#mann kendal on both together
secchi2<- secchi %>% filter(Characteristic_ID == "DEPTH-SECCHI")
secchimk<- rkt(secchi2$year, secchi2$Result_Value, secchi2$month, correct = TRUE, rep = "a") %>% 
  print(secchimk)

#there is no sig trend in commbined data -- since this is whats on published graph
#gonna stick with no sig trend


#now find one sd out for secchi #####
sd(secchi2$mean)
#sd = 5.38
#so subtract mean (22.65) to mean to get lake water quality standard that is one sd off of mean
#new lake water quality standard = 17.27



#now find sd for chl ####
chl<- wfsummer %>% filter(Characteristic_ID == "CHL-A-CP")
glimpse(chl)
sd(chl$Result_Value, na.rm = TRUE)
mean(chl$Result_Value, na.rm = TRUE)
#mean + sd = 2.51 = new lake water quality standard for both sites






#test all standards on graphs to see if they are similar #####
#secchi graph ####
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI"),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 13.24, linetype = "dashed", color = "red")+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 42)+
  ggtitle("Whitefish Secchi")+
  mlc_theme+
  ylab("Secchi Depth ft, (+/- s.e.)")+
  xlab("Year")
#chl    #####
wfsummerbothnd<- wf %>% filter(month == 6 | month == 7 | month == 8)

wfsummerbothnd<- wfsummerbothnd %>% 
  mutate(result_nd = case_when(Result_Detection_Condition == "Not Detected" ~ wfsummerbothnd$Method_Detection_Limit_Value,
                               Result_Detection_Condition == "" ~ wfsummerbothnd$Result_Value,
                               is.na(Result_Detection_Condition) ~ wfsummerbothnd$Result_Value)) 

wfsummerbothnd <- wfsummerbothnd %>% group_by(year, Characteristic_ID, Station_ID) %>% 
  mutate(mean = mean(result_nd, na.rm = TRUE), max = max(result_nd), min = min(result_nd),
         se = std.error(result_nd), n = length(result_nd))

wfsummerbothnd$year<- as.numeric(wfsummerbothnd$year)
#taking out 2011 since there is only one data point
chlmk1dat<-  wfsummerbothnd %>% filter(Station_ID == "WF-LK-IP1", Characteristic_ID == "CHL-A-CP", year > 2011)
chlmk1<- rkt(chlmk1dat$year, chlmk1dat$result_nd, chlmk1dat$month, correct = TRUE, rep = "a")
print(chlmk1)


#chl ip 2
chlmk2dat<-  wfsummerbothnd %>% filter(Station_ID == "WF-LK-IP2", Characteristic_ID == "CHL-A-CP", year >2011)
chlmk2<- rkt(chlmk2dat$year, chlmk2dat$result_nd, chlmk2dat$month, correct = TRUE, rep = "a")
print(chlmk2)


chlmk1dat$year<-as.numeric(chlmk1dat$year)
chlmk2dat$year<-as.numeric(chlmk2dat$year)


trend_line<-predict(loess(mean ~ year, data = chlmk1dat))

trend_line2<- predict(loess(mean~year, data = chlmk2dat))

#make graphs for this
ggplot()+
  geom_hline(yintercept = 2.51, linetype = "dashed", color = "red")+
  geom_point(data = chlmk1dat,
             aes(x = year, y = mean), size = 2.5, color = "blue")+
  geom_point(data = chlmk2dat,
             aes(x = year, y = mean), size = 2.5, color = "forestgreen")+
  geom_errorbar(data= chlmk1dat, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "blue")+
  geom_errorbar(data= chlmk2dat, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "forestgreen")+
  #geom_smooth(method = "lm", se = FALSE)+
  geom_line(data= chlmk1dat, aes(x = year, y = trend_line), color = "blue", size = 1)+
  geom_line(data= chlmk2dat, aes(x = year, y = trend_line2), color = "forestgreen", size = 1)+
  #ylim(0, 65)+
  # mlc_theme+
  # theme(axis.text=element_text(size=1),
  # axis.title=element_text(size=1,face="bold"))+
  ylab("Summer CHL, (ug/L, +/- s.e.)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(2012, 2023, 1))+
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
    panel.grid.minor = element_line(color = "grey", linewidth = 0.3))+
  ggtitle("Whitefish Lake Chlorophyll-a")







#Graphs not split out by site #####
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



#nitrogen

#filtering out 2009 because there is only one day of data and it is all nondetect
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009), #NOTE HERE 2009 IS FILTERED OUT
       aes(x = year, y = mean))+
  geom_hline(yintercept = 95, linetype = "dashed", color = "red")+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 130)+
  mlc_theme+
  ylab("Summer Total Nitrogen, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TN")



#tn trend
summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009)))

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009)))



#phosphorus ####
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TP"), 
       aes(x = year, y = mean))+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red")+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  #ylim(0, 40)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")+
  ggtitle("Whitefish TP")

#tp trend
summary(lm(result_nd~Activity_Start_Date, data = wfsummerbothnd %>% filter(Characteristic_ID == "TP")))

summary(lm(mean~year, data = wfsummerbothnd %>% filter(Characteristic_ID == "TP")))


#calculate all variances #####
tpnd<- wfsummerbothnd %>% filter(Characteristic_ID == "TP")
var(tpnd$mean)
var(tpnd$result_nd)
tnnd<- wfsummerbothnd %>% filter(Characteristic_ID == "TN")
var(tnnd$mean)
var(tnnd$result_nd)
chlnd<-wfsummerbothnd %>% filter(Characteristic_ID == "CHL-A-CP")

#calculate sd
sd(tpnd$mean)
sd(tnnd$mean)
sd(chlnd$mean)
