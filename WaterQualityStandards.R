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


#make all graphs with sites combined ####
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



#secchi #####
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
sd(secchi2$Result_Value)
#sd = 9.41
#so add to mean to get lake water quality standard that is one sd off of mean
#new lake water quality standard = 32.06
