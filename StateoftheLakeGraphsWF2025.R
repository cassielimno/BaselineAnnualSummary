#STATE OF THE LAKE GRAPHS FOR 2024 DATA

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
library(rkt)



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

#upload date
data <- read.csv("HydroShareFinalALL2024.csv")
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




#find mean for individual things ##### THESE MIGHT NOT BE RIGHT ####
secchimean<-wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI")
mean(secchimean$Result_Value)

chlmean<- wfsummerboth %>% filter(Characteristic_ID == "CHL-A-CP")
mean(chlmean$Result_Value, na.rm = TRUE)

tnmean<- wfsummerbothnd %>% filter(Characteristic_ID == "TN")
mean(tnmean$result_nd, na.rm = TRUE)

#find tp mean
tp<-wfsummerbothnd %>% filter(Characteristic_ID == "TP", year > 2012)
mean(tp$result_nd, na.rm = TRUE)

nnmean<- wfsummerboth %>% filter(Characteristic_ID == "NN")
mean(nnmean$Result_Value, na.rm = TRUE)

#doing trends again for nds ####
wfsummerbothnd<- wf %>% filter(month == 6 | month == 7 | month == 8)

wfsummerbothnd<- wfsummerbothnd %>% 
  mutate(result_nd = case_when(Result_Detection_Condition == "Not Detected" ~ wfsummerbothnd$Method_Detection_Limit_Value,
                               Result_Detection_Condition == "" ~ wfsummerbothnd$Result_Value,
                               is.na(Result_Detection_Condition) ~ wfsummerbothnd$Result_Value)) 

wfsummerbothnd <- wfsummerbothnd %>% group_by(year, Characteristic_ID) %>% 
  mutate(mean = mean(result_nd, na.rm = TRUE), max = max(result_nd), min = min(result_nd),
         se = std.error(result_nd), n = length(result_nd))

wfsummerbothnd$year<- as.numeric(wfsummerbothnd$year)

#FOLDER FOR SAVING GRAPHS INTO
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/Whitefish data and code/SummaryStats/BaselineAnnualSummary/StateoftheLakeGraphs2025")


#RE-DOING TP, TN, CHLA, AND SECCHI GRAPHS AGAIN WITH WATERSHED CRITERIA AS LINE ######
#THESE ARE THE FINAL GRAPHS FOR THIS EDITION MADE 2-5-2025
#test all standards on graphs to see if they are similar #####
#secchi graph ####
wfsummerboth$year<- as.character(wfsummerboth$year)
#taking out 2019 since it is missing june
png("SecchiWQCriteria.png", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerboth %>% filter(Characteristic_ID == "DEPTH-SECCHI", !(year == "2019")),
       aes(x = year, y = mean))+
  geom_hline(yintercept = 17.27, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 22.75, linetype = "dashed", color = "black", alpha = 0.7)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(42, 0)+
  mlc_theme+
  ylab("Secchi Depth ft, (+/- s.e.)")+
  xlab("Year")

dev.off()

#make chl graphs
#need mwans for individual sites
wfsummerbothnd<- wfsummerbothnd %>% group_by(Station_ID, year, Characteristic_ID) %>% 
  mutate(meansite= mean(result_nd, na.rm = TRUE), maxsite = max(result_nd), minsite = min(result_nd),
         sesite = std.error(result_nd), nsite = length(result_nd))

#filter to out years before 2016 as they not taken at chl max
#WITH NEW LIMITED DATES THERE IS NO LONGER A TREND
#chl    #####
#taking out 2011 since there is only one data point
chlmk1dat<-  wfsummerbothnd %>% filter(Station_ID == "WF-LK-IP1", Characteristic_ID == "CHL-A-CP", year > 2015)
chlmk1<- rkt(chlmk1dat$year, chlmk1dat$result_nd, chlmk1dat$month, correct = TRUE, rep = "a") %>% 
  print(chlmk1)



#chl ip 2
chlmk2dat<-  wfsummerbothnd %>% filter(Station_ID == "WF-LK-IP2", Characteristic_ID == "CHL-A-CP", year >2015)
chlmk2<- rkt(chlmk2dat$year, chlmk2dat$result_nd, chlmk2dat$month, correct = TRUE, rep = "a") %>% 
  print(chlmk2)


chlmk1dat$year<-as.numeric(chlmk1dat$year)
chlmk2dat$year<-as.numeric(chlmk2dat$year)

#calculate new mean
chlall<- rbind(chlmk1dat, chlmk2dat)
#filter for after 16 when methods change
chlall16<- chlall %>% filter(year> 2015)
mean(chlall16$result_nd, na.rm = TRUE)

#calculate new sd
sd(chlall16$result_nd, na.rm = TRUE)
mean(chlall16$Result_Value, na.rm = TRUE)

png("CHLAWQCriteria.png", units="in", width=7, height=5, res=300)
ggplot()+
  geom_hline(yintercept = 2.72, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 1.72, linetype = "dashed", color = "black", alpha = .7)+
  geom_point(data = chlmk1dat %>% filter(year > 2015),
             aes(x = year, y = meansite), size = 2.5, color = "blue")+
  geom_point(data = chlmk2dat %>% filter(year > 2015),
             aes(x = year, y = meansite), size = 2.5, color = "red")+
  geom_errorbar(data= chlmk1dat, aes(x = year, y = mean, ymin = meansite-sesite, ymax = meansite+sesite),
                width = 0.3, color = "blue")+
  geom_errorbar(data= chlmk2dat, aes(x = year, y = mean, ymin = meansite-sesite, ymax = meansite+sesite), 
                width = 0.3, color = "red")+
  ylab("Summer Chl-a, (ug/L, +/- s.e.)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(2012, 2024, 1))+
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
    panel.grid.minor = element_line(color = "grey", linewidth = 0.3))


dev.off()



#nitrogen
#NOTE SOMETIMES THIS GRAPH SEPARATES OUT SITES RE-LOAD THE DATAFRAME AND IT SHOULD CORRECT
#filtering out 2009 because there is only one day of data and it is all nondetect
glimpse(wfsummerbothnd)
#make sure year is char
wfsummerbothnd$year<- as.character(wfsummerbothnd$year)
png("TNWQCriteria.png", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TN", year > 2009), #NOTE HERE 2009 IS FILTERED OUT
       aes(x = year, y = mean))+
  geom_hline(yintercept = 95, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 79.04, linetype = "dashed", color = "black", alpha = 0.7)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 130)+
  mlc_theme+
  ylab("Summer Total Nitrogen, (ug/L, +/- s.e.)")+
  xlab("Year")







#phosphorus ####
wfsummerbothnd$year<-as.character(wfsummerbothnd$year)
png("TPWQCriteria.png", units="in", width=7, height=5, res=300)
ggplot(data = wfsummerbothnd %>% filter(Characteristic_ID == "TP", year > 2012), 
       aes(x = year, y = mean))+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 3.96, linetype = "dashed", color = "black", alpha = 0.7)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.3)+
  ylim(0, 8)+
  mlc_theme+
  ylab("Summer Total Phosphorus, (ug/L, +/- s.e.)")+
  xlab("Year")
#ggtitle("Whitefish TP")


dev.off()


#deep do ####
#also take only deepest measure for deep do
#cut this data to just bottom for ip1 oxygen 
wfsummerboth$year<- as.numeric(wfsummerboth$year)
deepdo<- wfsummerboth  %>% group_by(year, Characteristic_ID, Station_ID, month) %>% 
  filter(Result_Depth_Height_Measure == max(Result_Depth_Height_Measure), Characteristic_ID == "DO-SAT") 

deepdo<- deepdo %>% group_by(Station_ID, year) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE),
         se = std.error(Result_Value), n = length(Result_Value))


deepdoip1<- filter(deepdo, Station_ID == "WF-LK-IP1")

deepdoip2<- filter(deepdo, Station_ID == "WF-LK-IP2")

#now do tests??
domk<- rkt(deepdoip1$year, deepdoip1$Result_Value, deepdoip1$month, correct = TRUE, rep = "a")
print(domk)

domkip2<-  rkt(deepdoip2$year, deepdoip2$Result_Value, deepdoip2$month, correct = TRUE,  rep = "a")
print(domkip2)

#can i graph?

png("deepDO.png", units="in", width=7, height=5, res=300)

ggplot()+
  geom_point(data = deepdoip1,
             aes(x = year, y = mean), size = 2.5, color = "blue")+
  geom_point(data = deepdoip2,
             aes(x = year, y = mean), size = 2.5, color = "red")+
  geom_errorbar(data= deepdoip1, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "blue")+
  geom_errorbar(data= deepdoip2, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "red")+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0, 112)+
  # mlc_theme+
  # theme(axis.text=element_text(size=1),
  # axis.title=element_text(size=1,face="bold"))+
  ylab("Summer Deep Lake Dissolved Oxygen Saturation, (%, +/- s.e.)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(2007, 2024, 1))+
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
    panel.grid.minor = element_line(color = "grey", linewidth = 0.3))



dev.off()

#calculate average deep do for both sites
mean(deepdo$Result_Value)







#get deep temp #####
wfsummerboth$year<- as.numeric(wfsummerboth$year)
wfsummerboth<- wfsummerboth %>% select(-max, -min, -mean, -se)
deeptemp<- wfsummerboth  %>% group_by(year, Characteristic_ID, Station_ID, month) %>% 
  filter(Result_Depth_Height_Measure == max(Result_Depth_Height_Measure), Characteristic_ID == "TEMP-W") 

deeptemp<- deeptemp %>% group_by(Station_ID, year) %>% 
  mutate(mean = mean(Result_Value, na.rm = TRUE),
         se = std.error(Result_Value), n = length(Result_Value), meandepth = mean(Result_Depth_Height_Measure))

deeptempip1<- filter(deeptemp, Station_ID == "WF-LK-IP1")

deeptempip2<- filter(deeptemp, Station_ID == "WF-LK-IP2")

#now do tests??
tempmk<- rkt(deeptempip1$year, deeptempip1$Result_Value, deeptempip1$month, correct = TRUE, rep = "a")
print(tempmk)

#this has a decreasing trend with a slope of -.26 ---- this is not included on graph 
#---The depth at which the deepest sample is taken increases in more recent years
#I think that is driving this trend
#No idea why we started being able to sample deeper into the lake
tempmkip2<-  rkt(deeptempip2$year, deeptempip2$Result_Value, deeptempip2$month, correct = TRUE, rep = "a")
print(tempmkip2)


png("deeptempNew.png", units="in", width=7, height=5, res=300)
#graphs
ggplot()+
  #geom_hline(yintercept = 84.45, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(data = deeptempip1,
             aes(x = year, y = mean), size = 2.5, color = "blue")+
  geom_point(data = deeptempip2,
             aes(x = year, y = mean), size = 2.5, color = "red")+
  geom_errorbar(data= deeptempip1, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "blue")+
  geom_errorbar(data= deeptempip2, aes(x = year, y = mean, ymin = mean-se, ymax = mean+se), 
                width = 0.3, color = "red")+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0, 65)+
  # mlc_theme+
  # theme(axis.text=element_text(size=1),
  # axis.title=element_text(size=1,face="bold"))+
  ylab("Summer Deep Lake Temperature, (F, +/- s.e.)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(2007, 2024, 1))+
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
    panel.grid.minor = element_line(color = "grey", linewidth = 0.3))

dev.off()




#graph for mean temp
#make a top 5 meters temp graphs
wfsummertemp <- wfsummerboth %>% filter(Result_Depth_Height_Measure < 5.5, Characteristic_ID == "TEMP-W") %>% 
  group_by(year) %>% mutate(meantemp = mean(Result_Value), setemp = std.error(Result_Value),
                            ntemp = length(Result_Value))

png("Temp2023.png", units="in", width=7, height=5, res=300)
ggplot(data = wfsummertemp,
       aes(x = year, y = meantemp))+
  geom_hline(yintercept = 62.66, linetype = "dashed", color = "black", alpha = 0.5)+
  geom_point(size = 2.5)+
  geom_errorbar(aes(ymin = meantemp-setemp, ymax = meantemp+setemp), width = 0.3)+
  ylim(30, 68)+
  scale_x_continuous(breaks = seq(2007, 2024, 1))+
  mlc_theme+
  ylab("Summer Surface Temperature, (F, +/- s.e.)")+
  xlab("Year")

dev.off()

mean(wfsummertemp$meantemp, na.rm = TRUE)



