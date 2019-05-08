#removing all objects from the environment
rm(list=ls())

#setting the working directory
setwd("~./casestudy/SensorFiles")

#installing and loading the packages
install.packages(dplyr)
install.packages(ggplot2)
library(dplyr)
library(ggplot2)

#importing the files
building<-read.csv("building.csv",sep=",",header=T)
hvac<-read.csv("HVAC.csv",sep=",",header = T)

# Making three variables (temp_diff, temprange, extremetemp) in Hvac table 
#Temp_didff = actual temperature - target temperature 
#temprangecolumn indicates whether the actual temperature was: 
#  NORMAL - within 5 degrees of the target temperature.
#  COLD - more than five degrees colder than the target temperature.
#  HOT - more than 5 degrees warmer than the target temperature. 
#Extrememetemp: If the temperature is outside of the normal range, extremetemp is assigned a value of 1;
#otherwise its value is 0. 

hvac$temp_diff<- hvac$ActualTemp-hvac$TargetTemp
hvac$temprange<-ifelse(hvac$temp_diff<(-5),"COLD",ifelse(hvac$temp_diff>5,"HOT","NORMAL"))
hvac$extremetemp<-ifelse(hvac$temprange=="NORMAL",0,1)

#merging the dataset by joining
data<-merge(hvac,building,by=c("BuildingID"),all=T)

#convert the given date into default date format in R
data$Date<-as.Date(data$Date,"%m/%d/%y")


#VISUALISATIONS

#Calculate count of extremetemp (i.e where the temperature was more than five degrees higher or lower
#than the target temperature) by  each country and temprange

data1<-filter(data,extremetemp==1)

#count by country
qplot(Country,data=data1)+theme_bw()+theme(axis.title=element_text(face="bold",size="8")) + theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+ labs(title="Extreme temperature by Country") 

#count by country and extremetemp
qplot(Country,data=data1,facets=temprange~.)+theme_bw()+ theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+ labs(title="Extreme temperature by Country and temprange")             

#Data visualization/analysis by mapping the buildings that are most frequently outside of the optimal temperature range
#PLOT1
#extremetemp by building and temprange                                                                                    
qplot(BuildingMgr,data=data1)+ theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+ labs(title="Buildings outside temperature range") 

#PLOT 2 (colour difference in bars corresponds to the change in count)
qplot(BuildingMgr,data=data1,facets=temprange~.)+theme_linedraw()+geom_bar(aes(fill = ..count..))+theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+ labs(title="Extreme temperature by Buildings and temprange") 

#PLOT 2 stacked bar chart

qplot(BuildingMgr,data=data1,fill=temprange)+theme_classic()+theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))+ labs(title="Extreme temperature by Buildings and temprange") 

#Calculate count of offices run in hot and count of office run in cold by country.

qplot(Country,data=data1,fill=BuildingMgr,facets = temprange~.)+theme_light()+
  theme(axis.title=element_text(face="bold",size="8")) + theme(legend.position="bottom")+
  theme(text = element_text(size=10),axis.text.x =element_text(angle = 90, hjust = 1))+ 
  labs(title="Offices run in hot/cold by Country")

#count of extreamtemp by hvacproduct

#PLOT1(line plot)
hvac_list <- aggregate(extremetemp ~ HVACproduct ,data = data1,FUN=sum) 
qplot(HVACproduct,extremetemp,data=hvac_list,geom=c("point","line"),color=extremetemp,group=1)+theme(axis.title=element_text(face="bold",size="10")) +
  labs(title="Extreme Temperature by HVAC product")+theme(plot.title = element_text(face="bold", size=20))

#PLOT2(histograms)
qplot(HVACproduct,data=data1,facets=temprange~.)+geom_bar(aes(fill = ..count..))+theme_linedraw()+theme(axis.title=element_text(face="bold",size="10"))+
  labs(title="Extreme Temperature by HVAC product and temprange")+theme(plot.title = element_text(face="bold", size=15))
###############################################################################################################################

