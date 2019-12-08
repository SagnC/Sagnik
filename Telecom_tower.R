getwd()
setwd("C:/Users/A Chatterjee/Downloads")
Telecom_tower=read.csv("Assignment.csv",stringsAsFactors = F)
View(Telecom_tower)
library(dplyr)
library(ggplot2)
####Understanding the data 
glimpse(Telecom_tower)

####Need to verify whether there is any missing values in the data
lapply(Telecom_tower,function(x)sum(is.na(x)))

####converting EPOCH time to human time
Telecom_tower$Timestamp=as.POSIXct(Telecom_tower$Timestamp/1000, origin="1970-01-01",tz="")

####Application.status,Grid.status are factors.Thus converting them into factors
Telecom_tower$Application.status=as.factor(Telecom_tower$Application.status)
Telecom_tower$Grid.status=as.factor(Telecom_tower$Grid.status)

####Finding correlation between SOC and Grid.status
library(ltm)
####Using biserial correlation as one variable is numeric and other one is binary
biserial.cor(Telecom_tower$SOC,Telecom_tower$Grid.status,level = 1)
####Visualization of the relationship between SOC and Grid.status
ggplot(Telecom_tower,aes(y=SOC,x=Grid.status))+geom_point()
####Finding correlation between Equivalent.cycle and SOH and showing graphically
cor.test(Telecom_tower$Equivalent.cycle,Telecom_tower$SOH)
library(ggpubr)
ggscatter(Telecom_tower, x = "Equivalent.cycle", y = "SOH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

####Finding correlation between SOC and Temperature
cor.test(Telecom_tower$SOC,Telecom_tower$Temperature,method = "pearson")

ggplot(Telecom_tower,aes(x=Temperature,y=SOC))+geom_smooth(color="black")

