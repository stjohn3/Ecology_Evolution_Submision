setwd("./")

library(tidyr)
library(magrittr)
library(dplyr)
require(tidyverse)


###load in xy coordinates from DLTdv####
xypts.df<-read.csv("./xypts.csv")

####Lower Jaw Length (pt 1-2 in df)####
point1.x<-as.numeric(xypts.df[,2]) #selecting column with x-coordinate of point 1
point2.x<-as.numeric(xypts.df[,4]) #selecting column with x-coordinate of point 2

point1.y<-as.numeric(xypts.df[,3]) #selecting column with y-coordinate of point 1
point2.y<-as.numeric(xypts.df[,5]) #selecting column with y-coordinate of point 2

A<-as.numeric(point1.x-point2.x)
B<-as.numeric(point1.y-point2.y)

C_Sqr<-as.numeric((A^2)+(B^2)) #using Pythagorean theorem to calculate distance between two points

xypts.df$lower.jaw.length<-sqrt(C_Sqr)


####head grid (pt 20-21 in df)####
point1.x<-as.numeric(xypts.df[,40])
point2.x<-as.numeric(xypts.df[,42])

point1.y<-as.numeric(xypts.df[,41])
point2.y<-as.numeric(xypts.df[,43])

A<-as.numeric(point1.x-point2.x)
B<-as.numeric(point1.y-point2.y)

C_Sqr<-as.numeric((A^2)+(B^2)) #calculating measurement of one grid square in photo

xypts.df$grid<-sqrt(C_Sqr)
xypts.df$calibration.mm<-as.numeric(1) #corresponding length of one grid square is 1 mm


####Calculating mm####
measurements.mm<-xypts.df%>%
  dplyr::mutate(point1.2.lower.jaw.length.resids=(lower.jaw.length*calibration.mm)/(grid)) #using proportions to calculate the trait distance in mm
#add lines for other traits measuring

#selecting for the ID, traits
measurements.mm%<>%
  select(1,64:81)

#write.csv(measurements.mm, file="measurementss.mm.csv",row.names = F)