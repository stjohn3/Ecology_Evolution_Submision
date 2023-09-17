setwd("./")

library(tidyr)
library(magrittr)
library(dplyr)
library(lme4)
library(modelr)

####Load in linear distances####
measurements.mm<-read.csv("./measurements.mm.csv")

####Residuals####

measurements.resids<-measurements.mm

namelist<-c("point5.8.dentigerous.arm.width.resids","point7.5.dentigerous.arm.base.resids",
            "point1.2.lower.jaw.length.resids","point8.9.dentigerous.arm.depth.resids",
            "point2.11.palatine.height.resids","point2.3.jaw.closing.inlever.resids",
            "point15.16.cranial.height.resids", "point14.15.orbit.diameter.resids", 
            "point2.18.suspensorium.length.resids","point9.10.ascending.proess.length.resids",
            "point2.4.opening.inlever.resids","point12.19.nasal.tissue.protrusion.resids",
            "point11.14.ectopterygoid.resids", "point16.18.head.depth.resids",
            "point12.13.maxillary.head.protrusion.resids","point6.11.maxilla.length.resids",
            "point11.13.maxillary.head.height.resids","point17.18.pelvic.girdle.length.resids")

#run to calculate residuals on 18 traits listed above in namelist
for(i in 2:19){
  fit<-lm(log(measurements.resids[,i])~measurements.resids$point20.31.SL)
  measurements.resids<-modelr::add_residuals(data=measurements.resids, fit, var="resid")
  
  names(measurements.resids)[names(measurements.resids) == "resid"] <- namelist[i-1]
}

#select ID column and residual columns
measurements.resids%<>%
  dplyr::select(ID.Number,
                point5.8.dentigerous.arm.width.resids,point7.5.dentigerous.arm.base.resids,
                point1.2.lower.jaw.length.resids,point8.9.dentigerous.arm.depth.resids,
                point2.11.palatine.height.resids,point2.3.jaw.closing.inlever.resids,
                point15.16.cranial.height.resids,point14.15.orbit.diameter.resids,
                point2.18.suspensorium.length.resids,point9.10.ascending.proess.length.resids,
                point2.4.opening.inlever.resids,point12.19.nasal.tissue.protrusion.resids,
                point11.14.ectopterygoid.resids,point16.18.head.depth.resids,
                point12.13.maxillary.head.protrusion.resids,point6.11.maxilla.length.resids,
                point11.13.maxillary.head.height.resids,point17.18.pelvic.girdle.length.resids)

write.csv(measurements.resids, "./measurements.resids.csv",row.names = F)

