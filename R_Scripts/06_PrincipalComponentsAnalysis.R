setwd("./")

library(tidyr)
library(dplyr)
library(evolqg)
library(magrittr)
require(dplyr)

####read in covariance matrices####
ll_hyb_cov<-read.csv("./ll_hyb_cov.csv")
ll_M_cov<-read.csv("./ll_M_cov.csv")  
ll_P_cov<-read.csv("./ll_P_cov.csv")

####LL hyb####
#reorganizing data frame so that we can more easily do PCA
ll_hyb_cov%>%
  pivot_longer(cols = c(point5.8.dentigerous.arm.width.resids:point17.18.pelvic.girdle.length.resids),
               names_to = "measurement.2",
               values_to = "values")->ll_hyb_pc

colnames(ll_hyb_pc)<-c("measurement.1", "measurement.2", "values")

ll_hyb_pc%<>%
  mutate(paste.m1.m2=paste(measurement.1, measurement.2, sep = "_"))%>%
  dplyr::select(paste.m1.m2, values)%>%
  pivot_wider(id_cols = NULL, 
              names_from = paste.m1.m2,
              values_from = values)

####LL M####
#reorganizing data frame so that we can more easily do PCA
ll_M_cov%>%
  pivot_longer(cols = c(point5.8.dentigerous.arm.width.resids:point17.18.pelvic.girdle.length.resids),
               names_to = "measurement.2",
               values_to = "values")->ll_M_pc

colnames(ll_M_pc)<-c("measurement.1", "measurement.2", "values")

ll_M_pc%<>%
  mutate(paste.m1.m2=paste(measurement.1, measurement.2, sep = "_"))%>%
  dplyr::select(paste.m1.m2, values)%>%
  pivot_wider(id_cols = NULL, 
              names_from = paste.m1.m2,
              values_from = values)

####LL P####
#reorganizing data frame so that we can more easily do PCA
ll_P_cov%>%
  pivot_longer(cols = c(point5.8.dentigerous.arm.width.resids:point17.18.pelvic.girdle.length.resids),
               names_to = "measurement.2",
               values_to = "values")->ll_P_pc

colnames(ll_P_pc)<-c("measurement.1", "measurement.2", "values")

ll_P_pc%<>%
  mutate(paste.m1.m2=paste(measurement.1, measurement.2, sep = "_"))%>%
  dplyr::select(paste.m1.m2, values)%>%
  pivot_wider(id_cols = NULL, 
              names_from = paste.m1.m2,
              values_from = values)

####PCA and checking top 4 traits for LL####
ll_pc<-rbind(ll_hyb_pc,ll_P_pc,ll_M_pc) #data frame for principal components analysis - one row per population group

pc.ll<-prcomp(ll_pc[,1:324]) #perform principal components analysis
ll.pc.df<-as.data.frame(pc.ll$rotation)
write.csv(ll.pc.df,"./ll.pc.df.csv")

#calculate absolute value of PCA rotations so can determine the traits with the greatest contribution to PC1
ll.pc.df%<>%
  dplyr::mutate(pc1 = abs(PC1))

write.csv(ll.pc.df,"./ll.pc.abs.value.csv") 
#can then sort the PC1 column in descending order to find the traits with the greatest contribution to PC1
