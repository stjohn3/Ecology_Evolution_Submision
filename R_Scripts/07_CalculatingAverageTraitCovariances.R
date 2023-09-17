setwd("~/Downloads/Honors_Thesis/P-matrix/Cov_matrices_Nov_22/")

library(tidyr)
library(dplyr)
library(magrittr)
require(dplyr)

####read in covariance matrices for hybrids####
ll_hyb_cov<-read.csv("./ll_hyb_cov.csv")

####LL hyb cov means####
#looking at 4 traits that contributed the most to PC1 (Maxillary Head Height, Maxillary Head Protrusion, Nasal Tissue Protrusion, Opening In-lever)
#variance values (can see row number, col number is row number +1 to account for rownname col): MHH [17,18]  MHP [15,16]  NTP[12,13]  OIL [11,12]

ll_hyb_cov_means=data.frame()

lists <- list()
lists[[length(lists)+1]]<-list(a=18,b=17,row=1,traitname="maxillary.head.height")#a is column number of the specified trait, b is row number of that trait --> variance
lists[[length(lists)+1]]<-list(a=16,b=15,row=2,traitname="maxillary.head.protrusion")
lists[[length(lists)+1]]<-list(a=13,b=12,row=3,traitname="nasal.tissue.protrusion")
lists[[length(lists)+1]]<-list(a=12,b=11,row=4,traitname="opening.in.lever")

for(l in lists){
  trait<-ll_hyb_cov[-c(l[["b"]]),] #remove column of the trait we are calculating the covariance for (essentially removes the variance becaused we will next select for the row of that trait)
  trait<-trait[,l[["a"]]]
  trait%<>% #take absolute value of the covariances
    abs()
  print(trait)
  ll_hyb_cov_means[l[["row"]],1]<-mean(trait) #take mean of the covariances for that particular trait
  ll_hyb_cov_means[l[["row"]],2]<-l[["traitname"]]  #label mean for that trait in the data frame               
  print(ll_hyb_cov_means)
}

ll_hyb_cov_means%<>% #rename columns of new data frame
  dplyr::rename(mean = V1)%>%
  dplyr::rename(trait = V2)

####LL mean covariance of all traits####
for(i in 1:18){ 
  ll_hyb_cov[i,i]<-NA #preparing for removing the variances by turning them into NAs
}

ll_hyb_cov%<>% #removing name column
  select(point5.8.dentigerous.arm.width.resids:point17.18.pelvic.girdle.length.resids)

ll_hyb_cov_means[5,1]<-(mean(as.matrix(ll_hyb_cov), na.rm=TRUE)) #calculate mean of all covariance values
ll_hyb_cov_means[5,2]<-"all.traits" #label all trait mean

write.csv(ll_hyb_cov_means,"ll_hyb_cov_means.csv",row.names = FALSE) #download CSV of mean covariances for 4 individual traits and all traits
