setwd("./")

library(tidyr)
library(dplyr)
library(evolqg)
library(magrittr)
require(dplyr)

####read in dataframes of residuals for population groups####
ll.hyb.df<-read.csv("./08_LLHybridResiduals.csv")


####LL hybrid variance-covariance matrix (P matrix)####
ll_hyb_cov<-CalculateMatrix(lm(as.matrix(ll.hyb.df[,2:19])~1)) #calculate matrix from linear model of traits by ID of individuals
write.csv(ll_hyb_cov,"./19_LLHybridCovMatrix.csv",row.names = TRUE)

