library(librarian)
shelf(evolqg, tidyverse, magrittr, Rmisc)

setwd("./")

####Load in residual data frames####
ll.hyb.df<-read.csv("./08_LLHybridResiduals.csv")
ll.gen.df<-read.csv("./10_LLGeneralistResiduals.csv")


####Mean Matrix Stats for Little Lake hybrids####
dataset.1<-ll.hyb.df[,2:19]
iterations<-100
n.individuals<-nrow(dataset.1) #number of individuals
length.resampled.dataset<-nrow(dataset.1)

#100 iterations of sampling the Little Lake hybrid individuals with replacement
alply(1:iterations,
      1,
      function(x) dataset.1[sample(1:n.individuals,length.resampled.dataset, TRUE), ], 
      .parallel = FALSE)->resample.dataset.1

#for use with future comparisons
ll.hyb.resample.dataset<-resample.dataset.1

resample.dataset.1<-ll.hyb.resample.dataset
iterations<-100
ll.hyb.mms2.df<-data.frame()

for(i in 1:iterations){
  
  df.1<-as.data.frame(resample.dataset.1[i])  
  lm.1<-lm(as.matrix(df.1)~1)
  mat.1<-CalculateMatrix(lm.1) #produce variance-covariance matrix needed for MeanMatrixStatistics
  
  
  mms.output<-MeanMatrixStatistics(mat.1)%>%
    print()
  
  
  ll.hyb.mms2.df = rbind(ll.hyb.mms2.df,mms.output) #make new dataframe with MeanMatrixStatistics output
  colnames(ll.hyb.mms2.df)<-c("MeanSquaredCorrelation","pc1.percent","ICV","EigenSD","respondability","evolvability","conditional.evolvability","autonomy","flexibility","constraints")
  
}

ll.hyb.mms2.df<-ll.hyb.mms2.df[,- c(2:4,6:7)] #remove statistics did not focus on: PC 1 percent, ICV, EigenSD, evolvability, and conditional evolvability
ll.hyb.mms2.bootstrapping<-apply(ll.hyb.mms2.df, 2, Rmisc::CI) #calculate means and confidence intervals
write.csv(ll.hyb.mms2.bootstrapping,"./ll.hyb.mms2.bootstrapping.csv")


####Mean Matrix Stats for Little Lake generalists####
#Method to work with the small sample size for Little Lake generalists
ll.gen.samples<-ll.gen.df
ll.gen.samples<-ll.gen.samples[rep(seq(nrow(ll.gen.samples)), each=2), ]## replicate each unique individual so they each appear two times

dataset.1<-ll.gen.samples[,2:19]
iterations<-100
n.individuals<-nrow(dataset.1)
length.resampled.dataset<-(nrow(dataset.1)/2)#because have 2 of each individual- need to match original pop size

alply(1:iterations,
      1,
      function(x) dataset.1[sample(1:n.individuals,length.resampled.dataset, FALSE), ], #with replacement is FALSE
      .parallel = FALSE)->resample.dataset.1

ll.gen.resample.dataset<-resample.dataset.1

resample.dataset.1<-ll.gen.resample.dataset
iterations<-100
ll.gen.mms2.df<-data.frame()

for(i in 1:iterations){
  
  #dataset 1
  df.1<-as.data.frame(resample.dataset.1[i])  
  lm.1<-lm(as.matrix(df.1)~1)
  mat.1<-CalculateMatrix(lm.1) #produce variance-covariance matrix needed for MeanMatrixStatistics
  
  
  mms.output<-MeanMatrixStatistics(mat.1)%>%
    print()
  
  
  ll.gen.mms2.df = rbind(ll.gen.mms2.df,mms.output) #make new dataframe with MeanMatrixStatistics output
  colnames(ll.gen.mms2.df)<-c("MeanSquaredCorrelation","pc1.percent","ICV","EigenSD","respondability","evolvability","conditional.evolvability","autonomy","flexibility","constraints")
  
}

ll.gen.mms2.df<-ll.gen.mms2.df[,-4] #remove statistics did not focus on: PC 1 percent, ICV, EigenSD, evolvability, and conditional evolvability
ll.gen.mms2.bootstrapping<-apply(ll.gen.mms2.df, 2, Rmisc::CI) #calculate means and confidence intervals
write.csv(ll.gen.mms2.bootstrapping,"ll.gen.mms2.bootstrapping.csv")
