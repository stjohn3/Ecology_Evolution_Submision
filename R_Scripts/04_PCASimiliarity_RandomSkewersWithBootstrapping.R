library(librarian)
shelf(evolqg, tidyverse, magrittr, Rmisc, cowplot, stringi, glue)

setwd("./")

####Load in residual data frames####
ll.gen.df<-read.csv("./LL_A_resids.csv")
ll.hyb.df<-read.csv("./LL_hybrid_resids.csv")

####Preparing resampled data frames####
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


####PCASimilarity for Little Lake generalists and Little Lake hybrids####
resample.dataset.1<-ll.gen.resample.dataset

resample.dataset.2<-ll.hyb.resample.dataset

ll.gen.ll.hyb.pca.df = data.frame()

for(i in 1:iterations){
  
  #dataset 1
  df.1<-as.data.frame(resample.dataset.1[i])  
  lm.1<-lm(as.matrix(df.1)~1)
  mat.1<-CalculateMatrix(lm.1) #produce variance-covariance matrix needed for PCASimilarity
  
  #dataset 1
  df.2<-as.data.frame(resample.dataset.2[i])  
  lm.2<-lm(as.matrix(df.2)~1)
  mat.2<-CalculateMatrix(lm.2)
  
  pca.output<-PCAsimilarity(mat.1,mat.2)%>%
    print()
  
  
  ll.gen.ll.hyb.pca.df = rbind(ll.gen.ll.hyb.pca.df,pca.output)
  
}

colnames(ll.gen.ll.hyb.pca.df)<-c('PCASimilarity')
ll.gen.ll.hyb.pca.bootstrapping<-apply(ll.gen.ll.hyb.pca.df, 2, Rmisc::CI)
write.csv(ll.gen.ll.hyb.pca.bootstrapping,"./ll.gen.ll.hyb.pca.bootstrapping.csv")


####Random Skewers for Little Lake generalists and Little Lake hybrids####
resample.dataset.1<-ll.gen.resample.dataset

resample.dataset.2<-ll.hyb.resample.dataset

ll.gen.ll.hyb.rs.df = data.frame()


for(i in 1:iterations){
  
  #dataset 1
  df.1<-as.data.frame(resample.dataset.1[i])  
  lm.1<-lm(as.matrix(df.1)~1)
  mat.1<-CalculateMatrix(lm.1) #produce variance-covariance matrix needed for RandomSkewers
  
  #dataset 1
  df.2<-as.data.frame(resample.dataset.2[i])  
  lm.2<-lm(as.matrix(df.2)~1)
  mat.2<-CalculateMatrix(lm.2)
  
  rs.output<-RandomSkewers(mat.1,mat.2)%>%
    print()
  
  
  ll.gen.ll.hyb.rs.df = rbind(ll.gen.ll.hyb.rs.df,rs.output)
  
}

ll.gen.ll.hyb.rs.df<-ll.gen.ll.hyb.rs.df[1]
colnames(ll.gen.ll.hyb.rs.df)<-c('correlation') #subset the output that we will compare (correlation)
ll.gen.ll.hyb.rs.bootstrapping<-apply(ll.gen.ll.hyb.rs.df, 2, Rmisc::CI)
write.csv(ll.gen.ll.hyb.rs.bootstrapping,"./ll.gen.ll.hyb.rs.bootstrapping.csv")
