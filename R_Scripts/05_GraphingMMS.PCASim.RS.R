library(librarian)
shelf(evolqg, tidyverse, magrittr, Rmisc, cowplot,ggplot2)

setwd("./")

####Prep df for graphing####
#mean matrix stats
setwd("./")

carb.mms<-(read.csv("./carb.mms2.bootstrapping.csv"))
cp.gen.mms<-(read.csv("./cp.gen.mms2.bootstrapping.csv"))
cp.hyb.mms<-(read.csv("./cp.hyb.mms2.bootstrapping.csv"))
cp.M.mms<-(read.csv("./cp.M.mms2.bootstrapping.csv"))
cp.P.mms<-(read.csv("./cp.P.mms2.bootstrapping.csv"))
ll.gen.mms<-(read.csv("./ll.gen.mms2.bootstrapping.csv"))
ll.hyb.mms<-(read.csv("./ll.hyb.mms2.bootstrapping.csv"))
ll.M.mms<-(read.csv("./ll.M.mms2.bootstrapping.csv"))
ll.P.mms<-(read.csv("./ll.P.mms2.bootstrapping.csv"))
ssi.gen.mms<-(read.csv("./ssi.gen.mms2.bootstrapping.csv"))
ssi.rad.mms<-(read.csv("./ssi.rad.mms2.bootstrapping.csv"))

all.mms.df<-data.frame()

lists <- list()

lists[[length(lists)+1]]<-list(df1=(carb.mms), p="carb")
lists[[length(lists)+1]]<-list(df1=(cp.gen.mms), p="cp.gen")
lists[[length(lists)+1]]<-list(df1=(cp.hyb.mms), p="cp.hyb")
lists[[length(lists)+1]]<-list(df1=(cp.M.mms), p="cp.M")
lists[[length(lists)+1]]<-list(df1=(cp.P.mms), p="cp.P")
lists[[length(lists)+1]]<-list(df1=(ll.gen.mms), p="ll.gen")
lists[[length(lists)+1]]<-list(df1=(ll.hyb.mms), p="ll.hyb")
lists[[length(lists)+1]]<-list(df1=(ll.M.mms), p="ll.M")
lists[[length(lists)+1]]<-list(df1=(ll.P.mms), p="ll.P")
lists[[length(lists)+1]]<-list(df1=(ssi.gen.mms), p="ssi.gen")
lists[[length(lists)+1]]<-list(df1=(ssi.rad.mms), p="ssi.rad")


for(l in lists){
  pc <- t(l[["df1"]])
  pc<-as.data.frame(pc)
  names(pc)<-pc[1,]
  print(pc)
  pc<-pc[-1,]
  print(pc)
  pc <- as.data.frame(pc)
  pc%<>%dplyr::mutate(Pop=l[['p']])
  all.mms.df<-rbind(all.mms.df,pc)
}

write.csv(all.mms.df,"./all.mms.csv")
#repeat similar process for PCA Similarity and Random Skewers

####Graphing####
setwd("./")
all.pcasim<-read.csv("./23_PCASimilarityAllPopulations.csv")
#rename all to PCASimilarity
all.pcasim[1:10, 1]<-"PCASimilarity"

stat.labs <- c("PCA Similarity")
names(stat.labs) <- c("PCASimilarity")

#RS/PCAsim Bootstrap graph Caribb vs SSI rad vs SSI gen
setwd("./")
all.rs<-read.csv("./24_RandomSkewersAllPopulations.csv")
#rename all to correlation
all.rs[1:10, 1]<-"correlation"

SSI.Carib.pcasim<-all.pcasim%>%
  filter(Pop == "ssi.gen.carb" | Pop == "ssi.rad.carb" | Pop == "ssi.gen.rad")

Carib.ssi_pcasim<-ggplot(data=SSI.Carib.pcasim, aes(x=factor(Pop), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=4,shape=4)+#had size at 3 but 4 is more visible I think
  facet_wrap(.~X, nrow=1, scales = "free_y", labeller = labeller(X = stat.labs))+
  expand_limits(y = c(0.5,0.8))+
  theme_light(base_size = 18)+
  scale_color_manual(values=c(ssi.gen.carb = '#ffdd32',ssi.rad.carb = '#f5bd06', ssi.gen.rad = "#00ff00"))+ #ssi.gen.carb = '#fff75e'
  scale_x_discrete(labels=c(ssi.gen.carb = "SSI Generalist-only/Caribbean",ssi.rad.carb = "SSI Radiation/Caribbean", ssi.gen.rad = "SSI Generalist-only/SSI Radiation"))+
  scale_y_continuous(breaks = seq (0.5, 0.8, by = 0.1))+#setting to 0.5 to minimize white space
  xlab(NULL)+
  ylab(NULL)+
  theme(text = element_text(size=15),strip.text = element_text(size=15))+
  theme(legend.position="none",axis.text.x = element_text(size=15,angle = 45, vjust = 0.5), axis.text.y = element_text(size=15)) #angle = 45))

stat.labs <- c("Random Skewers")
names(stat.labs) <- c("correlation")

SSI.Carib.rs<-all.rs%>%
  filter(Pop == "ssi.gen.carb" | Pop == "ssi.rad.carb" | Pop == "ssi.gen.rad")

Carib.ssi_rs<-ggplot(data=SSI.Carib.rs, aes(x=factor(Pop), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=4,shape=4)+
  facet_wrap(.~X, nrow=1, scales = "free_y", labeller = labeller(X = stat.labs))+
  expand_limits(y = c(0.5,0.8))+
  theme_light(base_size = 18)+
  scale_color_manual(values=c(ssi.gen.carb = '#ffdd32',ssi.rad.carb = '#f5bd06', ssi.gen.rad = "#00ff00"))+ #ssi.gen.carb = '#fff75e'
  scale_x_discrete(labels=c(ssi.gen.carb = "SSI Generalist-only/Caribbean",ssi.rad.carb = "SSI Radiation/Caribbean", ssi.gen.rad = "SSI Generalist-only/SSI Radiation"))+
  scale_y_continuous(breaks = seq (0.5, 0.8, by = 0.1))+#setting to 0.5 to minimize white space
  xlab(NULL)+
  ylab(NULL)+
  theme(text = element_text(size=15),strip.text = element_text(size=15))+
  theme(legend.position="none",axis.text.x = element_text(size=15,angle = 45, vjust = 0.5), axis.text.y = element_text(size=15)) #angle = 45))

cowplot::plot_grid(Carib.ssi_rs,Carib.ssi_pcasim)

#RS/PCAsim Bootstrap graph LL vs CP comparison

radiation_rs<-all.rs%>%
  filter(Pop == "cp.P.hyb" | Pop == "cp.M.hyb" | Pop == "cp.gen.hyb" | Pop == "cp.ll.hyb" | Pop == "ll.P.hyb" | Pop == "ll.M.hyb" | Pop == "ll.gen.hyb")
stat.labs <- c("Random Skewers")
names(stat.labs) <- c("correlation")

radiation_level_order<-c('cp.P.hyb','cp.M.hyb','cp.gen.hyb','cp.ll.hyb', 'll.P.hyb','ll.M.hyb','ll.gen.hyb')
radiation.rs<-ggplot(data=radiation_rs, aes(x=factor(Pop, level = radiation_level_order), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=4,shape=4)+
  facet_wrap(.~X, nrow=3, scales = "free_y", labeller = labeller(X = stat.labs))+
  theme_light(base_size = 18)+
  scale_color_manual(values=c(cp.M.hyb = '#094074',cp.ll.hyb = '#9ad2cb', cp.P.hyb = "#ffd6ba", cp.gen.hyb = "#5c7457", ll.M.hyb = '#094074', ll.P.hyb = "#ffd6ba", ll.gen.hyb = "#5c7457"))+
  scale_x_discrete(labels=c(cp.M.hyb = "Snail-eater/Hybrid",cp.ll.hyb = "CP Hybrid/LL Hybrid", cp.P.hyb = "Scale-eater/Hybrid", cp.gen.hyb ="Generalist/Hybrid", ll.M.hyb = "Snail-eater/Hybrid",ll.P.hyb = "Scale-eater/Hybrid", ll.gen.hyb ="Generalist/Hybrid"))+
  scale_y_continuous(breaks = seq (0, 1, by = 0.1))+
  xlab(NULL)+
  ylab(NULL)+
  theme(text = element_text(size=16),strip.text = element_text(size=16))+
  theme(legend.position="none",axis.text.x = element_text(size=15,angle = 45, vjust = 0.5), axis.text.y = element_text(size=15)) #angle = 45))
  

#RS/PCAsim Bootstrap graph LL vs CP comparison
radiation_pcasim<-all.pcasim%>%
  filter(Pop == "cp.P.hyb" | Pop == "cp.M.hyb" | Pop == "cp.gen.hyb" | Pop == "cp.ll.hyb" | Pop == "ll.P.hyb" | Pop == "ll.M.hyb" | Pop == "ll.gen.hyb")

stat.labs <- c("PCA Similarity")
names(stat.labs) <- c("PCASimilarity")

radiation_level_order<-c('cp.P.hyb','cp.M.hyb','cp.gen.hyb','cp.ll.hyb', 'll.P.hyb','ll.M.hyb','ll.gen.hyb')
radiation.pcasim<-ggplot(data=radiation_pcasim, aes(x=factor(Pop, level = radiation_level_order), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=4,shape=4)+
  facet_wrap(.~X, nrow=3, scales = "free_y", labeller = labeller(X = stat.labs))+
  theme_light(base_size = 18)+
  scale_color_manual(values=c(cp.M.hyb = '#094074',cp.ll.hyb = '#9ad2cb', cp.P.hyb = "#ffd6ba", cp.gen.hyb = "#5c7457", ll.M.hyb = '#094074', ll.P.hyb = "#ffd6ba", ll.gen.hyb = "#5c7457"))+
  scale_x_discrete(labels=c(cp.M.hyb = "Snail-eater/Hybrid",cp.ll.hyb = "CP Hybrid/LL Hybrid", cp.P.hyb = "Scale-eater/Hybrid", cp.gen.hyb ="Generalist/Hybrid", ll.M.hyb = "Snail-eater/Hybrid",ll.P.hyb = "Scale-eater/Hybrid", ll.gen.hyb ="Generalist/Hybrid"))+
  scale_y_continuous(breaks = seq (0, 1, by = 0.1))+
  xlab(NULL)+
  ylab(NULL)+
  theme(text = element_text(size=16),strip.text = element_text(size=16))+
  theme(legend.position="none",axis.text.x = element_text(size=15,angle = 45, vjust = 0.5), axis.text.y = element_text(size=15)) #angle = 45))

cowplot::plot_grid(radiation.rs,radiation.pcasim) #plot Random Skewers and PCASimilarity side-by-side

####Mean Matrix Statistics Graphs####
setwd("./")
all.mms<-read.csv("./22_MeanMatrixStatisticsAllPopulations.csv")

stat.labs <- c("Mean Squared Correlation", "Respondability", "Autonomy",
               "Flexibility", "Constraints")

names(stat.labs) <- c("MeanSquaredCorrelation", "respondability",
                      "autonomy","flexibility","constraints")


#MMS Bootstrap graph Caribb vs SSI rad vs SSI gen
SSI.Carib.mms<-all.mms%>%
  filter(Pop == "carb" | Pop == "ssi.gen" | Pop == "ssi.rad")

SSI.Carib.mms.plot<-ggplot(data=SSI.Carib.mms, aes(x=factor(Pop), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=3,shape=4)+
  facet_wrap(.~X, nrow=3, scales = "free_y", labeller = labeller(X = stat.labs))+
  expand_limits(y = 0)+
  theme_light(base_size = 18)+
  theme(strip.text = element_text(size=13))+
  scale_color_manual(values=c(carb = '#ffdd32',ssi.gen = '#f5bd06', ssi.rad = "#00ff00"))+
  scale_x_discrete(labels=c(carb = "Caribbean",ssi.gen = "SSI Generalist-only", ssi.rad = "SSI Radiation"))+
  xlab(NULL)+
  ylab(NULL)+
  theme(legend.position="none")


#MMS Bootstrap graph LL vs CP comparison
Radiation.mms<-all.mms%>%
  filter(Pop == "cp.gen" | Pop == "cp.hyb" | Pop == "cp.M" | Pop == "cp.P" | Pop == "ll.gen" | Pop == "ll.hyb" | Pop == "ll.M" | Pop == "ll.P")



radiation_level_order<-c('cp.M','cp.hyb','cp.P','cp.gen', 'll.M','ll.hyb','ll.P','ll.gen')
Radiation.mms.plot<-ggplot(data=Radiation.mms, aes(x=factor(Pop, level = radiation_level_order), y=mean, colour=Pop))+
  geom_linerange(aes(ymin=`lower`, ymax=`upper`), size=1)+
  geom_point(size=3,shape=4)+
  facet_wrap(.~X, nrow=3, scales = "free_y", labeller = labeller(X = stat.labs))+
  theme_light(base_size = 18)+
  theme(strip.text = element_text(size=13))+
  expand_limits(y = 0)+
  scale_color_manual(values=c(cp.M = '#094074',cp.hyb = '#9ad2cb', cp.P = "#ffd6ba", cp.gen = "#5c7457", ll.M = '#094074',ll.hyb = '#9ad2cb', ll.P = "#ffd6ba", ll.gen = "#5c7457"))+
  scale_x_discrete(labels=c(cp.M = "Snail-eater",cp.hyb = "Hybrid", cp.P = "Scale-eater", cp.gen ="Generalist", ll.M = "Snail-eater",ll.hyb = "Hybrid", ll.P = "Scale-eater", ll.gen ="Generalist"))+
  xlab(NULL)+
  ylab(NULL)+
  theme(text = element_text(size=13))+#or 10 if including pop abbrev in each label name
  theme(legend.position="none")
