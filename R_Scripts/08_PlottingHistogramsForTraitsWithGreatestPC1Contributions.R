library(librarian)
shelf(tidyverse, magrittr, cowplot)

setwd("~/Downloads/Honors_Thesis/P-matrix/Resids")

####read in residual data frames for hybrids and parental types####
ll.hyb.df<-read.csv("./LL_hybrid_resids.csv")
ll.M.df<-read.csv("./LL_M_resids.csv")  
ll.P.df<-read.csv("./LL_P_resids.csv")
cp.hyb.df<-read.csv("./CRP_hybrid_resids.csv")
cp.M.df<-read.csv("./CRP_M_resids.csv")
cp.P.df<-read.csv("./CRP_P_resids.csv")

####Little Lake nasal tissue protrusion histogram####
#select nasal tissue protrusion residuals for Little Lake
ntp.ll.hyb<-ll.hyb.df%>% #hybrids
  select(1, 13)

ntp.ll.M<-ll.M.df%>% #snail-eaters
  select(1, 13)
ntp.ll.M.val<-mean(ntp.ll.M$point12.19.nasal.tissue.protrusion.resids) #mean of nasal tissue protrusion residuals for snail-eaters

ntp.ll.P<-ll.P.df%>% #scale-eaters
  select(1, 13)
ntp.ll.P.val<-mean(ntp.ll.P$point12.19.nasal.tissue.protrusion.resids) #mean of nasal tissue protrusion residuals for scale-eaters

ntp.ll.exp.hyb<-(ntp.ll.M.val+(ntp.ll.P.val)/2) #expected nasal tissue protrusion size for hybrids if value is average of parental means

ntp.ll.hyb.like.P<-sum(ntp.ll.hyb$point12.19.nasal.tissue.protrusion.resids < ntp.ll.P.val) #number of hybrids with nasal tissue protrusions more like those of scale-eaters (left of the scale-eater mean)
ntp.ll.hyb.like.M<-sum(ntp.ll.hyb$point12.19.nasal.tissue.protrusion.resids > ntp.ll.M.val) #number of hybrids with nasal tissue protrusions more like those of snail-eaters (right of the snail-eater mean)

ntp.ll<-ggplot(ntp.ll.hyb, aes(point12.19.nasal.tissue.protrusion.resids))+
  theme_classic()+xlab("Nasal Tissue Protrusion")+
  stat_bin(geom="step",breaks=seq(-1.25, 1.25, by=0.1),color="gray")+
  ylab("Number of Individuals")+
  scale_x_continuous(limits = c(-1.25,1.25), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,45), expand = c(0, 0))+
  geom_vline(xintercept = ntp.ll.M.val,color="#094074")+geom_vline(xintercept = ntp.ll.P.val,color="#ff8533")+geom_vline(xintercept = ntp.ll.exp.hyb,linetype = "dashed",color ="#56b3a8")+ #lines for parental means and hybrid intermediate value 
  annotate(x=ntp.ll.exp.hyb,y=+Inf,label="Hybrid",vjust=3,geom="label",color ="#56b3a8")+
  annotate(x=ntp.ll.M.val,y=+Inf,label="Snail-eater",vjust=1,hjust=-0.05,geom="label",color="#094074")+
  annotate(x=ntp.ll.P.val,y=+Inf,label="Scale-eater",vjust=1,hjust=1.05,geom="label",color="#ff8533")+
  ggtitle("Little Lake")+
  theme(plot.title = element_text(hjust = 0.5))

####Crescent Pond nasal tissue protrusion histogram####
#select nasal tissue protrusion residuals for Crescent Pond
ntp.cp.hyb<-cp.hyb.df%>%
  select(1, 13)

ntp.cp.M<-cp.M.df%>%
  select(1, 13)
ntp.cp.M.val<-mean(ntp.cp.M$point12.19.nasal.tissue.protrusion.resids) #mean of nasal tissue protrusion residuals for snail-eaters

ntp.cp.P<-cp.P.df%>%
  select(1, 13)
ntp.cp.P.val<-mean(ntp.cp.P$point12.19.nasal.tissue.protrusion.resids) #mean of nasal tissue protrusion residuals for scale-eaters

ntp.cp.exp.hyb<-((ntp.cp.M.val+ntp.cp.P.val)/2) #expected nasal tissue protrusion size for hybrids if value is average of parental means

ntp.cp.hyb.like.P<-sum(ntp.cp.hyb$point12.19.nasal.tissue.protrusion.resids < ntp.cp.P.val) #number of hybrids with nasal tissue protrusions more like those of scale-eaters (left of the scale-eater mean)
ntp.cp.hyb.like.M<-sum(ntp.cp.hyb$point12.19.nasal.tissue.protrusion.resids > ntp.cp.M.val) #number of hybrids with nasal tissue protrusions more like those of snail-eaters (right of the snail-eater mean)

ntp.cp<-ggplot(ntp.cp.hyb, aes(point12.19.nasal.tissue.protrusion.resids))+
  theme_classic()+xlab("Nasal Tissue Protrusion")+
  stat_bin(geom="step",breaks=seq(-1.25, 1.25, by=0.1),color="gray")+
  theme_classic()+xlab("Nasal Tissue Protrusion")+
  ylab("Number of Individuals")+
  scale_x_continuous(limits = c(-1.25,1.25), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,60), expand = c(0, 0))+ #higher y limit (60) to account for there being more Crescent Pond hybrids than Little Lake hybrids
  geom_vline(xintercept = ntp.cp.M.val,color="#094074")+geom_vline(xintercept = ntp.cp.P.val,color="#ff8533")+geom_vline(xintercept = ntp.cp.exp.hyb,linetype="dashed",color = "#56b3a8")+
  annotate(x=ntp.cp.exp.hyb,y=+Inf,label="Hybrid",vjust=3,geom="label",color="#56b3a8")+
  annotate(x=ntp.cp.M.val,y=+Inf,label="Snail-eater",vjust=1,hjust=-0.05,geom="label",color="#094074")+
  annotate(x=ntp.cp.P.val,y=+Inf,label="Scale-eater",vjust=1,hjust=1.05,geom="label",color="#ff8533")+
  ggtitle("Crescent Pond")+
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(ntp.cp,ntp.ll) #plot Crescent Pond and Little Lake graphs side-by-side
