library(ggplot2)
library(gridExtra)
library(ggpubr)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/stat_code/")

RA<-read.csv("Regression_Analysis_IND.csv",header=T)

RA_F192<-subset(RA,Species=="F192")
RA_F210<-subset(RA,Species=="F210")
RA_Anabena<-subset(RA,Species=="Anabena")
RA_Shewanella<-subset(RA,Species=="Shewanella")

data.lm1<-lm(Visual~Script,data=RA_F192)
summary(data.lm1)

F192<-ggplot(data=RA_F192, aes(x=Script,y=Visual))+
  theme_classic()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y="Manual counts")+
  labs(title="Microcystis F192")+
  theme(plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        plot.margin=unit(c(1,1,1,1),"cm"))+
  geom_abline(slope=1,intercept=0,color="red",size=2,alpha=0.4)+
  geom_abline(slope=coef(data.lm1)[["Script"]],
              intercept=coef(data.lm1)[["(Intercept)"]],
              linetype=2,
              size=2)+
  geom_point(size=5,shape=21,colour="black",fill="green",stroke=2)
F192

data.lm2<-lm(Visual~Script,data=RA_F210)
summary(data.lm2)

F210<-ggplot(data=RA_F210, aes(x=Script,y=Visual))+
  theme_classic()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y="Manual counts")+
  labs(title="Microcystis F210")+
  theme(plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        plot.margin=unit(c(1,1,1,1),"cm"))+
  geom_abline(slope=1,intercept=0,color="red",size=2,alpha=0.4)+
  geom_abline(slope=coef(data.lm2)[["Script"]],
              intercept=coef(data.lm2)[["(Intercept)"]],
              linetype=2,
              size=2)+
  geom_point(size=5,shape=22,colour="black",fill="blue",stroke=2)
F210

data.lm3<-lm(Visual~Script,data=RA_Anabena)
summary(data.lm3)

Anabena<-ggplot(data=RA_Anabena, aes(x=Script,y=Visual))+
  theme_classic()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y="Manual counts")+
  labs(title="Anabena sp.")+
  theme(plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        plot.margin=unit(c(1,1,1,1),"cm"))+
  geom_abline(slope=1,intercept=0,color="red",size=2,alpha=0.4)+
  geom_abline(slope=coef(data.lm3)[["Script"]],
              intercept=coef(data.lm3)[["(Intercept)"]],
              linetype=2,
              size=2)+
  geom_point(size=5,shape=23,colour="black",fill="orange",stroke=2)
Anabena

data.lm4<-lm(Visual~Script,data=RA_Shewanella)
summary(data.lm4)

Shewanella<-ggplot(data=RA_Shewanella, aes(x=Script,y=Visual))+
  theme_classic()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y="Manual counts")+
  labs(title="Shewanella IRI-160")+
  theme(plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        plot.margin=unit(c(1,1,1,1),"cm"))+
  geom_abline(slope=1,intercept=0,color="red",size=2,alpha=0.4)+
  geom_abline(slope=coef(data.lm4)[["Script"]],
              intercept=coef(data.lm4)[["(Intercept)"]],
              linetype=2,
              size=2)+
  geom_point(size=5,shape=24,colour="black",fill="purple",stroke=2)
Shewanella


set.seed(1)
stats<-data.frame(Species=rep(c('  Anabena sp.  ','  Microcystis F192  ',
                                '  Microcystis F210  ','  Shewanella IRI-160  ',
                                '  E. coli  ')),
                  R2=rep(c('  0.99  ','  0.99  ','  0.85  ','  0.77  ','NA')),
                  F.value=rep(c('  19.87  ','  233.1  ','  24.46  ','  13.35  ','NA')),
                  p.value=rep(c('  <0.001  ','  <0.001  ','  <0.001  ','  <0.001  ','NA')))
stat.table<-ggtexttable(stats,rows=NULL,
                        theme=ttheme("mOrange",base_size = 18))

tiff("RA_IND_Analysis.tiff", height = 32, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
grid.arrange(Anabena,F192,F210,stat.table,Shewanella,
             ncol=2,nrow=4,
             layout_matrix=rbind(c(1,4),
                                 c(2,3),
                                 c(5,NA),
                                 c(NA,NA)))
dev.off()
