library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_main/data")

RA<-read.csv("Regression_Analysis_IND.csv",header=T)

RA_F192<-subset(RA,Species=="F192")
RA_F210<-subset(RA,Species=="F210")
RA_Anabena<-subset(RA,Species=="Anabena")
RA_Shewanella<-subset(RA,Species=="Shewanella")

data.lm1<-lm(Visual~Script,data=RA_F192)
summary(data.lm1)

F192<-ggplot(data=RA_F192, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y=expression(paste("Manual counts")))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))+
  scale_x_continuous(labels=function(x) format(x,scientific=FALSE))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  xlim(0,500)+
  ylim(0,500)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm1)[["Script"]],
              intercept=coef(data.lm1)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=21,colour="black",fill="grey60",stroke=1.5)
F192

data.lm2<-lm(Visual~Script,data=RA_F210)
summary(data.lm2)

F210<-ggplot(data=RA_F210, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y=expression(paste("Manual counts")))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))+
  scale_x_continuous(labels=function(x) format(x,scientific=FALSE))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  xlim(0,500)+
  ylim(0,500)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm2)[["Script"]],
              intercept=coef(data.lm2)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=22,colour="black",fill="grey60",stroke=1.5)
F210

data.lm3<-lm(Visual~Script,data=RA_Anabena)
summary(data.lm3)

Anabena<-ggplot(data=RA_Anabena, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y=expression(paste("Manual counts")))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))+
  scale_x_continuous(labels=function(x) format(x,scientific=FALSE))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  xlim(0,500)+
  ylim(0,500)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm1)[["Script"]],
              intercept=coef(data.lm1)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=23,colour="black",fill="grey60",stroke=1.5)
Anabena

data.lm4<-lm(Visual~Script,data=RA_Shewanella)
summary(data.lm4)

Shewanella<-ggplot(data=RA_Shewanella, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results")))+
  labs(y=expression(paste("Manual counts")))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))+
  scale_x_continuous(labels=function(x) format(x,scientific=FALSE))+
  scale_y_continuous(labels=function(x) format(x,scientific=FALSE))+
  xlim(0,1000)+
  ylim(0,1000)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm1)[["Script"]],
              intercept=coef(data.lm1)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=24,colour="black",fill="grey60",stroke=1.5)
Shewanella



tiff("RA_IND_Analysis.tiff", height = 32, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
plot_grid(Anabena,F192,F210,Shewanella, nrow = 2, labels = "AUTO",label_size = 25)
dev.off()
