library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_main/data")

RA<-read.csv("Regression_Analysis.csv",header=T)

RA_F192<-subset(RA,Species=="F192")
RA_F210<-subset(RA,Species=="F210")
RA_Anabena<-subset(RA,Species=="Anabena")
RA_Shewanella<-subset(RA,Species=="Shewanella")

RA_F192$Script<-(RA_F192$Script/1000000)
RA_F192$Visual<-(RA_F192$Visual/1000000)
RA_F192$Microscope<-(RA_F192$Microscope/1000000)

data.lm1<-lm(Visual~Script,data=RA_F192)
summary(data.lm1)

F192<-ggplot(data=RA_F192, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm1)[["Script"]],
              intercept=coef(data.lm1)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=21,colour="black",fill="grey60",stroke=1.5)
F192


RA_F210$Script<-(RA_F210$Script/1000000)
RA_F210$Visual<-(RA_F210$Visual/1000000)
RA_F210$Microscope<-(RA_F210$Microscope/1000000)

data.lm2<-lm(Visual~Script,data=RA_F210)
summary(data.lm2)

F210<-ggplot(data=RA_F210, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm2)[["Script"]],
              intercept=coef(data.lm2)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=22,colour="black",fill="grey60",stroke=1.5)
F210


RA_Anabena$Script<-(RA_Anabena$Script/1000000)
RA_Anabena$Visual<-(RA_Anabena$Visual/1000000)
RA_Anabena$Microscope<-(RA_Anabena$Microscope/1000000)

data.lm3<-lm(Visual~Script,data=RA_Anabena)
summary(data.lm3)

Anabena<-ggplot(data=RA_Anabena, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm3)[["Script"]],
              intercept=coef(data.lm3)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=23,colour="black",fill="grey60",stroke=1.5)
Anabena


RA_Shewanella$Script<-(RA_Shewanella$Script/10000000000)
RA_Shewanella$Visual<-(RA_Shewanella$Visual/10000000000)

data.lm4<-lm(Visual~Script,data=RA_Shewanella)
summary(data.lm4)

Shewanella<-ggplot(data=RA_Shewanella, aes(x=Script,y=Visual))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{10},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{10},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm4)[["Script"]],
              intercept=coef(data.lm4)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=24,colour="black",fill="grey60",stroke=1.5)
Shewanella



tiff("RA_Analysis1.tiff", height = 32, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
plot_grid(Anabena,F192,F210,Shewanella, nrow = 2, labels = "AUTO",label_size = 25)
dev.off()





data.lm5<-lm(Microscope~Script,data=RA_F192)
summary(data.lm5)

F192_M<-ggplot(data=RA_F192, aes(x=Script,y=Microscope))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm5)[["Script"]],
              intercept=coef(data.lm5)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=21,colour="black",fill="grey60",stroke=1.5)
F192_M

data.lm6<-lm(Microscope~Script,data=RA_F210)
summary(data.lm6)

F210_M<-ggplot(data=RA_F210, aes(x=Script,y=Microscope))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm6)[["Script"]],
              intercept=coef(data.lm6)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=22,colour="black",fill="grey60",stroke=1.5)
F210_M

data.lm7<-lm(Microscope~Script,data=RA_Anabena)
summary(data.lm7)

Anabena_M<-ggplot(data=RA_Anabena, aes(x=Script,y=Microscope))+
  theme_bw()+
  labs(x=expression(paste(italic("cellcount"), " results (",x10^{6},")")))+
  labs(y=expression(paste("Manual counts ( ",x10^{6},")")))+
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
  xlim(0,5)+
  ylim(0,5)+
  geom_abline(slope=1,intercept=0,color="red",size=1,alpha=0.4)+
  geom_abline(slope=coef(data.lm7)[["Script"]],
              intercept=coef(data.lm7)[["(Intercept)"]],
              linetype=2,
              size=1)+
  geom_point(size=3,shape=23,colour="black",fill="grey60",stroke=1.5)
Anabena_M



tiff("RA_Analysis2.tiff", height = 30, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
plot_grid(Anabena_M,F192_M,F210_M, nrow = 2, labels = "AUTO",label_size = 25)
dev.off()
