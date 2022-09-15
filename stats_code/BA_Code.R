library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_main/data")

BA<-read.csv("Boxplot_Analysis.csv",header=T)

BA_F192<-subset(BA,Species=="F192")
BA_F210<-subset(BA,Species=="F210")
BA_Anabena<-subset(BA,Species=="Anabena")
BA_Anabena$Concentration_f = factor(BA_Anabena$Concentration, levels=c('High','Medium','Low'))
BA_Shewanella<-subset(BA,Species=="Shewanella")


BA_F192$Output<-(BA_F192$Output/1000000)
BA_F192$Output<-(sqrt(BA_F192$Output))

F192<-ggplot(data=BA_F192, aes(x=Concentration,y=Output,color=Method))+
  scale_colour_grey()+
  stat_boxplot(geom='errorbar')+
  geom_boxplot(size=0.5)+
  theme_bw()+
  labs(x="Concentration")+
  labs(y=expression(paste("Manual counts ( ",x10^{6},") (sqrt transformed)")))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))
F192  

BA_F210$Output<-(BA_F210$Output/1000000)
BA_F210$Output<-(sqrt(BA_F210$Output))

F210<-ggplot(data=BA_F210, aes(x=Concentration,y=Output,color=Method))+
  scale_colour_grey()+
  stat_boxplot(geom='errorbar')+
  geom_boxplot(size=0.5)+
  theme_bw()+
  labs(x="Concentration")+
  labs(y=expression(paste("Manual counts ( ",x10^{6},") (sqrt transformed)")))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))
F210

BA_Anabena$Output<-(BA_Anabena$Output/1000000)
BA_Anabena$Output<-(sqrt(BA_Anabena$Output))

Anabena<-ggplot(data=BA_Anabena, aes(x=Concentration_f,y=Output,color=Method))+
  scale_colour_grey()+
  stat_boxplot(geom='errorbar')+
  geom_boxplot(size=0.5)+
  theme_bw()+
  labs(x="Concentration")+
  labs(y=expression(paste("Manual counts ( ",x10^{6},") (sqrt transformed)")))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))
Anabena


BA_Shewanella$Output<-(BA_Shewanella$Output/10000000000)
BA_Shewanella$Output<-(sqrt(BA_Shewanella$Output))

Shewanella<-ggplot(data=BA_Shewanella, aes(x=Concentration,y=Output,color=Method))+
  scale_colour_grey()+
  stat_boxplot(geom='errorbar')+
  geom_boxplot(size=0.5)+
  theme_bw()+
  labs(x="Concentration")+
  labs(y=expression(paste("Manual counts ( ",x10^{10},") (sqrt transformed)")))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_line(size=1.5),
        axis.ticks.length = unit(0.25,"cm"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.5))
Shewanella

tiff("BA_Analysis.tiff", height = 25, width = 35, units = 'cm', 
     compression = "lzw", res = 300)
plot_grid(Anabena,F192,F210,Shewanella, nrow = 2, ncol = 2, labels = "AUTO",label_size = 25)
dev.off()
