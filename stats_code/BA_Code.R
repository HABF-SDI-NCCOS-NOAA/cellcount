library(ggplot2)
library(gridExtra)
library(ggpubr)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/cellcount_data/stat_code/")

BA<-read.csv("Boxplot_Analysis.csv",header=T)

BA_F192<-subset(BA,Species=="F192")
BA_F210<-subset(BA,Species=="F210")
BA_Anabena<-subset(BA,Species=="Anabena")
BA_Anabena$Concentration_f = factor(BA_Anabena$Concentration, levels=c('High','Medium','Low'))
BA_Shewanella<-subset(BA,Species=="Shewanella")


F192<-ggplot(data=BA_F192, aes(x=Concentration,y=Output,color=Method))+
  geom_boxplot(size=1)+
  facet_wrap(.~Concentration,nrow=1,ncol=5,scales="free")+
  theme_classic()+
  labs(y="Manual counts")+
  labs(title="Microcystis F192")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(plot.title = element_text(face="bold.italic",size=30,hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"))
F192  

F210<-ggplot(data=BA_F210, aes(x=Concentration,y=Output,color=Method))+
  geom_boxplot(size=1)+
  facet_wrap(.~Concentration,nrow=1,ncol=5,scales="free")+
  theme_classic()+
  labs(y="Manual counts")+
  labs(title="Microcystis F210")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(plot.title = element_text(face="bold.italic",size=30,hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"))
F210

Anabena<-ggplot(data=BA_Anabena, aes(x=Concentration,y=Output,color=Method))+
  geom_boxplot(size=1)+
  facet_wrap(.~Concentration_f,nrow=1,ncol=5,scales="free")+
  theme_classic()+
  labs(y="Manual counts")+
  labs(title="Anabena sp.")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme(plot.title = element_text(face="bold.italic",size=30,hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"))
Anabena

Shewanella<-ggplot(data=BA_Shewanella, aes(x=Concentration,y=Output,color=Method))+
  geom_boxplot(size=1)+
  facet_wrap(.~Concentration,nrow=1,ncol=5,scales="free")+
  theme_classic()+
  labs(y="Manual counts")+
  labs(title="Shewanella IRI-160")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  theme(plot.title = element_text(face="bold.italic",size=30,hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13,vjust=2))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold",size = 12,angle=30),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(1,1,1,1),"cm"))
Shewanella

tiff("BA_Analysis.tiff", height = 35, width = 25, units = 'cm', 
     compression = "lzw", res = 300)
grid.arrange(F192,F210,Anabena,Shewanella,
             ncol=1,nrow=4,
             layout_matrix=rbind(c(1),
                                 c(2),
                                 c(3),
                                 c(4)))
dev.off()
