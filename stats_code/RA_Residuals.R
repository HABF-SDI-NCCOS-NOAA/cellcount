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
RA_F192$predicted<-predict(data.lm1)
RA_F192$residuals<-residuals(data.lm1)

resid_F192<-ggplot(RA_F192,aes(x=Script,y=Visual))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted),alpha=0.2)+
  geom_point(aes(color=abs(residuals),size=abs(residuals)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted),shape=1)+
  theme_classic()+
  labs(title="Microcystis F192")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_F192

data.lm2<-lm(Visual~Script,data=RA_F210)
RA_F210$predicted<-predict(data.lm2)
RA_F210$residuals<-residuals(data.lm2)

resid_F210<-ggplot(RA_F210,aes(x=Script,y=Visual))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted),alpha=0.2)+
  geom_point(aes(color=abs(residuals),size=abs(residuals)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted),shape=1)+
  theme_classic()+
  labs(title="Microcystis F210")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_F210

data.lm3<-lm(Visual~Script,data=RA_Anabena)
RA_Anabena$predicted<-predict(data.lm3)
RA_Anabena$residuals<-residuals(data.lm3)

resid_Anabena<-ggplot(RA_Anabena,aes(x=Script,y=Visual))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted),alpha=0.2)+
  geom_point(aes(color=abs(residuals),size=abs(residuals)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted),shape=1)+
  theme_classic()+
  labs(title="Anabena sp.")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_Anabena

data.lm4<-lm(Visual~Script,data=RA_Shewanella)
RA_Shewanella$predicted<-predict(data.lm4)
RA_Shewanella$residuals<-residuals(data.lm4)

resid_Shewanella<-ggplot(RA_Shewanella,aes(x=Script,y=Visual))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted),alpha=0.2)+
  geom_point(aes(color=abs(residuals),size=abs(residuals)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted),shape=1)+
  theme_classic()+
  labs(title="Shewanella IRI-160")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_Shewanella


tiff("RA_IND_Residuals1.tiff", height = 32, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
grid.arrange(resid_F192,resid_F210,resid_Anabena,resid_Shewanella,
             ncol=2,nrow=2,
             layout_matrix=rbind(c(1,2),
                                 c(3,4)))
dev.off()





data.lm5<-lm(Microscope~Script,data=RA_F192)
RA_F192$predicted_M<-predict(data.lm5)
RA_F192$residuals_M<-residuals(data.lm5)

resid_F192_M<-ggplot(RA_F192,aes(x=Script,y=Microscope))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted_M),alpha=0.2)+
  geom_point(aes(color=abs(residuals_M),size=abs(residuals_M)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted_M),shape=1)+
  theme_classic()+
  labs(title="Microcystis F192")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_F192_M

data.lm6<-lm(Microscope~Script,data=RA_F210)
RA_F210$predicted_M<-predict(data.lm6)
RA_F210$residuals_M<-residuals(data.lm6)

resid_F210_M<-ggplot(RA_F210,aes(x=Script,y=Microscope))+
  geom_smooth(method="lm",se=FALSE,color="lightgrey")+
  geom_segment(aes(xend=Script,yend=predicted_M),alpha=0.2)+
  geom_point(aes(color=abs(residuals_M),size=abs(residuals_M)))+
  scale_color_continuous(low="green",high="red")+
  guides(color="none",size="none")+
  geom_point(aes(y=predicted_M),shape=1)+
  theme_classic()+
  labs(title="Microcystis F210")+
  theme(
    plot.title = element_text(face="bold.italic",size=17,hjust=0.5),
    axis.text.x = element_text(face = "bold", 
                               size = 13),
    axis.text.y = element_text(face = "bold", 
                               size = 13),
    axis.title.x = element_text(size=17,vjust=-1),
    axis.title.y = element_text(size=17,vjust=3),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
resid_F210_M


tiff("RA_Residuals2.tiff", height = 30, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
grid.arrange(resid_F192_M,resid_F210_M,
             ncol=2,nrow=1,
             layout_matrix=rbind(c(1,2)))
dev.off()