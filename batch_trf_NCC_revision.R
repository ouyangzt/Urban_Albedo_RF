library(tidyverse) 


setwd("E:/HOME/Albedo/Future_Forcings/")


trf<-expand.grid(SSPs<-c("ssp1","ssp2","ssp3","ssp4","ssp5"),
                 Periods<-c(30,40,50,60,70,80,90,100),
                 RCPs<-c("rcp26","rcp45","rcp85","No_Change"))%>%
  data.frame()%>%mutate(trf_m=0,trf_l=0,trf_u=0)%>%rename(SSPs=Var1,Periods=Var2,RCPs=Var3)


#######Fuctions for normalizing land cover data
convert_to_fraction<-function(file)
{
  y=rowSums(file[,3:19])
  for (i in 3:19)
  {
    file[,i]=file[,i]/y
  }
  file
}


###Albedo
Snowfree_BSA_Alb <- readRDS("./Albedo/Snowfree_BSA_Alb.rds")
Snowfree_WSA_Alb <- readRDS("./Albedo/Snowfree_WSA_Alb.rds")
Snowcover_WSA_Alb <- readRDS("./Albedo/Snowcover_WSA_Alb.rds")
Snowcover_BSA_Alb <- readRDS("./Albedo/Snowcover_BSA_Alb.rds")
##radiation
vddsf<-read.csv("./Radiation/vddsf.csv")
vbdsf<-read.csv("./Radiation/vbdsf.csv")
nddsf<-read.csv("./Radiation/nddsf.csv")
nbdsf<-read.csv("./Radiation/nbdsf.csv")

##read kernel
#kernel<-read.csv("./Radiation/kernel.csv")
#kernel=kernel[order(kernel$Id),]

for(k in 1:160)
{
  
  past<-read.csv("./LULC/LULC2018_3.csv")
  #future<-read.csv(paste0("./LULC/",trf[k,1],"_",2000+trf[k,2],"_mosaic_m.csv"))  ##for mean
  #future<-read.csv(paste0("./LULC/",trf[k,1],"_",2000+trf[k,2],"_mosaic_u.csv")) ##for u
  future<-read.csv(paste0("./LULC/",trf[k,1],"_",2000+trf[k,2],"_mosaic_l.csv")) ##for l
  
  m1<-past%>% replace(is.na(.), 0) %>%
    mutate(Total_Area=rowSums(.[3:19]))%>%
    mutate(P_Urban1=VALUE_13/Total_Area)%>%select(ID,Total_Area,P_Urban1)
  m2<-future%>% replace(is.na(.), 0) %>%
    mutate(Total_Area=rowSums(.[3:19]))%>%
    mutate(P_Urban2=VALUE_13/Total_Area)%>%select(ID,P_Urban2)%>%
    right_join(m1,by="ID")%>%select(ID,P_Urban1,P_Urban2,Total_Area)

  past<-convert_to_fraction(past)
  future<-convert_to_fraction(future)
  
  ##########Fraction of White Sky

  WhiteSky<-data.frame(Id=vddsf$Id)
  BlackSky<-data.frame(Id=vddsf$Id)
  for (i in 1:12)
  {
    WhiteSky[,i+1]=(vddsf[,i+3]+nddsf[,i+3])/(vddsf[,i+3]+nddsf[,i+3]+vbdsf[,i+3]+nbdsf[,i+3])
    BlackSky[,i+1]=1-(vddsf[,i+3]+nddsf[,i+3])/(vddsf[,i+3]+nddsf[,i+3]+vbdsf[,i+3]+nbdsf[,i+3])
    
  }
  BlackSky=BlackSky[order(BlackSky$Id),]
  WhiteSky=WhiteSky[order(WhiteSky$Id),]
  ###########Fraction of Snow
  Snowcover1 <- readRDS("./Snow/Snowcover_2018.rds")  ##snow cover for past
  
  if(trf[k,3]=="No_Change")
  {
     Snowcover2=Snowcover1
  
  } else
  {
    Snowcover2 <- readRDS(paste0("./Snow/",trf[k,3],"_snow",trf[k,2],".rds"))  ##snow cover for future
  }
    
  
  Albedo_p<-list()
  
  for (i in 1:17)
  {
    Albedo_p[[i]]<-data.frame(ID=past$ID,Jan=0,Feb=0,Mar=0,Apr=0,May=0,Jun=0,Jul=0,Aug=0,Sep=0,Oct=0,Nov=0,Dec=0)
    for (j in 1:12)
    {
      ##snow free
      BSA0=Snowfree_BSA_Alb[[j]][[i]]$MEAN*BlackSky[j+1]
      
      WSA0=Snowfree_WSA_Alb[[j]][[i]]$MEAN*WhiteSky[j+1]
      
      
      ##snow cover
      
      BSA1=Snowcover_BSA_Alb[[j]][[i]]$MEAN*BlackSky[j+1]
      
      WSA1=Snowcover_WSA_Alb[[j]][[i]]$MEAN*WhiteSky[j+1]
      
      Snow<-data.frame(ID=Albedo_p[[i]]$ID,Snowfree_a=BSA0+WSA0,Snow_a=BSA1+WSA1)
      names(Snow)=c("ID","Snowfree_a","Snow_a")
      
      Snow<-merge(Snow,Snowcover1[[j]],by.x="ID",by.y="ID",all=FALSE,all.x = TRUE)
      alb<-data.frame(ID=Snow$ID,value=Snow$Snow_a*(Snow$MEAN)/100+Snow$Snowfree_a*(1-(Snow$MEAN)/100))
      
      Albedo_p[[i]][j+1]=alb$value
      rm(Snow, alb, BSA0,WSA0,BSA1,WSA1)
    }
  }
  
  
  Albedo_f<-list()
  
  for (i in 1:17)
  {
    Albedo_f[[i]]<-data.frame(ID=future$ID,Jan=0,Feb=0,Mar=0,Apr=0,May=0,Jun=0,Jul=0,Aug=0,Sep=0,Oct=0,Nov=0,Dec=0)
    for (j in 1:12)
    {
      ##snow free
      BSA0=Snowfree_BSA_Alb[[j]][[i]]$MEAN*BlackSky[j+1]
      
      WSA0=Snowfree_WSA_Alb[[j]][[i]]$MEAN*WhiteSky[j+1]
      
      
      ##snow cover
      
      BSA1=Snowcover_BSA_Alb[[j]][[i]]$MEAN*BlackSky[j+1]
      
      WSA1=Snowcover_WSA_Alb[[j]][[i]]$MEAN*WhiteSky[j+1]
      
      Snow<-data.frame(ID=Albedo_f[[i]]$ID,Snowfree_a=BSA0+WSA0,Snow_a=BSA1+WSA1)
      names(Snow)=c("ID","Snowfree_a","Snow_a")
      
      Snow<-merge(Snow,Snowcover2[[j]],by.x="ID",by.y="ID",all=FALSE,all.x = TRUE)
      alb<-data.frame(ID=Snow$ID,value=Snow$Snow_a*(Snow$MEAN)/100+Snow$Snowfree_a*(1-(Snow$MEAN)/100))
      
      Albedo_f[[i]][j+1]=alb$value
      rm(Snow, alb, BSA0,WSA0,BSA1,WSA1)
    }
  }

  ##read kernels
  
  if(trf[k,3]=="No_Change")
  {
    kernal<-read.csv("./Radiation/kernel.csv")
    
  } else
  {
    kernal <- read.csv(paste0("./SW_Kernal/Kernal_",substr(trf[k,3],4,5),"_",trf[k,2],".csv"))  ##snow cover for future
  }
  
  kernal=kernal[order(kernal$Id),]
  
  ###compute contribution from different land covers
  forcings<-data.frame(ID=past$ID,c1=NA,c2=NA,c3=NA,c4=NA,c5=NA,c6=NA,c7=NA,c8=NA
                       ,c9=NA,c10=NA,c11=NA,c12=NA,c13=NA,c14=NA,c15=NA,c16=NA,c17=NA)
  for (i in 1:17)
  {
    if (i==13) next
    forcings[,i+1]=0
    for (j in 1:12)
    { 
      deltap=past[,i+2]-future[,i+2]
      deltaa=Albedo_f[[13]][,j+1]-Albedo_p[[i]][,j+1]
      forcings[,i+1]=forcings[,i+1]+deltap*deltaa*(kernal[,j+3]*(-1))
    }
    forcings[,i+1]=forcings[,i+1]/12
  }  
  

  
  rf2<-forcings%>% replace(is.na(.), 0)%>% mutate(Total_rf=rowSums(.[2:18]))%>%left_join(m2,by="ID")%>%
        select(ID,P_Urban1,P_Urban2,Total_rf,Total_Area)
  rf2<-rf2%>% replace(is.na(.), 0)%>% mutate(Total_rf2=if_else(!(P_Urban2>P_Urban1),0,Total_rf*Total_Area))
  
  global_rf<-sum(rf2$Total_rf2)/5.1/10^14
  #trf[k,4]=global_rf #for mean
  trf[k,5]=global_rf #for l
  #trf[k,6]=global_rf #for u
}
#################################plots###########################
library(ggplot2)
mytheme <- theme_bw() +
  theme(panel.border = element_blank(),
        axis.title=element_text(size=14), axis.text=element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=10),
        strip.background = element_rect(fill='white', colour='white',size=1))

ggplot(data=trf,aes(x=Periods,y=trf_m))+
  geom_ribbon(aes(ymin=trf_u,ymax=trf_l,fill=SSPs),alpha=0.1)+
  geom_line(aes(color=SSPs),size=2)+
  mytheme+facet_wrap(~RCPs,ncol=2)

       