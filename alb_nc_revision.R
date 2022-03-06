setwd("E:/HOME/Albedo/Future_Forcings")
past<-read.csv("./LULC/LULC2018_3.csv")
future<-read.csv("./LULC/ssp1_2100_mosaic_m.csv")

library(tidyverse) 

convert_to_fraction<-function(file)
{
  y=rowSums(file[,3:19])
  for (i in 3:19)
  {
    file[,i]=file[,i]/y
  }
  file
}

m1<-past%>% replace(is.na(.), 0) %>%
  mutate(Total_Area=rowSums(.[3:19]))%>%
  mutate(P_Urban1=VALUE_13/Total_Area)%>%select(ID,Total_Area,P_Urban1)
m2<-future%>% replace(is.na(.), 0) %>%
  mutate(Total_Area=rowSums(.[3:19]))%>%
  mutate(P_Urban2=VALUE_13/Total_Area)%>%select(ID,P_Urban2)%>%
  right_join(m1,by="ID")%>%select(ID,P_Urban1,P_Urban2,Total_Area)

#######Fuctions for normalizing land cover data
rf<-function(past, future){
  
  
  past<-convert_to_fraction(past)
  future<-convert_to_fraction(future)
  
  
  
  ##########Fraction of White Sky
  vddsf<-read.csv("./Radiation/vddsf.csv")
  vbdsf<-read.csv("./Radiation/vbdsf.csv")
  nddsf<-read.csv("./Radiation/nddsf.csv")
  nbdsf<-read.csv("./Radiation/nbdsf.csv")
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
  Snowcover2 <- readRDS("./Snow/rcp26_snow50.rds")  ##snow cover for future
  
  ###Albedo
  Snowfree_BSA_Alb <- readRDS("./Albedo/Snowfree_BSA_Alb.rds")
  Snowfree_WSA_Alb <- readRDS("./Albedo/Snowfree_WSA_Alb.rds")
  Snowcover_WSA_Alb <- readRDS("./Albedo/Snowcover_WSA_Alb.rds")
  Snowcover_BSA_Alb <- readRDS("./Albedo/Snowcover_BSA_Alb.rds")
  
  
  
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
  
  
  
  
  ###compute contribution from different land covers
  alb_change<-data.frame(ID=past$ID,c1=NA,c2=NA,c3=NA,c4=NA,c5=NA,c6=NA,c7=NA,c8=NA
                       ,c9=NA,c10=NA,c11=NA,c12=NA,c13=NA,c14=NA,c15=NA,c16=NA,c17=NA)
  for (i in 1:17)
  {
    if (i==13) next
    alb_change[,i+1]=0
    for (j in 1:12)
    { 
      deltap=past[,i+2]-future[,i+2]
      deltaa=Albedo_f[[13]][,j+1]-Albedo_p[[i]][,j+1]
      alb_change[,i+1]= alb_change[,i+1]+deltap*deltaa
    }
    alb_change[,i+1]=alb_change[,i+1]/12
  }  
  
  alb_change
}

rf1<-rf(past,future)

rf2<-rf1%>% replace(is.na(.), 0)%>%left_join(m2,by="ID")%>% 
  mutate(c1=if_else(!(P_Urban2>P_Urban1),0,c1))%>% 
  mutate(c2=if_else(!(P_Urban2>P_Urban1),0,c2))%>% 
  mutate(c3=if_else(!(P_Urban2>P_Urban1),0,c3))%>% 
  mutate(c4=if_else(!(P_Urban2>P_Urban1),0,c4))%>% 
  mutate(c5=if_else(!(P_Urban2>P_Urban1),0,c5))%>% 
  mutate(c6=if_else(!(P_Urban2>P_Urban1),0,c6))%>% 
  mutate(c7=if_else(!(P_Urban2>P_Urban1),0,c7))%>% 
  mutate(c8=if_else(!(P_Urban2>P_Urban1),0,c8))%>% 
  mutate(c9=if_else(!(P_Urban2>P_Urban1),0,c9))%>% 
  mutate(c10=if_else(!(P_Urban2>P_Urban1),0,c10))%>% 
  mutate(c11=if_else(!(P_Urban2>P_Urban1),0,c11))%>% 
  mutate(c12=if_else(!(P_Urban2>P_Urban1),0,c12))%>% 
  mutate(c14=if_else(!(P_Urban2>P_Urban1),0,c14))%>% 
  mutate(c15=if_else(!(P_Urban2>P_Urban1),0,c15))%>% 
  mutate(c16=if_else(!(P_Urban2>P_Urban1),0,c16))%>% 
  mutate(c17=if_else(!(P_Urban2>P_Urban1),0,c17))


write.csv(rf2,"alb_change_2100_2018_ssp1_rcp26.csv")
#global_rf[1]<-"2001-2018"
#trf[k,4]=global_rf #for mean
#write.csv(global_rf,"bycover_2001.csv")


