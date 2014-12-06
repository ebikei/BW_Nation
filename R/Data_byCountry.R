x<-c('dplyr','data.table')
lapply(x, require, character.only=T)

#drive=c("F:\\")
#drive=c("C:\\Users\\Keita\\Google Drive\\Research\\")
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
#setwd("C:\\Users\\Keita\\Google Drive\\Research\\BW_PMCoarse\\Data")
#setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
#output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
#output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')
output_location=c('/home/bst/other/kebisu/BW_PMCoarse/Result/')

#############
#CO Data
#############
load('COData.RData')#CO_data2
#table(CO_data2$Event.Type)
CO_data2=filter(CO_data2,Event.Type=='None')
InclVar1=names(CO_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','CO_Value')
CO_data3=CO_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
CO_data4=CO_data3[!(CO_data3$State.Code %in% Outside_main),]
#summary(CO_data4$CO_Value)
#quantile(CO_data4$CO_Value,c(0.9,0.95,0.97,0.98,0.99))
CO_Cut=quantile(CO_data4$CO_Value,0.99)
CO_data4=filter(CO_data4,CO_Value>=0&CO_Value<CO_Cut)
CO_data4=mutate(CO_data4,POC=sprintf("%02d",CO_data4$POC))
CO_data5=mutate(CO_data4,FIPS=paste(CO_data4$State.Code,CO_data4$County.Code,CO_data4$Site.Num,sep=''),
		FIPSPOC=paste(CO_data4$State.Code,CO_data4$County.Code,CO_data4$Site.Num,CO_data4$POC,sep='')) %>% 
		select(FIPS,Date.Local,CO_Value,POC,Latitude,Longitude) 
CO_data6=data.table(CO_data5)
setkey(CO_data6,FIPS,Date.Local)
#Take average by FIPS
CO_data7=data.table(CO_data6[,list(CO_level=mean(CO_Value)),by=c("FIPS","Date.Local")]) %>% mutate(FIPS_County=substr(FIPS,1,5))
setkey(CO_data7,FIPS_County,Date.Local)
#Take Average by county
CO_data=CO_data7[,list(CO=mean(CO_level)),by=c("FIPS_County","Date.Local")]
rm(list=setdiff(ls(), c('CO_data')))
 
#############
#NO2 Data
############# 
load('NO2Data.RData')#NO2_data2
#table(NO2_data2$Event.Type)
NO2_data2=filter(NO2_data2,Event.Type=='None')
InclVar1=names(NO2_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','NO2_Value')
NO2_data3=NO2_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
NO2_data4=NO2_data3[!(NO2_data3$State.Code %in% Outside_main),]
#summary(NO2_data4$NO2_Value)
#quantile(NO2_data4$NO2_Value,c(0.9,0.95,0.97,0.98,0.99))
NO2_Cut=quantile(NO2_data4$NO2_Value,0.99)
NO2_data4=filter(NO2_data4,NO2_Value>=0&NO2_Value<NO2_Cut)
NO2_data4=mutate(NO2_data4,POC=sprintf("%02d",NO2_data4$POC))
NO2_data5=mutate(NO2_data4,FIPS=paste(NO2_data4$State.Code,NO2_data4$County.Code,NO2_data4$Site.Num,sep=''),
		FIPSPOC=paste(NO2_data4$State.Code,NO2_data4$County.Code,NO2_data4$Site.Num,NO2_data4$POC,sep='')) %>% 
		select(FIPS,Date.Local,NO2_Value,POC,Latitude,Longitude) 
NO2_data6=data.table(NO2_data5)
setkey(NO2_data6,FIPS,Date.Local)
#Take average by FIPS
NO2_data7=data.table(NO2_data6[,list(NO2_level=mean(NO2_Value)),by=c("FIPS","Date.Local")]) %>% mutate(FIPS_County=substr(FIPS,1,5))
setkey(NO2_data7,FIPS_County,Date.Local)
#Take Average by county
NO2_data=NO2_data7[,list(NO2=mean(NO2_level)),by=c("FIPS_County","Date.Local")]
rm(list=setdiff(ls(), c('CO_data','NO2_data')))
 
#############
#SO2 Data
############# 
load('SO2Data.RData')#SO2_data2
#table(SO2_data2$Event.Type)
SO2_data2=filter(SO2_data2,Event.Type=='None')
InclVar1=names(SO2_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','SO2_Value')
SO2_data3=SO2_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
SO2_data4=SO2_data3[!(SO2_data3$State.Code %in% Outside_main),]
#summary(SO2_data4$SO2_Value)
#quantile(SO2_data4$SO2_Value,c(0.9,0.95,0.97,0.98,0.99))
SO2_Cut=quantile(SO2_data4$SO2_Value,0.99)
SO2_data4=filter(SO2_data4,SO2_Value>=0&SO2_Value<SO2_Cut)
SO2_data4=mutate(SO2_data4,POC=sprintf("%02d",SO2_data4$POC))
SO2_data5=mutate(SO2_data4,FIPS=paste(SO2_data4$State.Code,SO2_data4$County.Code,SO2_data4$Site.Num,sep=''),
		FIPSPOC=paste(SO2_data4$State.Code,SO2_data4$County.Code,SO2_data4$Site.Num,SO2_data4$POC,sep='')) %>% 
		select(FIPS,Date.Local,SO2_Value,POC,Latitude,Longitude) 
SO2_data6=data.table(SO2_data5)
setkey(SO2_data6,FIPS,Date.Local)
#Take average by FIPS
SO2_data7=data.table(SO2_data6[,list(SO2_level=mean(SO2_Value)),by=c("FIPS","Date.Local")]) %>% mutate(FIPS_County=substr(FIPS,1,5))
setkey(SO2_data7,FIPS_County,Date.Local)
#Take Average by county
SO2_data=SO2_data7[,list(SO2=mean(SO2_level)),by=c("FIPS_County","Date.Local")]
rm(list=setdiff(ls(), c('CO_data','NO2_data','SO2_data')))
 
#############
#PM10 Data
############# 
load('PM10Data.RData')#PM10_data2
#table(PM10_data2$Event.Type)
PM10_data2=filter(PM10_data2,Event.Type=='None')
InclVar1=names(PM10_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','PM10_Value')
PM10_data3=PM10_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
PM10_data4=PM10_data3[!(PM10_data3$State.Code %in% Outside_main),]
#summary(PM10_data4$PM10_Value)
#quantile(PM10_data4$PM10_Value,c(0.9,0.95,0.97,0.98,0.99))
PM10_Cut=quantile(PM10_data4$PM10_Value,0.99)
PM10_data4=filter(PM10_data4,PM10_Value>=0&PM10_Value<PM10_Cut)
PM10_data4=mutate(PM10_data4,POC=sprintf("%02d",PM10_data4$POC))
PM10_data5=mutate(PM10_data4,FIPS=paste(PM10_data4$State.Code,PM10_data4$County.Code,PM10_data4$Site.Num,sep=''),
		FIPSPOC=paste(PM10_data4$State.Code,PM10_data4$County.Code,PM10_data4$Site.Num,PM10_data4$POC,sep='')) %>% 
		select(FIPS,Date.Local,PM10_Value,POC,Latitude,Longitude) 
PM10_data6=data.table(PM10_data5)
setkey(PM10_data6,FIPS,Date.Local)
#Take average by FIPS
PM10_data7=data.table(PM10_data6[,list(PM10_level=mean(PM10_Value)),by=c("FIPS","Date.Local")]) %>% mutate(FIPS_County=substr(FIPS,1,5))
setkey(PM10_data7,FIPS_County,Date.Local)
#Take Average by county
PM10_data=PM10_data7[,list(PM10=mean(PM10_level)),by=c("FIPS_County","Date.Local")]
rm(list=setdiff(ls(), c('CO_data','NO2_data','SO2_data','PM10_data','PM10_data6')))

#############
#PM25 Data
############# 
load('PM25Data.RData')#PM25_data2
#table(PM25_data2$Event.Type)
PM25_data2=filter(PM25_data2,Event.Type=='None')
InclVar1=names(PM25_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','PM25_Value')
PM25_data3=PM25_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
PM25_data4=PM25_data3[!(PM25_data3$State.Code %in% Outside_main),]
#summary(PM25_data4$PM25_Value)
#quantile(PM25_data4$PM25_Value,c(0.9,0.95,0.97,0.98,0.99))
PM25_Cut=quantile(PM25_data4$PM25_Value,0.99)
PM25_data4=filter(PM25_data4,PM25_Value>=0&PM25_Value<PM25_Cut)
PM25_data4=mutate(PM25_data4,POC=sprintf("%02d",PM25_data4$POC))
PM25_data5=mutate(PM25_data4,FIPS=paste(PM25_data4$State.Code,PM25_data4$County.Code,PM25_data4$Site.Num,sep=''),
		FIPSPOC=paste(PM25_data4$State.Code,PM25_data4$County.Code,PM25_data4$Site.Num,PM25_data4$POC,sep='')) %>% 
		select(FIPS,Date.Local,PM25_Value,POC,Latitude,Longitude) 
PM25_data6=data.table(PM25_data5)
setkey(PM25_data6,FIPS,Date.Local)
#Take average by FIPS
PM25_data7=data.table(PM25_data6[,list(PM25_level=mean(PM25_Value)),by=c("FIPS","Date.Local")]) %>% mutate(FIPS_County=substr(FIPS,1,5))
setkey(PM25_data7,FIPS_County,Date.Local)
#Take Average by county
PM25_data=PM25_data7[,list(PM25=mean(PM25_level)),by=c("FIPS_County","Date.Local")]
rm(list=setdiff(ls(), c('CO_data','NO2_data','SO2_data','PM25_data','PM25_data6','PM10_data','PM10_data6')))

#############
#PMCoarse Data
#This is different from what I created in GIS or trend map.
############# 

PMCoarse_data2=merge(PM10_data6,PM25_data6,by=c('FIPS','Date.Local')) %>% 
	mutate(PMCoarse_level=PM10_Value-PM25_Value) %>%
	select(FIPS,Date.Local,PMCoarse_level) %>%
	filter(PMCoarse_level>=0) %>%
	mutate(FIPS_County=substr(FIPS,1,5))
setkey(PMCoarse_data2,FIPS_County,Date.Local)
PMCoarse_data=PMCoarse_data2[,list(PMCoarse=mean(PMCoarse_level)),by=c("FIPS_County","Date.Local")]
rm(PM25_data6,PM10_data6,PMCoarse_data2)

#############
#Merge All data
############# 
ptm=proc.time()
AirData=merge(CO_data,NO2_data,by=c('FIPS_County','Date.Local'),all=TRUE) %>%
	merge(.,SO2_data,by=c('FIPS_County','Date.Local'),all=TRUE) %>%
	merge(.,PM10_data,by=c('FIPS_County','Date.Local'),all=TRUE) %>%
	merge(.,PM25_data,by=c('FIPS_County','Date.Local'),all=TRUE) %>%
	merge(.,PMCoarse_data,by=c('FIPS_County','Date.Local'),all=TRUE)
proc.time()-ptm
save(AirData,file='AirData_20141206.Rdata')

rm(list=ls())
