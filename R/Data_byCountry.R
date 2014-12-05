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

State_FIPS=read.csv('state_geocodes.csv')
names(State_FIPS)[5:6]=c('RegionName','DivisionName')
State_FIPS$State.Code=sprintf("%02d",State_FIPS$St_FIPS)
State_FIPS=State_FIPS[order(State_FIPS$State.Code),]
State_Table=data.table(State_FIPS[,c('Name','State.Code','Region','RegionName','Division','DivisionName')])

load('COData.RData')#CO_data2
#table(CO_data2$Event.Type)
CO_data2=CO_data2[CO_data2$Event.Type=='None',]
InclVar1=names(CO_data2) %in% c('State.Code','County.Code','Site.Num','POC','Latitude','Longitude','Date.Local','CO_Value')
CO_data3=CO_data2[InclVar1]
Outside_main=c('02','15','72','78','80')
CO_data4=CO_data3[!(CO_data3$State.Code %in% Outside_main),]
rm(CO_data2,CO_data3)
summary(CO_data4$CO_Value)
quantile(CO_data4$CO_Value,c(0.9,0.95,0.97,0.98,0.99))
CO_Cut=quantile(CO_data4$CO_Value,0.99)
CO_data4=filter(CO_data4,CO_Value>=0&CO_Value<CO_Cut)
CO_data4=mutate(CO_data4,POC=sprintf("%02d",CO_data4$POC))
CO_data5=mutate(CO_data4,FIPS=paste(CO_data4$State.Code,CO_data4$County.Code,CO_data4$Site.Num,sep=''),
		FIPSPOC=paste(CO_data4$State.Code,CO_data4$County.Code,CO_data4$Site.Num,CO_data4$POC,sep='')
		) %>% select(.,FIPS,Date.Local,CO_Value,POC,Latitude,Longitude) 
CO_data6=data.table(CO_data5)
setkey(CO_data,Date.Local,FIPS)


CO_data5=left_join(CO_data4,State_FIPS,by='State.Code')


CO_DayAverage=CO_data5 %>% group_by(Date.Local) %>% summarise(mean(CO_Value))
names(CO_DayAverage)[2]='CO_Value'
CO_DayAverage=mutate(CO_DayAverage,
	Year_f=as.factor(substr(CO_DayAverage$Date.Local,1,4)),
	MonthDay_num=as.numeric(format(CO_DayAverage$Date.Local,'%j')),
	MonthName=factor(months(CO_DayAverage$Date.Local,abbreviate=TRUE)))
