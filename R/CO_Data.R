x<-c("dplyr","ggplot2",'data.table')#,"data.table")
lapply(x, require, character.only=T)
drive=c("E:\\")
#temp=c("C:\\Users\\ke52\\Downloads")
#temp=c("H:\\")
temp=c("C:\\Users\\Keita\\Desktop\\Download")
setwd(temp)


######################
############Download AQS SIte
######################
countylist=read.csv('E:\\BW_PMCoarse\\Data\\CountyList.csv')
names(countylist)=c('index','FP')
countylist$FIPSList=sprintf("%05d",countylist$FP)
COCT_AQS=data.frame(matrix(nrow=0,ncol=0))
test2=c(1998:2007)

ptm <- proc.time()
for (i in 1:length(test2)){  #Somehow 1993 to 1997 does not work unzipping; file failure?
	#url=paste("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_42602_",test[i],".zip",sep='')
	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_42101_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_42101_",test2[i],".csv",sep='')),header=TRUE)
	names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name','SampleDuration',
		'PollutantStandard','Date','Unit','EventType','ObsCount','ObsPercent','Value','MaxValue','MaxHour','AQI','MethodCode',
		'MethodName','SiteName','Address','StateName','CountyName','CityName','CBSAName','DateChange')
	temp=temp[temp$ObsCount >= 16,]
	temp$StateCode=as.numeric(as.character(temp$StateCode))
	temp$CountyCode=as.numeric(as.character(temp$CountyCode))
	temp$SiteID=as.numeric(as.character(temp$SiteID))
	temp$Date=as.Date(as.character(temp$Date),format="%Y-%m-%d")
	temp2=temp[,c(1:7,12,13,15,17,20,25:27)]
	temp2=filter(temp,!is.na(StateCode))
	temp2$FIPS_C=paste(sprintf("%02d",temp2$StateCode),sprintf("%03d",temp2$CountyCode),sep='')
	COCT_AQS=rbind(COCT_AQS,temp2)
	rm(url,temp,temp2,temp2_2)
}
proc.time() - ptm #This takes about 5min

CO_Data=COCT_AQS
Outside_main=c('02','15','66','72','78','80')
CO_Data=CO_Data[!(CO_Data$StateCode %in% Outside_main),]
CO_Cut=quantile(CO_Data$Value,0.99)
CO_Data=CO_Data %>%
	filter(Value>=0&Value<CO_Cut) %>%
	mutate(POC=sprintf("%02d",POC),StateCode=sprintf("%02d",StateCode),CountyCode=sprintf("%03d",CountyCode),SiteID=sprintf("%04d",SiteID)) 
CO_Data$FIPS_C[CO_Data$FIPS_C=='12086']='12025'
CO_Data=mutate(CO_Data,FIPS_Monitor=paste(StateCode,CountyCode,SiteID,sep=''),FIPSPOC=paste(StateCode,CountyCode,SiteID,POC,sep=''))
CO_Data_2=data.table(CO_Data)
setkey(CO_Data_2,FIPS_Monitor,Date)


#Take Avarage by POC This is because someties there are multiple observations on same day same monitor
#i.e. There are two types of Sample Duration. (24 ave and 1hour), both are similar, and decided taking average
CO_Data_3=data.table(CO_Data_2[,list(CO_Value=mean(Value)),by=c("FIPS_Monitor","Date","POC")])
#Take average by Monitor
CO_Data_4=data.table(CO_Data_3[,list(CO_Level=mean(CO_Value)),by=c("FIPS_Monitor","Date")]) %>% 
	mutate(FIPS_County=substr(FIPS_Monitor,1,5)) %>%
	select(FIPS_County,FIPS_Monitor,Date,CO_Level)

#Select Monitors which have more than 1 yr observation period and 120 observations.
CO_Data_4$n=sequence(rle(CO_Data_4$FIPS_Monitor)$lengths)
First_Date=data.frame(CO_Data_4[!duplicated(CO_Data_4$FIPS_Monitor),list(FIPS_Monitor,Date)])
names(First_Date)[2]='FirstObsDate'

Last_Date=data.frame(CO_Data_4[!duplicated(CO_Data_4$FIPS_Monitor,fromLast=TRUE),list(FIPS_Monitor,Date,n)])
names(Last_Date)[2]='LastObsDate'

Obs_List=merge(First_Date,Last_Date,by='FIPS_Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate)) %>%
		filter(n>=120&PeriodLength>=365) %>%
		select(FIPS_Monitor)

CO_Data=filter(CO_Data_4,FIPS_Monitor %in% Obs_List$FIPS_Monitor) %>%
	data.frame() %>%
	select(-n,-FIPS_County)

save(CO_Data,file=paste(drive,"BW_PMCoarse\\Data\\CO_Data.RData",sep=''))


##Extract Monitor Location
test=CO_Data_2[!duplicated(CO_Data_2$FIPS_Monitor),list(FIPS_Monitor,Latitude,Longitude,StateName,CountyName,CityName)]
CO_MonLoc=filter(test,FIPS_Monitor %in% Obs_List$FIPS_Monitor) %>%
	data.frame()
save(CO_MonLoc,file=paste(drive,"BW_PMCoarse\\Data\\CO_MonLoc.RData",sep=''))

rm(list=ls())
