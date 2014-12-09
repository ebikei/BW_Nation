x<-c('dplyr','data.table','zoo')
lapply(x, require, character.only=T)

#drive=c("F:\\")
#drive=c("C:\\Users\\Keita\\Google Drive\\Research\\")
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
#setwd("C:\\Users\\Keita\\Google Drive\\Research\\BW_PMCoarse\\Data")
#setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
#output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
#output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')
output_location=c('/home/bst/other/kebisu/BW_PMCoarse/Result/')

load('AirData_20141206.Rdata') #AirData
PMCoarse_Data=filter(AirData,!is.na(PMCoarse))
rm(AirData)

#Select Monitors which have more than 1 yr observation period and 120 observations.
#Also exposure period is 1997-2007 given available data (1998-2007)

PMCoarse_Data$n=sequence(rle(PMCoarse_Data$FIPS_County)$lengths)
First_Date=data.frame(PMCoarse_Data[!duplicated(PMCoarse_Data$FIPS_County),list(FIPS_County,Date.Local)])
names(First_Date)[2]='FirstObsDate'

Last_Date=data.frame(PMCoarse_Data[!duplicated(PMCoarse_Data$FIPS_County,fromLast=TRUE),list(FIPS_County,Date.Local,n)])
names(Last_Date)[2]='LastObsDate'

Obs_List=merge(First_Date,Last_Date,by='FIPS_County') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate)) %>%
		filter(n>=120&PeriodLength>=365&FirstObsDate<'2008-01-01') %>%
		select(FIPS_County)
#table(substr(Obs_List$FIPS_County,1,2))
PMCoarse_Data2=merge(PMCoarse_Data,Obs_List,by='FIPS_County',all.y=TRUE) %>%
		filter(Date.Local<'2008-01-01')
setkey(PMCoarse_Data2,FIPS_County,Date.Local)

PMCoarse_Data3=PMCoarse_Data2[CJ(unique(FIPS_County),seq(min(Date.Local),max(Date.Local),by=1))]
PMCoarse_Data3[,PMCoarse_Week:=rollapply(PMCoarse,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=FIPS_County]
PMCoarse_Data3$PMCoarse_Week[is.nan(PMCoarse_Data3$PMCoarse_Week)]=NA
PMCoarse_Data3$CO<-PMCoarse_Data3$NO2<-PMCoarse_Data3$SO2<-PMCoarse_Data3$PM10<-PMCoarse_Data3$PM25<-PMCoarse_Data3$n<-PMCoarse_Data3$PMCoarse<-NULL
rm(list=setdiff(ls(), c('PMCoarse_Data3')))

BirthList=data.table(read.csv('BirthOutcomeFips_Gest_Combination.csv'))
BirthList$FIPS_County=sprintf("%05d",BirthList$res_fips)
BirthList$dgestat=NULL
BirthList$res_fips=NULL
setkey(BirthList,FIPS_County)

CountyList=PMCoarse_Data3[!duplicated(PMCoarse_Data3$FIPS_County),list(FIPS_County)]
BirthList2=merge(BirthList,CountyList,by='FIPS_County',all.y=TRUE)
rm(BirthList)

BirthList2$temp=as.Date(paste(substr(BirthList2$LMPDate,1,4),substr(BirthList2$LMPDate,5,6),substr(BirthList2$LMPDate,7,8),sep='-'),"%Y-%m-%d")

DataMatrix=matrix(0,nrow=dim(BirthList2)[1],ncol=50)

#i=1
for (i in 1:50){
BirthList3=mutate(BirthList2,temp2=temp+i*7-1)
setkey(BirthList3,FIPS_County,temp2)
test=PMCoarse_Data3[BirthList3]
DataMatrix[,i]=test$PMCoarse_Week
rm(BirthList3,test)
}

BirthList4=cbind(data.frame(BirthList2),data.frame(DataMatrix))
names=paste(rep('Week',50),1:50,sep='')
colnames(BirthList4)[4:53]=names

for (i in 30:50){
BirthList4[[paste0('GestWeek_',i)]]=rowMeans(BirthList4[,c(4:i)],na.rm=TRUE)
}



rm(list=ls())
