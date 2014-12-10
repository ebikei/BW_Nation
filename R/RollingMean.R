
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
PMCoarse_Data$FIPS_County[PMCoarse_Data$FIPS_County=='12086']='12025'
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
PMCoarse_Data3$CO<-PMCoarse_Data3$NO2<-PMCoarse_Data3$SO2<-PMCoarse_Data3$PM10<-PMCoarse_Data3$PM25<-PMCoarse_Data3$n<-NULL
rm(list=setdiff(ls(), c('PMCoarse_Data3')))

BirthList=fread('BirthOutcomeFips_LMP_Combination.csv')
BirthList$FIPS_County=sprintf("%05d",BirthList$res_fips)
BirthList$LMPDate2=as.Date(paste(substr(BirthList$LMPDate,1,4),substr(BirthList$LMPDate,5,6),substr(BirthList$LMPDate,7,8),sep='-'),"%Y-%m-%d")
BirthList$res_fips=NULL
setkey(BirthList,FIPS_County,LMPDate2)

CountyList=PMCoarse_Data3[!duplicated(PMCoarse_Data3$FIPS_County),list(FIPS_County)]
BirthList2=merge(BirthList,CountyList,by='FIPS_County',all.y=TRUE)
rm(BirthList)

DataMatrix=matrix(0,nrow=dim(BirthList2)[1],ncol=50)

#i=1
for (i in 1:50){
BirthList3=mutate(BirthList2,Date.Local=LMPDate2+i*7-1)
setkey(BirthList3,FIPS_County,Date.Local)
test=PMCoarse_Data3[BirthList3]
#df_mg=merge(PMCoarse_Data3,BirthList2,by=c('FIPS_County','Date.Local'),all.y=TRUE)
DataMatrix[,i]=test$PMCoarse_Week
#DataMatrix[,i]=df_mg$PMCoarse_Week
rm(BirthList3,test)
}

BirthList4=cbind(data.frame(BirthList2),data.frame(DataMatrix))
names=paste(rep('Week',50),1:50,sep='')
colnames(BirthList4)[4:53]=names

for (i in 30:50){
BirthList4[[paste0('GestWeek_',i)]]=rowMeans(BirthList4[,c(4:(3+i))],na.rm=TRUE)
}

BirthList4$First_ObsWks=apply(BirthList4,1,function(f) sum(!is.na(f[4:16])))
BirthList4$Second_ObsWks=apply(BirthList4,1,function(f) sum(!is.na(f[17:29])))

BirthList5=filter(BirthList4,First_ObsWks>9,Second_ObsWks>9)
table(substr(BirthList5$FIPS_County,1,2))
table(substr(BirthList5$LMPDate,1,4))

#BirthList5$GestWeek_30=ifelse(apply(BirthList5,1,function(f) sum(!is.na(f[30:33])))<3,NA,BirthList5$GestWeek_30)
#i=30
for (i in 30:50){
BirthList5[,c(i+24)]=ifelse(apply(BirthList5,1,function(f) sum(!is.na(f[30:(i+3)])))<((i-26)*0.75),NA,BirthList5[,c(i+24)])
}

BirthList6=BirthList5[,c('FIPS_County','LMPDate2',c(54:74))]
rm(list=ls())
