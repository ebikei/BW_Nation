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

kk=seq(min(PMCoarse_Data2$Date.Local), max(PMCoarse_Data2$Date.Local))

kk=PMCoarse_Data2[CJ(unique(FIPS_County),seq(min(Date.Local),max(Date.Local),by=1))][,rollapply(PMCoarse,7,mean,na.rm=TRUE),by=FIPS_County]


PMCoarse_Data3=PMCoarse_Data2[CJ(unique(FIPS_County),seq(min(Date.Local),max(Date.Local),by=1))]
PMCoarse_Data3[,PMCoarse_Week:=rollapply(PMCoarse,7,mean,align=c("right"),na.rm=TRUE),by=FIPS_County]



PMCoarse_Data3$V1=rollapply(PMCoarse,7,mean,na.rm=TRUE),by=FIPS_County

PMCoarse_Data4=PMCoarse_Data3[, rollapply(PMCoarse,7,mean,na.rm=TRUE),by=FIPS_County]

get.mav <- function(bp,n=2){
  require(zoo)
  if(is.na(bp[1])) bp[1] <- mean(bp,na.rm=TRUE)
  bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(bp)
  c(bp[1:(n-1)],rollapply(bp,width=n,mean,align="right"))  
}
PMCoarse_Data3[,BLOOD_PRESSURE_UPDATED:=as.numeric(get.mav(PMCoarse,7)),by=FIPS_County]


z <- zoo(rep(c(3,NA,7,2,5),7), as.Date(31:65))
z
rollapply(z, 3, mean)
rollapply(z, 3, mean,align=c("center"))
rollapply(z, 3, mean,na.rm=TRUE,fill=NA)
rollapply(z, 3, mean,na.rm=TRUE,fill=TRUE)

rollapply(z, 3, mean,align=c("left"))
rollapply(z, 3, mean,na.rm=TRUE,fill=NA)
rollapply(z, 3, mean,na.rm=TRUE,fill=TRUE)
rollapply(z, 3, mean,align=c("right"),fill=NA)
rollapply(z, 3, mean,align=c("right"),fill=NA,na.rm=TRUE)


rm(list==ls())
