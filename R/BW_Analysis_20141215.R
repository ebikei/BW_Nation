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

load('BirthData/BWData_Coarse.RData') #BWData_Coarse

bwdata=filter(BWData_Coarse,dplural=='Single',dgestat>=37,dgestat<45)
new.age=c(1,1,2,3,4,5,6,6,6)
bwdata$mage8=relevel(factor(new.age[bwdata$mage8],labels=c('Under 20 years','20-24','25-29','30-34','35-39','Over 40')),ref='25-29')
new.educ=c(1,1,2,3,4,5)
bwdata$meduc6=relevel(factor(new.educ[bwdata$meduc6],labels=c('Less than HS','HS','Some College','College','Unknown')),ref='HS')
new.ord=c(1,2,2,2,2,2,2,2,9)
bwdata$totord9=relevel(factor(new.ord[bwdata$totord9],labels=c('First','After Second','Unknown')),ref='First')
new.monpre=c(1,2,2,2,3,3,3,4,4,4,5)
bwdata$monpre=relevel(factor(new.monpre[bwdata$monpre],labels=c('NoCare','First','Second','Third','Unknown')),ref='First')
bwdata$csex=relevel(bwdata$csex,ref='Male')
bwdata$tobacco=relevel(bwdata$tobacco,ref='No')
bwdata$alcohol=relevel(bwdata$alcohol,ref='No')
bwdata$gest_cat=cut(bwdata$dgestat,c(37,39,41,45),labels=c('37-38','39-40','41-44'),right=FALSE,include.highest=TRUE)
bwdata$gest_cat=relevel(bwdata$gest_cat,ref='39-40')
bwdata$year=factor(format(bwdata$BD_15,'%Y'))
spring=c('Mar','Apr','May')
summer=c('Jun','Jul','Aug')
autumn=c('Sep','Oct','Nov')
winter=c('Dec','Jan','Feb')
bwdata$season=factor(ifelse(format(bwdata$BD_15,"%b") %in% spring,'Spring',
			ifelse(format(bwdata$BD_15,"%b") %in% summer,'Summer',
			ifelse(format(bwdata$BD_15,"%b") %in% autumn,'Autumn','Winter'))))
bwdata$season=relevel(bwdata$season,ref='Spring')
bwdata$S
tate.Code=substr(bwdata$FIPS_County,1,2)
bwdata=data.table(bwdata)
setkey(bwdata,State.Code)
bwdata=merge(bwdata,State_FIPS,by='State.Code')

BWByState=data.frame(table(bwdata$Name))
names(BWByState)=c('Name','BWNumber')
MonitorNumber=data.frame(table(substr(unique(bwdata$FIPS_County),1,2)))
names(MonitorNumber)=c('State.Code','CountyNumber')
List=merge(State_FIPS,MonitorNumber,by='State.Code') %>%
	 merge(.,BWByState,by='Name') %>%
	 select(.,State.Code,Name,RegionName,DivisionName,CountyNumber,BWNumber)
#write.csv(List,paste(output_location,'FreqList.csv',sep=''))
table(bwdata$RegionName)
table(bwdata$DivisionName)


group_by(bwdata,RegionName) %>%
	summarise(PMCoarse=mean(PMCoarse_Exposure,na.rm=TRUE),PMCoarse_Sd=sd(PMCoarse_Exposure,na.rm=TRUE),PMCoarse_IQR=IQR(PMCoarse_Exposure,na.rm=TRUE))

group_by(bwdata,DivisionName) %>%
	summarise(PMCoarse=mean(PMCoarse_Exposure,na.rm=TRUE),PMCoarse_Sd=sd(PMCoarse_Exposure,na.rm=TRUE),PMCoarse_IQR=IQR(PMCoarse_Exposure,na.rm=TRUE))

summary(lm(dbirwt~PMCoarse_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season,data=bwdata))

bwdata2=filter(bwdata,RegionName=='West')
summary(lm(dbirwt~PMCoarse_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season,data=bwdata2))

bwdata2=filter(bwdata,DivisionName=='Pacific')
summary(lm(dbirwt~PMCoarse_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season,data=bwdata2))

bwdata2=filter(bwdata,Name=='California')
summary(lm(dbirwt~PMCoarse_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season,data=bwdata2))


rm(list=ls())
