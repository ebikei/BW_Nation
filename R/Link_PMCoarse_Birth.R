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

load('PMCoarse_Exposure.RData') #BirthList6

yearlist=c(1998:2007)
result=data.frame()
#load('BirthData/BW2002.RData')
for (i in 1:length(yearlist)){

DF=readRDS(paste('BirthData/BW',yearlist[i],'.rds',sep=''))
DF$lmpday[DF$lmpday==99]=15
DF=filter(DF,lmpyr!=9999,lmpmon!=99,res_fips==occ_fips) %>%
	mutate(DF,FIPS_County=sprintf('%05d',res_fips),
		LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),
		BD_15=as.Date(paste(biryr,sprintf('%02d',birmon),15,sep='-'),'%Y-%m-%d')) %>%
	select(FIPS_County,LMPDate2,BD_15,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)

DF=filter(DF,dgestat>29,dgestat!=99,dbirwt>2500,dbirwt!=9999) %>%
		mutate(.,BD_Est=LMPDate2+dgestat*7,diff=abs(as.numeric(BD_Est-BD_15))) %>%
		filter(.,diff<=30)
setkey(DF,FIPS_County,LMPDate2)

#Merge BW and Air data
DF2=merge(DF,BirthList6,by=c('FIPS_County','LMPDate2')) 
DF2$PMCoarse_Exposure=ifelse(DF2$dgestat==30,DF2$GestWeek_30,
							ifelse(DF2$dgestat==31,DF2$GestWeek_31,
							ifelse(DF2$dgestat==32,DF2$GestWeek_32,
							ifelse(DF2$dgestat==33,DF2$GestWeek_33,
							ifelse(DF2$dgestat==34,DF2$GestWeek_34,
							ifelse(DF2$dgestat==35,DF2$GestWeek_35,
							ifelse(DF2$dgestat==36,DF2$GestWeek_36,
							ifelse(DF2$dgestat==37,DF2$GestWeek_37,
							ifelse(DF2$dgestat==38,DF2$GestWeek_38,
							ifelse(DF2$dgestat==39,DF2$GestWeek_39,
							ifelse(DF2$dgestat==40,DF2$GestWeek_40,
							ifelse(DF2$dgestat==41,DF2$GestWeek_41,
							ifelse(DF2$dgestat==42,DF2$GestWeek_42,
							ifelse(DF2$dgestat==43,DF2$GestWeek_43,
							ifelse(DF2$dgestat==44,DF2$GestWeek_44,
							ifelse(DF2$dgestat==45,DF2$GestWeek_45,
							ifelse(DF2$dgestat==46,DF2$GestWeek_46,
							ifelse(DF2$dgestat==47,DF2$GestWeek_47,
							ifelse(DF2$dgestat==48,DF2$GestWeek_48,
							ifelse(DF2$dgestat==49,DF2$GestWeek_49,
							ifelse(DF2$dgestat==50,Birth2002$GestWeek_50,
							NA)))))))))))))))))))))
DF3=filter(DF2,!is.na(PMCoarse_Exposure)) %>%
#			filter(.,substr(FIPS_County,1,2)=='09') %>%
			select(FIPS_County,LMPDate2,BD_15,BD_Est,diff,PMCoarse_Exposure,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)
Coef=data.frame(summary(lm(dbirwt~PMCoarse_Exposure,data=DF3))$coefficients)
Coef$Year=yearlist[i]
result=rbind(result,Coef)
rm(DF,DF2,DF3,Coef)
}


rm(list=ls())


