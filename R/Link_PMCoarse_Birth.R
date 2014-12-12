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

#load('PMCoarse_Exposure.RData') #BirthList6,
load('BirthData/BW2002.RData')

BW2002$lmpday[BW2002$lmpday==99]=15
BW2002=filter(BW2002,lmpyr!=9999,lmpmon!=99,res_fips==occ_fips) %>%
	mutate(BW2002,FIPS_County=sprintf('%05d',res_fips),
		LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),
		BD_15=as.Date(paste(biryr,sprintf('%02d',birmon),15,sep='-'),'%Y-%m-%d')) %>%
	select(FIPS_County,LMPDate2,BD_15,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)

BW2002=filter(BW2002,dgestat>29,dgestat!=99,dbirwt>2500,dbirwt!=9999) %>%
		mutate(.,BD_Est=LMPDate2+dgestat*7,diff=abs(as.numeric(BD_Est-BD_15))) %>%
		filter(.,diff<=30)
setkey(BW2002,FIPS_County,LMPDate2)

load('PMCoarse_Exposure.RData') #BirthList6

Birth2002=merge(BW2002,BirthList6,by=c('FIPS_County','LMPDate2')) 

Birth2002$PMCoarse_Exposure=ifelse(Birth2002$dgestat==30,Birth2002$GestWeek_30,
							ifelse(Birth2002$dgestat==31,Birth2002$GestWeek_31,
							ifelse(Birth2002$dgestat==32,Birth2002$GestWeek_32,
							ifelse(Birth2002$dgestat==33,Birth2002$GestWeek_33,
							ifelse(Birth2002$dgestat==34,Birth2002$GestWeek_34,
							ifelse(Birth2002$dgestat==35,Birth2002$GestWeek_35,
							ifelse(Birth2002$dgestat==36,Birth2002$GestWeek_36,
							ifelse(Birth2002$dgestat==37,Birth2002$GestWeek_37,
							ifelse(Birth2002$dgestat==38,Birth2002$GestWeek_38,
							ifelse(Birth2002$dgestat==39,Birth2002$GestWeek_39,
							ifelse(Birth2002$dgestat==40,Birth2002$GestWeek_40,
							ifelse(Birth2002$dgestat==41,Birth2002$GestWeek_41,
							ifelse(Birth2002$dgestat==42,Birth2002$GestWeek_42,
							ifelse(Birth2002$dgestat==43,Birth2002$GestWeek_43,
							ifelse(Birth2002$dgestat==44,Birth2002$GestWeek_44,
							ifelse(Birth2002$dgestat==45,Birth2002$GestWeek_45,
							ifelse(Birth2002$dgestat==46,Birth2002$GestWeek_46,
							ifelse(Birth2002$dgestat==47,Birth2002$GestWeek_47,
							ifelse(Birth2002$dgestat==48,Birth2002$GestWeek_48,
							ifelse(Birth2002$dgestat==49,Birth2002$GestWeek_49,
							ifelse(Birth2002$dgestat==50,Birth2002$GestWeek_50,
							NA)))))))))))))))))))))

Birth2002_2=filter(Birth2002,!is.na(PMCoarse_Exposure)) %>%
#			filter(.,substr(FIPS_County,1,2)=='48') %>%
			select(FIPS_County,LMPDate2,BD_15,BD_Est,diff,PMCoarse_Exposure,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)

summary(lm(dbirwt~PMCoarse_Exposure,data=Birth2002_2))

rm(list=ls())
