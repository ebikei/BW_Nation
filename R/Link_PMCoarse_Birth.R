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
pb=txtProgressBar(min=0,max=length(yearlist), style = 3)
ptm=proc.time()
result=data.frame()
BWData_Coarse=data.frame()
for (i in 1:length(yearlist)){
	tryCatch({
	Sys.sleep(0.1)
	DF=readRDS(paste('BirthData/BW',yearlist[i],'.rds',sep=''))
	DF$lmpday[DF$lmpday==99]=15
	DF=filter(DF,lmpyr!=9999,lmpmon!=99,res_fips==occ_fips) %>%
		mutate(DF,FIPS_County=sprintf('%05d',res_fips),
			LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),
			BD_15=as.Date(paste(biryr,sprintf('%02d',birmon),15,sep='-'),'%Y-%m-%d')) %>%
		select(FIPS_County,LMPDate2,BD_15,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)

	DF=filter(DF,dgestat>29,dgestat!=99,dbirwt>1000,dbirwt<=5500,dbirwt!=9999) %>%
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
#				filter(.,substr(FIPS_County,1,2)=='12') %>%
				select(FIPS_County,LMPDate2,BD_15,BD_Est,diff,PMCoarse_Exposure,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3)
	Coef=data.frame(summary(lm(dbirwt~PMCoarse_Exposure,data=DF3))$coefficients)
	Coef$Year=yearlist[i]
	result=rbind(result,Coef)
	temp=data.frame(DF3)
	BWData_Coarse=rbind(BWData_Coarse,temp)
	rm(DF,DF2,DF3,Coef,temp)
	setTxtProgressBar(pb, i)
	}, error=function(e){})
}
proc.time()-ptm
close(pb)


BWData_Coarse$mage8=factor(BWData_Coarse$mage8,levels=c(1:9),labels=c('Under 15 years','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54'))
BWData_Coarse$pldel3=factor(BWData_Coarse$pldel3,levels=c(1:3),labels=c('InHospital','NotInHospital','Unknown'))
BWData_Coarse$mrace3=factor(BWData_Coarse$mrace3,levels=c(1:3),labels=c('Caucasian','Other','AfricanAmerican'))
BWData_Coarse$meduc6=factor(BWData_Coarse$meduc6,levels=c(1:6),labels=c('Under 8 years','9-11','12','13-15','16 Over','Unknown'))
BWData_Coarse$dmar=factor(BWData_Coarse$dmar,levels=c(1,2,9),labels=c('Married','Unmarried','Unknown'))
BWData_Coarse$totord9=factor(BWData_Coarse$totord9,levels=c(1:9),labels=c('First','Second','Third','Fourth','Fifth','Sixth','Seventh','Eighth more','Unknown'))
BWData_Coarse$monpre=factor(BWData_Coarse$monpre,levels=c(0:9,99),labels=c('No Prenatal Care','1st month','2nd month','3rd month','4th month','5th month','6th month','7th month','8th month','9th month','Unknown'))
BWData_Coarse$nprevis[BWData_Coarse$nprevis==99]=NA
BWData_Coarse$dfage[BWData_Coarse$dfage==99]=NA
BWData_Coarse$csex=factor(BWData_Coarse$csex,levels=c(1:2),labels=c('Male','Female'))
BWData_Coarse$dplural=factor(BWData_Coarse$dplural,levels=c(1:5),labels=c('Single','Twin','Triplet','Quadruplet','Higher'))
BWData_Coarse$fmaps[BWData_Coarse$fmaps==99]=NA
BWData_Coarse$delmeth5=factor(BWData_Coarse$delmeth5,levels=c(1:5),labels=c('Vaginal','Vaginal after C-section','Primary C-section','Repeat C-section','Unknown'))
BWData_Coarse$diabetes=factor(BWData_Coarse$diabetes,levels=c(1:2,9),labels=c('Yes','No','Unknown'))
BWData_Coarse$tobacco=factor(BWData_Coarse$tobacco,levels=c(1:2,9),labels=c('Yes','No','Unknown'))
BWData_Coarse$alcohol=factor(BWData_Coarse$alcohol,levels=c(1:2,9),labels=c('Yes','No','Unknown'))
BWData_Coarse$wtgain[BWData_Coarse$wtgain==99]=NA
BWData_Coarse$frace3=factor(BWData_Coarse$frace3,levels=c(1:4),labels=c('Caucasian','Other','AfricanAmerican','Unknown'))
summary(BWData_Coarse)

save(BWData_Coarse,file='BirthData/BWData_Coarse.RData')

summary(lm(dbirwt~PMCoarse_Exposure+csex,data=BWData_Coarse))

rm(list=ls())


