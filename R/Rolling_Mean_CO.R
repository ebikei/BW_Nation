#jhpce01.jhsph.edu
#qrsh -l mf=16G,h_vmem=32G 

x<-c("dplyr","ggplot2",'data.table','zoo')#,"data.table")
lapply(x, require, character.only=T)
#drive=c("E:\\")
#temp=paste(drive,'BW_PMCoarse\\Data',sep='')
temp='/home/bst/other/kebisu/BW_PMCoarse/Data'
setwd(temp)

load('CO_Data.RData') #CO_Data
load('COMon_CountyPWC.RData') #COMon_CountyPWC

#Calculated Weekly Rolling Mean
CO_DF=filter(CO_Data,FIPS_Monitor %in% unique(COMon_CountyPWC$FIPS_Monitor)) %>%
	data.table()
setkey(CO_DF,FIPS_Monitor,Date)
CO_DF2=CO_DF[CJ(unique(FIPS_Monitor),seq(min(Date),max(Date),by=1))]
CO_DF2[,CO_Week:=rollapply(CO_Level,7,mean,align=c("right"),fill=NA,na.rm=TRUE),by=FIPS_Monitor]
CO_DF2$CO_Week[is.nan(CO_DF2$CO_Week)]=NA

# Calculate gestational exposure
load('BirthData/LMP_FIPS_Cobination.RData') #Combination_List
MonitorList=CO_DF2[!duplicated(CO_DF2$FIPS_Monitor),list(FIPS_Monitor)] %>%
	mutate(FIPS_County=substr(FIPS_Monitor,1,5)) %>%
	arrange(FIPS_County,FIPS_Monitor)
Combination=inner_join(Combination_List,MonitorList,by='FIPS_County')
rm(Combination_List,CO_DF,CO_Data,COMon_CountyPWC,MonitorList)
output=select(Combination,FIPS_Monitor,LMPDate2,FIPS_County) %>% data.table()
setkey(output,FIPS_Monitor,LMPDate2)
rm(Combination)

for (i in 1:50){
	BirthList3=mutate(Combination,Date=LMPDate2+i*7-1)
	setkey(BirthList3,FIPS_Monitor,Date)
	test=CO_DF2[BirthList3] %>% select(FIPS_Monitor,LMPDate2,Date,CO_Week)
	setkey(test,FIPS_Monitor,LMPDate2)
	output=cbind(output,test$CO_Week)
	rm(BirthList3,test)
}
output2=data.frame(output)
names=paste(rep('Week',50),1:50,sep='')
colnames(output2)[4:53]=names
rm(CO_DF2,output)

pb=txtProgressBar(min=30,max=50, style = 3)
for (i in 30:50){
	Sys.sleep(0.1)
	output2[[paste0('GestWeek_',i)]]=rowMeans(output2[,c(4:(3+i))],na.rm=TRUE)
	setTxtProgressBar(pb,i)
}

output2=output2[rowSums(!is.na(output2[,4:29]))>0,]
output2=output2[rowSums(!is.na(output2[,4:16]))>9,]
output2=output2[rowSums(!is.na(output2[,17:29]))>9,]

pb=txtProgressBar(min=30,max=50, style = 3)
for (i in 30:50){
	Sys.sleep(0.1)
	output2[,c(i+24)]=ifelse(rowSums(!is.na(output2[,30:(i+3)]))<((i-26)*0.75),NA,output2[,c(i+24)])
	setTxtProgressBar(pb,i)
}

output3=output2[,c(1:3,54:74)]
output3=output3[rowSums(!is.na(output3[,4:24]))>0,]
rm(output2)

load('COMon_CountyPWC.RData') #COMon_CountyPWC
output3=inner_join(COMon_CountyPWC,output3,by=c('FIPS_County','FIPS_Monitor')) %>%
	data.table()
setkey(output3,FIPS_County,LMPDate2)

yearlist=c(1999:2007)
result=data.frame()
BWData_CO=data.frame()
ptm=proc.time()
pb=txtProgressBar(min=0,max=length(yearlist),style=3)
i=1
#for (i in 1:length(yearlist)){

	Sys.sleep(0.1)
	tryCatch({
	DF=readRDS(paste('BirthData/BW',yearlist[i],'.rds',sep=''))
	DF=mutate(DF,ID=paste(yearlist[i],seq(1:dim(DF)[1]),sep='_')) %>% filter(lmpmon!=99,res_fips!=0,res_fips==occ_fips)
	DF$lmpday[DF$lmpday==99]=15
	DF2=mutate(DF,FIPS_County=sprintf("%05d",res_fips),LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),
			BD_15=as.Date(paste(biryr,sprintf('%02d',birmon),15,sep='-'),'%Y-%m-%d'),Diff=as.numeric(LMPDate2+7*dgestat-BD_15)) #%>%
#Repeat this process until I fill out lmpyr=9999 to a reasonable year
	DF2$lmpyr[DF2$lmpyr==9999]=yearlist[i]
	DF2=mutate(DF2,LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),Diff=as.numeric(LMPDate2+7*dgestat-BD_15))
	DF2$lmpyr[abs(DF2$Diff)>200]=(yearlist[i]-1)
	DF2=mutate(DF2,LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),Diff=as.numeric(LMPDate2+7*dgestat-BD_15))
	DF2$lmpyr[abs(DF2$Diff)>200]=(yearlist[i]-2)
	DF2=mutate(DF2,LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),Diff=as.numeric(LMPDate2+7*dgestat-BD_15))
	DF2$lmpyr[abs(DF2$Diff)>200]=(yearlist[i])
	DF2=mutate(DF2,LMPDate2=as.Date(paste(lmpyr,sprintf('%02d',lmpmon),sprintf('%02d',lmpday),sep='-'),"%Y-%m-%d"),Diff=as.numeric(LMPDate2+7*dgestat-BD_15))
	DF2=select(DF2,ID,FIPS_County,LMPDate2,BD_15,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3) %>%
		filter(dgestat>29,dgestat!=99,dbirwt>1000,dbirwt<=5500,dbirwt!=9999) %>%
		mutate(.,BD_Est=LMPDate2+dgestat*7,diff=abs(as.numeric(BD_Est-BD_15))) %>%
		filter(.,diff<=30)
	setkey(DF2,FIPS_County,LMPDate2)
	DF3=inner_join(DF2,output3,by=c('FIPS_County','LMPDate2'))
	DF3=DF2[output3, nomatch=0]

	rm(DF,DF2)
	DF3$PM25_Exposure=ifelse(DF3$dgestat==30,DF3$GestWeek_30,
								ifelse(DF3$dgestat==31,DF3$GestWeek_31,
								ifelse(DF3$dgestat==32,DF3$GestWeek_32,
								ifelse(DF3$dgestat==33,DF3$GestWeek_33,
								ifelse(DF3$dgestat==34,DF3$GestWeek_34,
								ifelse(DF3$dgestat==35,DF3$GestWeek_35,
								ifelse(DF3$dgestat==36,DF3$GestWeek_36,
								ifelse(DF3$dgestat==37,DF3$GestWeek_37,
								ifelse(DF3$dgestat==38,DF3$GestWeek_38,
								ifelse(DF3$dgestat==39,DF3$GestWeek_39,
								ifelse(DF3$dgestat==40,DF3$GestWeek_40,
								ifelse(DF3$dgestat==41,DF3$GestWeek_41,
								ifelse(DF3$dgestat==42,DF3$GestWeek_42,
								ifelse(DF3$dgestat==43,DF3$GestWeek_43,
								ifelse(DF3$dgestat==44,DF3$GestWeek_44,
								ifelse(DF3$dgestat==45,DF3$GestWeek_45,
								ifelse(DF3$dgestat==46,DF3$GestWeek_46,
								ifelse(DF3$dgestat==47,DF3$GestWeek_47,
								ifelse(DF3$dgestat==48,DF3$GestWeek_48,
								ifelse(DF3$dgestat==49,DF3$GestWeek_49,
								ifelse(DF3$dgestat==50,DF3$GestWeek_50,
								NA)))))))))))))))))))))
	DF3$PM25_TriThird=ifelse(DF3$dgestat==30,DF3$PM25_TriThird_Week_30,
								ifelse(DF3$dgestat==31,DF3$PM25_TriThird_Week_31,
								ifelse(DF3$dgestat==32,DF3$PM25_TriThird_Week_32,
								ifelse(DF3$dgestat==33,DF3$PM25_TriThird_Week_33,
								ifelse(DF3$dgestat==34,DF3$PM25_TriThird_Week_34,
								ifelse(DF3$dgestat==35,DF3$PM25_TriThird_Week_35,
								ifelse(DF3$dgestat==36,DF3$PM25_TriThird_Week_36,
								ifelse(DF3$dgestat==37,DF3$PM25_TriThird_Week_37,
								ifelse(DF3$dgestat==38,DF3$PM25_TriThird_Week_38,
								ifelse(DF3$dgestat==39,DF3$PM25_TriThird_Week_39,
								ifelse(DF3$dgestat==40,DF3$PM25_TriThird_Week_40,
								ifelse(DF3$dgestat==41,DF3$PM25_TriThird_Week_41,
								ifelse(DF3$dgestat==42,DF3$PM25_TriThird_Week_42,
								ifelse(DF3$dgestat==43,DF3$PM25_TriThird_Week_43,
								ifelse(DF3$dgestat==44,DF3$PM25_TriThird_Week_44,
								ifelse(DF3$dgestat==45,DF3$PM25_TriThird_Week_45,
								ifelse(DF3$dgestat==46,DF3$PM25_TriThird_Week_46,
								ifelse(DF3$dgestat==47,DF3$PM25_TriThird_Week_47,
								ifelse(DF3$dgestat==48,DF3$PM25_TriThird_Week_48,
								ifelse(DF3$dgestat==49,DF3$PM25_TriThird_Week_49,
								ifelse(DF3$dgestat==50,DF3$PM25_TriThird_Week_50,
								NA)))))))))))))))))))))
