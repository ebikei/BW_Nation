#jhpce01.jhsph.edu
#qrsh -l mf=16G,h_vmem=32G 

x<-c('dplyr','data.table','biglm','ggplot2','tlnise','lme4')
lapply(x, require, character.only=T)
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
output_location='/home/bst/other/kebisu/Download/'

load('bwdata.RData')

lis=data.frame(table(bwdata$FIPS_County)) %>% filter(.,Freq<1000) %>% select(.,Var1)
## Check Correlation by FIPS_County
County_List=unique(bwdata$FIPS_County)
cr=data.frame()
pb=txtProgressBar(min=0,max=length(County_List), style = 3)
ptm=proc.time()

for (i in 1:length(County_List)){
	tryCatch({
	Sys.sleep(0.1)	
	bwdata2=filter(bwdata,FIPS_County==County_List[i])
	bw_exp=select(bwdata2,PM25_Exposure,PMCoarse_Exposure)
	cr1=data.frame(County_List[i],cor(bw_exp)[1,2],dim(bwdata2)[1])
	cr=rbind(cr,cr1)
	rm(cr1)
	setTxtProgressBar(pb, i)
	}, error=function(e){})
}
proc.time()-ptm
close(pb)
names(cr)=c('FIPS_County','R','Num')

summary(cr)
cr2=filter(cr,abs(R)>0.5)
ExtremeCor=cr2$FIPS_County

State_FIPS=read.csv('state_geocodes.csv')
names(State_FIPS)[5:6]=c('RegionName','DivisionName')
State_FIPS$State.Code=sprintf("%02d",State_FIPS$St_FIPS)
State_FIPS=State_FIPS[order(State_FIPS$State.Code),]

cr2$State.Code=substr(cr2$FIPS_County,1,2)
cr3=inner_join(cr2,State_FIPS,by='State.Code')
table(cr3$RegionName)
table(cr3$DivisionName)

##################Change Restriction###########
bwdata2=filter(bwdata,year!=1998,!(FIPS_County %in% ExtremeCor))
test=data.frame(table(bwdata2$FIPS_County,bwdata2$year)) %>% filter(.,Freq>500)
test2=data.frame(table(test$Var1)) %>% filter(.,Freq>5) %>% select(.,Var1)
temp=test2$Var1
bwdata3=filter(bwdata2,FIPS_County %in% temp)

Model4=biglm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata3)
summary(Model4)

## Region
Region4=c('Northeast','Midwest','South','West')
out2=data.frame()

for (i in 1:4){
	bwdata4=filter(bwdata3,RegionName==Region4[i])
	Model5=lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata4)
	test=data.frame(Region4[i],summary(Model5)$coefficients[c(2:3),])
	test$Pol=c('PMCoarse','PM25')
	test$Numb=dim(bwdata4)[1]
	out2=rbind(out2,test)
	rm(bwdata4,Model5,test)
}
row.names(out2)=c(1:dim(out2)[1])
names(out2)=c('Name','PE','SD','t','Pvalue','Pol','Numb') 
out3=filter(out2,Pol=='PM25') %>% arrange(.,Pvalue)



#############################
##############################
###### Look into South######3
##############################

load('CountySize.RData') #County
South=filter(bwdata3,RegionName=='South') %>% merge(.,County,by='FIPS_County')

summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South))
summary(lm(dbirwt~PMCoarse_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South))
summary(lm(dbirwt~PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South))

##County
out2=data.frame()

for (i in 1:length(unique(South$FIPS_County))){
	South2=filter(South,FIPS_County==unique(South$FIPS_County)[i])
	Model5=lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+First_AT+Second_AT+Third_AT,data=South2)
	test=data.frame(unique(South$FIPS_County)[i],summary(Model5)$coefficients[c(2:3),])
	test$Pol=c('PMCoarse','PM25')
	test$Numb=dim(South2)[1]
	out2=rbind(out2,test)
	rm(South2,Model5,test)
}
row.names(out2)=c(1:dim(out2)[1])
names(out2)=c('Name','PE','SD','t','Pvalue','Pol','Numb') 

out2=merge(out2,County,by.x='Name',by.y='FIPS_County')
out3=filter(out2,Pol=='PM25') %>% arrange(.,PE)

Table1=group_by(South,FIPS_County) %>% summarize(PM25Ave=mean(PM25_Exposure,na.rm=TRUE),
	PMCAve=mean(PMCoarse_Exposure,na.rm=TRUE),BWAve=mean(dbirwt,na.rm=TRUE))

p=ggplot(Table1,aes(x=PM25Ave,y=BWAve))+geom_point()+geom_text(aes(label=FIPS_County),hjust=0, vjust=0,size=2.5)

pdf(paste(output_location,'South.pdf',paste=''),15,10)
p
dev.off()


set.seed(21)
seed=round(10000*runif(1))
prior=0
IQR=4.67
comb=tlnise(out3$PE,out3$SD^2,prior=prior,maxiter=5000,seed=seed)
Result2=data.frame(comb$gamma) %>%
	mutate(.,Effect=(est*IQR),LCI=((est-1.96*se)*IQR),UCI=((est+1.96*se)*IQR),CIs=paste('[',round(LCI,2),', ',round(UCI,2),']',sep='')) %>%
	select(.,Effect,CIs)
Result2

outlist=filter(out3,PE>0,Numb>60000)
#outlist=filter(outlist,Name=='37119'|Name=='48439'|Name=='47093'|Name=='12011')

out4=anti_join(out3,outlist,by='Name')
#out4=out3
comb=tlnise(out4$PE,out4$SD^2,prior=prior,maxiter=5000,seed=seed)
data.frame(comb$gamma) %>%
	mutate(.,Effect=(est*IQR),LCI=((est-1.96*se)*IQR),UCI=((est+1.96*se)*IQR),CIs=paste('[',round(LCI,2),', ',round(UCI,2),']',sep='')) %>%
	select(.,Effect,CIs)

South2=filter(South,!(FIPS_County %in% outlist$Name))
#prop.table(table(South2$mrace3))
summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South2))
South22=filter(South,(FIPS_County %in% outlist$Name))
#prop.table(table(South22$mrace3))
summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South22))
summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure,data=South22))


##Compare basic stat beteeen these 13 counties
South$Index=ifelse(South$FIPS_County %in% outlist$Name,1,0)
group_by(South,Index) %>% summarize(BW=mean(dbirwt,na.rm=TRUE),PMC=mean(PMCoarse_Exposure,na.rm=TRUE),PM25=mean(PM25_Exposure,na.rm=TRUE),PM10=mean(PMCoarse_Exposure,na.rm=TRUE),HS=mean(LessThanHS,na.rm=TRUE),PV=mean(LessThanPov_Line,na.rm=TRUE),Size=mean(AreaSizeSqKm,na.rm=TRUE))
prop.table(table(South$mrace3,South$Index),2)
prop.table(table(South$meduc6,South$Index),2)
group_by(South,Index,season) %>% summarize(BW=mean(dbirwt,na.rm=TRUE),PMC=mean(PMCoarse_Exposure,na.rm=TRUE),PM25=mean(PM25_Exposure,na.rm=TRUE),PM10=mean(PMCoarse_Exposure,na.rm=TRUE),HS=mean(LessThanHS,na.rm=TRUE),PV=mean(LessThanPov_Line,na.rm=TRUE),Size=mean(AreaSizeSqKm,na.rm=TRUE))

group_by(South,mrace3) %>% summarize(PM25=mean(PM25_Exposure,na.rm=TRUE))
group_by(South2,mrace3) %>% summarize(PM25=mean(PM25_Exposure,na.rm=TRUE))
group_by(South2,year) %>% summarize(PM25=mean(PM25_Exposure,na.rm=TRUE))
group_by(South2,season) %>% summarize(PM25=mean(PM25_Exposure,na.rm=TRUE))

summary(South2$PM25_Exposure)
summary(South22$PM25_Exposure)
summary(South2$dbirwt)
summary(South22$dbirwt)


South_AA=filter(South22,mrace3=='AfricanAmerican')
summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South_AA))
South_W=filter(South22,mrace3=='Caucasian')
summary(lm(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=South_W))

fits=lmList(dbirwt~PMCoarse_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+
	meduc6+dmar+csex+totord9+monpre+tobacco+year+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT|season, data=South22)


bw_exp=filter(South22,season=='Autumn') %>% select(.,PM25_Exposure,PMCoarse_Exposure)
cor(bw_exp)
rm(bw_exp)

