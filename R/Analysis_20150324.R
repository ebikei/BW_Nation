#jhpce01.jhsph.edu
#qrsh -l mf=16G,h_vmem=32G 

x<-c('dplyr','data.table','biglm')
lapply(x,require,character.only=T)
setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
output_location=c('/home/bst/other/kebisu/BW_PMCoarse/Result/')


load('BirthData/BWData_C_20150322.RData') #BWData_C
BWData_C=data.table(BWData_C) %>% setkey(ID)
load('BirthData/BWData_25_20150322.RData') #BWData_25
BWData_25=select(BWData_25,ID,PM25_Exposure,FIPS_PM25Monitor,Distance_PM25) %>% data.table() %>% setkey(ID)
load('BirthData/BWData_10_20150322.RData') #BWData_10
BWData_10=select(BWData_10,ID,PM10_Exposure,FIPS_PM10Monitor,Distance_PM10) %>% data.table() %>% setkey(ID)
BWData_C=BWData_25[BWData_C]
BWData_C=BWData_10[BWData_C]

BWData_C=select(BWData_C,ID,FIPS_County,LMPDate2,BD_15,BD_Est,PMC_Exposure,PM10_Exposure,PM25_Exposure,FIPS_PMCMonitor,Distance_PMC,FIPS_PM10Monitor,Distance_PM10,FIPS_PM25Monitor,Distance_PM25,dbirwt,dgestat,mage8,pldel3,mrace3,meduc6,dmar,totord9,monpre,nprevis,dfage,csex,dplural,fmaps,delmeth5,diabetes,tobacco,alcohol,wtgain,frace3,First_AT,Second_AT,Third_AT) %>%
					arrange(FIPS_County,LMPDate2) %>%
					data.frame()
rm(BWData_10,BWData_25)

State_FIPS=read.csv('state_geocodes.csv') %>% filter(Name!='Alaska',Name!='Hawaii')
State_FIPS$Name=as.character(State_FIPS$Name)
names(State_FIPS)[5:6]=c('RegionName','DivisionName')
State_FIPS$State.Code=sprintf("%02d",State_FIPS$St_FIPS)
State_FIPS=State_FIPS[order(State_FIPS$State.Code),]

#load('BirthData/BWData_Coarse.RData') #BWData_Coarse
load('NBH_Stat.RData') #NBH_Stat

bwdata=filter(BWData_C,dplural=='Single',dgestat>=37,dgestat<45,substr(BD_Est,1,4)!=1998,substr(BD_15,1,4)!=1998)
bwdata$LBW=ifelse(bwdata$dbirwt<2500,1,0)
bwdata$SGA10=0
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==22&bwdata$dbirwt<393]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==23&bwdata$dbirwt<453]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==24&bwdata$dbirwt<498]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==25&bwdata$dbirwt<554]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==26&bwdata$dbirwt<594]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==27&bwdata$dbirwt<674]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==28&bwdata$dbirwt<766]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==29&bwdata$dbirwt<906]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==30&bwdata$dbirwt<1044]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==31&bwdata$dbirwt<1241]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==32&bwdata$dbirwt<1475]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==33&bwdata$dbirwt<1712]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==34&bwdata$dbirwt<1957]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==35&bwdata$dbirwt<2192]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==36&bwdata$dbirwt<2410]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==37&bwdata$dbirwt<2609]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==38&bwdata$dbirwt<2807]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==39&bwdata$dbirwt<2947]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==40&bwdata$dbirwt<3029]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==41&bwdata$dbirwt<3063]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==42&bwdata$dbirwt<2979]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==43&bwdata$dbirwt<2949]=1
bwdata$SGA10[bwdata$csex=='Male'&bwdata$dgestat==44&bwdata$dbirwt<2954]=1

bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==22&bwdata$dbirwt<362]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==23&bwdata$dbirwt<416]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==24&bwdata$dbirwt<470]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==25&bwdata$dbirwt<504]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==26&bwdata$dbirwt<556]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==27&bwdata$dbirwt<622]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==28&bwdata$dbirwt<693]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==29&bwdata$dbirwt<845]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==30&bwdata$dbirwt<965]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==31&bwdata$dbirwt<1180]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==32&bwdata$dbirwt<1390]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==33&bwdata$dbirwt<1638]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==34&bwdata$dbirwt<1872]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==35&bwdata$dbirwt<2099]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==36&bwdata$dbirwt<2299]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==37&bwdata$dbirwt<2495]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==38&bwdata$dbirwt<2694]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==39&bwdata$dbirwt<2834]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==40&bwdata$dbirwt<2919]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==41&bwdata$dbirwt<2949]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==42&bwdata$dbirwt<2893]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==43&bwdata$dbirwt<2849]=1
bwdata$SGA10[bwdata$csex=='Female'&bwdata$dgestat==44&bwdata$dbirwt<2863]=1

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
bwdata$State.Code=substr(bwdata$FIPS_County,1,2)
bwdata=data.table(bwdata)
setkey(bwdata,FIPS_County)
bwdata=merge(bwdata,NBH_Stat,by='FIPS_County')
setkey(bwdata,State.Code)
bwdata=merge(bwdata,State_FIPS,by='State.Code')

## Check Correlation by FIPS_County
County_List=unique(bwdata$FIPS_County)
cr=data.frame()
pb=txtProgressBar(min=0,max=length(County_List), style = 3)
ptm=proc.time()
for (i in 1:length(County_List)){
	tryCatch({
	Sys.sleep(0.1)	
	bwdata2=filter(bwdata,FIPS_County==County_List[i])
	bw_exp=select(bwdata2,PM25_Exposure,PMC_Exposure)
	cr1=data.frame(County_List[i],cor(bw_exp)[1,2],dim(bwdata2)[1])
	cr=rbind(cr,cr1)
	rm(cr1)
	setTxtProgressBar(pb, i)
	}, error=function(e){})
}
proc.time()-ptm
close(pb)
names(cr)=c('FIPS_County','R','Num')
cr2=filter(cr,abs(R)>0.6)
ExtremeCor2=cr2$FIPS_County
bwdata2=filter(bwdata,!(FIPS_County %in% ExtremeCor2))
#Remove county which is less than 100 and less than 4yr data county
test=data.frame(table(bwdata2$FIPS_County,bwdata2$year)) %>% filter(.,Freq>100)
test2=data.frame(table(test$Var1)) %>% filter(.,Freq>4) %>% select(.,Var1)
bwdata=filter(bwdata2,FIPS_County %in% test2$Var1)
rm(bwdata2)

Model1=biglm(dbirwt~PMC_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata)
summary(Model1)
Model2=biglm(dbirwt~PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata)
summary(Model2)
Model3=biglm(dbirwt~PM10_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata)
summary(Model3)
Model4=biglm(dbirwt~PMC_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+alcohol+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata)
summary(Model4)

Region4=c('Northeast','Midwest','South','West')
out2=data.frame()

for (i in 1:4){
	bwdata4=filter(bwdata,RegionName==Region4[i])
	Model5=lm(dbirwt~PMC_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata4)
	test=data.frame(Region4[i],summary(Model5)$coefficients[c(2:3),])
	test$Pol=c('PMCoarse','PM25')
	test$Numb=dim(bwdata4)[1]
	out2=rbind(out2,test)
	rm(bwdata4,Model5,test)
}
row.names(out2)=c(1:dim(out2)[1])
names(out2)=c('Name','PE','SD','t','Pvalue','Pol','Numb') 
out3=filter(out2,Pol=='PMCoarse') %>% arrange(.,Name)
out3

i=4
Region4[i]
bwdata4=filter(bwdata,RegionName==Region4[i]) %>% filter(!is.na(PM25_Exposure),!is.na(PMC_Exposure))
bw_exp=select(bwdata4,PM25_Exposure,PMC_Exposure) %>% data.frame()
cor(bw_exp)[1,2]

bwdata=mutate(bwdata,hyp_PMC=PM10_Exposure-PM25_Exposure,dif=PMC_Exposure-hyp_PMC)
summary(bwdata$dif)
tt=filter(bwdata,abs(dif)>25)

bwdata=filter(bwdata,FIPS_County!='48141')

out2=data.frame()
bwdata4=filter(bwdata,RegionName=='Midwest')
for (i in 1:length(unique(bwdata4$FIPS_County))){
	bwdata5=filter(bwdata4,FIPS_County!=unique(FIPS_County)[i])
	Model5=lm(dbirwt~PMC_Exposure+PM25_Exposure+gest_cat+mage8+mrace3+meduc6+dmar+csex+totord9+monpre+tobacco+year+season+Name+LessThanHS+LessThanPov_Line+First_AT+Second_AT+Third_AT,data=bwdata5)
	test=data.frame(summary(Model5)$coefficients[c(2:3),])
	test$Pol=c('PMCoarse','PM25')
	test$Numb=dim(bwdata5)[1]
	test$fips=unique(bwdata4$FIPS_County)[i]
	out2=rbind(out2,test)
	rm(bwdata5,Model5,test)
}

filter(out2,Pol=='PM25') %>% arrange(Estimate)
filter(out2,Pol=='PMCoarse') %>% arrange(Estimate)

Check ID 2006-37023
