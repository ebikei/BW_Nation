## Figure by Region
par(mfrow=c(1,1),mar=c(2.1,4.7,4.1,2.1))
beta=c(-6.6,-2.8,-2.8,-13.0,-4.6)
low=c(-7.2,-5.6,-4.3,-14.3,-5.7)
up=c(-5.9,0.0,-1.2,-11.6,-3.5)

#xvalues=c(1,4,6,8,10)
xvalues=c(1,3,4,5,6)
pch=rep(20,5)
lty=rep(1,5)
pch_t=rep(20,5)
text3=expression('PM'[10-2.5]*' Effects by Region')
data=data.frame(xvalues,beta,low,up,pch)
central<-data$beta
low<-data$low
high<-data$up
plot(xvalues,central,xlim=c(0.5,7),ylim=c(-17.5,2.5),xaxt="n",
xlab="",ylab="Effect Change (g)",lwd=2,yaxt='n',bty='n',pch=data$pch,cex=1.5,cex.lab=1.35,cex.main=1.25)
axis(2, yaxp=c(-20,2.5,9),las=2,cex.axis=1.1)
for (i in 1:5)
{
	segments(xvalues[i],low[i],xvalues[i],high[i],lwd=3,col='Black',lty=lty[i])
	points(xvalues[i],central[i],pch=pch[i],cex=1.35)
}
abline(h=0)
abline(v=c(2),lty=4)

text2=c('Whole U.S.','East','North','South','West')
midpoint=c(1,3,4,5,6)
par(las=1)
text(midpoint,-16,labels=text2,cex=1.25)
