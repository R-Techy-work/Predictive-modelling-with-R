require('readxl')
load('flunet_1995-2020.rdata')
month<-as.Date(paste(2020,2:9,1,sep="-"))
country<-unique(dat[,1])
x<-read.csv('WHO-COVID-19-global-data-2.csv')
country<-unique(x[,3])
names(x)[1] <-'time'
x$time<-as.Date(x$time)




cmp<-function(c){
y<-x[which(c==x[,3]),]
y$confirm<- diff(c(0,y$Cumulative_cases))
y$dead<- diff(c(0,y$Cumulative_deaths))
print(head(y))

if(c=='The United Kingdom')c<-'United Kingdom of Great Britain and Northern Ireland'
i<-which(c==dat[,1])
if(c=='United Kingdom of Great Britain and Northern Ireland')c<-'United Kingdom'
uk<-dat[i,]
print(head(uk))
print(unique(uk[,1]))
uk<-uk[sort(as.numeric(as.Date(uk$End.date)),index=T)$ix,]
sk<-y
par(las=1)
matplot(as.Date(sk$time),cbind(sk$confirm,sk$dead),type='b',log='y',pch=17:18,ylab='daily (weekly) confirmtions',axe=F)
for(year in 0:5){
uk1<-uk[grep(2015+year,uk$End.date),]
lines(as.Date(uk1$End.date)+365*(5-year),uk1$Total.number.of.influenza.positive,col='blue',pch=19,lty=1,lwd=ifelse(year==5,3,1))
}
legend("topright",bty='n',legend=c('COVID-19 cases','COVID-19 deaths','Flu positive pre 5 years'),col=c(1:2,'blue'),pch=c(17:18,NA),lty=1)
axis(2)
axis(1,at=month,lab=month.abb[2:9])
mtext(side=3,c)
}
tiff('Figure1.tiff',,width=10,height=8,res=200,unit='in')
par(las=1,mar=c(3,4,1,1),mfrow=c(2,2))
cmp('Italy')
cmp('Spain')
cmp('Japan')
cmp('Republic of Korea')
dev.off()
tiff('FigureS1.tiff',width=10,height=8,res=200,unit='in')
par(las=1,mar=c(3,4,1,1),mfrow=c(2,2))
cmp('France')
cmp('The United Kingdom')
#cmp('South Africa')
cmp('United States of America')
cmp('Canada')
dev.off()
tiff('FigureS2.tiff',width=10,height=8,res=200,unit='in')
par(las=1,mar=c(3,4,1,1),mfrow=c(2,2))
cmp('Sweden')
cmp('Netherlands')
cmp('Brazil')
cmp('Iran (Islamic Republic of)')
dev.off()
