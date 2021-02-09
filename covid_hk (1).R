x<-read.csv('covid_hk.csv',as.is=T)
x[,2]<-as.Date(x[,2])

x$week<-round(as.numeric(x[,2]-x[1,2])/7)+1 
i<-grep('Asymptomatic',x[,3])
y<-x[i,]
head(y)
i1<-table(y$week)
i2<-table(x$week)

months<-as.numeric(as.Date(paste(2020,2:12,1,sep="-"))-as.Date('2020-1-23'))/7*1.24 

i<-which(x$Case %in% c('I'))
z<-x[i,]
i3<-table(z$week)
wk<-sort(as.numeric(unique(c(names(i1),names(i2),names(i3)))))
wk1<-matrix(0,ncol=3,nrow=length(wk))
row.names(wk1)<-wk
wk1[match(names(i1),wk),1]<-i1
wk1[match(names(i2),wk),2]<-i2
wk1[match(names(i3),wk),3]<-i3
p<-wk1[,1]/wk1[,2] 
se<-1.96*sqrt(p*(1-p))/sqrt(wk1[,2])
jpeg('hk_asymp.jpeg',width=800,height=600)
par(las=1,mar=c(3,5,1,5))
barplot(wk1[,2],col='lightgrey',xaxt='n',ylim=c(0,max(wk1[,2])+20))
par(new=T)
barplot(wk1[,3],ylim=par('usr')[3:4],col='white',ann=F,axe=F,axisnames=F,xaxt='n')
legend("topleft",legend=c('Imported','Local'),pch=22:22,pt.bg=c('white','lightgrey'),bty='n')
axis(1,at=months,lab=month.abb[2:12],padj=-1)
par(new=T)
plot(as.numeric(names(p)),p,xlim=c(0.5,length(wk)+0.4),ann=F,axe=F,frame=T,ylim=c(0,1),col='red')
axis(4)
mtext(side=2,line=3,'Weekly confirmations',las=0)
mtext(side=4,line=3,'Asymptomatic ratio',las=0,col='red')
#mtext(side=1,line=2,'week since Jan 23')
for(i in 1:length(p))
lines(c(i,i),p[i]+c(-1,1)*se[i],col='red')
smoothingSpline = smooth.spline(1:length(p),p, spar=0.8) 
lines(smoothingSpline)
dev.off()

for(i in c(4,7)) x[,i]<-as.factor(x[,i])
x$asym<-0
x$asym[which(x$'Date of onset'=='Asymptomatic')]<-1
x$Date<-as.numeric(as.Date(x$'Report.date')-as.Date('2020-1-22'))


jpeg('trend.jpeg',width=800,height=400)
at<-as.numeric(as.Date(c('2020-6-20','2020-7-1','2020-7-10','2020-7-20','2020-8-1','2020-8-10','2020-8-20','2020-9-1','2020-9-10'))-as.Date('2020-1-22'))
at1<-as.numeric(as.Date(paste(2020,2:9,1,sep="-"))-as.Date('2020-1-22'))
 x1<-x[x$Case=='L',]
 y1<-table(x1$Date)
 y<-table(x$Date)
 par(las=1,mar=c(3,4,1,1),xaxs='i',yaxs='i')
plot2<-function(x){
plot(as.numeric(names(y)),y,log=x,type='b',xlim=c(ifelse(x=="",0,158),250),ann=F,axe=F,frame=T)
if(x=="y")
axis(1,tck=0.01,padj=-0.8,at,lab=c('Jun-20','Jul-1','Jul-10','Jul-20','Aug-1','Aug-10','Aug-20','Sept-1','Sept-10'))
if(x=="")
axis(1,tck=0.01,padj=-0.8,at1,lab=month.abb[2:9])
axis(2,tck=0.01,hadj=0.5,at=c(1,5,10,50,100,149))
abline(h=c(5,10,50,100,120),lty=2,col='lightgrey')
abline(v=at[-1],lty=2,col='lightgrey')
if(x=="")
mtext(side=2,line=2,las=0,'daily confirmations')
if(x=="y")
mtext(side=2,line=2,las=0,'daily confirmations (logarithm)')
 points(as.numeric(names(y1)),y1,pch=17,type='b',col='red')
 legend(ifelse(x=="","topleft","bottomright"),pch=c(21,17),col=c('black','red'),legend=c('all cases','local cases'),bty='n')
#mtext(side=1,line=-1,adj=0.8,Sys.Date())
}
par(mfrow=c(1,2),mar=c(3,3,1,1))
plot2("")
plot2("y")
dev.off()
