library(HH)
library(here)

salinity <- read.csv(here::here("data", "raw", "Salinity Data.csv"), strip.white=TRUE)

discharge <- read.csv(here::here("data", "raw", "Discharge.csv"))

salinity$Date <- as.Date(salinity$Date,"%d/%m/%Y")
discharge$Date <- as.Date(discharge$Date,"%d/%m/%Y")

locs<-tapply(X=discharge$Date, FUN=min, INDEX=format(discharge$Date,'%Y%m'))
t(t(locs))
at=discharge$Date%in%locs
discharge$Date[at]
at2=at&format(discharge$Date,'%m')%in% c('01','03','05','07','09','11')
discharge$Date[at2]

par(mar=c(5,5,3,5))
plot(Salinity~Date, data = salinity, ylim = c(0,35), pch=c(21,17,15,22,16,24)[as.factor(salinity$Site)], bg = c("white"), ylab = "Salinity (psu)",
     cex = 1, xaxt= "n", xlab = "", cex.lab = 1.5)

par(new=TRUE)
plot(X~Date, data = discharge, ylim = c(0,10), xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     pch = 20, cex = 0.5, type = "l", lty = 2, col = "gray47")
axis(4)
y2axis<-expression("Fraser River discharge " ~ (10^{3}~m^{3}~s^{-1}))
mtext(y2axis,side=4,line=3.5,cex=1.5)
axis(side=1, at=discharge$Date[at],labels=F)
text(x=discharge$Date[at2], par("usr")[3]-0.9, srt = 45, adj = 1,labels=format(discharge$Date[at2],'%b-%y'), xpd = TRUE)

#export.eps("salinitygraph.eps")

plot.new()
legend(locator(1),c("HS1","HS2","HS3","LS1","LS2","LS3","Discharge"), pch=c(15,16,17,0,1,2,3), lty=c(0,0,0,0,0,0,2), col=c('black','black','black','black','black','black','gray47'),bty="o",cex=0.8,pt.cex=1,y.intersp=0.9,x.intersp=0.1)

export.eps("salinitygraph.eps")
png(file = "salinitygraph.png",res=800)
jpeg(file = "salinitygraph2.jpeg")
