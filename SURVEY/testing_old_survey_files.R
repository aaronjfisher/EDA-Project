setwd("/Users/aaronfisher/Documents/JH/EDA Versions/Sourcetree EDA Git Repo/SURVEY")

load("data_for_1plots_pilot.RData" )

pbreaks<-c(.001,.01,.05,.15,1)

plot(pvals,(nes),pch=19,log='x',xaxt='n',yaxt='n',col=c('blue','black')[(pres=='plain')+1],ylim=c(0,300))
axis(side=1,pbreaks,at=pbreaks)
axis(side=2,unique(nes),at=unique(nes))
abline(v=pbreaks[pbins],lty=2)
abline(h=nes,lty=2)
text(pvals[pres!="plain"],nes[pres!="plain"]+10,pres[pres!="plain"])
text(pvals[whichrep],nes[whichrep]-10,"rep")

#looks cool!