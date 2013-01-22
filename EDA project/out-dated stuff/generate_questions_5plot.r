#Also add: which ones are significant?

olddir<-getwd()
setwd("C:/Users/Aaron/Documents/JH/EDA project")
load('data_for_5plots.RData')
setwd(olddir)

# hist(tvals,breaks=60)
# hist(pvals,breaks=100)
# median(pvals)
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$breaks
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$counts
# hist(pvals[pvals<.06],breaks=50)

#ticker<-1
ticker<-ticker+1

n<-nes[ticker]

print(c('pvals -',round(p5[ticker,],digits=5),'n -',n,' ticker -',ticker))

question<-paste("Which plot do you think corresponds with the lowest p-value?")

png('FigureQuestion.png',height=800,width=1200)
	par(mfrow=c(2,3))
	for(i in 1:5){ #right now you're only looking at 3
		x<-x5[[ticker]][i,]
		y<-y5[[ticker]][i,]
		plot(x,y,xlab='X',ylab='Y',main=paste("Q",ticker," Plot-",i,sep=''))
	}
dev.off()

answers<-paste('Plot',1:5,sep='-')
true.statement.1<-answers[which(p5[ticker,]==min(p5[ticker,]) )]
rorder<-sample(4,1)
false.statement.1<-answers[which(p5[ticker,]!=min(p5[ticker,]) )][rorder[1]]
false.statement.2<-answers[which(p5[ticker,]!=min(p5[ticker,]) )][rorder[2]]
false.statement.3<-answers[which(p5[ticker,]!=min(p5[ticker,]) )][rorder[3]]
false.statement.4<-answers[which(p5[ticker,]!=min(p5[ticker,]) )][rorder[4]]
false.statement.5<-'None of the plots show statistically significant relationships.'

if(min(p5[ticker,])>.05){
true.statement.1<-'None of the plots show statistically significant relationships.'
rorder<-sample(4,1)
false.statement.1<-answers[rorder[1]]
false.statement.2<-answers[rorder[2]]
false.statement.3<-answers[rorder[3]]
false.statement.4<-answers[rorder[4]]
false.statement.5<-answers[rorder[5]]
}


true.reason.1<-paste('The p-values for these 5 plots are: ',round(p5[ticker,1],digits=5),', ',round(p5[ticker,2],digits=5),', ',round(p5[ticker,3],digits=5),', ',round(p5[ticker,4],digits=5),' and ',round(p5[ticker,5],digits=5),'.',sep='')
