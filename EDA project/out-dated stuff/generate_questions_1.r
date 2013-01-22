setwd("C:/Users/Aaron/Documents/JH/EDA project")
load('data_for_plots.RData')

# hist(tvals,breaks=60)
# hist(pvals,breaks=100)
# median(pvals)
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$breaks
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$counts
# hist(pvals[pvals<.06],breaks=50)

#ticker<-1
ticker<-ticker+1

n<-nes[ticker]
x<-xes[ticker,1:n]
y<-yes[ticker,1:n]
pval<-pvals[ticker]
tval<-tvals[ticker]
m<-lm(y~x)
style<-pres[ticker]



#png('FigureQuestion.png',height=500,width=500)
par(mfrow=c(1,1))
plot(x,y,xlab='X',ylab='Y',main=ticker)
if(style=='lowess') lines(lowess(x,y))
if(style=='bestFit') abline(m$coef)
if(style=='axes')plot(x,y,xlab='X',ylab='Y',main=ticker,xlim=c(min(x)-1.5*sd(x),max(x)+1.5*sd(x)),ylim=c(min(y)-1.5*sd(y),max(y)+1.5*sd(y)))
#dev.off()

print(c('pval -',round(pval,digits=5),' tstat -',round(tval,digits=4),'n -',n,' ticker -',ticker,'pres -',style,'cor -',round(cor(x,y),digits=5)))

q2<-''
if(style=='lowess') q2<-'Here we also show a lowess line.'
if(style=='bestFit') q2<-'Here we also show the best fit linear model through the data (from ordinary least squares regression).'
question<-paste(q2,"<br/><br/>What do you think the p-value is?")


# if(style=='5plots'){
	#you can fix this in later
	#png('FigureQuestion.png',height=400,width=1300)
	# plot(x,y,xlab='X',ylab='Y')
	# par(mfrow=c(1,3))
	# for(i in 1:3){ #right now you're only looking at 3
		# x<-x5[[ticker]][i,]
		# y<-y5[[ticker]][i,]
		# plot(x,y,xlab='X',ylab='Y',main=paste(ticker,i,sep='-'))
	# }
	#dev.off()
# }
