olddir<-getwd()
setwd("C:/Users/Aaron/Documents/JH/EDA project")
load('data_for_1plots.RData')
setwd(olddir)

#beforeRep has nreps*2 unique values, and 6 indexes at the end refering to the original index of the graphs that are replicated

# hist(tvals,breaks=60)
# hist(pvals,breaks=100)
# median(pvals)
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$breaks
# hist(pvals,breaks=c(0,.001,.01,.05,.15,.6,1),plot=F)$counts
# hist(pvals[pvals<.06],breaks=50)

#tick1plots<-0
tick1plots<-tick1plots+1

n<-nes[tick1plots]
x<-xes[tick1plots,1:n]
y<-yes[tick1plots,1:n]
pval<-pvals[tick1plots]
tval<-tvals[tick1plots]
m<-lm(y~x)
style<-pres[tick1plots]



png('FigureQuestion.png',height=500,width=500)
	par(mfrow=c(1,1))
	plot(x,y,xlab='X',ylab='Y',main=tick1plots)
	if(style=='lowess') lines(lowess(x,y))
	if(style=='bestFit') abline(m$coef)
	if(style=='axes')plot(x,y,xlab='X',ylab='Y',main=tick1plots,xlim=c(min(x)-1.5*sd(x),max(x)+1.5*sd(x)),ylim=c(min(y)-1.5*sd(y),max(y)+1.5*sd(y)))
dev.off()

print(c('pval -',round(pval,digits=5),' tstat -',round(tval,digits=4),'n -',n,' tick1plots -',tick1plots,'pres -',style,'cor -',round(cor(x,y),digits=5)))

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
		# x<-x5[[tick1plots]][i,]
		# y<-y5[[tick1plots]][i,]
		# plot(x,y,xlab='X',ylab='Y',main=paste(tick1plots,i,sep='-'))
	# }
	#dev.off()
# }
