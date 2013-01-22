#Generate all the data for the plots
#it will get saved, then called back in and cycled through

setwd("C:/Users/Aaron/Documents/JH/EDA project")

nreps<-1000 #make game first
nes<-rep(c(20,50,100,200,500),length=nreps)[sample(nreps)]
pres<-rep(c('plain','plain','plain','5plots','lowess','bestFit','axes'),length=nreps)[sample(nreps)] #about 50% plain
t<-2
xes<-matrix(nrow=nreps,ncol=max(nes))
yes<-matrix(nrow=nreps,ncol=max(nes))
pvals<-1
tvals<-1
x5<-list()
y5<-list()
p5<-matrix(nrow=nreps,ncol=5)
for(i in 1:nreps){
	n<-nes[i]
	x<-rnorm(n)
	e<-rnorm(n)
	bhat<-t*sd(e)/(sqrt(n)*sd(x))
	y<-x*bhat*sample(c(-1,1),1)+e
	
	tvals[i]<-summary(lm(y~x))$coeff[2,3]
	pvals[i]<-summary(lm(y~x))$coeff[2,4]
	xes[i,1:n]<-x
	yes[i,1:n]<-y
	
	if(pres[i]=='5plots'){
		x5[[i]]<-matrix(nrow=5,ncol=n)
		y5[[i]]<-matrix(nrow=5,ncol=n)
		for(j in 1:5){
			x<-rnorm(n)
			e<-rnorm(n)
			bhat<-t*sd(e)/(sqrt(n)*sd(x))
			y<-x*bhat+e
			x5[[i]][j,]<-x
			y5[[i]][j,]<-y
			p5[i,j]<-summary(lm(y~x))$coef[2,4]
		}
	}	
}


save(list=c('xes','yes','pvals','tvals','nes','pres','x5','y5','p5'),file='data_for_plots.RData')

checkit<-F
if(checkit){
hist(tvals,breaks=30)
hist(pvals,breaks=100)
hist(pvals,breaks=c(0,.001,.01,.05,.15,1),plot=F)
hist(pvals[pvals<.06],breaks=50)
}
