#Generate all the data for the plots
#it will get saved, then called back in and cycled through
#Here we're focusing on the game!

set.seed(2134091)

#Intro question:
nreps<-50 #we will end up with twice as many graphs, half of which will be plain and half will have fancy twists
#also there will be replicates, so really there will be more like nreps*2.1 plots
#first we generate these nreps values, then we double up by adding another set of the same graphs, but this time with fancy presentations
nes<-rep(c(20,50,100,200,500),length=nreps)[sample(nreps)]
#five plots will be a separate question
pbreaks<-c(.001,.01,.05,.15,1)
pbins<-rep(1:length(pbreaks),length=nreps)[sample(nreps)]
xes<-matrix(nrow=nreps,ncol=max(nes))
yes<-matrix(nrow=nreps,ncol=max(nes))
pvals<-1

tvals<-1


#First generate baseline data
#then add presentation data (same data + presentaion vector)
#then change data for when the twist is adding an outlier
#then add replicates of all of the vector to see how consistent people are on the exact same stuff

for(i in 1:nreps){
	tryagain<-T
	if(i %in% c(1:100*nreps/100)) print(i)
	while(tryagain){ #to ENSURE that we get a good mix of p-value ranges
		#get t close to what we want
		t<-qnorm(pbreaks[pbins[i]],mean=0,sd=1,lower.tail=F)
		if(abs(t)>5)t<-0 #guards against when the bin is 1, and quantile has infinite size
		n<-nes[i]
		x<-rnorm(n)
		e<-rnorm(n)
		bhat<-t*sd(e)/(sqrt(n)*sd(x))
		if(pbins[i]>=5 & sample(c(2,2,1),1)==2) bhat<-0 #sometimes generate it actually from a null
		y<-x*bhat*sample(c(-1,1),1)+e

		tvals[i]<-summary(lm(y~x))$coeff[2,3]
		pvals[i]<-summary(lm(y~x))$coeff[2,4]
		xes[i,1:n]<-x
		yes[i,1:n]<-y

		pi<-pvals[i]
		bini<-min(which(pi<pbreaks))
		if(bini==pbins[i]) tryagain<-F
	}	
}


###############################################
#add same plot with new presentation style

pres<-c(rep('plain',length=nreps),rep(c('outlier','lowess','bestFit','axesScale','axesLabel'),length=nreps)[sample(nreps)])#first all plain, then with variatin in presentation

if(dim(xes)[1]==nreps){
	nes<-c(nes,nes)
	pbreaks<-c(pbreaks,pbreaks)
	pbins<-c(pbins,pbins)
	xes<-rbind(xes,xes)
	yes<-rbind(yes,yes)
	pvals<-c(pvals,pvals)
	tvals<-c(tvals,tvals)
}
origPlot<-c(1:nreps,1:nreps)

################################################
#Fix those that now get an extra outlier
#add it one sd above the max x and max y, or w/e is appropriate for the right corner
for(i in which(pres=='outlier')){
	n<-nes[i]
	#grab the middle point from x and y
	mx<-rep(mean(x),n)
	my<-rep(mean(y),n)
	x<-xes[i,1:n]
	y<-yes[i,1:n]
	distvec<-sqrt((mx-x)^2+(my-y)^2)
	switch<-which(distvec==min(distvec))
	
	#should we add it to the top of the graph or the bottom?
	updown<-sample(c('up','down'),1)
	#tval conveys if the best fit line is pos or neg
	if(updown=='down' & tvals[i]<1) {
		x[switch]<-max(x)+sd(x)
		y[switch]<-min(y)-sd(y)
	}
	if(updown=='down' & tvals[i]>1) {
		x[switch]<-min(x)-sd(x)
		y[switch]<-min(y)-sd(y)
	}
	if(updown=='up' & tvals[i]<1) {
		x[switch]<-min(x)-sd(x)
		y[switch]<-max(y)+sd(y)
	}
	if(updown=='up' & tvals[i]>1) {
		x[switch]<-max(x)+sd(x)
		y[switch]<-max(y)+sd(y)
	}

	tvals[i]<-summary(lm(y~x))$coeff[2,3]
	pvals[i]<-summary(lm(y~x))$coeff[2,4]
	xes[i,1:n]<-x
	yes[i,1:n]<-y
}

################################################
#also add replicates!
nreplicates<-floor(2*nreps/10)
whichrep<-sample(1:(2*nreps),nreplicates)
if(dim(xes)[1]==nreps*2){
	nes<-c(nes,nes[whichrep])
	pbreaks<-c(pbreaks,pbreaks[whichrep])
	pbins<-c(pbins,pbins[whichrep])
	xes<-rbind(xes,xes[whichrep,])
	yes<-rbind(yes,yes[whichrep,])
	pvals<-c(pvals,pvals[whichrep])
	tvals<-c(tvals,tvals[whichrep])
	pres<-c(pres,pres[whichrep])
	origPlot<-c(origPlot,origPlot[whichrep])
}
beforeRep<-c(1:(nreps*2),whichrep)
#beforeRep has 100 unique values, and 6 indexes at the end refering to the original index of the graphs that are replicated


setwd("C:/Users/Aaron/Documents/JH/EDA project")
save(list=c('xes','yes','nreps','origPlot','pbins','pvals','tvals','nes','pres','whichrep','beforeRep','nreplicates'),file='data_for_1plots.RData')

