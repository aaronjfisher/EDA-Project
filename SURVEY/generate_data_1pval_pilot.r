#Generate all the data for the plots
#it will get saved, then called back in and cycled through
#Here we're focusing on the game!

set.seed(234092)

#Intro question:
setwd("/Users/aaronfisher/Documents/JH/EDA Versions/Sourcetree EDA Git Repo/SURVEY")
nreps<-20 #In the standard method, not used for the survey questions, we would end up with twice as many graphs, half of which will be plain and half will have fancy twists.
#HOWEVER, here we don't double it, we just make the second half have specific pres styles later on.
#also there will be replicates, so really there will be more like nreps*2.1 plots
#first we generate these nreps values, then we double up by adding another set of the same graphs, but this time with fancy presentations
#five plots will be a separate question
pbreaks<-c(.001,.01,.05,.15,1)
pbins<-rep(1:5,length=nreps)
nes<-c(30,70,140,225,225,140,225,30,70,30,70,140,70,30,70,225,30,225,140,140)
cbind(nes[1:5],nes[6:10],nes[11:15],nes[16:20])
xes<-matrix(nrow=nreps,ncol=max(nes)) #NOTE some rows will have NA values because of the different nes variable values.
yes<-matrix(nrow=nreps,ncol=max(nes))
pvals<-1 #initialize variables
tvals<-1

plot((pbreaks[pbins]),(nes),pch=19,log='x',xaxt='n',col=rep(c('black','blue'),c(10,11)))
axis(side=1,pbreaks,at=pbreaks)
abline(v=pbreaks[pbins],lty=2)
abline(h=nes,lty=2)

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
	#plot(x,y,main=n)
	#readline(prompt='go')
}


###############################################
#add same plot with new presentation style

npres<-10
pres<-c(rep('plain',length=10),c('outlier','lowess','bestFit','axesScale','axesLabel')[c(4,4,3,1,5,5,2,3,1,2)])
cbind(pres,pbreaks,nes)

#THIS IS NO LONGER HOW YOU MAKE DATA FOR THE SURVEY!!!!!!!
#NOW THE VECTORS ARE SET APRIORI AT THEIR FULL SIZE, THEY AREN'T DOUBLED LATER.
#Double check to make sure you didn't already do it
#if(dim(xes)[1]==nreps){
	#nes<-c(nes,nes[1:npres])
	#pbreaks<-c(pbreaks,pbreaks[1:npres])
	#pbins<-c(pbins,pbins[1:npres])
	#xes<-rbind(xes,xes[1:npres,])
	#yes<-rbind(yes,yes[1:npres,])
	#pvals<-c(pvals,pvals[1:npres])
	#tvals<-c(tvals,tvals[1:npres])
#}
#origPlot<-c(1:nreps,1:npres)

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
#Repeat numbers 2 and 4
nreplicates<-2
whichrep<-c(2,4)
if(dim(xes)[1]==nreps){
	nes<-c(nes,nes[whichrep])
	pbins<-c(pbins,pbins[whichrep])
	xes<-rbind(xes,xes[whichrep,])
	yes<-rbind(yes,yes[whichrep,])
	pvals<-c(pvals,pvals[whichrep])
	tvals<-c(tvals,tvals[whichrep])
	pres<-c(pres,pres[whichrep])
	#origPlot<-c(origPlot,origPlot[whichrep]) ## used when there's overlaping data between plain and styled
}
beforeRep<-c(1:(nreps*2),whichrep)
#beforeRep has 20 unique values, and 2 indexes at the end refering to the original index of the graphs that are replicated

plot(pvals,(nes),pch=19,log='x',xaxt='n',yaxt='n',col=c('blue','black')[(pres=='plain')+1],ylim=c(0,300))
axis(side=1,pbreaks,at=pbreaks)
axis(side=2,unique(nes),at=unique(nes))
abline(v=pbreaks[pbins],lty=2)
abline(h=nes,lty=2)
text(pvals[pres!="plain"],nes[pres!="plain"]+10,pres[pres!="plain"])
text(pvals[whichrep],nes[whichrep]-10,"rep")

save(list=c('xes','yes','nreps','pbins','pvals','tvals','nes','pres','whichrep','beforeRep','nreplicates'),file='data_for_1plots_pilot.RData')

