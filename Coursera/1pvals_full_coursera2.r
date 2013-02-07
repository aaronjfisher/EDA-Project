#COURSERA STUFF
#Here we're using 7 questions
#Each with 2 versions, one significant one not
#Questions are: Small sample (35), medium sample (100) (reference),
#large sample (200), axis scale, labels, outlier, bestfit

set.seed(234032)

#Intro question:
setwd("/Users/aaronfisher/Documents/JH/EDA Versions/Sourcetree EDA Git Repo/Coursera")
nreps<-14
pbreaks<-c(.01,.03,.07,.5,1)
pbins<-rep(c(2,4),times=7)
nes<-c(35,35,100,100,200,200,rep(100,times=8))
yes<-matrix(nrow=nreps,ncol=max(nes))
xes<-matrix(nrow=nreps,ncol=max(nes))
pvals<-1 #initialize variables
tvals<-1
pres<-c(rep('plain',length=6),rep(c('bestFit','axesScale','axesLabel','outlier'),each=2))
#dropping Lowess here

#First generate baseline data
#then add presentation data (same data + presentaion vector)
#then change data for when the twist is adding an outlier

for(i in 1:nreps){
	tryagain<-T
#	if(i %in% c(1:100*nreps/100)) print(i)
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


################################################
#Fix those that now get an extra outlier
#add it one sd above the max x and max y, or w/e is appropriate for the upper corner that works
#if it's not sig, add the outlier close to above the mean.

for(i in which(pres=='outlier')){
  tryAgain<-TRUE
  while(tryAgain){
  	n<-nes[i]
    sig.i<-pvals[i]<.05
  	#grab the middle point from x and y
  	mx<-rep(mean(x),n)
  	my<-rep(mean(y),n)
  	x<-xes[i,1:n]
  	y<-yes[i,1:n]
  	distvec<-sqrt((mx-x)^2+(my-y)^2)
  	switch<-which(distvec==min(distvec))
  	
    #NOTE IT PREV VERSIONS THIS NICE DEALY LET US RANDOMLY PUT THE OUTLIER IN W/E CORNER WE WANTED
    #Put it in an UPPER CORNER
  	if(tvals[i]<1 & sig.i) { #slope going down
  		x[switch]<-min(x)-sd(x)
  		y[switch]<-max(y)+sd(y)
  	}
  	if(tvals[i]>1 & sig.i) {
  		x[switch]<-max(x)+sd(x)
  		y[switch]<-max(y)+sd(y)
  	}
    if(!sig.i){
      #leave X where it is now
      y[switch]<-max(y)+sd(y)*sqrt(2)
    }
  
  	tvals[i]<-summary(lm(y~x))$coeff[2,3]
  	pvals[i]<-summary(lm(y~x))$coeff[2,4]
    if(sig.i&pvals[i]<.04) tryAgain=FALSE
    if(!sig.i&pvals[i]>.06) tryAgain=FALSE
  }
	xes[i,1:n]<-x
	yes[i,1:n]<-y
}

#save(list=c('xes','yes','nreps','pbins','pvals','tvals','nes','pres'),file='data_for_1plots_coursera2.RData')
#load('data_for_1plots_coursera2.RData')




