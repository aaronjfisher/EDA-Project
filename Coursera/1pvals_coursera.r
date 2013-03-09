#COURSERA STUFF

#Here we're using 8 questions
#Each with 10 options people can see (with the exception of probs 2-3), half are significant half are not
#Questions are: Small sample (n=35),medium sample not sig (n=100) (reference), 
#medium sample sig (n=100) (reference),
#large sample (n=200), bestfit, axis scale, labels, & outlier
#For the medium "reference" sample, each person HAS to see both questions (sig and not) so we have something to compare against.
#The other versions are to cancel out any effects of a "bad generated sample" 
#we make 5 versions of each data/question type
#All p-values are fixed to be in the range (.023,.025) or in the range (.33,.35).
#To prevent outliers where we don't want them, we generate 10 extra xes, and then trim off the bottom and top 5.
#Note - in general we get great matching between the theoretical generating b.hat and the
#empirical b.hat from the simulation (bhat.theory v. bhat.emp)
	#However, in the outlier plots, we don't update the bhat.emp vector to reflect the new
	#true slope, just update tvals & pvals.
#You have deleted out line here that could have zoomed out a little bit for the outlier plots, to make that last point more visible, but seems too complicated any subjectively justified.

set.seed(234032)

#Intro question:
setwd("/Users/aaronfisher/Documents/JH/EDA Versions/EDA Git Repo/Coursera")

nversions<-5 #how many times to generate the data?

#will be used to ensure that each plot has the right pvalue
pbreaks<-c(.023,.025,.33,.35,1)
pbins.base<-rep(c(2,4),times=7)
nes.base<-c(35,35,100,100,200,200,rep(100,times=8)) #10 will be added IN THE GENERATION PROCESS and then trimmed off
pres.base<-(rep(c('n35','n100ref','n200','bestFit','axesScale','axesLabel','outlier'),each=2))
pbins<-rep(pbins.base,each=nversions)
nes<-rep(nes.base,each=nversions)
pres<-rep(pres.base,each=nversions)

version.base<-rep(1:nversions)#Note, divide by two because sig and nonsig are two diff versions
probnum.base<-c('1-1','1-2','2','3',paste0(rep(4:8,each=2),rep(c('-1','-2'),times=5)))
version<-rep(version.base,times=length(pbins.base))  
probnum<-rep(probnum.base,each=nversions) 


cbind(pbreaks[pbins],nes,pres,version)

nreps<-length(pbins)

yes<-matrix(nrow=nreps,ncol=max(nes))
xes<-matrix(nrow=nreps,ncol=max(nes))

pvals<-rep(0,nreps) #initialize variables
tvals<-rep(0,nreps)
bhat.theory<-rep(0,nreps)
bhat.emp<-rep(0,nreps)

#No Lowess here

#First generate baseline data
#then add presentation data (same data + presentaion vector)
#then change data for when the twist is adding an outlier

print(paste('nreps = ',nreps))
pb<-txtProgressBar(min = 1, max = nreps,  char = "=", style = 3)

for(i in 1:nreps){
	tryagain<-TRUE
	while(tryagain){ #to ENSURE that we get a good mix of p-value ranges
		n<-nes[i]
		#get t close to what we want
		t.i<-qt(pbreaks[pbins[i]]/2,df=n-2,lower.tail=F)
		if(abs(t.i)>5) t.i<-0 #guards against when the bin is 1, and quantile has infinite size
		#Add & Trim Xes
		x.pre<-rnorm(n +10)
		x<-x.pre[order(x.pre)[6:(length(x.pre)-5)] ] #trim off extra x's
		e<-rnorm(n)
		bhat<-t.i*sd(e)/(sqrt(n)*sd(x))
		if(pbins[i]>=5 & sample(c(2,2,1),1)==2) bhat<-0 #sometimes generate it actually from a null
		y<-x*bhat*sample(c(-1,1),1)+e
		
		bhat.theory[i]<-bhat
		bhat.emp[i]<-summary(lm(y~x))$coeff[2,1]
		tvals[i]<-summary(lm(y~x))$coeff[2,3]
		pvals[i]<-summary(lm(y~x))$coeff[2,4]
		xes[i,1:n]<-x
		yes[i,1:n]<-y

		pi<-pvals[i]
		bini<-min(which(pi<pbreaks))
		if(bini==pbins[i]) tryagain<-FALSE
		

		
	}	
	setTxtProgressBar(pb,i)
	#plot(x,y,main=n)
	#readline(prompt='go')
}

plot(bhat.theory,abs(bhat.emp))
abline(0,1)
################################################
#Fix those that now get an extra outlier
#add it one sd above the max x and max y, or w/e is appropriate for the upper corner that works
#if it's not sig, add the outlier close to above the mean.

for(i in which(pres=='outlier')){
  	n<-nes[i]
  	x<-xes[i,1:n]
  	y<-yes[i,1:n]

    sig.i<-pvals[i]<.05
  	#grab the middle point from x and y
  	mx<-rep(mean(x),n)
  	my<-rep(mean(y),n)
  	distvec<-sqrt((mx-x)^2+(my-y)^2)
  	switch<-which(distvec==min(distvec))
  	
    #NOTE IT PREV VERSIONS THIS NICE DEALY LET US RANDOMLY PUT THE OUTLIER IN W/E CORNER WE WANTED. here we put it in an UPPER CORNER
    #case1: #sig slope going down
  	if(tvals[i]<0 & sig.i) { 
  		x[switch]<-min(x)-sd(x)
  		y[switch]<-max(y)+sd(y)
  	}
  	#case2: sig slope going up
  	if(tvals[i]>0 & sig.i) { 
  		x[switch]<-max(x)+sd(x)
  		y[switch]<-max(y)+sd(y)
  	}
  	#case3: flat slope -> move it up!
    if(!sig.i){
      y[switch]<-max(y)+sd(y)*sqrt(2) #sqrt 2 makes it the same distance move as the others!
    }
  
  	tvals[i]<-summary(lm(y~x))$coeff[2,3]
  	pvals[i]<-summary(lm(y~x))$coeff[2,4]

	xes[i,1:n]<-x
	yes[i,1:n]<-y

	#NOTE - NOTE UPDATING bhat for the outlier plots
}

save(list=c('xes','yes','nreps','pbins','pvals','tvals','nes','pres'),file='data_for_1plots_coursera.RData')
#load('data_for_1plots_coursera.RData')

for(i in 1:nreps){
  n<-nes[i]
  x<-xes[i,1:n]
  y<-yes[i,1:n]
  pval<-pvals[i]
  tval<-tvals[i]
  m<-lm(y~x)
  style<-pres[i]

  t2<-'Data'
  if(style=='lowess') t2<-'with Lowess Line'
  if(style=='bestFit') t2<-'with OLS Best Fit Line'
  title<-paste('Sample ',t2,sep='')
  xl<-paste("Cranial Electrode",floor(runif(1,11,44)),"(Standardized)")
  yl<-paste("Cranial Electrode",floor(runif(1,53,97)),"(Standardized)")
  
  png(paste0("pvalue plot images/coursera2_#",probnum[i],'_datVer-',version[i],'_',pres[i],'_pval-',round(pvals[i],digits=3),".png"), width = 400, height = 400)
    par(mfrow=c(1,1))
  	plot(x,y,xlab='X',ylab='Y',main=title)
  	if(style=='lowess') lines(lowess(x,y))
  	if(style=='bestFit') abline(m$coef)
  	if(style=='axesScale')plot(x,y,xlab='X',ylab='Y',main=title,xlim=c(min(x)-1.5*sd(x),max(x)+1.5*sd(x)),ylim=c(min(y)-1.5*sd(y),max(y)+1.5*sd(y)))
 
  	if(style=='axesLabel') plot(x,y,xlab=xl,ylab=yl,main=title)
  dev.off()
}
#rm(list=ls())
