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
#2013/03/12 - Added lowess. Since these are the new "last plots," the random first plots are still exactly the same as previous versions.

seedForPlots<-234032
set.seed(seedForPlots)

#Intro question:
setwd("/Users/aaronfisher/Documents/JH/EDA Versions/EDA Git Repo/Coursera")

nversions<-5 #how many times to generate the data?

#will be used to ensure that each plot has the right pvalue
pbreaks<-c(.023,.025,.33,.35,1)
pbins.base<-rep(c(2,4),times=8)
nes.base<-c(35,35,100,100,200,200,rep(100,times=10)) #10 will be added IN THE GENERATION PROCESS and then trimmed off
pres.base<-(rep(c('n35','n100ref','n200','bestFit','axesScale','axesLabel','outlier','lowess'),each=2))
pbins<-rep(pbins.base,each=nversions)
nes<-rep(nes.base,each=nversions)
pres<-rep(pres.base,each=nversions)

version.base<-rep(1:nversions)#Note, divide by two because sig and nonsig are two diff versions
probnum.base<-c('1-1','1-2','2','3',paste0(rep(4:9,each=2),rep(c('-1','-2'),times=5)))
version<-rep(version.base,times=length(pbins.base))  
probnum<-rep(probnum.base,each=nversions) 


cbind(pbreaks[pbins],nes,pres,version)

nreps<-length(pbins)

yes<-matrix(nrow=nreps,ncol=max(nes))
xes<-matrix(nrow=nreps,ncol=max(nes))
ees<-matrix(nrow=nreps,ncol=max(nes))
resids<-matrix(nrow=nreps,ncol=max(nes))

#initialize variables
pvals<-rep(0,nreps)
tvals<-rep(0,nreps)
bhat.theory<-rep(0,nreps)
bhat.emp<-rep(0,nreps)

#First make basic data
#then add outliers

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
		bhat<-t.i*sd(e)/(sqrt(n)*sd(x)) #sd(e)=true σ, sqrt(n)*sd(x) = Σ[(x-bar(x))^2]
    #In previous iterations of this code, we had it set up to sometimes generate from an actual null (if pval=0). Below, we disable that option with the added FALSE statement, but it will still affect the seed & later random #'s generated.
		if(FALSE & pbreaks[pbins[i]]>.5 & sample(c(2,2,1),1)==2) {bhat<-0}
		y<-x*bhat*sample(c(-1,1),1)+e
		
		bhat.theory[i]<-bhat
		bhat.emp[i]<-summary(lm(y~x))$coeff[2,1]
		tvals[i]<-summary(lm(y~x))$coeff[2,3]
		pvals[i]<-summary(lm(y~x))$coeff[2,4]
		xes[i,1:n]<-x
		yes[i,1:n]<-y
    ees[i,1:n]<-e
    resids[i,1:n]<-summary(lm(y~x))$resid
    
		pi<-pvals[i]
		bini<-min(which(pi<pbreaks))
		if(bini==pbins[i]) tryagain<-FALSE
		
	}	
	setTxtProgressBar(pb,i)
}

cbind(pbreaks[pbins],round(pvals,digits=3),nes,apply(xes,1,function(v){sum(!is.na(v))}),pres,version)

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

################################################
#resid diagnostics

#Note, it could be argued that you didn't really simulated from a normal, because you culled from a normal to get the pvals you wanted. Here we check to make sure that the process of culling didn't effect the dist of the errors you generated, or the dist of the fitted errors from the models
#we compare the qqplots of these errors (generating and fitted) against a qqplot for nrep of random Z's, varying vector size according to nes (so the Z vectors match the e & resid vectors in length)

library(colorspace)
c.r.hcl<-function(x,n=1000, ...){ #cut rainbow_hcl
  xCut<-cut(x,breaks=n)
  colors<-rainbow_hcl(n=n, ...)
	out<-colors[xCut]
	return(out)
}
pal <- function(col, border = "light gray", ...)
#Copy pasted from HCL-Based Color Palettes in R
#http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
 {
 n <- length(col)
 plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
 axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
 }

pal(c.r.hcl(1:nreps,l=60,))
mypal<-c.r.hcl(1:nreps,l=50)
mypal<-sample(mypal)

par(mfrow=c(1,3))

xy<-qqnorm(ees[1,],plot=F)
plot(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main='generated errors')
for(i in 2:nreps){
  xy<-qqnorm(ees[i,],plot=F)
  lines(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',col=mypal[i],lwd=2)
}
abline(0,1,lwd=4)

xy<-qqnorm(resids[1,],plot=F)
plot(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main='fitted errors')
for(i in 2:nreps){
  xy<-qqnorm(resids[i,],plot=F)
  lines(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',col=mypal[i],lwd=2)
}
abline(0,1,lwd=4)


xy<-qqnorm(rnorm(nes[1]),plot=F)
plot(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main='reference random Z')
for(i in 2:nreps){
  xy<-qqnorm(rnorm(nes[i]),plot=F)
  lines(xy$x[order(xy$x)],xy$y[order(xy$x)],type='l',col=mypal[i],lwd=2)
}
abline(0,1,lwd=4)

par(mfrow=c(1,1))

######################################################

#rm(list=ls())


