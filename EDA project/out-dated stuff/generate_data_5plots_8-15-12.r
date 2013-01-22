################################################
################################################
#5 plots question:
set.seed(8130219)

#The smallest pval of the 5 (stored in pbins) 

#Intro question:
olddir<-getwd()
setwd("C:/Users/Aaron/Documents/JH/EDA project")
nreps<-500 #we will end up with twice as many graphs, half of which will be plain and half will have fancy twists
#also there will be replicates, so really there will be more like nreps*2.1 plots
#first we generate these nreps values, then we double up by adding another set of the same graphs, but this time with fancy presentations
nes<-rep(c(20,50,100,200,500),length=nreps)[sample(nreps)]
pbreaks<-c(.001,.005,.01,.025,.05,.075,.15,.5,.75,1) #Need to add more detail so we can have all others be at least one bin above the least significant. The highest we will go is is .5.
#pbins will hold the value of the lowest pval
#Could make this type of barrier to ensure that pvalues aren't too close more distinct and well defined, by using some distance on the log scale (or multiplicative scale)?
pbins<-rep(1:sum(pbreaks<=.5),length=nreps)[sample(nreps)]
x5<-list()
y5<-list()
p5<-matrix(nrow=nreps,ncol=5)


for(i in 1:nreps){
	if(i %in% c(1:100*nreps/100)) print(i)
	n<-nes[i]
	x5[[i]]<-matrix(nrow=5,ncol=n)
	y5[[i]]<-matrix(nrow=5,ncol=n)
	#ensure that one of the p-values is the lowest (no ties)
	pbinsub<-c(pbins[i],sample(which(1:length(pbreaks)>pbins[i]+1),4,replace=T))[sample(5)]
	for(j in 1:5){
		tryagain<-T
		while(tryagain==T){
			x<-rnorm(n)
			e<-rnorm(n)
			
			t<-qnorm(pbreaks[pbinsub[j]],mean=0,sd=1)*sample(c(-1,1),1)
			if(abs(t)>5)t<-0 #guards against when the bin is 1, and quantile has infinite size
			bhat<-t*sd(e)/(sqrt(n)*sd(x))
			if(pbreaks[pbinsub[j]]>.05 & sample(c(2,2,1),1)==2) bhat<-0 #sometimes generate it actually from a null
			y<-x*bhat+e
			x5[[i]][j,]<-x
			y5[[i]][j,]<-y
			p5[i,j]<-summary(lm(y~x))$coef[2,4]
			
			pi<-p5[i,j]
			binj<-min(which(pi<pbreaks))
			if(binj==pbinsub[j]){ tryagain<-F}
		}
	}
}

save(list=c('nes','nreps','p5','pbins','pbreaks','x5','y5'),file='data_for_5plots.RData')