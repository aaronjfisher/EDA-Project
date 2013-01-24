################################################
################################################
#2 plots question:
set.seed(5430212)

#???????Should I very N and errors between two plots??? Otherwise its askign which has higher signal??????????? This is what I want to ask right????????
#Naw, keep it the same N (Jan 2013). See refAA

nreps<-50 
nplots<-2 #number of plots to compare against
nes2<-rep(c(20,50,100,200,500),length=nreps*nplots)[sample(nreps*nplots)]
n2<-matrix(nes2,ncol=nplots) #don't fix n? Not here yet, but you DID change n in a few lines from here:
n2same<-rep(c(T,T,T,F),nreps)[sample(nreps)] #refAA where we only make some n's the same
n2[n2same,]<-n2[n2same,1]
for(i in 1:nreps){
	if(n2[i,1]==n2[i,2]) n2same[i]<-TRUE #refAA
}


pbreaks<-c(.001,.01,.05,1)
#pbins will hold the value of the lowest pval
#Don't bother to make sure they're not too close, just go with it?
pbins<-matrix(rep(1:length(pbreaks),length=nreps*nplots)[sample(nreps*nplots)], ncol=nplots)
x2<-list()
y2<-list()
p2<-matrix(nrow=nreps,ncol=nplots)


for(i in 1:nreps){
	if(i %in% c(1:100*nreps/100)) print(i)
	x2[[i]]<-list()
	y2[[i]]<-list()
	
	for(j in 1:nplots){
		tryagain<-T
		while(tryagain==T){
			n<-n2[i,j] #refAA
			x<-rnorm(n)
			e<-rnorm(n)
			
			t<-qnorm(pbreaks[pbins[i,j]],mean=0,sd=1)*sample(c(-1,1),1)
			if(abs(t)>5)t<-0 #guards against when the bin is 1, and quantile has infinite size
			bhat<-t*sd(e)/(sqrt(n)*sd(x))
			if(pbreaks[pbins[i,j]]>.05) bhat<-0# Always generate it actually from a null
			y<-x*bhat+e
			x2[[i]][[j]]<-x
			y2[[i]][[j]]<-y
			p2[i,j]<-summary(lm(y~x))$coef[2,4]
			
			pi<-p2[i,j]
			binj<-min(which(pi<=pbreaks))
			if(binj==pbins[i,j]){ tryagain<-F}
		}
	}
}


#Add replicates
num_new_reps<-floor(nreps/10)
newreps<-sample(1:nreps,num_new_reps,replace=F)
for(i in 1:num_new_reps){
	newpos<-i+nreps
	oldpos<-newreps[i]
	n2<-rbind(n2,n2[oldpos,])
	n2same[newpos]<-n2same[oldpos]
	p2<-rbind(p2,p2[oldpos,])
	pbins<-rbind(pbins,pbins[oldpos,])
	x2[[newpos]]<-x2[[oldpos]]
	y2[[newpos]]<-y2[[oldpos]]
}
b4replicates2<-c(1:nreps,newreps)

setwd("C:/Users/Aaron/Dropbox/EDA project")
#save(list=c('n2','nreps','n2same','p2','pbins','b4replicates2','pbreaks','x2','y2'),file='data_for_2compare.RData')


##Practice Graphing it
# i<-1
# par(mfrow=c(1,2))
# plot(x2[[i]][[1]],y2[[i]][[1]])
# plot(x2[[i]][[2]],y2[[i]][[2]])
# pbreaks[pbins[i,]]
# p2[i,]