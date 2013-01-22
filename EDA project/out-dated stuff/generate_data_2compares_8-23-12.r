################################################
################################################
#2 plots question:
set.seed(523142)

#???????Should I very N and errors between two plots??? Otherwise its just askign which has higher signal???????????

olddir<-getwd()
setwd("C:/Users/Aaron/Documents/JH/EDA project")
nreps<-100 
nplots<-2 #number of plots to compare against
nes<-rep(c(20,50,100,200,500),length=nreps*nplots)[sample(nreps*nplots)]
n2<-matrix(nes,ncol=nplots) #don't fix n?
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
	pbinsub<-pbins[i,] #2dim vector
	for(j in 1:nplots){
		tryagain<-T
		while(tryagain==T){
			n<-n2[i,j]
			x<-rnorm(n)
			e<-rnorm(n)
			
			t<-qnorm(pbreaks[pbinsub[j]],mean=0,sd=1)*sample(c(-1,1),1)
			if(abs(t)>5)t<-0 #guards against when the bin is 1, and quantile has infinite size
			bhat<-t*sd(e)/(sqrt(n)*sd(x))
			if(pbreaks[pbinsub[j]]>.05) bhat<-0# Always generate it actually from a null
			y<-x*bhat+e
			x2[[i]][[j]]<-x
			y2[[i]][[j]]<-y
			p2[i,j]<-summary(lm(y~x))$coef[2,4]
			
			pi<-p2[i,j]
			binj<-min(which(pi<=pbreaks))
			if(binj==pbinsub[j]){ tryagain<-F}
		}
	}
}

save(list=c('n2','nreps','p2','pbins','pbreaks','x2','y2'),file='data_for_2compare.RData')

i<-1
par(mfrow=c(1,2))
plot(x2[[i]][[1]],y2[[i]][[1]])
plot(x2[[i]][[2]],y2[[i]][[2]])
pbreaks[pbins[i,]]
p2[i,]