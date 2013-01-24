################################################
################################################
#2 plots question:
set.seed(523142)


nreps<-7
nplots<-2 #number of plots to compare against
nes2<-c(100,100,100,350,350,350,100)
#first 6 are harder than last one, which has more difference in the bins
n2<-matrix(nes2,nrow=nreps,ncol=nplots) #FIX n
#FOLLOWING CODE HAS BEEN BLOCKED B/C WE'RE NOW FIXING N!!!!
 # n2same<-rep(c(T,T,F),nreps)[sample(nreps)]
# n2[n2same,]<-n2[n2same,1]
# for(i in 1:nreps){
	# if(n2[i,1]==n2[i,2]) n2same[i]<-TRUE
# }


pbreaks<-c(.001,.01,.05,1)
#pbins will hold the value of the lowest pval
#ensure semi-specific distances
pbins<-matrix(c(1,2,  2,3,   3,4,  1,2,   2,3,   3,4, 2,4), ncol=nplots,byrow=T)
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
			n<-n2[i,j]
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


#No Replicates Here :(
# num_new_reps<-1
# newreps<-2
# for(i in 1:num_new_reps){
	# newpos<-i+nreps
	# oldpos<-newreps[i]
	# n2<-rbind(n2,n2[oldpos,])
	# n2same[newpos]<-n2same[oldpos]
	# p2<-rbind(p2,p2[oldpos,])
	# pbins<-rbind(pbins,pbins[oldpos,])
	# x2[[newpos]]<-x2[[oldpos]]
	# y2[[newpos]]<-y2[[oldpos]]
# }
# b4replicates2<-c(1:nreps,newreps)

setwd( "C:/Users/Aaron/Dropbox/EDA project/SURVEY")
save(list=c('n2','nes2','nreps','p2','pbins','pbreaks','x2','y2'),file='data_for_2compare_pilot.RData')


##Practice Graphing it
testme<-F
if(testme==T){
 for(i in 1:nreps){
par(mfrow=c(1,2))
plot(x2[[i]][[1]],y2[[i]][[1]],main=nes2[i])
plot(x2[[i]][[2]],y2[[i]][[2]],main=nes2[i])
pbreaks[pbins[i,]]
print(round(p2[i,],digits=5))
readline(prompt="go")
}
}