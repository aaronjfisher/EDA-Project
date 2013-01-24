################################################
################################################
#5 plots question:
set.seed(813029)

#Here we're just FIXING N
#NO REPLICATES
#We end up with rows of:
# half sig
# mostly sig
# no sig
# mostly sig
# all sig
# mostly sig
# half sig

#Intro question:
nreps<-6
nplots<-4
N<-nplots*nreps
nes4<-rep(c(100,200,100,150,100,100),length=N)
n4<-matrix(nes4,ncol=4)
n4same<-rep(c(T,T,T),length=nreps)
#n4[n4same,]<-n4[n4same,1]
#add in the other sames
# for(i in 1:nreps){
	# if(n4[i,1]==n4[i,2]&n4[i,2]==n4[i,2]&n4[i,2]==n4[i,4])n4same[i]<-T
# }


pbreaks<-c(.001,.01,.05,.15,1) #only marking those that are significant
pbins<-matrix(rep(1:length(pbreaks),length=nreps*nplots)[sample(nreps*nplots)], ncol=nplots)
x4<-list()
y4<-list()
p4<-matrix(nrow=nreps,ncol=nplots)
pbins #LOOKS GOOD!

for(i in 1:nreps){
	if(i %in% c(1:100*nreps/100)) print(i)
	x4[[i]]<-list()
	y4[[i]]<-list()
	#ensure that one of the p-values is the lowest (no ties)
	for(j in 1:nplots){
		tryagain<-T
		while(tryagain==T){
			n<-n4[i,j]
			x<-rnorm(n)
			e<-rnorm(n)
			
			t<-qnorm(pbreaks[pbins[i,j]],mean=0,sd=1)*sample(c(-1,1),1)
			if(abs(t)>5)t<-0 #guards against when the bin is 1, and quantile has infinite size
			bhat<-t*sd(e)/(sqrt(n)*sd(x))
			if(pbreaks[pbins[i,j]]>.05) bhat<-0 #Always generate it actually from a null in this case
			y<-x*bhat+e
			x4[[i]][[j]]<-x
			y4[[i]][[j]]<-y
			pi<-summary(lm(y~x))$coef[2,4]
			p4[i,j]<-pi
			
			binj<-min(which(pi<pbreaks))
			if(binj==pbins[i,j]){ tryagain<-F}
		}
	}
}


#HERE WE'LL SKIP REPLICATES
# num_new_reps<-floor(nreps/10)
# newreps<-sample(1:nreps,num_new_reps,replace=F)
# for(i in 1:num_new_reps){
	# newpos<-i+nreps
	# oldpos<-newreps[i]
	# n4<-rbind(n4,n4[oldpos,])
	# p4<-rbind(p4,p4[oldpos,])
	# n4same[newpos]<-n4same[oldpos]
	# pbins<-rbind(pbins,pbins[oldpos,])
	# x4[[newpos]]<-x4[[oldpos]]
	# y4[[newpos]]<-y4[[oldpos]]
# }
# b4replicates4<-c(1:nreps,newreps)


setwd( "C:/Users/Aaron/Dropbox/EDA project/SURVEY")
save(list=c('n4','nreps','p4','pbins','n4same','pbreaks','x4','y4'),file='data_for_4plots_pilot.RData')


##Practice Graphing it
testit<-F
if(testit==T){
for(i in 1:nreps){
par(mfrow=c(2,2))
plot(x4[[i]][[1]],y4[[i]][[1]],main=paste(nes4[i],1))
plot(x4[[i]][[2]],y4[[i]][[2]],main=paste(nes4[i],2))
plot(x4[[i]][[3]],y4[[i]][[3]],main=paste(nes4[i],3))
plot(x4[[i]][[4]],y4[[i]][[4]],main=paste(nes4[i],4))
print(round(p4[i,],digits=5))
readline(prompt="go")
}
}


