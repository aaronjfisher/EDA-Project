###CORRELATION GENERATING DATA


set.seed(3419)

setwd("C:/Users/Aaron/Dropbox/EDA project/SURVEY")
nreps<-6 
corrbreaks<-c(.1,.3,.5,.7,.8,.9)
nes<-rep(100,nreps)
xes<-matrix(nrow=nreps,ncol=max(nes))
yes<-matrix(nrow=nreps,ncol=max(nes))
cors<-1



#First generate baseline data
#then add presentation data (same data + presentaion vector)
#then change data for when the twist is adding an outlier
#then add replicates of all of the vector to see how consistent people are on the exact same stuff

for(i in 1:nreps){
	n<-nes[i]
	x<-rnorm(n)
	sde<-1
	if(corrbreaks[i]>.7) sde<-runif(1,.3,.9)
	e<-rnorm(n,0,sde)
	bhat<-corrbreaks[i]
	y<-x*bhat*sample(c(-1,1),1)+e
	#print(bhat)
	cors[i]<-cor(x,y)
	xes[i,1:n]<-x
	yes[i,1:n]<-y
}
plot(xes[1,],yes[1,])
plot(xes[2,],yes[2,])
plot(xes[3,],yes[3,])
plot(xes[4,],yes[4,])
plot(xes[5,],yes[5,])
plot(xes[6,],yes[6,])

###############################################
#add same plot with new presentation style

save(list=c('xes','yes','nreps','nes','cors','corrbreaks'),file='data_for_corr_pilot.RData')

