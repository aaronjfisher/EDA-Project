#N and number of clusters is determined independently for each plot (maybe a bad idea?)
#The minimum size for a cluster is now 10 people
#cluster number ranges from 2 to 10, sample size from 170-200

setwd("C:/Users/Aaron/Documents/JH/EDA project")

set.seed(5894220)
library(MASS)

###Initial Settings
nreps<-40 #number of plots
#how many clusters each rep has
clusters<-rep(2:15,length=nreps)[sample(nreps)]
N<-sample(190:230,nreps,replace=T) #Number in eachs sample

###To store data
samples<-list()
refmats<-list()
switchups<-list()



###Generate Data
for(i in 1:nreps) {
	print(i)
	ci<-clusters[i]
	
	##Generate proportions of people in each cluster so that no cluster has less than 10 people
	groupvec<-1:N[i]
	for(j in 1:(ci*10)){
		groupvec[j]<-ceiling((j)/10)
	}
	remaining<-(ci*10+1):N[i]
	nremaining<-length(remaining)
	groupvec[remaining]<-sample(1:ci,nremaining,replace=T,prob=runif(ci,0,1))
	switchup<-sample(N[i]) #add randomn shuffling!
	groupvec<-groupvec[switchup]

		
	##To store distances between people (initialize here)
	mati<-matrix(0,nrow=N[i],ncol=N[i]) #ensure diag is zero
	rownames(mati)<-groupvec
	colnames(mati)<-groupvec
	
	##Make mean dists for between each cluster
	##ensure dist(a,a)=0
	distmu<-diag(rep(0,times=ci))
	for(a in 1:ci){
		for(b in 1:ci){
			if(a!=b){
				distab<-runif(1,2,5) #have to be some distance apart
				distmu[a,b]<-distab
				distmu[b,a]<-distab
			}
		}
	}
	
	#now make full dist mat for people
	refmat<-mati
	for(a in 1:N[i]){
		for(b in 1:N[i]){
			if(a!=b){
				ca<-groupvec[a] #cluster a
				cb<-groupvec[b]
				distab<-rnorm(1,distmu[ca,cb],sd=1.5)
				mati[a,b]<-distab^2
				mati[b,a]<-distab^2
				refmat[a,b]<-distmu[ca,cb]
			}
		}
	}
	
	##image(mati)
	##heatmap(mati)
	refmats[[i]]<-refmat
	samples[[i]]<-mati
	switchups[[i]]<-switchup
}
head(clusters)
heatmap(samples[[1]])
heatmap(samples[[2]])
heatmap(samples[[3]])
heatmap(samples[[4]])

getwd()
setwd("C:/Users/Aaron/Dropbox/EDA project")
save(list=c('clusters','nreps','refmats','samples','switchups'),file="data_for_clusts.RData")




##Making Pairwise distances for network plots
# mat<-samples[[1]]
# tab<-matrix(NA, ncol=3,nrow=length(mat))
# row<-0
# for(i in 1:dim(mat)[1]){
	# for(j in 1:dim(mat)[2]){
		# row<-row+1
		# tab[row,1]<-i
		# tab[row,2]<-j
		# tab[row,3]<-mat[r,j]
	# }
# }
# write.table(tab,file="tabfornetworkplots.txt",row.names=F)


#############################################################
####Checking what some look like.

# png(file='HeatMapAnswers.png',width=1000,height=500)
	# par(mfcol=c(2,5))
	# for(i in 1:5){
		# if (i<=nreps){
			# image(samples[[i]], main=paste("Plot",i))
			# image(refmats[[i]], main=paste("True Distance",i))
		# }
	# }
# dev.off()

# pdf(file="Heatmaps.pdf",height=5,width=5)
	# for(i in 1:nreps){
		# heatmap(samples[[i]])
		# print(i)
	# }
# dev.off()