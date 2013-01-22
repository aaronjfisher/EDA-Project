#N and number of clusters is determined independently for each plot (maybe a bad idea?)
#The minimum size for a cluster is now 10 people
#cluster number ranges from 2 to 10, sample size from 170-200

set.seed(587320)
library(MASS)

setwd("C:/Users/Aaron/Documents/JH/EDA project")
nreps<-10
#how many clusters each rep has
clusters<-rep(2:10,length=nreps)[sample(nreps)]
Npre<-sample(170:200,nreps) #this is almost what the final N vector will be
N<-Npre
samples<-list()
refmats<-list()
switchups<-list()

for(i in 1:nreps) {
	print(i)
	ci<-clusters[i]
	
	gettingprops<-T
	iter<-0
	while(gettingprops==T){
		props1<-runif(ci,0,1)
		props2<-props1/sum(props1)
		groups1<-floor(props2*Npre[i])
		if(min(groups1)>=10) gettingprops<-F
		iter<-iter+1
	}
	N[i]<-sum(groups1)
	switchup<-sample(N[i]) #add randomn shuffling!
	groupvec<-rep(1:ci,times=groups1)
		
	mati<-matrix(0,nrow=N[i],ncol=N[i]) #ensure diag is zero
	rownames(mati)<-groupvec
	colnames(mati)<-groupvec
	
	#Make mean dists
	distmu<-diag(rep(0,times=ci))
	for(a in 1:ci){
		for(b in 1:ci){
			if(a!=b){
				distab<-runif(1,0,10)
				distmu[a,b]<-distab
				distmu[b,a]<-distab
			}
		}
	}
	
	#now make full dist mat
	refmat<-mati
	for(a in 1:N[i]){
		for(b in 1:N[i]){
			if(a!=b){
				ca<-groupvec[a]
				cb<-groupvec[b]
				distab<-rnorm(1,distmu[ca,cb],sd=2.5)
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

png(file='HeatMapAnswers.png',width=1000,height=500)
	par(mfcol=c(2,5))
	for(i in 1:5){
		if (i<=nreps){
			image(samples[[i]], main=paste("Plot",i))
			image(refmats[[i]], main=paste("True Distance",i))
		}
	}
dev.off()

pdf(file="Heatmaps.pdf",height=5,width=5)
	for(i in 1:nreps){
		heatmap(samples[[i]])
		print(i)
	}
dev.off()