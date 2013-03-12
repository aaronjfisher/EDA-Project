########################################################
##############          FUNCTIONS          #############
########################################################

# Load Aaron's little functions for plotting with colorspace
library(colorspace)
library(plotrix)
library(pheatmap)

pal <- function(col, border = "light gray", ...)
#Copy pasted from HCL-Based Color Palettes in R
#http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
 {
 n <- length(col)
 plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
 axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
 }
#rainbow_hcl(10)
#pal(rainbow_hcl(50,c=100,l=80))
#pal(sequential_hcl(50))


c.r.hcl<-function(x,n=1000, ...){ #cut rainbow_hcl
	xCut<-cut(x,breaks=n)
	colors<-rainbow_hcl(n=n, ...)
	out<-colors[xCut]
	return(out)
}
c.s.hcl<-function(x,n=1000,only.up.to=n, ...){ #cut sequantial_hcl
	xCut<-1 #these two lines guard against errors caused when this function is piped into c.d.hcl
	if(only.up.to>1) xCut <-cut(x,breaks=only.up.to)
	colors<-sequential_hcl(n=n, ...)[n:(n-only.up.to)]#reverse the order
	#if you don't want the full sequence, this will chop it short!
	out<-colors[xCut]
	return(out)
}

c.d.hcl<-function(x,n=1000, h=c(0,260),c=80,...){ #cut divergent_hcl
#c is the max chroma, 
#hues: 0 is low, 260 is the higher.
	xNegInd <- which(x<0)
	xPosInd <- which(x>=0)
	
	nNeg <- length(xNegInd) #so we only get more faded parts on the side of the spectrum that doesn't have the same magnitude
	nPos <- length(xPosInd)
	biggerN<-max(nPos,nNeg)
	
	out<-rep(0,length(x))	
	if(!length(xNegInd)==0){
		xNegCol<-c.s.hcl(abs(x[xNegInd]),n=biggerN,only.up.to=nNeg,h=h[1],c=c(c,0),...)
		out[xNegInd]<-xNegCol
	}
	if(!length(xPosInd)==0){
		xPosCol<-c.s.hcl(x[xPosInd],n=biggerN,only.up.to=nPos,h=h[2],c=c(c,0),...)
		out[xPosInd]<-xPosCol
	}

	return(out)
}



########################################################
##############          HEATMAPS           #############
########################################################


set.seed(148192)
setwd('~/Documents/JH/EDA Versions/EDA Git Repo/Coursera')

par(mfrow=c(3,1))
testcol<-c.r.hcl(0:18*20,c=60,l=50)
pal(testcol)
mycol<-c.d.hcl(-80:80,h=c(295,40),c=120)
pal(mycol)
mycol<-c.d.hcl(-80:80,h=c(0,260),c=80,l=c(30,90))
pal(mycol)

#6 different group sizes, 2:7
#3 difficulty levels, with different max magnitudes of signal

N<-40
p<-500
sd.x<-1

groupsizes<-rep(2:7,times=3)
magnitudes<-rep(c(3,2,1)*sd.x,each=6)
Xes<-list()

maxIndex<-6*3
pb<-txtProgressBar(min = 1, max = maxIndex,  char = "=", style = 3)

for(i in 1:length(magnitudes)){
	setTxtProgressBar(pb,i)

	mag.i<-magnitudes[i] #magnitude of max possible signal
	ngroups.i<-groupsizes[i]

	X = matrix(rnorm(N*p,mean=0,sd=sd.x),nrow=p)

	#Assign Groups
	smallestGroupSize<-0
	while(smallestGroupSize<4) {
		groupId<-sample(ngroups.i,N,replace=T)
		smallestGroupSize <- min(table(groupId))
	}
	#heatmap(X,col=mycol)

	#Add signals
	for(j in 2:ngroups.i){ #leave first group as ref
		sig.j<-runif(p,-1*mag.i,mag.i)
		X[,groupId==j] <- 	X[,groupId==j] + sig.j
	}
	QNum<-which(unique(magnitudes)==mag.i)
	VerNum<-which(unique(groupsizes)==ngroups.i)
	
	X.ind<-heatmap(X,col=mycol)
	dev.off()
	
	XSort<-X[X.ind$rowInd[p:1],]
	filename.i<-paste0('Heatmap_Images/' , 'Q-', QNum,'_Ver-', VerNum,'_Mag-',mag.i,'_ngroups-',ngroups.i,'.png')
	png(filename=filename.i,height=600,width=600)
	pheatmap(XSort,cluster_rows=FALSE,col=mycol)
	dev.off()

	filename.i<-paste0('Dendrogram_Images/' , 'Q-', QNum,'_Ver-', VerNum,'_Mag-',mag.i,'_ngroups-',ngroups.i,'.png')
	png(filename=filename.i,height=450,width=450)
	dx<-dist(t(X))
	plot(hclust(dx),xlab='ID Number',sub='',ylab='Distance')
	dev.off()


	Xes[[i]]<-X
	
}
save.image('Clustering_coursera.RData')

############################################################
############################################################
#Workspace


X.ind<-heatmap(X,col=mycol)
XSort<-X[X.ind$rowInd[p:1],X.ind$colInd]
image(X)
image(XSort)

dev.off()

quartz()
png(filename='normTest.png')
heatmap(X,col=mycol)
dev.off()
#quartz()
#pheatmap(X,col=mycol,cluster_rows=FALSE)
quartz()
png(filename='prettyTest.png')
pheatmap(XSort,col=mycol,cluster_rows=FALSE,cluster_cols=TRUE)
dev.off()

xind2<-pheatmap(XSort,col=mycol,cluster_rows=FALSE,cluster_cols=TRUE)
quartz()
plot(xind2[[2]])


#hclust is doing well!!!!
X.ind<-heatmap(X,col=mycol)
XSort<-X[X.ind$rowInd[p:1],]
pheathcl<-pheatmap(XSort,cluster_rows=FALSE,plot=F)
par(mfrow=c(2,1))
dx<-dist(t(X))
plot(hclust(dx))

plot(pheathcl[[2]])
