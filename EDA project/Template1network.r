##########################################################
###################### CODE SECTION #######################
##########################################################

library(rjson)
template.type <- 1


##########################################################
#AF CODE

roundpval<-function(pv){
	rpv<-formatC(pv, digits = 2, format = "f")
	if(pv<.1) rpv<-formatC(pv, digits = 3, format = "f")
	if(pv<.01) rpv<-formatC(pv, digits = 4, format = "f")
	if(pv<.0001) rpv<-formatC(pv, digits = 6, format = "f")
	if(pv<.00001) rpv<-"<.00001"
	return(rpv)
}


#########################
#Get data

olddir<-getwd()
setwd("C:/Users/Aaron/Dropbox/EDA project")
load('data_for_clusts_network.RData')
setwd(olddir)

#beforeRep has nreps*2 unique values, and 6 indexes at the end refering to the original index of the graphs that are replicated

#tick1plots<-0
tick1clusts<-tick1clusts+1
i<-tick1clusts


###################################################
############### ANSWER SECTION ####################
###################################################

###### Assign the true p-value for the data to "answer"
###### (change from the current "NA" value)
answer <- NA
answer <- clusters[i]

########################################################
################ FIGURE SECTION ########################
#######################################################


s<-samples[[i]]
s<-s[1:90,1:90]
nn<-dim(s)[1] # number of nodes


#Making Pairwise distances for network plots
#count the unique pairs
cutoff<-10
tab<-c()
for(i in 1:(dim(s)[1]-1)){
	for(j in (i+1):dim(s)[2]){ #Don't include the diagonal!
		newrow<-1:3
		newrow[1]<-i
		newrow[2]<-j
		newrow[3]<-ceiling(sqrt(s[i,j]))+1
		if(newrow[3]<=cutoff) tab<-rbind(tab,newrow) #re-write on the next one
	}
}
rownames(tab)<-NULL

##FOR REFERENCE: columns of tab are "source","target", and "value"


nodenames<-(paste("node",1:nn,sep='-'))
nodelist<-list()
for(i in 1:nn) {
	nodelist[[i]]<-list(name=nodenames[i],group=i)
}

##FOR REFERENCE: columns of tab are "source","target", and "value". Target is off by an index for some reason.
linklist<-list()
for(i in 1:nn) {
	linklist[[i]]<-list(source=tab[i,1],target=tab[i,2]-1,value=tab[i,3])
}




out<-toJSON(list(nodes=nodelist,links=linklist))
cat(out,file='test.json')
#x<-fromJSON(file="http://mbostock.github.com/d3/ex/miserables.json")

