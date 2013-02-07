#SIMPLIFIED FOR COURSERA!

###### AF CODE


###################################################

setwd("/Users/aaronfisher/Documents/JH/EDA Versions/Sourcetree EDA Git Repo/Coursera - pvals - feb3")
load('data_for_2compare_coursera1.RData')


###################################################################
###################### FIGURE SECTION #############################
###################################################################
for(i in 1:nreps){
png(paste0("images/2compare_coursera1_",i,".png"), width = 800, height = 400)
  par(mfrow=c(1,2))
	plot(x2[[i]][[1]],y2[[i]][[1]],main="Sample 1",xlab="X",ylab="Y")
	plot(x2[[i]][[2]],y2[[i]][[2]],main="Sample 2",xlab="X",ylab="Y")
dev.off()
}



