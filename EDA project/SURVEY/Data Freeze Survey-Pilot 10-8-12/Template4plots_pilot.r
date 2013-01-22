###################################################################
###################### CODE SECTION ###############################
###################################################################

template.type <- 2

##### AF CODE ####

###### AF CODE
roundpval<-function(pv){
	rpv<-formatC(pv, digits = 2, format = "f")
	if(pv<.1) rpv<-formatC(pv, digits = 3, format = "f")
	if(pv<.01) rpv<-formatC(pv, digits = 4, format = "f")
	if(pv<.0001) rpv<-formatC(pv, digits = 6, format = "f")
	if(pv<.00001) rpv<-"<.00001"
	return(rpv)
}

###################################################
olddir<-getwd()
setwd("C:/Users/Aaron/Dropbox/EDA project/SURVEY")
load('data_for_4plots_pilot.RData')
setwd(olddir)

tick4plots<-tick4plots+1
i<-tick4plots



###################################################################
###################### ANSWER SECTION #############################
###################################################################

###### Record which dataset(s) (1, 2, 3, or 4) has a p-value
###### below 0.05 as "correct.choice" (replace the current "NA" value)
###### Examples of possible values: c(1), c(1,2), c("NA")
correct.choice <- which(p4[i,]<.05)
if(length(correct.choice)==0) correct.choice<-"NA" #??? right AF???

###### Assign the true p-value for dataset 1 to "answer.1"
answer.1 <- roundpval(p4[1,1])

###### Assign the true p-value for dataset 1 to "answer.1"
answer.2 <- roundpval(p4[1,2])

###### Assign the true p-value for dataset 1 to "answer.1"
answer.3 <- roundpval(p4[1,3])

###### Assign the true p-value for dataset 1 to "answer.1"
answer.4 <- roundpval(p4[1,4])

###################################################################
###################### FIGURE SECTION #############################
###################################################################

png("FigureQuestion1.png", width = 400, height = 400)
	plot(x4[[i]][[1]],y4[[i]][[1]],main="Sample 1",xlab="X",ylab="Y")
dev.off()

png("FigureQuestion2.png", width = 400, height = 400)
	plot(x4[[i]][[2]],y4[[i]][[2]],main="Sample 2",xlab="X",ylab="Y")
dev.off()

png("FigureQuestion3.png", width = 400, height = 400)
	plot(x4[[i]][[3]],y4[[i]][[3]],main="Sample 3",xlab="X",ylab="Y")
dev.off()

png("FigureQuestion4.png", width = 400, height = 400)
	plot(x4[[i]][[4]],y4[[i]][[4]],main="Sample 4",xlab="X",ylab="Y")
dev.off()
