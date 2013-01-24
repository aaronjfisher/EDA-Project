###################################################################
###################### CODE SECTION ###############################
###################################################################

template.type <- 3

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
load('data_for_2compare_pilot.RData')
setwd(olddir)

tick2comp<-tick2comp+1
i<-tick2comp


###################################################################
###################### FIGURE SECTION #############################
###################################################################

png("FigureQuestion1.png", width = 400, height = 400)
	plot(x2[[i]][[1]],y2[[i]][[1]],main="Sample 1",xlab="X",ylab="Y")
dev.off()

png("FigureQuestion2.png", width = 400, height = 400)
	plot(x2[[i]][[2]],y2[[i]][[2]],main="Sample 2",xlab="X",ylab="Y")
dev.off()


###################################################################
###################### ANSWER SECTION #############################
###################################################################

smallerp<-which(p2[i,]==min(p2[i,]))


###### Record with dataset (1 or 2) has the lowest
###### p-value as "correct.choice" (replace the current "NA" value)
correct.choice <- c("Sample 1", "Sample 2")[smallerp]
### ??? Add option for "They're basically the same?" AF???

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.1 <- NA
answer.1 <- roundpval(p2[i,1])

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.2 <- NA
answer.2 <- roundpval(p2[i,2])


