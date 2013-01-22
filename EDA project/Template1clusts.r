##########################################################
###################### CODE SECTION #######################
##########################################################

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


#################################
#Get data

olddir<-getwd()
setwd("C:/Users/Aaron/Documents/JH/EDA project")
load('data_for_clusts.RData')
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

png("FigureQuestion.png", width = 500, height = 500)
	heatmap(samples[[i]])
dev.off()
