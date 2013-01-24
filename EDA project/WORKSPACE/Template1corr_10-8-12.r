##########################################################
###################### CODE SECTION #######################
##########################################################

template.type <- 1 #<-????
answer<-NA

##########################################################
#AF CODE


# roundpval<-function(pv){
	# rpv<-formatC(pv, digits = 2, format = "f")
	# if(pv<.1) rpv<-formatC(pv, digits = 3, format = "f")
	# if(pv<.01) rpv<-formatC(pv, digits = 4, format = "f")
	# if(pv<.0001) rpv<-formatC(pv, digits = 6, format = "f")
	# if(pv<.00001) rpv<-"<.00001"
	# return(rpv)
# }


#################################
#Get data

olddir<-getwd()
setwd("C:/Users/Aaron/Dropbox/EDA project/SURVEY")
load('data_for_corr_pilot.RData')
setwd(olddir)

#beforeRep has nreps*2 unique values, and 6 indexes at the end refering to the original index of the graphs that are replicated

#tick1plots<-0
tickCorrplots<-tickCorrplots+1

n<-nes[tickCorrplots]
x<-xes[tickCorrplots,1:n]
y<-yes[tickCorrplots,1:n]
cori<-cors[tickCorrplots]


###################################################
############### ANSWER SECTION ####################
###################################################

###### Assign the true corr for the data to "answer"
###### (change from the current "NA" value)
answer <- round(cori,digits=3)

########################################################
################ FIGURE SECTION ########################
#######################################################

t2<-'Data'
title<-paste('Sample ',t2,sep='')

png("FigureQuestion.png", width = 400, height = 400)
	par(mfrow=c(1,1))
	plot(x,y,xlab='X',ylab='Y',main=title)
dev.off()
