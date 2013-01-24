##########################################################
###################### CODE SECTION #######################
##########################################################

template.type <- 1
answer<-NA

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
load('data_for_1plots.RData')
setwd(olddir)

#beforeRep has nreps*2 unique values, and 6 indexes at the end refering to the original index of the graphs that are replicated

#tick1plots<-0
tick1plots<-tick1plots+1

n<-nes[tick1plots]
x<-xes[tick1plots,1:n]
y<-yes[tick1plots,1:n]
pval<-pvals[tick1plots]
tval<-tvals[tick1plots]
m<-lm(y~x)
style<-pres[tick1plots]


#print(c('pval -',round(pval,digits=5),' tstat -',round(tval,digits=4),'n -',n,' tick1plots -',tick1plots,'pres -',style,'cor -',round(cor(x,y),digits=5)))

# q2<-''
# if(style=='lowess') q2<-'Here we also show a lowess line.'
# if(style=='bestFit') q2<-'Here we also show the best fit linear model through the data (from ordinary least squares regression).'
# question<-paste(q2,"<br/><br/>What do you think the p-value is?")


###################################################
############### ANSWER SECTION ####################
###################################################

###### Assign the true p-value for the data to "answer"
###### (change from the current "NA" value)
answer <- roundpval(pval)

########################################################
################ FIGURE SECTION ########################
#######################################################

t2<-'Data'
if(style=='lowess') t2<-'with Lowess Line'
if(style=='bestFit') t2<-'with OLS Best Fit Line'
title<-paste('Sample ',t2,sep='')
xl<-paste("Cranial Electrode",floor(runif(1,11,44)),"(Standardized)")
yl<-paste("Cranial Electrode",floor(runif(1,53,97)),"(Standardized)")

png("FigureQuestion.png", width = 300, height = 300)
	par(mfrow=c(1,1))
	plot(x,y,xlab='X',ylab='Y',main=title)
	if(style=='lowess') lines(lowess(x,y))
	if(style=='bestFit') abline(m$coef)
	if(style=='axesScale')plot(x,y,xlab='X',ylab='Y',main=title,xlim=c(min(x)-1.5*sd(x),max(x)+1.5*sd(x)),ylim=c(min(y)-1.5*sd(y),max(y)+1.5*sd(y)))
	if(style=='axesLabel') plot(x,y,xlab=xl,ylab=yl,main=title)
dev.off()
