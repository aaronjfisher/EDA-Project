##########################################################
###################### CODE SECTION #######################
##########################################################

#template.type <- 1
#answer<-NA

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

#No need for this wrapper.
#olddir<-getwd()
setwd("/Users/aaronfisher/Documents/JH/EDA Versions/Sourcetree EDA Git Repo/Coursera - pvals - feb3")
load('data_for_1plots_coursera1.RData')
#setwd(olddir)

#SCRATCH THIS
#DO IT ALL AT ONCE
#tick1plots<-0
#tick1plots<-tick1plots+1
#i<-tick1plots


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
#answer <- roundpval(pval)

########################################################
################ FIGURE SECTION ########################
#######################################################

#Note, this works b/c there's no replicates
for(i in 1:nreps){
  n<-nes[i]
  x<-xes[i,1:n]
  y<-yes[i,1:n]
  pval<-pvals[i]
  tval<-tvals[i]
  m<-lm(y~x)
  style<-pres[i]

  t2<-'Data'
  if(style=='lowess') t2<-'with Lowess Line'
  if(style=='bestFit') t2<-'with OLS Best Fit Line'
  title<-paste('Sample ',t2,sep='')
  xl<-paste("Cranial Electrode",floor(runif(1,11,44)),"(Standardized)")
  yl<-paste("Cranial Electrode",floor(runif(1,53,97)),"(Standardized)")
  drx<-diff(range(x))
  dry<-diff(range(y))
  
  png(paste0("images/1pval_coursera1_",i,".png"), width = 400, height = 400)
  	par(mfrow=c(1,1))
  	plot(x,y,xlab='X',ylab='Y',main=title)
  	if(style=='lowess') lines(lowess(x,y))
  	if(style=='bestFit') abline(m$coef)
  	if(style=='axesScale')plot(x,y,xlab='X',ylab='Y',main=title,xlim=c(min(x)-1.5*sd(x),max(x)+1.5*sd(x)),ylim=c(min(y)-1.5*sd(y),max(y)+1.5*sd(y)))
    if(style=='outlier')plot(x,y,xlab='X',ylab='Y',main=title,xlim=c(min(x)-.2*drx,max(x)+.2*drx),ylim=c(min(y)-.2*dry,max(y)+.2*dry) )
  	if(style=='axesLabel') plot(x,y,xlab=xl,ylab=yl,main=title)
  dev.off()
}
