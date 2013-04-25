
#Cleaning and analyzing the p-value quiz data


#############
#NOTES - Thoughts about the project in general
##Do you think this p-value is strong, very strong ...
##do you think this graph is strong, very strong, ...
#############


#########################################################
########                SETUP                  ##########
#########################################################


library(plotrix)




#########################################################
########           TIDY UP THE DATA            ##########
#########################################################



mydir<-"/Users/aaronfisher/Documents/JH/EDA Versions/EDA Git Repo/Coursera/quiz_responses" 

setwd(mydir)

x<-scan("[00000012] Quiz Responses [160].txt", what="character", sep=NULL)
breaks<-c(1,which(x=="<br><br>"),length(x)-1)

nLines<-length(breaks)-2
y<-rep('',nLines)
for(i in 3:(nLines+2)){ #don't actually want the first line
  startInd<-breaks[i-1]+1 #in first line we'll miss a useless word
  stopInd<-breaks[i]-1
  if(i==nLines+2) stopInd<-length(x)
  y[i-2]<-paste(x[startInd:stopInd],collapse=' ')
}

afterWord<-function(pattern,x){
  i<-regexpr(pattern,x)
  return(i[1]+unlist(attributes(i)[1]))
}

filename<-rep('',nLines)
qNum<-rep('',nLines)
datVer<-rep('',nLines)
style<-rep('',nLines)
pval<-rep('',nLines)
sig<-rep('',nLines)
notSig<-rep('',nLines)
trueSig<-rep(T,nLines)
right<-rep('',nLines)
wrong<-rep('',nLines)
sense<-rep(NA,nLines)
spec<-rep(NA,nLines)

for(i in 1:nLines){
  
  #Latent Truth Data
  line<-y[i]
  startFile<-afterWord("%23",line)
  stopFile<-regexpr('.png',line)[1]-1
  filename[i]<-substr(line,startFile,stopFile)
  
  pieces<-strsplit(filename[i],"_")[[1]]
  qNum[i]<-pieces[1]
  style[i]<-pieces[3]
  datVer[i]<-strsplit(pieces[2],split='-')[[1]][2]
  pval[i]<-strsplit(pieces[4],split='-')[[1]][2]
  
  #Response Data
  startAnswer<-regexpr('>',line)[1]+2
  endAnswer<-nchar(line)
  answer<-substr(line,startAnswer,endAnswer)

  pieces2<-strsplit(answer,split=c(' The '))[[1]]
  num1<-pieces2[1]
  num2<-strsplit(pieces2[2],split=c('significant. '))[[1]][2]
  if(grepl('not',pieces2[2])) {notSig[i]<-num1; sig[i]<-num2}
  else {notSig[i]<-num2; sig[i]<-num1}
  
  trueSig[i]<-eval(pval[i])<.05
  if(trueSig[i]){
    right[i]<-sig[i]
    wrong[i]<-notSig[i]
  }
  if(!trueSig[i]) {
    right[i]<-notSig[i]
    wrong[i]<-sig[i]
  }  
}

#Change formats:
pval<-as.numeric(pval)
sig<-as.numeric(sig)
notSig<-as.numeric(notSig)
right<-as.numeric(right)
wrong<-as.numeric(wrong)
datVer<-as.numeric(datVer)

#make new metrics:
total<-right+wrong
accuracy<-right/total
se.p<-sqrt( accuracy*(1-accuracy)/total )
sense<-rep(NA,nLines)
spec<-rep(NA,nLines)
sense[trueSig]<-accuracy[trueSig]
spec[!trueSig]<-accuracy[!trueSig]


d<-data.frame(qNum,datVer,style, pval,sig,notSig, trueSig,right,wrong,total, accuracy, sense, spec,se.p,
  stringsAsFactors=FALSE)
#data for testing
dt<-d[which(datVer==1),]
#rm(d)

save(file='pvalQuizData.RData', list=c('d','dt'))







####################################################
############  ANALYZE DATA        ##################
####################################################
#From here on, d is the whote data frame.
#dt is a split up version of the data to explore.
#Random effects model still not done here.

load(file='pvalQuizData.RData')

####################
#plot the accuracy
par(mfrow=c(2,1))

senseInd<-which(dt$trueSig==TRUE)
plotCI(dt$sense[senseInd],uiw=2*dt$se.p[senseInd],col=c("#f08080"),ylim=c(0,1),pch=19,cex=.5,ylab='Sensitivity',xaxt='n',xlab='')
axis(1, at=(1:8), labels=unique(dt$style) )
abline(h=.5,col='blue',lwd=2)

specInd<-which(dt$trueSig==FALSE)
plotCI(dt$spec[specInd],uiw=2*dt$se.p[specInd],col=c("darkblue"),ylim=c(0,1),pch=19,cex=.5,ylab='Specificity',xaxt='n',xlab='')
axis(1, at=(1:8), labels=unique(dt$style))
abline(h=.5,col='blue',lwd=2)
####################


#######################################
#  Sensitivity  & Specificity Comparison by Style 
#We'll loop through each style, comparing each to the "control" of n=100 (n100ref)

senseCompare<- which(  (dt$trueSig==TRUE) & (dt$style!='n100ref')  )
specCompare<- which(  (dt$trueSig==FALSE) & (dt$style!='n100ref')  )

senseTests<-list()
sensePvals<-c()
senseDiff<-c()
for(i in senseCompare){
  testMat<-as.matrix( dt[  c(which(dt$qNum==2),i) , names(dt)%in% c('sig','notSig') ] )
  test.i<-prop.test(x= testMat)
  senseTests[[ dt$style[i] ]] <- test.i
  sensePvals[which(senseCompare==i)]<-test.i$p.value
  senseDiff[which(senseCompare==i)]<- diff(test.i$estimate)
}

specTests<-list()
specPvals<-c()
specDiff<-c()
for(i in specCompare){
  testMat<-as.matrix( dt[  c(which(dt$qNum==3),i ),c( which(names(dt)=='notSig'),which(names(dt)=='sig')) ] )
  test.i<-prop.test(x= testMat)
  specTests[[ dt$style[i] ]] <- test.i
  specPvals[which(specCompare==i)]<-test.i$p.value
  specDiff[which(specCompare==i)]<- diff(test.i$estimate)
}

pvalTable<-cbind(senseDiff,sensePvals,specDiff,specPvals)
row.names(pvalTable)<-names(senseTests)
round(pvalTable,8)

#save(file='exploreEDAdata.RData',list=c('pvalTable','dt'))
#######################################



#compare stuff that should be the same
prop.test(x=as.matrix(  d[c(1,3), names(dt)%in% c('sig','notSig')]  ))
#Adding up some of the dat vers in prob 1
prop.test(x=matrix( c(52+64+91,    135+183+149,
                      83+61, 120+158),nrow=2 ))

#compare lowess to bestFit
styleSig<-paste0(dt$style,dt$trueSig)
testMat<- dt[styleSig %in%c('lowessTRUE', 'bestFitTRUE'), names(dt)%in% c('sig','notSig')] 
prop.test(x=as.matrix( testMat )) 
testMat<- dt[styleSig %in%c('lowessFALSE', 'bestFitFALSE'), names(dt)%in% c('sig','notSig')] 
prop.test(x=as.matrix( testMat )) 


# A way to aggregate over all 5 data versions for each question (as if iid)
#dAll would be the final dataframe
totalInd<-(1:16)*5
dAll<-d[totalInd,]
for(i in 1:length(totalInd)){
  iInd<-which(qNum==(qNum[totalInd])[i])
  d$pval[i]<-mean(d$pval[iInd])
  addMe<-which(names(d) %in% c('pval','notSig','sig','right','wrong','total'))
  dAll[i,addMe]<-colSums(d[iInd,addMe])
}
