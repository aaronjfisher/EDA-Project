library(shiny)



mainFun<-function(pval,n,x,e,nulldist){
  
  #start to get bhat by first getting close with the theory estimate

  t<-qt(pval/2,df=n-2,lower.tail=F)  
  bhat.theory<-t*sd(e)/sqrt(sum((x-mean(x))^2)  ) #sd(e)=true σ, sqrt(n)*sd(x) = Σ[(x-bar(x))^2]
  bhat<-bhat.theory
  y<-x*bhat+e
  bhat.emp<-abs(summary(lm(y~x))$coeff[2,1])
  pval.emp<-summary(lm(y~x))$coeff[2,4]
    
  getp<-function(beta){
    y<-x*beta+e
    p<-summary(lm(y~x))$coeff[2,4]
    return(p)
  }
  pval.emp<-getp(beta=bhat)
  
  #Now get a total of three betas, with at least one corresponding to a pval.emp below pval, and at least one above pval
  bhat1<-bhat.theory
  count<-0
  while(pval.emp<pval & count<100){
    bhat<-bhat*.9
    pval.emp<-getp(bhat)  
    count<-count+1
  }
  bhat2<-bhat
  count<-0
  while(pval.emp>pval & count<100 ){
    bhat<-bhat*1.1
    pval.emp<-getp(bhat)
    count<-count+1
  }
  bhat3<-bhat
  
  #then iteratively narrow down.
  #take the max & min of the three betas (possible replicates)
  #take their mean.
  #Of these three new betas (min, max & mean) keep the two with pvalues closest to the one you want
  #Set these as the new "min" & "max" and continue to narrow down!
  bmin<-min(bhat1,bhat2,bhat3)
  bmax<-max(bhat1,bhat2,bhat3)
  iter<-0
  while(iter<25&!nulldist){
    iter<-iter+1
  
    bmid<-mean(c(bmin,bmax))
    
    pmax<-getp(bmin)
    pmin<-getp(bmax)
    pmid<-getp(bmid)
    
    #print(pval>pmin & pval<pmax)#test
      
    if(pval>pmid){#bmin too small
      bmin<-bmin
      bmax<-bmid
    }
    if(pval<=pmid){#bmax too big
      bmin<-bmid
      bmax<-bmax
    }
    #print(c(iter,pmid,pval))
  }
  
  bmid<-mean(c(bmin,bmax))
  bhat<-bmid
  if(nulldist)bhat<-0
  y<-x*bhat+e
  pval.emp<-summary(lm(y~x))$coeff[2,4]
  
  return(list(t=t,e=e,x=x,bhat=bhat,y=y,pval.emp=pval.emp) )
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  all.xe<-reactive({
    set.seed(input$XEseed)
    e.all<-rnorm(2000)
    x.all<-rnorm(2000)
    list(x=x.all,e=e.all)
  })
  
  pval<-reactive({  10^(input$logp) })
  pbreaks<-reactive({  c(pval()*1.1,pval()*.9) })
  e<-reactive({ all.xe()$e[1:input$n] })
  x<-reactive({ all.xe()$x[1:input$n] })
  #seedForPlots<-234032
  #set.seed(seedForPlots)
  
  data22<-reactive({ mainFun(pval=pval(),n=input$n,x=x(),e=e(),nulldist=input$nulldist)  })
  
  coefs<-reactive({ lm(data22()$y ~ data22()$x )$coef  }) 
  lowessXY<- reactive({ lowess(data22()$x,data22()$y) })
  
  output$outplot<-renderPlot( {
      plot(data22()$x,data22()$y,xlab='X',ylab='Y')
      if(input$bestFit) abline( coefs() ,col='darkgreen',lwd=2)
      if(input$lowess) lines(lowessXY(),col='blue',lwd=2)
  })
   
     
  

#   
#   output$detailTable<-renderTable({ 
#    data.frame(matmat<-matrix(c('Model:', 'Y=α+Xβ+ε',
# 'Generating Parameters:',paste0('α=0; β =',round(data22()$bhat,digits=4)),'Fitted Parameters: ', paste0('α=',round(coefs()[1],digits=4), '; β =',round(coefs()[2],digits=4) )),byrow=TRUE,ncol=2  ))
#   })
#   
  output$detailTable<-renderTable({ 
    tabtab<- data.frame(matmat<-matrix(c(0,round(data22()$bhat,digits=4),round(coefs()[1],digits=4),round(coefs()[2],digits=4) ),byrow=TRUE,ncol=2))
    row.names(tabtab)<-c('Generating Parameters:','Fitted Parameters:')
  colnames(tabtab)<-c('α','β')
  tabtab
  },digits=4)
  
  
  #note, right now we're showing the empirical pval
  output$pval<-renderText(paste('p-value =',prettyNum(data22()$pval.emp)) ) 
  #output$test<-renderText(data22()$pval.emp)
  output$formula<-renderText('Model: Y=α+Xβ+ε')
  output$genParams<-renderText(paste0('Generating Parameters: α=0; β =',round(data22()$bhat,digits=4)) )
  output$fitParams<-renderText(paste0('Fitted Parameters: α=',round(coefs()[1],digits=4), '; β =',round(coefs()[2],digits=4) ))

})