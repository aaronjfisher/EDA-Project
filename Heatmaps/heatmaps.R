########################################################
##############          FUNCTIONS          #############
########################################################

# Load Aaron's little functions for plotting with colorspace
library(colorspace)
library(plotrix)

pal <- function(col, border = "light gray", ...)
#Copy pasted from HCL-Based Color Palettes in R
#http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
 {
 n <- length(col)
 plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
 axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
 }
#rainbow_hcl(10)
#pal(rainbow_hcl(50,c=100,l=80))
#pal(sequential_hcl(50))

c.r.hcl<-function(x,n=1000, ...){ #cut rainbow_hcl
  xCut<-cut(x,breaks=n)
	colors<-rainbow_hcl(n=n, ...)
	out<-colors[xCut]
	return(out)
}
c.s.hcl<-function(x,n=1000, ...){ #cut sequantial_hcl
	xCut<-cut(x,breaks=n)
	colors<-sequential_hcl(n=n, ...)[n:1]#reverse the order
	out<-colors[xCut]
	return(out)
}
c.d.hcl<-function(x,n=1000, ...){ #cut divergent_hcl
	xCut<-cut(x,breaks=n)
	colors<-diverge_hcl(n=n, ...)
	out<-colors[xCut]
	return(out)
}



########################################################
##############          HEATMAPS           #############
########################################################

mycol<-c.d.hcl(1:100,h=c(0,180),c=100)
pal(mycol)

X = matrix(rnorm(100*10),nrow=100)
heatmap(X,col=mycol)

# Add a signal to the last 10 "voxels" that creates two clusters of "people"
X = X + c(rep(0,90),rnorm(n=10,mean=6)) %*% t(rep(c(0,1),each=5))
heatmap(X,col=mycol)

# Add another signal
X = X + c(rep(0,30),rnorm(n=10,mean=-6),rep(0,60)) %*% t(rep(c(0,1),each=5))
heatmap(X,col=mycol)
color.legend(1.01, 0,1.05,1,legend=c('low values','high values'), rect.col=mycol, align='rb',gradient='y', cex=.5)
