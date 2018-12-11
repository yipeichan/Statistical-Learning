## SVM
##========================================================

##Linear SVM classifier
##---------------------
# generate some data in two dimensions, and make them a little separated.
set.seed(10111)
x=matrix(rnorm(40),20,2)
y=rep(c(-1,1),c(10,10))
x[y==1,]=x[y==1,]+1
plot(x,col=y+3,pch=19)

#load the package `e1071` which contains the `svm` function we will use. We then compute the fit. 
#Notice that we have to specify a `cost` parameter, which is a tuning parameter. 
install.packages('e1071')
library('e1071')
dat=data.frame(x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit,dat)

#The support points (points on the margin, or on the wrong side of the margin) are indexed in the `$index` component of the fit.

make.grid=function(x,n=75){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}
xgrid=make.grid(x)
ygrid=predict(svmfit,xgrid)
{plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
  points(x,col=y+3,pch=19)
  points(x[svmfit$index,],pch=5,cex=2)}


##chapter 12 of ESL ("Elements of Statistical Learning").
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho
{plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
  points(x,col=y+3,pch=19)
  points(x[svmfit$index,],pch=5,cex=2)
  abline(beta0/beta[2],-beta[1]/beta[2])
  abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
  abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)}

# the tuning parameter `C` has to be selected.
# Different values will give different solutions. Rerun the code above, but using `C=1`, and see what we mean. 
#One can use cross-validation to do this.


####### Nonlinear SVM
#--------------
# Instead, we will run the SVM on some data where a non-linear boundary is called for. We will use the mixture data from ESL

load(url("http://www.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)


# plot them and fit a nonlinear SVM, using a radial kernel.
plot(x,col=y+1)
dat=data.frame(y=factor(y),x)
fit=svm(factor(y)~.,data=dat,scale=FALSE,kernel="radial",cost=5)
summary(fit)


# create a grid, as before, and make predictions on the grid.
#These data have the grid points for each variable included on the data frame.

xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
{plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
  points(x,col=y+1,pch=19)}


# If we plot its 0.5 contour, that will give us the _Bayes Decision Boundary_, which is the best one could ever do.
func=predict(fit,xgrid,decision.values=TRUE)
func=attributes(func)$decision
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
{plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
  points(x,col=y+1,pch=19)
  contour(px1,px2,matrix(func,69,99),level=0,add=TRUE)
  contour(px1,px2,matrix(prob,69,99),level=.5,add=TRUE,col="blue",lwd=2)}


