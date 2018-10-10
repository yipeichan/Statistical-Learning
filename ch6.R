install.packages('ISLR')
library('ISLR')
summary(Hitters)
Hitters=na.omit(Hitters)  # remove missing values 
with(Hitters,sum(is.na(Salary)))

##### Best Subset regression
# use the package `leaps` to evaluate all the best-subset models.
install.packages('leaps')
library('leaps')
regfit.full=regsubsets(Salary~.,data=Hitters)
summary(regfit.full)

# It gives by default best-subsets up to size 8; lets increase that to 19, i.e. all the variables
regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
?names
{plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
  which.min(reg.summary$cp)
  points(10,reg.summary$cp[10],pch=20,col="red")}

# There is a plot method for the `regsubsets`  object
plot(regfit.full,scale="Cp")
coef(regfit.full,10)


##### Forward Stepwise Selection
# use the `regsubsets` function but specify the `method="forward" option:
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")


#### Model Selection Using a Validation Set
# make a training and validation set, so that we can choose a good subset model.
# do it using a slightly different approach from what was done in the the book.
dim(Hitters)
set.seed(1)
train=sample(seq(263),180,replace=FALSE)
train
regfit.fwd=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward")

# make predictions on the observations not used for training. 
# there are 19 models, so we set up some vectors to record the errors. 
# We have to do a bit of work here, because there is no predict method for `regsubsets`.
val.errors=rep(NA,19)
x.test=model.matrix(Salary~.,data=Hitters[-train,]) # notice the -index!
for(i in 1:19){
  coefi=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}
{plot(sqrt(val.errors),ylab="Root MSE",ylim=c(300,400),pch=19,type="b")
  points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b")   
  legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)}  #adding discription at the top right corner
# I added bracket{} to make chunks; 
# otherwise the code won't work, showing: Error in plot.xy(xy.coords(x, y), type = type, ...) : plot.new has not been called yet

