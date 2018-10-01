install.packages('boot')
library('boot')
#Q1 Download the file 5.R.RData and load it into R using load("5.R.RData"). Consider the linear regression model of y on X1 and X2. What is the standard error for ??_1?
#Ans. 0.02593
load("/Users/yi-peichan/Desktop/statistical learning_???/5.R.RData")
attach(Xy)
fit <- lm(y ~ ., data = Xy)
summary(fit)


#Q2 Next, plot the data using matplot(Xy,type="l"). Which of the following do you think is most likely given what you see?
# Ans There is very strong autocorrelation between consecutive rows of the data matrix. 
# Roughly speaking, we have about 10-20 repeats of every data point, so the sample size is in effect much smaller than the number of rows (1000 in this case).
matplot(Xy,type="l")

#Q3 Now, use the (standard) bootstrap to estimate s.e.(??_1). To within 10%, what do you get?
#Ans 0.02753709
set.seed(1)
dim(Xy)
error.fn=function(data, index){
  with(data[index,],coef(lm(y ~ X1+X2)))
}
error.fn(Xy,1:1000)
error.fn (Xy,sample(1:1000,1000,replace=TRUE))
boot.out=boot(Xy,error.fn,R=1000)
boot.out
plot(boot.out)

#Q4 Finally, use the block bootstrap to estimate s.e.(??_1). Use blocks of 
# 100 contiguous observations, and resample ten whole blocks with replacement
# then paste them together to construct each bootstrap time series. 
#To within 10%, what do you get?
# ANS 0.18562784
tsboot(Xy, boot.fn.ts, R = 1000, sim = "fixed", l = 100)

