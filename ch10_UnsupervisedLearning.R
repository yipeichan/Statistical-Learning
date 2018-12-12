# Principal Components
#====================
# use the `USArrests` data (which is in R)

dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2, var)

# standardize the variables when we perform PCA
pca.out=prcomp(USArrests, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)


# K-Means Clustering
# ==================
#  shifting the means of the points around.
set.seed(101)
x=matrix(rnorm(100*2),100,2)
xmean=matrix(rnorm(8,sd=4),4,2)
which=sample(1:4,100,replace=TRUE)
x=x+xmean[which,]
plot(x,col=which,pch=19)

km.out=kmeans(x,4,nstart=15)
km.out
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)


# Hierarchical Clustering
# =======================
hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)
hc.single=hclust(dist(x),method="single")
plot(hc.single)
hc.average=hclust(dist(x),method="average")
plot(hc.average)

#use the function `cutree` to cut the tree at level 4.
#This will produce a vector of numbers from 1 to 4, saying which branch each observation is on. 

# use `table` to see how well they match:

hc.cut=cutree(hc.complete,4)
table(hc.cut,which)
table(hc.cut,km.out$cluster)

#or use group membership as labels for the leaves of the dendrogram:
plot(hc.complete,labels=which)

