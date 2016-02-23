install.packages("ggplot2")
install.packages("gplots")
install.packages("pvclust")
library(gplots)
library(ggplot2)
library(rpart)
library(pvclust)
library(StatMatch)

#A good dataset mtcars from rpart
#[, 1]	 mpg	 Miles/(US) gallon
#[, 2]	 cyl	 Number of cylinders
#[, 3]	 disp	 Displacement (cu.in.)
#[, 4]	 hp	 Gross horsepower
#[, 5]	 drat	 Rear axle ratio
#[, 6]	 wt	 Weight (lb/1000)
#[, 7]	 qsec	 1/4 mile time
#[, 8]	 vs	 V/S
#[, 9]	 am	 Transmission (0 = automatic, 1 = manual)
#[,10]	 gear	 Number of forward gears
#[,11]	 carb	 Number of carburetors


x <- mtcars["Honda Civic",] 
y <- mtcars["Camaro Z28",] 
dist(rbind(x, y)) 
rnames<-attributes(mtcars)$row.names

z <- mtcars["Pontiac Firebird",] 
dist(rbind(y, z)) 


dst<-as.matrix(dist(as.matrix(mtcars)))
dim <- ncol(dst)

persp(1:dim, 1:dim, dst)
image(1:dim, 1:dim, dst,xlab="",ylab="",axes=FALSE)
axis(1, 1:dim, rnames, cex.axis = 0.5, las=3)
axis(2, 1:dim, rnames, cex.axis = 0.5, las=1)

#@@@@
#@@@@ Some Clustering
#Mahalanobis distance
dst<-as.matrix(mahalanobis.dist(as.matrix(mtcars)))

heatmap(dst,symm=T)
#dst<-as.matrix(dist(as.matrix(mtcars),method="manhattan"))
heatmap.2(dst, trace="none")


#@@@ Scaling by median and mad

medians = apply(mtcars,2,median)
mads = apply(mtcars,2,mad)
cars.use = scale(mtcars,center=medians,scale=mads)

# Eucledian Distance of the scaled values
cars.dist = dist(cars.use)
cars.dist<-dst
cov(cars.use)


# Hierarchical cluster 
#hclust uses the complete linkage method by default. 
# defines the cluster distance as the maximum distance between their individual components.
#At every stage of the clustering process, the two nearest clusters are merged into a new cluster. 
#The process is repeated until the whole data set is agglomerated into one single cluster. 
cars.hclust = hclust(cars.dist)
# plot the hierarchical cluster
plot(cars.hclust,labels=cars$Car,main='Default from hclust')
box()

#Determine how many groups do we have
groups.3 = cutree(cars.hclust,3)
table(groups.3)

#How many cars are in each of the groups
counts = sapply(2:6,function(ncl)table(cutree(cars.hclust,ncl)))
names(counts) = 2:6
counts

#Which cars are in group 1
attributes(mtcars)$row.names[groups.3 == 1]

# which cars are in each group
sapply(unique(groups.3),function(g)attributes(mtcars)$row.names[groups.3 == g])

# What happens when we use the four cluster solution 
groups.4 = cutree(cars.hclust,4)
sapply(unique(groups.4),function(g)attributes(mtcars)$row.names[groups.4 == g])

#Check cluster association with the number of carburators
#@@@ Need ot add a country variable and look how does it work with that variable
table(groups.3,mtcars$carb)

#See distribution of clusters by variables
aggregate(mtcars,list(groups.3),median)

# Add numbers of observations
a3 = aggregate(mtcars,list(groups.3),median)
data.frame(Cluster=a3[,1],Freq=as.vector(table(groups.3)),a3[,-1])

#Compare with 4 cluster solution
a4 = aggregate(mtcars,list(groups.4),median)
data.frame(Cluster=a4[,1],Freq=as.vector(table(groups.4)),a4[,-1])


#@@@@@ Bootsrap clustering
pv <- pvclust(scale(t(mtcars)), method.dist="correlation", method.hclust="complete", nboot=10) 
# Perform the hierarchical cluster analysis. Due to time resrictions, we are using here only 10 bootstrap repetitions. 
# Usually, one should use at least 1000 repetitions.
box()
plot(pv, hang=-1); 
pvrect(pv, alpha=0.95) 
# Plots result as a dendrogram where the significant clusters are highlighted with red rectangles.

#@@@@@@@@@@@@ Partitioning around Median PAM (K-Means)
library(cluster)
cars.pam = pam(mtcars,3)
#Can do clustering of distance matrix
cars.pam = pam(dst,3)
names(cars.pam)

#Compare the clusters in hierarchical and non-hierarchical clustering
table(cars.pam$clustering)
table(groups.3,cars.pam$clustering)
