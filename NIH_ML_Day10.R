#first read-in data#
mydata = USArrests 
install.packages("mclust")
install.packages("cluster")
library(mclust)

# Prepare Data #
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata <- scale(mydata) # standardize variables

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram

k1 = 2 # eyeball the no. of clusters

# cut tree into k1 clusters
groups <- cutree(fit, k=k1)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=k1, border="red")

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #


# Use optimal no. of clusters in k-means #
k1=2

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution

# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

# Model Based Clustering
#library(mclust)

fit <- Mclust(mydata)
fit # view solution summary

fit$BIC # lookup all the options attempted
classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows

# Use only if you want to save the output
#write.table(mydata1,file.choose())#save output

# get cluster means
cmeans=aggregate(mydata.orig,by=list(classif),FUN=mean); cmeans





