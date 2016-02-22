install.packages("ggplot2")
install.packages("gplots")
install.packages("pvclust")
library(gplots)
library(ggplot2)
library(rpart)
library(pvclust)

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
image(1:dim, 1:dim, dst,xlab="",ylab="")
axis(1, 1:dim, rnames, cex.axis = 0.5, las=3)
axis(2, 1:dim, rnames, cex.axis = 0.5, las=1)

#@@@@
#@@@@ Some Clustering

heatmap(dst,symm=T)
dst<-as.matrix(dist(as.matrix(mtcars),method="manhattan"))
heatmap.2(dst, trace="none")


#@@@ Scaling by median and mad

medians = apply(mtcars,2,median)
mads = apply(mtcars,2,mad)
cars.use = scale(mtcars,center=medians,scale=mads)

# Distance of the scaled values
cars.dist = dist(cars.use)

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









#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@  BIOLOGICAL APPLICATIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





## CRAN Packages
install.packages(c("pvclust", "biclust", "modeltools", "som", "flexclust", "cluster", "scatterplot3d", "gplots", "e1071", "kernlab"))

## BioConductor Packages 
source("http://www.bioconductor.org/biocLite.R")
biocLite(c("geneplotter", "ellipse", "ctc")) 

setwd("C:/Users/bobashev/Documents/NIHMachineLearning")

#@@@@
#@@@@	Import the data set into R
#@@@@ Arabidopsis thaliana gene expression in response to IAA treatment challange 
#@@@@
temp <- readLines("GSE1110_series_matrix.txt"); 
cat(temp[-grep("^!|^\"$", temp)], file="GSE1110clean.txt", sep="\n") 
mydata <- read.delim("GSE1110clean.txt", header=T, sep="\t") 
# These import commands include a cleanup step to get rid of annotation lines and corrupted return signs.
rownames(mydata) <- mydata[,1] 
mydata <- as.matrix(mydata[,-1]) 
# Assigns row indices and converts the data into a matrix object.


#@@@@
#@@@@	Filter the data
#@@@@
mydata <- mydata[apply(mydata > 100, 1, sum)/length(mydata[1,])>0.5 & apply(log2(mydata), 1, IQR) > 1.5, ] 
# Retrieves all rows with high intensities (50% > 100) and high variability (IQR > 1.5).
dim(mydata) # Dimension of final sample matrix 
mydata[1:10,1:10] # Slice of sample matrix

#@@@@@
#@@@	Hierarchical clustering
#@@@@@

#source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/my.colorFct.R")

mydatascale <- t(scale(t(mydata))) # Centers and scales data.
mydatascale[1:10,1:10]
mean(mydatascale[3,])
mean(mydatascale[,3])

hr <- hclust(as.dist(1-cor(t(mydatascale), method="pearson")), method="complete") # Cluster rows by Pearson correlation.
hc <- hclust(as.dist(1-cor(mydatascale, method="spearman")), method="complete") 
# Clusters columns by Spearman correlation.

heatmap(mydata, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hc), col=my.colorFct(), scale="row") 
# Plot the data table as heatmap and the cluster results as dendrograms.

mycl <- cutree(hr, h=max(hr$height)/1.5); mycolhc <- sample(rainbow(256)); 
mycolhc <- mycolhc[as.vector(mycl)]; 
heatmap(mydata, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolhc) 
# Cut the tree at specific height and color the corresponding clusters in the heatmap color bar.

#@@@@
#@@@@???	Obtain significant clusters by pvclust bootstrap analysis
#@@@@
library(pvclust); 
library(gplots) # Loads the required packages.

pv <- pvclust(scale(t(mydata)), method.dist="correlation", method.hclust="complete", nboot=10) 
# Perform the hierarchical cluster analysis. Due to time resrictions, we are using here only 10 bootstrap repetitions. 
# Usually, one should use at least 1000 repetitions.

plot(pv, hang=-1); 
pvrect(pv, alpha=0.95) 
# Plots result as a dendrogram where the significant clusters are highlighted with red rectangles.

clsig <- unlist(pvpick(pv, alpha=0.95, pv="au", type="geq", max.only=TRUE)$clusters) # Retrieve members of significant clusters.

source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/dendroCol.R") # Import tree coloring function.

dend_colored <- dendrapply(as.dendrogram(pv$hclust), dendroCol, keys=clsig, xPar="edgePar", bgr="black", fgr="red", pch=20) 
# Create dendrogram object where the significant clusters are labeled in red.

heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolhc) 
# Plot the heatmap from above, but with the significant clusters in red and the cluster bins from the tree cutting step in 
# the color bar.


x11(height=12); 
heatmap.2(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", trace="none", RowSideColors=mycolhc) # Plot heatmap with heatmap.2() function which scales better for many entries.

mydatasort <- mydata[pv$hclust$labels[pv$hclust$order], hc$labels[hc$order]] # Sort rows in data table by 'dend_colored' and its colums by 'hc'.
x11(height=16, width=12); 
par(mfrow=c(1,2)); plot(dend_colored, horiz=T, yaxt="n"); 
image(scale(t(mydatasort)), col=my.colorFct(), xaxt="n",yaxt="n") # Plot heatmap with bootstrap tree in larger format using instead of heatmap the image function.

#pdf("pvclust.pdf", height=21, width=6); plot(dend_colored, horiz=T, yaxt="n"); dev.off(); pdf("heatmap.pdf", height=20, width=6); image(scale(t(mydatasort)), col=my.colorFct(), xaxt="n",yaxt="n"); dev.off() 
# Save graphical results to two PDF files: 'pvclust.pdf' and'heatmap.pdf'.


#@@@@@
#@@@@@	Compare PAM (K-means) with hierarchical clustering
#@@@@@
library(cluster) # Loads required library.
mydist <- t(scale(t(mydata))) # Center and scale data.
mydist <- as.dist(1-cor(t(mydist), method="pearson")) # Generates distance matrix using Pearson correlation as distance method.
pamy <- pam(mydist, max(mycl)) # Clusters distance matrix into as many clusters as obtained by tree cutting step (6).
mycolkm <- sample(rainbow(256)); 
mycolkm <- mycolkm[as.vector(pamy$clustering)]
heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolkm) 
# Compare PAM clustering results with hierarchical clustering by labeling it in heatmap color bar.
#pdf("pam.pdf", height=20, width=20); heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolkm); dev.off() 
# Save graphical results to PDF file 'pvclust.pdf'.


#@@@@@
#@@@@@ Compare SOM with Hierarchical Clustering
#@@@@@

library(som) # Loads required library.
y <- t(scale(t(mydata))) # Center and scale data.
y.som <- som(y, xdim = 2, ydim = 3, topol = "hexa", neigh = "gaussian") # Performs SOM clustering.
plot(y.som) # Plots results.
#pdf("som.pdf"); 
#plot(y.som); dev.off() # Save plot to PDF 'som.pdf'.
somclid <- as.numeric(paste(y.som$visual[,1], y.som$visual[,2], sep=""))+1 # Returns SOM cluster assignment in order of input data.
mycolsom <- sample(rainbow(256)); 
mycolsom <- mycolsom[somclid]; 
heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolsom) 
# Compare SOM clustering results with hierarchical clustering by labeling it in heatmap color bar.
#pdf("somhc.pdf", height=20, width=20); heatmap(mydata, Rowv=dend_colored, Colv=as.dendrogram(hc), col=my.colorFct(), scale="row", RowSideColors=mycolsom); dev.off() # Save graphical results to PDF file 'somhc.pdf'.

#@@@@@
#@@@@@ Compare PCA with SOM 
#@@@@@
pca <- prcomp(mydata, scale=T) # Performs principal component analysis after scaling the data.
summary(pca) # Prints variance summary for all principal components.

library(scatterplot3d) # Loads 3D library.
scatterplot3d(pca$x[,1:3], pch=20, color=mycolsom) # Plots PCA result in 3D. The SOM clusters are highlighted in their color.
#pdf("pcasom.pdf"); scatterplot3d(pca$x[,1:3], pch=20, color=mycolsom); dev.off() # Saves PCA plot in PDF format 'pcasom.pdf'.


#@@@@@
#@@@@@ Compare MDS with SOM with HC with KM
#@@@@@

loc <- cmdscale(mydist, k = 3) # Performs MDS analysis and returns results for three dimensions.
x11(height=8, width=8, pointsize=12); par(mfrow=c(2,2)) # Sets plotting parameters.
plot(loc[,1:2], pch=20, col=mycolsom, main="MDS vs SOM 2D") 
# Plots MDS-SOM comparison in 2D. The SOM clusters are highlighted in their color.
scatterplot3d(loc, pch=20, color=mycolsom, main="MDS vs SOM 3D") # Plots MDS-SOM comparison in 3D.
scatterplot3d(loc, pch=20, color=mycolhc, main="MDS vs HC 3D") # Plots MDS-HC comparison.
scatterplot3d(loc, pch=20, color=mycolkm, main="MDS vs KM 3D") # Plots MDS-KM comparison. 


#@@@@@
#@@@@@ Fuzzy Clustering
#@@@@@

library(cluster) # Loads cluster library.
fannyy <- fanny(mydist, k= max(mycl), memb.exp = 1.5); round(fannyy$membership, 2); fannyy$clustering 
# Performs fuzzy clustering with as many coefficients as clusters were obtained by tree cutting step in HC. The hard 
# clustering results are provided in the 'clustering' slot.
fannyyMA <- round(fannyy$membership, 2) > 0.3; apply(fannyyMA, 1, which) 
# Returns multiple cluster memberships for coefficient above a certain value (here >0.3).








