setwd("C:/Health-Informatics-Course-Notes/plots")
#install.packages("gplots", dependencies = TRUE)
library(gplots)
#install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)
x  = as.matrix(mtcars)
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,0,length=100),  # for red
               seq(0,0.8,length=100),              # for yellow
               seq(0.8,1,length=100))              # for green
distance = dist(x, method = "manhattan")
cluster = hclust(distance, method = "ward.D")

# creates a 5 x 5 inch image
png("mtcars_cluster_htmp.png",    # create PNG for the heat map        
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8)        # smaller font size

heatmap.2(x,
          #cellnote = mat_data,  # same data set for cell labels
          main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA"
          #Rowv = as.dendrogram(cluster), # apply default clustering method
          #Colv = as.dendrogram(cluster)
          ) # apply default clustering method)            # turn off column clustering

dev.off()               # close the PNG device



##clustering  
#http://www.statmethods.net/advstats/cluster.html  
# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)





