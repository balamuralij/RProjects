

# Loading data
data<-iris[,-c(5)]
rawdata<-iris
# To standarize the variables 
data = scale(data) 
# Assessing cluster tendency
install.packages("clustertend")
library(clustertend)

# Compute Hopkins statistic for the dataset
set.seed(234)
#?hopkins
hopkins(data, n = nrow(data)-1)
#Since the H value = 0.1815 which is far below the threshold 0.5, it is highly clusterable
####################### K Means clustering 
# K-mean - Determining optimal number of clusters
# NbClust Package : 30 indices to determine the number of clusters in a dataset
# If index = 'all' - run 30 indices to determine the optimal no. of clusters
# If index = "silhouette" - It is a measure to estimate the dissimilarity between clusters.
# A higher silhouette width is preferred to determine the optimal number of clusters
install.packages("NbClust")
library("NbClust")
?NbClust
nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans",
              index = "silhouette")
nb$All.index
nb$Best.nc

#Method II : Same Silhouette Width analysis with fpc package
install.packages("fpc")
library(fpc)
?pamk
pamkClus <- pamk(data, krange = 2:15, criterion="multiasw", ns=2, critout=TRUE)
pamkClus$nc
cat("number of clusters estimated by optimum average silhouette width:", pamkClus$nc, "\n")

?apply

#Method III : Scree plot to determine the number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(data,centers=i)$withinss)
} 
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(data,pamkClus$nc)
# get cluster means
aggregate(data,by=list(fit$cluster),FUN=mean)
?aggregate

# append cluster assignment
data <- data.frame(data, clusterid=fit$cluster)
View(data)
?summary
