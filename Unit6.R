#UNIT 6
#CLUSTERING

movieLens <- read.table("./data/movieLens.txt", header = FALSE, sep = "|", quote = "\"") #"\"" to read in data easily

str(movieLens)

##Giving new column names
colnames(movieLens) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

##To remove variables from data set
movieLens$ID = NULL
movieLens$ReleaseDate = NULL
movieLens$VideoReleaseDate = NULL
movieLens$IMDB = NULL


##Removing duplicates from the dataset

movies <- unique(movieLens)


##Quick question
table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movieLens$Drama)


#Hierarchical Clustering

##Finding distances b/w all data points
distances <- dist(movies[2:20], method = "euclidean")

##Creating clusters

clusterMovies <- hclust(distances, method = "ward.D")

plot(clusterMovies) #Dendrogram of the cluster

##Label data points according to cluster

clusterGroups <- cutree(clusterMovies, k = 10) #10 clusters

##Compute the % of movies in each Genre and cluster
tapply(movies$Action, clusterGroups, mean) #Divides data into 10 cluster and calculates average value for Action for each cluster (% movies in cluster belong to Action)

tapply(movies$Romance, clusterGroups, mean) #For Romance

## Can create a cross table for the same^

##Using clusters in a Recommendation system

subset(movies, Title == "Men in Black (1997)")

##Cluster for 257th row (MIB) in data

clusterGroups[257] #cluster number

cluster2 <- subset(movies, clusterGroups == 2)

cluster2$Title[1:10] #Good movies to recommend for someone who watched MIB

#Quick Question
clusterGroups2 <- cutree(clusterMovies, k = 2) #2 clusters

plot(clusterGroups2)

table(clusterGroups2, movies$Drama)


# IMAGE SEGMENTATION

flower <- read.csv("./data/flower.csv", header = F)

## Create Matrix
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

## Converting Matrix to vector
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

## Creating distance Matrix

distance <- dist(flowerVector, method = "euclidean")

## Creating Hierarchical Clusters

clusterIntensity <- hclust(distance, method = "ward") #ward's method = min variance method (compact spherical clusters)

## Plot Dendrogram
plot(clusterIntensity)

## Choose number of clusters
rect.hclust(clusterIntensity, k = 3, border = "red")

## Split data into 3 clusters

flowerClusters <- cutree(clusterIntensity, k = 3)
flowerClusters #cluster numbers

tapply(flowerVector, flowerClusters, mean) #avg intensity values per cluster

dim(flowerClusters) = c(50,50) #converting flower clusters vector to a matrix

image(flowerClusters, axes = F) #Clustered image
image(flowerMatrix, axes = F,col = grey(seq(0,1, length = 256))) #Original image

## Clustering MRI Image of the Brain

healthy <- read.csv("./data/healthy.csv", header = F)

healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)


image(healthyMatrix, axes = F, col = grey(seq(0,1, length = 256)))

## Isolate substances using Hierarchical clustering

healthyVector <- as.vector(healthyMatrix)

str(healthyVector)
n = 365636 #Can't use Hierarchical Clustering

## Use K-Means Clustering

k <- 5
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)

str(KMC)

healthyClusters <- KMC$cluster
KMC$centers #Mean intensity for each cluster

dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = F, col = rainbow(k))

## Identifying tumors

tumor <- read.csv("./data/tumor.csv", header = F)

tumorMatrix <- as.matrix(tumor)
tumorVector <- as.matrix(tumorMatrix)

## Apply the clusters created on healthy image and apply on tumor vector, healthy vector training set, tumor vector testing set


