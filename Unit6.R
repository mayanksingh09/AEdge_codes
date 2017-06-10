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
table(movieLens$Comedy)
table(movieLens$Western)
table(movieLens$Romance, movieLens$Drama)


#Hierarchical Clustering

##Finding distances b/w all data points
distances <- dist(movies[2:20], method = "euclidean")

##Creating clusters

clusterMovies <- hclust(distances, method = "ward")

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
clusterGroups2 <- cutree(clusterMovies, k = 2) #10 clusters

plot(clusterGroups2)

table(clusterGroups2, movies$Drama)

