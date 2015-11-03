# Want to complete a clustering exercise looking at the course descriptions across the University

#import all the things
library(tm)
library(dplyr)
library (ggplot2)
library (rvest)
library (xml)
library (xml2)
library(proxy)
library(cluster)

# Read in downloaded list
lou <- read.csv("courses1152Data.csv")
str(lou)
lou$number <- as.factor(lou$Number) # make a new variable containing course number as factor for grouping
lou.u <- lou %>%   # keep only one record per course 
  group_by(Mnemonic, number) %>% 
  summarize(sections=n(), students=sum(Enrollment), level=first(Number),
            title=as.character(first(Title)), 
            description=as.character(first(Description)))
str(lou.u)
lou.u <- as.data.frame(lou.u)
# Remove columns that end up empty 
lou.u <- lou.u[-c(42,144,956,979,980,1016,1222,1223,1882,2220,2273,2503,2591,2798,2839,2860,2949,2974),]
# Make separate datasets for just the undergraduate courses
u.lou <- subset(lou.u, level<5000)

# Create corpus from dataframe
docvar <- list(content="description", dept="Mnemonic", level="level")
myReader <- readTabular(mapping=docvar)

# Undergraduate
ucorpus <- Corpus(DataframeSource(u.lou), readerControl=list(reader=myReader))
ucorpus
str(ucorpus[1])

# Remove capitalization
ucorpus <- tm_map(ucorpus,content_transformer(tolower))
ucorpus[[1]][1]

# Create a function, toSpace, using tm's content_transformer
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
ucorpus <- tm_map(ucorpus, toSpace, "-")

# Remove punctuation
ucorpus <- tm_map(ucorpus, removePunctuation) 

# Remove numbers
ucorpus <- tm_map(ucorpus, removeNumbers) 

# Remove stop words
ucorpus <- tm_map(ucorpus, removeWords, stopwords("english")) 

# Strip whitespace
ucorpus <- tm_map(ucorpus, stripWhitespace)
ucorpus[[1]][1]

#Stem document
ucorpus <- tm_map(ucorpus, stemDocument)

#Make a document term matrix
louTDM <-DocumentTermMatrix(ucorpus)

louCommon <- removeSparseTerms(louTDM, 0.99)

########### Kmeans clustering ###############
louCommon2 <- weightTfIdf(louCommon)
lou.m <- as.matrix(louCommon2)

# Normalize the tf.idf matrix (for Euclidean distance)
lou.mN <- lou.m / apply(lou.m, MARGIN=1, FUN = function(x) sum(x^2)^0.5)

# Plot the within group sum of squares + look for elbow
ssPlot <- function(data, maxCluster=10) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- kmeans(data, centers=i)$tot.withinss
  }
  plot(1:maxCluster, SSw, type="b", xlab="Number of Clusters", ylab="Within sum of squares")
}
set.seed(121)
ssPlot(lou.mN)

# And cluster!
km <- kmeans(lou.mN, 6, iter.max=25, nstart=5) # try 6 groups
km.cluster <- km$cluster
table(km.cluster)

tw.pc<-prcomp(lou.mN)
summary(tw.pc) 
plot(tw.pc$x[,1:2])
# Note that the first to PCs account for only .024 and .019 of the variance.
# It therefore makes sense that this plotting shows everything clumped together
# with a couple trailing plots of points. These dimensions just don't do a lot to
# differentiate among the points.

# Add cluster assignments to dataframe (mpc added - thanks!)
u.lou$km.cluster <- as.factor(km.cluster)

# now you can look at cluster assignments, here's one way: conditional indexing
u.lou[u.lou$km.cluster==1,c("Mnemonic", "level")]

#########################################

table(km.cluster)
u.lou$description[u.lou$km.cluster==6]
# or sample from each cluster
sample_docs = list()
for (n in 1:6){
  samp <- sample(unname(which(km$cluster==n)), 6, replace=F)
  sample_docs[[n]] <- u.lou$description[samp]
}
sample_docs

# Or look at features by clusters
detach("package:plyr", unload=TRUE)
u.lou %>% 
  group_by(km.cluster) %>% 
  summarize(mean=mean(level), min=min(level), max=max(level))


# Cluster Labeling
twDF <- as.data.frame(lou.m) # coerce to dataframe
twDF <- data.frame(twDF, km$cluster) # append clusters

# Find most representative member (closest to center) as examplar
# apply this function to observations for each level of the cluster
candidates <- by(twDF[-ncol(twDF)], twDF[ncol(twDF)], function(data) {
  dists <- sapply(data, function(x) (x - mean(x))^2) # for each var, calc each ob's deviation from mean
  dists <- rowSums(dists) # for each ob, sum the deviations across vars
  rownames(data)[dists == min(dists)] # the row number of the minimum sum
})

# List representative docs
candidates # index number
candidates <- as.numeric(unlist(candidates))
candidateDescription <- u.lou[candidates, c(1,5)] 
candidateDescription

#  Function to obtain most highly weighted words in each cluster
twList <- split(twDF, twDF$km.cluster) # split into lists by cluster
# apply function to each list: sum columns/weights, sort, save top 10
topwords <- lapply(twList, function(x) { 
  sort(apply(x[,-ncol(x)], 2, sum), decreasing=TRUE)[1:10]
})
topwords

########### Hierarchical Clustering ###############

# Run clustering algorithm
# Create distance matrix
sou.cd <- dist(lou.m, method="cosine")
# And cluster
hc.cd.ward <- hclust(sou.cd, method = "ward.D2")
plot(hc.cd.ward, main="Cosine Distance, Ward's Method",
     xlab="", sub="", cex=.6)
# Add helpful rectangles around clusters
rect.hclust(hc.cd.ward, k=5, border="red")
rect.hclust(hc.cd.ward, k=8, border="blue") 

# mpc added: make cluster assignments and add to data frame (thanks!)
hc.cluster <- cutree(hc.cd.ward, k=6) 
table(hc.cluster)
# now you can add these to the data frame 
u.lou[, "hc.cluster"] <- as.factor(hc.cluster)


### thoughts for other stuff, generally ignore everything below this for the moment

# dist_mat <- as.matrix(sou.cd)
# clust_cuts <- data.frame(cut_level=1:10, 
#                          avg_size=0, avg_dist=0)
# 
# for (i in 1:(nrow(clust_cuts) - 1)) {
#   clust_cuts[clust_cuts$cut_level==i, 'avg_size'] <- 
#     mean(table(cutree(tree=hc.cd.ward, k=i)))
#   df_dist <- data.frame(doc_name=paste(u.lou$Mnemonic,u.lou$number), 
#                         clust_cut=cutree(tree=hc.cd.ward, k=i)) %>%
#     inner_join(x=., y=., by='clust_cut') %>%
#     filter(doc_name.x != doc_name.y)
#   df_dist$cos_dist <- NA
#   for (t in 1:nrow(df_dist)) {
#     df_dist$cos_dist[t] <- dist_mat[df_dist$doc_name.x[t], df_dist$doc_name.y[t]]
#   }
#   df_dist <- df_dist %>%
#     group_by(clust_cut) %>%
#     summarise(cos_dist=mean(cos_dist))
#   clust_cuts[clust_cuts$cut_level==i, 'avg_dist'] <- mean(df_dist$cos_dist)
# }
# 
# p <- ggplot(data=clust_cuts, aes(x=cut_level, y=avg_dist)) 
# p + geom_line() +
#   labs(title='Cosine Distance of Hierarchical Clusters by Cut',
#        x='Number of Clusters/Cut',
#        y='Intra-Cluster Mean Cosine Distance')  +
#   coord_cartesian(xlim=c(0,20),ylim=c(.5,1))

###########################################
