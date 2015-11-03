# Problem Set 3
# Adriel Barrett-Johnson and Will Schwieder
# 10/24/15

# We want to complete a clustering exercise looking at the course descriptions across the University
library(tm)
library(dplyr)
library (ggplot2)
library (rvest)
library (xml)
library (xml2)
library(proxy)
library(cluster)

setwd("C:/Users/Will Schwieder/Desktop/DS TextasData/")

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

# We want to eliminate the words that do not show up more than twice among departments 
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

# So looking at the graph we think that 6 could be a good place to start for clustering groups but we are sceptical 
# because we believe there should be more than 6 clusters

# And cluster!
km <- kmeans(lou.mN, 6, iter.max=25, nstart=5) # try 6 groups
km.cluster <- km$cluster
table(km.cluster)

tw.pc<-prcomp(lou.mN)
summary(tw.pc) 
plot(tw.pc$x[,1:2])
# We note that the first to PCs account for only .024 and .019 of the variance.
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

# a. Find most representative member (closest to center) as examplar
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

# b. Function to obtain most highly weighted words in each cluster
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
# now you can add these to the data frame (here's a differnet way)
u.lou[, "hc.cluster"] <- as.factor(hc.cluster)
# and start examining attributes of the clusters (here's a different way: subsetting)

# # mpc added: I wouldn't run this -- it takes too long with 1500 rows
# # Mimize within-cluster dissimilarity (akin to "elbow plot" for kmeans)
# # Captures mean documents per cluster and mean distance between points per cluster
# # Adapted from http://rpubs.com/frankdevans/sotu_cluster
# dist_mat <- as.matrix(sou.cd)
# clust_cuts <- data.frame(cut_level=1:10, # mpc changed
#                          avg_size=0, avg_dist=0)
# 
# for (i in 1:(nrow(clust_cuts) - 1)) {
#   clust_cuts[clust_cuts$cut_level==i, 'avg_size'] <- 
#     mean(table(cutree(tree=hc.cd.ward, k=i)))
#   df_dist <- data.frame(doc_name=paste(u.lou$Mnemonic,u.lou$number), # mpc changed
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

# K-means Clustering
# We found the most meaningful exploration of the clusters to be examining the top 
# words in each cluster. 
# 
# Cluster 1: Independent, Thesis, Faculty, Study, Supervision, Research, Member,
# Major, Program, Student 
# I'm actually super impressed at how well this process produced a group of classes
# with just logistical descriptions/issues. In every department there are classes
# like this, where the only description is something like "independent study"
# or "research, faculty supervision"

# Cluster 2: Politics, Culture, Historical, American, Social study,  
# Century, Explore, Examine, Survey
# # This grouping is of classes about politics and culture and history,
# with an american focus. Again, what I find super interesting is that
# I would naturally think of classes from a wide range of departments as fitting 
# this description. In choosing classes for myself, this is a category I would
# naturally see as cohesive/coherent. 

# Cluster 3: Art, Instructor, Permission, Prerequisites, Spanish, Placement, 
# Department, Equivalent, Drama, Credit
# # In the art and drama department, as well as the Spanish department, every single
# class lists the extensive pre-reqs. I think this process picked up on that. 
# 
# 
# Cluster 4: Include, Will, Student, System, Design, Basic, Course, 
# Prerequisite,  Develop, Continue
# We see that this set of top words from cluster 4 contains a lot of words 
# normally seen in class descriptions of middle or even upper level classes.
# 
# Cluster 5: New, Subject, Offer, Opportunity, Course,  Provide, 
# Perform, Music, May, Topic
# 
# Cluster 6: Website, Subject, Offer, opportunity, Course,  Provide , perform, 
# Music, May, Topic, Httpwwwvirginiaeduphilosophi, Year
# 
# Clusters 5 and 6 have almost the same words, with minor differences. We see that all
# philosophy classes seem to have been put in cluster 6. The music classes seem to
# be split into clusters 5 and 6. This indicates to me that possibly the music classes
# have very divergent course descriptions, or that music fits into both of the
# more fundamental natures of clusters 5 and 6, and the process just distributed 
# the music classes into each. 
# 
# 
# 
# 
# # 
# # Hierarchical Clustering
# # For our hierarchical clustering visualization, we have a red line drawn where 
# # there are five clusters and a blue line drawn where there are eight. There appear
# # to be seven distinct clusters in the groupings. We hypothesize these are due 
# # to the seven distinct undergraduate schools at the University (College,
# # Engineering, Commerce, Nursing, Architecture, Batten, and Curry). 
# # That there are seven clusters through the 
# # hierarchical clustering is interesting because these seven clusters were 
# # organically shaped, while in the k clustering we imposed precisely six clusters.
# # Thus the hierarchical clustering graph indicates that it might be that the most 
# # accurate number of clusters for this data is seven.
# # 
# # Further exploration:
# #   
# # A couple things. First, I would go back and redo the k clustering with 7 groups and see
# # what comes of that. From the hierarchical clustering we saw 7 groups naturally form,
# # and I want to see if bringing that to the k clustering changes the nature of 
# # those groupings significantly.
# # Second, I would really like to look at the words beyond just the top words for 
# each group. I hypothesize that looking at the next ten characteristic words
# will show even more nuanced differences between these groupings. 
# 
# I would be very interested in seeing to what degree major requirements
# for all the different majors make people take classes all within the same group/
# have people take classes in different groupings.
# 
# I would also be very interested in seeing what would happen if we took our all
# of the logistical/administrative words. Maybe a more interesting/content oriented
# set of groupings would emerge. Or maybe this shows us the importance of the way 
# classes are structured in how people actually experience/think about classes. 
# 
# A hypothesis: perhaps the ways we have separated departments is arbitrary or wrong.
# Perhaps these need to be reorganized, or thought of differently. 
# Particularly at a university as large as UVA, where the departments are physically
# housed in separate places. 
# Are there professors who actually study really related ideas who don't interact
# with each other because they are arbitrarily in such different locations?
# These would be super fascinating things to further research!

# # One more thing: it would be cool to feed the course descriptions from each group
# into a babble function like we did on the first problemset, and see what kinds of 
# class descriptions it would generate.