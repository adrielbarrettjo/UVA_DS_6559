####Process the Text: Inaugural Addresses
setwd("DropBox")
dir.create("inaugTMtest")
setwd("inaugTMtest")

# import all the things
library(rvest)
library(magrittr)
library(stringr)
library(tm)
library(ngram)
library(topicmodels)
library(SnowballC)

# Load the source page
source.page <- read_html("http://www.presidency.ucsb.edu/inaugurals.php")

# Get the urls
url <- source.page %>%
  html_nodes(".ver12 a") %>%  # get the CSS nodes
  html_attr("href") # extract the URLs

link <- source.page %>% # feed `source.page` to the next step
  html_nodes(".ver12 a") %>%  # get the CSS nodes
  html_text()

inaugural<- data.frame(link=link, url=url, stringsAsFactors=FALSE)
n<-dim(inaugural)[1]
inaugural<-inaugural[1:(n-1),]

# Change the link to include numeric values, keep only the last 4 digits in the link
inaugural$link <- str_sub(inaugural$link,-4,-1)

# Create vector for all and early
all<- c(1789, 1793, 1797, 1801, 1805, 1809, 1813, 1817, 1821, 1825, 1829, 1833, 1837, 1841, 1845, 1849, 1853, 1857, 1861, 1865, 1869, 1873, 1877, 1881, 1885, 1889, 1893, 1897, 1901, 1905, 1909, 1913, 1917, 1921, 1925,1929, 1933, 1937, 1941, 1945, 1949, 1953, 1957, 1961, 1965, 1969, 1973, 1977, 1981, 1985, 1989, 1993, 1997, 2001, 2005, 2009, 2013)
early<- c(1789, 1793, 1797, 1801, 1805, 1809, 1813, 1817, 1821, 1825, 1829, 1833, 1837, 1841, 1845, 1849, 1853, 1857, 1861, 1865, 1869, 1873, 1877, 1881, 1885, 1889, 1893)
late<- c(1897, 1901, 1905, 1909, 1913, 1917, 1921, 1925,1929, 1933, 1937, 1941, 1945, 1949, 1953, 1957, 1961, 1965, 1969, 1973, 1977, 1981, 1985, 1989, 1993, 1997, 2001, 2005, 2009, 2013)

# subset the datasheet into a sheet 
inaugural.all <- subset(x=inaugural, link %in% all)

# Get text for all so can make corpus
for(i in seq(nrow(inaugural.all))) {
  text.all <- read_html(inaugural.all$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isolate the text
    html_text() # get the text
  
  timeperiod <- ifelse(test = inaugural.all$link[i] %in% early,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(timeperiod, "-", inaugural.all$link[i], ".txt")
  
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.all)  # write the file
  sink() # close the file
}

# Corpus
cname <- file.path("/Users/adrielbarrett-johnson/Dropbox/inaugTMtest")
cname
allcor<-Corpus(DirSource(cname))
summary(allcor)
inspect(allcor)

# Remove capitalization
allcor <- tm_map(allcor,content_transformer(tolower))
inspect(allcor)

# create spaces so can accurately remove all punctuation
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
allcor <- tm_map(allcor, toSpace, "-")

# Remove punctuation
allcor <- tm_map(allcor, removePunctuation)
allcor[1]

# Remove stop words because we don't need them here
allcor <- tm_map(allcor, removeWords, stopwords("english"))

# Strip whitespace
allcor <- tm_map(allcor, stripWhitespace)

allcor[[1]]$content

# Stemming
allStem <- tm_map(allcor, stemDocument) 

###### fixing the wordstems for the TDM
wordStem(c("strong", "stronger"))
wordStem(c("smaller", "small"))
wordStem(c("bigger", "big"))
wordStem(c("children", "child"))
wordStem(c("citizenship", "citizen"))
wordStem(c("worker", "work"))
wordStem(c("woman", "women"))
wordStem(c("willing", "will"))
wordStem(c("weaken", "weak"))
wordStem(c("threaten", "threat"))
wordStem(c("understood", "understand"))
wordStem(c("truth", "true"))
wordStem(c("taken", "take"))
wordStem(c("surest", "sure"))


# Create Document term Matrix
allDTM <- DocumentTermMatrix(allStem)
inspect <- (allDTM)
str (allDTM)

# Remove sparse/uncommon terms
allcorcommon <- removeSparseTerms(allDTM, 0.97) 
dim(allDTM)
dim(allcorcommon)

######## topic modeling ############

# Initial topic model (for testing)
seed1=823
tm1 <- LDA(allcorcommon, k=5, control=list(seed=seed1))

# 1. Evaluating K
# Evaluating k on held-out data, using VEM approximation
# sample held-out data - test, and estimating data - train
set.seed(823) 
train_size <- floor(.75 * nrow(allcorcommon))
train_index <- sample(seq_len(nrow(allcorcommon)), size=train_size)

train <- allcorcommon[train_index,]
test <- allcorcommon[-train_index,]

# Perplexity using a held-out set
train5 <- LDA(train, k=5, control=list(seed=seed1))
per <- perplexity(train5, test)
per

test5 <- LDA(test, model=train5, control=list(estimate.beta=FALSE, seed=seed1))
str(test5)
# calculate "by hand"
exp(-1*(sum(test5@loglikelihood)/test5@n))

# Now do this across a range of K (10,20,30)
seed1 <- 12171
trainK10 <- lapply(seq(10,30,10), function(k){LDA(train, k)})
perplexityK <- lapply(trainK10, function(k){perplexity(k, test)})

perplexityK <- c(per, perplexityK) # add perplexity for k=5 to list


# Across another range of K (40,50,60)
trainK10 <- lapply(seq(40,60,10), function(k){LDA(train, k)})
perplexityK2 <- lapply(trainK10, function(k){perplexity(k, test)})

perplexityK <- c(perplexityK, perplexityK2) # combine the perxplexity lists
px <- unlist(perplexityK)
perplex <- as.data.frame(cbind(px, k=c(5,10,20,30,40,50,60))) # and add k values
plot <- ggplot(perplex, aes(x=k, y=px)) 
plot + geom_line()

# M1: Run the k=5 model on the sample data
seed1=823
inaugural5 <- LDA(allcorcommon, k=5, control=list(seed=seed1))

# M2: Run the k=30 model on the sample data
seed2=500
inaugural20 <- LDA(allcorcommon, k=20, control=list(seed=seed2))

# Evaluate/Interpret
# Terms
terms(inaugural5, 3) # top 3
terms(inaugural5, threshold=.005) # surpassing a threshold probability
probterms <- as.data.frame(posterior(inaugural5)$terms) # all the topic-term probabilities

terms(inaugural20, 3) # top 3
terms(inaugural20, threshold=.005)
probterms2 <- as.data.frame(posterior(inaugural20)$terms) 

################# Commentary on Terms of M1 and M2
# Q: Do these seem like plausible themes?
# Yes they seem like plausible themes because each of these Presidents is addressing 
# similar audience (not same: people die). In an inaugural address, presidents consistently
# are tryna get people amped up about their future work, which is reflected in these 
# groupings.

# Top Three words for the five topics:
# 1: govern, state, power >>> theme = the power of the state
# 2: nation, will, can >>> theme = the capacity for progress
# 3: will, state, govern >>> theme = governing
# 4: will, world, must >>> theme = the imperative of foreign relations
# 5: peopl, govern, will >>> theme = governing

# Note: themes 3 and 5 seem highly similar. 

# Q: Do M1 or M2 strick you as producing more meaningful topics? Why?
# >>> M1 (with 5 clusters) makes the most sense. Looking at M2, the clusters are super
# repetitive. Even with 5 clusters, the groupings are similar and have multiple repeated
# words, especially in the top 3 words for each cluster. Looking at the full listings of each
# topic the five top topic clusters differentiates itself a little bit more. 

#################

# Topics
topics(inaugural30, threshold=.03) 
probtopic <- as.data.frame(posterior(inaugural30)$topics)

topics(inaugural50, threshold=.02) 
probtopic2 <- as.data.frame(posterior(inaugural50)$topics)

#
t3 <- subset(probtopic, probtopic[,3]>.75)
t3$id <- row.names(t3)

#############
# Graph the topic distribution for one document as an example
topiclab <- as.data.frame(t(terms(inaugural5, 5)))
topiclab$lab <- paste0(topiclab$V1, "-", topiclab$V2, "-", topiclab$V3, "-", topiclab$V4, "-", topiclab$V5)

# Most likely topics for each document 
topics(inaugural5, threshold=.10) # those composing 10% or more

# More specifically, probability of topic in document
prob <- as.data.frame(t(posterior(inaugural5)$topics))
prob$topic <- as.factor(rownames(prob))
prob$topiclab <- topiclab$lab

# Graph topic probabilities for (late-1961.txt)
ggplot(prob, aes(x=topiclab, y=prob[,44])) + 
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  labs(y=colnames(prob[44]), title="Topic Assignment")
##It graphed only the topic nation-will-can-world-peopl for late-1961.txt

# And graph the same document for M2 as well - do the models with varying k assign
# the document to a seemingly similar topic??
topic2lab <- as.data.frame(t(terms(inaugural20, 20)))
topic2lab$lab <- paste0(topic2lab$V1, "-", topic2lab$V2, "-", topic2lab$V3, "-", topic2lab$V4, "-", topic2lab$V5)

# Most likely topics for each document 
topics(inaugural20, threshold=.10) # those composing 10% or more

# More specifically, probability of topic in document
prob2 <- as.data.frame(t(posterior(inaugural20)$topics))
prob2$topic <- as.factor(rownames(prob2))
prob2$topic2lab <- topic2lab$lab

# Graph topic probabilities for (late-1961.txt)
ggplot(prob2, aes(x=topic2lab, y=prob2[,44])) + 
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  labs(y=colnames(prob[44]), title="Topic Assignment")

# It graphed the topics nation-world-people-will-free 
# and will-new-world-america-nation for late-1961.txt
# So yes, the models with differing k still assign super similar topics to the
# same document, which is makes sense - and is super cool. 

#For one to two topics that struck you as more valid above; identify a document to 
#which those topics are strongly associated and have a look at these documents. 
#Upon reading them more closely, do they seem to contain the “theme” you believed 
#the terms represented?
# 
# 1949, 1953, and 1957 all are solely identified with topic 20, which is 
# can-countri-faith-free-help-hope-know-live-may-must-nation-peac-peopl-secur-shall-unit-will-world
# Reading those three speeches, they do seem to hold together as a common theme, although
# I think there are also significant enough differences in tone and issues discussed
# that I am surprised that with k=20 the model did not ascribe more topics to 
# these speeches.
# 
# 1949 can be summed up by "The first half of this century has been marked by 
# unprecedented and brutal attacks on the rights of man, and by the two most 
# frightful wars in history. The supreme need of our time is for men to learn to live 
# together in peace and harmony."
# 
# 1953 can be summed up by "How far have we come in man's long pilgrimage from darkness
# toward the light? Are we nearing the light--a day of freedom and of peace for all 
# mankind? Or are the shadows of another night closing in upon us?"
# 
# 1957 can be summed up by "We must use our skills and knowledge and, at times, 
# our substance, to help others rise from misery, however far the scene of suffering 
# may be from our shores. "