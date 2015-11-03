##### Text as Data Problem Set 4
##### November 2, 2015

####Process the Text: Inaugural Addresses

# import all the things
library(rvest)
library(magrittr)
library(stringr)
library(tm)
library(ngram)
library(topicmodels)
library(SnowballC)

#setwd("Box Sync/mpc/DataServices/DS6559/problemsets/")
#dir.create("inaugTMtest")
#setwd("inaugTMtest") # mpc added

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
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  party <- ifelse(test = inaugural.all$link[i] %in% early,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.all$link[i], ".txt")
  
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.all)  # write the file
  sink() # close the file
}

# Corpus
cname<- file.path("C:/Users/Will Schwieder/Desktop/DS TextasData/ALL")
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
allcorcommon <- removeSparseTerms(allDTM, 0.97) # This makes a matrix that is only 15% empty space, maximum.
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
# I changed this to rows instead of columns (Adriel)
train_index <- sample(seq_len(ncol(allcorcommon)), size=train_size)

#(adriel: and here as well, changed it to rows)
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

# M1: Run the k=30 model on the sample data
seed1=823
inaugural30 <- LDA(allcorcommon, k=30, control=list(seed=seed1))

# M2: Run the k=30 model on the sample data
seed2=500
inaugural50 <- LDA(allcorcommon, k=50, control=list(seed=seed2))


# Evaluate/Interpret
# Terms
terms(inaugural30, 3) # top 3
terms(inaugural30, threshold=.005) # surpassing a threshold probability
probterms <- as.data.frame(posterior(inaugural30)$terms) # all the topic-term probabilities

terms(inaugural50, 3) # top 3
terms(inaugural50, threshold=.005)
probterms2 <- as.data.frame(posterior(inaugural50)$terms) 

################# Commentary on Terms of M1 and M2
>>>Will, something to check: which lines printed the top term assignments for the
topics? // Do we have lines that do that?

Q: Do these seem like probably themes?
Q: Does M1 or M2 strick you as producing more meaningful topics? Why?
>>> I think M2, with only 30 topics.

#################


# Topics
topics(inaugural30, threshold=.03) 
probtopic <- as.data.frame(posterior(inaugural30)$topics)

topics(inaugural50, threshold=.02) 
probtopic2 <- as.data.frame(posterior(inaugural50)$topics)

#
t3 <- subset(probtopic, probtopic[,3]>.75)
t3$id <- row.names(t3)

######################Commentary on top topic assignments for each document
>>> to check: DO WE HAVE CODE THAT PRINTS THEM TO THE CONSOLE?
Q: Graph the topic distribution for one document as an example
(I DONT THINK WE HAVE CODE THAT DOES THIS)
Q: Graph the same document for M1 and M2 - do the models with varying k assign
the document to a seemingly similar topic??
######################

# Cohesiveness 
# a. A function to calculate cohesiveness of topics based on the
# occurrenc/co-occurrence of top 10 word assignments across documents
cohesiveness<- function(mod.out, allDTM, k){
  topWords <- terms(mod.out, 10)[,k] 
  data <- as.matrix(allDTM)
  cohvec <- c()
  for (l in 1:10){
    word1 <- topWords[l]
    m <- 1
    while (m < l){
      word2 <- topWords[m] 
      D_m <- length(which(data[,word2] > 0)) # occurrence of word m
      D_l_m <- length(which(data[,word1] > 0 & data[,word2] > 0)) # co-occurrence of word l and m
      cohvec<- append(cohvec, log( (D_l_m + 1)/ D_m) ) # plug into formula (log of pair-wise ratios)
      m <- m + 1
    }
  } 
  return (sum(cohvec)) # sum pair-wise ratios across topic
}

# A second function that calls the cohesiveness function over values of K
cohesive <- function(mod.out, allDTM{
  k <- mod.out@k
  coh <- numeric(k)
  for (i in 1:k) {
    coh[i] <- cohesiveness(mod.out, allDTM, i)
  }
  names(coh) <- seq(1,k,1)
  coh
}
# Call cohesive function: cohesive(model, dtm)
cohesive(inaugural30, allcorcommon)
# Bigger numbers (less negative numbers) are more cohesive

# Exclusivity
# A function to calculate exclusivity of topics based on the probability
# of the top 10 words probability assignment to multiple topics
exclusivity <- function(mod.out, M=10){
  tbeta <- t(exp(mod.out@beta))
  s <- rowSums(tbeta)
  mat <- tbeta/s #normed by columns of beta now.
  ex <- apply(mat,2,rank)/nrow(mat)
  index <- apply(tbeta, 2, order, decreasing=TRUE)[1:M,]
  exc <- vector(length=ncol(tbeta))
  for(i in 1:length(exc)) {
    exc[i] <- sum(ex[index[,i],i])
  }
  names(exc) <- seq(1,length(exc),1)
  exc
}

# exclusivity(model)
exclusivity(inaugural30)