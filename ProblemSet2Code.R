##### Text as Data Problem Set 2
##### October 16, 2015

######### FIRST: PreProcess the Text

# import all the things
library(rvest)
library(magrittr)
library(stringr)
library(tm)
library(ngram)

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

# Create vector for each party
republican <- c(1861, 1865, 1869, 1877, 1881, 1889, 1897, 1901, 1909, 1921, 1923, 1929, 1953, 1969, 1974, 1981, 1989, 2001)
democrat <- c(1801, 1809, 1817, 1825, 1829, 1837, 1845, 1853, 1857, 1865, 1885, 1893, 1913, 1933, 1945, 1961, 1963, 1977, 1993, 2009)
whig <- c(1841, 1849, 1850)
federalist <- c(1797)
independent <- c(1789)

# Create vector for all
all <- c(1861, 1865, 1869, 1877, 1881, 1889, 1897, 1901, 1909, 1921, 1923, 1929, 1953, 1969, 1974, 1981, 1989, 2001,1801, 1809, 1817, 1825, 1829, 1837, 1845, 1853, 1857, 1865, 1885, 1893, 1913, 1933, 1945, 1961, 1963, 1977, 1993, 2009, 1841, 1849, 1850, 1797, 1789)

# subset the datasheet into a sheet for each party in history
inaugural.republican <- subset(x=inaugural, link %in% republican)
inaugural.democrat <- subset(x=inaugural, link %in% democrat)
inaugural.whig <- subset(x=inaugural, link %in% whig)
inaugural.federalist <- subset(x=inaugural, link %in% federalist)
inaugural.independent <- subset(x=inaugural, link %in% independent)
inaugural.all <- subset(x=inaugural, link %in% all)

# Get text for all so can make corpus
for(i in seq(nrow(inaugural.all))) {
  text.all <- read_html(inaugural.all$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
party <- ifelse(test = inaugural.all$link[i] %in% all,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.all$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.all)  # write the file
  sink() # close the file
}

###### get text for republican
for(i in seq(nrow(inaugural.republican))) {
  text.republican <- read_html(inaugural.republican$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text

  party <- ifelse(test = inaugural.republican$link[i] %in% republican,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.republican$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.republican)  # write the file
  sink() # close the file
}

###### get text for whig
for(i in seq(nrow(inaugural.whig))) {
  text.whig <- read_html(inaugural.whig$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  party <- ifelse(test = inaugural.whig$link[i] %in% whig,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.whig$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.whig)  # write the file
  sink() # close the file
}

###### get text for independent
for(i in seq(nrow(inaugural.independent))) {
  text.independent <- read_html(inaugural.independent$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  party <- ifelse(test = inaugural.independent$link[i] %in% independent,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.independent$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.independent)  # write the file
  sink() # close the file
}

###### get text for federalist 
for(i in seq(nrow(inaugural.federalist))) {
  text.federalist <- read_html(inaugural.federalist$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  party <- ifelse(test = inaugural.federalist$link[i] %in% federalist,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.federalist$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.federalist)  # write the file
  sink() # close the file
}

###### get text for democrat
for(i in seq(nrow(inaugural.democrat))) {
  text.democrat <- read_html(inaugural.democrat$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  # Find the time period of this link ...this doesn't make sense but it works?
  party <- ifelse(test = inaugural.democrat$link[i] %in% democrat,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.democrat$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.democrat)  # write the file
  sink() # close the file
}

########### clean up the text for all of the inaugural addresses first
allcor <- Corpus(VectorSource(text.all))
########### clean up the text for only the democrats AND republicans ################
# Create a corpus
dem <- Corpus(VectorSource(text.democrat))
rep <- Corpus(VectorSource(text.republican))
# Remove capitalization
allcor <- tm_map(allcor,content_transformer(tolower)) 
dem <- tm_map(dem,content_transformer(tolower)) 
rep <- tm_map(rep,content_transformer(tolower)) 

# create spaces so can accurately remove all punctuation
toSpace <- content_transformer(function(x, pattern) 
{return (gsub(pattern, "  ", x))})
allcor <- tm_map(allcor, toSpace, "-")
dem <- tm_map(dem, toSpace, "-")
rep <- tm_map(rep, toSpace, "-")

# Remove punctuation
allcor <- tm_map(allcor, removePunctuation)
allcor[[1]][1]

dem <- tm_map(dem, removePunctuation) 
dem[[1]][1]

rep <- tm_map(rep, removePunctuation) 
rep[[1]][1]

# Remove stop words because we don't need them here
allcor <- tm_map(allcor, removeWords, stopwords("english"))

dem <- tm_map(dem, removeWords, stopwords("english")) 
dem[[1]][1]

rep <- tm_map(rep, removeWords, stopwords("english")) 
rep[[1]][1]

# Strip whitespace
allcor <- tm_map(allcor, stripWhitespace)

dem<- tm_map(dem, stripWhitespace)
dem[[1]][1]

rep <- tm_map(rep, stripWhitespace)
rep[[1]][1]

library(wordcloud)

# Stemming
install.packages("SnowballC")
library(SnowballC)

allStem <- tm_map(allcor, stemDocument) ######################################

demStem <- tm_map(dem, stemDocument)
demStem[[1]][1]

repStem <- tm_map(rep, stemDocument)
repStem[[1]][1]

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


# Create Term Document Matrix
allTDM <- TermDocumentMatrix(allStem)
inspect <- (allTDM)
str (allTDM)

repTDM <- TermDocumentMatrix(repStem)
inspect(repTDM)
str(repTDM)

demTDM <- TermDocumentMatrix(demStem)
inspect(demTDM)
str(demTDM)

# Frequent terms
demFreq <- findFreqTerms(demTDM, lowfreq=5, highfreq=Inf)
demFreq
## if lowfreq = 2, there are 232 words
## if lowfreq = 3, there are 91 words
## if lowfreq = 10, there 5 words
## if lowfreq = 5, there are 25 words

repFreq <- findFreqTerms(repTDM, lowfreq=5, highfreq=Inf)
repFreq
## lowfreq=5, get 27 words 

## wordcloud for each individually 
wordcloud(allcor, max.words=100, scale=c(3,.25))
wordcloud(dem, max.words=100, scale=c(3,.25))
wordcloud(rep, max.words=100, scale=c(3,.25))

# Remove sparse/uncommon terms
allcorcommon <- removeSparseTerms(allTDM, 0.85) # This makes a matrix that is only 85% empty space, maximum.
dim(allTDM)
dim(allcorcommon)

## vizualization of freq terms after removing sparse terms
wordcloud(allcorcommon, max.words=25, scale=c(3,.25))

# wordcloud to compare dems and reps
allcorC <- as.matrix(allcorcommon)
allcorC[,] 
comparison.cloud(allcorC[,], max.words=100, 
                 scale=c(3,.25), random.order=FALSE, 
                 colors=c("blue3", "red3"), title.size=1)

# initial observations comparing the reps and dems:
# Democrats use a lot of words like can, courage, new, common, seek, will, etc.
# In general, these are future slanted words as well as focused on the individual 
# person.
# Republicans use words like character, defend, responsibility, commitment, etc.
# These are words I would expect from someone trying to appeal to conservatives.
