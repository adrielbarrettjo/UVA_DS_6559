
# import all the things
library(rvest)
library(magrittr)
library(stringr)
library(tm)
library(ngram)

# Load the source page
source.page <- read_html("http://www.presidency.ucsb.edu/inaugurals.php")

# need to work out exactly which URLs on the page is the text we want to pull
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
# subset the datasheet into two sheets for early and late
inaugural.m <- subset(x=inaugural, link %in% 1789:1929) # 
inaugural.s <- subset(x=inaugural, link %in% 1933:2013)
# Append appropriate time label to downloaded file
# Create vector for early period
early <- c(1789:1924)
###### get text for early
for(i in seq(nrow(inaugural.m))) {
  text.m <- read_html(inaugural.m$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
# Find the time period of this link
  party <- ifelse(test = inaugural.m$link[i] %in% early,
                  yes = "early", no = "late")
  
  # Create the file name
  filename <- paste0(party, "-", inaugural.m$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.m)  # write the file
  sink() # close the file
}

late <- c(1933:2013)
###### get text for late
for(i in seq(nrow(inaugural.s))) {
  text.s <- read_html(inaugural.s$url[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
# Find the time period
  party.s <- ifelse(test = inaugural.s$link[i] %in% late,
                  yes = "late", no = "early")
  
# Create the file name
  filename <- paste0(party.s, "-", inaugural.s$links[i], ".txt")
# and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text.s)  # write the file
  sink() # close the file
}
 
# want to look at early versus late babble chunks
# babble for early
earlytext <- paste(text.m)
ngr <- ngram(earlytext, n = 3)
babble(ngr, 200, seed=012171)

#babble for late text
latetext <- paste(text.s)
ngr <- ngram(latetext, n = 3)
babble(ngr, 200, seed=012171)



