# Exploratory text analysis

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(tm)
library(quanteda)

# ==================================================================================

# TIDY TEXT

# Extract a random sample of 30% of each file and store in a file
sampleFile <- function(infile, outfile, header = TRUE) {
        set.seed(12345)
        ci <- file(infile, "r")
        co <- file(outfile, "w")
        if (header) {
                hdr <- readLines(ci, n = 1)
                writeLines(hdr, co)
        }
        recnum = 0
        numout = 0
        while (TRUE) {
                inrec <- readLines(ci, n = 1)
                if (length(inrec) == 0) { # end of file?
                        close(co)
                        close(ci)
                        return(numout)
                }
                recnum <- recnum + 1
                if (rbinom(1, 1, prob = .3) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
sampleFile(t, "twitter.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt", "r")
twitter <- readLines(con) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
sampleFile(n, "news.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
news <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
blogs <- readLines(con) 
close(con) 

# Clean twitter text
tweets.text <- twitter
tweets.text.cleaned <- gsub("@\\w+ *#", "", tweets.text)
tweets.text.cleaned <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweets.text.cleaned)
tweets.text.cleaned <- gsub("[^0-9A-Za-z///' ]", "", tweets.text.cleaned)

tweets.text.corpus <- Corpus(VectorSource(tweets.text.cleaned))

tweets.text.final <- tm_map (tweets.text.corpus, removePunctuation)
tweets.text.final2 <- tm_map (tweets.text.final, content_transformer(tolower))
tweets.text.final2 <- tm_map(tweets.text.final2, removeNumbers)
tweets.text.final2 <- tm_map(tweets.text.final2, removePunctuation)
tweets.text.final2 <- tm_map(tweets.text.final2,removeWords, stopwords("English"))
tweets.text.final2 = tm_map(tweets.text.final2, removeWords, c("amp", "&"))

#create corpus
housing.tweets.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words
housing.tweets.corpus <- tm_map(housing.tweets.corpus, function(x)removeWords(x,stopwords()))

#install wordcloud if not already installed
install.packages("wordcloud")
library("wordcloud")

#generate wordcloud
wordcloud(housing.tweets.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 500)
