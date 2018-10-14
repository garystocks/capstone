# Model Build 8
# The strategy is to load each source (twitter, news, blogs) separately and then combine.

library(dplyr)
library(tidytext)
library(data.table)
library(quanteda)
library(sqldf)
library(hunspell)
library(tm)

# Get a list of profanities to remove --------------------------------------------------------

# Download a list of profanities to remove
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

# Save to file
saveRDS(profanity, file = "profanity.rds")


# Extract samples -----------------------------------------------------------------------

# Define a function to extract a random sample of 10% of each file and store in a file
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
                if (rbinom(1, 1, prob = .1) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
sampleFile(t, "twitter.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt", "r")
twitter <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
sampleFile(n, "news.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
news <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
close(con)

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogs.txt", "r")
blogs <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
close(con)


# Clean text ---------------------------------------------------------------------------

# Function to clean text
cleanText <- function(txt) {
        
        # Remove non-ASCII characters
        txt <- iconv(txt, "latin1", "ASCII", sub = "")
        
        # Remove smileys
        txt <- gsub("<3|</3|\\bxd\\b|\\bx-d\\b|:&|:-&|:p\\b|:-p\\b|
              \\b=p\\b|\\b:d\\b|;d\\b|\\b:o\\)\\b|\\b8\\)|\\b8d
                     \\b|\\b8-d\\b|:3\\b|:-x\\b|:x\\b|:o\\)|:-d\\b|:-o
                     \\b|:o\\b|o_o\\b|o-o\\b|=p\\b|:s\\b|\\bd:", " ", txt)
        
        # Remove RTs
        txt <- gsub("\\brt\\b", " ", txt)
        txt <- gsub("rt2win", " ", txt)
        txt <- gsub("<3RT", " ", txt)
        
        # Change symbols & / to words
        txt<- gsub("\\&", " and ", txt)
        txt <-gsub("\\/", " or ", txt)
        
        # Remove full stops in abbreviations
        txt <- gsub("\\s([A-Z])\\.\\s", " \\1", txt)
        txt <- gsub("\\s([A-Z][a-z]{1,3})\\.\\s", " \\1", txt)
        txt <- gsub("^([A-Z])\\.\\s", " \\1", txt)
        txt <- gsub("^([A-Z][a-z]{1,3})\\.\\s", " \\1", txt)
        
        # Convert to lower case
        txt<- tolower(txt)
        
        # Replace :.?! with end of sentence tags <eos>
        # and eliminate other punctuation except apostrophes
        txt<- gsub("[:.?!]+", " EOS ", gsub("(?![:.?!'])[[:punct:]]", " ", txt, perl=T))
        
        # Remove errant apostrohes
        txt<-gsub(" ' "," ", txt)        
        txt<-gsub("\\' ", " ", txt)
        txt<-gsub("^'", "", txt)
        
        # Replaces number with number tag <num>
        txt<- gsub("[0-9]+"," NUM ", txt)
        
        # Removes website Url
        txt <-gsub(" www(.+) ", " ", txt)
        
        # Remove extra spaces
        txt<- gsub("^[ ]","",txt)
        txt<- gsub("[ ]$", "", txt)
        txt<- stripWhitespace(txt)
        
        return(txt)
}

# Clean samples
twitterClean <- cleanText(twitter)
newsClean <- cleanText(news)
blogsClean <- cleanText(blogs)

# Save files
saveRDS(twitterClean, file = "./data/twitter.txt")
saveRDS(newsClean, file = "./data/news.txt")
saveRDS(blogsClean, file = "./data/blogs.txt")

# Create corpus --------------------------------------------------------------------------

# Combine text vectors
textClean <- c(twitterClean, newsClean, blogsClean)

# Create corpus
myCorpus <- quanteda::corpus(textClean)

# Create n-grams -------------------------------------------------------------------------

# Tokenise the corpus
myTokens <- tokens(myCorpus, what = "word")

# Extract unigrams, create a document feature matrix, save in a data table
unigrams <- tokens_ngrams(myTokens, n = 1L)
dfmUnigrams <- dfm(unigrams)
unigramsDT <- data.table(ngram = featnames(dfmUnigrams), count = colSums(dfmUnigrams), 
                         stringsAsFactors = FALSE)

# Extract bigrams, create a document feature matrix, save in a data table and extract tail word
bigrams <- tokens_ngrams(myTokens, n = 2L, concatenator = "_")
dfmBigrams <- dfm(bigrams)
bigramsDT <- data.table(ngram = featnames(dfmBigrams), count = colSums(dfmBigrams),
                        stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(bigramsDT$ngram, "_", fixed = TRUE), '[[', 2)
bigramsDT <- bigramsDT[, tail := tailWords]

# Extract trigrams, create a document feature matrix, save in a data table and extract tail word
trigrams <- tokens_ngrams(myTokens, n = 3L, concatenator = "_")
dfmTrigrams <- dfm(trigrams)
trigramsDT <- data.table(ngram = featnames(dfmTrigrams), count = colSums(dfmTrigrams),
                        stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 3)
trigramsDT <- trigramsDT[, tail := tailWords]

# Extract quadgrams, create a document feature matrix, save in a data table and extract tail word
quadgrams <- tokens_ngrams(myTokens, n = 4L, concatenator = "_")
dfmQuadgrams <- dfm(quadgrams)
quadgramsDT <- data.table(ngram = featnames(dfmQuadgrams), count = colSums(dfmQuadgrams),
                         stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 4)
quadgramsDT <- quadgramsDT[, tail := tailWords]

# Remove low frequency ngrams -----------------------------------------------------------


