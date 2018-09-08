# Model build

# Libraries
library(quanteda)
library(data.table)
library(sqldf)


# LOAD DATA ----------------------------------------------------------------------------

# Load 100% of files
t <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
twitter <- readLines(t)
close(t)

n <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(n)
close(n)

b <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(b)
close(b)

# Store in a data table
twitterTable <- data.table(doc = rep("Twitter", length(twitter)), text = twitter)
newsTable <- data.table(doc = rep("News", length(news)), text = news)
blogsTable <- data.table(doc = rep("Blogs", length(blogs)), text = blogs)

l <- list(twitterTable, newsTable, blogsTable)
textTable <- rbindlist(l)

# Select a random sample of 60% for training and the remaining 40% for testing
sampleSize <- floor(.6 * nrow(textTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(textTable)), size = sampleSize) 

textTraining <- textTable[trainIndex, ]
textTesting <- textTable[-trainIndex, ]


# CREATE CORPUS ------------------------------------------------------------------------

# Create quanteda corpus with training data
qCorpus <- quanteda::corpus(textTraining)

# Replace fullstops with EOS marker?

# Remove profanities?

# Clean text - remove special characters, punctuation
qTokens <- tokens(qCorpus, remove_numbers = TRUE, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE,
                    remove_hyphens = TRUE, remove_url = TRUE)

# Download a list of profanities
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

qTokens <- tokens_remove(qTokens, profanity$V1)

# Create n-grams
nGrams <- tokens_ngrams(qTokens, n = 1:3)


# Save to Data Table and / or SQL file?


# WORD RELATIONSHIPS -------------------------------------------------------------------

# Extract n-grams

# Remove singletons for performance?

# Create n-gram tables with 3 columns (n-gram for n-1 words, word n and count


# PREDICTION FUNCTION ------------------------------------------------------------------

# Accept a string as input

# Lookup string in the sqldf

# Use a back off model to determine the probability of unobserved n-grams

# Return the words with highest probabilities?

# Save in data.table?
