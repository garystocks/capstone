# Model build

# Libraries
library(ngram)
library(R.utils)
library(quanteda)
library(data.table)
library(sqldf)
library(doParallel)
require(dplyr)

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

# CLEAN TEXT ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
twitter <- gsub("&amp;|&lt;|&gt;", "", twitter, ignore.case = TRUE) # special characters
twitter <- gsub("[0-9]+", "", twitter) # numbers
twitter <- gsub("ðÿ", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("â", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("[^a-zA-Z]", " ", twitter)

news <- gsub("&amp;|&lt;|&gt;", "", news, ignore.case = TRUE) # special characters
news <- gsub("[0-9]+", "", news) # numbers
news <- gsub("ðÿ", "", news, fixed = TRUE) # special characters
news <- gsub("â", "", news, fixed = TRUE) # special characters
news <- gsub("[^a-zA-Z]", " ", news)

blogs <- gsub("&amp;|&lt;|&gt;", "", blogs, ignore.case = TRUE) # special characters
blogs <- gsub("[0-9]+", "", blogs) # numbers
blogs <- gsub("ðÿ", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("â", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("[^a-zA-Z]", " ", blogs)

# STORE DATA ---------------------------------------------------------------------------

# Store in a data table
twitterTable <- data.table(doc = rep("Twitter", length(twitter)), text = twitter)
newsTable <- data.table(doc = rep("News", length(news)), text = news)
blogsTable <- data.table(doc = rep("Blogs", length(blogs)), text = blogs)

l <- list(twitterTable, newsTable, blogsTable)
textTable <- rbindlist(l)

# Select a random sample of 10% for training and the remaining for testing
sampleSize <- floor(.1 * nrow(textTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(textTable)), size = sampleSize) 

textTraining <- textTable[trainIndex, ]
textTesting <- textTable[-trainIndex, ]

# CREATE CORPUS ------------------------------------------------------------------------

# Generic function for parallelizing any task (when possible)
parallelizeTask <- function(task, ...) {
        # Calculate the number of cores
        ncores <- detectCores() - 1
        # Initiate cluster
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        #print("Starting task")
        r <- task(...)
        #print("Task done")
        stopCluster(cl)
        r
}

# Create quanteda corpus with training data
qCorpus <- quanteda::corpus(textTraining)

# Download a list of profanities to remove
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

# Define a function to divide the text into sentences and create start-of-sentence and end-of-sentence markers
makeSentences <- function(input) {
        output <- tokens(input, what = "sentence", remove_numbers = TRUE,
                           remove_punct = TRUE, remove_separators = TRUE,
                           remove_twitter = TRUE, remove_hyphens = TRUE, 
                           remove_url = TRUE)
        output <- tokens_remove(output, profanity)
        unlist(lapply(output, function(a) paste('#s#', tolower(a), '#e#')))
}

# Define a function to create n-grams
makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", remove_numbers = TRUE,
                 remove_punct = TRUE, remove_separators = TRUE,
                 remove_twitter = TRUE, remove_hyphens = TRUE,
                 remove_url = TRUE,
                 ngrams = n)
}

# Create the document feature matrix
sentences <- parallelizeTask(makeSentences, qCorpus)
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)

dfm1 <- parallelizeTask(dfm, ngram1, ignoredFeatures=profanity)
dfm2 <- parallelizeTask(dfm, ngram2, ignoredFeatures=profanity)
dfm3 <- parallelizeTask(dfm, ngram3, ignoredFeatures=profanity)
dfm4 <- parallelizeTask(dfm, ngram4, ignoredFeatures=profanity)

# Delete singletons
dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")
dfm4 <- dfm_trim(dfm4, min_termfreq = 2, termfreq_type = "count")

# Create vectors with sum of frequencies for each n-gram
ngram1Sums <- colSums(dfm1)
ngram2Sums <- colSums(dfm2)
ngram3Sums <- colSums(dfm3)
ngram4Sums <- colSums(dfm4)

# Create data tables with individual words and n-gram count as the variables
ngram1Table <- data.table(word1 = names(ngram1Sums), count = ngram1Sums)

ngram2Table <- data.table(word1 = sapply(strsplit(names(ngram2Sums), "_", fixed = TRUE), '[[', 1),
                          word2 = sapply(strsplit(names(ngram2Sums), "_", fixed = TRUE), '[[', 2),
                          count = ngram2Sums)

ngram3Table <- data.table(word1 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 1),
                          word2 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 2),
                          word3 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 3),
                          count = ngram3Sums)

ngram4Table <- data.table(word1 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 1),
                          word2 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 2),
                          word3 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 3),
                          word4 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 4),
                          count = ngram4Sums)




# Clean text - remove special characters, punctuation
qTokens <- tokens(qCorpus, remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE,
                  remove_hyphens = TRUE, remove_url = TRUE)






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
