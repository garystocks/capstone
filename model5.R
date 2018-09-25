# Model Build 5
# The strategy is to load each source (twitter, news, blogs) separately.
# This script loads and transforms NEWS data

library(dplyr)
library(tidytext)
library(quanteda)
library(data.table)
library(doParallel)

# Load NEWS data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
n <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(n, encoding='UTF-8', skipNul = TRUE)
close(n)

# Clean NEWS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
news <- gsub("\\.+", " EOS ", news) # Replace fullstops with end-of-sentence marker
news <- gsub("\\!+", " EOS ", news) # Replace exclamation marks with end-of-sentence marker
news <- gsub("\\?+", " EOS ", news) # Replace question marks with end-of-sentence marker

news <- gsub("[0-9]+", "", news) # numbers
news <- gsub("\\'", "9", news) # make apostrophes "9"
news <- gsub("[[:punct:]]"," ", news) # all punctuation
news <- gsub("[^a-zA-Z0-9]", " ", news) # replace anything not a letter or number
news <- gsub("9", "\\'", news) # Replace "9" with apostrophe
news <- gsub("&amp;|&lt;|&gt;", "", news, ignore.case = TRUE) # special characters
news <- gsub("ðÿ", "", news, fixed = TRUE) # special characters
news <- gsub("ð", "", news, fixed = TRUE) # special characters
news <- gsub("Ÿ", "", news, fixed = TRUE) # special characters
news <- gsub("˜", "", news, fixed = TRUE) # special characters
news <- gsub("¡", "", news, fixed = TRUE) # special characters
news <- gsub("â", "", news, fixed = TRUE) # special characters

# Store NEWS data ---------------------------------------------------------------------------

# Store in a data table
newsTable <- data.table(doc = rep("News", length(news)), text = news)

# Select a random sample of 80% for training and the remaining for testing
sampleSize <- floor(.8 * nrow(newsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(newsTable)), size = sampleSize) 

newsTraining <- newsTable[trainIndex, ]
newsTesting <- newsTable[-trainIndex, ]

remove(n)
remove(news)
remove(newsTable)
remove(trainIndex)

# Get a list of profanities -------------------------------------------------------------

# Download a list of profanities to remove
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

# Extract ngrams ---------------------------------------------------------------------------

# Extract unigrams
unigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1) %>%
        count(word, sort = TRUE)

# Delete all rows which contain "eos" as a word or in an ngram        
unigrams <- unigrams[-(unigrams$word == "eos"), ]

# Delete singletons
unigrams <- unigrams[!(unigrams$n == 1), ]

gc()

# Extract bigrams
bigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, to_lower = TRUE) %>%
        count(ngram, sort = TRUE) 

# Delete singletons
bigrams <- bigrams[!(bigrams$n == 1), ]

# Separate words
bigrams <- mutate(bigrams,
                  word1 = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 1),
                  word2 = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 2))

# Remove profanities
bigrams <- bigrams[!(bigrams$word1 %in% profanity$V1 | bigrams$word2 %in% profanity$V1), ]

# Remove ngrams with "eos"
bigrams <- bigrams[!(bigrams$word1 == "eos" | bigrams$word2 == "eos"), ]

gc()

# Extract trigrams
trigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 3, to_lower = TRUE) %>%
        count(ngram, sort = TRUE)

# Delete singletons
trigrams <- trigrams[!(trigrams$n == 1), ]

# Separate words
trigrams <- mutate(trigrams,
                   word1 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 1),
                   word2 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 2),
                   word3 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 3))

# Remove profanity
trigrams <- trigrams[!(trigrams$word1 %in% profanity$V1 | trigrams$word2 %in% profanity$V1 | trigrams$word3 %in% profanity$V1), ]

# Remove ngrams with "eos"
trigrams <- trigrams[!(trigrams$word1 == "eos" | trigrams$word2 == "eos" | trigrams$word3 == "eos"), ]

gc()

# Extract quadgrams
quadgrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 4, to_lower = TRUE) %>%
        count(ngram, sort = TRUE)

# Delete singletons
quadgrams <- quadgrams[!(quadgrams$n == 1), ]

# Separate words
quadgrams <- mutate(quadgrams,
                    word1 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 1),
                    word2 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 2),
                    word3 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 3),
                    word4 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 4))

# Remove profanity
quadgrams <- quadgrams[!(quadgrams$word1 %in% profanity$V1 | quadgrams$word2 %in% profanity$V1 | quadgrams$word3 %in% profanity$V1 | quadgrams$word4 %in% profanity$V1), ]

# Remove ngrams with "eos"
quadgrams <- quadgrams[!(quadgrams$word1 == "eos" | quadgrams$word2 == "eos" | quadgrams$word3 == "eos" | quadgrams$word4 == "eos"), ]

# Convert to data tables
unigrams <- data.table(word1 = unigrams$word, count = unigrams$n)
bigrams <- data.table(word1 = bigrams$word1, word2 = bigrams$word2, count = bigrams$n)
trigrams <- data.table(word1 = trigrams$word1, word2 = trigrams$word2, word3 = trigrams$word3, count = trigrams$n)
quadgrams <- data.table(word1 = quadgrams$word1, word2 = quadgrams$word2, word3 = quadgrams$word3, word4 = quadgrams$word4, count = quadgrams$n)

# Index the ngrams to improve performance
setkey(unigrams, word1)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3, word4)


# Define a function to create tokens -------------------------------------------------
makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE,
               remove_url = TRUE,
               ngrams = n)
}

# BACK OFF MODEL --------------------------------------------------------------------
