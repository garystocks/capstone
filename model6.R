# Model Build 6
# The strategy is to load each source (twitter, news, blogs) separately.
# This script loads and transforms BLOGS data

library(dplyr)
library(tidytext)
library(quanteda)
library(data.table)
library(doParallel)

# Load BLOGS data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
b <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(b, encoding='UTF-8', skipNul = TRUE)
close(b)

# Clean BLOGS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
blogs <- gsub("\\.+", " EOS ", blogs) # Replace fullstops with end-of-sentence marker
blogs <- gsub("\\!+", " EOS ", blogs) # Replace exclamation marks with end-of-sentence marker
blogs <- gsub("\\?+", " EOS ", blogs) # Replace question marks with end-of-sentence marker

blogs <- gsub("[0-9]+", "", blogs) # numbers
blogs <- gsub("\\'", "9", blogs) # make apostrophes "9"
blogs <- gsub("[[:punct:]]"," ", blogs) # all punctuation
blogs <- gsub("[^a-zA-Z0-9]", " ", blogs) # replace anything not a letter or number
blogs <- gsub("9", "\\'", blogs) # Replace "9" with apostrophe
blogs <- gsub("&amp;|&lt;|&gt;", "", blogs, ignore.case = TRUE) # special characters
blogs <- gsub("ðÿ", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("ð", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("Ÿ", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("˜", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("¡", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("â", "", blogs, fixed = TRUE) # special characters

# Store BLOGS data ---------------------------------------------------------------------------

# Store in a data table
blogsTable <- data.table(doc = rep("Blogs", length(blogs)), text = blogs)

# Select a random sample of 80% for training and the remaining for testing
sampleSize <- floor(.8 * nrow(blogsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(blogsTable)), size = sampleSize) 

blogsTraining <- blogsTable[trainIndex, ]
blogsTesting <- blogsTable[-trainIndex, ]

remove(b)
remove(blogs)
remove(blogsTable)
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
unigrams <- tibble(text = blogsTraining$text) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1) %>%
        count(word, sort = TRUE)

# Delete all rows which contain "eos" as a word or in an ngram        
unigrams <- unigrams[-(unigrams$word == "eos"), ]

# Delete singletons
unigrams <- unigrams[!(unigrams$n == 1), ]

gc()

# Extract bigrams
bigrams <- tibble(text = blogsTraining$text) %>%
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
trigrams <- tibble(text = blogsTraining$text) %>%
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
quadgrams <- tibble(text = blogsTraining$text) %>%
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
