# Model Build 4
# The strategy is to load each source (twitter, news, blogs) separately.
# This script loads and transforms TWITTER data

library(dplyr)
library(tidytext)
library(quanteda)
library(data.table)
library(doParallel)

# Load TWITTER data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
t <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
twitter <- readLines(t, encoding='UTF-8', skipNul = TRUE)
close(t)

# Clean TWITTER text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
twitter <- gsub("\\.+", " EOS ", twitter) # Replace fullstops with end-of-sentence marker
twitter <- gsub("\\!+", " EOS ", twitter) # Replace exclamation marks with end-of-sentence marker
twitter <- gsub("\\?+", " EOS ", twitter) # Replace question marks with end-of-sentence marker

twitter <- gsub("[0-9]+", "", twitter) # numbers
twitter <- gsub("\\'", "9", twitter) # make apostrophes "9"
twitter <- gsub("[[:punct:]]"," ", twitter) # all punctuation
twitter <- gsub("[^a-zA-Z0-9]", " ", twitter) # replace anything not a letter or number
twitter <- gsub("9", "\\'", twitter) # Replace "9" with apostrophe
twitter <- gsub("&amp;|&lt;|&gt;", "", twitter, ignore.case = TRUE) # special characters
twitter <- gsub("ðÿ", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("ð", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("Ÿ", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("˜", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("¡", "", twitter, fixed = TRUE) # special characters
twitter <- gsub("â", "", twitter, fixed = TRUE) # special characters

#twitter <- gsub("[^a-zA-Z]", " ", twitter)
#twitter <- gsub("[[:punct:,^\\']]"," ", twitter)

# Store TWITTER data ---------------------------------------------------------------------------

# Store in a data table
twitterTable <- data.table(doc = rep("Twitter", length(twitter)), text = twitter)

# Select a random sample of 80% for training and the remaining for testing
sampleSize <- floor(.8 * nrow(twitterTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(twitterTable)), size = sampleSize) 

twitterTraining <- twitterTable[trainIndex, ]
twitterTesting <- twitterTable[-trainIndex, ]

remove(t)
remove(twitter)
remove(twitterTable)
remove(trainIndex)

# Extract ngrams ---------------------------------------------------------------------------

# Extract unigrams
unigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        count(word, sort = TRUE)

# Delete all rows which contain "eos" as a word or in an ngram        
unigrams <- unigrams[-(unigrams$word == "eos"), ]

# Delete singletons
unigrams <- unigrams[!(unigrams$n == 1), ]

gc()

# Extract bigrams
bigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, to_lower = TRUE) %>%
        count(ngram, sort = TRUE) 

# Delete singletons
bigrams <- bigrams[!(bigrams$n == 1), ]

# Separate words
bigrams <- mutate(bigrams,
                  word1 = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 1),
                  word2 = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 2))

# Remove ngrams with "eos"
bigrams <- bigrams[!(bigrams$word1 == "eos" | bigrams$word2 == "eos"), ]

gc()

# Extract trigrams
trigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 3, to_lower = TRUE) %>%
        count(ngram, sort = TRUE)

# Delete singletons
trigrams <- trigrams[!(trigrams$n == 1), ]

# Separate words
trigrams <- mutate(trigrams,
                   word1 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 1),
                   word2 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 2),
                   word3 = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 3))

# Remove ngrams with "eos"
trigrams <- trigrams[!(trigrams$word1 == "eos" | trigrams$word2 == "eos" | trigrams$word3 == "eos"), ]

gc()

# Extract quadgrams
quadgrams <- tibble(text = twitterTraining$text) %>%
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

# Remove profanities ----------------------------------------------------------------

# Download a list of profanities to remove
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

# -----------------------------------------------------------------------------------
# Define a function to create tokens
makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE,
               remove_url = TRUE,
               ngrams = n)
}

# BACK OFF MODEL --------------------------------------------------------------------

# BACK OFF MODEL ----------------------------------------------------------------------

# Quiz 2
"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" # BEER
"You're the reason why I smile everyday. Can you follow me please? It would mean the" # WORLD
"Hey sunshine, can you follow me and make me the" # HAPPIEST
"Very early observations on the Bills game: Offense still struggling but the" 
"Go on a romantic date at the" # BEACH
"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" # WAY
"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" # TIME
"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" # FINGERS
"Be grateful for the good times and keep the faith during the" # WORSE
"If this isn't the cutest thing you've ever seen, then you must be" # INSANE

# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
gamma4 <- .5 # quadgram discount

inputText <- "struggling but the"

# Find OBSERVED quadgrams and the counts
getObservedQuadgrams <- function(inputString, inputQuadgrams) {
        quadgramsFound <- data.table(word1 = vector(mode = "character", length = 0),
                                     word2 = vector(mode = "character", length = 0),
                                     word3 = vector(mode = "character", length = 0),
                                     word4 = vector(mode = "character", length = 0),
                                     count = vector(mode = "integer", length = 0))
        words <- makeTokens(inputString, n = 1L)
        quadgramIndices <- inputQuadgrams[inputQuadgrams$word1 == words$text1[1] & 
                                                  inputQuadgrams$word2 == words$text1[2] & 
                                                  inputQuadgrams$word3 == words$text1[3], ]
        if(length(quadgramIndices) > 0 ) {
                quadgramsFound <- quadgramIndices
        }
        return(quadgramsFound)
}

observedQuadgrams <- getObservedQuadgrams(inputText, quadgrams)

# Calculate the probabilities of observed quadgrams beginning with inputText
getObservedQuadgramProbs <- function(observedQuadgrams, inputTrigrams, inputString, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(NULL)
        obsCount <- filter(inputTrigrams, word1 == makeTokens(inputString)$text1[1] & word2 == makeTokens(inputString)$text1[2] & word3 == makeTokens(inputString)$text1[3])$count[1]
        obsQuadgramProbs <- mutate(observedQuadgrams, count = ((count - quadgramDisc) / obsCount))
        colnames(obsQuadgramProbs) <- c("word1", "word2", "word3", "word4", "prob")
        
        return(obsQuadgramProbs)
}

qObservedQuadgramProbs <- getObservedQuadgramProbs(observedQuadgrams, trigrams, inputText, gamma4)

# Find the tail (last) words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams
getUnobservedQuadgramTails <- function(observedQuadgrams, inputUnigrams) {
        observedQuadgramTails <- observedQuadgrams$word4
        unobservedQuadgramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedQuadgramTails), ]$word1
        return(unobservedQuadgramTails)
}

unobservedQuadgramTails <- getUnobservedQuadgramTails(observedQuadgrams, unigrams)

# Calculate the discount mass probability for trigrams
getAlphaTrigram <- function(bigram, inputTrigrams, trigramDisc = .5) {
        # Get all trigrams that start with the bigram
        trigramsWithBigram <- inputTrigrams[inputTrigrams$word1 == bigram$word1 & 
                                                    inputTrigrams$word2 == bigram$word2, ]
        if(nrow(trigramsWithBigram) < 1) return(0)
        alpha <- 1 - (sum(trigramsWithBigram$count - trigramDisc) / bigram$count)
        
        return(alpha)
}

bigram <- bigrams[bigrams$word1 == makeTokens(inputText, n = 1L)$text1[2] &
                          bigrams$word2 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaTrigram <- getAlphaTrigram(bigram, trigrams, gamma3)

# Get backed off trigrams
getBoTrigrams <- function(inputString, unobservedQuadgramTails) {
        w_i_minus2 <- makeTokens(inputString, n = 1L)$text1[[2]]
        w_i_minus1 <- makeTokens(inputString, n = 1L)$text1[[3]]
        boTrigrams <- data.table(word1 = rep(w_i_minus2, length(unobservedQuadgramTails)), 
                                 word2 = rep(w_i_minus1, length(unobservedQuadgramTails)), 
                                 word3 = unobservedQuadgramTails)
        return(boTrigrams)
}

boTrigrams <- getBoTrigrams(inputText, unobservedQuadgramTails)

# Find OBSERVED trigrams from BO trigrams
getObservedBoTrigrams <- function(inputString, unobservedQuadgramTails, inputTrigrams) {
        boTrigrams <- getBoTrigrams(inputString, unobservedQuadgramTails)
        observedBoTrigrams <- inputTrigrams[inputTrigrams$word1 %in% boTrigrams$word1 & 
                                                    inputTrigrams$word2 %in% boTrigrams$word2 &
                                                    inputTrigrams$word3 %in% boTrigrams$word3]
        return(observedBoTrigrams)
}

observedBoTrigrams <- getObservedBoTrigrams(inputText, unobservedQuadgramTails, trigrams)

# Calculate the probabilities of observed BO trigrams
getObservedBoTrigramProbs <- function(observedBoTrigrams, inputBigrams, inputString, trigramDisc = .5) {
        if(nrow(observedBoTrigrams) < 1) return(NULL)
        obsCount <- filter(inputBigrams, inputBigrams$word1 == makeTokens(inputString)$text1[1] & 
                                   inputBigrams$word2 == makeTokens(inputString)$text1[2])$count[1]
        obsTrigramProbs <- mutate(observedBoTrigrams, count = ((count - trigramDisc) / obsCount))
        colnames(obsTrigramProbs) <- c("word1", "word2", "word3", "prob")
        
        return(obsTrigramProbs)
}

qObservedBoTrigramProbs <- getObservedBoTrigramProbs(observedBoTrigrams, bigrams, inputText, gamma3)

# Find UNOBSERVED trigrams from BO trigrams
getUnobservedBoTrigrams <- function(inputString, unobservedQuadgramTails, observedBoTrigrams) {
        boTrigrams <- getBoTrigrams(inputString, unobservedQuadgramTails)
        unobservedTrigrams <- boTrigrams[!(boTrigrams$word1 %in% observedBoTrigrams$word1 & 
                                                   boTrigrams$word2 %in% observedBoTrigrams$word2 &
                                                   boTrigrams$word3 %in% observedBoTrigrams$word3)]
        return(unobservedTrigrams)
}

# Find the tail words of UNOBSERVED trigrams that start with the first 2 words of observedTrigrams
getUnobservedTrigramTails <- function(observedTrigrams, inputUnigrams) {
        observedTrigramTails <- observedTrigrams$word3
        unobservedTrigramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedTrigramTails), ]$word1
        return(unobservedTrigramTails)
}

unobservedTrigramTails <- getUnobservedTrigramTails(observedBoTrigrams, unigrams)

# Calculate the discount mass probability for bigrams
# alpha for observed bigrams = 1 - sum_of((observed_bigram_count - discount) / unigram_count)
getAlphaBigram <- function(unigram, inputBigrams, bigramDisc = .5) {
        # Get all bigrams that start with the unigram
        bigramsWithUnigram <- inputBigrams[word1 == unigram$word1]
        if(nrow(bigramsWithUnigram) < 1) return(0)
        alpha <- 1 - (sum(bigramsWithUnigram$count - bigramDisc) / unigram$count)
        
        return(alpha)
}

unigram <- unigrams[unigrams$word1 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaBigram <- getAlphaBigram(unigram, bigrams, gamma2)

# Get backed off bigrams
getBoBigrams <- function(inputString, unobservedTrigramTails) {
        w_i_minus1 <- makeTokens(inputString, n = 1L)$text1[[3]]
        boBigrams <- data.table(word1 = rep(w_i_minus1, length(unobservedTrigramTails)), word2 = unobservedTrigramTails)
        return(boBigrams)
}

boBigrams <- getBoBigrams(inputText, unobservedTrigramTails)

# Get OBSERVED bigrams from the set of BO bigrams
getObservedBoBigrams <- function(inputString, unobservedTrigramTails, inputBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        observedBoBigrams <- inputBigrams[inputBigrams$word1 %in% boBigrams$word1 & inputBigrams$word2 %in% boBigrams$word2]
        return(observedBoBigrams)
}

observedBoBigrams <- getObservedBoBigrams(inputText, unobservedTrigramTails, bigrams)

# Get UNOBSERVED bigrams from the set of BO bigrams
getUnobservedBoBigrams <- function(inputString, unobservedTrigramTails, observedBoBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        unobservedBigrams <- boBigrams[!(boBigrams$word1 %in% observedBoBigrams$word1 & boBigrams$word2 %in% observedBoBigrams$word2)]
        return(unobservedBigrams)
}

unobservedBoBigrams <- getUnobservedBoBigrams(inputText, unobservedTrigramTails, observedBoBigrams)

# Calculate probabilities for OBSERVED BO bigrams
# q_bo(w1 | w2)
getObservedBigramProbs <- function(observedBoBigrams, inputUnigrams, bigramDisc = .5) {
        word <- observedBoBigrams$word1[1]
        wordCount <- inputUnigrams[inputUnigrams$word1 == word]
        observedBigramProbs <- (observedBoBigrams$count - bigramDisc) / wordCount$count
        observedBigramProbs <- data.table(word1 = observedBoBigrams$word1, 
                                          word2 = observedBoBigrams$word2, 
                                          prob = observedBigramProbs)
        
        return(observedBigramProbs)
}

qObservedBigramProbs <- getObservedBigramProbs(observedBoBigrams, unigrams, gamma2)

# Calculate probabilities for UNOBSERVED BO bigrams
# q_bo(w1 | w2)
getqBoUnobservedBigrams <- function(unobservedBoBigrams, inputUnigrams, alphaBigram) {
        # get unobserved bigram tails
        qBoUnobservedBigrams <- unobservedBoBigrams$word2
        w_in_Aw_iminus1 <- inputUnigrams[!inputUnigrams$word1 %in% qBoUnobservedBigrams]
        # convert to data table with counts
        qBoUnobservedBigrams <- inputUnigrams[inputUnigrams$word1 %in% qBoUnobservedBigrams]
        denom <- sum(qBoUnobservedBigrams$count)
        # convert counts to probabilities
        qBoUnobservedBigrams <- data.table(word1 = unobservedBoBigrams$word1, 
                                           word2 = unobservedBoBigrams$word2,
                                           prob = (alphaBigram * qBoUnobservedBigrams$count / denom))
        
        return(qBoUnobservedBigrams)
}

qUnobservedBoBigramProbs <- getqBoUnobservedBigrams(unobservedBoBigrams, unigrams, alphaBigram)

# Calculate UNOBSERVED trigram probabilities
getUnobservedTrigramProbs <- function(inputString, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram) {
        qBoBigrams <- rbind(qBoObservedBigrams, qBoUnobservedBigrams)
        qBoBigrams <- qBoBigrams[order(-qBoBigrams$prob), ]
        sumqBoBigrams <- sum(qBoBigrams$prob)
        word <- makeTokens(inputString, n = 1L)$text1[[2]]
        # unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = "_")
        unobservedTrigramProbs <- alphaTrigram * qBoBigrams$prob / sumqBoBigrams
        unobservedTrigramsDT <- data.table(word1 = word, word2 = qBoBigrams$word1, 
                                           word3 = qBoBigrams$word2, prob = unobservedTrigramProbs)
        
        return(unobservedTrigramsDT)
}

unobservedTrigramProbs <- getUnobservedTrigramProbs(inputText, qObservedBigramProbs, qUnobservedBoBigramProbs, alphaTrigram)

# Calculate discounted probability mass for quadgrams
getAlphaQuadgram <- function(observedQuadgrams, trigram, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(1)
        alphaQuadgram <- 1 - sum((observedQuadgrams$count - quadgramDisc) / trigram$count[1])
        
        return(alphaQuadgram)
}

trigram <- trigrams[trigrams$word1 == makeTokens(inputText, n = 1L)$text1[1] &
                    trigrams$word2 == makeTokens(inputText, n = 1L)$text1[2] &
                    trigrams$word3 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaQuadgram <- getAlphaQuadgram(observedQuadgrams, trigram, gamma4)

# Calculate UNOBSERVED quadgram probabilities
getUnobservedQuadgramProbs <- function(inputString, qBoObservedTrigrams, qBoUnobservedTrigrams, alphaQuadgram) {
        qBoTrigrams <- rbind(qBoObservedTrigrams, qBoUnobservedTrigrams)
        qBoTrigrams <- qBoTrigrams[order(-qBoTrigrams$prob), ]
        sumqBoTrigrams <- sum(qBoTrigrams$prob)
        word <- makeTokens(inputString, n = 1L)$text1[[1]]
        # unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = "_")
        unobservedQuadgramProbs <- alphaQuadgram * qBoTrigrams$prob / sumqBoTrigrams
        unobservedQuadgramsDT <- data.table(word1 = word, word2 = qBoTrigrams$word1, 
                                            word3 = qBoTrigrams$word2, 
                                            word4 = qBoTrigrams$word3,
                                            prob = unobservedQuadgramProbs)
        
        return(unobservedQuadgramsDT)
}

unobservedQuadgramProbs <- getUnobservedQuadgramProbs(inputText, qObservedBoTrigramProbs, unobservedTrigramProbs, alphaQuadgram)


qBoQuadgrams <- rbind(qObservedQuadgramProbs, unobservedQuadgramProbs)
qBoQuadgrams <- qBoQuadgrams[order(-qBoQuadgrams$prob), ]




