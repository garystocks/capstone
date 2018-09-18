# Model build for QUADGRAMS model

# Libraries
library(ngram)
library(R.utils)
library(quanteda)
library(data.table)
library(sqldf)
library(doParallel)
require(dplyr)

# LOAD DATA ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
t <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
twitter <- readLines(t, skipNul = TRUE)
close(t)

n <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(n, skipNul = TRUE)
close(n)

b <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(b, skipNul = TRUE)
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

# Select a random sample of 60% for training and the remaining for testing
sampleSize <- floor(.6 * nrow(textTable))

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
ngram5 <- parallelizeTask(makeTokens, sentences, 5)
ngram6 <- parallelizeTask(makeTokens, sentences, 6)


dfm1 <- parallelizeTask(dfm, ngram1, remove=profanity)
dfm2 <- parallelizeTask(dfm, ngram2, remove=profanity)
dfm3 <- parallelizeTask(dfm, ngram3, remove=profanity)
dfm4 <- parallelizeTask(dfm, ngram4, remove=profanity)
dfm5 <- parallelizeTask(dfm, ngram5, remove=profanity)
dfm6 <- parallelizeTask(dfm, ngram6, remove=profanity)


# Delete singletons
# dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
# dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
# dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")
# dfm4 <- dfm_trim(dfm4, min_termfreq = 2, termfreq_type = "count")

# Create vectors with sum of frequencies for each n-gram
ngram1Sums <- colSums(dfm1)
ngram2Sums <- colSums(dfm2)
ngram3Sums <- colSums(dfm3)
ngram4Sums <- colSums(dfm4)

# Create data tables with individual words and n-gram count as the variables
unigrams <- data.table(word1 = names(ngram1Sums), count = ngram1Sums)

bigrams <- data.table(word1 = sapply(strsplit(names(ngram2Sums), "_", fixed = TRUE), '[[', 1),
                      word2 = sapply(strsplit(names(ngram2Sums), "_", fixed = TRUE), '[[', 2),
                      count = ngram2Sums)

trigrams <- data.table(word1 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 1),
                       word2 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 2),
                       word3 = sapply(strsplit(names(ngram3Sums), "_", fixed = TRUE), '[[', 3),
                       count = ngram3Sums)

quadgrams <- data.table(word1 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 1),
                        word2 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 2),
                        word3 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 3),
                        word4 = sapply(strsplit(names(ngram4Sums), "_", fixed = TRUE), '[[', 4),
                        count = ngram4Sums)

# Index the n-grams to improve performance
setkey(unigrams, word1)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3)


# BACK OFF MODEL ----------------------------------------------------------------------


### DEV DATA SET - DELETE ONCE TESTED ###

# unigrams <- data.table(word1 = c("book", "buy", "eos", "house", "paint", "sell", "the"), 
#                        count = as.integer(c(5, 6, 8, 3, 1, 1, 8)))
# bigrams <- data.table(word1 = c("book", "buy", "house", "paint", "sell", "sos", "sos", "sos", "the", "the"),
#                       word2 = c("eos", "the", "eos", "the", "the", "buy", "paint", "sell", "book", "house"),
#                       count = as.integer(c(5, 6, 3, 1, 1, 6, 1, 1, 5, 3)))
# trigrams <- data.table(word1 = c("buy", "buy", "paint", "sell", "sos", "sos", "sos", "the", "the"),
#                        word2 = c("the", "the", "the", "the", "buy", "paint", "sell", "book", "house"),
#                        word3 = c("book", "house", "house", "book", "the", "the", "the", "eos", "eos"),
#                        count = as.integer(c(4, 2, 1, 1, 6, 1, 1, 5, 3)))

# setkey(unigrams, word1)
# setkey(bigrams, word1, word2)
# setkey(trigrams, word1, word2, word3)

### END ###


# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
gamma4 <- .5 # quadgram discount
inputText <- "you must be"

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

# Calculate the probabilities of observed quadgrams beginning with inputText
getQuadgramsProbs <- function(observedQuadgrams, inputTrigrams, inputString, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(NULL)
        obsCount <- filter(inputTrigrams, word1 == makeTokens(inputString)$text1[1] & word2 == makeTokens(inputString)$text1[2] & word3 == makeTokens(inputString)$text1[3])$count[1]
        obsQuadgramProbs <- mutate(observedQuadgrams, count = ((count - quadgramDisc) / obsCount))
        colnames(obsQuadgramProbs) <- c("word1", "word2", "word3", "word4", "prob")
        
        return(obsQuadgramProbs)
}

# Get quadgrams and counts
observedQuadgrams <- getObservedQuadgrams(inputText, quadgrams)

# Convert counts to probabilities
qBoObservedQuadgrams <- getQuadgramsProbs(observedQuadgrams, trigrams, inputText, gamma4)
qBoObservedQuadgrams


# Find the tail words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams
getUnobservedQuadgramTails <- function(observedQuadgrams, inputUnigrams) {
        observedQuadgramTails <- observedQuadgrams$word4
        unobservedQuadgramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedQuadgramTails), ]$word1
        return(unobservedQuadgramTails)
}

unobservedQuadgramTails <- getUnobservedQuadgramTails(observedQuadgrams, unigrams)
unobservedQuadgramTails

# Calculate the discount mass probability for trigrams
getAlphaTrigram <- function(bigram, inputTrigrams, trigramDisc = .5) {
        # Get all trigrams that start with the bigram
        trigramsWithBigram <- inputTrigrams[word1 == bigram$word1 & word2 == bigram$word2]
        if(nrow(trigramsWithBigram) < 1) return(0)
        alpha <- 1 - (sum(trigramsWithBigram$count - trigramDisc) / bigram$count)
        
        return(alpha)
}

bigram <- bigrams[bigrams$word1 %in% makeTokens(inputText, n = 1L)$text1[[2]] & bigrams$word2 %in% makeTokens(inputText, n = 1L)$text1[[3]]]
alphaTrigram <- getAlphaTrigram(bigram, trigrams, gamma3)
alphaTrigram

# Get backed off trigrams
getBoTrigrams <- function(inputString, unobservedQuadgramTails) {
        w_i_minus2 <- makeTokens(inputString, n = 1L)$text1[[2]]
        w_i_minus1 <- makeTokens(inputString, n = 1L)$text1[[3]]
        boTrigrams <- data.table(word1 = rep(w_i_minus2, length(unobservedQuadgramTails)), 
                                 word2 = rep(w_i_minus1, length(unobservedQuadgramTails)), 
                                 word3 = unobservedQuadgramTails)
        return(boTrigrams)
}

# Find OBSERVED trigrams from BO trigrams
getObservedBoTrigrams <- function(inputString, unobservedQuadgramTails, inputTrigrams) {
        boTrigrams <- getBoTrigrams(inputString, unobservedQuadgramTails)
        observedBoTrigrams <- inputTrigrams[inputTrigrams$word1 %in% boTrigrams$word1 & 
                                                    inputTrigrams$word2 %in% boTrigrams$word2 &
                                                    inputTrigrams$word3 %in% boTrigrams$word3]
        return(observedBoTrigrams)
}

# Find UNOBSERVED trigrams from BO trigrams
getUnobservedBoTrigrams <- function(inputString, unobservedQuadgramTails, observedBoTrigrams) {
        boTrigrams <- getBoTrigrams(inputString, unobservedQuadgramTails)
        unobservedTrigrams <- boTrigrams[!(boTrigrams$word1 %in% observedBoTrigrams$word1 & 
                                                   boTrigrams$word2 %in% observedBoTrigrams$word2 &
                                                   boTrigrams$word3 %in% observedBoTrigrams$word3)]
        return(unobservedTrigrams)
}

# Calculate probabilities for OBSERVED BO trigrams
getObservedTrigramProbs <- function(observedBoTrigrams, inputBigrams, trigramDisc = .5) {
        bigramWord1 <- observedBoTrigrams$word1[1]
        bigramWord2 <- observedBoTrigrams$word2[1]
        wordCount <- inputBigrams[inputBigrams$word1 == bigramWord1 & 
                                          inputBigrams$word2 == bigramWord2]
        observedTrigramProbs <- (observedBoTrigrams$count - trigramDisc) / wordCount$count
        observedTrigramProbs <- data.table(word1 = observedBoTrigrams$word1, 
                                          word2 = observedBoTrigrams$word2, 
                                          word3 = observedBoTrigrams$word3,
                                          prob = observedTrigramProbs)
        
        return(observedTrigramProbs)
}


# Find the tail words of UNOBSERVED trigrams that start with the first 2 words of observedTrigrams
getUnobservedTrigramTails <- function(observedTrigrams, inputUnigrams) {
        observedTrigramTails <- observedTrigrams$word3
        unobservedTrigramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedTrigramTails), ]$word1
        return(unobservedTrigramTails)
}

unobservedTrigramTails <- getUnobservedTrigramTails(observedBoTrigrams, unigrams)
unobservedTrigramTails

# Calculate the discount mass probability for bigrams
# alpha for observed bigrams = 1 - sum_of((observed_bigram_count - discount) / unigram_count)
getAlphaBigram <- function(unigram, inputBigrams, bigramDisc = .5) {
        # Get all bigrams that start with the unigram
        bigramsWithUnigram <- inputBigrams[word1 == unigram$word1]
        if(nrow(bigramsWithUnigram) < 1) return(0)
        alpha <- 1 - (sum(bigramsWithUnigram$count - bigramDisc) / unigram$count)
        
        return(alpha)
}

unigram <- makeTokens(inputText, n = 1L)$text1[[2]]
unigram <- unigrams[unigrams$word1 == unigram]
alphaBigram <- getAlphaBigram(unigram, bigrams, gamma2)
alphaBigram

# Get backed off bigrams
getBoBigrams <- function(inputString, unobservedTrigramTails) {
        w_i_minus1 <- makeTokens(inputString, n = 1L)$text1[[2]]
        boBigrams <- data.table(word1 = rep(w_i_minus1, length(unobservedTrigramTails)), word2 = unobservedTrigramTails)
        return(boBigrams)
}

# Get OBSERVED bigrams from the set of BO bigrams
getObservedBoBigrams <- function(inputString, unobservedTrigramTails, inputBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        observedBoBigrams <- inputBigrams[inputBigrams$word1 %in% boBigrams$word1 & inputBigrams$word2 %in% boBigrams$word2]
        return(observedBoBigrams)
}

# Get UNOBSERVED bigrams from the set of BO bigrams
getUnobservedBoBigrams <- function(inputString, unobservedTrigramTails, observedBoBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        unobservedBigrams <- boBigrams[!(boBigrams$word1 %in% observedBoBigrams$word1 & boBigrams$word2 %in% observedBoBigrams$word2)]
        return(unobservedBigrams)
}

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

boBigrams <- getBoBigrams(inputText, unobservedTrigramTails)
# Separate according to different formulae
observedBoBigrams <- getObservedBoBigrams(inputText, unobservedTrigramTails, bigrams)
unobservedBoBigrams <- getUnobservedBoBigrams(inputText, unobservedTrigramTails, observedBoBigrams)
# calculate observed bigram probabilities
qBoObservedBigrams <- getObservedBigramProbs(observedBoBigrams, unigrams, gamma2)

# Why is this here????
unigram <- makeTokens(inputText, n = 1L)$text1[[2]]
unigram <- unigrams[unigrams$word1 == unigram]

# Distribute discounted bigram probability mass to unobserved bigrams in proportion to unigram MLE
qBoUnobservedBigrams <- getqBoUnobservedBigrams(unobservedBoBigrams, unigrams, alphaBigram)
qBoBigrams <- rbind(qBoObservedBigrams, qBoUnobservedBigrams)
qBoBigrams 

# Calculate discounted probability mass for quadgrams
getAlphaQuadgram <- function(observedQuadgrams, trigram, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(1)
        alphaQuadgram <- 1 - sum((observedQuadgrams$count - quadgramDisc) / trigram$count[1])
        
        return(alphaQuadgram)
}

trigram <- trigrams[trigrams$word1 %in% makeTokens(inputText, n = 1L)$text1[[1]] & 
                            trigrams$word2 %in% makeTokens(inputText, n = 1L)$text1[[2]] &
                            trigrams$word3 %in% makeTokens(inputText, n = 1L)$text1[[3]]]
alphaQuadgram <- getAlphaQuadgram(observedQuadgrams, trigram, gamma4)
alphaQuadgram

# Calculate UNOBSERVED quadgram probabilities
getUnobservedQuadgramProbs <- function(inputString, qBoObservedTrigrams, qBoUnobservedTrigrams, alphaQuadgram) {
        qBoTrigrams <- rbind(qBoTrigrams, qBoUnobservedTrigrams)
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

qBoUnobservedQuadgrams <- getUnobservedQuadgramProbs(inputText, qBoObservedTrigrams, qBoUnobservedTrigrams, alphaQuadgram)
qBoUnobservedQuadgrams


# Select word with the highest probability
getPrediction <- function(qBoQuadgrams) {
        # find tail word of highest probability trigram
        prediction <- qBoQuadgrams$word4[1]
        result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                          " <<< which has probability = ", qBoQuadgrams$prob[1])
        
        return(result)
}

qBoQuadgrams <- rbind(qBoObservedQuadgrams, qBoUnobservedQuadgrams)
qBoQuadgrams <- qBoQuadgrams[order(-qBoQuadgrams$prob), ]
output <- getPrediction(qBoQuadgrams)
output

