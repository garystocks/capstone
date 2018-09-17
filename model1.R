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
inputText <- "must be"

# Find OBSERVED quadgrams and the counts

# Calculate the probabilities of observed quadgrams beginning with inputText

# Find the tail words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams

# Calculate the discount mass probability for trigrams

# Get backed off trigrams

# Find OBSERVED trigrams from BO trigrams

# Find UNOBSERVED trigrams from BO trigrams

# Calculate probabilities for OBSERVED BO trigrams

# Calculate probabilities for UNOBSERVED BO trigrams

# Distribute discounted trigram probability to UNOBSERVED trigrams in proportion to ???


# Find OBSERVED trigrams and their counts
getObservedTrigrams <- function(inputString, inputTrigrams) {
        trigramsFound <- data.table(word1 = vector(mode = "character", length = 0),
                                    word2 = vector(mode = "character", length = 0),
                                    word3 = vector(mode = "character", length = 0),
                                    count = vector(mode = "integer", length = 0))
        words <- makeTokens(inputString, n = 1L)
        trigramIndices <- inputTrigrams[inputTrigrams$word1 == words$text1[1] & inputTrigrams$word2 == words$text1[2], ]
        if(length(trigramIndices) > 0 ) {
                trigramsFound <- trigramIndices[trigramIndices, c("word1", "word2", "word3", "count")]
        }
        return(trigramsFound)
}

# Calculate the probabilities of OBSERVED trigrams beginning with the bigram inputText
# Probability for observed trigrams = (count_of_observed_trigram - discount) / total_bigram_count
getTrigramsProbs <- function(observedTrigrams, inputBigrams, inputString, trigramDisc = .5) {
        if(nrow(observedTrigrams) < 1) return(NULL)
        obsCount <- filter(inputBigrams, word1 == makeTokens(inputString)$text1[1] & word2 == makeTokens(inputString)$text1[2])$count[1]
        obsTrigramProbs <- mutate(observedTrigrams, count = ((count - trigramDisc) / obsCount))
        colnames(obsTrigramProbs) <- c("word1", "word2", "word3", "prob")
        
        return(obsTrigramProbs)
}

# Get trigrams and counts
observedTrigrams <- getObservedTrigrams(inputText, trigrams)

# Convert counts to probabilities
qBoObservedTrigrams <- getTrigramsProbs(observedTrigrams, bigrams, inputText, gamma3)
qBoObservedTrigrams


# Find the tail words of UNOBSERVED trigrams that start with the first 2 words of observedTrigrams
getUnobservedTrigramTails <- function(observedTrigrams, inputUnigrams) {
        observedTrigramTails <- observedTrigrams$word3
        unobservedTrigramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedTrigramTails), ]$word1
        return(unobservedTrigramTails)
}

unobservedTrigramTails <- getUnobservedTrigramTails(observedTrigrams, unigrams)
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

# Calculate discounted probability mass for trigrams
getAlphaTrigram <- function(observedTrigrams, bigram, trigramDisc = .5) {
        if(nrow(observedTrigrams) < 1) return(1)
        alphaTrigram <- 1 - sum((observedTrigrams$count - trigramDisc) / bigram$count[1])
        
        return(alphaTrigram)
}

bigram <- bigrams[bigrams$word1 %in% makeTokens(inputText, n = 1L)$text1[[1]] & bigrams$word2 %in% makeTokens(inputText, n = 1L)$text1[[2]]]
alphaTrigram <- getAlphaTrigram(observedTrigrams, bigram, gamma3)
alphaTrigram

# Calculate UNOBSERVED trigram probabilities
getUnobservedTrigramProbs <- function(inputString, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram) {
        qBoBigrams <- rbind(qBoBigrams, qBoUnobservedBigrams)
        qBoBigrams <- qBoBigrams[order(-qBoBigrams$prob), ]
        sumqBoBigrams <- sum(qBoBigrams$prob)
        word <- makeTokens(inputString, n = 1L)$text1[[1]]
        # unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = "_")
        unobservedTrigramProbs <- alphaTrigram * qBoBigrams$prob / sumqBoBigrams
        unobservedTrigramsDT <- data.table(word1 = word, word2 = qBoBigrams$word1, 
                                           word3 = qBoBigrams$word2, prob = unobservedTrigramProbs)
        
        return(unobservedTrigramsDT)
}

qBoUnobservedTrigrams <- getUnobservedTrigramProbs(inputText, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram)
qBoUnobservedTrigrams

# Select word with the highest probability
getPrediction <- function(qBoTrigrams) {
        # find tail word of highest probability trigram
        prediction <- qBoTrigrams$word3[1]
        result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                          " <<< which has probability = ", qBoTrigrams$prob[1])
        
        return(result)
}

qBoTrigrams <- rbind(qBoObservedTrigrams, qBoUnobservedTrigrams)
qBoTrigrams <- qBoTrigrams[order(-qBoTrigrams$prob), ]
output <- getPrediction(qBoTrigrams)
output


### -----------------------------------------------------------------------------------
# Kneser-Key smoothing???
discount <- .75

# Find number of bigram words
ngram2Total <- nrow(ngram2Table[by = .(word1, word2)])

# Divide the number of times word 2 occurs as the second part of the bigram by the total number of bigrams 
ckn <- ngram2Table[, .(Prob = ((.N) / ngram2Total)), by = word2]
setkey(ckn, word2)

# Assign the probabilities as the second word of bigram to unigrams
ngram1Table[, Prob := ckn[word1, Prob]]
ngram1Table <- ngram1Table[!is.na(ngram1Table$Prob)]

# Find the number of times word 1 occurred as word 1 of bigrams
n1wi <- ngram2Table[, .(N = .N), by = word1]
setkey(n1wi, word1)

# Assign the total times word 1 occured to bigram cn1
ngram2Table[, Cn1 := ngram1Table[word1, count]]

# Kneser Kney Algorithm
ngram2Table[, Prob := ((count - discount) / Cn1 + 
                            discount / Cn1 * n1wi[word1, N] * ngram1Table[word2, Prob])]


# Find the count of word1-word2 combinations in bigrams 
ngram3Table[, Cn2 := ngram2Table[.(word1, word2), count]]

# Finding count of word1-word2 combination in trigram
n1w12 <- ngram3Table[, .N, by = .(word1, word2)]
setkey(n1w12, word1, word2)

# Kneser Kney Algorithm
ngram3Table[, Prob := (count - discount) / Cn2 + discount / Cn2 * n1w12[.(word1, word2), N] * ngram2Table[.(word1, word2), Prob]]

# Single out 50 most used unigrams
ngram1Table <- ngram1Table[order(-Prob)][1:50]

# Prediction algorithm - function to return highly probable previous word given two successive words
triWords <- function(w1, w2, n = 5) {
        pwords <- ngram3Table[.(w1, w2)][order(-Prob)]
        if (any(is.na(pwords)))
                return(biWords(w2, n))
        if (nrow(pwords) > n)
                return(pwords[1:n, word3])
        count <- nrow(pwords)
        bwords <- biWords(w2, n)[1:(n - count)]
        return(c(pwords[, word3], bwords))
}

# Back off to bigram - function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
        pwords <- ngram2Table[w1][order(-Prob)]
        if (any(is.na(pwords)))
                return(uniWords(n))
        if (nrow(pwords) > n)
                return(pwords[1:n, word2])
        count <- nrow(pwords)
        unWords <- uniWords(n)[1:(n - count)]
        return(c(pwords[, word2], unWords))
}

# Back off to unigram - function to return random words from unigrams
uniWords <- function(n = 5) {  
        return(sample(ngram1Table[, word1], size = n))
}

# The prediction app
getWords <- function(str){
        require(quanteda)
        tokens <- tokens(x = char_tolower(str))
        tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
        
        words <- triWords(tokens[1], tokens[2], 5)
        chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")
        
        print(words[1])
}


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
