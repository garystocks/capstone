# Model Build
# The strategy is to load each source (twitter, news, blogs) separately.
# This script loads and transforms TWITTER data

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

twitter <- gsub("[[:punct:,^\\']]"," ", twitter) # all punctuation except apostrophes
twitter <- gsub("[0-9]+", "", twitter) # numbers
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

# Divide TWITTER data into 3------------------------------------------------------------

# Get number of rows in training data set
n <- nrow(twitterTraining)

# Divide training data into 3
twitterTrain1 <- twitterTraining[1:(n/3)]
twitterTrain2 <- twitterTraining[(n/3 + 1):((n*2)/3)]
twitterTrain3 <- twitterTraining[((n*2)/3 + 1):n]

# Create TWITTER corpus ------------------------------------------------------------------------

# Create quanteda corpus with training data
qCorpus1 <- quanteda::corpus(twitterTrain1)
qCorpus2 <- quanteda::corpus(twitterTrain2)
qCorpus3 <- quanteda::corpus(twitterTrain3)

# Download a list of profanities to remove
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)

# Create tokens from corpus objects
tokens1 <- tokens(qCorpus1, what = "word")
tokens1 <- tokens_remove(tokens1, profanity) # remove profanity

tokens2 <- tokens(qCorpus2, what = "word")
tokens2 <- tokens_remove(tokens2, profanity) # remove profanity

tokens3 <- tokens(qCorpus3, what = "word")
tokens3 <- tokens_remove(tokens3, profanity) # remove profanity

gc()

# Extract unigrams
unigrams1 <- tokens_ngrams(tokens1, n = 1L, skip = 0L, concatenator = "_")
unigrams2 <- tokens_ngrams(tokens2, n = 1L, skip = 0L, concatenator = "_")
unigrams3 <- tokens_ngrams(tokens3, n = 1L, skip = 0L, concatenator = "_")

# Create document feature matrices
dfm1 <- dfm(unigrams1)
dfm2 <- dfm(unigrams2)
dfm3 <- dfm(unigrams3)

# Delete singletons
#dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
#dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
#dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")

# Combine unigrams
unigramsDfm <- rbind(dfm1, dfm2, dfm3)

# Count unigrams
unigramsSums <- colSums(unigramsDfm)

# Create a data table with unigrams
unigrams <- data.table(word1 = names(unigramsSums), count = unigramsSums)

# Extract bigrams
bigrams1 <- tokens_ngrams(tokens1, n = 2L, skip = 0L, concatenator = "_")
bigrams2 <- tokens_ngrams(tokens2, n = 2L, skip = 0L, concatenator = "_")
bigrams3 <- tokens_ngrams(tokens3, n = 2L, skip = 0L, concatenator = "_")

# Create document feature matrices
dfm1 <- dfm(bigrams1)
dfm2 <- dfm(bigrams2)
dfm3 <- dfm(bigrams3)

# Delete singletons
#dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
#dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
#dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")

bigramsDfm <- rbind(dfm1, dfm2, dfm3)

# Count bigrams
bigramsSums <- colSums(bigramsDfm)

# Create a data table with bigrams
bigrams <- data.table(word1 = sapply(strsplit(names(bigramsSums), "_", fixed = TRUE), '[[', 1),
                      word2 = sapply(strsplit(names(bigramsSums), "_", fixed = TRUE), '[[', 2),
                      count = bigramsSums)

gc()

# Extract trigrams
trigrams1 <- tokens_ngrams(tokens1, n = 3L, skip = 0L, concatenator = "_")
trigrams2 <- tokens_ngrams(tokens2, n = 3L, skip = 0L, concatenator = "_")
trigrams3 <- tokens_ngrams(tokens3, n = 3L, skip = 0L, concatenator = "_")

# Create document feature matrices
dfm1 <- dfm(trigrams1)
dfm2 <- dfm(trigrams2)
dfm3 <- dfm(trigrams3)

# Delete singletons
#dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
#dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
#dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")

trigramsDfm <- rbind(dfm1, dfm2, dfm3)

# Count trigrams
trigramsSums <- colSums(trigramsDfm)

# Create a data table with trigrams
trigrams <- data.table(word1 = sapply(strsplit(names(trigramsSums), "_", fixed = TRUE), '[[', 1),
                      word2 = sapply(strsplit(names(trigramsSums), "_", fixed = TRUE), '[[', 2),
                      word3 = sapply(strsplit(names(trigramsSums), "_", fixed = TRUE), '[[', 3),
                      count = trigramsSums)

gc()

# Extract quadgrams
quadgrams1 <- tokens_ngrams(tokens1, n = 4L, skip = 0L, concatenator = "_")
quadgrams2 <- tokens_ngrams(tokens2, n = 4L, skip = 0L, concatenator = "_")
quadgrams3 <- tokens_ngrams(tokens3, n = 4L, skip = 0L, concatenator = "_")

# Create document feature matrices
dfm1 <- dfm(quadgrams1)
dfm2 <- dfm(quadgrams2)
dfm3 <- dfm(quadgrams3)

# Delete singletons
#dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
#dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
#dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")

# Combine quadgram DFMs
quadgramsDfm <- rbind(dfm1, dfm2, dfm3)

# Count quadgrams
quadgramsSums <- colSums(quadgramsDfm)

# Create a data table with quadgrams
quadgrams <- data.table(word1 = sapply(strsplit(names(quadgramsSums), "_", fixed = TRUE), '[[', 1),
                       word2 = sapply(strsplit(names(quadgramsSums), "_", fixed = TRUE), '[[', 2),
                       word3 = sapply(strsplit(names(quadgramsSums), "_", fixed = TRUE), '[[', 3),
                       word4 = sapply(strsplit(names(quadgramsSums), "_", fixed = TRUE), '[[', 4),
                       count = quadgramsSums)


# Define a function to create tokens
makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE,
               remove_url = TRUE,
               ngrams = n)
}

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

# Index the n-grams to improve performance
setkey(unigrams, word1)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3, word4)
# setkey(quingrams, word1, word2, word3, word4, word5)


# BACK OFF MODEL ----------------------------------------------------------------------

# Quiz 2
"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" # BEER
"You're the reason why I smile everyday. Can you follow me please? It would mean the" # WORLD
"Hey sunshine, can you follow me and make me the" # HAPPIEST
"Very early observations on the Bills game: Offense still struggling but the" 
"Go on a romantic date at the" # BEACH
"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" # WAY
"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" # TIME
"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
"Be grateful for the good times and keep the faith during the"
"If this isn't the cutest thing you've ever seen, then you must be" # INSANE

# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
gamma4 <- .5 # quadgram discount

inputText <- "and a case of"

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
getQuadgramsProbs <- function(observedQuadgrams, inputTrigrams, inputString, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(NULL)
        obsCount <- filter(inputTrigrams, word1 == makeTokens(inputString)$text1[1] & word2 == makeTokens(inputString)$text1[2] & word3 == makeTokens(inputString)$text1[3])$count[1]
        obsQuadgramProbs <- mutate(observedQuadgrams, count = ((count - quadgramDisc) / obsCount))
        colnames(obsQuadgramProbs) <- c("word1", "word2", "word3", "word4", "prob")
        
        return(obsQuadgramProbs)
}

# Find the tail words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams
getUnobservedQuadgramTails <- function(observedQuadgrams, inputUnigrams) {
        observedQuadgramTails <- observedQuadgrams$word4
        unobservedQuadgramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedQuadgramTails), ]$word1
        return(unobservedQuadgramTails)
}

# Calculate the discount mass probability for trigrams
getAlphaTrigram <- function(bigram, inputTrigrams, trigramDisc = .5) {
        # Get all trigrams that start with the bigram
        trigramsWithBigram <- inputTrigrams[inputTrigrams$word1 == bigram$word1 & 
                                            inputTrigrams$word2 == bigram$word2]
        if(nrow(trigramsWithBigram) < 1) return(0)
        alpha <- 1 - (sum(trigramsWithBigram$count - trigramDisc) / bigram$count)
        
        return(alpha)
}

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

# Calculate the probabilities of observed BO trigrams
getTrigramsProbs <- function(observedBoTrigrams, inputBigrams, inputString, trigramDisc = .5) {
        if(nrow(observedBoTrigrams) < 1) return(NULL)
        obsCount <- filter(inputBigrams, inputBigrams$word1 == makeTokens(inputString)$text1[1] & 
                                   inputBigrams$word2 == makeTokens(inputString)$text1[2])$count[1]
        obsTrigramProbs <- mutate(observedBoTrigrams, count = ((count - trigramDisc) / obsCount))
        colnames(obsTrigramProbs) <- c("word1", "word2", "word3", "prob")
        
        return(obsTrigramProbs)
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

# Calculate the discount mass probability for bigrams
# alpha for observed bigrams = 1 - sum_of((observed_bigram_count - discount) / unigram_count)
getAlphaBigram <- function(unigram, inputBigrams, bigramDisc = .5) {
        # Get all bigrams that start with the unigram
        bigramsWithUnigram <- inputBigrams[word1 == unigram$word1]
        if(nrow(bigramsWithUnigram) < 1) return(0)
        alpha <- 1 - (sum(bigramsWithUnigram$count - bigramDisc) / unigram$count)
        
        return(alpha)
}

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


# Why is this here????
unigram <- makeTokens(inputText, n = 1L)$text1[[2]]
unigram <- unigrams[unigrams$word1 == unigram]

# Calculate discounted probability mass for quadgrams
getAlphaQuadgram <- function(observedQuadgrams, trigram, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(1)
        alphaQuadgram <- 1 - sum((observedQuadgrams$count - quadgramDisc) / trigram$count[1])
        
        return(alphaQuadgram)
}

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

# Define the function to compute the back-off probabilities
computeProbs <- function(inputText, inputUnigrams, inputBigrams, inputTrigrams, inputQuadgrams, 
                         gamma1, gamma2, gamma3, gamma4) {
        # Get quadgrams and counts
        observedQuadgrams <- getObservedQuadgrams(inputText, inputQuadgrams)
        # Convert quadgram counts to probabilities
        qBoObservedQuadgrams <- getQuadgramsProbs(observedQuadgrams, inputTrigrams, inputText, gamma4)
        # Get unobserved quadgram tails
        unobservedQuadgramTails <- getUnobservedQuadgramTails(observedQuadgrams, inputUnigrams)
        
        # Calculate the probability mass for trigrams
        bigram <- inputBigrams[inputBigrams$word1 %in% makeTokens(inputText, n = 1L)$text1[[2]] & 
                                       inputBigrams$word2 %in% makeTokens(inputText, n = 1L)$text1[[3]]]
        alphaTrigram <- getAlphaTrigram(bigram, inputTrigrams, gamma3)
        # Get observed BO trigrams
        observedBoTrigrams <- getObservedBoTrigrams(inputText, unobservedQuadgramTails, inputTrigrams)
        # Get unobserved trigram tails
        unobservedTrigramTails <- getUnobservedTrigramTails(observedBoTrigrams, inputUnigrams)
        # Calculate probabilities for observed BO trigrams
        qObservedBoTrigrams <- getTrigramsProbs(observedBoTrigrams, inputBigrams, inputText, trigramDisc = .5)
                
        # Calculate discount mass for bigrams
        unigram <- makeTokens(inputText, n = 1L)$text1[[2]]
        unigram <- inputUnigrams[inputUnigrams$word1 == unigram]
        alphaBigram <- getAlphaBigram(unigram, inputBigrams, gamma2)
        # Separate observed and unobserved BO bigrams
        observedBoBigrams <- getObservedBoBigrams(inputText, unobservedTrigramTails, inputBigrams)
        unobservedBoBigrams <- getUnobservedBoBigrams(inputText, unobservedTrigramTails, observedBoBigrams)
        # calculate observed bigram probabilities
        qBoObservedBigrams <- getObservedBigramProbs(observedBoBigrams, inputUnigrams, gamma2)
        # Distribute discounted bigram probability mass to unobserved bigrams in proportion to unigram MLE
        qBoUnobservedBigrams <- getqBoUnobservedBigrams(unobservedBoBigrams, inputUnigrams, alphaBigram)
        qBoBigrams <- rbind(qBoObservedBigrams, qBoUnobservedBigrams)
        
        # Calculate probabilities for UNOBSERVED trigrams
        qUnobservedBoTrigrams <- getUnobservedTrigramProbs(inputText, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram)
        qBoTrigrams <- rbind(qobservedBoTrigrams, qUnobservedBoTrigrams)
        
        # Calculate discount probability mass for quadgrams
        trigram <- inputTrigrams[inputTrigrams$word1 %in% makeTokens(inputText, n = 1L)$text1[[1]] & 
                                    inputTrigrams$word2 %in% makeTokens(inputText, n = 1L)$text1[[2]] &
                                    inputTrigrams$word3 %in% makeTokens(inputText, n = 1L)$text1[[3]]]

        # Calculate discount probability mass for quadgrams
        alphaQuadgram <- getAlphaQuadgram(observedQuadgrams, trigram, gamma4)
        # Calculate probabilities for UNOBSERVED quadgrams
        qBoUnobservedQuadgrams <- getUnobservedQuadgramProbs(inputText, qBoObservedTrigrams, qBoUnobservedTrigrams, alphaQuadgram)
        # 
        qBoQuadgrams <- rbind(qBoObservedQuadgrams, qBoUnobservedQuadgrams)
        qBoQuadgrams <- qBoQuadgrams[order(-qBoQuadgrams$prob), ]
        
        return(qBoQuadgrams)
        
}

x <- computeProbs(inputText, unigrams, bigrams, trigrams, quadgrams, gamma1, gamma2, gamma3, gamma4)

# Select word with the highest probability
getPrediction <- function(qBoQuadgrams) {
        # find tail word of highest probability trigram
        prediction <- qBoQuadgrams$word4
        # result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
        #                  " <<< which has probability = ", qBoQuadgrams$prob[1])
        
        return(result)
}


output <- getPrediction(x)
output





# Load NEWS data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
n <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(n, encoding='UTF-8', skipNul = TRUE)
close(n)

# Clean NEWS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
news <- gsub("&amp;|&lt;|&gt;", "", news, ignore.case = TRUE) # special characters
news <- gsub("[0-9]+", "", news) # numbers
news <- gsub("ðÿ", "", news, fixed = TRUE) # special characters
news <- gsub("â", "", news, fixed = TRUE) # special characters
news <- gsub("[^a-zA-Z]", " ", news)

# Store NEWS data ---------------------------------------------------------------------------

# Store in a data table
newsTable <- data.table(doc = rep("News", length(news)), text = news)

# Select a random sample of 80% for training and the remaining for testing
sampleSize <- floor(.8 * nrow(newsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(newsTable)), size = sampleSize) 

newsTraining <- newsTable[trainIndex, ]
newsTesting <- newsTable[-trainIndex, ]

# Create NEWS corpus ------------------------------------------------------------------------

# Create quanteda corpus with training data
qCorpus <- quanteda::corpus(newsTraining)

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

# Extract sentences
sentences <- parallelizeTask(makeSentences, qCorpus)

# Extract n-grams
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)
ngram5 <- parallelizeTask(makeTokens, sentences, 5)

# Use document-feature-matrices to count n-grams
dfm1 <- parallelizeTask(dfm, ngram1, remove=profanity)
dfm2 <- parallelizeTask(dfm, ngram2, remove=profanity)
dfm3 <- parallelizeTask(dfm, ngram3, remove=profanity)
dfm4 <- parallelizeTask(dfm, ngram4, remove=profanity)
dfm5 <- parallelizeTask(dfm, ngram5, remove=profanity)

# Delete singletons
dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")
dfm4 <- dfm_trim(dfm4, min_termfreq = 2, termfreq_type = "count")
dfm5 <- dfm_trim(dfm5, min_termfreq = 2, termfreq_type = "count")

# Create vectors with sum of frequencies for each n-gram
ngram1Sums <- colSums(dfm1)
ngram2Sums <- colSums(dfm2)
ngram3Sums <- colSums(dfm3)
ngram4Sums <- colSums(dfm4)
ngram5Sums <- colSums(dfm5)

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

quingrams <- data.table(word1 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 1),
                        word2 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 2),
                        word3 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 3),
                        word4 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 4),
                        word5 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 5),
                        count = ngram5Sums)

# Index the n-grams to improve performance
setkey(unigrams, word1)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3, word4)
setkey(quingrams, word1, word2, word3, word4, word5)


# BACK OFF MODEL ----------------------------------------------------------------------

# Quiz 2
"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" # BEER
"You're the reason why I smile everyday. Can you follow me please? It would mean the" # WORLD
"Hey sunshine, can you follow me and make me the" # HAPPIEST
"Very early observations on the Bills game: Offense still struggling but the" # PLAYERS?? defense / crowd / referees / players
"Go on a romantic date at the" # BEACH
"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" # WAY
"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" # TIME
"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" # fingers / eyes / ears / toes
"Be grateful for the good times and keep the faith during the" # BAD
"If this isn't the cutest thing you've ever seen, then you must be" # INSANE

# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
gamma4 <- .5 # quadgram discount
gamma5 <- .5 # quingram discount

inputText <- "still struggling but the"

# Find OBSERVED quingrams and their counts
getObservedQuingrams <- function(inputString, inputQuingrams) {
        trigramsFound <- data.table(word1 = vector(mode = "character", length = 0),
                                    word2 = vector(mode = "character", length = 0),
                                    word3 = vector(mode = "character", length = 0),
                                    word4 = vector(mode = "character", length = 0),
                                    count = vector(mode = "integer", length = 0))
        words <- makeTokens(inputString, n = 1L)
        quingramIndices <- inputQuingrams[inputQuingrams$word1 == words$text1[1] & 
                                                  inputQuingrams$word2 == words$text1[2] &
                                                  inputQuingrams$word3 == words$text[3] &
                                                  inputQuingrams$word4 == words$text[4], ]
        quingramsFound <- quingramIndices
        # if(length(trigramIndices) > 0 ) {
        #         trigramsFound <- trigramIndices[trigramIndices, c("word1", "word2", "word3", "count")]
        # }
        return(quingramsFound)
}

# Calculate the probabilities of OBSERVED quingrams beginning with the quadgram inputText
# Probability for observed quingrams = (count_of_observed_quingram - discount) / total_quadgram_count
getQuingramsProbs <- function(observedQuingrams, inputQuadgrams, inputString, quingramDisc = .5) {
        if(nrow(observedQuingrams) < 1) return(NULL)
        obsCount <- filter(inputQuadgrams, word1 == makeTokens(inputString)$text1[1] & 
                                   word2 == makeTokens(inputString)$text1[2] &
                                   word3 == makeTokens(inputString)$text1[3] &
                                   word4 == makeTokens(inputString)$text1[4])$count[1]
        obsQuingramProbs <- mutate(observedQuingrams, count = ((count - quingramDisc) / obsCount))
        colnames(obsQuingramProbs) <- c("word1", "word2", "word3", "word4", "word5", "prob")
        
        return(obsQuingramProbs)
}

# Get quingrams and counts
observedQuingrams <- getObservedQuingrams(inputText, quingrams)

# Convert counts to probabilities
qBoObservedQuingrams <- getQuingramsProbs(observedQuingrams, quadgrams, inputText, gamma5)
qBoObservedQuingrams


# Load BLOGS data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
b <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(b, encoding='UTF-8', skipNul = TRUE)
close(b)

# Clean BLOGS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
blogs <- gsub("&amp;|&lt;|&gt;", "", blogs, ignore.case = TRUE) # special characters
blogs <- gsub("[0-9]+", "", blogs) # numbers
blogs <- gsub("ðÿ", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("â", "", blogs, fixed = TRUE) # special characters
blogs <- gsub("[^a-zA-Z]", " ", blogs)

# Store BLOGS data ---------------------------------------------------------------------------

# Store in a data table
blogsTable <- data.table(doc = rep("Blogs", length(blogs)), text = blogs)

# Select a random sample of 80% for training and the remaining for testing
sampleSize <- floor(.8 * nrow(blogsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(blogsTable)), size = sampleSize) 

blogsTraining <- blogsTable[trainIndex, ]
blogsTesting <- blogsTable[-trainIndex, ]

# Create BLOGS corpus ------------------------------------------------------------------------

# Create quanteda corpus with training data
qCorpus <- quanteda::corpus(blogsTraining)

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

# Extract sentences
sentences <- parallelizeTask(makeSentences, qCorpus)

# Extract n-grams
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)
ngram5 <- parallelizeTask(makeTokens, sentences, 5)

# Use document-feature-matrices to count n-grams
dfm1 <- parallelizeTask(dfm, ngram1, remove=profanity)
dfm2 <- parallelizeTask(dfm, ngram2, remove=profanity)
dfm3 <- parallelizeTask(dfm, ngram3, remove=profanity)
dfm4 <- parallelizeTask(dfm, ngram4, remove=profanity)
dfm5 <- parallelizeTask(dfm, ngram5, remove=profanity)

# Delete singletons
dfm1 <- dfm_trim(dfm1, min_termfreq = 2, termfreq_type = "count")
dfm2 <- dfm_trim(dfm2, min_termfreq = 2, termfreq_type = "count")
dfm3 <- dfm_trim(dfm3, min_termfreq = 2, termfreq_type = "count")
dfm4 <- dfm_trim(dfm4, min_termfreq = 2, termfreq_type = "count")
dfm5 <- dfm_trim(dfm5, min_termfreq = 2, termfreq_type = "count")

# Create vectors with sum of frequencies for each n-gram
ngram1Sums <- colSums(dfm1)
ngram2Sums <- colSums(dfm2)
ngram3Sums <- colSums(dfm3)
ngram4Sums <- colSums(dfm4)
ngram5Sums <- colSums(dfm5)

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

quingrams <- data.table(word1 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 1),
                        word2 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 2),
                        word3 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 3),
                        word4 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 4),
                        word5 = sapply(strsplit(names(ngram5Sums), "_", fixed = TRUE), '[[', 5),
                        count = ngram5Sums)

# Index the n-grams to improve performance
setkey(unigrams, word1)
setkey(bigrams, word1, word2)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3, word4)
setkey(quingrams, word1, word2, word3, word4, word5)


# BACK OFF MODEL ----------------------------------------------------------------------

# Quiz 2
"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" # BEER
"You're the reason why I smile everyday. Can you follow me please? It would mean the" # WORLD
"Hey sunshine, can you follow me and make me the" # HAPPIEST
"Very early observations on the Bills game: Offense still struggling but the" # defense / crowd / referees / NOT players
"Go on a romantic date at the" # BEACH
"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" # WAY
"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" # TIME
"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" # fingers / eyes / ears / toes
"Be grateful for the good times and keep the faith during the" # BAD
"If this isn't the cutest thing you've ever seen, then you must be" # INSANE

# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
gamma4 <- .5 # quadgram discount
gamma5 <- .5 # quingram discount

inputText <- "still struggling but the"

# Find OBSERVED quingrams and their counts
getObservedQuingrams <- function(inputString, inputQuingrams) {
        trigramsFound <- data.table(word1 = vector(mode = "character", length = 0),
                                    word2 = vector(mode = "character", length = 0),
                                    word3 = vector(mode = "character", length = 0),
                                    word4 = vector(mode = "character", length = 0),
                                    count = vector(mode = "integer", length = 0))
        words <- makeTokens(inputString, n = 1L)
        quingramIndices <- inputQuingrams[inputQuingrams$word1 == words$text1[1] & 
                                                  inputQuingrams$word2 == words$text1[2] &
                                                  inputQuingrams$word3 == words$text[3] &
                                                  inputQuingrams$word4 == words$text[4], ]
        quingramsFound <- quingramIndices
        # if(length(trigramIndices) > 0 ) {
        #         trigramsFound <- trigramIndices[trigramIndices, c("word1", "word2", "word3", "count")]
        # }
        return(quingramsFound)
}

# Calculate the probabilities of OBSERVED quingrams beginning with the quadgram inputText
# Probability for observed quingrams = (count_of_observed_quingram - discount) / total_quadgram_count
getQuingramsProbs <- function(observedQuingrams, inputQuadgrams, inputString, quingramDisc = .5) {
        if(nrow(observedQuingrams) < 1) return(NULL)
        obsCount <- filter(inputQuadgrams, word1 == makeTokens(inputString)$text1[1] & 
                                   word2 == makeTokens(inputString)$text1[2] &
                                   word3 == makeTokens(inputString)$text1[3] &
                                   word4 == makeTokens(inputString)$text1[4])$count[1]
        obsQuingramProbs <- mutate(observedQuingrams, count = ((count - quingramDisc) / obsCount))
        colnames(obsQuingramProbs) <- c("word1", "word2", "word3", "word4", "word5", "prob")
        
        return(obsQuingramProbs)
}

# Get quingrams and counts
observedQuingrams <- getObservedQuingrams(inputText, quingrams)

# Convert counts to probabilities
qBoObservedQuingrams <- getQuingramsProbs(observedQuingrams, quadgrams, inputText, gamma5)
qBoObservedQuingrams


