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

### FOR READLINES, ADD SKIPNUL = TRUE !!!!!

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
ngram5 <- parallelizeTask(makeTokens, sentences, 5)
ngram6 <- parallelizeTask(makeTokens, sentences, 6)


dfm1 <- parallelizeTask(dfm, ngram1, remove=profanity)
dfm2 <- parallelizeTask(dfm, ngram2, remove=profanity)
dfm3 <- parallelizeTask(dfm, ngram3, remove=profanity)
dfm4 <- parallelizeTask(dfm, ngram4, remove=profanity)
dfm5 <- parallelizeTask(dfm, ngram5, remove=profanity)
dfm6 <- parallelizeTask(dfm, ngram6, remove=profanity)


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

# Index the n-grams to improve performance
setkey(ngram1Table, word1)
setkey(ngram2Table, word1, word2)
setkey(ngram3Table, word1, word2, word3)


# BACK OFF MODEL ----------------------------------------------------------------------

# Set discounts
gamma2 <- .5 # bigram discount
gamma3 <- .5 # trigram discount
inputText <- "zoo with"

# Define function to return observed trigrams and their frequencies
getTrigrams_obs <- function(inputText, trigrams) {
        trigramsFound <- data.table(ngrams = vector(mode = "character", length = 0),
                                    count = vector(mode = "integer", length = 0))
        regex <- sprintf("%s%s%s", "^", inputText, "_")
        trigramIndices <- grep(regex, trigrams$ngram)
        if(length(trigramIndices) > 0 ) {
                trigramsFound <- trigramsFound[trigramIndices, ]
        }
        return(trigramsFound)
}

# Define function to calculate probability of observed trigrams that start with bigram inputText
getTrigrams_probs <- function(Trigrams_obs, bigrams, inputText, trigramDisc = .5) {
        if(nrow(Trigrams_obs) < 1) return(NULL)
        obsCount <- filter(bigrams, ngrams == inputText)$count[1]
        obsTrigramProbs <- mutate(Trigrams_obs, count = ((count - trigramDisc) / obsCount))
        colnames(obsTrigramProbs) <- c("ngram", "prob")
        
        return(obsTrigramProbs)
}

# Get trigrams and counts
obsTrigrams <- getTrigrams_obs(inputText, ngram3Table)

# Convert counts to probabilities
qboObsTrigrams <- getTrigrams_probs(obsTrigrams, ngram2Table, inputText, gamma3)
qboObsTrigrams

# Calculate probabilities of words completing unobserved trigrams

# Get tail words of unobserved trigrams that start with the first 2 words of obsTrigrams
getUnobservedTrigramTails < function(obsTrigrams, unigrams) {
        obsTrigramTails <- strsplit(obsTrigrams, "_", fixed = TRUE)[, 3]
        unobservedTrigramTails <- unigrams[!(unigrams$ngram %in% obsTrigramTails), ]$ngram
        return(unobservedTrigramTails)
}

unobservedTrigramTails <- getUnobservedTrigramTails(obsTrigrams$ngrams, ngram1Table)
unobservedTrigramTails

# Calculate discount mass probability for bigrams
# Get the total mass discounted from all observed bigrams
getAlphaBigram <- function(unigram, bigrams, bigramDisc = .5) {
        # Get all bigrams that start with the unigram
        regex <- sprintf("%s%s%s", "^", unigram$ngram[1], "_")
        bigramsWithUnigram <- bigrams[grep(regex, bigrams$ngram), ]
        if(nrow(bigramsWithUnigram) < 1) return(0)
        alpha <- 1 - (sum(bigramsWithUnigram$count - bigramDisc) / unigram$count)
        
        return(alpha)
}

unigram <- strsplit(inputText, "_")[[1]][2]
unigram <- ngram1Table[ngram1Table$ngram == unigram, ]
alphaBigram <- getAlphaBigram(unigram, ngram2Table, gamma2)
alphaBigram

# Get backed off bigrams
getBoBigrams < - function(inputText, unobservedTrigramTails) {
        w_i_minus1 <- strsplit(inputText, "_")[[1]][2]
        boBigrams <- paste(w_i_minus1, unobservedTrigramTails, sep = "_")
        return(boBigrams)
}

# 
getObservedBoBigrams <- function(inputText, unobservedTrigramTails, ngram2Table) {
        boBigrams <- getBoBigrams(inputText, unobservedTrigramTails)
        observedBoBigrams <- ngram2Table[ngram2Table$ngram %in% boBigrams, ]
        return(observedBoBigrams)
}

# Get backed off bigrams which are unobserved
getUnobservedBoBigrams <- function(inputText, unobservedTrigramTails, observedBoBigrams) {
        boBigrams <- getBoBigrams(inputText, unobservedTrigramTails)
        unobservedBigrams <- boBigrams[!(boBigrams %in% observedBoBigrams$ngram)]
        return(unobservedBigrams)
}

# Calculate q_bo(w1 | w2)
getObservedBigramProbs <- function(observedBoBigrams, unigrams, bigramDisc = .5) {
        word1 <- strsplit(observedBoBigrams, "_", fixed = TRUE)[, 1]
        word1Count <- unigrams[unigrams$ngram %in% word1, ]
        observedBigramProbs <- (observedBoBigrams$count - bigramDisc) / word1Count$count
        observedBigramProbs <- data.table(ngram = observedBoBigrams$ngram, prob = observedBigramProbs)
        
        return(observedBigramProbs)
}

# Calculate q_bo(w1 | w2) for unobserved bigrams
getqBoUnobservedBigrams <- function(unobservedBoBigrams, ngram1Table, alphaBigram) {
        # get unobserved bigram tails
        qBoUnobservedBigrams <- strsplit(unobservedBoBigrams, "_", fixed = TRUE)[, 2]
        w_in_Aw_iminus1 <- ngram1Table[!ngram1Table$ngram %in% qBoUnobservedBigrams, ]
        # convert to data table with counts
        qBoUnobservedBigrams <- ngram1Table[ngram1Table$ngram %in% qBoUnobservedBigrams, ]
        denom <- sum(qBoUnobservedBigrams)
        # convert counts to probabilities
        qBoUnobservedBigrams <- data.table(ngram = unobservedBoBigrams, prob = (alphaBigram * qBoUnobservedBigrams$count / denom))
        
        return(qBoUnobservedBigrams)
}

boBigrams <- getBoBigrams(inputText, unobservedTrigramTails)
# Separate according to different formulae
observedBoBigrams <- getObservedBoBigrams(inputText, unobservedTrigramTails, ngram2Table)
unobservedBigrams <- getUnobservedBoBigrams(inputText, unobservedTrigramTails, observedBoBigrams)
# calculate observed bigram probabilities
qBoObservedBigrams <- getObservedBigramProbs(observedBoBigrams, ngram1Table, gamma2)

unigram <- strsplit(inputText, "_")[[1]][2]
unigram <- ngram1Table[ngram1Table$ngram == unigram, ]

# Distribute discounted bigram probability mass to unobserved bigrams in proportion to unigram MLE
qBoUnobservedBigrams <- getqBoUnobservedBigrams(unobservedBoBigrams, ngram1Table, alphaBigram)
qBoBigrams <- rbind(qBoObservedBigrams, qBoUnobservedBigrams)
qBoBigrams 

# Calculate discounted probability mass for trigrams
# Get total probability mass for all observed trigrams
getAlphaTrigram <- function(observedTrigrams, bigram, trigramDisc = .5) {
        if(nrow(observedTrigrams) < 1) return(1)
        alphaTrigram <- 1 - sum((observedTrigrams$count - trigramDisc) / bigram$count[1])
        
        return(alphaTrigram)
}

bigram <- ngram2Table[ngram2Table$ngram %in% inputText, ]
alphaTrigram <- getAlphaTrigram(observedTrigrams, bigram, gamma3)
alphaTrigram

# Calculate unobserved trigram probabilities
getUnobservedTrigramProbs <- function(inputText, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram) {
        qBoBigrams <- rbind(qBoBigrams, qBoUnobservedBigrams)
        qBoBigrams <- qBoBigrams[order(-qBoBigrams$prob), ]
        sumqBoBigrams <- sum(qBoBigrams$prob)
        word1 <- strsplit(inputText, "_")[[1]][1]
        unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = "_")
        unobservedTrigramProbs <- alphaTrigram * qBoBigrams$prob / sumqBoBigrams
        unobservedTrigramsDT <- data.table(ngram = unobservedTrigramNgrams, prob = unobservedTrigramProbs)
        
        return(unobservedTrigramsDT)
}

qBoUnobservedTrigrams <- getUnobservedTrigramProbs(inputText, qBoObservedBigrams, qBoUnobservedBigrams, alphaTrigram)
qBoUnobservedTrigrams

# Select word with the highest probability
getPrediction <- function(qBoTrigrams) {
        # find tail word of highest probability trigram
        prediction <- strsplit(qBoTrigrams$ngram[1], "_")[[1]][3]
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
