# Model Build 8
# The strategy is to load each source (twitter, news, blogs) separately and then combine.

library(dplyr)
library(tidytext)
library(data.table)
library(quanteda)
library(sqldf)
library(hunspell)
library(tm)
library(readr)

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


# Get a list of commonly misspelt words ------------------------------------------------

con <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/misspellings.txt"
misspellings <- readLines(con)
close(con)


# Extract samples -----------------------------------------------------------------------

## NEED TO UPDATE THE SAMPLE CODE TO EXTRACT A TEST SET AT THE SAME TIME !!!
## 70% for training, 10% for HOLD OUT (for lambdas if using interpolation), 20% testing

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
                if (rbinom(1, 1, prob = .8) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
sampleFile(t, "twitter.txt", header = FALSE)

twitter <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt")

#con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt", "r")
#twitter <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
#close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
sampleFile(n, "news.txt", header = FALSE)

news <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt")

#con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
#news <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
#close(con)

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

blogs <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogs.txt")

#con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogs.txt", "r")
#blogs <- readLines(con, encoding = 'UTF-8', skipNul = TRUE) 
#close(con)


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

# Build 3 corpii and reshape to sentences
twitterCorpus <- quanteda::corpus(twitterClean)
twitterCorpus <- corpus_reshape(twitterCorpus, to = "sentences")
remove(twitterClean)
gc()

newsCorpus <- quanteda::corpus(newsClean)
newsCorpus <- corpus_reshape(newsCorpus, to = "sentences")
remove(newsClean)
gc()

blogsCorpus <- quanteda::corpus(blogsClean)
blogsCorpus <- corpus_reshape(blogsCorpus, to = "sentences")
remove(blogsClean)
gc()

# Combine text vectors
corpusList <- c(twitterCorpus, newsCorpus, blogsCorpus)

# Create corpus
myCorpus <- quanteda::corpus(corpusList)
remove(twitterCorpus)
remove(newsCorpus)
remove(blogsCorpus)
gc()


# Create n-grams -------------------------------------------------------------------------

# Tokenise the corpus
myTokens <- tokens(myCorpus, what = "word")

remove(myCorpus)
remove(corpusList)

# Remove profanities
myTokens <- tokens_remove(myTokens, profanity$V1, valuetype = "fixed", 
                          case_insensitive = TRUE, padding = TRUE, verbose = TRUE)

# Correct spelling of commonly misspelt words
for(i in 1:length(misspellings)) {
        
        # Separate correct spelling from common misspellings
        correct <- sapply(strsplit(misspellings[i], "[:]"), '[[', 1)
        incorrect <- sapply(strsplit(misspellings[i], "[:]"), '[[', 2)
        incorrect <- strsplit(incorrect, "[,]")
        incorrect <- trimws(incorrect, which = "both")
        correct <- rep(correct, length(incorrect[[1]]))
        
        # Replace common misspellings with correct spelling
        tokens_replace(myTokens, incorrect[[1]], replacement = correct, case_insensitive = TRUE, verbose = TRUE)
}


# Extract unigrams, create a document feature matrix, save in a data table
unigrams <- tokens_ngrams(myTokens, n = 1L)
dfmUnigrams <- dfm(unigrams)
unigramsDT <- data.table(ngram = featnames(dfmUnigrams), count = colSums(dfmUnigrams), 
                         stringsAsFactors = FALSE)
tailWords <- unigramsDT$ngram
unigramsDT <- unigramsDT[, tail := tailWords]

saveRDS(unigramsDT, file = "./data/unigramsDT.rds")
remove(unigrams)
remove(dfmUnigrams)
remove(unigramsDT)

gc()

# Extract bigrams, create a document feature matrix, save in a data table and extract tail word
bigrams <- tokens_ngrams(myTokens, n = 2L, concatenator = "_")
dfmBigrams <- dfm(bigrams)
bigramsDT <- data.table(ngram = featnames(dfmBigrams), count = colSums(dfmBigrams),
                        stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(bigramsDT$ngram, "_", fixed = TRUE), '[[', 2)
bigramsDT <- bigramsDT[, tail := tailWords]

# Remove tail word from ngram
# bigramsDT <- bigramsDT[, ngram := sapply(strsplit(bigramsDT$ngram, "_", fixed = TRUE), '[[', 1)]

# Save to file
saveRDS(bigramsDT, file = "./data/bigramsDT.rds")
remove(bigrams)
remove(dfmBigrams)
remove(bigramsDT)

gc()

# Extract trigrams, create a document feature matrix, save in a data table and extract tail word
trigrams <- tokens_ngrams(myTokens, n = 3L, concatenator = "_")
dfmTrigrams <- dfm(trigrams)
trigramsDT <- data.table(ngram = featnames(dfmTrigrams), count = colSums(dfmTrigrams),
                        stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 3)
trigramsDT <- trigramsDT[, tail := tailWords]

# Remove tail word from ngram
# trigramsDT <- trigramsDT[, ngram := paste(sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 2), sep = "_")]

# Save file
saveRDS(trigramsDT, file = "./data/trigramsDT.rds")
remove(trigrams)
remove(dfmTrigrams)
remove(trigramsDT)

gc()

# Extract quadgrams, create a document feature matrix, save in a data table and extract tail word
quadgrams <- tokens_ngrams(myTokens, n = 4L, concatenator = "_")
dfmQuadgrams <- dfm(quadgrams)
quadgramsDT <- data.table(ngram = featnames(dfmQuadgrams), count = colSums(dfmQuadgrams),
                         stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 4)
quadgramsDT <- quadgramsDT[, tail := tailWords]

# Remove tail word from each quadgram
# quadgramsDT <- quadgramsDT[, ngram := paste(sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 3), sep = "_")]

# Save to file
saveRDS(quadgramsDT, file = "./data/quadgramsDT.rds")
remove(quadgrams)
remove(dfmQuadgrams)
remove(quadgramsDT)

gc()

# Extract 5-grams, create a document feature matrix, save in a data table and extract tail word
fivegrams <- tokens_ngrams(myTokens, n = 5L, concatenator = "_")
dfmFivegrams <- dfm(fivegrams)
fivegramsDT <- data.table(ngram = featnames(dfmFivegrams), count = colSums(dfmFivegrams),
                          stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[, tail := tailWords]

# Remove tail word from each fivegram
# fivegramsDT <- fivegramsDT[, ngram := paste(sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 4), sep = "_")]

saveRDS(fivegramsDT, file = "./data/fivegramsDT.rds")
remove(fivegrams)
remove(dfmFivegrams)
remove(fivegramsDT)

gc()

# Extract 6-grams, create a document feature matrix, save in a data table and extract tail word
sixgrams <- tokens_ngrams(myTokens, n = 6L, concatenator = "_")
dfmSixgrams <- dfm(sixgrams)
sixgramsDT <- data.table(ngram = featnames(dfmSixgrams), count = colSums(dfmSixgrams),
                          stringsAsFactors = FALSE)
tailWords <- sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[, tail := tailWords]

# Remove tail word from each sixgram
# sixgramsDT <- sixgramsDT[, ngram := paste(sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 4), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 5), sep = "_")]

saveRDS(sixgramsDT, file = "./data/sixgramsDT.rds")
remove(sixgrams)
remove(dfmSixgrams)
remove(sixgramsDT)

remove(myTokens)
gc()

# Remove low frequency ngrams -----------------------------------------------------------

# Open files
unigramsDT <- readRDS("./data/unigramsDT.rds")
bigramsDT <- readRDS("./data/bigramsDT.rds")
trigramsDT <- readRDS("./data/trigramsDT.rds")
quadgramsDT <- readRDS("./data/quadgramsDT.rds")
fivegramsDT <- readRDS("./data/fivegramsDT.rds")
sixgramsDT <- readRDS("./data/sixgramsDT.rds")

# Remove ngrams with counts of 1 or less
unigramsDT <- unigramsDT[count > 1, ]
bigramsDT <- bigramsDT[count > 1, ]
trigramsDT <- trigramsDT[count > 1, ]
quadgramsDT <- quadgramsDT[count > 1, ]
fivegramsDT <- fivegramsDT[count > 1, ]
sixgramsDT <- sixgramsDT[count > 1, ]


# Combine into a single data table ------------------------------------------------------

# Combine ngrams
ngramsDT <- rbind(unigramsDT, bigramsDT, trigramsDT, quadgramsDT, fivegramsDT, sixgramsDT)

# Label ngrams
ngramsDT <- ngramsDT[, n := c(rep(1, nrow(unigramsDT)), rep(2, nrow(bigramsDT)),
                              rep(3, nrow(trigramsDT)), rep(4, nrow(quadgramsDT)),
                              rep(5, nrow(fivegramsDT)), rep(6, nrow(sixgramsDT)))]

remove(unigramsDT)
remove(bigramsDT)
remove(trigramsDT)
remove(quadgramsDT)
remove(fivegramsDT)
remove(sixgramsDT)

gc()

# Index on ngram column
setkey(ngramsDT, ngram)


############################ Remove ngrams with words not in dictionary ???


# Stupid Backoff Prediction Algorithm --------------------------------------------------

# Extract last 5 words from input text
input <- "she coped very well with"
inputTokens <- tokens(input, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
              remove_symbols = FALSE, remove_separators = TRUE, remove_twitter = TRUE)
inputText <- inputTokens$text1[(length(inputTokens$text1)-4):length(inputTokens$text1)]
#inputString <- paste(inputText[1], inputText[2], inputText[3], inputText[4], inputText[5], sep = "_")

# Prediction function
myPrediction <- function(tkns, ngrams) {
        
        # Create denominator search terms
        unigram <- tkns[5]
        bigram <- paste(tkns[4], tkns[5], sep = "_")
        trigram <- paste(tkns[3], tkns[4], tkns[5], sep = "_")
        quadgram <- paste(tkns[2], tkns[3], tkns[4], tkns[5], sep = "_") 
        fivegram <- paste(tkns[1], tkns[2], tkns[3], tkns[4], tkns[5], sep = "_")
        
        # Create numerator search terms
        unigramPrefix <- paste(unigram, "_", sep = "")
        bigramPrefix <- paste(bigram, "_", sep = "")
        trigramPrefix <- paste(trigram, "_", sep = "")
        quadgramPrefix <- paste(quadgram, "_", sep = "")
        fivegramPrefix <- paste(fivegram, "_", sep = "")
        
        # Find sixgrams
        sixgrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        sixgrams <- ngrams[n == 6][ngram %like% fivegramPrefix]
        
        # Get the fivegram count
        denom <- ngrams[ngram == fivegram]
     
        # Calculate probabilities
        if(nrow(denom) > 0) {
                sixgrams <- sixgrams[, prob := sixgrams$count / denom$count]
        }
        
        # Find unobserved sixgrams
        # First get the tail words of unobserved sixgrams
        observedSixgramTails <- sixgrams[, tail]
        unobservedSixgramTails <- ngrams[n == 1][!(ngram %in% observedSixgramTails)]
        
        # Back off to fivegrams
        fivegrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        fivegrams <- ngrams[n == 5][ngram %like% quadgramPrefix]
        
        # Exclude fivegrams which end in the tail word of sixgrams found
        fivegrams <- fivegrams[!(tail %in% observedSixgramTails)]
        
        # Note observed fivegram tail words
        observedFivegramTails <- fivegrams[, tail]
        
        # Get the quadgram count
        denom <- ngrams[ngram == quadgram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                fivegrams <- fivegrams[, prob := (fivegrams$count / denom$count) * .4]
        }
        
        # Back off to quadgrams
        quadgrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        quadgrams <- ngrams[n == 4][ngram %like% trigramPrefix]
        
        # Exclude quadgrams with tail words already observed
        quadgrams <- quadgrams[!(tail %in% observedSixgramTails)][!(tail %in% observedFivegramTails)]
        
        # Note the observed quadgram tail words
        observedQuadgramTails <- quadgrams[, tail]
        
        # Get the trigram count
        denom <- ngrams[ngram == trigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                quadgrams <- quadgrams[, prob := (quadgrams$count / denom$count) * .4]
        }
        
        # Back off to trigrams
        trigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        trigrams <- ngrams[ngram %like% bigramPrefix]
        
        # Exclude trigrams with tail words already observed
        trigrams <- trigrams[!(tail %in% observedSixgramTails)][!(tail %in% observedFivegramTails)][!(tail %in% observedQuadgramTails)]
        
        # Note the observed trigram tail words
        observedTrigramTails <- trigrams[, tail]
        
        # Get the bigram count
        denom <- ngrams[ngram == bigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                trigrams <- trigrams[, prob := (trigrams$count / denom$count) * .4]
        }
        
        # Back off to bigrams
        bigrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        bigrams <- ngrams[ngram %like% unigramPrefix]
        
        # Exclude bigrams with tail words already observed
        bigrams <- bigrams[!(tail %in% observedSixgramTails)][!(tail %in% observedFivegramTails)][!(tail %in% observedQuadgramTails)][!(tail %in% observedTrigramTails)]
        
        # Note the observed bigram tail words
        observedBigramTails <- bigrams[, tail]
        
        # Get the unigram count
        denom <- ngrams[ngram == unigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                bigrams <- bigrams[, prob := (bigrams$count / denom$count) * .4]
        }
        
        # Back off to unigrams
        unigrams <- data.table(ngram = vector(mode = "character", length = 0),
                              count = vector(mode = "integer", length = 0),
                              tail = vector(mode = "character", length = 0))
        
        # Get unigrams which were not observed as tail words for any observed ngram
        unigrams <- ngrams[n == 1][!(tail %in% observedSixgramTails)][!(tail %in% observedFivegramTails)][!(tail %in% observedQuadgramTails)][!(tail %in% observedTrigramTails)][!(tail %in% observedBigramTails)]
        
        # Get the total unigram count
        denom <- ngrams[n == 1][, sum(count)]
        
        # Calculate MLEs
        if(denom > 0) {
                unigrams <- unigrams[, prob := (unigrams$count / denom) * .4]
        }
        
        # Put all the MLEs into a single table
        MLEs <- rbind(sixgrams, fivegrams, quadgrams, trigrams, bigrams, unigrams, fill = TRUE)
        MLEs <- MLEs[order(-MLEs$prob), ]
        
        return(MLEs)
}

