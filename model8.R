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

remove(profanity)


# Get a list of commonly misspelt words ------------------------------------------------

#con <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/misspellings.txt"
#misspellings <- readLines(con)
#close(con)


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
                if (rbinom(1, 1, prob = 1) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
sampleFile(t, "twitter.txt", header = FALSE)

twitter <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt")

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
sampleFile(n, "news.txt", header = FALSE)

news <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt")

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

blogs <- read_file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogs.txt")


# Clean text -----------------------------------------------------------------------------

# Remove non-ASCII characters
twitter <- stringi::stri_trans_general(twitter, "latin-ascii")
news <- stringi::stri_trans_general(news, "latin-ascii")
blogs <- stringi::stri_trans_general(blogs, "latin-ascii")


# Create corpus --------------------------------------------------------------------------

# Build 3 corpii and reshape to sentences
twitterCorpus <- quanteda::corpus(twitter)
twitterCorpus <- corpus_reshape(twitterCorpus, to = "sentences")
remove(twitter)
gc()

newsCorpus <- quanteda::corpus(news)
newsCorpus <- corpus_reshape(newsCorpus, to = "sentences")
remove(news)
gc()

blogsCorpus <- quanteda::corpus(blogs)
blogsCorpus <- corpus_reshape(blogsCorpus, to = "sentences")
remove(blogs)
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
myTokens <- tokens(myCorpus, what = "word", remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE, 
                   remove_url = TRUE)

remove(myCorpus)
remove(corpusList)

# Remove profanities
profanity <- readRDS("profanity.rds")
myTokens <- tokens_remove(myTokens, profanity$V1, valuetype = "fixed", 
                          case_insensitive = TRUE, padding = TRUE, verbose = TRUE)
remove(profanity)

# Save tokens
saveRDS(myTokens, file = "./data/myTokens.rds")

# Correct spelling of commonly misspelt words
#for(i in 1:length(misspellings)) {
        
        # Separate correct spelling from common misspellings
#        correct <- sapply(strsplit(misspellings[i], "[:]"), '[[', 1)
#        incorrect <- sapply(strsplit(misspellings[i], "[:]"), '[[', 2)
#        incorrect <- strsplit(incorrect, "[,]")
#        incorrect <- trimws(incorrect, which = "both")
#        correct <- rep(correct, length(incorrect[[1]]))
        
        # Replace common misspellings with correct spelling
#        tokens_replace(myTokens, incorrect[[1]], replacement = correct, case_insensitive = TRUE, verbose = TRUE)
#}


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
remove(tailWords)

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
remove(tailWords)

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
remove(tailWords)

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
remove(tailWords)

gc()

# Extract 5-grams, create a document feature matrix, save in a data table and extract tail word
# First divide tokens to process
myTokens1 <- myTokens[1:2409649]
myTokens2 <- myTokens[2409650:4835363]
remove(myTokens)

fivegrams1 <- tokens_ngrams(myTokens1, n = 5L, concatenator = "_")
dfmFivegrams1 <- dfm(fivegrams1)
remove(fivegrams1)
fivegramsDT1 <- data.table(ngram = featnames(dfmFivegrams1), count = colSums(dfmFivegrams1),
                           stringsAsFactors = FALSE)
remove(dfmFivegrams1)

gc() 

fivegrams2 <- tokens_ngrams(myTokens2, n = 5L, concatenator = "_")
dfmFivegrams2 <- dfm(fivegrams2)
remove(fivegrams2)
fivegramsDT2 <- data.table(ngram = featnames(dfmFivegrams2), count = colSums(dfmFivegrams2),
                           stringsAsFactors = FALSE)
remove(dfmFivegrams2)

gc()

# Merge data tables and combine counts
fivegramsDT <- merge(fivegramsDT1, fivegramsDT2, by = "ngram", all = TRUE)
remove(fivegramsDT1)
remove(fivegramsDT2)
fivegramsDT[is.na(count.x)]$count.x <- 0
fivegramsDT[is.na(count.y)]$count.y <- 0
fivegramsDT <- fivegramsDT[, count := count.x + count.y]
fivegramsDT <- fivegramsDT[, count.x := NULL]
fivegramsDT <- fivegramsDT[, count.y := NULL]

tailWords <- sapply(strsplit(fivegramsDT$ngram[1:10000000], "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[1:10000000, tail := tailWords]
tailWords <- sapply(strsplit(fivegramsDT$ngram[10000001:20000000], "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[10000001:20000000, tail := tailWords]
tailWords <- sapply(strsplit(fivegramsDT$ngram[20000001:30000000], "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[20000001:30000000, tail := tailWords]
tailWords <- sapply(strsplit(fivegramsDT$ngram[30000001:46871267], "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[30000001:46871267, tail := tailWords]

# Remove tail word from each fivegram
# fivegramsDT <- fivegramsDT[, ngram := paste(sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 4), sep = "_")]

saveRDS(fivegramsDT, file = "./data/fivegramsDT.rds")
remove(fivegramsDT)

gc()

# Extract 6-grams, create a document feature matrix, save in a data table and extract tail word
sixgrams1 <- tokens_ngrams(myTokens1, n = 6L, concatenator = "_")
dfmSixgrams1 <- dfm(sixgrams1)
remove(sixgrams1)
sixgramsDT1 <- data.table(ngram = featnames(dfmSixgrams1), count = colSums(dfmSixgrams1),
                          stringsAsFactors = FALSE)
remove(dfmSixgrams1)

saveRDS(sixgramsDT1, file = "./data/sixgramsDT1.rds")
remove(sixgramsDT1)

gc()

sixgrams2 <- tokens_ngrams(myTokens2, n = 6L, concatenator = "_")
dfmSixgrams2 <- dfm(sixgrams2)
remove(sixgrams2)
sixgramsDT2 <- data.table(ngram = featnames(dfmSixgrams2), count = colSums(dfmSixgrams2),
                          stringsAsFactors = FALSE)
remove(dfmSixgrams2)

gc()

# Merge data tables and combine counts
sixgramsDT1 <- readRDS("./data/sixgramsDT1.rds")
sixgramsDT <- merge(sixgramsDT1, sixgramsDT2, by = "ngram", all = TRUE)
remove(sixgramsDT1)
remove(sixgramsDT2)
sixgramsDT[is.na(count.x)]$count.x <- 0
sixgramsDT[is.na(count.y)]$count.y <- 0
sixgramsDT <- sixgramsDT[, count := count.x + count.y]
sixgramsDT <- sixgramsDT[, count.x := NULL]
sixgramsDT <- sixgramsDT[, count.y := NULL]

tailWords <- sapply(strsplit(sixgramsDT$ngram[1:10000000], "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[1:10000000, tail := tailWords]
tailWords <- sapply(strsplit(sixgramsDT$ngram[10000001:20000000], "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[10000001:20000000, tail := tailWords]
tailWords <- sapply(strsplit(sixgramsDT$ngram[20000001:30000000], "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[20000001:30000000, tail := tailWords]
tailWords <- sapply(strsplit(sixgramsDT$ngram[30000001:45227515], "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[30000001:45227515, tail := tailWords]

remove(tailWords)

# Remove tail word from each sixgram
# sixgramsDT <- sixgramsDT[, ngram := paste(sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 4), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 5), sep = "_")]

saveRDS(sixgramsDT, file = "./data/sixgramsDT.rds")
remove(sixgramsDT)

remove(myTokens1)
remove(myTokens2)
gc()


# Remove low frequency ngrams -----------------------------------------------------------

# Open files and remove ngrams with counts of 1 or less
unigramsDT <- readRDS("./data/unigramsDT.rds")
unigramsDT <- unigramsDT[count > 1, ]

bigramsDT <- readRDS("./data/bigramsDT.rds")
bigramsDT <- bigramsDT[count > 1, ]

trigramsDT <- readRDS("./data/trigramsDT.rds")
trigramsDT <- trigramsDT[count > 1, ]

quadgramsDT <- readRDS("./data/quadgramsDT.rds")
quadgramsDT <- quadgramsDT[count > 1, ]

fivegramsDT <- readRDS("./data/fivegramsDT.rds")
fivegramsDT <- fivegramsDT[count > 1, ]

sixgramsDT <- readRDS("./data/sixgramsDT.rds")
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

# Save
saveRDS(ngramsDT, file = "./data/ngramsDT.rds")


############################ Remove ngrams with words not in dictionary ???


# Stupid Backoff Prediction Algorithm --------------------------------------------------

# Extract last 5 words from input text
input <- "Offense still struggling but the"
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

