# Model Build 8
# The strategy is to load each source (twitter, news, blogs) separately and then combine.

library(data.table)
library(quanteda)
library(textcat)
library(readr)
library(stringi)
library(stringr)


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
                hdr <- read_lines(ci, n_max = 1)
                writeLines(hdr, co)
        }
        recnum = 0
        numout = 0
        while (TRUE) {
                inrec <- read_lines(ci)
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
#t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
#sampleFile(t, "twitter.txt", header = FALSE)
unclean_tweet <- read_lines("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt")

# Extract news sample
#n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
#sampleFile(n, "news.txt", header = FALSE)
news <- read_lines("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt")

# Extract blogs sample
#b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
#sampleFile(b, "blogs.txt", header = FALSE)
unclean_blog <- read_lines("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt")


# Clean twitter text --------------------------------------------------------------------

# Create regex to remove URLs, twitter user names, hashtags, possessives and unicode / html tags
stuff_to_remove <- c("http[s]?://[[:alnum:].\\/]+", "@[\\w]*", "#[\\w]*", "<.*>", "'s")
stuff_to_remove <-  paste(stuff_to_remove, sep = "|", collapse="|")

# Clean tweets
clean_tweet <- str_replace_all(unclean_tweet, stuff_to_remove, "")
remove(unclean_tweet)

# Remove leading and trailing spaces
clean_tweet <- str_trim(clean_tweet)

# Clean blogs
clean_blog <- str_replace_all(unclean_blog, "[^[:graph:]]", "")
remove(unclean_blog)


# Create corpus --------------------------------------------------------------------------

# Build 3 corpii and reshape to sentences
twitterCorpus <- quanteda::corpus(clean_tweet)
twitterCorpus <- corpus_reshape(twitterCorpus, to = "sentences")
remove(clean_tweet)
gc()

newsCorpus <- quanteda::corpus(news)
newsCorpus <- corpus_reshape(newsCorpus, to = "sentences")
remove(news)
gc()

blogsCorpus <- quanteda::corpus(clean_blog)
blogsCorpus <- corpus_reshape(blogsCorpus, to = "sentences")
remove(clean_blog)
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

# Save to file
saveRDS(quadgramsDT, file = "./data/quadgramsDT.rds")
remove(quadgrams)
remove(dfmQuadgrams)
remove(quadgramsDT)

gc()

# Extract 5-grams, create a document feature matrix, save in a data table and extract tail word
# First divide tokens to process
h <- round(length(myTokens) / 2, 0)
myTokens1 <- myTokens[1:h]
myTokens2 <- myTokens[(h+1):length(myTokens)]
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

saveRDS(sixgramsDT, file = "./data/sixgramsDT.rds")
remove(sixgramsDT)

remove(myTokens1)
remove(myTokens2)
gc()


# Remove low frequency ngrams -----------------------------------------------------------

# Open files and remove ngrams with counts of 1 or less
unigramsDT <- readRDS("./data/unigramsDT.rds")
unigramsDT <- unigramsDT[count > 1, ]
tailWords <- unigramsDT$ngram
unigramsDT <- unigramsDT[, tail := tailWords]

bigramsDT <- readRDS("./data/bigramsDT.rds")
bigramsDT <- bigramsDT[count > 1, ]
tailWords <- sapply(strsplit(bigramsDT$ngram, "_", fixed = TRUE), '[[', 2)
bigramsDT <- bigramsDT[, tail := tailWords]
bigramsDT <- bigramsDT[, ngram := sapply(strsplit(bigramsDT$ngram, "_", fixed = TRUE), '[[', 1)]

trigramsDT <- readRDS("./data/trigramsDT.rds")
trigramsDT <- trigramsDT[count > 1, ]
tailWords <- sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 3)
trigramsDT <- trigramsDT[, tail := tailWords]
trigramsDT <- trigramsDT[, ngram := paste(sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(trigramsDT$ngram, "_", fixed = TRUE), '[[', 2), sep = "_")]

quadgramsDT <- readRDS("./data/quadgramsDT.rds")
quadgramsDT <- quadgramsDT[count > 1, ]
tailWords <- sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 4)
quadgramsDT <- quadgramsDT[, tail := tailWords]
quadgramsDT <- quadgramsDT[, ngram := paste(sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(quadgramsDT$ngram, "_", fixed = TRUE), '[[', 3), sep = "_")]

fivegramsDT <- readRDS("./data/fivegramsDT.rds")
fivegramsDT <- fivegramsDT[count > 1, ]
tailWords <- sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 5)
fivegramsDT <- fivegramsDT[, tail := tailWords]
fivegramsDT <- fivegramsDT[, ngram := paste(sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(fivegramsDT$ngram, "_", fixed = TRUE), '[[', 4), sep = "_")]

sixgramsDT <- readRDS("./data/sixgramsDT.rds")
sixgramsDT <- sixgramsDT[count > 1, ]
tailWords <- sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 6)
sixgramsDT <- sixgramsDT[, tail := tailWords]
sixgramsDT <- sixgramsDT[, ngram := paste(sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 1), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 2), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 3), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 4), sapply(strsplit(sixgramsDT$ngram, "_", fixed = TRUE), '[[', 5), sep = "_")]

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
remove(tailWords)

gc()

# Save
saveRDS(ngramsDT, file = "./data/ngramsDT.rds")

############################ Remove ngrams with words not in dictionary ???


# Setup data ---------------------------------------------------------------------------

ngramsDT <- readRDS("./data/ngramsDT.rds")
setkey(ngramsDT, ngram)


# Stupid Backoff Prediction Algorithm --------------------------------------------------

# Prediction function
myPrediction <- function(input, ngrams) {
        
        # Extract last 5 words from input text
        input <- tokens(input, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = FALSE, remove_separators = TRUE, remove_twitter = TRUE)
        tkns <- input$text1[(length(input$text1)-4):length(input$text1)]
        tkns <- tolower(tkns)
        
        # Create ngram search terms
        unigram <- tkns[5]
        bigram <- paste(tkns[4], tkns[5], sep = "_")
        trigram <- paste(tkns[3], tkns[4], tkns[5], sep = "_")
        quadgram <- paste(tkns[2], tkns[3], tkns[4], tkns[5], sep = "_") 
        fivegram <- paste(tkns[1], tkns[2], tkns[3], tkns[4], tkns[5], sep = "_")
        
        # Create denominator search terms
        fivegramPre <- paste(tkns[1], tkns[2], tkns[3], tkns[4], sep = "_")
        quadgramPre <- paste(tkns[2], tkns[3], tkns[4], sep = "_") 
        trigramPre <- paste(tkns[3], tkns[4], sep = "_")
        bigramPre <- paste(tkns[4], sep = "_")
        
        # Find sixgrams
        sixgrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        sixgrams <- ngrams[ngram == fivegram]
        
        # Get the fivegram count
        denom <- ngrams[ngram == fivegramPre][tail == unigram]
     
        # Calculate probabilities
        if(nrow(denom) > 0) {
                sixgrams <- sixgrams[, prob := sixgrams$count / denom$count]
        }
        
        # Find unobserved sixgrams
        # First get the tail words of unobserved sixgrams
        if(nrow(sixgrams) > 0) {
                observedSixgramTails <- sixgrams[, tail]
        } else {
                observedSixgramTails <- as.character()
        }
        
        unobservedSixgramTails <- ngrams[n == 1][!(tail %in% observedSixgramTails)]$tail
        
        # Back off to fivegrams
        fivegrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        fivegrams <- ngrams[ngram == quadgram][!(tail %in% observedSixgramTails)]
        
        # Note observed fivegram tail words
        if(nrow(fivegrams) > 0) {
                observedFivegramTails <- fivegrams[, tail]
                # Update observedSixgramTails
                observedSixgramTails <- c(observedSixgramTails, observedFivegramTails)
        }
        
        # Get the quadgram count
        denom <- ngrams[ngram == quadgramPre][tail == unigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                fivegrams <- fivegrams[, prob := (fivegrams$count / denom$count) * .4]
        }
        
        # Back off to quadgrams
        quadgrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        quadgrams <- ngrams[ngram == trigram][!(tail %in% observedSixgramTails)]
        
        # Note the observed quadgram tail words
        if(nrow(quadgrams) > 0) {
                observedQuadgramTails <- quadgrams[, tail]
                # Update observedSixgramTails
                observedSixgramTails <- c(observedSixgramTails, observedQuadgramTails)
        }
        
        # Get the trigram count
        denom <- ngrams[ngram == trigramPre][tail == unigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                quadgrams <- quadgrams[, prob := (quadgrams$count / denom$count) * .4]
        }
        
        # Back off to trigrams
        trigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        trigrams <- ngrams[ngram == bigram][!(tail %in% observedSixgramTails)]
        
        # Note the observed trigram tail words
        if(nrow(trigrams) > 0) {
                observedTrigramTails <- trigrams[, tail]
                # Update observedSixgramTails
                observedSixgramTails <- c(observedSixgramTails, observedTrigramTails)
        }
        
        # Get the bigram count
        denom <- ngrams[ngram == bigramPre][tail == unigram]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                trigrams <- trigrams[, prob := (trigrams$count / denom$count) * .4]
        }
        
        # Back off to bigrams
        bigrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        bigrams <- ngrams[ngram == unigram][!(tail %in% observedSixgramTails)]
        
        # Note the observed bigram tail words
        if(nrow(bigrams) > 0) {
                observedBigramTails <- bigrams[, tail]
                # Update observedSixgramTails
                observedSixgramTails <- c(observedSixgramTails, observedBigramTails)
        }
        
        # Get the unigram count
        denom <- ngrams[ngram == unigram][n == 1]
        
        # Calculate probabilities
        if(nrow(denom) > 0) {
                bigrams <- bigrams[, prob := (bigrams$count / denom$count) * .4]
        }
        
        # Back off to unigrams
        unigrams <- data.table(ngram = vector(mode = "character", length = 0),
                              count = vector(mode = "integer", length = 0),
                              tail = vector(mode = "character", length = 0))
        
        # Get unigrams which were not observed as tail words for any observed ngram
        unigrams <- ngrams[n == 1][!(tail %in% observedSixgramTails)]
        
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

