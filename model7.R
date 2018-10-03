# Model Build 7
# The strategy is to load each source (twitter, news, blogs) separately and then combine.

library(dplyr)
library(tidytext)
library(data.table)
library(quanteda)
library(sqldf)
library(hunspell)

# Function to create tokens from a string -----------------------------------------
makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", remove_numbers = TRUE,
               remove_punct = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_hyphens = TRUE,
               remove_url = TRUE,
               ngrams = n)
}

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

# Load TWITTER data ----------------------------------------------------------------------------

### LOOK AT http://rpubs.com/BParisi83/DSCapstoneW2

# Load 100% of files
t <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
twitter <- readLines(t, encoding='UTF-8', skipNul = TRUE)
close(t)

# Clean TWITTER text ---------------------------------------------------------------------------

# Clean text of special characters and numbers
twitter <- gsub("[[:space:]]*[.?!:;]+[[:space:]]*", '.', twitter) # Replace all terminal puntuation with a fullstop
twitter <- gsub("[[:space:]]+", " ", twitter) # collapse white space
twitter <- gsub(" ?\\. +?", ".", twitter) # make sure terminals are tight
twitter <- gsub("\\.+", " EOS ", twitter) # Replace fullstops with end-of-sentence marker
#twitter <- gsub("\\!+", " EOS ", twitter) # Replace exclamation marks with end-of-sentence marker
#twitter <- gsub("\\?+", " EOS ", twitter) # Replace question marks with end-of-sentence marker
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
twitter <- gsub(" ?' ?", "'", twitter) # make sure contractions are tight

# Remove words not in dictionary ???

# Store TWITTER data ---------------------------------------------------------------------------

# Store in a data table
twitterTable <- data.table(doc = rep("Twitter", length(twitter)), text = twitter)

# Select a random sample of 70% for training and the remaining for testing and validation
sampleSize <- floor(.7 * nrow(twitterTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(twitterTable)), size = sampleSize) 

twitterTraining <- twitterTable[trainIndex, ]
twitterTesting <- twitterTable[-trainIndex, ]

# Save to files
saveRDS(twitterTraining, file = "twitterTraining.rds")
saveRDS(twitterTesting, file = "twitterTesting.rds")

remove(t)
remove(twitter)
remove(twitterTable)
remove(trainIndex)

# Extract TWITTER ngrams ---------------------------------------------------------------------------

# Extract unigrams
twitterUnigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1)

saveRDS(twitterUnigrams, file = "twitterUnigrams.rds")        
remove(twitterUnigrams)
gc()

# Extract bigrams
twitterBigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, to_lower = TRUE)

saveRDS(twitterBigrams, file = "twitterBigrams.rds")        
remove(twitterBigrams)
gc()

# Extract trigrams
twitterTrigrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 3, to_lower = TRUE)

saveRDS(twitterTrigrams, file = "twitterTrigrams.rds")
remove(twitterTrigrams)
gc()

# Extract quadgrams
twitterQuadgrams <- tibble(text = twitterTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 4, to_lower = TRUE)

# Split table due to size
n1 <- round(nrow(twitterQuadgrams) / 3, 0)
n2 <- n1 * 2
twitterQuadgrams1 <- twitterQuadgrams[1:n1, ]
twitterQuadgrams2 <- twitterQuadgrams[(n1+1):n2, ]
twitterQuadgrams3 <- twitterQuadgrams[(n2+1):nrow(twitterQuadgrams), ]

saveRDS(twitterQuadgrams, file = "twitterQuadgrams.rds")
saveRDS(twitterQuadgrams1, file = "twitterQuadgrams1.rds")
saveRDS(twitterQuadgrams2, file = "twitterQuadgrams2.rds")
saveRDS(twitterQuadgrams3, file = "twitterQuadgrams3.rds")

remove(twitterQuadgrams)
remove(twitterQuadgrams1)
remove(twitterQuadgrams2)
remove(twitterQuadgrams3)

gc()

remove(twitterTraining)
remove(twitterTesting)

# Load NEWS data ----------------------------------------------------------------------------

# Load 100% of files
n <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(n, encoding='UTF-8', skipNul = TRUE)
close(n)

# Clean NEWS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
news <- gsub("[[:space:]]*[.?!:;]+[[:space:]]*", '.', news) # Replace all terminal puntuation with a fullstop
news <- gsub("[[:space:]]+", " ", news) # collapse white space
news <- gsub(" ?\\. +?", ".", news) # make sure terminals are tight
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
news <- gsub(" ?' ?", "'", news) # make sure contractions are tight

# Remove words not in dictionary ???

# Store NEWS data ---------------------------------------------------------------------------

# Store in a data table
newsTable <- data.table(doc = rep("News", length(news)), text = news)

# Select a random sample of 70% for training and the remaining for testing and validation
sampleSize <- floor(.7 * nrow(newsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(newsTable)), size = sampleSize) 

newsTraining <- newsTable[trainIndex, ]
newsTesting <- newsTable[-trainIndex, ]

# Save to files
saveRDS(newsTraining, file = "newsTraining.rds")
saveRDS(newsTesting, file = "newsTesting.rds")

remove(n)
remove(news)
remove(newsTable)
remove(trainIndex)

# Extract NEWS ngrams ---------------------------------------------------------------------------

# Extract unigrams
newsUnigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1) 

saveRDS(newsUnigrams, file = "newsUnigrams.rds")
remove(newsUnigrams)
gc()

# Extract bigrams
newsBigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, to_lower = TRUE)

saveRDS(newsBigrams, file = "newsBigrams.rds")
remove(newsBigrams)
gc()

# Extract trigrams
newsTrigrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 3, to_lower = TRUE)

saveRDS(newsTrigrams, file = "newsTrigrams.rds")
remove(newsTrigrams)
gc()

# Extract quadgrams
newsQuadgrams <- tibble(text = newsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 4, to_lower = TRUE)

saveRDS(newsQuadgrams, file = "newsQuadgrams.rds")
remove(newsQuadgrams)

remove(newsTraining)
remove(newsTesting)

gc()

# Load BLOGS data ----------------------------------------------------------------------------

# Load 100% of files
b <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(b, encoding='UTF-8', skipNul = TRUE)
close(b)

# Clean BLOGS text ---------------------------------------------------------------------------

# Clean training text of special characters and numbers

# Clean text of special characters and numbers
blogs <- gsub("[[:space:]]*[.?!:;]+[[:space:]]*", '.', blogs) # Replace all terminal puntuation with a fullstop
blogs <- gsub("[[:space:]]+", " ", blogs) # collapse white space
blogs <- gsub(" ?\\. +?", ".", blogs) # make sure terminals are tight
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
blogs <- gsub(" ?' ?", "'", blogs) # make sure contractions are tight

# Remove words not in dictionary ???


# Store BLOGS data ---------------------------------------------------------------------------

# Store in a data table
blogsTable <- data.table(doc = rep("Blogs", length(blogs)), text = blogs)

# Select a random sample of 70% for training and the remaining for testing and validation
sampleSize <- floor(.7 * nrow(blogsTable))

set.seed(12345)
trainIndex <- sample(seq_len(nrow(blogsTable)), size = sampleSize) 

blogsTraining <- blogsTable[trainIndex, ]
blogsTesting <- blogsTable[-trainIndex, ]

# Save to files
saveRDS(blogsTraining, file = "blogsTraining.rds")
saveRDS(blogsTesting, file = "blogsTesting.rds")

remove(b)
remove(blogs)
remove(blogsTable)
remove(trainIndex)

# Extract BLOGS ngrams ---------------------------------------------------------------------------

# Extract unigrams
blogsUnigrams1 <- tibble(text = blogsTraining$text[1:(round(nrow(blogsTraining) / 2, 0))]) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1)

blogsUnigrams2 <- tibble(text = blogsTraining$text[(round(nrow(blogsTraining) / 2, 0) + 1):(nrow(blogsTraining))]) %>%
        unnest_tokens(word, text, to_lower = TRUE) %>%
        filter(!word %in% profanity$V1)

blogsUnigrams <- rbindlist(list(blogsUnigrams1, blogsUnigrams2), 
                           use.names = TRUE, fill = FALSE, idcol = NULL)

remove(blogsUnigrams1)
remove(blogsUnigrams2)

saveRDS(blogsUnigrams, file = "blogsUnigrams.rds")
remove(blogsUnigrams)
gc()

# Extract bigrams
blogsBigrams <- tibble(text = blogsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, to_lower = TRUE)

saveRDS(blogsBigrams, file = "blogsBigrams.rds")
remove(blogsBigrams)
gc()

# Extract trigrams
blogsTrigrams <- tibble(text = blogsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 3, to_lower = TRUE) 

saveRDS(blogsTrigrams, file = "blogsTrigrams.rds")
remove(blogsTrigrams)
gc()

# Extract quadgrams
blogsQuadgrams <- tibble(text = blogsTraining$text) %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 4, to_lower = TRUE) 

# Divide up the data frame
n1 <- round(nrow(blogsQuadgrams) / 5, 0)
n2 <- round(n1 * 2, 0)
n3 <- round(n1 * 3, 0)
n4 <- round(n1 * 4, 0)
blogsQuadgrams1 <- blogsQuadgrams[1:n1, ]
blogsQuadgrams2 <- blogsQuadgrams[(n1+1):n2, ]
blogsQuadgrams3 <- blogsQuadgrams[(n2+1):n3, ]
blogsQuadgrams4 <- blogsQuadgrams[(n3+1):n4, ]
blogsQuadgrams5 <- blogsQuadgrams[(n4+1):nrow(blogsQuadgrams), ]

saveRDS(blogsQuadgrams1, file = "blogsQuadgrams1.rds")
saveRDS(blogsQuadgrams2, file = "blogsQuadgrams2.rds")
saveRDS(blogsQuadgrams3, file = "blogsQuadgrams3.rds")
saveRDS(blogsQuadgrams4, file = "blogsQuadgrams4.rds")
saveRDS(blogsQuadgrams5, file = "blogsQuadgrams5.rds")
saveRDS(blogsQuadgrams, file = "blogsQuadgrams.rds")

remove(blogsQuadgrams1)
remove(blogsQuadgrams2)
remove(blogsQuadgrams3)
remove(blogsQuadgrams4)
remove(blogsQuadgrams5)
remove(blogsQuadgrams)

remove(blogsTraining)
remove(blogsTesting)

gc()

# Combine UNIGRAMS ----------------------------------------------------------------

# Open files
twitterUnigrams <- readRDS("twitterUnigrams.rds") 
newsUnigrams <- readRDS("newsUnigrams.rds")
blogsUnigrams <- readRDS("blogsUnigrams.rds")

# Combine unigrams and count
unigrams <- rbindlist(list(twitterUnigrams, newsUnigrams), 
                      use.names = TRUE, fill = FALSE, idcol = NULL)

unigrams <- rbindlist(list(unigrams, blogsUnigrams), 
                      use.names = TRUE, fill = FALSE, idcol = NULL)

unigrams <- count(unigrams, word, sort = TRUE)

# Also SQLDF
# sqldf("SELECT word, COUNT(word) FROM unigrams GROUP BY word ORDER BY COUNT(word) DESC")
 
# Delete singletons
unigrams <- unigrams[!(unigrams$n == 1), ]

# Remove words not in the dictionary
unigrams <- unigrams[hunspell_check(unigrams$ngram) == TRUE, ]

remove(twitterUnigrams)
remove(newsUnigrams)
remove(blogsUnigrams)

gc()

# Convert to a data table
unigrams <- data.table(ngram = unigrams$word, count = unigrams$n)

# Save unigrams to a file
saveRDS(unigrams, file = "unigrams.rds")

# Restore unigrams from file
unigrams <- readRDS(file = "unigrams.rds")

# Combine BIGRAMS ------------------------------------------------------------------

# Open files
twitterBigrams <- readRDS("twitterBigrams.rds")
newsBigrams <- readRDS("newsBigrams.rds")
blogsBigrams <- readRDS("blogsBigrams.rds")

# Combine bigrams and count
bigrams <- rbindlist(list(twitterBigrams, newsBigrams), 
                      use.names = TRUE, fill = FALSE, idcol = NULL)

bigrams <- rbindlist(list(bigrams, blogsBigrams), 
                      use.names = TRUE, fill = FALSE, idcol = NULL)

# Remove bigrams containing a word not in the dictionary
# bigrams <- bigrams[hunspell(bigrams$ngram)]

bigrams <- count(bigrams, ngram, sort = TRUE)

# Delete singletons
bigrams <- bigrams[!(bigrams$n == 1), ]

remove(twitterBigrams)
remove(newsBigrams)
remove(blogsBigrams)

gc()

# Convert to a data table
bigrams <- data.table(ngram = bigrams$ngram, count = bigrams$n)

# Separate out the tail (last) word
# bigrams <- mutate(bigrams, word1 = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 1))
bigrams <- mutate(bigrams, tail = sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 2))

# Save bigrams to a file
saveRDS(bigrams, file = "bigrams.rds")

# Restore bigrams from file
bigrams <- readRDS(file = "bigrams.rds")

# Combine TRIGRAMS ----------------------------------------------------------------

# Open files
twitterTrigrams <- readRDS("twitterTrigrams.rds")
newsTrigrams <- readRDS("newsTrigrams.rds")
blogsTrigrams <- readRDS("blogsTrigrams.rds")

# Combine trigrams and count
trigrams <- rbindlist(list(twitterTrigrams, newsTrigrams), 
                     use.names = TRUE, fill = FALSE, idcol = NULL)

trigrams <- rbindlist(list(trigrams, blogsTrigrams), 
                     use.names = TRUE, fill = FALSE, idcol = NULL)

# Count trigrams
trigrams <- count(trigrams, ngram, sort = TRUE)

# Delete singletons
trigrams <- trigrams[!(trigrams$n == 1), ]

# Remove trigrams with words not in the dictionary
trigrams <- trigrams[hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 1)) == TRUE &
                             hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 2)) == TRUE &
                             hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 3)) == TRUE, ]

remove(twitterTrigrams)
remove(newsTrigrams)
remove(blogsTrigrams)

gc()

# Separate tail words
trigrams <- mutate(trigrams, tail = sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 3))

# Convert to a data table
trigrams <- data.table(ngram = trigrams$ngram, count = trigrams$n, tail = trigrams$tail)

# Save trigrams to a file
saveRDS(trigrams, file = "trigrams.rds")

# Restore trigrams from file
trigrams <- readRDS(file = "trigrams.rds")

# Combine QUADGRAMS ---------------------------------------------------------------

# Open blogs files
blogsQuadgrams1 <- readRDS("blogsQuadgrams1.rds")
blogsQuadgrams2 <- readRDS("blogsQuadgrams2.rds")
blogsQuadgrams3 <- readRDS("blogsQuadgrams3.rds")
blogsQuadgrams4 <- readRDS("blogsQuadgrams4.rds")
blogsQuadgrams5 <- readRDS("blogsQuadgrams5.rds")

# Count quadgrams separately and combine
blogsQuadgrams1 <- count(blogsQuadgrams1, ngram, sort = TRUE)
blogsQuadgrams2 <- count(blogsQuadgrams2, ngram, sort = TRUE)
blogsQuadgrams3 <- count(blogsQuadgrams3, ngram, sort = TRUE)
blogsQuadgrams4 <- count(blogsQuadgrams4, ngram, sort = TRUE)
blogsQuadgrams5 <- count(blogsQuadgrams5, ngram, sort = TRUE)

# Look at this for merging https://stackoverflow.com/questions/42942405/combine-data-tables-and-sum-the-shared-column

# Try this code to calculate the counts of ngrams
# sqldf("select count(distinct(x)) from df1")
# count(distinct(x))

blogsQuadgrams6 <- merge(blogsQuadgrams1, blogsQuadgrams2, by = "ngram")
blogsQuadgrams6 <- mutate(blogsQuadgrams6, count6 = blogsQuadgrams6$n.x + blogsQuadgrams6$n.y)
blogsQuadgrams6 <- blogsQuadgrams6[ , !(names(blogsQuadgrams6) %in% c("n.x", "n.y"))]

blogsQuadgrams7 <- merge(blogsQuadgrams3, blogsQuadgrams4, by = "ngram")
blogsQuadgrams7 <- mutate(blogsQuadgrams7, count7 = blogsQuadgrams7$n.x + blogsQuadgrams7$n.y)
blogsQuadgrams7 <- blogsQuadgrams7[ , !(names(blogsQuadgrams7) %in% c("n.x", "n.y"))]

blogsQuadgrams8 <- merge(blogsQuadgrams6, blogsQuadgrams7, by = "ngram")
blogsQuadgrams9 <- merge(blogsQuadgrams5, blogsQuadgrams8, by = "ngram")
blogsQuadgrams9 <- mutate(blogsQuadgrams9, count = blogsQuadgrams9$n + blogsQuadgrams9$count6 + blogsQuadgrams9$count7)
blogsQuadgrams <- blogsQuadgrams9[ , !(names(blogsQuadgrams9) %in% c("n", "count6", "count7"))]

# Save blogs quadgrams and frequencies
saveRDS(blogsQuadgrams, file = "blogsQuadgrams.rds")

# Remove unnecessary files
remove(blogsQuadgrams1)
remove(blogsQuadgrams2)
remove(blogsQuadgrams3)
remove(blogsQuadgrams4)
remove(blogsQuadgrams5)
remove(blogsQuadgrams6)
remove(blogsQuadgrams7)
remove(blogsQuadgrams8)
remove(blogsQuadgrams9)

gc()

# Open twitter files
twitterQuadgrams1 <- readRDS("twitterQuadgrams1.rds")
twitterQuadgrams2 <- readRDS("twitterQuadgrams2.rds")
twitterQuadgrams3 <- readRDS("twitterQuadgrams3.rds")

# Count twitter quadgrams
twitterQuadgrams1 <- count(twitterQuadgrams1, ngram, sort = TRUE)
twitterQuadgrams2 <- count(twitterQuadgrams2, ngram, sort = TRUE)
twitterQuadgrams3 <- count(twitterQuadgrams3, ngram, sort = TRUE)

# Merge quadgrams data tables
twitterQuadgrams1 <- data.table(twitterQuadgrams1)
twitterQuadgrams2 <- data.table(twitterQuadgrams2)
twitterQuadgrams3 <- data.table(twitterQuadgrams3)

setkey(twitterQuadgrams1, ngram)
setkey(twitterQuadgrams2, ngram)
setkey(twitterQuadgrams3, ngram)

twitterQuadgrams <- merge(twitterQuadgrams1, twitterQuadgrams2, by = "ngram", all = TRUE)
twitterQuadgrams <- merge(twitterQuadgrams, twitterQuadgrams3, by = "ngram", all = TRUE)

# Set counts to 0 if NA
twitterQuadgrams[is.na(twitterQuadgrams)] <- 0

# Aggregate count
twitterQuadgrams <- mutate(twitterQuadgrams, count = twitterQuadgrams$n.x + twitterQuadgrams$n.y + twitterQuadgrams$n)

# Remove unneessary files
remove(twitterQuadgrams1)
remove(twitterQuadgrams2)
remove(twitterQuadgrams3)

gc()

# Remove unnecessary columns
twitterQuadgrams <- twitterQuadgrams[, !(names(twitterQuadgrams) %in% c("n.x", "n.y", "n"))]

# Save twitter quadgrams and frequencies
saveRDS(twitterQuadgrams, file = "twitterQuadgrams.rds")

# Open news quadgrams
newsQuadgrams <- readRDS("newsQuadgrams.rds")

# Count news quadgrams
newsQuadgrams <- count(newsQuadgrams, ngram, sort = TRUE)

# Merge twitter and news quadgrams
newsQuadgrams <- data.table(newsQuadgrams)
setkey(newsQuadgrams, ngram)

twitterQuadgrams <- data.table(twitterQuadgrams)
setkey(twitterQuadgrams, ngram)

quadgrams <- merge(twitterQuadgrams, newsQuadgrams, by = "ngram", all = TRUE)

# Set counts to 0 if NA
quadgrams[is.na(quadgrams)] <- 0

# Aggregate counts
quadgrams <- mutate(quadgrams, newcount = quadgrams$count + quadgrams$n)

# Remove unnecessary columns
quadgrams <- quadgrams[, !(names(quadgrams) %in% c("count", "n"))]

# Close unnecessary files
remove(twitterQuadgrams)
remove(newsQuadgrams)

gc()

# Merge with blogs quadgrams
quadgrams <- merge(quadgrams, blogsQuadgrams, by = "ngram")

# Aggregate counts
quadgrams <- mutate(quadgrams, totalcount = quadgrams$newcount + quadgrams$count)

# Remove unnecessary columns
quadgrams <- quadgrams[, !(names(quadgrams) %in% c("newcount", "count"))]

# Delete singletons
quadgrams <- quadgrams[!(quadgrams$n == 1), ]

gc()

# Convert to a data table
quadgrams <- data.table(ngram = quadgrams$ngram, count = quadgrams$totalcount)

# Separate individual words
quadgrams <- mutate(quadgrams, word1 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 1))
quadgrams <- mutate(quadgrams, word2 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 2))
quadgrams <- mutate(quadgrams, word3 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 3))
quadgrams <- mutate(quadgrams, word4 = sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 4))

# Save quadgrams to a file
saveRDS(quadgrams, file = "quadgrams.rds")

# Restore quadgrams from file
quadgrams <- readRDS(file = "quadgrams.rds")

# Index the ngrams to improve performance
setkey(unigrams, ngram)
setkey(bigrams, ngram)
setkey(trigrams, word1, word2, word3)
setkey(quadgrams, word1, word2, word3, word4)


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

inputText <- "a case of"

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

qObservedQuadgrams <- getObservedQuadgramProbs(observedQuadgrams, trigrams, inputText, gamma4)

# Calculate discounted probability mass for quadgrams to redistribute to trigrams
getAlphaQuadgram <- function(observedQuadgrams, trigram, quadgramDisc = .5) {
        if(nrow(observedQuadgrams) < 1) return(1)
        alphaQuadgram <- 1 - sum((observedQuadgrams$count - quadgramDisc) / trigram$count[1])
        
        return(alphaQuadgram)
}

trigram <- trigrams[trigrams$word1 == makeTokens(inputText, n = 1L)$text1[1] &
                            trigrams$word2 == makeTokens(inputText, n = 1L)$text1[2] &
                            trigrams$word3 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaQuadgram <- getAlphaQuadgram(observedQuadgrams, trigram, gamma4)

# Find the tail (last) words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams
getUnobservedQuadgramTails <- function(observedQuadgrams, inputUnigrams) {
        observedQuadgramTails <- observedQuadgrams$word4
        unobservedQuadgramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedQuadgramTails), ]$word1
        return(unobservedQuadgramTails)
}

unobservedQuadgramTails <- getUnobservedQuadgramTails(observedQuadgrams, unigrams)

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
        # Concatenate words to create an n-gram column
        boTrigrams <- mutate(boTrigrams, ngram = paste(boTrigrams$word1, boTrigrams$word2, boTrigrams$word3))
        # Concatenate words to create an n-gram column
        inputTrigrams <- mutate(inputTrigrams, ngram = paste(inputTrigrams$word1, inputTrigrams$word2, inputTrigrams$word3))
        observedBoTrigrams <- inputTrigrams[inputTrigrams$ngram %in% boTrigrams$ngram, ]
        observedBoTrigrams <- observedBoTrigrams[, !(names(observedBoTrigrams) %in% "ngram")]
        return(observedBoTrigrams)
}

observedBoTrigrams <- getObservedBoTrigrams(inputText, unobservedQuadgramTails, trigrams)

# Calculate the probabilities of observed BO trigrams
getObservedBoTrigramProbs <- function(observedBoTrigrams, inputBigrams, inputString, trigramDisc = .5) {
        if(nrow(observedBoTrigrams) < 1) return(NULL)
        obsCount <- filter(inputBigrams, inputBigrams$word1 == makeTokens(inputString)$text1[2] & 
                                   inputBigrams$word2 == makeTokens(inputString)$text1[3])$count[1]
        obsTrigramProbs <- mutate(observedBoTrigrams, count = ((count - trigramDisc) / obsCount))
        colnames(obsTrigramProbs) <- c("word1", "word2", "word3", "prob")
        
        return(obsTrigramProbs)
}

qObservedBoTrigrams <- getObservedBoTrigramProbs(observedBoTrigrams, bigrams, inputText, gamma3)

# Calculate the discount mass probability to redistribute to bigrams
getAlphaTrigram <- function(observedBoTrigrams, bigram, trigramDisc = .5) {
        if(nrow(observedBoTrigrams) < 1) return(0)
        alpha <- 1 - sum((observedBoTrigrams$count - trigramDisc) / bigram$count[1])
        
        return(alpha)
}

bigram <- bigrams[bigrams$word1 == makeTokens(inputText, n = 1L)$text1[2] &
                          bigrams$word2 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaTrigram <- getAlphaTrigram(observedBoTrigrams, bigram, gamma3)

# Find UNOBSERVED trigrams from BO trigrams
getUnobservedBoTrigrams <- function(boTrigrams, inputString, observedBoTrigrams) {
        # Concatenate words to create an n-gram column
        boTrigrams <- mutate(boTrigrams, ngram = paste(boTrigrams$word1, boTrigrams$word2, boTrigrams$word3))
        # Concatenate words to create an n-gram column
        observedBoTrigrams <- mutate(observedBoTrigrams, ngram = paste(observedBoTrigrams$word1, observedBoTrigrams$word2, observedBoTrigrams$word3))
        unobservedTrigrams <- boTrigrams[!(boTrigrams$ngram %in% observedBoTrigrams$ngram), ]
        return(unobservedTrigrams)
}

# Find the tail words of UNOBSERVED trigrams that start with the first 2 words of observedTrigrams
getUnobservedBoTrigramTails <- function(observedBoTrigrams, inputUnigrams) {
        observedTrigramTails <- observedBoTrigrams$word3
        unobservedTrigramTails <- inputUnigrams[!(inputUnigrams$word1 %in% observedTrigramTails), ]$word1
        return(unobservedTrigramTails)
}

unobservedBoTrigramTails <- getUnobservedBoTrigramTails(observedBoTrigrams, unigrams)

# Get backed off bigrams
getBoBigrams <- function(inputString, unobservedTrigramTails) {
        w_i_minus1 <- makeTokens(inputString, n = 1L)$text1[[3]]
        boBigrams <- data.table(word1 = rep(w_i_minus1, length(unobservedTrigramTails)), word2 = unobservedTrigramTails)
        return(boBigrams)
}

boBigrams <- getBoBigrams(inputText, unobservedBoTrigramTails)

# Get OBSERVED bigrams from the set of BO bigrams
getObservedBoBigrams <- function(inputString, unobservedTrigramTails, inputBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        # Concatenate words to create an n-gram column
        boBigrams <- mutate(boBigrams, ngram = paste(boBigrams$word1, boBigrams$word2))
        # Concatenate words to create an n-gram column
        inputBigrams <- mutate(inputBigrams, ngram = paste(inputBigrams$word1, inputBigrams$word2))
        observedBoBigrams <- inputBigrams[inputBigrams$ngram %in% boBigrams$ngram, ]
        return(observedBoBigrams)
}

observedBoBigrams <- getObservedBoBigrams(inputText, unobservedBoTrigramTails, bigrams)

# Calculate probabilities for OBSERVED BO bigrams
# q_bo(w1 | w2)
getObservedBoBigramProbs <- function(observedBoBigrams, inputUnigrams, bigramDisc = .5) {
        word <- observedBoBigrams$word1[1]
        wordCount <- inputUnigrams[inputUnigrams$word1 == word]
        observedBigramProbs <- (observedBoBigrams$count - bigramDisc) / wordCount$count
        observedBigramProbs <- data.table(word1 = observedBoBigrams$word1, 
                                          word2 = observedBoBigrams$word2, 
                                          prob = observedBigramProbs)
        
        return(observedBigramProbs)
}

qObservedBoBigrams <- getObservedBoBigramProbs(observedBoBigrams, unigrams, gamma2)

# Calculate the discount mass probability for bigrams
# alpha for observed bigrams = 1 - sum_of((observed_bigram_count - discount) / unigram_count)
getAlphaBigram <- function(observedBoBigrams, unigram, bigramDisc = .5) {
        if(nrow(observedBoBigrams) < 1) return(0)
        alpha <- 1 - sum((observedBoBigrams$count - bigramDisc) / unigram$count[1])
        
        return(alpha)
}

unigram <- unigrams[unigrams$word1 == makeTokens(inputText, n = 1L)$text1[3], ]
alphaBigram <- getAlphaBigram(observedBoBigrams, unigram, gamma2)

# Get UNOBSERVED bigrams from the set of BO bigrams
getUnobservedBoBigrams <- function(inputString, unobservedTrigramTails, observedBoBigrams) {
        boBigrams <- getBoBigrams(inputString, unobservedTrigramTails)
        # Concatenate words to create an n-gram column
        boBigrams <- mutate(boBigrams, ngram = paste(boBigrams$word1, boBigrams$word2))
        # Concatenate words to create an n-gram column
        observedBoBigrams <- mutate(observedBoBigrams, ngram = paste(observedBoBigrams$word1, observedBoBigrams$word2))
        unobservedBigrams <- boBigrams[!(boBigrams$ngram %in% observedBoBigrams$ngram), ]
        return(unobservedBigrams)
}

unobservedBoBigrams <- getUnobservedBoBigrams(inputText, unobservedBoTrigramTails, observedBoBigrams)

# Calculate probabilities for UNOBSERVED BO bigrams
# q_bo(w1 | w2)
getUnobservedBoBigramProbs <- function(unobservedBoBigrams, inputUnigrams, alphaBigram) {
        # get unobserved bigram tails
        unobsBigrams <- unobservedBoBigrams$word2
        w_in_Aw_iminus1 <- inputUnigrams[!(inputUnigrams$word1 %in% unobsBigrams), ]
        # convert to data table with counts
        qUnobservedBoBigrams <- inputUnigrams[inputUnigrams$word1 %in% unobsBigrams]
        denom <- sum(qUnobservedBoBigrams$count)
        # convert counts to probabilities
        qUnobservedBoBigrams <- data.table(word1 = unobservedBoBigrams$word1, 
                                           word2 = unobservedBoBigrams$word2,
                                           prob = (alphaBigram * qUnobservedBoBigrams$count / denom))
        
        return(qUnobservedBoBigrams)
}

qUnobservedBoBigrams <- getUnobservedBoBigramProbs(unobservedBoBigrams, unigrams, alphaBigram)

# Calculate UNOBSERVED trigram probabilities
getUnobservedBoTrigramProbs <- function(inputString, qObservedBoBigrams, qUnobservedBoBigrams, alphaTrigram) {
        qBoBigrams <- rbind(qObservedBoBigrams, qUnobservedBoBigrams)
        qBoBigrams <- qBoBigrams[order(-qBoBigrams$prob), ]
        sumqBoBigrams <- sum(qBoBigrams$prob)
        word <- makeTokens(inputString, n = 1L)$text1[[2]]
        # unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = "_")
        unobservedTrigramProbs <- alphaTrigram * qBoBigrams$prob / sumqBoBigrams
        unobservedTrigramsDT <- data.table(word1 = word, word2 = qBoBigrams$word1, 
                                           word3 = qBoBigrams$word2, prob = unobservedTrigramProbs)
        
        return(unobservedTrigramsDT)
}

qUnobservedBoTrigrams <- getUnobservedBoTrigramProbs(inputText, qObservedBoBigrams, qUnobservedBoBigrams, alphaTrigram)

# Calculate UNOBSERVED quadgram probabilities
getUnobservedQuadgramProbs <- function(inputString, qObservedBoTrigrams, qUnobservedBoTrigrams, alphaQuadgram) {
        qBoTrigrams <- rbind(qObservedBoTrigrams, qUnobservedBoTrigrams)
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

qUnobservedQuadgrams <- getUnobservedQuadgramProbs(inputText, qObservedBoTrigrams, qUnobservedBoTrigrams, alphaQuadgram)


qQuadgrams <- rbind(qObservedQuadgrams, qUnobservedQuadgrams)
qQuadgrams <- qQuadgrams[order(-qQuadgrams$prob), ]

