# TASK 2: EXPLORATORY DATA ANALYSIS

library(tm)
library(quanteda)
library(readtext)

# 1. Exploratory analysis - perform a thorough exploratory analysis of the data, 
# understanding the distribution of words and relationship between the words 
# in the corpora.

# Create a tm corpus
# myCorpus <- Corpus(DirSource(directory = "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US"))

# Write to disk
# writeCorpus(myCorpus)

# Define a function to extract a random sample of 30% of the data and store in a file
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
                if (rbinom(1, 1, prob = .3) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Extract random samples of en_US files
infile_twitter <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
infile_news <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
infile_blogs <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"

# Extract samples of text files and store in a local file
myTwitter <- sampleFile(infile_twitter, "myTwitter.txt", header = FALSE)
myNews <- sampleFile(infile_news, "myNews.txt", header = FALSE)
myBlogs <- sampleFile(infile_blogs, "myBlogs.txt", header = FALSE)

# Load files
require(readtext)

myTwitterRead <- readtext("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/myTwitter.txt")
myNewsRead <- readtext("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/myNews.txt")
myBlogsRead <- readtext("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/myBlogs.txt")

# Create a tm corpus
myCorpus <- SimpleCorpus(DirSource(directory = "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/sampledata"))

# Eliminate extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# Convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# Create document-term matrices
myDtm <- DocumentTermMatrix(myCorpus)


# Create a quanteda corpus object
myQCorpus <- corpus(myCorpus)
textstat_readability(myQCorpus)

# Understand the distribution of words and relationship between words in the corpora
myTokens <- tokens(myQCorpus, remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE)
tokens_ngrams(myTokens)

my_dfm <- dfm(myQCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
topfeatures(my_dfm, 20)
textstat_frequency(my_dfm)

set.seed(100)
textplot_wordcloud(my_dfm, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


# 2. Understand frequencies of words and word pairs - build figures and tables 
# to understand variation in the frequencies of words and word pairs in the data.

