# Exploratory Data Analysis

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(tm)
library(quanteda)

# ==================================================================================
        
# TIDY TEXT

# Extract a random sample of 30% of each file and store in a file
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

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt"
sampleFile(t, "twitter.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt", "r")
twitter <- readLines(con) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt"
sampleFile(n, "news.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
news <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/news.txt", "r")
blogs <- readLines(con) 
close(con) 

# Clean text using regular expressions
reg1 <- "&amp;|&lt;|&gt;"
reg2 <- "[0-9]+"
reg3 <- "[Ss][Hh][Ii][Tt] | [Ff][Uu][Cc][Kk] | [Ff][Uu][Cc][Kk][Ii][Nn][Gg] | [Bb][Ii][Tt][Cc][Hh] | [Aa][Ss][Ss]"
reg4 <- "\\ðÿ"
reg5 <- "â"

##### Find the top 50 words from the files in other languages, create a data frame
##### with these terms and perform an anti-join to remove them

# German data set ------------------------------------------------------------------

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/de_DE/de_DE.twitter.txt"
sampleFile(t, "twitterDE.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitterDE.txt", "r")
twitterDE <- readLines(con) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/de_DE/de_DE.news.txt"
sampleFile(n, "newsDE.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/newsDE.txt", "r")
newsDE <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/de_DE/de_DE.blogs.txt"
sampleFile(b, "blogsDE.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/newsDE.txt", "r")
blogsDE <- readLines(con) 
close(con) 

# Create a tibble with a row for every word in every row of all 3 samples
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitterDE, newsDE, blogsDE)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                filter(!str_detect(text, "^RT")) %>%
                unnest_tokens(word, text) %>%
                mutate(source = titles[[i]]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Count word frequency
frequency <- series %>%
        count(word, sort = TRUE)

# Find the subset of unique words in a frequency-sorted dictionary
# to cover 50% of all word instances in the language
count <- 0
wordSum <- sum(frequency$n)
wordDictionary <- data.frame(word = as.character(""), stringsAsFactors = FALSE)

for(i in 1:length(frequency$n)) {
        count <- count + frequency$n[i]
        wordDictionary$word[i] <- frequency$word[i]
        
        if(count / wordSum >= .5) {
                break
        }
        wordDictionary[nrow(wordDictionary) + 1, ] = as.character("")
}

wordDictionaryDE <- wordDictionary

# Finnish data set ------------------------------------------------------------------

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/fi_FI/fi_FI.twitter.txt"
sampleFile(t, "twitterFI.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitterFI.txt", "r")
twitterFI <- readLines(con) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/fi_FI/fi_FI.news.txt"
sampleFile(n, "newsFI.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/newsFI.txt", "r")
newsFI <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/fi_FI/fi_FI.blogs.txt"
sampleFile(b, "blogsFI.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/newsFI.txt", "r")
blogsFI <- readLines(con) 
close(con) 

# Create a tibble with a row for every word in every row of all 3 samples
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitterFI, newsFI, blogsFI)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                filter(!str_detect(text, "^RT")) %>%
                unnest_tokens(word, text) %>%
                mutate(source = titles[[i]]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Count word frequency
frequency <- series %>%
        count(word, sort = TRUE)

# Find the subset of unique words in a frequency-sorted dictionary
# to cover 50% of all word instances in the language
count <- 0
wordSum <- sum(frequency$n)
wordDictionary <- data.frame(word = as.character(""), stringsAsFactors = FALSE)

for(i in 1:length(frequency$n)) {
        count <- count + frequency$n[i]
        wordDictionary$word[i] <- frequency$word[i]
        
        if(count / wordSum >= .5) {
                break
        }
        wordDictionary[nrow(wordDictionary) + 1, ] = as.character("")
}

wordDictionaryFI <- wordDictionary

# Russian data set ------------------------------------------------------------------

# Extract Twitter sample
t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/ru_RU/ru_RU.twitter.txt"
sampleFile(t, "twitterRU.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitterRU.txt", "r")
twitterRU <- readLines(con) 
close(con) 

# Extract news sample
n <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/ru_RU/ru_RU.news.txt"
sampleFile(n, "newsRU.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/newsRU.txt", "r")
newsRU <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/ru_RU/ru_RU.blogs.txt"
sampleFile(b, "blogsRU.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogsRU.txt", "r")
blogsRU <- readLines(con) 
close(con) 

# Create a tibble with a row for every word in every row of all 3 samples
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitterRU, newsRU, blogsRU)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                filter(!str_detect(text, "^RT")) %>%
                unnest_tokens(word, text) %>%
                mutate(source = titles[[i]]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Count word frequency
frequency <- series %>%
        count(word, sort = TRUE)

# Find the subset of unique words in a frequency-sorted dictionary
# to cover 50% of all word instances in the language
count <- 0
wordSum <- sum(frequency$n)
wordDictionary <- data.frame(word = as.character(""), stringsAsFactors = FALSE)

for(i in 1:length(frequency$n)) {
        count <- count + frequency$n[i]
        wordDictionary$word[i] <- frequency$word[i]
        
        if(count / wordSum >= .5) {
                break
        }
        wordDictionary[nrow(wordDictionary) + 1, ] = as.character("")
}

wordDictionaryRU <- wordDictionary


# Back to English data set ---------------------------------------------------------

twitter_cleaned <- gsub(reg1, " ", twitter)
twitter_cleaned <- gsub(reg2, " ", twitter_cleaned)
twitter_cleaned <- gsub(reg3, " ", twitter_cleaned)
twitter_cleaned <- gsub(reg4, " ", twitter_cleaned, fixed = TRUE)
twitter_cleaned <- gsub(reg5, " ", twitter_cleaned, fixed = TRUE)

news_cleaned <- gsub(reg1, " ", news)
news_cleaned <- gsub(reg2, " ", news_cleaned)
news_cleaned <- gsub(reg3, " ", news_cleaned)
news_cleaned <- gsub(reg4, " ", news_cleaned, fixed = TRUE)
news_cleaned <- gsub(reg5, " ", news_cleaned, fixed = TRUE)

blogs_cleaned <- gsub(reg1, " ", blogs)
blogs_cleaned <- gsub(reg2, " ", blogs_cleaned)
blogs_cleaned <- gsub(reg3, " ", blogs_cleaned)
blogs_cleaned <- gsub(reg4, " ", blogs_cleaned, fixed = TRUE)
blogs_cleaned <- gsub(reg5, " ", blogs_cleaned, fixed = TRUE)

# Create a tibble with a row for every word in every row of all 3 samples
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitter_cleaned, news_cleaned, blogs_cleaned)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                filter(!str_detect(text, "^RT")) %>%
                unnest_tokens(word, text) %>%
                mutate(source = titles[[i]]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Download a list of profanities
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)
profanity <- as.data.frame(profanity, stringsAsFactors = FALSE)
colnames(profanity) <- c("word")

# Remove profanities
series <- anti_join(series, profanity)

# Count word frequency
frequency <- series %>%
        count(word, sort = TRUE)

# Find the subset of unique words in a frequency-sorted dictionary
# to cover 50% of all word instances in the language
count <- 0
wordSum <- sum(frequency$n)
wordDictionary <- data.frame(word = as.character(""), stringsAsFactors = FALSE)

for(i in 1:length(frequency$n)) {
        count <- count + frequency$n[i]
        wordDictionary$word[i] <- frequency$word[i]

        if(count / wordSum >= .5) {
                break
        }
        wordDictionary[nrow(wordDictionary) + 1, ] = as.character("")
}

# Count word frequency after removing stop words and common foreign words
series2 <- anti_join(series, wordDictionaryDE)
series3 <- anti_join(series2, wordDictionaryFI)
series4 <- anti_join(series3, wordDictionaryRU)
series <- series4

series %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)

# Find the top 10 most common words from each source
topWords <- series %>%
        anti_join(stop_words) %>%
        group_by(source) %>%
        count(word, sort = TRUE) %>%
        top_n(10)

# Is this necessary?
series %>%
        filter(!str_detect(text, "^RT")) %>%
        mutate(text = str_remove_all(text, remove_reg)) %>%
        unnest_tokens(word, text, token = )

# Visualise the top 10 most common words from each source
series %>%
        anti_join(stop_words) %>%
        group_by(source) %>%
        count(word, sort = TRUE) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(source = factor(source, levels = titles),
               text_order = nrow(.):1) %>%
        ggplot(aes(reorder(word, text_order), n, fill = source)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ source, scales = "free_y") +
        labs(x = "NULL", y = "Frequency") +
        coord_flip() +
        theme(legend.position="none")

# Calculate percent of word use across all sources
source_pct <- series %>%
        anti_join(stop_words) %>%
        count(word) %>%
        transmute(word, all_words = n / sum(n))

# Calculate percent of word use within each source
frequency <- series %>%
        anti_join(stop_words) %>%
        count(source, word) %>%
        mutate(source_words = n / sum(n)) %>%
        left_join(source_pct) %>%
        arrange(desc(source_words)) %>%
        ungroup()

frequency

# Visualise
ggplot(frequency, aes(x = source_words, y = all_words, color = abs(all_words - source_words))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = scales::percent_format()) +
        scale_y_log10(labels = scales::percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
        facet_wrap(~ source, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Sources", x = NULL)

# Quantify how similar and different these sets of word frequencies are using 
# a correlation test
frequency %>%
        group_by(source) %>%
        summarize(correlation = cor(source_words, all_words),
                  p_value = cor.test(source_words, all_words)$p.value)

# ==================================================================================

# SENTIMENT ANALYSIS

# Use the nrc sentiment data set to assess the different sentiments
series %>%
        right_join(get_sentiments("nrc")) %>%
        filter(!is.na(sentiment)) %>%
        count(sentiment, sort = TRUE)

# Look at the change in sentiment for every 50 words across each source
series %>%
        group_by(source) %>% 
        mutate(word_count = 1:n(),
               index = word_count %/% 50 + 1) %>% 
        inner_join(get_sentiments("bing")) %>%
        count(source, index = index , sentiment) %>%
        ungroup() %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative,
               source = factor(source, levels = titles)) %>%
        ggplot(aes(index, sentiment, fill = source)) +
        geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
        facet_wrap(~ source, ncol = 2, scales = "free_x")

# Examine how the different sentiment lexicons differ for each source
afinn <- series %>%
        group_by(source) %>% 
        mutate(word_count = 1:n(),
               index = word_count %/% 50 + 1) %>% 
        inner_join(get_sentiments("afinn")) %>%
        group_by(source, index) %>%
        summarise(sentiment = sum(score)) %>%
        mutate(method = "AFINN")

bing_and_nrc <- bind_rows(series %>%
                                  group_by(source) %>% 
                                  mutate(word_count = 1:n(),
                                         index = word_count %/% 50 + 1) %>% 
                                  inner_join(get_sentiments("bing")) %>%
                                  mutate(method = "Bing"),
                          series %>%
                                  group_by(source) %>% 
                                  mutate(word_count = 1:n(),
                                         index = word_count %/% 50 + 1) %>%
                                  inner_join(get_sentiments("nrc") %>%
                                                     filter(sentiment %in% c("positive", "negative"))) %>%
                                  mutate(method = "NRC")) %>%
        count(source, method, index = index , sentiment) %>%
        ungroup() %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>%
        select(source, index, method, sentiment)

# Analyse word counts that contribute to each sentiment
bing_word_counts <- series %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()

# Views this visually to assess the top n words for each sentiment
bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment", x = NULL) +
        coord_flip()

# Sentence sentiment analysis

# Use the news analysis
# Break up by row and sentence
news_sentences <- tibble(row = 1:length(news),
                       text = news) %>% 
        unnest_tokens(sentence, text, token = "sentences")

# Create a tibble with individual words by sentence within each row
# Use the AFINN lexicon and compute the net sentiment score for each row (entry)
row_sent <- news_sentences %>%
        group_by(row) %>%
        mutate(sentence_num = 1:n(),
               index = round(sentence_num / n(), 2)) %>%
        unnest_tokens(word, sentence) %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(row, index) %>%
        summarise(sentiment = sum(score, na.rm = TRUE)) %>%
        arrange(desc(sentiment))

# Visualise with a heatmap
ggplot(row_sent, aes(index, factor(row, levels = sort(unique(row), decreasing = TRUE)), fill = sentiment)) +
        geom_tile(color = "white") +
        scale_fill_gradient2() +
        scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(x = "Row / Entry Progression", y = "Row") +
        ggtitle("Sentiment of News",
                subtitle = "Summary of the net sentiment score as you progress through each row") +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "top")

# ==================================================================================

# TERM VS DOCUMENT FREQUENCY

# Compute the term frequency for each word
source_words <- series %>%
        count(source, word, sort = TRUE) %>%
        ungroup()

series_words <- source_words %>%
        group_by(source) %>%
        summarise(total = sum(n))

source_words <- left_join(source_words, series_words)

# Visualise the distribution of term frequency
source_words %>%
        mutate(ratio = n / total) %>%
        ggplot(aes(ratio, fill = source)) +
        geom_histogram(show.legend = FALSE) +
        scale_x_log10() +
        facet_wrap(~ source, ncol = 2)

# Visualise frequency by rank (see Zipf's law)
freq_by_rank <- source_words %>%
        group_by(source) %>%
        mutate(rank = row_number(),
               `term freq` = n / total)

ggplot(freq_by_rank, aes(rank, `term freq`, color = source)) +
        geom_line(show.legend = TRUE) +
        scale_x_log10() +
        scale_y_log10()

# Compare the distribution to a regression line
lower_rank <- freq_by_rank %>%
        filter(rank < 500)

lm(log10(`term freq`) ~ log10(rank), data = lower_rank)

freq_by_rank %>% 
        ggplot(aes(rank, `term freq`, color = source)) +
        geom_abline(intercept = -0.9414, slope = -0.9694, color = "gray50", linetype = 2) +
        geom_line(size = 1.2, alpha = 0.8) +
        scale_x_log10() +
        scale_y_log10()

# Determine inverse document frequency and tf-idf to find the important words by descreasing
# the weight for commonly used words
source_words <- source_words %>%
        bind_tf_idf(word, source, n)

source_words %>%
        arrange(desc(tf_idf))

# To understand the most important contextual words from each source,
# look at the top 15 terms with the highest tf-idf
source_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word))),
               source = factor(source, levels = titles)) %>% 
        group_by(source) %>%
        top_n(15, wt = tf_idf) %>%
        ungroup() %>%
        ggplot(aes(word, tf_idf, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        labs(title = "Highest tf-idf Words",
             x = NULL, y = "tf-idf") +
        facet_wrap(~source, ncol = 2, scales = "free") +
        coord_flip()

# ==================================================================================

# WORD RELATIONSHIPS

# n-gram analysis
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitter_cleaned, news_cleaned, blogs_cleaned)
series <- tibble()

### Need to remove profanities

# 2-gram
for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                mutate(source = titles[i]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Look at the most common bi-grams across all sources
series %>%
        count(bigram, sort = TRUE)

# Filter out common stop words
series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)

# Visualise the top 10 bi-grams for each source
series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(source, word1, word2, sort = TRUE) %>%
        unite("bigram", c(word1, word2), sep = " ") %>%
        group_by(source) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(source = factor(source) %>% forcats::fct_rev()) %>%
        ggplot(aes(bigram, n, source, n, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        facet_wrap(~ source, ncol = 2, scales = "free") +
        coord_flip()

# Identify the tf-idf of n-grams
bigram_tf_idf <- series %>%
        count(source, bigram, sort = TRUE) %>%
        bind_tf_idf(bigram, source, n) %>%
        arrange(desc(tf_idf))

# Visualise the bigrams with the highest tf-idf for each source
bigram_tf_idf %>%
        group_by(source) %>%
        top_n(15, wt = tf_idf) %>%
        ungroup() %>%
        mutate(source = factor(source) %>% forcats::fct_rev()) %>%
        ggplot(aes(bigram, tf_idf, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        labs(title = "Highest tf-idf bi-grams",
             x = NULL, y = "tf-idf") +
        facet_wrap(~source, ncol = 2, scales = "free") +
        coord_flip()

# Perform sentiment analysis on bi-gram data
series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(word1 == "not") %>%
        count(source, word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

nots <- series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(word1 == "not") %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word2, score, sort = TRUE) 

nots %>%
        mutate(contribution = n * score) %>%
        arrange(desc(abs(contribution))) %>%
        head(20) %>%
        ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        xlab("Words preceded by 'not'") +
        ylab("Sentiment score * # of occurrances") +
        coord_flip()

negation_words <- c("not", "no", "never", "without")

negated <- series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(word1 %in% negation_words) %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word1, word2, score, sort = TRUE) %>%
        ungroup()

# Visualise
negated %>%
        mutate(contribution = n * score) %>%
        arrange(desc(abs(contribution))) %>%
        group_by(word1) %>%
        top_n(10, abs(contribution)) %>%
        ggplot(aes(word2, contribution, fill = contribution > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        xlab("Words preceded by 'not'") +
        ylab("Sentiment score * # of occurrances") +
        facet_wrap(~ word1, scales = "free") +
        coord_flip()

# Visualise n-gram networks
library(igraph)

bigram_graph <- series %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE) %>%
        unite("bigram", c(word1, word2), sep = " ") %>%
        filter(n > 20) %>%
        graph_from_data_frame()

# Utilise ggraph to convert igraph object to a ggplot-like graphic
library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
        geom_edge_link() +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()

# Repeat for 3-gram
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(row = seq_along(samples[[i]]),
                        text = samples[[i]]) %>%
                unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                mutate(source = titles[i]) %>%
                select(source, everything())
        
        series <- rbind(series, clean)
}

# Look at the most common tri-grams across all sources
series %>%
        count(trigram, sort = TRUE)

# Filter out common stop words
series %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%        
        count(word1, word2, word3, sort = TRUE)

# Visualise the top 10 tri-grams for each source
series %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(source, word1, word2, word3, sort = TRUE) %>%
        unite("trigram", c(word1, word2, word3), sep = " ") %>%
        group_by(source) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(source = factor(source) %>% forcats::fct_rev()) %>%
        ggplot(aes(trigram, n, source, n, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        facet_wrap(~ source, ncol = 2, scales = "free") +
        coord_flip()

# Identify the tf-idf of n-grams
trigram_tf_idf <- series %>%
        count(source, trigram, sort = TRUE) %>%
        bind_tf_idf(trigram, source, n) %>%
        arrange(desc(tf_idf))

# Visualise the tri-grams with the highest tf-idf for each source
trigram_tf_idf %>%
        group_by(source) %>%
        top_n(15, wt = tf_idf) %>%
        ungroup() %>%
        mutate(source = factor(source) %>% forcats::fct_rev()) %>%
        ggplot(aes(trigram, tf_idf, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        labs(title = "Highest tf-idf tri-grams",
             x = NULL, y = "tf-idf") +
        facet_wrap(~source, ncol = 2, scales = "free") +
        coord_flip()



# Word correlation
news_words <- tibble(row = seq_along(news),
                     text = news) %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% stop_words$word)

# We can leverage the widyr package to count common pairs of words in the same row
library(widyr)

word_pairs <- news_words %>%
        pairwise_count(word, row, sort = TRUE)

# Look for which words most often follow "hard", for example
word_pairs %>% 
        filter(item1 == "hard")

# Correlation
word_cor <- news_words %>%
        group_by(word) %>%
        filter(n() >= 20) %>%
        pairwise_cor(word, row) %>%
        filter(!is.na(correlation))

# Assess correlation for words of interest, e.g. "home"
word_cor %>%
        filter(item1 == "home") %>%
        arrange(desc(correlation))

# Visualise correlation for word clusters
set.seed(123)

news_words %>%
        group_by(word) %>%
        filter(n() >= 20) %>%
        pairwise_cor(word, row) %>%
        filter(!is.na(correlation),
               correlation > .1) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_void()

# ==================================================================================

# CONVERTING BETWEEN TIDY AND NON-TIDY FORMATS

# See https://uc-r.github.io/text_conversion

# Create a document-feature-matrix (DFM) for each source


# ==================================================================================

# TM ANALYSIS

# Create a corpus
myCorpus_tm <- SimpleCorpus(DirSource(directory = "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/sampledata"))

# Download a list of profanities
fileURL <- "https://community.jivesoftware.com/servlet/JiveServlet/download/1907-1-3237/profanity-list.zip"
temp <- tempfile()

if(!file.exists("profanity-list.csv")) {
        download.file(fileURL,temp, mode="wb")
        unzip(temp, "profanity-list.csv")
}

profanity <- read.csv("profanity-list.csv", header=FALSE)
profanity <- as.vector(t(profanity))

# Pre-process the data by removing punctuation, white space, stop words, numbers 
# and profanities, and convert to lower case
myCorpus_tm <- tm_map(myCorpus_tm, removePunctuation)
myCorpus_tm <- tm_map(myCorpus_tm, stripWhitespace)
myCorpus_tm <- tm_map(myCorpus_tm, removeWords, stopwords("english"))
myCorpus_tm <- tm_map(myCorpus_tm, removeNumbers)
myCorpus_tm <- tm_map(myCorpus_tm, removeWords, profanity)
myCorpus_tm <- tm_map(myCorpus_tm, content_transformer(tolower))


# Create a document term matrix (DTM)
dtm <- DocumentTermMatrix(myCorpus_tm, control = list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      stopwords = TRUE))

# ==================================================================================

# QUANTEDA ANALYSIS

# Create document frequency matrices
twitter_dfm <- quanteda::dfm(twitter_cleaned, verbose = FALSE)

# Convert to a tidy data frame
twitter_tidy <- tidy(twitter_dfm)

# Convert tidy data frame (series) to a document frequency matrix (dfm)
my_dfm <- series %>%
        cast_dfm(word, source)

# Create a corpus
my_corpus_qu <- quanteda::corpus(myCorpus_tm)

# Tokenize -  ADD ARGUMENTS TO REMOVE NUMBERS, ETC.
my_tokens <- tokens(my_corpus_qu)

# Create a dictionary object
my_dictionary_DE <- dictionary(as.list(wordDictionaryDE))

# Find out how many common German words are in the corpus
dfm(tokens_lookup(my_tokens, my_dictionary_DE, valuetype = "glob", verbose = TRUE))

# Create n-grams
tokens_ngrams(my_tokens, n = 2L)
tokens_ngrams(my_tokens, n = 3L)

# Create a document-feature-matrix (dfm)
my_dfm <- dfm(my_corpus_qu, valuetype = "glob")

# Use stemming to reduce the number of words required ??????
my_tokens_stemmed <- tokens_wordstem(my_tokens, language = quanteda_options("language_stemmer"))
