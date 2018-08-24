# Creating Tidy Text

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions

# Extract the first 50 lines
con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
twitter <- readLines(con, 50) 
close(con) 

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")
news <- readLines(con, 50) 
close(con) 

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
blogs <- readLines(con, 50) 
close(con) 

# Create a tibble
text_tb <- tibble(source = as.integer(1), text = sample)

# Unnest the texts
text_tb %>% unnest_tokens(word, text)

# Repeat across all sources
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitter, news, blogs)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(source = titles[[i]],
                        text = samples[[i]]) %>%
                unnest_tokens(word, text)
        
        series <- rbind(series, clean)
}

# Count word frequency
series %>%
        count(word, sort = TRUE)

# Count word frequency after removing stop words
series %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)

# Find the top 10 most common words from each source
series %>%
        anti_join(stop_words) %>%
        group_by(source) %>%
        count(word, sort = TRUE) %>%
        top_n(10)

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


# SENTIMENT ANALYSIS

# Use the nrc sentiment data set to assess the different sentiments
series %>%
        right_join(get_sentiments("nrc")) %>%
        filter(!is.na(sentiment)) %>%
        count(sentiment, sort = TRUE)

# Try the afinn sentiment data
series %>%
        right_join(get_sentiments("afinn")) %>%
        filter(!is.na(sentiment)) %>%
        count(sentiment, sort = TRUE)

# This code doesn't work
afinn <- series %>%
        group_by(source) %>%
        get_sentiments("afinn") %>%
        inner_join(get_sentiments("afinn")) %>%
        summarise(sentiment = sum(score)) %>%
        mutate(method = "AFINN")

# Analyse word counts that contribute to each sentiment
bing_word_counts <- series %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()

# View this visually to assess the top n words for each sentiment
bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment", x = NULL) +
        coord_flip()

# Tokenize sentences to determine sentence sentiment - news example
tibble(text = news) %>% 
        unnest_tokens(sentence, text, token = "sentences")

news_sentences <- tibble(text = news) %>% 
        unnest_tokens(sentence, text, token = "sentences")

news_sentiment <- news_sentences %>%
        mutate(sentence_num = 1:n(), index = round(sentence_num / n(), 2)) %>%
        unnest_tokens(word, sentence) %>%
        inner_join(get_sentiments("afinn")) %>%
        summarise(sentiment = sum(score, na.rm = TRUE)) %>%
        arrange(desc(sentiment))


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
        geom_line() +
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

# Look at the top 15 terms with the highest tf-idf
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


# WORD RELATIONSHIPS

# n-gram analysis
titles <- c("Twitter", "News", "Blogs")
samples <- list(twitter, news, blogs)
series <- tibble()

for(i in seq_along(titles)) {
        
        clean <- tibble(source = titles[[i]],
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
        ggplot(aes(drlib::reorder_within(bigram, n, source), n, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        drlib::scale_x_reordered() +
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
        ggplot(aes(drlib::reorder_within(bigram, tf_idf, source), tf_idf, fill = source)) +
        geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
        labs(title = "Highest tf-idf bi-grams",
             x = NULL, y = "tf-idf") +
        drlib::scale_x_reordered() +
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

negated %>%
        mutate(contribution = n * score) %>%
        arrange(desc(abs(contribution))) %>%
        group_by(word1) %>%
        top_n(10, abs(contribution)) %>%
        ggplot(aes(drlib::reorder_within(word2, contribution, word1), contribution, fill = contribution > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        xlab("Words preceded by 'not'") +
        ylab("Sentiment score * # of occurrances") +
        drlib::scale_x_reordered() +
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

library(ggraph)
set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
        geom_edge_link() +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()

# Word correlation
news_words <- tibble(row = seq_along(news),
                     text = news) %>%
                unnest_tokens(word, text) %>%
                filter(!word %in% stop_words$word)

# We can leverage the widyr package to count common pairs of words in the same row
library(widyr)

word_pairs <- news_words %>%
                pairwise_count(word, row, sort = TRUE)

# Look for which words most often follow "hard"
word_pairs %>% 
        filter(item1 == "hard")

# Correlation
word_cor <- news_words %>%
                group_by(word) %>%
                filter(n() >= 20) %>%
                pairwise_cor(word, row) %>%
                filter(!is.na(correlation))
