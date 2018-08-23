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
