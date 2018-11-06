#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(ggplot2)

# Load data
setwd("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone")
ngrams <- readRDS("./data/ngramsTest.rds")
setkey(ngrams, ngram)

# Define server logic required to predict the next word
shinyServer(function(input, output) {
        
        # Function to predict next word
        wordPrediction <- reactive({
                
                # Create regex to remove URLs, twitter user names, hashtags, possessives and unicode / html tags
                stuff_to_remove <- c("http[s]?://[[:alnum:].\\/]+", "@[\\w]*", "#[\\w]*", "<.*>", "'s")
                stuff_to_remove <-  paste(stuff_to_remove, sep = "|", collapse="|")
                
                # Clean tweets
                clean_x <- str_replace_all(input$phrase, stuff_to_remove, "")
                
                # Remove leading and trailing spaces
                clean_x <- str_trim(clean_x)
                
                # Extract last 5 words from input text
                tkns <- tokens(clean_x, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols = FALSE, remove_separators = TRUE, remove_twitter = TRUE)
                tkns <- tolower(tkns)
                n <- length(tkns)
                
                # Create ngram search terms
                unigram <- ""
                unigramPre <- ""
                bigram <- ""
                bigramPre <- ""
                trigram <- ""
                trigramPre <- ""
                quadgram <- ""
                quadgramPre <- ""
                fivegram <- ""
                fivegramPre <- ""
                
                if(n > 0) {unigram <- tkns[n]}
                if(n > 1) {
                        bigram <- paste(tkns[n-1], tkns[n], sep = "_")
                        bigramPre <- paste(tkns[n-1], sep = "_")
                }
                if(n > 2) {
                        trigram <- paste(tkns[n-2], tkns[n-1], tkns[n], sep = "_")
                        trigramPre <- paste(tkns[n-2], tkns[n-1], sep = "_")
                }
                if(n > 3) {
                        quadgram <- paste(tkns[n-3], tkns[n-2], tkns[n-1], tkns[n], sep = "_")
                        quadgramPre <- paste(tkns[n-3], tkns[n-2], tkns[n-1], sep = "_")
                }
                if(n > 4) {
                        fivegram <- paste(tkns[n-4], tkns[n-3], tkns[n-2], tkns[n-1], tkns[n], sep = "_")
                        fivegramPre <- paste(tkns[n-4], tkns[n-3], tkns[n-2], tkns[n-1], sep = "_")
                }
                
                # Find sixgrams
                sixgrams <- data.table(ngram = vector(mode = "character", length = 0),
                                       count = vector(mode = "integer", length = 0),
                                       tail = vector(mode = "character", length = 0))
                sixgrams <- ngrams[ngram == fivegram]
                
                observedTails <- ""
                
                # Calculate the aggregate sixgram count and probabilities
                if(nrow(sixgrams) > 0) {
                        denom <- sixgrams[, .(sum(count))][1, 1]
                        sixgrams <- sixgrams[, prob := (count / denom[[1]])]
                        # Note observed tailwords
                        observedTails <- sixgrams$tail
                }
                
                # Back off to fivegrams
                fivegrams <- data.table(ngram = vector(mode = "character", length = 0),
                                        count = vector(mode = "integer", length = 0),
                                        tail = vector(mode = "character", length = 0))
                fivegrams <- ngrams[ngram == quadgram][!(tail %in% observedTails)]
                
                # Calculate the aggregate fivegram count and probabilities
                if(nrow(fivegrams) > 0) {
                        denom <- fivegrams[, .(sum(count))][1, 1]
                        fivegrams <- fivegrams[, prob := (count / denom[[1]] * .6)]
                        # Note observed fivegram tail words
                        observedTails <- c(observedTails, fivegrams$tail)
                }
                
                # Back off to quadgrams
                quadgrams <- data.table(ngram = vector(mode = "character", length = 0),
                                        count = vector(mode = "integer", length = 0),
                                        tail = vector(mode = "character", length = 0))
                quadgrams <- ngrams[ngram == trigram][!(tail %in% observedTails)]
                
                # Calculate the aggregate quadgram count and probabilities
                if(nrow(quadgrams) > 0) {
                        denom <- quadgrams[, .(sum(count))][1, 1]
                        quadgrams <- quadgrams[, prob := (count / denom[[1]] * .4)]
                        # Note observed quadgram tail words
                        observedTails <- c(observedTails, quadgrams$tail)
                }
                
                # Back off to trigrams
                trigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                       count = vector(mode = "integer", length = 0),
                                       tail = vector(mode = "character", length = 0))
                trigrams <- ngrams[ngram == bigram][!(tail %in% observedTails)]
                
                # Calculate the aggregate trigram count and probabilities
                if(nrow(trigrams) > 0) {
                        denom <- trigrams[, .(sum(count))][1, 1]
                        trigrams <- trigrams[, prob := (count / denom[[1]] * .3)]
                        # Note observed trigram tail words
                        observedTails <- c(observedTails, trigrams$tail)
                }
                
                # Back off to bigrams
                bigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                      count = vector(mode = "integer", length = 0),
                                      tail = vector(mode = "character", length = 0))
                bigrams <- ngrams[ngram == unigram][n == 2][!(tail %in% observedTails)]
                
                # Calculate the aggregate bigram count and probabilities
                if(nrow(bigrams) > 0) {
                        denom <- bigrams[, .(sum(count))][1, 1]
                        bigrams <- bigrams[, prob := (count / denom[[1]] * .2)]
                        # Note observed bigram tail words
                        observedTails <- c(observedTails, bigrams$tail)
                }
                
                # Back off to unigrams
                unigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                       count = vector(mode = "integer", length = 0),
                                       tail = vector(mode = "character", length = 0))
                
                # Get unigrams which were not observed as tail words for any observed ngram
                unigrams <- ngrams[n == 1][!(tail %in% observedTails)]
                
                # Get the total unigram count
                denom <- ngrams[n == 1][, .(sum(count))][1, 1]
                
                # Calculate MLEs
                if(denom > 0) {
                        unigrams <- unigrams[, prob := (count / denom[[1]]) * .1]
                }
                
                # Put all the MLEs into a single table
                MLEs <- rbind(sixgrams, fivegrams, quadgrams, trigrams, bigrams, unigrams, fill = TRUE)
                MLEs <- MLEs[order(-MLEs$prob), ]
                
                # Put top 3 predictions in a character string
                output <- MLEs[1:10, ]
                
                # Predict next word
                # paste(output[1:3, tail])

        })
        
        # Predict next word
        output$prediction <- renderText({
                wordPrediction()[1:3, tail]

        })
        
        # Plot top words
        output$plot <- renderPlot({
                
                ggplot(data = wordPrediction(), aes(tail, prob)) + labs(x = "Word", y = "Score") + geom_col(fill = "cornflowerblue") + coord_flip()
                
        })

})


