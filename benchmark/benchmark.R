require(digest)
require(stringi)
require(data.table)
require(quanteda)
require(stringr)

# https://www.coursera.org/learn/data-science-project/discussions/weeks/3/threads/fG-14OvXEeWTdBIkpCpI1Q

setwd("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/benchmark")
ngrams <- readRDS("./data/ngramsDT.rds")
setkey(ngrams, ngram)

################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################


# 01b. Get text from randomly selected tweets
################################################################################################

t <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/benchmark/data/tweets.txt"
tweets <- readLines(t, encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"


# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/benchmark/data/blogs.txt"
blogs <- readLines(b, encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(blogs, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2"



################################################################################################
#
# 02. Define the functions used for benchmarking
#
################################################################################################

# 02a. Pre-processing functions
################################################################################################

# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
    require(stringi)
    # append a space to the sentence (to make sure we always create one result with only the 
    # last word missing)
    sent <- paste0(line, ' ')

    sep <- stri_locate_all_regex(line, 
                                 pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                 omit_empty=T, 
                                 case_insensitive=T)[[1]]
    sapply(seq_len(nrow(sep)), 
           function(i) {
               c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                    nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
               )
               })
}, options=list(optimize=3))


# 02b. Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.
benchmark <- compiler::cmpfun(function(FUN, ..., sent.list, ext.output=T) {
    require(stringi)
    require(digest)
    require(data.table)
    
    result <- rbindlist(lapply(names(sent.list), 
           function(list.name) {  
               sentences <- sent.list[[list.name]]
               
               score <- 0
               max.score <- 0
               hit.count.top3 <- 0
               hit.count.top1 <- 0
               total.count <- 0
               time <- system.time({
                   for (sent in sentences) {
                       split <- split.sentence(sent[1])
                       max.score <- max.score + ncol(split)*3
                       total.count <- total.count + ncol(split)
                       rank <- sapply(seq_len(ncol(split)),
                                      function(i) {
                                          min(which(FUN(split[1,i], ...)==split[2,i]),4)
                                      })
                       score <- score + sum(4-rank)
                       hit.count.top3 <- hit.count.top3 + sum(rank<4)
                       hit.count.top1 <- hit.count.top1 + sum(rank==1)
                   }
               })
               
               list('list.name' = list.name,
                    'line.count' = length(sentences),
                    'word.count' = sum(stri_count_words(sentences)),
                    'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                    'score' = score,
                    'max.score' = max.score,
                    'hit.count.top3' = hit.count.top3,
                    'hit.count.top1' = hit.count.top1,
                    'total.count' = total.count,
                    'total.runtime' = time[3]
               )               
           }), use.names=T)
    
    setkey(result, list.name)
    
    # The overall scores are calculated weighting each data set equally (independent of the 
    # number of lines in each dataset).
    overall.score.percent = 100 * result[,sum(score/max.score)/.N]
    overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
    overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
    average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
    number.of.predictions = result[,sum(total.count)]
    total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                       function(x) {
                                           object.size(get(x,
                                                           envir = .GlobalEnv,
                                                           inherits = FALSE))
                                           })))/(1024^2)
    cat(sprintf(paste0('Overall top-3 score:     %.2f %%\n',
                       'Overall top-1 precision: %.2f %%\n',
                       'Overall top-3 precision: %.2f %%\n',
                       'Average runtime:         %.2f msec\n',
                       'Number of predictions:   %d\n',
                       'Total memory used:       %.2f MB\n'),
                overall.score.percent,
                overall.precision.top1,
                overall.precision.top3,
                average.runtime,
                number.of.predictions,
                total.mem.used
                ))
    
    cat('\nDataset details\n')
    for (p.list.name in result$list.name) {
        res <- result[list(p.list.name)]
        cat(sprintf(paste0(' Dataset "%s" (%d lines, %d words, hash %s)\n',
                           '  Score: %.2f %%, Top-1 precision: %.2f %%, Top-3 precision: %.2f %%\n'
                           ),
                    p.list.name,
                    res$line.count,
                    res$word.count,
                    res$hash,
                    100 * res$score/res$max.score,
                    100 * res$hit.count.top1/res$total.count,
                    100 * res$hit.count.top3/res$total.count
        ))
    }
    
    if (ext.output==T) {
        packages <- sort(stri_replace_first_fixed(search()[stri_detect_regex(search(), 
                                                                             '^package:')], 
                                                  'package:', ''))
        
        cat(sprintf(paste0('\n\n%s, platform %s\n', 
                           'Attached non-base packages:   %s\n',
                           'Unattached non-base packages: %s'
                           ),
                   sessionInfo()$R.version$version.string,
                   sessionInfo()$platform,
                   paste0(sapply(sessionInfo()$otherPkgs, 
                                 function(pkg) {
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', '),
                   paste0(sapply(sessionInfo()$loadedOnly, 
                                 function(pkg) { 
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', ')
                   ))
    }
}, options=list(optimize =3))




################################################################################################
#
# 03a. Define the KATZ BACKOFF wrapper function to be called by benchmark
#
################################################################################################

myPredictionFunction <- compiler::cmpfun(function(x){
        
        # Set absoluate discount
        disc <- 0.75
        
        # Create regex to remove URLs, twitter user names, hashtags, possessives and unicode / html tags
        stuff_to_remove <- c("http[s]?://[[:alnum:].\\/]+", "@[\\w]*", "#[\\w]*", "<.*>", "'s")
        stuff_to_remove <-  paste(stuff_to_remove, sep = "|", collapse="|")
        
        # Clean tweets
        clean_x <- str_replace_all(x, stuff_to_remove, "")
        
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
        alphaSixgrams <- 1
        
        # Calculate the aggregate sixgram count and probabilities
        if(nrow(sixgrams) > 0) {
                denom <- sixgrams[, .(sum(count))][1, 1]
                sixgrams <- sixgrams[, prob := ((count - disc[[1]]) / denom[[1]])]
                # Calculate alpha for sixgrams
                #alphaSixgrams <- 1 - sixgrams[, .(sum(prob))][1, 1]
                alphaSixgrams <- (nrow(sixgrams) * disc[[1]]) / denom[[1]]
                # Note observed tailwords
                observedTails <- sixgrams$tail
        }
        
        # Back off to fivegrams
        alphaFivegrams <- 1
        
        fivegrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        fivegrams <- ngrams[ngram == quadgram][!(tail %in% observedTails)]
        
        # Calculate the aggregate fivegram count and probabilities
        if(nrow(fivegrams) > 0) {
                denom <- fivegrams[, .(sum(count))][1, 1]
                fivegrams <- fivegrams[, prob := alphaSixgrams[[1]] * ((count - disc[[1]]) / denom[[1]])]
                # Calculate alpha for fivegrams
                #alphaFivegrams <- alphaSixgrams[[1]] - fivegrams[, .(sum(prob))][1, 1]
                alphaFivegrams <- (nrow(fivegrams) * alphaSixgrams[[1]] * disc[[1]]) / denom[[1]]
                # Note observed fivegram tail words
                observedTails <- c(observedTails, fivegrams$tail)
        }
        
        # Back off to quadgrams
        alphaQuadgrams <- 1
        
        quadgrams <- data.table(ngram = vector(mode = "character", length = 0),
                                count = vector(mode = "integer", length = 0),
                                tail = vector(mode = "character", length = 0))
        quadgrams <- ngrams[ngram == trigram][!(tail %in% observedTails)]
        
        # Calculate the aggregate quadgram count and probabilities
        if(nrow(quadgrams) > 0) {
                denom <- quadgrams[, .(sum(count))][1, 1]
                quadgrams <- quadgrams[, prob := alphaFivegrams[[1]] * ((count - disc[[1]]) / denom[[1]])]
                # Calculate alpha for quadgrams
                #alphaQuadgrams <- alphaFivegrams[[1]] - quadgrams[, .(sum(prob))][1, 1]
                alphaQuadgrams <- (nrow(quadgrams) * alphaFivegrams[[1]] * disc[[1]]) / denom[[1]]
                # Note observed quadgram tail words
                observedTails <- c(observedTails, quadgrams$tail)
        }
        
        # Back off to trigrams
        alphaTrigrams <- 1
        
        trigrams <- data.table(ngram = vector(mode = "character", length = 0),
                               count = vector(mode = "integer", length = 0),
                               tail = vector(mode = "character", length = 0))
        trigrams <- ngrams[ngram == bigram][!(tail %in% observedTails)]
        
        # Calculate the aggregate trigram count and probabilities
        if(nrow(trigrams) > 0) {
                denom <- trigrams[, .(sum(count))][1, 1]
                trigrams <- trigrams[, prob := alphaQuadgrams[[1]] * ((count - disc[[1]]) / denom[[1]])]
                # Calculate alpha for trigrams
                #alphaTrigrams <- alphaQuadgrams[[1]] - trigrams[, .(sum(prob))][1, 1]
                alphaTrigrams <- (nrow(trigrams) * alphaQuadgrams[[1]] * disc[[1]]) / denom[[1]]
                # Note observed trigram tail words
                observedTails <- c(observedTails, trigrams$tail)
        }
        
        # Back off to bigrams
        alphaBigrams <- 1
        
        bigrams <- data.table(ngram = vector(mode = "character", length = 0),
                              count = vector(mode = "integer", length = 0),
                              tail = vector(mode = "character", length = 0))
        bigrams <- ngrams[ngram == unigram][n == 2][!(tail %in% observedTails)]
        
        # Calculate the aggregate bigram count and probabilities
        if(nrow(bigrams) > 0) {
                denom <- bigrams[, .(sum(count))][1, 1]
                bigrams <- bigrams[, prob := alphaTrigrams[[1]] * ((count - disc[[1]]) / denom[[1]])]
                # Calculate alpha for bigrams
                #alphaBigrams <- alphaTrigrams[[1]] - bigrams[, .(sum(prob))][1, 1]
                alphaBigrams <- (nrow(bigrams) * alphaBigrams[[1]] * disc[[1]]) / denom[[1]]
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
                unigrams <- unigrams[, prob := alphaBigrams[[1]] * (count / denom[[1]])]
        }
        
        # Put all the MLEs into a single table
        MLEs <- rbind(sixgrams, fivegrams, quadgrams, trigrams, bigrams, unigrams, fill = TRUE)
        MLEs <- MLEs[order(-MLEs$prob), ]
        
        # Put top 3 predictions in a character string
        output <- MLEs[1:3, ]$tail
        
        return(output)
        
})


################################################################################################
#
# 03b. Define the STUPID BACKOFF wrapper function to be called by benchmark
#
################################################################################################


myPredictionFunction <- compiler::cmpfun(function(x){

        # Create regex to remove URLs, twitter user names, hashtags, possessives and unicode / html tags
        stuff_to_remove <- c("http[s]?://[[:alnum:].\\/]+", "@[\\w]*", "#[\\w]*", "<.*>", "'s")
        stuff_to_remove <-  paste(stuff_to_remove, sep = "|", collapse="|")
        
        # Clean tweets
        clean_x <- str_replace_all(x, stuff_to_remove, "")
        
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
        output <- MLEs[1:3, ]$tail
        
        return(output)
        
})



################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(myPredictionFunction,
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
