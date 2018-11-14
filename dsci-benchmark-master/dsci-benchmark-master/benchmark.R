require(digest)
require(stringi)
require(data.table)
require(hunspell)

################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################

# 01a. Load ngrams 
################################################################################################

# Open data files and index
unigrams <- readRDS("unigrams.rds")
unigrams <- data.table(unigrams)
setkey(unigrams, ngram)

bigrams <- readRDS("bigrams.rds")
bigrams <- data.table(bigrams)
setkey(bigrams, ngram)

trigrams <- readRDS("trigrams.rds")
trigrams <- data.table(trigrams)
setkey(trigrams, ngram)

quadgrams <- readRDS("quadgrams.rds")
quadgrams <- data.table(quadgrams)
setkey(quadgrams, ngram)

# Reduce table sizes by removing rows with word(s) not in the dictionary
unigrams <- unigrams[hunspell_check(unigrams$ngram), ]
bigrams <- bigrams[hunspell_check(sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 1)) &
                           hunspell_check(sapply(strsplit(bigrams$ngram, " ", fixed = TRUE), '[[', 2)), ]
trigrams <- trigrams[hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 1)) &
                             hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 2)) &
                             hunspell_check(sapply(strsplit(trigrams$ngram, " ", fixed = TRUE), '[[', 3)), ]
quadgrams <- quadgrams[hunspell_check(sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 1)) &
                               hunspell_check(sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 2)) &
                               hunspell_check(sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 3)) &
                               hunspell_check(sapply(strsplit(quadgrams$ngram, " ", fixed = TRUE), '[[', 4))  , ]

# Generate samples
# Define a function to extract a random sample of 30% of each file and store in a file
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
sampleFile(t, "tweets.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/twitter.txt", "r")
twitter <- readLines(con) 
close(con) 

# Extract blogs sample
b <- "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt"
sampleFile(b, "blogs.txt", header = FALSE)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/blogs.txt", "r")
blogs <- readLines(con) 
close(con) 


# 01b. Get text from randomly selected tweets
################################################################################################

tweets <- readLines('tweets.txt', encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"


# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
blogs <- readLines('blogs.txt', encoding = 'UTF-8')

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
               max.score <-0
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
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
predict.baseline <- function(inputString, unigrams, bigrams, trigrams, quadgrams){
        
        # Ensure libraries are loaded
        require(dplyr)
        require(tidytext)
        require(data.table)
        require(sqldf)
        
        # Set discounts
        gamma2 <- .5 # bigram discount
        gamma3 <- .7 # trigram discount
        gamma4 <- .8 # quadgram discount
        
        # Find OBSERVED quadgrams and the counts
        getObservedQuadgrams <- function(inputString, inputQuadgrams) {
                quadgramsFound <- data.table(ngram = vector(mode = "character", length = 0),
                                             count = vector(mode = "integer", length = 0),
                                             tail = vector(mode = "character", length = 0))
                quadgramsFound <- sqldf(sprintf("SELECT * FROM inputQuadgrams WHERE ngram LIKE '%s%s'",
                                                inputString, "%"))
                
                return(quadgramsFound)
        }
        
        observedQuadgrams <- getObservedQuadgrams(inputString, quadgrams)
        
        # Calculate the probabilities of observed quadgrams beginning with inputText
        getObservedQuadgramProbs <- function(observedQuadgrams, inputTrigrams, inputString, quadgramDisc = .5) {
                if(nrow(observedQuadgrams) < 1) return(NULL)
                obsCount <- sqldf(sprintf("SELECT count FROM inputTrigrams WHERE ngram = '%s'", inputString))$count
                obsQuadgramProbs <- mutate(observedQuadgrams, prob = ((count - quadgramDisc) / obsCount))
                
                return(obsQuadgramProbs)
        }
        
        qObservedQuadgrams <- getObservedQuadgramProbs(observedQuadgrams, trigrams, inputString, gamma4)
        
        # Calculate discounted probability mass for quadgrams to redistribute to trigrams
        getAlphaQuadgram <- function(observedQuadgrams, trigram, quadgramDisc = .5) {
                if(nrow(observedQuadgrams) < 1) return(1)
                alphaQuadgram <- 1 - sum((observedQuadgrams$count - quadgramDisc) / trigram$count[1])
                
                return(alphaQuadgram)
        }
        
        trigram <- sqldf(sprintf("SELECT * FROM trigrams WHERE ngram = '%s'", inputString))
        alphaQuadgram <- getAlphaQuadgram(observedQuadgrams, trigram, gamma4)
        
        # Find the tail (last) words of UNOBSERVED quadgrams that start with the first 3 words of observedQuadgrams
        getUnobservedQuadgramTails <- function(observedQuadgrams, inputUnigrams) {
                observedQuadgramTails <- observedQuadgrams$tail
                unobservedQuadgramTails <- inputUnigrams[!(inputUnigrams$ngram %in% observedQuadgramTails)]$ngram
                return(unobservedQuadgramTails)
        }
        
        unobservedQuadgramTails <- getUnobservedQuadgramTails(observedQuadgrams, unigrams)
        
        # Get backed off trigrams
        getBoTrigrams <- function(inputString, unobservedQuadgramTails) {
                w_i_minus2 <- strsplit(inputString, " ")[[1]][2] 
                w_i_minus1 <- strsplit(inputString, " ")[[1]][3]
                boTrigrams <- paste(w_i_minus2, w_i_minus1, unobservedQuadgramTails, sep = " ")
                
                return(boTrigrams)
        }
        
        boTrigrams <- getBoTrigrams(inputString, unobservedQuadgramTails)
        
        # Find OBSERVED trigrams from BO trigrams
        getObservedBoTrigrams <- function(boTrigrams, inputString, unobservedQuadgramTails, inputTrigrams) {
                observedBoTrigrams <- inputTrigrams[inputTrigrams$ngram %in% boTrigrams, ]
                
                return(observedBoTrigrams)
        }
        
        observedBoTrigrams <- getObservedBoTrigrams(boTrigrams, inputString, unobservedQuadgramTails, trigrams)
        
        # Calculate the probabilities of observed BO trigrams
        getObservedBoTrigramProbs <- function(observedBoTrigrams, inputBigrams, inputString, trigramDisc = .5) {
                if(nrow(observedBoTrigrams) < 1) return(NULL)
                w_i_minus2 <- strsplit(inputString, " ")[[1]][2] 
                w_i_minus1 <- strsplit(inputString, " ")[[1]][3]
                bigram <- paste(w_i_minus2, w_i_minus1, sep = " ")
                obsCount <- sqldf(sprintf("SELECT count FROM inputBigrams WHERE ngram = '%s'", bigram))$count
                obsTrigramProbs <- mutate(observedBoTrigrams, prob = ((count - trigramDisc) / obsCount))
                
                return(obsTrigramProbs)
        }
        
        qObservedBoTrigrams <- getObservedBoTrigramProbs(observedBoTrigrams, bigrams, inputString, gamma3)
        
        # Calculate the discount mass probability to redistribute to bigrams
        getAlphaTrigram <- function(observedBoTrigrams, bigram, trigramDisc = .5) {
                if(nrow(observedBoTrigrams) < 1) return(1)
                alpha <- 1 - sum((observedBoTrigrams$count - trigramDisc) / bigram$count[1])
                
                return(alpha)
        }
        
        bigram <- sqldf(sprintf("SELECT * FROM bigrams WHERE ngram = '%s%s%s'", 
                                strsplit(inputString, " ")[[1]][2], " ", strsplit(inputString, " ")[[1]][3]))
        alphaTrigram <- getAlphaTrigram(observedBoTrigrams, bigram, gamma3)
        
        # Find UNOBSERVED trigrams from BO trigrams
        getUnobservedBoTrigrams <- function(boTrigrams, inputString, observedBoTrigrams) {
                # Put BO trigrams in a data table
                boTrigrams <- data.table(boTrigrams, ngram = boTrigrams)
                names(boTrigrams) <- "ngram"
                # Find unobserved trigrams
                unobservedTrigrams <- boTrigrams[!(boTrigrams$ngram %in% observedBoTrigrams$ngram), ]
                return(unobservedTrigrams)
        }
        
        # Find the tail words of UNOBSERVED trigrams that start with the first 2 words of observedTrigrams
        getUnobservedBoTrigramTails <- function(observedBoTrigrams, unobservedQuadgramTails) {
                observedTrigramTails <- observedBoTrigrams$tail
                unobservedQuadgramTails <- data.table(unobservedQuadgramTails)
                names(unobservedQuadgramTails) <- "tail"
                unobservedTrigramTails <- unobservedQuadgramTails[!(unobservedQuadgramTails$tail %in% observedTrigramTails), ]
                
                return(unobservedTrigramTails)
        }
        
        unobservedBoTrigramTails <- getUnobservedBoTrigramTails(observedBoTrigrams, unobservedQuadgramTails)
        
        # Get backed off bigrams
        getBoBigrams <- function(inputString, unobservedTrigramTails) {
                w_i_minus1 <- strsplit(inputString, " ")[[1]][3]
                boBigrams <- paste(w_i_minus1, unobservedTrigramTails$tail, sep = " ")
                
                return(boBigrams)
        }
        
        boBigrams <- getBoBigrams(inputString, unobservedBoTrigramTails)
        
        # Get OBSERVED bigrams from the set of BO bigrams
        getObservedBoBigrams <- function(boBigrams, inputBigrams) {
                # Put BO bigrams in a data table
                boBigrams <- data.table(boBigrams)
                names(boBigrams) <- "ngram"
                # Find observed BO bigrams
                observedBoBigrams <- inputBigrams[inputBigrams$ngram %in% boBigrams$ngram, ]
                return(observedBoBigrams)
        }
        
        observedBoBigrams <- getObservedBoBigrams(boBigrams, bigrams)
        
        # Calculate probabilities for OBSERVED BO bigrams
        # q_bo(w1 | w2)
        getObservedBoBigramProbs <- function(observedBoBigrams, inputUnigrams, bigramDisc = .5) {
                if(nrow(observedBoBigrams) < 1) return(NULL)
                # Get the first word of the bigram
                word <- strsplit(observedBoBigrams$ngram, " ")[[1]][1]
                # word <- observedBoBigrams$word1[1]
                wordCount <- inputUnigrams[inputUnigrams$ngram == word]
                observedBigramProbs <- (observedBoBigrams$count - bigramDisc) / wordCount$count
                observedBigramProbs <- data.table(ngram = observedBoBigrams$ngram, 
                                                  prob = observedBigramProbs)
                
                return(observedBigramProbs)
        }
        
        qObservedBoBigrams <- getObservedBoBigramProbs(observedBoBigrams, unigrams, gamma2)
        
        # Calculate the discount mass probability for bigrams
        # alpha for observed bigrams = 1 - sum_of((observed_bigram_count - discount) / unigram_count)
        getAlphaBigram <- function(observedBoBigrams, unigram, bigramDisc = .5) {
                if(nrow(observedBoBigrams) < 1) return(1)
                alpha <- 1 - sum((observedBoBigrams$count - bigramDisc) / unigram$count[1])
                
                return(alpha)
        }
        
        unigram <- unigrams[unigrams$ngram == strsplit(inputText, " ")[[1]][3]]
        alphaBigram <- getAlphaBigram(observedBoBigrams, unigram, gamma2)
        
        # Get UNOBSERVED bigrams from the set of BO bigrams
        getUnobservedBoBigrams <- function(boBigrams, observedBoBigrams) {
                # Put BO bigrams in a data table
                boBigrams <- data.table(boBigrams)
                names(boBigrams) <- "ngram"
                # Find unobserved BO bigrams
                unobservedBigrams <- boBigrams[!(boBigrams$ngram %in% observedBoBigrams$ngram), ]
                
                return(unobservedBigrams)
        }
        
        unobservedBoBigrams <- getUnobservedBoBigrams(boBigrams, observedBoBigrams)
        
        # Calculate probabilities for UNOBSERVED BO bigrams
        # q_bo(w1 | w2)
        getUnobservedBoBigramProbs <- function(unobservedBoBigrams, inputUnigrams, alphaBigram) {
                # get unobserved bigram tails
                unobsBigrams <- vector(mode = "character")
                for(i in 1:length(unobservedBoBigrams$ngram)) {
                        tailWord <- strsplit(unobservedBoBigrams$ngram[i], " ")[[1]][2]
                        unobsBigrams[i] <- tailWord
                }
                w_in_Aw_iminus1 <- inputUnigrams[!(inputUnigrams$ngram %in% unobsBigrams), ]
                # convert to data table with counts
                qUnobservedBoBigrams <- inputUnigrams[inputUnigrams$ngram %in% unobsBigrams]
                denom <- sum(qUnobservedBoBigrams$count)
                # convert counts to probabilities
                qUnobservedBoBigrams <- data.table(ngram = unobservedBoBigrams$ngram, 
                                                   prob = (alphaBigram * qUnobservedBoBigrams$count / denom))
                
                return(qUnobservedBoBigrams)
        }
        
        qUnobservedBoBigrams <- getUnobservedBoBigramProbs(unobservedBoBigrams, unigrams, alphaBigram)
        
        # Calculate UNOBSERVED trigram probabilities
        getUnobservedBoTrigramProbs <- function(inputString, qObservedBoBigrams, qUnobservedBoBigrams, alphaTrigram) {
                qBoBigrams <- rbind(qObservedBoBigrams, qUnobservedBoBigrams)
                qBoBigrams <- qBoBigrams[order(-qBoBigrams$prob), ]
                sumqBoBigrams <- sum(qBoBigrams$prob)
                # Get the first word of the trigram, which is the second word of the input string
                word1 <- strsplit(inputString, " ")[[1]][2]
                unobservedTrigramNgrams <- paste(word1, qBoBigrams$ngram, sep = " ")
                # Calculate probabilities
                unobservedTrigramProbs <- alphaTrigram * qBoBigrams$prob / sumqBoBigrams
                unobservedTrigramsDT <- data.table(ngram = unobservedTrigramNgrams, 
                                                   prob = unobservedTrigramProbs)
                
                return(unobservedTrigramsDT)
        }
        
        qUnobservedBoTrigrams <- getUnobservedBoTrigramProbs(inputString, qObservedBoBigrams, qUnobservedBoBigrams, alphaTrigram)
        
        # Calculate UNOBSERVED quadgram probabilities
        getUnobservedQuadgramProbs <- function(inputString, qObservedBoTrigrams, qUnobservedBoTrigrams, alphaQuadgram) {
                qBoTrigrams <- rbind(qObservedBoTrigrams, qUnobservedBoTrigrams, fill = TRUE)
                qBoTrigrams <- qBoTrigrams[order(-qBoTrigrams$prob), ]
                sumqBoTrigrams <- sum(qBoTrigrams$prob)
                # Get the first word of the quadgram, which is the first word of the input string
                word1 <- strsplit(inputString, " ")[[1]][1]
                unobservedQuadgramNgrams <- paste(word1, qBoTrigrams$ngram, sep = " ")
                # Get probabilities
                unobservedQuadgramProbs <- alphaQuadgram * qBoTrigrams$prob / sumqBoTrigrams
                unobservedQuadgramsDT <- data.table(ngram = unobservedQuadgramNgrams, 
                                                    prob = unobservedQuadgramProbs)
                
                return(unobservedQuadgramsDT)
        }
        
        qUnobservedQuadgrams <- getUnobservedQuadgramProbs(inputString, qObservedBoTrigrams, qUnobservedBoTrigrams, alphaQuadgram)
        
        
        qQuadgrams <- rbind(qObservedQuadgrams, qUnobservedQuadgrams, fill = TRUE)
        qQuadgrams <- qQuadgrams[order(-qQuadgrams$prob), ]
        
        # Extract tail words of top 3 ngrams to return
        prediction <- qQuadgrams$ngram[1:3]
        prediction <- strsplit(prediction, " ")[[1]][4] 
        
        return(prediction)
        
        
}


################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          inputUnigrams = unigrams,
          inputBigrams = bigrams,
          inputTrigrams = trigrams,
          inputQuadgrams = quadgrams,
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
