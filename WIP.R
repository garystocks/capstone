# Working code for capstone project

# Load libraries
library(tm)

# Load the first 5 lines of en_US twitter data
con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
sample <- readLines(con, 5) 
close(con) 

# Extract random sample of data and store in a file
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
                if (rbinom(1, 1, prob = .5) == 1) {
                        numout <- numout + 1
                        writeLines(inrec, co)
                }
        }
}

# Example file to run the sampleFile function
infile <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")

lines <- length(readLines(infile))


# GETTING STARTED QUIZ

# In the en_US twitter data set, if you divide the number of lines where the word 
# "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) 
# occurs, about what do you get?

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
recnum = 0
numout = 0
while (TRUE) {
        inrec <- readLines(con, n = 1)
        if (length(inrec) == 0) { # end of file?
                close(con)
                return(numout)
        }
        recnum <- recnum + 1
        if (grepl("hate" ,inrec)) {
                numout <- numout + 1
        }
}
close(con)

# The one tweet in the en_US twitter data set that matches the word "biostats" 
# says what?

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")

recnum = 0
lineout = ""
while (TRUE) {
        inrec <- readLines(con, n = 1)
        if (length(inrec) == 0) { # end of file?
                close(con)
                return(lineout)
        }
        if (grepl("biostats" ,inrec)) {
                numout <- numout + 1
                lineout <- inrec
        }
}

close(con)

# How many tweets have the exact characters "A computer once beat me at chess, 
# but it was no match for me at kickboxing". 
# (I.e. the line matches those characters exactly.)

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")

recnum = 0
numout = 0 
while (TRUE) {
        inrec <- readLines(con, n = 1)
        if (length(inrec) == 0) { # end of file?
                close(con)
                return(numout)
        }
        if (grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$" ,inrec)) {
                numout <- numout + 1
        }
}

close(con)

# What is the length of the longest line seen in any of the three en_US data sets?

con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.twitter.txt", "r")
con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.blogs.txt", "r")
con <- file("D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US/en_US.news.txt", "r")

recnum = 0
numout = 0 
while (TRUE) {
        inrec <- readLines(con, n = 1)
        nChar <- nchar(inrec)
        if (length(inrec) == 0) { # end of file?
                close(con)
                return(numout)
        }
        if (nChar > numout) {
                numout <- nChar
        }
}

close(con)



# Import files

en_US <- Corpus(DirSource(directory = "D:/Users/gary.stocks/Desktop/Coursera/Course 10 Project/capstone/data/en_US"))
