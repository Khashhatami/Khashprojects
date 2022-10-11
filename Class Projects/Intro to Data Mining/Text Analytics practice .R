# ----------- Chapter 14:  Chapter 14: Word Perfect -----------

library(XML)
library(tm)




#Different ways to read the read the speech 
#in all these cases sba.txt is locally saved on my computer

#use scan to read the file
sba <- scan("sba.txt", character(0),sep = "\n")
head(sba,3)

#Alternatively we could have used readLines() function for the same speech in this way
#use readLines

#sba <- readLines("sba.txt")
#head(sba, 3)

#Use a web file to the read the file: Note the web location for the speech
#sbaLocation <- URLencode("http://www.historyplace.com/speeches/anthony.htm")

# Read and parse HTML file
#doc.html = htmlTreeParse(sbaLocation, useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
#sba = unlist(xpathApply(doc.html, '//p', xmlValue))
#head(sba, 3)

words.vec <- VectorSource(sba)
words.corpus <- Corpus(words.vec)
words.corpus
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
tdm

#the function inspect() shows us how often each paticular term that is interesting (none stopwords)
#repeated in each of one of our documents. Note  that this corpus has 9 documents. what does that mean?
#well it means it has absorbed 9 lines (remember that we took in sba.txt and we set the seperator to
#\n which means every document here is a line)

inspect(tdm)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

library(wordcloud)

#Cloud.frame is a data frame that has terms as its rows ans the freq as its column 
cloudFrame <- data.frame(word = names(wordCounts), freq=wordCounts)

wordcloud(cloudFrame$word, cloudFrame$freq)

#the argument min.freq determines that only terms that have been repeated across all documents for at least 2 times
#to be included in our word cloud
wordcloud(names(wordCounts), wordCounts, min.freq=2, max.words=50, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# ------------------ Chapter 15: Happy Words? ------------------

#after downloading our dictionary, we should have two text file one for positive words and the other 
#for negative words


lexicon_bing()
lexicon_bing(dir="data/")
#then in console, select 1

pos <- "C:\\Users\\wecan\\OneDrive\\Documents\\bing\\positive-words.txt"
neg <- "C:\\Users\\wecan\\OneDrive\\Documents\\bing\\negative-words.txt"

#now we have read both the positive and the negative words, we should put them in vectors
p <- scan(pos, character(0),sep = "\n")
n <- scan(neg, character(0),sep = "\n")

#one we print out the head of both vectors, we notice that roughly the first 30 elements of our 
#vectors are infact extra information which are not useful for our code, we are just going to take them
#out
head(p, 50)
head(n, 50)

p <- p[-1:-34]
n <- n[-1:-34]
head(p, 10)
head(n, 10)

#totalWords is the sum of the all the terms used in sba spa.txt in the order of their frequency
totalWords <- sum(wordCounts)

#words is the actual terms in the speech which are aquired by using the names function
words <- names(wordCounts)

#now we use the match function to determine how many of the words used in sba.txt are are found
#within the positive words dictionary. the ones that there are no match for (nuetral words for instance)
#are simply designated to be 0

matched <- match(words, p, nomatch = 0) 

#Now we have a vector of all positive words from our dictionary that are matched with positive words
#with our sba.txt speech

head(matched,10)
# output:
# [1]    0    0    0    0    0    0    0 1083    0    0
# we see that there is a match with the 8th word in the speech which is 1083 word in 
#our dictionary which words is it?
matched[8]
p[1083]
#[1] "liberty"
words[8]
#[1] "liberty"

#it seems like the 8th word in the specch and the 1083th word in our dictionary is "liberty"!

#mCounts finds which words in matched vector are not 0 (in other words which words were matched from 
#our dictionary to our speech)
#mCounts vector therefore includes only words that were matched

mCounts <- wordCounts[which(matched != 0)]

#let's see which words they are
mCounts

#  liberty     right    secure    lawful      work   perfect enjoyment      well   supreme  educated 
#4         2         2         1         1         1         1         1         1         1 
#rich precisely 
#1         1 

#length counts the number of unique words in this match
length(mCounts)
#[1] 12

#now let's see how many positive words are there in total
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos
#[1] 17

# we will do the same for our negative words:
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched != 0)]
nNeg <- sum(nCounts)
nWords <- names(nCounts)
nNeg
length(nCounts)

#here we modified the totalWords to be the length of all words in the speech
#"words" was the name of all the terms in wordCounts.
totalWords <- length(words)

#Now let's compare the ratio of the all positive words used in the speech against all the words in
#the pseech as well as the ratio of all negative words used in the speech against all the words in 
#the speech
ratioPos <- nPos/totalWords
ratioPos
ratioNeg <- nNeg/totalWords
ratioNeg

#> ratioPos <- nPos/totalWords
#> ratioPos
#[1] 0.08900524
#> ratioNeg <- nNeg/totalWords
#> ratioNeg
#[1] 0.06806283

#this tells us something about the 9% of words in the speech were positive and 7% wordsused in the speech were negative
