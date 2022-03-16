Happy_words.R
Cameron Willliams
2020-02-23
library(tm)
## Loading required package: NLP
library(XML)
library(rvest)
## Loading required package: xml2
## 
## Attaching package: 'rvest'
## The following object is masked from 'package:XML':
## 
##     xml
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
pos <- "positive-words.txt"
neg <- "negative-words.txt"

p <-scan(pos, character(0), sep = "\n")
n <- scan(neg, character(0), sep = "\n")

head(p,10)
##  [1] ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
##  [2] "; "                                                                          
##  [3] "; Opinion Lexicon: Positive"                                                 
##  [4] ";"                                                                           
##  [5] "; This file contains a list of POSITIVE opinion words (or sentiment words)." 
##  [6] ";"                                                                           
##  [7] "; This file and the papers can all be downloaded from "                      
##  [8] ";    http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html"                
##  [9] ";"                                                                           
## [10] "; If you use this list, please cite the following paper:"
head(n,10)
##  [1] ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
##  [2] "; "                                                                            
##  [3] "; Opinion Lexicon: Negative"                                                   
##  [4] ";"                                                                             
##  [5] "; This file contains a list of NEGATIVE opinion words (or sentiment words)."   
##  [6] ";"                                                                             
##  [7] "; This file and the papers can all be downloaded from "                        
##  [8] ";    http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html"                  
##  [9] ";"                                                                             
## [10] "; If you use this list, please cite the following paper:"
##================================================
doc.html = htmlTreeParse('http://www.analytictech.com/mb021/mlk.htm', useInternal = TRUE)
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text = gsub ('\\n',' ', doc.text)
doc.text = gsub('\\r', ' ', doc.text)

words.vec <- VectorSource(doc.text)
words.corpus <- Corpus(words.vec)
words.corpus
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 26
tdm <- TermDocumentMatrix(words.corpus)
m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)
##     the     and    will freedom     one    from 
##      54      27      16      13      12      12
##=================================================
totalWords <- sum(wordCounts)
words <- names(wordCounts)

matched <- match(words, p, nomatch = 0)
head(matched, 10)
##  [1]   0   0   0 795   0   0   0   0   0   0
matched[4]
## [1] 795
p[795]
## [1] "freedom"
words[9]
## [1] "with"
##=================================================
mCounts <- wordCounts[which(matched != 0)]
length(mCounts)
## [1] 26
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos
## [1] 45
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched != 0)]
nNeg <- sum(nCounts)
nWords <- names(nCounts)
nNeg
## [1] 23
length(nCounts)
## [1] 21
totalWords <- length(words)
ratioPos <- nPos/totalWords
ratioPos
## [1] 0.1355422
ratioNeg <- nNeg/totalWords
ratioNeg
## [1] 0.06927711
##=========================================================
cutpoint <- round(length(words.corpus)/4)

words.corpus1 <- words.corpus[1:cutpoint]
tdm1 <- TermDocumentMatrix(words.corpus1)
m1 <- as.matrix(tdm1)
wordCounts1 <- rowSums(m1)
wordCounts1 <- sort(wordCounts1, decreasing = TRUE)
first25 <- nPos/wordCounts1 
##=========================================================
cutpoint2 <- round(length(words.corpus)/4)

words.corpus2 <- words.corpus[2:cutpoint2]
tdm2 <- TermDocumentMatrix(words.corpus2)
m2 <- as.matrix(tdm2)
wordCounts2 <- rowSums(m2)
wordCounts2 <- sort(wordCounts2, decreasing = TRUE)
second25 <- nPos/wordCounts2

##============================================================
cutpoint3 <- round(length(words.corpus)/4)

words.corpus3 <- words.corpus[3:cutpoint3]
tdm3 <- TermDocumentMatrix(words.corpus3)
m3 <- as.matrix(tdm3)
wordCounts3 <- rowSums(m3)
wordCounts3 <- sort(wordCounts3, decreasing = TRUE)
##===========================================================
cutpoint4 <- round(length(words.corpus)/4)

words.corpus4 <- words.corpus[4:cutpoint4]
tdm4 <- TermDocumentMatrix(words.corpus4)
m4 <- as.matrix(tdm4)
wordCounts4 <- rowSums(m4)
wordCounts4 <- sort(wordCounts4, decreasing = TRUE)