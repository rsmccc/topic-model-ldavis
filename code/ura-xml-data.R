# XML Data Validation 10-6-2015

setwd("~/Development/R/ura")
library(XML)
input.dir <- "data/TestWeek"

## Create vector of filenames within input directory
files.v <- dir(path = input.dir, pattern = "*.xml")

## Mallet
library(mallet)

documents <- mallet.read.dir(input.dir)

mallet.instances <- mallet.import(documents$id, documents$text, "data/stoplist1.csv", FALSE, token.regexp="[\\p{L}']+")
## Java object, mallet instances list

## create topic trainer object.
n.topics <- 25
topic.model <- MalletLDA(num.topics = n.topics)

topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
vocabulary

word.freqs <- mallet.word.freqs(topic.model)
word.freqs
# term.freq is freq of word in corpus, doc.freq is how many documents contain word at least once

topic.model$train(800) ## only took 4 seconds, GPA used 2000

topic.words.m <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE) ## topics by words with distribution of terms for ea. topic
## set normalized = TRUE to get proportions of each word in topic, FALSE is counts
dim(topic.words.m)
rowSums(topic.words.m)

# colnames(topic.words.m) <- vocabulary

## randomly selected row
# mallet.top.words(topic.model, topic.words.m[15,], 10)

#####



doc.topics.m <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE) ## probability of each topic appearing in each document
doc.topics.m.counts <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = FALSE)