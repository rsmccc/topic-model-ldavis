# URA Topic Model with LDAvis, Riley McCloskey (RSM)

setwd("~/Development/R/ura")
library(XML)
input.dir <- "data/ALL39IN"

## Create vector of filenames within input directory
files.v <- dir(path = input.dir, pattern = "*.xml")

chunk.size <- 500 ## number of words per chunk

## Load makeFlexTextChunks function
source("code/corpusFunctions1.R")

## Loop through documents and chunk them up
topic.m <- NULL
for (i in 1:length(files.v)) {
    doc.object<-xmlTreeParse(file.path(input.dir, files.v[i]),
                             useInternalNodes=TRUE)
    chunk.df<-makeFlexTextChunks(doc.object, chunk.size,
                                 percentage=FALSE)
    textname<-gsub("\\..*","", files.v[i])
    segments.m<-cbind(paste(textname, i, segment=1:nrow(chunk.df), sep="_"), chunk.df)
    topic.m<-rbind(topic.m, segments.m)
}

## length(files.v) is 210 so num documents is 210 but topic.m gets 278 ????

documents <- as.data.frame(topic.m, stringsAsFactors = F)
colnames(documents) <- c("id", "text")

## Why 
#### ERROR: in documents, duplicate row.names ???

## Part 2 - Mallet

library(mallet)

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
docs.length <- rowSums(doc.topics.m.counts)

dim(doc.topics.m)

file.ids.v <- documents[,1]

length(unique(documents$id))
length(documents$id) ## NO DUPLICATES

file.id.l<-strsplit(file.ids.v, "_")
file.chunk.id.l<-lapply(file.id.l, rbind)
file.chunk.id.m<-do.call(rbind, file.chunk.id.l)

doc.topics.df <- as.data.frame(doc.topics.m)

doc.topics.df<-cbind(file.chunk.id.m[,1], doc.topics.df)
doc.topic.means.df<-aggregate(doc.topics.df[, 2:ncol(doc.topics.df)],
                              list(doc.topics.df[,1]), mean)

# phi <- topic.words.m
# theta <- doc.topics.m
# doc.length <- docs.length
# vocab <- vocabulary
# term.frequency <- word.freqs$term.freq 

## Why is docs.length not an integer?? 
## Why does doc.topics.df have 278 rows when there are only 210 documents?

library(LDAvis)

json <- createJSON(phi = topic.words.m, theta = doc.topics.m, doc.length = docs.length, vocab = vocabulary, term.frequency = word.freqs$term.freq)
serVis(json)


library(tm)
??tm












