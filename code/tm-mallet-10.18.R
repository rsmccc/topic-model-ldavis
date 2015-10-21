# URA Topic Model with LDAvis, Riley McCloskey (RSM)

setwd("~/Development/R/ura")
library(XML)
input.dir <- "data/ALL39IN"

files.v <- dir(path = input.dir, pattern = "*.xml")

source("code/corpusFunctions1.R")

# Clean documents and import as text array and id array d.f.

getDocumentTokensAsVector <- function(doc.object) {
    paras<-getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
    words<-paste(sapply(paras,xmlValue), collapse=" ")
    words.lower<-tolower(words)
    words.lower<-gsub("[^[:alnum:][:space:]']", " ", words.lower)
    words.l<-strsplit(words.lower, "\\s+")
    word.v<-unlist(words.l)
    return (word.v)
}

getNumTokensForDoc <- function(doc.object) {
    word.v.l <- getDocumentTokensAsVector(doc.object)
    return (length(word.v.l))
}

getCleanDocument <- function(doc.object) {
    paras<-getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
    words<-paste(sapply(paras,xmlValue), collapse=" ")
    words.lower<-tolower(words)
    words.lower<-gsub("[^[:alnum:][:space:]']", " ", words.lower)
    return (words.lower)
}

docs.length <- NULL
topic.m <- NULL
for (i in 1:length(files.v)) {
    doc.object<-xmlTreeParse(file.path(input.dir, files.v[i]),
                             useInternalNodes=TRUE)
    #chunk.df<-makeFlexTextChunks(doc.object, 1, percentage=TRUE)
    chunk.df <- getCleanDocument(doc.object)
    textname<-gsub("\\..*","", files.v[i])
    segments.m<-cbind(paste(textname, i, sep="_"), chunk.df)
    topic.m<-rbind(topic.m, segments.m)
    docs.length <- rbind(docs.length, getNumTokensForDoc(doc.object))
}

documents <- as.data.frame(topic.m, stringsAsFactors = F)
colnames(documents) <- c("id", "text")

library(mallet)
library(tm)
writeLines(stopwords(), "data/stoplist1.csv")

mallet.instances <- mallet.import(documents$id, documents$text, "data/stoplist1.csv", FALSE, token.regexp="[\\p{L}']+")

n.topics <- 25
topic.model <- MalletLDA(num.topics = n.topics)

topic.model$loadDocuments(mallet.instances)

word.freqs <- mallet.word.freqs(topic.model)

topic.model$train(800) ## only took 4 seconds, GPA used 2000

topic.words <- mallet.topic.words(topic.model, smoothed = FALSE, normalized = FALSE)
topic.words.c <- apply(topic.words, 2, sum)

doc.topics.m <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)

vocabulary <- topic.model$getVocabulary()

docs.length <- as.vector(docs.length)
documents$n.tokens <- docs.length

phi <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)

# Part 3 - LDAvis

library(LDAvis)

json <- createJSON(theta = doc.topics.m, phi = phi, term.frequency = word.freqs$term.freq, vocab = vocabulary, doc.length = documents$n.tokens)

serVis(json)



