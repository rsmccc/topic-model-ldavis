## setup

setwd("~/Development/R/ura")
library(XML)
input.dir <- "data/ALL39IN"
files.v <- dir(path = input.dir, pattern = ".*xml")

## list paths of all files in files.v
file.path(input.dir, files.v)

chunk.size <- 1000 ## number of words per chunk

source("./code/corpusFunction.R")

textname <- gsub("\\..*","", files.v[1]) ## removes file extensions from file names

topic.m<-NULL
for(i in 1:length(files.v)){
  doc.object<-xmlTreeParse(file.path(input.dir, files.v[i]),
                           useInternalNodes=TRUE)
  chunk.df<-makeFlexTextChunks(doc.object, chunk.size,
                               percentage=FALSE)
  textname<-gsub("\\..*","", files.v[i])
  segments.m<-cbind(paste(textname,
                          segment=1:nrow(chunk.df), sep="_"), chunk.df)
  topic.m<-rbind(topic.m, segments.m)
}



documents<-as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents)<-c("id", "text")

#Topic Model Code Part 2

library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "./data/stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")
## Create a topic trainer object.
num.topics <- 43
topic.model <- MalletLDA(num.topics)
topic.model$loadDocuments(mallet.instances)
freqs <- mallet.word.freqs(topic.model)
vocabulary <- topic.model$getVocabulary()


#GPA-code from http://cpsievert.github.io/LDAvis/reviews/reviews.html
#GPA-I'm not using the following for various reasons
#stopwords <- as.character(subset(freqs, term.freq <= 9)$words)
# 's' and 't' show up frequently and aren't very informative, so they are #also #included as stopwords
#writeLines(c(readLines("data/stoplist.csv"), stopwords, "s", "t"),  "stopwords2.txt")
#GPA-this is the next active bit
# Re-'initiate' topic model without the infrequent words
instance2 <- mallet.import(documents$id,
                           documents$text, "data/stoplist.csv")
topic.model2 <- MalletLDA(num.topics = 25)
topic.model2$loadDocuments(instance2)
freqs2 <- mallet.word.freqs(topic.model2)

# CS-this takes about 4.5 minutes on a macbook pro laptop with 2GB RAM and a 2.26GHz processor 
topic.model2$train(2000)

# Here, we compute the estimated topic-term distribution, incorporating the effect
# of the prior using 'smoothed = TRUE'.
phi <- t(mallet.topic.words(topic.model2, smoothed = TRUE, normalized = TRUE))
#GPA I've added some of this for my use elsewhere
write.csv(phi, "./data/1phi-BWCOM-min.csv") 
#GPA note, the spreadsheet below--"~/Documents/EH2015/data/1phi-transposed-withwords" is the same as this, #transposed and with words visible
write.csv(mallet.topic.words(topic.model2, smoothed = T, normalized = T), "./data/4topicwords_BWCOM-min.csv") # RSM 9-1-15: fixed error w/ args

#GPA back to LDAvis
# Let's look at the table of topics and terms by setting 'normalized = FALSE'
phi.count <- t(mallet.topic.words(topic.model2, smoothed = TRUE, normalized = FALSE))
#GPA-I write this to spreadsheet
write.csv(phi.count, "./data/3phi-count_BWCOM-min.csv")
#GPA note-when this is transposed, it produces 4topicwords2, more or less
# CS-LDAvis-Now get the smoothed estimates of the document-topic distributions:
topic.words <- mallet.topic.words(topic.model2, smoothed = TRUE, normalized = FALSE)
#GPA-write this to csv
write.csv(topic.words, "./data/4topicwords2-_BWCOM-min.csv")

# LDAvis-'count' of the number of tokens per topic (including pseudo-tokens from the priors)
topic.counts <- rowSums(topic.words)
#GPA-I write this to a spreadsheet
write.csv(topic.counts, "./data/5topic-counts-_BWCOM-min.csv")

topic.proportions <- topic.counts/sum(topic.counts)
write.csv(topic.proportions, "./data/6topicproportions-_BWCOM-min.csv")

#GPA note this (6topicproportions)can be used to reorder the 10topiclabelsBWCOM-min.csv produced below)!

vocab <- topic.model2$getVocabulary()

write.csv(vocab, "./data/7vocab-_BWCOM-min.csv")

# write.csv(topic.model2, "./data/8topicmode-_BWCOM-min.csv") ## RSM commented this out, not sure what it's for
write.csv(topic.m,"./data/9Documents-_BWCOM-min.csv")

topic.words.m <- mallet.topic.words(topic.model2,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)

doc.topics.m <- mallet.doc.topics(topic.model2,
                                  smoothed=TRUE,
                                  normalized=TRUE)
dim(doc.topics.m)

file.ids.v<-documents[,1]
head(file.ids.v)

file.id.l<-strsplit(file.ids.v, "_")
file.chunk.id.l<-lapply(file.id.l, rbind)
file.chunk.id.m<-do.call(rbind, file.chunk.id.l)
head(file.chunk.id.m)

rowSums(topic.words.m)

topic.words.m[1:3, 1:3]

vocabulary <- topic.model2$getVocabulary()
colnames(topic.words.m)<-vocabulary
topic.words.m[1:3, 1:3]

documents<-as.data.frame(topic.m, stringsAsFactors=F)

colnames(documents)<-c("id", "text")

#vocabulary <- topic.model$getVocabulary()
#word.freqs <- mallet.word.freqs(topic.model)
#topic.model$train(400)
#topic.words.m <- mallet.topic.words(topic.model,
#smoothed=TRUE,
#normalized=TRUE)
#colnames(topic.words.m)<-vocabulary

num.topics <- 25
topics.labels <- rep("", num.topics)
for (topic in 1:num.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model2, topic.words.m[topic,], num.top.words=5)$words, collapse=" ")
#have a look at keywords for each topic
topics.labels

topic.docs <- t(doc.topics.m)
topic.docs <- topic.docs / rowSums(topic.docs)

#figure out how to make labels for the cluster the document names?

# create data.frame with columns as people and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

file.ids.v<-documents[,1]
file.id.l<-file.ids.v
file.chunk.id.l<-lapply(file.id.l, rbind)
file.chunk.id.m<-do.call(rbind, file.chunk.id.l)
head(file.chunk.id.m)

doc.topics.df<-as.data.frame(doc.topics.m)

doc.topics.df<-cbind(file.chunk.id.m[,1], doc.topics.df)
doc.topic.means.df<-aggregate(doc.topics.df[, 2:ncol(doc.topics.df)],
                              list(doc.topics.df[,1]), mean)

## cluster based on shared words
pdf("./data/1Clustertopics-_BWCOM-min.pdf")
plot(hclust(dist(topic.words.m)), labels=topics.labels)
dev.off()

write.csv(topic.words.m, "./data/1phi-transposed-withwords-_BWCOM-min.csv")
write.csv(doc.topics.df, "./data/3btopics-by-document-rev-_BWCOM-min.csv")

write.csv(topics.labels, "./data/10topiclabels-_BWCOM-min.csv")


#the following is the heart of the problem!

# LDAvis can be installed from GitHub via #`devtools::install_github("cpsievert/LDAvisData")`
library(LDAvis)


## RSM - attempted fixes 9-1-15, from http://cpsievert.github.io/LDAvis/reviews/reviews.html
# get.terms <- function(x) {
#   index <- match(x, vocab)
#   index <- index[!is.na(index)]
#   rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
# }
# documents1 <- lapply(topic.m, get.terms)


# phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
# theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))

# fit <- lda.collapsed.gibbs.sampler(documents = documents1, K = num.topics, vocab = vocab, 
#                                    num.iterations = G, alpha = alpha, 
#                                    eta = eta, initial = NULL, burnin = 0,
#                                    compute.log.likelihood = TRUE)

## not sure if any of these are right
phi2 <- topic_docs
theta <- doc.topics.m
doc.length <- freqs2$doc.freq ## guessing here
# doc.length <- [1:3523]
term.frequency <- freqs2$term.freq ## also just a guess

# create the json object, start a local file server, open in default browser
## Not run: 
#GPA--The question is how do I get the data produced above into shape for the next step?

data(topic.model2)

# create the json object, start a local file server, open in default browser
createJSON(phi = phi2,
           theta = theta, 
           doc.length = doc.length, 
           vocab = vocab, 
           term.frequency = term.frequency)
serVis(json) # press ESC or Ctrl-C to kill



