# Undergraduate Research Assistantship (URA) Topic Model with LDAvis
## Riley McCloskey (RSM)

setwd("~/Development/R/ura")
input.dir <- "data/ALL39IN"

library(XML)

## Create vector of filenames within input directory
files.v <- dir(path = input.dir, pattern = "*.xml")

chunk.size <- 500 ## number of words per chunk

getNumTokensForDoc <- function(doc.object) {
    paras<-getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
    words<-paste(sapply(paras,xmlValue), collapse=" ")
    words.lower<-tolower(words)
    words.lower<-gsub("[^[:alnum:][:space:]']", " ", words.lower)
    words.l<-strsplit(words.lower, "\\s+")
    word.v<-unlist(words.l)
    return (length(word.v))
}

## Load makeFlexTextChunks function
makeFlexTextChunks<-function(doc.object, chunk.size=500, percentage=TRUE){
    paras<-getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
    words<-paste(sapply(paras,xmlValue), collapse=" ")
    words.lower<-tolower(words)
    words.lower<-gsub("[^[:alnum:][:space:]']", " ", words.lower)
    words.l<-strsplit(words.lower, "\\s+")
    word.v<-unlist(words.l)
    x <- seq_along(word.v)
    if (percentage){
        max.length<-length(word.v)/chunk.size
        chunks.l <- split(word.v, ceiling(x/max.length))
    } else {
        chunks.l <- split(word.v, ceiling(x/chunk.size))
        #deal with small chunks at the end
        if (length(chunks.l[[length(chunks.l)]]) <= length(chunks.l[[length(chunks.l)]]) / 2){ ## RSM 9-22-15
            chunks.l[[length(chunks.l) - 1]] <- c(chunks.l[[length(chunks.l)-1]],
                                                  chunks.l[[length(chunks.l)]])
            chunks.l[[length(chunks.l)]]<-NULL
        }
    }
    chunks.l<-lapply(chunks.l, paste, collapse=" ")
    chunks.df<-do.call(rbind, chunks.l)
    return(chunks.df)
}

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

doc.length <- NULL
length(doc.length) <- length(files.v)

## Loop through documents and get number of tokens per document
for (i in 1:length(files.v)) {
    doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]),
                             useInternalNodes=TRUE)
    n.tokens <- getNumTokensForDoc(doc.object)
    doc.length[i] <- n.tokens
}

documents <- as.data.frame(topic.m, stringsAsFactors = F)
colnames(documents) <- c("id", "text")

# RSM 10-7-2015: fix for duplicate row.names Error
rownames(documents) <- make.names(documents$id, unique = TRUE)

## Part 2 - Mallet

library(mallet)

mallet.instances <- mallet.import(documents$id, documents$text, "data/stoplist1.csv", FALSE, token.regexp="[\\p{L}']+")

n.topics <- 25
topic.model <- MalletLDA(num.topics = n.topics)

topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()

# RSM - BUG: Source of error and program breaking, do not use mallet.word.freqs()
# word.freqs <- mallet.word.freqs(topic.model)

topic.model$train(800) ## RSM - GPA used 2000, this only takes about 10 mins on MacBook Pro

topic.words.m <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed = FALSE, normalized = FALSE)
topic.words.c <- apply(topic.words, 2, sum)

doc.topics.m <- mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)

docs.length <- rep(chunk.size, length(documents$text))
docs.length <- as.vector(docs.length)

library(LDAvis)

json <- createJSON(phi = topic.words.m, theta = doc.topics.m, doc.length = docs.length, vocab = vocabulary, term.frequency = topic.words.c)

serVis(json)



