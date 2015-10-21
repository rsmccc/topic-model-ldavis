getTEIWordTableList<-function(doc.object){
  paras<-getNodeSet(doc,
                    "/d:TEI/d:text/d:body//d:p",
                    c(d = "http://www.tei-c.org/ns/1.0"))
  words<-paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <-tolower(words)
  words.l<-strsplit(words.lower, "\\W")
  word.v<-unlist(words.l)
  book.freqs.t<-table(word.v[which(word.v!="")])
  book.freqs.rel.t<-100*(book.freqs.t/sum(book.freqs.t))
  return(book.freqs.rel.t)
}

getTEIWordSegmentTableList<-function(doc.object, chunk.size=10){
  paras<-getNodeSet(doc.object,
                    "/d:TEI/d:text/d:body//d:p",
                    c(d = "http://www.tei-c.org/ns/1.0"))
  words<-paste(sapply(paras,xmlValue), collapse=" ")
  words.lower <-tolower(words)
  words.list<-strsplit(words.lower, "\\W")
  word.v<-unlist(words.list)
  max.length<-length(word.v)/chunk.size
  x <- seq_along(word.v)
  chunks.l <- split(word.v, ceiling(x/max.length))
  chunks.l<-lapply(chunks.l, removeBlanks)
  freq.chunks.l<-lapply(chunks.l, table)
  rel.freq.chunk.l<-lapply(freq.chunks.l, prop.table)
  return(rel.freq.chunk.l)
}

splitText <- function(text) {
  unlist(strsplit(text," "))
}

selectTaggedWords <- function(tagged.words, target.tag) {
  tagged.words[grep(target.tag, tagged.words)]	
}


removeTags <- function(word.pos) {
  sub("_[A-Z]{2,3}", "", word.pos)
}

removeBlanks<-function(x){
  x[which(x!="")]
}

makeFlexTextChunksFromTagged<-function(tagged.text, chunk.size=750, percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words, "_NN$$"))
  words <- removeTags(tagged.words.keep)
  words.lower<-tolower(words)
  word.v<-gsub("[^[:alnum:][:space:]']", "", words.lower)
  x <- seq_along(word.v)
  if(percentage){
    max.length<-length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]]) <=
         length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]]<-
        c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]]<-NULL
    }
  }
  chunks.l<-lapply(chunks.l, paste, collapse=" ")
  chunks.df<-do.call(rbind, chunks.l)
  return(chunks.df)
}


makeFlexTextChunks<-function(doc.object, chunk.size=750, percentage=TRUE){
  paras<-getNodeSet(doc.object,
                    "/d:TEI/d:text/d:body//d:p",
                    c(d = "http://www.tei-c.org/ns/1.0"))
  words<-paste(sapply(paras,xmlValue), collapse=" ")
  words.lower<-tolower(words)
  words.lower<-gsub("[^[:alnum:][:space:]']", " ", words.lower)
  words.l<-strsplit(words.lower, "\\s+")
  word.v<-unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length<-length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
         length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]],
          chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]]<-NULL
    }
  }
  chunks.l<-lapply(chunks.l, paste, collapse=" ")
  chunks.df<-do.call(rbind, chunks.l)
  return(chunks.df)
}

makeTextFromTagged<-function(tagged.text,
                             chunk.size=1000, percentage=False){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,"_NN$"))
  words <- removeTags(tagged.words.keep)
  words.lower<-tolower(words)
  word.v<-gsub("[^[:alnum:][:space:]']", "", words.lower)
  x <- seq_along(word.v)
  if(percentage){
    max.length<-length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]]) <=
         length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]]<-
        c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]]<-NULL
    }
  }
  chunks.l<-lapply(chunks.l, paste, collapse=" ")
  chunks.df<-do.call(rbind, chunks.l)
  return(chunks.df)
}