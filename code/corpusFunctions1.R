## RSM 9-15-15, unchanged from Dr. Arndt's function
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