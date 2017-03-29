#split the text and try time analysis

rm(list = ls())
wd <- 'E:/workspace'
setwd(wd)
library(syuzhet)
library(tm)
library(slam)
input.dir <- "good"
files.v <- dir(input.dir,"\\.txt$")
afinncorpus <- function(corpus){
  sent <- rep(0,length(corpus))
  for(i in 1:length(corpus)){
    sent[i] <- get_sentiment(paste(corpus[[i]]$content, collapse = " "),method = 'afinn')
  }
  return(sent)
}
maketext.l <- function(files.v, input.dir){
  text.word.l <- list() # set up empty list
  for(i in 1:length(files.v)){ # loop over the files.v in input.dir
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n",encoding='UTF-8') # read a file
    text.word.l[[files.v[i]]] <- text.v 
  }
  return(text.word.l)
}
text.l <- maketext.l(files.v,input.dir)
names(text.l) <- gsub("\\..*","",names(text.l))
slice_text <- function(text,bin){
  sliced.text.l <- split(text, cut(1:length(text),bin))
}

text.l <- unlist(lapply(text.l,slice_text,10), recursive=FALSE)
filenames.v <- gsub("\\..*","",names(text.l))


s<-list()

for(j in 1:9){
  for( i in 1:10){
    t<-unlist(text.l[i])  
    s[[i]]<-t[grep(paste('0',j,sep=''),t)]
    }
text.cor <- Corpus(VectorSource(lapply(s, paste, collapse = " ")))
str(text.cor)

text.cor <- tm_map(text.cor, removeNumbers)
text.cor <- tm_map(text.cor, removePunctuation)
text.cor <- tm_map(text.cor, removeWords, stopwords("english"))
text.cor <- tm_map(text.cor, stripWhitespace)
text.cor <- tm_map(text.cor, stemDocument)
text.cor <- tm_map(text.cor, content_transformer(tolower))

sent.v <- afinncorpus(text.cor) 
jpeg(filename=paste(j,"s.jpeg",sep=''))
plot(sent.v, main="narrator sentiments in d1 ",type='b')
dev.off
}