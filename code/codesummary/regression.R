####unique words for each slice
rm(list = ls())
wd <- 'E:/workspace'
setwd(wd)
source('util_fun.R')
input.dir <- "good"
files.v <- dir(input.dir,"\\.txt$")
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

text.l <- unlist(lapply(text.l,slice_text,10), recursive=FALSE)#split each document into 10 slices
filenames.v <- gsub("\\..*","",names(text.l))

# create corpus from slices
library(tm)
text.cor <- Corpus(VectorSource(lapply(text.l, paste, collapse = " ")))
# clean and filter
text.cor <- tm_map(text.cor, removeNumbers)
text.cor <- tm_map(text.cor, removePunctuation)
text.cor <- tm_map(text.cor, removeWords, stopwords("english"))
text.cor <- tm_map(text.cor, stripWhitespace)
text.cor <- tm_map(text.cor, stemDocument)
text.cor <- tm_map(text.cor, content_transformer(tolower))
## create document term matrix
text.dtm <- DocumentTermMatrix(text.cor)
text.dtm <- docsparse(2,text.dtm)
## create matrix
dtm2 <- as.matrix(text.dtm)
## measure frequency of words
for(w in 1:12){
  v <- list()
  for(i in (10*(w-1)+1):(10*(w-1)+10)){
  d=dtm2[i,]
  b<-sum(d)
  a=d[which(d>=1)]
  v[[i]]=length(a)/b
}
jpeg(filename=paste(w,"pp.jpeg",sep=''))
c=1:10
plot(unlist(v)~c,xlab="time(section)",ylab="uniqueword");abline(lm(unlist(v)~c))
dev.off
}




  