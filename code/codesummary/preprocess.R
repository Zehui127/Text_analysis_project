####topic modelling for each slice
#regular step to set workspace 
rm(list = ls())
wd <- 'E:/workspace'
setwd(wd)
source('util_fun.R')

##input 12 documents
input.dir <- "good"
files.v <- dir(input.dir,"\\.txt$")#get file name
#set a function to inout documents
maketext.l <- function(files.v, input.dir){
  text.word.l <- list() # set up empty list
  for(i in 1:length(files.v)){ # loop over the files.v in input.dir
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n",encoding='UTF-8') # read a file
    text.word.l[[files.v[i]]] <- text.v 
  }
  return(text.word.l)
}
text.l <- maketext.l(files.v,input.dir)#creat a list to contain the documents
names(text.l) <- gsub("\\..*","",names(text.l))
##split each document to certain slices
slice_text <- function(text,bin){
    sliced.text.l <- split(text, cut(1:length(text),bin))
  }
#use split_text function to split each document to three slices
text.l <- unlist(lapply(text.l,slice_text,3), recursive=FALSE)
filenames.v <- gsub("\\..*","",names(text.l))

# create corpus from slices
library(tm)
text.cor <- Corpus(VectorSource(lapply(text.l, paste, collapse = " ")))
str(text.cor)
# clean and filter
text.cor <- tm_map(text.cor, removeNumbers)
text.cor <- tm_map(text.cor, removePunctuation)
text.cor <- tm_map(text.cor, removeWords, stopwords("english"))
text.cor <- tm_map(text.cor, stripWhitespace)
text.cor <- tm_map(text.cor, stemDocument)
text.cor <- tm_map(text.cor, content_transformer(tolower))

## create document term matrix
text.dtm <- DocumentTermMatrix(text.cor)
##try to eliminate the sparse terms
text.dtm <- docsparse(2,text.dtm)
summary(col_sums(text.dtm)) 
#extract each document from the big matrix 
text.dtm$dimnames$Docs <- filenames.v
#creat 12 matrix and each one represent a document
v01.idx <- filenames.v == '01'
v01.dtm <- text.dtm[v01.idx,]

v02.idx <- filenames.v == '02'
v02.dtm <- text.dtm[v02.idx,]

v03.idx <- filenames.v == '03'
v03.dtm <- text.dtm[v03.idx,]

v04.idx <- filenames.v == '04'
v04.dtm <- text.dtm[v04.idx,]

v05.idx <- filenames.v == '05'
v05.dtm <- text.dtm[v05.idx,]

v06.idx <- filenames.v == '06'
v06.dtm <- text.dtm[v01.idx,]

v07.idx <- filenames.v == '07'
v07.dtm <- text.dtm[v01.idx,]

v08.idx <- filenames.v == '08'
v08.dtm <- text.dtm[v08.idx,]

v09.idx <- filenames.v == '09'
v09.dtm <- text.dtm[v09.idx,]

v10.idx <- filenames.v == '10'
v10.dtm <- text.dtm[v10.idx,]

v11.idx <- filenames.v == '11'
v11.dtm <- text.dtm[v11.idx,]

v12.idx <- filenames.v == '12'
v12.dtm <- text.dtm[v12.idx,]

# train topic model based latent dirichlet allocation
library(topicmodels) # Based on Blei's code
k = 3 # number of topics
seed <- 1234
  #by changing the index of the 
  v.dtm<-v11.dtm
  mdl1 <- LDA(v.dtm[1,], k = k, method = 'VEM', control = list(seed = seed))
  mdl2 <- LDA(v.dtm[2,], k = k, method = 'VEM', control = list(seed = seed))
  mdl3<- LDA(v.dtm[3,], k = k, method = 'VEM', control = list(seed = seed))
  terms(mdl1,15)#1/5 of document01
  # documents' topic distribution
  par(mfrow=c(3,1))
  doctopic.mat <- mdl1@gamma
  doctopic.mat[1,]# topic saturation of document 1
  barplot(doctopic.mat[1,])
  doctopic.mat <- mdl2@gamma
  doctopic.mat[1,]# topic saturation of document 1
  barplot(doctopic.mat[1,])
  doctopic.mat <- mdl3@gamma
  doctopic.mat[1,]# topic saturation of document 1
  barplot(doctopic.mat[1,])


  