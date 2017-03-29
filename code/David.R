#split the text and try time analysis

rm(list = ls())
wd <- 'E:/workspace'
setwd(wd)


library(slam)
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

text.l <- unlist(lapply(text.l,slice_text,10), recursive=FALSE)
filenames.v <- gsub("\\..*","",names(text.l))
head(text.cor$content[1])
ls(text.l)
head(text.l[3])#test of vector
class(text.l[1])
text.l[3]$'01'[1]
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
text.dtm <- docsparse(2,text.dtm)
summary(col_sums(text.dtm)) 

## create matrix

dtm2 <- as.matrix(text.dtm)

## measure frequency of words
for(w in 1:12){
  v <- list()
  for(i in (10*(w-1)+1):(10*(w-1)+10)){
  d=dtm2[i,]
  a=d[which(d!=0)]
  v[[i]]=length(a)
}
jpeg(filename=paste(w,"tt.jpeg",sep=''))
c=1:10
plot(c,unlist(v),xlab="time(section)",ylab="uniqueword",type="b")
dev.off
}


d=sort(dtm2[3,], decreasing = TRUE)

a=d[which(d!=0)]


length(a)







### word counting with tm
library(tm)




# prune dtm
prune <- function(dtm,mx){
  mx <- ceiling(dim(dtm)[1]*mx)
  dtm <- dtm[,slam::col_sums(as.matrix(dtm) > 0) < mx]
  return(dtm)
}
text.dtm <- prune(text.dtm,.60)# try other levels of pruning
summary(col_sums(text.dtm))



# extract document and section individually
text.dtm$dimnames$Docs <- filenames.v






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
mdl1 <- LDA(v01.dtm[1,], k = k, method = 'VEM', control = list(seed = seed))

terms(mdl1,5)#1/5 of document01
# documents' topic distribution
doctopic.mat <- mdl1@gamma
dim(doctopic.mat)
doctopic.mat[1,]# topic saturation of document 1
row_sums(doctopic.mat)[1]
barplot(doctopic.mat[1,])




  