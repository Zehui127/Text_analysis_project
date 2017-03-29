rm(list = ls())
wd <- '/Users/jonasfenger/Documents/AU/Text_Mining'
setwd(wd)
source('utility.R')

###### word level lexicons/dictionaries 
# handheld with some help
group.v <- paste(scan('project/data/read_04_10_13.txt', what = 'character', sep='\n', encoding = 'UTF-8'), collapse = " ")
?paste

# sentence tokenizer
library(NLP)
library(openNLP) # annotation package
token_sent <- function(text, lang = "en") {
  sentannotator <- Maxent_Sent_Token_Annotator(language = lang) ## "here's a sentence, here's a sentence" like POS
  text <- as.String(text) # convert to string
  sentbound <- annotate(text, sentannotator)
  sentences <- text[sentbound]# extract sentences
  return(sentences)# return sentences
}

# sentiment function
lexicon_scr <- function(sentences,lexicon){
  token_word <- strsplit(tolower(sentences), "[^A-Za-z']+")# tokenize sentences + lowercasing
  sentiment.mat = matrix()
  for(i in 1:length(token_word)){
    tmp <- lexicon$value[which(lexicon$word %in% token_word[[i]])]# valence
    w <- length(tmp)# number of words
    if (length(tmp) > 0){
      sentiment.mat[i] <- sum(tmp)/w} # scale according to length - normalization
    else{sentiment.mat[i] = 0}
  }
  # sentiment.mat <- TTR::SMA(sentiment.mat,n = 10)# optional smoothing + ["TTR::SMA" = accessing a single function in a library]
  return(sentiment.mat)
}

# extract sentences
sent.ch <- token_sent(group.v)
head(sent.ch)

# import sentiment lexicon
afinn.dt <- read.table('AFINN-111.txt', header = FALSE, sep = '\t',quote = "\"")
names(afinn.dt) <- c('word','value') # adding names

# test the sentiment code
test.v <- c('I love whales. I hate Ahab because he is the epitome of whaling')
test.ch <- token_sent(test.v)
print(lexicon_scr(test.ch,afinn.dt))


# run on GROUP
groupsentiment.v <- lexicon_scr(sent.ch,afinn.dt)
library(TTR)
dev.new()
par(mfrow = c(3,1))
hist(groupsentiment.v)
plot(groupsentiment.v,type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')
plot(TTR::SMA(groupsentiment.v,10),type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')

# aesthetics
library(ggplot2)
groupsentiment.df <- data.frame(line = 1:length(groupsentiment.v), sentiment = TTR::SMA(groupsentiment.v,10))
dev.new()
ggplot(data = groupsentiment.df, aes(x = line, y = sentiment)) +
  geom_bar(stat = "identity", colour ="#FF9999")+
  theme_minimal() +
  xlab("Narrative Time (line)")+
  ylab("Sentiment") +
  labs(title = expression(paste("Sentiment in ", italic("Group")))) 

### with Syuzhet library
library(syuzhet)
library(tm)
library(qdap) #qual

# tokenize at sentence level
text_sent <- get_sentences(group.v)
head(text_sent)

# AFINN sentiment lexicon
text_afinn <- get_sentiment(text_sent, method = 'afinn')

# explore
text_sent[which(text_afinn == max(text_afinn))]
text_sent[which(text_afinn == min(text_afinn))]
text_sent[which(text_afinn > (mean(text_afinn)+sd(text_afinn)*2))]
text_sent[which(text_afinn < (mean(text_afinn)-sd(text_afinn)*2))]

# the NRC lexicon
group_nrc <- get_nrc_sentiment(text_sent)

### scaling with tm
library(tm)
dd = "/Users/jonasfenger/Documents/AU/Text_Mining/project/data";
books.cor  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language = "lat"))
names(books.cor) <- gsub("\\..*","",names(books.cor))# remove ending
filenames <- names(books.cor)
books.cor <- tm_map(books.cor, PlainTextDocument)
books.cor <- tm_map(books.cor, content_transformer(tolower))
books.cor <- tm_map(books.cor, removePunctuation)
books.cor <- tm_map(books.cor, removeNumbers)
books.cor <- tm_map(books.cor, stripWhitespace)
names(books.cor) <- filenames

# sentiment for each document
afinncorpus <- function(corpus){
  sent <- rep(0,length(corpus))
  for(i in 1:length(corpus)){
    sent[i] <- get_sentiment(paste(corpus[[i]]$content, collapse = " "),method = 'afinn')
  }
  return(sent)
}
sent.v <- afinncorpus(books.cor) 
dev.new(); barplot(sent.v, main="Group readings sentiments", horiz=FALSE)

# loop for measuring sentiment on each entity in each document in corpus


for (i in 1:length(books.cor)){
  books.cor[i]
  ppl.v=grep("\D:",books.cor[i])
  for(w in 1:length(ppl.v)){
    ppl.v[w]
    v=grep(ppl.v)
    
    
    
    
  }
}




