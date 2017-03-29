rm(list=ls())
dd="E:/workspace"
setwd(dd)
text.v<-scan("E:/workspace/01.txt",what='character',sep='\n',encoding='UTF-8')
t.v<-grep("05",text.v)
t.v<-text.v[t.v]
t.cv<-paste(t.v,collapse=" ")

library(NLP)
library(openNLP)
token_sent <- function(text, lang = "en") {
  sentannotator <- Maxent_Sent_Token_Annotator(language = lang)
  text <- as.String(text)# convert to string
  sentbound <- annotate(text, sentannotator)
  sentences <- text[sentbound]# extract sentences
  return(sentences)# return sentences
}

# sentiment function
lexicon_scr <- function(sentences,lexicon){
  token_word <- strsplit(tolower(sentences), "[^A-Za-z']+")# tokenize sentences
  sentiment.mat = matrix()
  for(i in 1:length(token_word)){
    tmp <- lexicon$value[which(lexicon$word %in% token_word[[i]])]# valence
    w <- length(tmp)# number of words
    if (length(tmp) > 0){
      sentiment.mat[i] <- sum(tmp)/w}
    else{sentiment.mat[i] = 0}
  }
  # sentiment.mat <- TTR::SMA(sentiment.mat,n = 10)# optional smoothing
  return(sentiment.mat)
}

sent.ch <- token_sent(t.cv)
# import sentiment lexicon
afinn.dt <- read.table('AFINN-111.txt', header = FALSE, sep = '\t',quote = "\"")
names(afinn.dt) <- c('word','value')
mattsentiment.v <- lexicon_scr(sent.ch,afinn.dt)

library(ggplot2)
mattsentiment.df <- data.frame(line = 1:length(mattsentiment.v), sentiment = TTR::SMA(mattsentiment.v,10))
dev.new()
ggplot(data = mattsentiment.df, aes(x = line, y = sentiment)) +
  geom_bar(stat = "identity", colour ="#FF9999")+
  theme_minimal() +
  xlab("Narrative Time (line)")+
  ylab("Sentiment") +
  labs(title = expression(paste("Sentiment in ", italic("5")))) 

