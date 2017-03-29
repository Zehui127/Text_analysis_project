rm(list=ls())
dd="/Users/jonasfenger/Documents/AU/Text_Mining"
setwd(dd)
text.v <- scan("/Users/jonasfenger/Documents/AU/Text_Mining/project/data/01.txt", what='character',sep='\n',encoding='UTF-8')


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


for(w in 4:8){
  a<-paste('t.v',w,sep='')
  a<-grep(as.character (paste("0",w,sep='')),text.v)
  a<-text.v[a]
  sent.ch <- token_sent(a)
  # import sentiment lexicon
  afinn.dt <- read.table('AFINN-111.txt', header = FALSE, sep = '\t',quote = "\"")
  names(afinn.dt) <- c('word','value')
  mattsentiment.v <- lexicon_scr(sent.ch,afinn.dt)
  jpeg(filename=paste(w,".jpeg",sep=''))
  plot(TTR::SMA(mattsentiment.v,10),type = 'l', xlab = w, ylab = 'Sentiment')
  dev.off
}

