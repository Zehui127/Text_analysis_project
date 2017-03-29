getwd()
# wd = "/Users/jonasfenger/Documents/AU/Text_Mining/project"
source('utility.R')
scan()
filename.v <- "/Users/jonasfenger/Documents/AU/Text_Mining/project/data/read_01_11_13.txt" 
text.v <- scan(filename.v, what = 'character', sep='\n', encoding = 'UTF-8')
text.v[1:10]
jane.v <- grep("Jane:", text.v)
jane.v=text.v[jane.v]
jane.v[1:10]

## sentiment analysis

library(NLP)
library(openNLP) 

# sentence tokenizer
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
sent.ch <- token_sent(jane.v)
head(sent.ch)

# import sentiment lexicon
?read.table
afinn.dt <- read.table('AFINN-111.txt', header = FALSE, sep = '\t',quote = "\"")
names(afinn.dt) <- c('word','value') # adding names

# test the sentiment code
test.v <- c('I love whales. I hate Ahab because he is the epitome of whaling')
test.ch <- token_sent(test.v)
print(lexicon_scr(test.ch,afinn.dt))


# run on jane
janesentiment.v <- lexicon_scr(sent.ch,afinn.dt)
library(TTR)
dev.new()
par(mfrow = c(3,1))
hist(janesentiment.v)
plot(janesentiment.v,type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')
plot(TTR::SMA(janesentiment.v,10),type = 'l', xlab = 'Narrative Time', ylab = 'Sentiment')

# aesthetics
library(ggplot2)
janesentiment.df <- data.frame(line = 1:length(janesentiment.v), sentiment = TTR::SMA(janesentiment.v,10))
dev.new()
ggplot(data = janesentiment.df, aes(x = line, y = sentiment)) +
  geom_bar(stat = "identity", colour ="#FF9999")+
  theme_minimal() +
  xlab("Narrative Time (line)")+
  ylab("Sentiment") +
  labs(title = expression(paste("Sentiment in ", italic("Jane")))) 
