### Something went wrong and all disappeared though I managed to work it all besides structural topic modelling
# and text networks (something did not work well with 'textnets' package)
#Day 3(1): Basic Text Analysis
library(dplyr)
library(ggplot2)
load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
head(trumptweets$text)
library(tm)
trump_corpus <- Corpus(VectorSource(as.vector(trumptweets$text))) 
trump_corpus
library(tidytext)
#loading tweets in 'tidytext' format -> each word a separate observation (row)
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
head(tidy_trump_tweets)
#count most popular words
tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))
## Text pre-processing
#cleaning stop words
trump_corpus <- tm_map(trump_corpus, removeWords, stopwords("english"))
data("stop_words")
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words)
#after cleaning, repeat the most popular words count
tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))
#better but not perfect ('https', 't.co', 'amp', 'rt')
#to remove punctuation
trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation))
#to remove numbers
trump_corpus <- tm_map(trump_corpus, content_transformer(removeNumbers))
#removing numeric digits and dashes (-) in 'tidytext'
tidy_trump_tweets<-tidy_trump_tweets[-grep("\\b\\d+\\b", tidy_trump_tweets$word),]
#lower-casing all the words
trump_corpus <- tm_map(trump_corpus,  content_transformer(tolower)) 
#cleaning whitespaces
trump_corpus <- tm_map(trump_corpus, content_transformer(stripWhitespace))
tidy_trump_tweets$word <- gsub("\\s+","",tidy_trump_tweets$word)
#stemming words (shorting the words to a common stem, like "processing" to "process") in Corpus
trump_corpus  <- tm_map(trump_corpus, content_transformer(stemDocument), language = "english")
#stemming in 'tidytext'
library(SnowballC)
tidy_trump_tweets<-tidy_trump_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))
##creating document-term matrix: matrix where each word is a row and each column is a document
trump_DTM <- DocumentTermMatrix(trump_corpus, control = list(wordLengths = c(2, Inf)))
inspect(trump_DTM[1:5,3:8])
#creating DTM in 'tidytext'
tidy_trump_DTM<-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)
