install.packages("rtweet")
library(rtweet)
install.packages("twitteR")
library(twitteR)
library(tidyverse)
install.packages("httpuv")

####API####
api_key <- "###"
api_secret_key <- "###"

token <- create_token(
  app = "NRRPs",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

#####SEARCHES TWEETS####

g7_tweets_rec<-search_tweets("G7", n=10000, type="recent", 
                         lang="en")
##### POPULAR AND MIXED TWEETS

g7_tweets_pop<-search_tweets("G7", n=5000, type="popular", 
                         lang="en")

g7_tweets_mix<-search_tweets("G7", n=10000, type="mixed", 
                             lang="en")

g7_tweets_pop_sun<-search_tweets("G7", n=10000, type="popular", 
                             lang="en")

#####

sunday_tweets<-g7_tweets_rec[g7_tweets_rec$created_at="2021-06-12"]

names(g7_tweets_rec)

head(g7_tweets$text)

#####GRAPH####
library(ggplot2)

ts_plot(g7_tweets, "secs") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Covid-19 Around 1pm, May 3, 2020",
    subtitle = "Tweet counts aggregated by second",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
#####CORPUS QUANTEDA#####

g7_tweets_rec$text

library(quanteda)
corpus <- corpus(g7_tweets_rec, text_field="text",meta=list("created_at"))



View(corpus)

corpus_china <- kwic(corpus,
                            pattern = "china", 
                            window = 10)
View(corpus_china)

##### Corpus kwic categories####

corpus_tax <- kwic(corpus,
                     pattern = "tax*", 
                     window = 10)

corpus_climate <- kwic(corpus,
                   pattern = "climate", 
                   window = 10)

corpus_climate_change <- kwic(corpus,
                       pattern = phrase("climate change"), 
                       window = 10)
corpus_climate_crisis <- kwic(corpus,
                              pattern = phrase("climate crisis"), 
                              window = 10)

docvars(corpus, "Time") <- g7_tweets_rec$created_at

g7_tweets_china <- g7_tweets_rec[grepl("China", 
                                   g7_tweets_rec$text, 
                                   ignore.case = TRUE),]

g7_tweets_tax <- g7_tweets_rec[grepl("tax*", 
                                   g7_tweets_rec$text, 
                                   ignore.case = TRUE),]

g7_tweets_climate <- g7_tweets_rec[grepl("climate", 
                                     g7_tweets_rec$text, 
                                     ignore.case = TRUE),]

g7_tweets_covid <- g7_tweets_rec[grepl("covid", 
                                         g7_tweets_rec$text, 
                                         ignore.case = TRUE),]
head(g7_tweets_covid)

ts_plot(g7_tweets_china, "mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about China Around 1pm, June 15, 2021",
    subtitle = "Tweet counts aggregated by second",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(g7_tweets_tax, "mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Taxation Around 1pm, June 15, 2021",
    subtitle = "Tweet counts aggregated by second",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(g7_tweets_climate, "mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Climate Around 1pm, June 15, 2021",
    subtitle = "Tweet counts aggregated by second",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(g7_tweets_covid, "mins") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about Covid Around 1pm, June 15, 2021",
    subtitle = "Tweet counts aggregated by second",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


#TIME TREND
head(g7_tweets_rec$created_at)

g7_tweets_rec$date<-as.Date(g7_tweets_rec$created_at, format="%Y-%m-%d")
head(g7_tweets_rec$date)

#keywords
corpus_tax <- kwic(g7_tweets_rec$text,
                   pattern = "tax*", 
                   window = 10)
