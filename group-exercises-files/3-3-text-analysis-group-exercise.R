###Day3 group exercise
load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
library(ggplot2)
library(tidyverse)
library(dplyr)
ggplot(trump_sentiment_plot, aes(x=date, y=n))+
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Negative Words")+
  xlab("")+
  ggtitle("Negative Sentiment in Trump Tweets")+
  theme(aspect.ratio=1/4)
view(trump_sentiment_plot)
#filtering approval dataset to distill voters as a group
trump_approval_voters <- filter(trump_approval, subgroup=="Voters")
library(dplyr)
#joining datasets together by date overlaps
trump_sentiment_negative_approval <- dplyr::inner_join(trump_sentiment_plot, trump_approval_voters, by="date")
#chacking means of approval and disapproval
mean(trump_sentiment_negative_approval$approve_estimate)
mean(trump_sentiment_negative_approval$disapprove_estimate)
#regression
regression = lm(approve_estimate ~ n, data = trump_sentiment_negative_approval)
summary(regression)
regression = lm(n ~ approve_estimate, data = trump_sentiment_negative_approval)
summary(regression)
##plotting
library(stringr)
install.packages("taRifx")
library(taRifx)
ggplot()+ 
  geom_line(data=trump_sentiment_negative_approval, aes(x=date, y=approve_estimate), color="red") +
  geom_line(data=trump_sentiment_negative_approval, aes(x=date, y=n), color="green")+
  scale_y_continuous("Number of Negative Words", sec.axis = sec_axis(~.*1, name = "Approval ratings")) +
  scale_x_continuous("Date", breaks = 1:379) +
  ggtitle("Negative Sentiment in Trump Tweets Compared to Trump's Estimated Approval Ratings")+
  theme(aspect.ratio=1/2)
#regression 2 - disapprove estimate
regression = lm(disapprove_estimate ~ n, data = trump_sentiment_negative_approval)
summary(regression)
regression = lm(n ~ disapprove_estimate, data = trump_sentiment_negative_approval)
summary(regression)
#plot2 - disapprove estimate
ggplot()+ 
  geom_line(data=trump_sentiment_negative_approval, aes(x=date, y=disapprove_estimate), color="red") +
  geom_line(data=trump_sentiment_negative_approval, aes(x=date, y=n), color="green")+
  scale_y_continuous("Number of negative words", sec.axis = sec_axis(~.*1, name = "Dispproval ratings")) +
  scale_x_continuous("Date", breaks = 1:379) +
  ggtitle("Negative Sentiment in Trump Tweets Compared to Trump's Estimated Dispproval Ratings")+
  theme(aspect.ratio=1/2)
trump_sentiment_positive <- filter(trump_tweet_sentiment, sentiment=="positive")
trump_sentiment_positive$date<-as.Date(trump_sentiment_positive$created_at, 
                                       format="%Y-%m-%d %x")
trump_sentiment_positive<-
  tidy_trump_tweets %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="positive") %>%
  count(date, sentiment)
trump_sentiment_positive_approval <- dplyr::inner_join(trump_sentiment_positive, trump_approval_voters, by="date")
regression = lm(approve_estimate ~ n, data = trump_sentiment_positive_approval)
summary(regression)
regression = lm(n ~ approve_estimate, data = trump_sentiment_positive_approval)
summary(regression)
#plot 3 - approve estimate with positive words
ggplot()+ 
  geom_line(data=trump_sentiment_positive_approval, aes(x=date, y=approve_estimate), color="red") +
  geom_line(data=trump_sentiment_positive_approval, aes(x=date, y=n), color="green")+
  scale_y_continuous("Number of positive words", sec.axis = sec_axis(~.*1, name = "Approval ratings")) +
  scale_x_continuous("Date", breaks = 1:422) +
  ggtitle("Positive Sentiment in Trump Tweets Compared to Trump's Estimated Approval Ratings")+
  theme(aspect.ratio=1/2)
#regression 4 - disapprove estimate
regression = lm(disapprove_estimate ~ n, data = trump_sentiment_positive_approval)
summary(regression)
regression = lm(n ~ disapprove_estimate, data = trump_sentiment_positive_approval)
summary(regression)
#plot4 - disapprove estimate with positive words
ggplot()+ 
  geom_line(data=trump_sentiment_positive_approval, aes(x=date, y=disapprove_estimate), color="red") +
  geom_line(data=trump_sentiment_positive_approval, aes(x=date, y=n), color="green")+
  scale_y_continuous("Number of positive words", sec.axis = sec_axis(~.*1, name = "Dispproval ratings")) +
  scale_x_continuous("Date", breaks = 1:422) +
  ggtitle("Positive Sentiment in Trump Tweets Compared to Trump's Estimated Dispproval Ratings")+
  theme(aspect.ratio=1/2)
trump_dictionary<-c("security","Mexico", "border", "wall")
trump_dictionary_tweets <- economic_tweets<-trumptweets[str_detect(trumptweets$text, paste(trump_dictionary, collapse="|")),]
view(trump_dictionary_tweets)
library(tidytext)
library(stringr)
library(dplyr)
tidy_trump_dictionary_tweets<- trump_dictionary_tweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
head(tidy_trump_dictionary_tweets)
library(tm)
trump_dictionary_corpus <- tm_map(trump_corpus, removeWords, stopwords("english"))
data("stop_words")
tidy_trump_dictionary_tweets<-tidy_trump_dictionary_tweets %>%
  anti_join(stop_words)
trump_dictionary_corpus <- tm_map(trump_dictionary_corpus, content_transformer(removePunctuation))
trump_dictionary_corpus <- tm_map(trump_dictionary_corpus, content_transformer(removeNumbers))
tidy_trump_dictionary_tweets<-tidy_trump_dictionary_tweets[-grep("\\b\\d+\\b", tidy_trump_dictionary_tweets$word),]
trump_dictionary_corpus <- tm_map(trump_dictionary_corpus,  content_transformer(tolower))
trump_dictionary_corpus <- tm_map(trump_dictionary_corpus, content_transformer(stripWhitespace))
tidy_trump_dictionary_tweets$word <- gsub("\\s+","",tidy_trump_dictionary_tweets$word)
trump_tweet_dictionary_sentiment <- tidy_trump_dictionary_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(created_at, sentiment)
trump_tweet_dictionary_sentiment_pos <- filter(trump_tweet_dictionary_sentiment, sentiment=="positive")
mean(trump_tweet_dictionary_sentiment_pos$n)
trump_tweet_dictionary_sentiment_neg <- filter(trump_tweet_dictionary_sentiment, sentiment=="negative")
mean(trump_tweet_dictionary_sentiment_neg$n)
trump_tweet_sentiment_pos <- filter (trump_tweet_sentiment, sentiment=="positive")
mean(trump_tweet_sentiment_pos$n)
trump_tweet_sentiment_neg <- filter (trump_tweet_sentiment, sentiment=="negative")
mean(trump_tweet_sentiment_neg$n)
trump_sentiment_means <- c(mean(trump_tweet_dictionary_sentiment_pos$n), mean(trump_tweet_dictionary_sentiment_neg$n), mean(trump_tweet_sentiment_pos$n), mean(trump_tweet_sentiment_neg$n))
mean_positive_1 <- mean(trump_tweet_sentiment_pos$n)
mean_positive_2 <- mean(trump_tweet_sentiment_neg$n)
mean_positive_3 <- mean(trump_tweet_dictionary_sentiment_pos$n)
mean_positive_4 <- mean(trump_tweet_dictionary_sentiment_neg$n)
means_dataframe <- data.frame(mean_positive_1, mean_positive_2, mean_positive_3, mean_positive_4)
names <- c("Positive words generally", "Negative words generally", "Positive words particularly", "Negative words particularly")
H <- c(mean_positive_1, mean_positive_2, mean_positive_3, mean_positive_4)
barplot(H, names.arg=names, xlab="Positive and Negative Words, generally and particularly (with regard to the selected terms)", ylab="Mean number of words per tweet")
