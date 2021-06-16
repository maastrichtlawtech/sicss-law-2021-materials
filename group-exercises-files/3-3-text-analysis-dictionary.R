###Day3(2): Dictrionary-Based Analysis
library(tidytext)
library(dplyr)
data("stop_words")
#let’s count the top words after removing stop words and other meaningless words
top_words<-
  tidy_trump_tweets %>%
  anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(word) %>%
  arrange(desc(n))
library(ggplot2)
#plot graph of top 20 words
top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Trump Tweets")+
  guides(fill=FALSE)
###inverse document frequency (IDF) - giving more weight to terms appearing in less documents
##->calculate the tf-idf for the Trump tweets databased in tidytext
tidy_trump_tfidf<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)
#let’s see what the most unusual words are
top_tfidf<-tidy_trump_tfidf %>%
  arrange(desc(tf_idf))
#return the most unusual word
top_tfidf$word[1]
###dictionary-based quantitative text analysis
#create a dictionary
economic_dictionary<-c("economy","unemployment","trade","tariffs")
library(stringr)
#having created a very simple/primitive dictionary, we can now subset 
#->the parts of our tidytext dataframe that contain these words
#-> creating 'economic_tweets' dataset
economic_tweets<-trumptweets[str_detect(trumptweets$text, paste(economic_dictionary, collapse="|")),]
##sentiment analysis
library(tidytext)
head(get_sentiments("bing"))
#applying 'bing' sentiment dictionary to the tweets
trump_tweet_sentiment <- tidy_trump_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(created_at, sentiment) 

head(trump_tweet_sentiment)
#let’s make a visual that compares the frequency of positive and negative tweets by day
#transforming 'created_at' variable into 'date' object
#tells R how to read in the date character string
tidy_trump_tweets$date<-as.Date(tidy_trump_tweets$created_at, 
                                format="%Y-%m-%d %x")
#let’s aggregate negative sentiment by day
trump_sentiment_plot <-
  tidy_trump_tweets %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)
#let's plot the aggregate negative sentiment
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
#comparing the negative sentiment trend in Trump's tweets with Trump’s approval rating
#getting database on Trump's approval
trump_approval<-read.csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv")
#formatting date in the data
trump_approval$date<-as.Date(trump_approval$modeldate, format="%m/%d/%Y")
#creating dataset for plotting
approval_plot<-
  trump_approval %>%
  filter(subgroup=="Adults") %>%
  filter(date>min(trump_sentiment_plot$date)) %>% 
  group_by(date) %>%
  summarise(approval=mean(approve_estimate))
#plotting the data
ggplot(approval_plot, aes(x=date, y=approval))+
  geom_line(group=1)+
  theme_minimal()+
  ylab("% of American Adults who Approve of Trump")+
  xlab("Date")
