load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
library(tidytext)
library(dplyr)
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

tidy_trump_tweets <- tidy_trump_tweets %>% filter(!(word=="https"|
                                 word=="rt"|
                                 word=="t.co"|
                                 word=="u."|
                                 word=="amp")) 

tidy_trump_tweets2 <- tidy_trump_tweets %>% filter(!(word=="https"|
                                                      word=="rt"|
                                                      word=="t.co"|
                                                      word=="u."|
                                                      word=="amp"|
                                                      word=="trump"|
                                                       word=="countr*"|
                                                       word=="america"
                                                       
)) 
trump_corpus <- tm_map(trump_corpus, content_transformer(removePunctuation))
trump_corpus <- tm_map(trump_corpus, content_transformer(removeNumbers))

trump_corpus <- tm_map(trump_corpus, content_transformer(removeNumbers))

trump_corpus <- tm_map(trump_corpus,  content_transformer(tolower)) 

trump_corpus <- tm_map(trump_corpus, content_transformer(stripWhitespace))

tidy_trump_tweets$word <- gsub("\\s+","",tidy_trump_tweets$word)

trump_corpus  <- tm_map(trump_corpus, content_transformer(stemDocument), language = "english")

installed.packages("SnowballC")
library("SnowballC")

tidy_trump_tweets<-tidy_trump_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))

trump_DTM <- DocumentTermMatrix(trump_corpus, control = list(wordLengths = c(2, Inf)))

inspect(trump_DTM[1:5,3:8])

tidy_trump_DTM<-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

tidy_trump_DTM2<-
  tidy_trump_tweets2 %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)


#In order to create a corpus of these tweets, we need to use the Corpus function within the tm package. First let’s install that package

install.packages("tm")
#Now let’s load the tm package in order to use its Corpus function:
  
  library(tm)
trump_corpus <- Corpus(VectorSource(as.vector(trumptweets$text))) 
trump_corpus

library(tidytext)
library(dplyr)
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
head(tidy_trump_tweets)

#A major advantage of tidytext format is that once the text has been tidy-ed, regular R functions can be used to analyze it instead of the specialized functions necessary to analyze a Corpus object. For example, to count the most popular words in Trump’s tweets, we can do the following:
  
  tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

  trump_corpus <- tm_map(trump_corpus, removeWords, stopwords("english"))
  #In tidytext we can remove stopwords as follows:
    
    data("stop_words")
  tidy_trump_tweets<-tidy_trump_tweets %>%
    anti_join(stop_words)
  #And now we can repeat the count of top words above:
    
    tidy_trump_tweets %>%
    count(word) %>%
    arrange(desc(n))  
    data("stop_words")
    
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
    
    #We can calculate the tf-idf for the Trump tweets databased in tidytext as follows:
    
    tidy_trump_tfidf<- trumptweets %>%
      select(created_at,text) %>%
      unnest_tokens("word", text) %>%
      anti_join(stop_words) %>%
      count(word, created_at) %>%
      bind_tf_idf(word, created_at, n)
    
    #Now let’s see what the most unusual words are:
    
    top_tfidf<-tidy_trump_tfidf %>%
      arrange(desc(tf_idf))
    
    top_tfidf$word[1]
    
    head(trumptweets$text)   
    
    #DICTIONARY METHOD
    
    economic_dictionary<-c("economy","unemployment","trade","tariffs")
    
    library(stringr)
    ## Warning: package 'stringr' was built under R version 3.5.2
    economic_tweets<-trumptweets[str_detect(trumptweets$text, paste(economic_dictionary, collapse="|")),]   
    
#SENTIMENT
    
    library(tidytext)
    head(get_sentiments("bing"))
    trump_tweet_sentiment <- tidy_trump_tweets %>%
      inner_join(get_sentiments("bing")) %>%
      count(created_at, sentiment) 
    
    head(trump_tweet_sentiment)    

    #Now let’s make a visual that compares the frequency of positive and negative tweets by day. 
    #To do this, we’ll need to work a bit with the created_at variable—more specifically, 
    #we will need to transform it into a “date” object that we can use to pull out the day 
    #during which each tweet was made:
      
      tidy_trump_tweets$date<-as.Date(tidy_trump_tweets$created_at, 
                                      format="%Y-%m-%d %x")  
      trump_sentiment_plot <-
        tidy_trump_tweets %>%
        inner_join(get_sentiments("bing")) %>% 
        filter(sentiment=="negative") %>%
        count(date, sentiment)    
      ## Joining, by = "word"
      
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

      #There appears to be an upward trend. Is it possible that this increase is being shaped by Trump’s approval rating? Let’s take a look, downloading data from the 
      #survey polling group 538 for the same time period as our Twitter data above:
      
      trump_approval<-read.csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv")
      
      trump_approval$date<-as.Date(trump_approval$modeldate, format="%m/%d/%Y")
      
      approval_plot<-
        trump_approval %>%
        filter(subgroup=="Adults") %>%
        filter(date>min(trump_sentiment_plot$date)) %>% 
        group_by(date) %>%
        summarise(approval=mean(approve_estimate))
      
      #plot
      ggplot(approval_plot, aes(x=date, y=approval))+
        geom_line(group=1)+
        theme_minimal()+
        ylab("% of American Adults who Approve of Trump")+
        xlab("Date")      
      
      #Top keywords
      top_words %>%
        slice(1:80) %>%
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
      
      
      #TOPIC MODELS
      
      library(readtext) # To read different types of text data into R
      library(quanteda.textmodels) # To perform several well-known text models such as wordscores, for instance
      library(seededlda)
      
      library(tidyverse)
      
      library(stm) # Structural topic model package 
      
      library(topicmodels)

      
      trump_topic_model<-LDA(tidy_trump_DTM2, k=6, control = list(seed = 321))
      library(tidytext)
      library(dplyr)
      library(ggplot2)
      
      trump_topic <- tidy(trump_topic_model, matrix = "beta")
      
      trump_top_terms <- 
        trump_topic %>%
        group_by(topic) %>%
        top_n(5, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      
      trump_top_terms %>%
        mutate(term = reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()
      
      trumptweets_small <- trumptweets[1:100,1:6]
      
      posterior(trump_topic_model,trumptweets_small)
      
toptopics <- as.data.frame(cbind(document = row.names(trumptweets),topic = apply(trumptweets,1,
                                                     function(x) names(trumptweets)[which(x==max(x))])))
     