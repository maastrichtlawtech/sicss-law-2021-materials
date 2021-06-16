library(tidyverse)

library(tidytext)

library(dplyr)

library(tm)

tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

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

democracy_dictionary<-c("media","elections","fake","democrats", "vote", "republicans", "win")

install.packages("stringr")
library(stringr)

democracy_tweets <- trumptweets[str_detect(trumptweets$text, paste(democracy_dictionary, collapse="|")),]


migration_dictionary<-c("border","illegal","borders","immigration", "migrant", "wall", "Mexico", "Muslim")

migration_tweets <- trumptweets[str_detect(trumptweets$text, paste(migration_dictionary, collapse="|")),]

election_dictionary<-c("election", "elections","fraud","democrats", "vote", "republicans", "win", "Russia")

election_tweets <- trumptweets[str_detect(trumptweets$text, paste(election_dictionary, collapse="|")),]

media_dictonary <- c("fake", "media", "news", "fox", "disinformation", "misinformation", "propaganda", "corrupt", "corruption")

media_tweets <- trumptweets[str_detect(trumptweets$text, paste(media_dictonary, collapse="|")),]



Count_words <- tidy_trump_tweets %>% 
  anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  group_by(word, format(as.Date(created_at),format="%m")) %>%
  dplyr::summarise(num_word_month = n()) %>%
arrange(desc(num_word_month)) 


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


Count_words[1:10,] %>%
  ggplot(aes(x=word, y=num_word_month))+
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


