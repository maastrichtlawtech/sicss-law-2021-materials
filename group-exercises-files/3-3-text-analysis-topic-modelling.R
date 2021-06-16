###Day3(3) Topic Modelling
library(topicmodels)
data("AssociatedPress")
#Latent Dirichlet Allocation; we select number of topics (k=10) 
#argument to pass a random number (321) to seed the assignment of topics to each word in the corpus
AP_topic_model<-LDA(AssociatedPress, k=10, control = list(seed = 321))
library(tidytext)
library(dplyr)
library(ggplot2)

AP_topics <- tidy(AP_topic_model, matrix = "beta")
#selecting top_n=10 most popular words in k=10 topics
ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
##Structural topic modeling
install.packages("stm")
library(stm)
google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" 
# google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)
processed <- textProcessor(poliblogs$documents, metadata = poliblogs)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)
plot(First_STM)
