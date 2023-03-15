remove(list=ls())

library(readxl)
library(dplyr)
library(tidytext)

amazon_alexa <- read.csv("Amazon_Alexa_Final.csv")

df <- data.frame(amazon_alexa)
amazon_alexa_reviews <- df$lemma
head(amazon_alexa_reviews, n=10)

alexa_df <- data.frame(id = 1:3150 , text = (amazon_alexa_reviews))

df_tokens <- alexa_df %>%
  unnest_tokens(word, text)
df_tokens

df_tokens %>%
  count(word, sort = TRUE)

library(ggplot2)

df_tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

bing_word_counts <- df_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

library(wordcloud)

df_tokens %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

df_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

bing_word_counts <- df_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

library(tidyverse)

tf <- df_tokens %>%
  count(id, word) %>%
  group_by(id) %>%
  mutate(tf = n / sum(n))

tf

tf_idf <- tf %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

tf_idf

alexa_bigrams <- alexa_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

alexa_bigrams

alexa_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- alexa_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(id, bigram) %>%
  bind_tf_idf(bigram, id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library(widyr)
word_cors <- bigrams_separated %>%
  group_by(word1) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word1,word2, sort = TRUE)

word_cors

library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


