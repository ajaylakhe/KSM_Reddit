# KSM_Reddit
library(RSQLite)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(RColorBrewer)


db <- src_sqlite('../input/database.sqlite', create = F)

query <- "SELECT subreddit, body, ups FROM May2015 
  WHERE body LIKE 'I hate%'"

db_subset <- db %>%
  tbl(sql(query))
  
df <- data.frame(db_subset)

crazy <- df %>% 
  group_by(subreddit) %>% 
  summarize(N = n()) %>% 
  top_n(50, N) %>% 
  arrange(N) %>%
  mutate(subreddit = factor(subreddit, levels=subreddit, ordered=TRUE))

ggplot(crazy, aes(x = subreddit, y = N)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_bw() +
  xlab("Number of comments") +
  ggtitle("Reddits")
