# KSM_Reddit

require(RSQLite)
require(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

max_words <- 125

db <- src_sqlite('../input/database.sqlite', create = F)

query <- "SELECT body FROM May2015 
  WHERE controversiality == 1 and body LIKE 'I hate%'"

db_subset <- db %>%
  tbl(sql(query))
          
db_subset <- data.frame(db_subset)

corp <- Corpus(VectorSource(db_subset$body))
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removeWords, c("also","isnt","use","doesnt","see","take","wasnt","said","let","cant","didnt","just","theyre","going","many", "thats","will","much","made","used","got","youre","get","though"))

dtm <- DocumentTermMatrix(corp,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))
dtm_sparse <- removeSparseTerms(dtm, 0.995)
post_words <- as.data.frame(as.matrix(dtm_sparse))

total_words <- data.frame(words = colnames(post_words),
                          counts = colSums(post_words))
                          

wordcloud(words = total_words$words,
          freq=total_words$counts, 
          max.words = max_words,
          color = brewer.pal(8,"Dark2"))
          
print(total_words$words)
