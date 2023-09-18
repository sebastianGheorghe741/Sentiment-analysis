library(rtweet)
library(tidyr)
library(tidytext)
library(tidyverse)
library(dplyr)

rtweet_app('AAAAAAAAAAAAAAAAAAAAAO4ZjAEAAAAAoUtYG%2F1CoXYnXw6Cojg1u51WJFY%3DD1LPl9XmPEkO1BGobDu4NZrks9jr5KxmVCRYwudQwxcHM5M3PZ')

df <-search_tweets('crypto', n=5000, lang = 'en', include_rts = FALSE)
class(df)
df <- as.data.frame(df)
df <- select(df, -metadata, -coordinates, -place, -quoted_status)
df <- select(df, -entities)
df <- select (df, - geo)
write.csv2(df, file = 'Tweeturi1.csv', row.names = FALSE)
?write.csv
library(syuzhet)
sa.value <- get_nrc_sentiment(df$text)
score <- colSums(sa.value[,])
View(as.matrix(score))

score_df <- data.frame(score)
View(score_df)
sa.score <- cbind(sentiment = row.names(score_df), 
                  score_df, row.names = NULL)
print(sa.score)
library(ggplot2)
ggplot(sa.score, aes(sentiment, score)) + geom_bar(aes(fill = sentiment), stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust=1))

install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)
library(RColorBrewer)
install.packages('wordcloud2')
library(wordcloud2)


tweets <- data.frame(df$text)
colnames(tweets) <- 'text'
tweets <- as.vector(tweets)
install.packages('tm')
library(tm)


tweets$text<-gsub("https\\S*", "", tweets$text) 
tweets$text<-gsub("@\\S*", "", tweets$text) 
tweets$text<- gsub("amp", "", tweets$text) 
tweets$text<-gsub("[\r\n]", "", tweets$text)
tweets$text<-gsub("[[:punct:]]", "", tweets$text)

# you should either work with a copy from now on or implement the copy into the original :)
words.copy <- words

words.copy <- words.copy[-which(words.copy$word == 'it'),]

nono <- c('be', 'and', 'of', 'a', 'is', 'this',
          'in', 'are', 'i', 'the', 'to', 'am', 'their', 'that', 'was', 'were')
library(tidytext)
tweets_words <-  tweets %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- tweets_words %>% count(word, sort=TRUE)
set.seed(1234)
wordcloud(words = words.copy$word, freq=words.copy$n, min.freq = 1, 
          max.words = 200, random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))



# wordcloud for sentiments...?
library(sentimentr)
profanity_list <- unique(tolower(lexicon::profanity_alvarez))
words_data <- df %>% select(text)  %>% 
  unnest_tokens(word, text)

words_data %>% count(word, sort = TRUE)


words_data <- words_data %>% filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s'))
words_data2 <- words_data %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
head(words_data2, n = 10)
wordcloud(words = words_data2$word, freq = words_data2$n, min.freq = 1, 
          max.words = 200, random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'))

words_data2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)
words_data %>% filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s', profanity_list)) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 50)








# checking the data set ---------------------------------------------------

class(df$created_at)
max(df$created_at)
min(df$created_at)
df2 <- search_tweets('#crypto', include_rts = FALSE, n = 10000, lang = 'en')
max(df2$created_at)
min(df2$created_at)
