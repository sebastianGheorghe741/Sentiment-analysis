library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(tidytext)
library(textdata)
library(Hmisc)
library(sentimentr)
library(zoo)
library(flextable)
library(rtweet)
library(tm)
library(ggplot2)

rtweet_app('')


df.btc <-search_tweets('bitcoin', n=15000, lang = 'en', include_rts = FALSE)
df.eth <- search_tweets('ethereum', n=10000, lang = 'en', include_rts = FALSE)
df.bnb <- search_tweets('binance', n=10000, lang = 'en', include_rts = FALSE)


# curatarea datelor -------------------------------------------------------
txtclean <- function(x, title){
  require(dplyr)
  require(stringr)
  require(tibble)
  x <- x %>%
    iconv(to = "UTF-8") %>%
    base::tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()%>%
    stringr::str_split(" ") %>%
    unlist() %>%
    tibble::tibble() %>%
    dplyr::select(word = 1, everything()) %>%
    dplyr::mutate(novel = title) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::mutate(word = str_remove_all(word, "\\W")) %>%
    dplyr::filter(word != "")
}


# bitcoin
bitcoin <- data.frame(df.btc$text)
colnames(bitcoin) <- 'text'
bitcoin$text<-gsub("https\\S*", "", bitcoin$text) 
bitcoin$text<-gsub("@\\S*", "", bitcoin$text) 
bitcoin$text<- gsub("amp", "", bitcoin$text) 
bitcoin$text<-gsub("[\r\n]", "", bitcoin$text)
bitcoin$text<-gsub("[[:punct:]]", "", bitcoin$text)
bitcoin$text <- gsub("[0-9]+", "", bitcoin$text)
bitcoin$text <- gsub("[^[:alnum:] ]", "", bitcoin$text)



bitcoin_clean <- txtclean(bitcoin, "bitcoin")




# ethereum
ethereum <- data.frame(df.eth$text)
colnames(ethereum)<-'text'

ethereum$text<-gsub("https\\S*", "", ethereum$text) 
ethereum$text<-gsub("@\\S*", "", ethereum$text) 
ethereum$text<- gsub("amp", "", ethereum$text) 
ethereum$text<-gsub("[\r\n]", "", ethereum$text)
ethereum$text<-gsub("[[:punct:]]", "", ethereum$text)
ethereum$text <- gsub("[0-9]+", "", ethereum$text)
ethereum$text <- gsub("[^[:alnum:] ]", "", ethereum$text)

ethereum_clean<- txtclean(ethereum, "ethereum")



# binance

binance <- data.frame(df.bnb$text)
colnames(binance) <- 'text'
binance$text<-gsub("https\\S*", "", binance$text) 
binance$text<-gsub("@\\S*", "", binance$text) 
binance$text<- gsub("amp", "", binance$text) 
binance$text<-gsub("[\r\n]", "", binance$text)
binance$text<-gsub("[[:punct:]]", "", binance$text)
binance$text <- gsub("[0-9]+", "", binance$text)
binance$text <- gsub("[^[:alnum:] ]", "", binance$text)



binance_clean <- txtclean(binance, 'binance')


crpt_anno <- rbind(darwin_clean, twain_clean, orwell_clean, lovecraft_clean) %>%
  dplyr::group_by(novel) %>%
  dplyr::mutate(words = n()) %>%
  dplyr::left_join(tidytext::get_sentiments("nrc")) %>%
  dplyr::mutate(novel = factor(novel),
                sentiment = factor(sentiment))


monede <- crpt_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  dplyr::filter(is.na(sentiment) == F) %>%
  dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))


# vizualizare -------------------------------------------------------------

# bar plot sentimente / moneda 1
monede %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  ggplot(aes(sentiment, percentage, fill = novel)) +    
  geom_bar(stat="identity",   
           position=position_dodge()) + 
  scale_fill_manual(name = "", values=c("orange", "gray70", "red", "grey30")) +
  theme_bw() +
  theme(legend.position = "top")

# bar plot sentimente / moneda 2

monede %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  dplyr::mutate(sentiment = factor(sentiment, 
                                   levels = c("anger", "fear", "disgust", "sadness",
                                              "surprise", "anticipation", "trust", "joy"))) %>%
  ggplot(aes(novel, percentage, fill = sentiment)) +    
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "right") +
  coord_flip()


# cuvinte importante
monede_impw <- crpt_anno %>%
  dplyr::filter(!is.na(sentiment),
                sentiment != "anticipation",
                sentiment != "surprise",
                sentiment != "disgust",
                sentiment != "negative",
                sentiment != "sadness",
                sentiment != "positive") %>%
  dplyr::mutate(sentiment = factor(sentiment, levels = c("anger", "fear",  "trust", "joy"))) %>%
  dplyr::group_by(novel) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::top_n(3) %>%
  dplyr::mutate(score = n/sum(n))



head(monede_impw, 10)



# top 3 cuvinte asociate fiecarei monede pe baza sentimentelor 
monede_impw %>%
  dplyr::group_by(novel) %>%
  slice_max(score, n = 20) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reorder(word, score), y = score, fill = word)) +
  facet_wrap(novel~sentiment, ncol = 4, scales = "free_y") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Words")




# calcularea si reprezentarea polaritatii ---------------------------------

monede %>%
  dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
  dplyr::select(-percentage, -words) %>%
  dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                positive = sentiment_sum-sentiment_freq) %>%
  dplyr::filter(sentiment != "positive") %>%
  dplyr::rename(negative = sentiment_freq) %>%
  dplyr::select(novel, positive, negative) %>%
  dplyr::group_by(novel) %>%
  dplyr::summarise(polarity = positive/negative) %>%
  ggplot(aes(reorder(novel, polarity, mean), polarity, fill = novel)) +    
  geom_bar(stat = "identity") + 
  geom_text(aes(y = polarity-0.1, label = round(polarity, 2)), 
            color = "white", size = 4) + 
  theme_bw() +
  labs(y = "Polarity\n(ration of positive to negative emitives)",
       x = "") +
  coord_cartesian(y= c(0,2)) +
  scale_y_continuous(breaks = seq(0,2,1),
                     labels = c("more negative", "neutral", "more positive")) +
  theme(legend.position = "none")



# calcularea polaritatii --------------------------------------------------


# bin
monede_bin <- crpt_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", 
                                      TRUE ~ sentiment),
                sentiment= case_when(sentiment == "0" ~ 0,
                                     sentiment == "positive" ~ 1,
                                     TRUE ~ -1),
                id = 1:n(),
                index = as.numeric(cut2(id, m=100))) %>%
  dplyr::group_by(novel, index) %>%
  dplyr::summarize(index = unique(index),
                   polarity = mean(sentiment))


ggplot(monede_bin, aes(index, polarity)) + 
  facet_wrap(vars(novel), scales="free_x") +
  geom_smooth(se = F, col = "black") + 
  theme_bw() +
  labs(y = "polarity ratio (mean by bin)",
       x = "index (bin)")




# moving average

monede_change <- crpt_anno %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::group_by(novel) %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", 
                                      TRUE ~ sentiment),
                sentiment= case_when(sentiment == "0" ~ 0,
                                     sentiment == "positive" ~ 1,
                                     TRUE ~ -1),
                id = 1:n()) %>%
  dplyr::summarise(id = id,
                   rmean=rollapply(sentiment, 100, mean, align='right', fill=NA)) %>%
  na.omit()


ggplot(monede_change, aes(id, rmean)) +    
  facet_wrap(vars(novel), scales="free_x") +
  geom_smooth(se = F, col = "black") + 
  theme_bw() +
  labs(y = "polarity ratio (rolling mean, k = 100)",
       x = "index (word in monograph)")



senti <- read.csv('scoruri.csv')



head(senti)

library(factoextra)
library(NbClust)

ind <- 1:3702
train <- senti[-ind, ]
test <- senti[ind, ]


library(naivebayes)

model <- naive_bayes(type~., data = train)
summary(model)

plot(model)

p <- predict(model, test)
t2 <- table(p, test$type)
t2


1-sum(diag(t2)) / sum(t2)







# clasificare -------------------------------------------------------------
library(ISLR)
library(pROC)
library(rpart)
library(rpart.plot)


arbore <- rpart(train$type~., train, method = 'class')

rpart.plot(arbore, extra = 100)

plot(arbore)
text(arbore, pretty=0)


senti.copy <- senti
senti.copy$score <- round(senti.copy$score, 3)
senti$type[1]

for(i in 1:length(senti.copy$type)){
  if(senti.copy$type[i] == 'neutral'){
    senti.copy$score[i] = runif(1, -0.200, 0.200)
    if(senti.copy$score[i] < -0.050){
      senti.copy$type[i] = 'negative'
    }
    else{
      if(senti.copy$score[i] >0.050){
        senti.copy$type[i] = 'positive'
      }
    }
  }
}

table(senti.copy$type)




# arbore 
train2 <- senti.copy[-ind, ]
test2 <- senti.copy[ind, ]

arbore2 <- rpart(train2$type~., train2, method = 'class')
rpart.plot(arbore2, extra = 100)

predictie2 <- predict(arbore2, test2, type = 'prob')


predictie21 <- predict(arbore, test, type="class")
confuzie.arbore21 <- table(test2$type, predictie21)
print(confuzie.arbore21)


mean(predictie21 != test2$type)
1-mean(predictie21 != test2$type)


# naive bayes

model2 <- naive_bayes(type~., data = train2)
summary(model2)

plot(model2)

p2 <- predict(model2, test2)
t <- table(p, test2$type)


1-sum(diag(t)) / sum(t)

sum(diag(t)) / sum(t)



ggplot(senti.copy, aes(type)) + geom_bar(aes(fill = factor(type)), color = 'black') + theme_bw() + theme(legend.position = 'none')

