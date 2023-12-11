library(tidyverse)
library(tidytext)
library(stringr)
library(qdap)
library(foreach)
library(doSNOW)

rm(list=ls())

#Setup backend to use many processors
totalCores = 8

#Leave two cores
cluster <- makeCluster(totalCores[1]-2) 
registerDoSNOW(cluster)

# Read Data
getReviews <- function(set = "train", sent = "pos")
{
  urls <- read.delim(paste0("Stanford/Stanford Data/", set, "/urls_", sent, ".txt"), header = FALSE)
  
  files <- list.files(paste0("Stanford/Stanford Data/", set, "/", sent, "/"), full.names = TRUE)
  
  iterations <- length(files)
  
  pb <- txtProgressBar(max = iterations, style = 3)
  
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  reviews <- data.frame()
  
  reviews <- foreach(i = 1:iterations, .combine = rbind,
                     .packages = c("tidyverse", "tidytext", "stringr", "qdap"), 
                     .options.snow = opts) %dopar% {
                       # cat(paste0(i, "/", length(files), "\r"))
                       
                       review_path <- files[i]
                       
                       review_text <- read.delim(review_path, header = FALSE)[1,1]
                       
                       # path_id <- review_path %>% str_extract("[0-9]+_") %>% str_extract("[0-9]+") %>% as.numeric() + 1
                       path_id <- review_path %>% str_extract("[0-9]+(?=_)") %>% as.numeric +1
                       
                       imdb_url <- urls[path_id, 1] %>% str_replace("usercomments", "")
                       
                       imdb_id <- imdb_url %>% str_extract("/tt[0-9]+/") %>% str_replace_all("/", "")
                       
                       # rating <- review_path %>% str_extract("_[0-9]+.txt$") %>% str_extract("[0-9]+") %>% as.numeric()
                       rating <- review_path %>% str_extract("[0-9]+(?=[.txt])") %>% as.numeric
                       
                       data.frame(Set = set, Sent = sent, Path_ID = path_id, IMDB_ID = imdb_id, IMDB_URL = imdb_url, Review = review_text, Rating = rating)
                     }
  
  reviews
  
}

scoreSentiments <- function(reviews, sentiment = TRUE)
{
  reviews <- reviews[!is.na(reviews$Review),]
  
  # reviews$SentimentScore = NA
  # reviews$SentimentVar = NA
  
  iterations <- nrow(reviews)
  
  pb <- txtProgressBar(max = iterations, style = 3)
  
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # for(i in 1:nrow(reviews))
  reviews_sent <- foreach(i = 1:iterations, .combine = rbind,
                     .packages = c("tidyverse", "tidytext", "stringr", "qdap"), 
                     .options.snow = opts) %dopar% {
                       # cat(paste0(i, "/", nrow(reviews), "\r"))
                       
                       reviews_sent <- data.frame()
                       
                       text <- reviews$Review[i]
                       
                       sent=qdap::sent_detect(text)
                       
                       if(length(sent)==0 || all(is.na(sent)))
                       {
                         return(NULL)
                       }
                       
                       overallSentimentScore <- NA
                       overallSentimentVar <- NA
                       
                       if(sentiment)
                       {
                         overallPol=qdap::polarity(iconv(stringi::stri_join_list(qdap::rm_stopwords(sent), sep = " "), to = 'UTF-8'), constrain = TRUE)
                         
                         overallSentimentScore <- overallPol$group$ave.polarity
                         overallSentimentVar <- overallPol$group$stan.mean.polarity
                       }
                       
                       for(s in 1:length(sent))
                       {
                         sent[s] <- gsub("[[:punct:]]", " ", sent[s])
                         #---------------------------------------------------------------------
                         ##removing stopwords
                         sentencewithoutstopwords <- qdap::rm_stopwords(sent[s])
                         sent_new <- paste(sentencewithoutstopwords[[1]], collapse = " ")
                         #---------------------------------------------------------------------
                         
                         sentPol <- list(group = NA)
                         
                         if(sentiment)
                         {
                           sentPol=qdap::polarity(sent_new, constrain = TRUE)
                         }
                         
                         reviews_sent <- rbind(reviews_sent, cbind(reviews[i,], data.frame(OverallSent = overallSentimentScore,
                                                                       overallSentVar = overallSentimentVar,
                                                                       Sentence = sent_new, SentimentScore = sentPol$group)))
                       }
                       
                       reviews_sent
                     }
  
  # reviews = as.data.frame(do.call(rbind,lapply(reviews_comb,function(x){x[[1]]})))
  # reviews_sent  = as.data.frame(do.call(rbind,lapply(reviews_comb,function(x){x[[2]]})))
  
  reviews <- unique(reviews_sent[,c(1:9)])
  
  list(reviews, reviews_sent)
}

# Positive Train Data
tr_pos <- getReviews("train", "pos")
tr_pos <- scoreSentiments(tr_pos)

write.csv(tr_pos[[1]], "Stanford/Stanford Data/TrainPositive.csv")
write.csv(tr_pos[[2]], "Stanford/Stanford Data/TrainPositiveSentences.csv")


# Negative Train Data
tr_neg <- getReviews("train", "neg")
tr_neg <- scoreSentiments(tr_neg)

write.csv(tr_neg[[1]], "Stanford/Stanford Data/TrainNegative.csv")
write.csv(tr_neg[[2]], "Stanford/Stanford Data/TrainNegativeSentences.csv")


# Positive Test Data
te_pos <- getReviews("test", "pos")
te_pos <- scoreSentiments(te_pos)

write.csv(tr_pos[[1]], "Stanford/Stanford Data/TestPositive.csv")
write.csv(tr_pos[[2]], "Stanford/Stanford Data/TestPositiveSentences.csv")


# Negative Train Data
te_neg <- getReviews("test", "neg")
te_neg <- scoreSentiments(te_neg)

write.csv(tr_neg[[1]], "Stanford/Stanford Data/TestNegative.csv")
write.csv(tr_neg[[2]], "Stanford/Stanford Data/TestNegativeSentences.csv")

# Unsupervised Train Data
tr_unsup <- getReviews("train", "unsup")
tr_unsup <- scoreSentiments(tr_unsup, FALSE)

write.csv(tr_unsup[[1]], "Stanford/Stanford Data/TrainUnsupervised.csv")
write.csv(tr_unsup[[2]], "Stanford/Stanford Data/TrainUnsupervisedSentences.csv")


#Stop cluster
stopCluster(cluster)

