library(tidyverse)

rm(list=ls())

tr_pos <- read.csv("Stanford/Stanford Data/TrainPositiveSentences.csv")
tr_neg <- read.csv("Stanford/Stanford Data/TrainNegativeSentences.csv")
te_pos <- read.csv("Stanford/Stanford Data/TestPositiveSentences.csv")
te_neg <- read.csv("Stanford/Stanford Data/TestNegativeSentences.csv")
tr_unsub <- read.csv("Stanford/Stanford Data/TrainUnsupervisedSentences.csv")

sent_data <- bind_rows(tr_pos, tr_neg, te_pos, te_neg, tr_unsub)

theme_map <- read_tsv("Stanford/data.tsv")

sent_data <- merge(sent_data, theme_map, by.x = "IMDB_ID", by.y = "tconst", all.x = TRUE, all.y = FALSE)

write.csv(sent_data, "Stanford/Stanford Data/AllSentences.csv")

tr_pos <- read.csv("Stanford/Stanford Data/TrainPositive.csv")
tr_neg <- read.csv("Stanford/Stanford Data/TrainNegative.csv")
te_pos <- read.csv("Stanford/Stanford Data/TestPositive.csv")
te_neg <- read.csv("Stanford/Stanford Data/TestNegative.csv")
tr_unsub <- read.csv("Stanford/Stanford Data/TrainUnsupervised.csv")

sent_data <- bind_rows(tr_pos, tr_neg, te_pos, te_neg, tr_unsub)

theme_map <- read_tsv("Stanford/data.tsv")

sent_data <- merge(sent_data, theme_map, by.x = "IMDB_ID", by.y = "tconst", all.x = TRUE, all.y = FALSE)

write.csv(sent_data, "Stanford/Stanford Data/AllReviews.csv")