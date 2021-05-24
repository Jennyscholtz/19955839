library(tidyverse)
library(dplyr)

df1 <- readRDS(file = "~/Meesters/19955839/Question 3/data/Tweets/tweets_bbc.rds")
df2 <- readRDS(file = "~/Meesters/19955839/Question 3/data/Tweets/tweets_cnn.rds")
df3 <- readRDS(file = "~/Meesters/19955839/Question 3/data/Tweets/tweets_eco.rds")

Tweets <- bind_rows(df1, df2, df3)


Tweets$photos <- Tweets$photos %>% na_if("[]")
Tweet$photos <- Tweets$photos %>% replace_na(0)
Tweet$photos <- Tweets$photos %>%
    mutate_if(., grepl('https',.), ~replace(.,grepl('https',.), 1))

Tweets$photos %>% replace(.,sapply(photos, function(.)grepl('https',.)), "1")

Tweets %>% group_by(name) %>%
    summarise

