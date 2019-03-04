library(rtweet)
library(tidyverse)
library(gganimate)
library(tidytext)
library(lubridate)

# Loop streaming for 5-minute intervals -----------------------------------
streamInBlocks <- function(t=60, m=5, tag="bbcqt") {  # t is the time-duration of show
  p <- ceiling(t/m)                                   # m is the length of the slots
  for (x in 1:p){                                     # tag is the hashtag (no hash)
    seTime <- now() %>% 
      as.character(paste0(hour(.), minute(.), "-", hour(.), minute(.)+m))
    bbcqt <- stream_tweets(paste0("#", tag),
                           timeout=60*m,
                           file_name = paste0(tag, seTime, ".json"))
  }
}

streamInBlocks(5)
