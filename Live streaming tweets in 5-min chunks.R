library(rtweet)
library(tidyverse)
library(gganimate)
library(tidytext)
library(lubridate)

# Loop streaming for 5-minute intervals -----------------------------------
streamBBC <- function(t=60) { # t is the time-duration of show
  p <- ceiling(t/5)
  for (x in 1:p){
    sTime <- now() %>% 
      as.character(paste0(hour(.), minute(.)))
    eTime <- (now()+5*60) %>% 
      as.character(paste0(hour(.), minute(.)))
    bbcqt <- stream_tweets("#bbcqt",
                           timeout=60*5,
                           file_name = paste0("bbcqt", sTime, "-", eTime, ".json"))
  }
}

streamBBC(5)
