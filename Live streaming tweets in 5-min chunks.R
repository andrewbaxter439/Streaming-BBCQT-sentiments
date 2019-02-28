library(rtweet)
library(tidyverse)
library(gganimate)
library(tidytext)
library(lubridate)
library(magrittr)

# Loop streaming for 5-minute intervals -----------------------------------
streamBBC <- function(t)
  p <- t/5
  sTime <- now() %>% 
    as.character(paste0(hour(.), minute(.)))
  eTime <- now() %>% 
    as.character(paste0(hour(.), minute(.)+5))
for (x in 1:p){
  bbcqt <- stream_tweets("#bbcqt",
            timeout=60*5,
            file_name = paste0("bbcqt", sTime, "-", eTime, ".json"),
            screen_name!="andybaxter")
}
