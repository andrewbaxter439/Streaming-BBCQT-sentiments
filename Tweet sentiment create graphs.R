library(rtweet)
library(tidyverse)
library(gganimate)
library(tidytext)
library(lubridate)
library(twitteR)


# Create graphs and tweet ------------------------------------------------------------------------------------

## Run in new session!

graphBBC <- function (t=60, tag="bbcqt") { # t is the length of the programme in minutes
  p <- ceiling(t/5)
  i = 0
  while (i < p){
    repeat {
      filenames <- list.files()
      seTime <- (now()-6*60) %>% 
        as.character(paste0(hour(.), minute(.), "-", hour(.), minute(.)+5))
      if (any(grepl(seTime, filenames))){
        bbcQTFile <- paste0(tag, seTime, ".json")
        seTime <- seTime
        break
      }
    }
    i <- i+1
    bbcqt <- parse_stream(bbcQTFile)
    bbcqt %>% 
      mutate(min=paste(hour(created_at), minute(created_at), sep=":")) %>% 
      group_by(min) %>% 
      unnest_tokens(word, text) %>%
      ungroup() %>% 
      select(min, word) %>% 
      inner_join(get_sentiments("nrc")) %>%
      group_by(min, sentiment) %>% 
      add_tally() %>% 
      summarise(total=sum(n)) %>% 
      group_by(min) %>% 
      filter(sentiment!="negative", sentiment!="positive") %>% 
      mutate(time=min,
             share=total/sum(total)) %>% 
      ggplot(aes(x=1, y=share, fill=sentiment, label=sentiment))+
      geom_bar(stat="identity", position="fill")+
      coord_polar(theta = "y") +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size=20, face="bold", colour="black", hjust = 1,
                                  margin = margin(b = -100)),
        line = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(-1,0,-5,0), "cm")
      ) +
      geom_text(aes(x=1.7, size=share, col=sentiment),
                position=position_stack(vjust=0.5)
      ) +
      scale_size(range=c(3,6)) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      labs(title="Time: {closest_state}") +
      transition_states(time, 2, 1, wrap = TRUE) +
      ease_aes("sine-in-out") -> sentimation
    
    # animate(sentimation, duration=5, fps=1, width=700, height=700)
    
    # positive or negative bar -----------------------------------------------------------------------------------
    
    bbcqt %>% 
      mutate(min=paste(hour(created_at), minute(created_at), sep=":")) %>% 
      group_by(min) %>% 
      unnest_tokens(word, text) %>%
      ungroup() %>% 
      select(min, word) %>% 
      inner_join(get_sentiments("nrc")) %>%
      group_by(min, sentiment) %>% 
      add_tally() %>% 
      summarise(total=sum(n)) %>% 
      group_by(min) %>% 
      filter(sentiment=="negative" | sentiment=="positive") %>% 
      mutate(time = min,
             total=ifelse(sentiment=="negative", total*-1, total)) %>% 
      group_by(sentiment) %>% 
      mutate(labpos=mean(total)/2) %>% 
      ggplot(aes(x=1, y=total, fill=sentiment)) +
      geom_bar(position="stack",stat="identity", width=0.2) +
      theme_minimal() +
      theme(legend.position="none",
            axis.text = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank()) +
      ylab("Total tweets") +
      geom_text(aes(x=1, y=labpos, label=sentiment), hjust=0.5) +
      xlim(0.9, 1.1) +
      transition_states(time, 2, 1, wrap = TRUE) +
      ease_aes("sine-in-out") -> positiveThoughts
    
    
    # animate and join -------------------------------------------------------------------------------------------
    
    PNgif <- animate(positiveThoughts, duration=10, fps=20, width=100, height=700)
    Sengif <- animate(sentimation, duration=10, fps=20,width = 700, height=700)
    
    pnMgif <- image_read(PNgif)
    SenMgif <- image_read(Sengif)
    
    joingif <- image_append(c(SenMgif[1], pnMgif[1]))
    for (i in 2:100){
      combined <- image_append(c(SenMgif[i], pnMgif[i]))
      joingif <- c(joingif, combined)
    }
    
    image_write(joingif, paste0("gifs/", tag, seTime, ".gif"))
    
    topHash <- bbcqt %>% 
      filter(is_quote==FALSE, is_retweet==FALSE, !is.na(hashtags)) %>% 
      pull(hashtags) %>% 
      unlist() %>% 
      enframe() %>% 
      count(value) %>% 
      arrange(desc(n)) %>% 
      filter(!grepl(tag, value, ignore.case = TRUE)) %>% 
      top_n(1)
    
    topMentions <- bbcqt %>% 
      filter(is_quote==FALSE, is_retweet==FALSE, !is.na(mentions_screen_name)) %>% 
      pull(mentions_screen_name) %>% 
      unlist() %>% 
      enframe() %>% 
      count(value) %>% 
      arrange(desc(n)) %>% 
      filter(!grepl("^bbc", value, ignore.case = TRUE)) %>% 
      top_n(3) %>% 
      pull(value)
    
    
    updateStatus(paste0("Graphing how folks are feeling on Twitter watching #",
                        tag,
                        " in the last five minutes.\n\n",
                        "Top mentions:\n@", topMentions[1], 
                        "/n@", topMentions[2],
                        "/n@", topMentions[3],
                        "/n/nTop Hastag: #", topHash),
                 mediaPath=paste0("gifs/", tag, seTime, ".gif"))
  }
}
