graphFromFile <- function (filename="", tag="bbcqt", m=5, tweet=TRUE, reply=FALSE, height=500, width=600) { # t is the length of the programme in minutes
seTime <- gsub("(^.*)(\\d\\d\\d\\d-\\d\\d\\d\\d)(.*$)", "\\2",filename)
  bbcqt <- parse_stream(filename)
  bbcqt %>% 
    mutate(time=format(created_at, "%H:%M")) %>% 
    group_by(time) %>% 
    unnest_tokens(word, text) %>%
    ungroup() %>% 
    select(time, word) %>% 
    inner_join(get_sentiments("nrc")) %>%
    group_by(time, word, sentiment) %>% 
    summarise(total=n()) %>% 
    group_by(time, sentiment) %>% 
    summarise(total=sum(total))%>% 
    group_by(time) %>% 
    filter(sentiment!="negative", sentiment!="positive") %>% 
    mutate(share=total/sum(total)) %>% 
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
    mutate(time=format(created_at, "%H:%M")) %>% 
    group_by(time) %>% 
    unnest_tokens(word, text) %>%
    ungroup() %>% 
    mutate(nsec=ifelse(time==min(time), 60 - second(min(created_at)),
                       ifelse(time==max(time), second(max(created_at))+1, 60))) %>% 
    select(time, word, nsec) %>% 
    inner_join(get_sentiments("nrc")) %>%
    group_by(time, word, sentiment, nsec) %>% 
    summarise(total=n()) %>% 
    group_by(time, sentiment) %>% 
    summarise(total=sum(total)/mean(nsec)) %>% 
    group_by(time) %>% 
    filter(sentiment=="negative" | sentiment=="positive") %>% 
    mutate(total=ifelse(sentiment=="negative", total*-1, total)) %>% 
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
    geom_text(aes(x=1.15, y=labpos, label=sentiment), hjust=0) +
    xlim(0.9, 1.55) +
    transition_states(time, 2, 1, wrap = TRUE) +
    ease_aes("sine-in-out") -> positiveThoughts
  
  
  # animate and join -------------------------------------------------------------------------------------------
  
  PNgif <- animate(positiveThoughts, duration=10, fps=15, width=width/6, height=height)
  Sengif <- animate(sentimation, duration=10, fps=15, width = 5*width/6, height=height)
  
  pnMgif <- image_read(PNgif)
  SenMgif <- image_read(Sengif)
  
  joingif <- image_append(c(SenMgif[1], pnMgif[1]))
  for (j in 2:150){
    combined <- image_append(c(SenMgif[j], pnMgif[j]))
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
    top_n(1) %>% 
    pull(value)
  
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
  
  tweet_message <- paste0("Graphing how folks are feeling on Twitter watching #",
                          tag,
                          " in the last ", m, " minutes.\n\n",
                          "Top mentions:\n@", topMentions[1], 
                          "\n@", topMentions[2],
                          "\n@", topMentions[3],
                          "\n\nTop Hashtag: #", topHash[1])
  
  if (tweet){
    updateStatus(tweet_message,
                 mediaPath=paste0("gifs/", tag, seTime, ".gif"),
                 inReplyTo = ifelse(reply==TRUE,
                                    get_timeline("andybaxter", n=1) %>% pull(status_id),
                                    NULL),
                 bypassCharLimit = TRUE)
  } else {
    print(tweet_message)
  }
}