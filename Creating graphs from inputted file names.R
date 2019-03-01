graph <- function(bbcQTFile){
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
    plot.title = element_text(size=20, face="bold", colour="black", hjust = 0.5),
    line = element_blank(),
    panel.background = element_blank()
  ) +
  geom_text(aes(x=1.7, size=share, col=sentiment),
            position=position_stack(vjust=0.5)
  ) +
  scale_size(range=c(3,6)) +
  labs(title="Time: {closest_state}") +
  transition_states(time, 2, 1, wrap = TRUE) +
  ease_aes("sine-in-out") -> sentimation

animate(sentimation, duration=10, fps=20, width=700, height=700) %>% 
  anim_save(paste0("bbcqt", seTime, ".gif"), animation = .)
}