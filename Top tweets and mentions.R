bbcqt %>% 
  filter(is_quote==FALSE, is_retweet==FALSE, !is.na(hashtags)) %>% 
  pull(hashtags) %>% 
  unlist() %>% 
  enframe() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  filter(!grepl("bbcqt|bbcquestiontime", value, ignore.case = TRUE)) %>% 
  top_n(1)

bbcqt %>% 
  filter(is_quote==FALSE, is_retweet==FALSE, !is.na(mentions_screen_name)) %>% 
  pull(mentions_screen_name) %>% 
  unlist() %>% 
  enframe() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  filter(!grepl("^bbc", value, ignore.case = TRUE)) %>% 
  top_n(3) %>% 
  pull(value)
