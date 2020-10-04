library(renv)
library(rvest)
library(tidyverse)
library(tidytext)


### Dont forget to do renv::restore()


#####################################################################
## Getting the dialogues ##
#####################################################################


###########################
## Test with episode 1 ##
###########################


page <- read_html("http://transcripts.foreverdreaming.org/viewtopic.php?f=116&t=15871")

page_text <- html_node(page, "div.postbody") %>%
  html_children() %>%
  xml2::xml_find_all("//div[contains(@class, 'postbody')]") %>%
  html_text(trim = TRUE)

# We want to focus on dialogues, so we remove the music (between ♪) and the descriptive info (between []) 
# https://stackoverflow.com/questions/45414913/gsub-and-remove-all-characters-between-and-in-r
# https://stackoverflow.com/questions/23966678/remove-all-text-between-two-brackets
page_text_cleaned <- page_text %>%
  gsub("♪♪", "", .) %>% # double music symbol
  gsub("♪ [^♪]+♪", "", .) %>% # text between music symbol (= lyrics)
  gsub("\\n", " ", .) %>% # new line symbol
  gsub("\\t", " ", .) %>% # \t
  gsub("\\[[^\\]]*\\]", "", ., perl = TRUE) %>% # text between brackets
  gsub("\\([^\\)]*\\)", "", ., perl = TRUE) %>% # text between parenthesis
  gsub("\\\\", "", .) %>% # backslash
  gsub("\\(adsbygoogle = window.adsbygoogle \\|\\| \\).push\\(\\{\\}\\);", "", .) %>% # ads
  gsub("Philip:", "", .) %>% 
  gsub("Elizabeth:", "", .) %>%
  gsub("Paige:", "", .) %>%
  gsub("Henry:", "", .) %>%
  gsub("Stan:", "", .) 
 
 
# How to generalize to all episodes?

# The links are almost identical between episodes, just one id changes.
# Ex:
# Episode 1: http://transcripts.foreverdreaming.org/viewtopic.php?f=116&t=15871
# Episode 3: http://transcripts.foreverdreaming.org/viewtopic.php?f=116&t=15872
# Only the argument t changes




###########################
## Obtain all values for t ##
###########################

# For each menu page (3 in total), I extract the id of each episode (use the element inspector for that). We also remove the two first ids, which correspond to "Online Store" and "Board Updates"

##########
## Menu 1 ##
##########

menu_1 <- read_html("http://transcripts.foreverdreaming.org/viewforum.php?f=116")

episode_ids_1 <- html_node(menu_1, "body") %>%
  html_children() %>%
  xml2::xml_find_all("//a[contains(@class, 'topictitle')]") %>%
  sub('.*f=116&amp;t= *(.*?) *&amp;sid.*', "\\1", .) %>%
  as.numeric()

episode_ids_1 <- episode_ids_1[-c(1, 2)]


##########
## Menu 2 ##
##########

menu_2 <- read_html("http://transcripts.foreverdreaming.org/viewforum.php?f=116&start=25")

episode_ids_2 <- html_node(menu_2, "body") %>%
  html_children() %>%
  xml2::xml_find_all("//a[contains(@class, 'topictitle')]") %>%
  sub('.*f=116&amp;t= *(.*?) *&amp;sid.*', "\\1", .) %>%
  as.numeric() 

episode_ids_2 <- episode_ids_2[-c(1, 2)]


##########
## Menu 3 ##
##########

menu_3 <- read_html("http://transcripts.foreverdreaming.org/viewforum.php?f=116&start=50")

episode_ids_3 <- html_node(menu_3, "body") %>%
  html_children() %>%
  xml2::xml_find_all("//a[contains(@class, 'topictitle')]") %>%
  sub('.*f=116&amp;t= *(.*?) *&amp;sid.*', "\\1", .) %>%
  as.numeric()

episode_ids_3 <- episode_ids_3[-c(1, 2)]


##########
## All episode ids ##
##########

episode_ids_total <- c(episode_ids_1, episode_ids_2, episode_ids_3)



###########################
## Extract dialogues of all episodes available ##
###########################


list_dialogues <- purrr::map(episode_ids_total, .f = function(x) {
  
  page <- read_html(paste0("http://transcripts.foreverdreaming.org/viewtopic.php?f=116&t=", x))
  
  page_text <- html_node(page, "div.postbody") %>%
    html_children() %>%
    xml2::xml_find_all("//div[contains(@class, 'postbody')]") %>%
    html_text(trim = TRUE)
 
  page_text_cleaned <- page_text %>%
    gsub("♪♪", "", .) %>%
    gsub("♪ [^♪]+♪", "", .) %>%
    gsub("\\n", " ", .) %>%
    gsub("\\t", " ", .) %>%
    gsub("\\[[^\\]]*\\]", "", ., perl = TRUE) %>%
    gsub("\\([^\\)]*\\)", "", ., perl = TRUE) %>%
    gsub("\\\\", "", .) %>%
    gsub("\\(adsbygoogle = window.adsbygoogle \\|\\| \\).push\\(\\{\\}\\);", "", .) %>%
    gsub("Philip:", "", .) %>%
    gsub("Elizabeth:", "", .) %>%
    gsub("Paige:", "", .) %>%
    gsub("Henry:", "", .) %>%
    gsub("Stan:", "", .) 
  
  return(page_text_cleaned)
  
})




#####################################################################
## Analyzing the dialogues ##
#####################################################################

list_dialogues_words <- map(list_dialogues, .f = function(x) {
  as_tibble(x) %>%
    unnest_tokens(word, value)
})   


###########################
## Number of words per episode per season ##
###########################

map(list_dialogues_words, nrow) %>%
  unlist() %>%
  as_tibble() %>%
  tibble::rowid_to_column() %>%
  mutate(
    season = case_when(
      rowid %in% c(1:13) ~ 1,
      rowid %in% c(14:26) ~ 2,
      rowid %in% c(27:39) ~ 3,
      rowid %in% c(40:52) ~ 4
    )
  ) %>%
  group_by(season) %>%
  mutate(rowid = c(1:13)) %>%
  ungroup() %>%
  ggplot(aes(x = rowid, y = value, color = factor(season))) + 
  geom_line() +
  scale_x_discrete(name = "Episode number", limits = factor(c(1:13))) +
  scale_y_continuous(name = "Number of words") +
  ggtitle("Number of words per episode per season") +
  scale_color_discrete(name = "Season")


###########################
## Words most said ##
###########################

onomatopoeia <- c("hey", "uh", "um", "yeah", "uhh")

### In episode 1 

list_dialogues_words[[1]] %>%
  filter(!(word %in% onomatopoeia)) %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  geom_text(aes(label = n), nudge_y = 1) +
  coord_flip() +
  ggtitle("Most said words in episode 1 of season 1")


### In season 1

list_dialogues_words[1:13] %>%
  unlist() %>%
  as_tibble() %>%
  rename("word" = "value") %>%
  filter(!(word %in% onomatopoeia)) %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  geom_text(aes(label = n), nudge_y = 10) +
  coord_flip() +
  ggtitle("Most said words in season 1")


###########################
## Sentiment analysis ##
###########################

dialogues_sentiment <- map_dfr(list_dialogues_words, function(x) {
  x %>%
    inner_join(tidytext::sentiments) %>%
    count(sentiment)
}) %>%
  mutate(
    season = rep(c(1:4), each = 26),
    episode = rep(rep(1:13, each = 2), times = 4)
  )


### Number of positive and negative words per episode and per season

dialogues_sentiment %>%
  ggplot(aes(x = episode, y = n, color = factor(sentiment))) + 
  geom_line() +
  scale_x_discrete(name = "Episode number", limits = factor(c(1:13))) +
  scale_y_continuous(name = "Number of words") +
  ggtitle(paste0("Number of positive and negative words per episode and per season")) +
  facet_wrap(~ season) +
  scale_color_discrete("Sentiment")


### Details on positive and negative words in season 1

list_dialogues_words[1:13] %>%
  unlist() %>%
  as_tibble() %>%
  rename("word" = "value") %>%
  inner_join(tidytext::sentiments) %>%
  group_by(word) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct() %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free_y") +
  geom_text(aes(label = n), nudge_y = 10) + 
  ggtitle("Positive and negative words most used in season 1")













