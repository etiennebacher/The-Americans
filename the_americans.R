library(here)
library(renv)
library(rvest)
library(tidyverse)


### Dont forget to do renv::restore()


#####################################################################
## Getting the dialogues ##
#####################################################################


###########################
## Test with episode 1 ##
###########################


page <- read_html("http://transcripts.foreverdreaming.org/viewtopic.php?f=116&t=15871&sid=4659be7fe06af87129c0d7d648b7d470")

page_text <- html_node(page, "div.postbody") %>%
  html_children() %>%
  xml2::xml_find_all("//div[contains(@class, 'postbody')]") %>%
  html_text(trim = TRUE)

# We want to focus on dialogues, so we remove the music (between ♪) and the descriptive info (between []) 
# https://stackoverflow.com/questions/45414913/gsub-and-remove-all-characters-between-and-in-r
# https://stackoverflow.com/questions/23966678/remove-all-text-between-two-brackets
page_text_cleaned <- page_text %>%
  gsub("♪[^♪]+♪", "", .) %>%
  gsub("\\n", " ", .) %>%
  gsub("\\t", " ", .) %>%
  gsub("\\[[^\\]]*\\]", "", ., perl = TRUE) %>%
  gsub("\\\\", "", .)
 
 
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


