library(here)
library(renv)
library(rvest)
library(tidyverse)


### Dont forget to do renv::restore()


# Getting the dialogues ---------------------------------------------------


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
 


