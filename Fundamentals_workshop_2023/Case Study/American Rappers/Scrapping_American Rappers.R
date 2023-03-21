# Title: American Rappers Data Scrapping
# Purpose: to illustrate 
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: 2023

# Let's explore Kanye West's repertoire.

# Load packages----------------------------------------------------------------------------------------------------------------------------------------------
library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(ggplot2)
library(tidyr)


# Get URL and setup song nodes for scrapping ---------------------------------------------------------------------------------------------------------------

#EVERYTHING UNTIL LINE 59 IS THE SETUP AND SCRAPPING CODE
#WE ALREADY RUN IT AND SAVED THE DATA, SO YOU CAN JUMP TO LINE 60

artist_url <- 'https://www.azlyrics.com/w/west.html'
#you can change the link above to the artist of your choice
song_nodes <- read_html(artist_url) %>% # load the html
  html_nodes("#listAlbum a")
song_links <-  html_attr(song_nodes, name='href')
song_links <- na.omit(ifelse(startsWith(song_links, "/lyrics/"),
                             song_links, NA_character_))

# Scrape lyrics and albums----------------------------------------------------------------------------------------------------------------------------------

Kanye_West <- data.frame()
#remember to rename line 32 acc. to your artist of choice AND to change it in line 54

for(i in 1:length(song_links)) {
  # always nice to know where a long program is
  message(str_c('scraping ', i, ' of ', length(song_links) ))
  # scrape the text of a song
  lyrics <- paste0("https://www.azlyrics.com", song_links[i]) %>%
    read_html() %>%
    html_nodes("br+ div") %>%
    html_text()
  lyrics <- lyrics[1]
  title <-paste0("https://www.azlyrics.com", song_links[i]) %>%
    read_html() %>%
    html_nodes(".ringtone+ b") %>%
    html_text()
  title <- title[1]
  album <-paste0("https://www.azlyrics.com", song_links[i]) %>%
    read_html() %>%
    html_nodes(".breadcrumb+ .noprint .songinalbum_title , .breadcrumb+ .noprint b") %>%
    html_text()
  album <- album[1]
  Kanye_West <- rbind(Kanye_West, tibble(title = title,
                                         lyrics = lyrics,
                                         album = album))
  Sys.sleep(10)
}

# save the data to your working directory
# saveRDS(Kanye_West, file = "Kanye_West.RDS")
