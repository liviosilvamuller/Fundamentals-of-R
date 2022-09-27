# Title: Fundamentals of R
# Purpose: Block 1 - Case Study 2: Kayne West
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

# Does Kayne West swears more, or less, in time?

# Load packages
library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(ggplot2)

# Get URL and setup song nodes for scrapping 
artist_url <- 'https://www.azlyrics.com/w/west.html'
song_nodes <- read_html(artist_url) %>% # load the html
  html_nodes("#listAlbum a")
song_links <-  html_attr(song_nodes, name='href')
song_links <- na.omit(ifelse(startsWith(song_links, "/lyrics/"),
                             song_links, NA_character_))

# create dataset and scrape lyrics and albums
Kayne_West <- data.frame()
for(i in 1:length(song_links)) {
  # always nice to know where a long program is
  message(str_c('scraping ', i, ' of ', length(song_links) ))
  # scape the text of a song
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
  Kayne_West <- rbind(Kayne_West, tibble(title = title,
                                         lyrics = lyrics,
                                         album = album))
  Sys.sleep(10) 
}

# save the data to your working directory
# saveRDS(Kayne_West, file = "Kayne_West.RDS")
# If you do not want to scrape,
# load the data already scraped from Moodle materials!

# Clean variables and text cleaning
# Keep only songs where album is known
Kayne_West$album <- ifelse(startsWith(Kayne_West$album, "album"),
                           Kayne_West$album, NA_character_)
# Tip: ifelse is cool BTW!
# ifelse is a vectorized function that works through elements in string.
# Very helpful for coding when conditions are to be met!
# Extract year from string (4 digits)
Kayne_West$year <- as.numeric(stringr::str_extract_all(Kayne_West$album,
                                                       "[:digit:]{4}"))
# Remove obs that are missing for an album or year
Kayne_West <- na.omit(Kayne_West)
# Should we clean the text or not?
# We can remove punctuations, change all to lower case, and remove signs.
Kayne_West$lyrics <- tm::removePunctuation(Kayne_West$lyrics)
Kayne_West$lyrics <- tolower(Kayne_West$lyrics)
Kayne_West$lyrics <- gsub("\r|\n", " ", Kayne_West$lyrics) # sub markers

# Create a dictionary: can you help?
swear_words <- "fuck"

# Count appearances in songs and add as variable in data.
Kayne_West$swear_words <- stringr::str_count(Kayne_West$lyrics, swear_words)

# Plot in by song
ggplot(Kayne_West, aes(year, swear_words)) +
  geom_point(aes(color = as.factor(swear_words), size = swear_words)) +
  geom_text(aes(label = title), check_overlap=T) +
  labs(x = "Year", y = "Swear Words",
       title = "Swear words in Kayne West songs")

# But really, how has this changed in time?
ggplot(Kayne_West, aes(year, swear_words)) +
  geom_smooth() +
  labs(x = "Year", y = "Swear Words",
       title = "Average number of swear words in Kayne West songs in time")

# Can you tell me what the issue is with this anylysis?
# Yes, we should normalize things...
