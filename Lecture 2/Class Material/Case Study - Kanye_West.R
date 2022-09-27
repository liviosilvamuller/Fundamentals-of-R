# Title: Fundamentals of R, Block 1, Case Study 2
# Purpose: to illustrate 
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

# Let's explore Kanye West's repertoire.

# Load packages-----------------------------------------------------------------
library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(ggplot2)
library(tidyr)


# Get URL and setup song nodes for scrapping ----------------------------------

#EVERYTHING UNTIL LINE 59 IS THE SETUP AND SCRAPPING CODE
# WE ALREADY RUN IT AND SAVED THE DATA, SO YOU CAN JUMP TO LINE 60

artist_url <- 'https://www.azlyrics.com/w/west.html'
song_nodes <- read_html(artist_url) %>% # load the html
  html_nodes("#listAlbum a")
song_links <-  html_attr(song_nodes, name='href')
song_links <- na.omit(ifelse(startsWith(song_links, "/lyrics/"),
                             song_links, NA_character_))

# Scrape lyrics and albums-----------------------------------------------------
Kanye_West <- data.frame()
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


# Let's start by loading and cleaning the dataset -------------------------------

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 2/Class Material")
Kanye_West <- readRDS("Kanye_West.RDS")

# Checkout the dataset manually
# There are songs in the dataset that do not come from Wests' albums, but from somewhere else
# Let's mark only Wests' songs that are in his albums:

Kanye_West$album <- ifelse(startsWith(Kanye_West$album, "album"), Kanye_West$album, NA_character_)

# Tip: ifelse is cool BTW!
# remember the structure ifelse (LOGICALTEST , VALUE IF TRUE, VALUE IF FALSE)
# ifelse is a vectorized function that works through elements in string.
# Very helpful for coding when conditions are to be met!

# Extract year from string (4 digits)
Kanye_West$year <- as.numeric(stringr::str_extract_all(Kanye_West$album,
                                                       "[:digit:]{4}"))
# Remove obs that are missing for an album or year
Kanye_West <- na.omit(Kanye_West)

# Should we clean the text or not?

# We can remove punctuation, change all to lower case, and remove signs.

Kanye_West$lyrics <- tm::removePunctuation(Kanye_West$lyrics)
Kanye_West$lyrics <- tolower(Kanye_West$lyrics)
Kanye_West$lyrics <- gsub("\r|\n", " ", Kanye_West$lyrics) # sub markers
Kanye_West$lyrics <- tm::removePunctuation(Kanye_West$lyrics)
Kanye_West$title <- tm::removePunctuation(Kanye_West$title)

# We can also clean the album titles

Kanye_West$album <- substring(Kanye_West$album,9)
Kanye_West$album <- substring(Kanye_West$album,1, nchar(Kanye_West$album)-8)

# Create a dictionary for religion and swearing: can you help?------------------

swear_words <- "fuck"
religious_words <- "god"

# Count appearances in songs and add as variable in data.
Kanye_West$swear_words <- stringr::str_count(Kanye_West$lyrics, swear_words)
Kanye_West$religious_words <- stringr::str_count(Kanye_West$lyrics, religious_words)

# Plot in by song and Album--------------------------------------------------------------

ggplot(Kanye_West, aes(year, swear_words)) +
  geom_point(aes(size = swear_words), color="gold") +
  geom_text(aes(label = title), check_overlap=T, size=3) +
  labs(x = "", y = "Count",
       title = "Swearing in Kanye West's songs",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")

Kanye_West %>% group_by (album) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  ggplot(., aes(x=swear_words, y=reorder(album, swear_words))) +
  geom_bar(stat="identity", fill="gold") +
  labs(x = "Count", y = "Album Title",
       title = "Swearing in Kanye West's Albums",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")

# But really, how has this changed in time?-------------------------------------

Kanye_West %>% group_by (year) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  ggplot(., aes(year, swear_words)) +
  geom_smooth(se=FALSE, color="black") +
  labs(x = "Year", y = "Swear Words",
       title = " Number of swear words in Kanye West's songs per year",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")

# Can you tell us what the issue is with this analysis?

# We need to normalize. What would be the best normalization?
# What is the best possible normalization given our data?

Kanye_West %>%
  group_by(year) %>%
  mutate(songs_per_year = n()) %>%
  group_by(year,songs_per_year) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  mutate(normalized_swear_words = swear_words/songs_per_year)%>%
  ggplot(., aes(year, normalized_swear_words)) +
  geom_smooth(se=FALSE, color="black") +
  labs(x = "", y = "Normalized Count",
       title = " Average swearing per song by year",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")

# Can we use religious vocabulary to "predict" 2019s Sunday Service?------------
# how can we best visualize such an effect?

Kanye_West %>%
  gather("topic", "word_count", 5:6) %>%
  group_by(year) %>%
  mutate(songs_per_year = n()) %>%
  group_by(year,songs_per_year, topic) %>%
  summarise(word_count = sum(word_count, na.rm = TRUE))%>%
  mutate(normalized_word_count = word_count/songs_per_year)%>%
  ggplot(., aes(x=year, y=normalized_word_count, color=topic)) +
  geom_smooth(se=FALSE) +
  labs(x = "", y = "Normalized Count",
       title = " Religion and Swearing in Kanye West's Repertoire",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom")

# Is there a Kim Kardashian effect?---------------------------------------------
# Kanye and Kim were a couple from 2011 to 2020

# let's create a Kim variable


Kanye_West <- Kanye_West %>%
  mutate(kim_kardashian= case_when(year > 2010 & year <= 2020  ~ "With Kim",
                                   year < 2011  ~ "Before Kim",
                                   year >2019  ~ "After Kim"))

Kanye_West$kim_kardashian <- factor(Kanye_West$kim_kardashian,                                    
                   levels = c("Before Kim", "With Kim", "After Kim"))

# Change ordering manually

Kanye_West %>%
  gather("topic", "word_count", 5:6) %>%
  group_by(year) %>%
  mutate(songs_per_year = n()) %>%
  group_by(songs_per_year, topic, kim_kardashian) %>%
  summarise(word_count = sum(word_count, na.rm = TRUE))%>%
  mutate(normalized_word_count = word_count/songs_per_year)%>%
  ggplot(., aes(x=kim_kardashian, y=normalized_word_count, fill=topic)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x = "", y = "Normalized Count",
       title = " The Kardashian Effect?",
       subtitle= "214 songs in 13 albums since 2004.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom")
