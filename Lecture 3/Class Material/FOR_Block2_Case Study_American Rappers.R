# Title: American Rappers in the 21st Century
# Purpose: illustrate how one can use base R and tidy tools to optimize wrangling.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: October 2022

# Load packages, import data.----------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tm)
library(ggplot2)
library(tidyr)

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 3/Class Material")

Kanye_West <- readRDS("Kanye_West.RDS")
Jay_Z <-readRDS("Jay_Z.RDS")
Eminem<- readRDS("Eminem.RDS")
Kendrick_Lamar <-readRDS("Kendrick_Lamar.RDS")

# Let's consolidate these datasets in a single one----------------------------------------------------------------------------------------------

#If we simply merge them, we will lose the name of the artist

Eminem$rapper <- "Eminem"
Kanye_West$rapper <- "Kanye West"
Jay_Z$rapper <- "Jay Z"
Kendrick_Lamar$rapper <- "Kendrick Lamar"

#How would this look like in dplyr?
#Eminem <- Eminem %>% mutate( rapper= "Eminem")

american_rappers <- full_join(Eminem, 
                              full_join(Kanye_West,
                                        full_join(Jay_Z, Kendrick_Lamar)))
                                        

# note we can only merge them without specifying the by= argument in the dplyr::_join, because the dataset has the same variables.
# We are joining this way, because there are only four datasets and we said only with dplyr, 
# a more elegant solution would entail lists and the purrr package.This solution could work for multiple datasets:

library(purrr)
american_rappers <- list(Eminem, Kanye_West, Jay_Z, Kendrick_Lamar) %>% 
  reduce(full_join)

# Now that we merged the data, let's clean it----------------------------------------------------------------------------------------------

# There are songs in the dataset that do not come from rappers' albums, but from somewhere else.
# Let's mark only Wests' songs that are in West's albums:

american_rappers$album <- ifelse(startsWith(american_rappers$album, "album"), american_rappers$album, NA_character_)

#here we could also use a mixed version, with ifelse and dplyr:

# american_rappers <-
#   american_rappers %>%
#   mutate(album=
#            ifelse(startsWith(american_rappers$album, "album"), american_rappers$album, NA_character_)
#          )


# Extract year from string (4 digits)
american_rappers$year <- as.numeric(stringr::str_extract_all(american_rappers$album,
                                                       "[:digit:]{4}"))

# Remove obs that are missing for an album or year

american_rappers <- na.omit(american_rappers)

#dplyr alternative
american_rappers <- american_rappers %>% drop_na()

# Should we clean the text or not?

# We can remove punctuation, change all to lower case, and remove signs.

american_rappers$lyrics <- tm::removePunctuation(american_rappers$lyrics) #removing punctuation
american_rappers$lyrics <- tolower(american_rappers$lyrics) #making all lowercase
american_rappers$lyrics <- gsub("\r|\n", " ", american_rappers$lyrics) # sub markers
american_rappers$title <- tm::removePunctuation(american_rappers$title)

# We can also clean the album titles

american_rappers$album <- substring(american_rappers$album,9) #removing the up to the 9th character (album: )
american_rappers$album <- substring(american_rappers$album,1, nchar(american_rappers$album)-8) #removing the last eight characters (" (year))

# Create a dictionary for religion and swearing: can you help?-----------------------------------------------------------------------------------------------

swear_words <- "fuck|bitch|pussy|shit|dick|ass|cunt"
religious_words <- "god|bible|jesus|hell|heaven|lord|praise"

# Count appearances in songs and add as variable in data.

#base for swearing
american_rappers$swear_words <- stringr::str_count(american_rappers$lyrics, swear_words)


#tidy for religion
american_rappers <- american_rappers%>%
  mutate(religious_words=
  stringr::str_count(american_rappers$lyrics, religious_words)
  )


# Plots by song and Album-----------------------------------------------------------------------------------------------------------------------------------

ggplot(american_rappers, aes(year, swear_words)) +
  geom_point(aes(size = swear_words), color="gold") +
  geom_text(aes(label = title), check_overlap=T, size=3) +
  labs(x = "", y = "Count",
       title = "Swearing in American Rappers' songs",
       subtitle= "782 songs from Eminem, Jay Z, Kendrick Lamar, and Kanye West.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")

american_rappers %>% filter(swear_words < 100) %>%
  ggplot(., aes(year, swear_words)) +
  geom_point(aes(color = rapper)) +
  geom_text(aes(label = title), check_overlap=T, size=3) +
  labs(x = "", y = "Count",
       title = "Swearing in American Rappers' songs",
       subtitle= "782 songs from Eminem, Jay Z, Kendrick Lamar, and Kanye West.")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom")


american_rappers %>% group_by (album, rapper) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  ggplot(., aes(x=swear_words, y=reorder(album, swear_words))) +
  geom_bar(aes(fill=rapper), stat="identity") +
  labs(x = "Count", y = "Album Title",
       title = "Swearing in American Rappers' Albums",
       subtitle= "46 Albums since 1996")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom")

# But really, how has this changed in time?------------------------------------------------------------------------------------------------------------------

american_rappers %>% group_by (year, rapper) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  ggplot(., aes(year, swear_words)) +
  geom_smooth(se=FALSE, color="black") +
  labs(x = "Year", y = "Swear Words",
       title = " Swear words by year",
       subtitle= "782 songs since 1996")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")+
    facet_wrap(~rapper)

# Can you tell us what the issue is with this analysis?

# We need to normalize. What would be the best normalization?
# What is the best possible normalization given our data?

american_rappers %>%
  group_by(year, rapper) %>%
  mutate(songs_per_year = n()) %>%
  group_by(year,songs_per_year, rapper) %>%
  summarise(swear_words = sum(swear_words, na.rm = TRUE))%>%
  mutate(normalized_swear_words = swear_words/songs_per_year)%>%
  ggplot(., aes(year, normalized_swear_words)) +
  geom_smooth(se=FALSE, color="black") +
  labs(x = "", y = "Normalized count by songs per year",
       title = " Average swearing per song by year",
       subtitle= "782 songs since 1996")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")+
  facet_wrap(~rapper)

# Create a variable that counts the number of songs in an album.

american_rappers$word_per_song <-str_count(american_rappers$lyrics, "\\w+") # counting words in lyrics
american_rappers$normalized_swear_words2 <- american_rappers$swear_words/american_rappers$word_per_song # normalize
american_rappers$normalized_religious_words2 <- american_rappers$religious_words/american_rappers$word_per_song #normalize

american_rappers %>%
  ggplot(., aes(year, normalized_swear_words2)) +
  geom_smooth(se=FALSE, color="black") +
  labs(x = "", y = "Normalized Count (by song length)",
       title = " Average swearing normalized by song length",
       subtitle= "782 songs since 1996")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "none")+
    facet_wrap(~rapper)

# What about religious vocabulary? -----------------------------------------------------------------------------------------
# how can we best visualize such an effect?

american_rappers %>%
  gather("topic", "normalized_count2", 9:10) %>%
  ggplot(., aes(x=year, y=normalized_count2, color=topic)) +
  geom_smooth(se=FALSE) +
  labs(x = "", y = "Normalized Count",
       title = " Religion and Swearing in American Rappers' Repertoire",
       subtitle= "782 songs since 1996")+
  theme(
    panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom")+
    facet_wrap(~rapper)

# Is there a Kim Kardashian effect?--------------------------------------------------------------------------------------------------------------------------

# Kanye and Kim were a couple from 2011 to 2020

# let's create a Kim variable


Kanye_West <- american_rappers %>% 
  filter(rapper== "Kanye West")%>%
  mutate(kim_kardashian= case_when(year > 2010 & year <= 2020  ~ "With Kim",
                                   year < 2011  ~ "Before Kim",
                                   year >2019  ~ "After Kim"))

Kanye_West %>%
  gather("topic", "word_count", 6:7) %>%
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
    legend.position = "bottom") #weird reorder, can we change it?

Kanye_West$kim_kardashian <- factor(Kanye_West$kim_kardashian,                                    
                                    levels = c("Before Kim", "With Kim", "After Kim")) #Changes ordering manually, re-run the plot!



# Let's take a quick dive into Lamar's Repertoire!-----------------------------


cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
} #the function "function" creates a function.

stops <- c(stopwords('SMART'), "dont", "youre") # stopwords ('SMART') is a vector with common words (a, the, about...) that we want to remove from the songs

Kendrick_Lamar <- american_rappers %>%
  filter(rapper=="Eminem") # creating a dataset only for Kendrick
  
# the next few lines perform a few operations to create a dataset counting appearances of words in all songs.
lamarCorpus_t <- VCorpus(VectorSource(Kendrick_Lamar$lyrics))
lamarCorpus_t <- cleanCorpus(lamarCorpus_t, stops)
LamarTDM_t  <- TermDocumentMatrix(lamarCorpus_t)
lamar_dtm_t  <- DocumentTermMatrix(lamarCorpus_t)
LamarTDMm_t <- as.matrix(LamarTDM_t)

LamarSums_t <- rowSums(LamarTDMm_t)
LamarFreq_t <- data.frame(word=names(LamarSums_t),frequency=LamarSums_t) #LamarFreq_t is the dataset 
rownames(LamarFreq_t) <- NULL
topWords_t  <- subset(LamarFreq_t, LamarFreq_t$frequency >= 100) #here we are subsetting to those that appear more than 50 times.
topWords_t  <- topWords_t[order(topWords_t$frequency, decreasing=F),]
topWords_t$word <- factor(topWords_t$word, levels=unique(as.character(topWords_t$word)))

ggplot(topWords_t, aes(x=word, y=log(frequency))) +
  geom_bar(stat="identity", fill='gold') +
  coord_flip()+
  geom_text(aes(label=frequency), colour="black",hjust=1.25, size=3.0)+
  labs( x="", y= "", title= "Most frequent words in Lamar's repertoire",
        subtitle="All albums since 2011")+
  theme(panel.background = element_rect ("white", "black", .5, "solid"),
        panel.grid.major = element_line(color="grey", size=0.3, linetype= "solid"),
        axis.text = element_text(color="black", size=10),
        title = element_text(color="black", size=10, face="bold"),
        axis.text.x=element_blank(),
        plot.subtitle = element_text(color="black", size=9, face= "plain"),
        legend.position = "bottom")
