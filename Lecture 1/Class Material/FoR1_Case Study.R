# Title: Who makes international regimes possible?
# Purpose: showcase a conceptually sound approach to answering a question.
# Author: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 1/Class Material/")
#remember to set your own directory here

library(tidyverse)
library(skimr)
library(scales)
library(RColorBrewer)
library(gt)

options(stringsAsFactors = FALSE, scipen = 999) 
#define a few global options for how things are computed and displayed.
# the first argument stops strings to be imported as factors (no worries, we will cover this next week).
# the second argument stops R from using scientific notation (7245 instead of 7.245E+3).

## First, how would you design the data collection?
# Think about (1) the sample frame.
# How should the final dataset should ideally look like?
# Where are you taking the data from? Is it available?

##Now that we thought about the design and collection, let's take a look at a random sample:

dat <- read_csv("io_income_rs.csv") 

skim(dat) # quick way to skim your data, from the "skimr" package we loaded at line nine
View(dat)

## Let's start by checking the top 10 donors overall

dat %>% # %>% is the pipe operator, it means pipe "dat" through whatever comes next;
  drop_na(donor) %>% # this line removes all observation that contain a missing value in the column "donor"
  group_by(donor) %>% # you can just keep piping and it will use the result; this line groups obs by "donor"
  summarize(amount_nominal = sum(amount_nominal, na.rm = TRUE)) %>% # the argument na.rm=TRUE makes the function skip missing values.
  arrange(desc(amount_nominal)) %>% 
  slice_head(n=10)
  
## How are donations distributed?

# We will use the ggplot2 package, which is part of the tidyverse, loaded in line eight.

dat %>% 
  drop_na(type_donor)%>% 
  ggplot(., aes(x = amount_nominal, fill=type_donor))+ # this initiates the plotting system, and defines our x variable, and our "fill" variable.
  geom_density (alpha=.5, position="identity", aes(y=..count..)) + # as y variable, we want the count of observation per value, which is not in the dataset, but ggplot computes it for us.
  scale_x_log10() # because of the range of our variables, we are using a logarithmic function.

# kind of ugly numbers in the x-axis, 
#the labels argument in the scales_x_log10 function can sort that (labels come from the "scales" package in line 10)

dat %>%  
  drop_na(type_donor)%>%
  ggplot(., aes(x = amount_nominal, fill=type_donor))+
  geom_density (alpha=.5, position="identity", aes(y=..count..)) +
  scale_x_log10(labels=dollar) # this is where we specify the label, notice how we simply "added" (+) this line of code.

# colors still kind of ugly... 
#let's get some palettes from the "RColorBrewer" we loaded in line 11
# we can ask for help: ??RColorBrewer

dat %>%
  drop_na(type_donor)%>%
  ggplot(., aes(x = amount_nominal, fill=type_donor))+
  geom_density (alpha=.5, position="identity", aes(y=..count..))+
  scale_x_log10(labels=dollar)+
  scale_fill_brewer(type="qual", palette= "Accent", direction=1)  #here we specify what we just selected

# It would be nice to get some summary statistics for this.

dat %>%
  drop_na(type_donor)%>%
  ggplot(., aes(x = amount_nominal, fill=type_donor))+
  geom_density (alpha=.5, position="identity", aes(y=..count..))+
  scale_x_log10(labels=dollar)+ 
  scale_fill_brewer(type="qual", palette= "Accent", direction=1)+ # we can get summary stats within the code itself (next two lines)
  geom_vline(aes(xintercept = mean(dat$amount_nominal, na.rm=TRUE), colour="mean"), linetype = "longdash")+ 
  geom_vline(aes(xintercept = median(dat$amount_nominal, na.rm=TRUE), colour="median"), linetype = "longdash")

# what's the interpretation, given the relationship between mean and median?

# polishing up with some titles

dat %>%
  drop_na(type_donor)%>%
  ggplot(., aes(x = amount_nominal, fill=type_donor))+
  geom_density (alpha=.5, position="identity", aes(y=..count..))+
  scale_x_log10(labels=dollar)+
  geom_vline(aes(xintercept = mean(dat$amount_nominal, na.rm=TRUE), colour="mean"), linetype = "longdash")+
  geom_vline(aes(xintercept = median(dat$amount_nominal, na.rm=TRUE), colour="median"), linetype = "longdash")+
  scale_fill_brewer(type="qual", palette= "Accent", direction=1)+
  labs( x = NULL, y = "Contributions count", title = "Distribution of contributions per amount", # labs() adds text to axis, titles, subtitles and so on...
    subtitle=" composed by 4500 randomly sampled observations from dataset",
    caption = "Source: GVA Income Dataset (2022)")+
  theme(panel.background = element_rect("white", "black", .5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 10),
    title = element_text(color = "black", size = 10, face = "bold"),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
    legend.position = "bottom") # theme() changes the color of the background, size, color, and face of texts...
  

# Finally, we can use the assign operator to create an object with the plot.
# We can also export manually, by clicking on export at the lower-right panel.

 our_plot <- 
   dat %>%
   drop_na(type_donor)%>%
   ggplot(., aes(x = amount_nominal, fill=type_donor))+
   geom_density (alpha=.5, position="identity", aes(y=..count..))+
   scale_x_log10(labels=dollar)+
   geom_vline(aes(xintercept = mean(dat$amount_nominal, na.rm=TRUE), colour="mean"), linetype = "longdash")+
   geom_vline(aes(xintercept = median(dat$amount_nominal, na.rm=TRUE), colour="median"), linetype = "longdash")+
   scale_fill_brewer(type="qual", palette= "Accent", direction=1)+
   labs(
     x = NULL, y = "Contributions count", title = "Distribution of contributions per amount",
     subtitle=" composed by 4500 randomly sampled observations from dataset",
     caption = "Source: GVA Income Dataset (2022)"
   )+
   theme(
     panel.background = element_rect("white", "black", .5, "solid"),
     panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
     axis.text = element_text(color = "black", size = 10),
     title = element_text(color = "black", size = 10, face = "bold"),
     legend.title = element_blank(),
     plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
     legend.position = "bottom"
   )

## Let's try to plot the yearly evolution of the donations by public and private donors
 #we will need to get the data in the right format for that. What is that format?
 
 evolution <- dat %>% # again, we start from the original dataset "dat", which is not modified by any code using %>%
              group_by(year, type_donor) %>% # we tell R to group observations by year and type of donor
              summarize(amount_nominal_usd = sum(amount_nominal, na.rm = TRUE))%>%# each of these "grouped" observations, should have the sum of the contributions
              drop_na(type_donor)
   
  evolution %>%  #we save the result of our "wrangling" as evolution, so now we can just start from that.
   ggplot(., aes(x = year, y = amount_nominal_usd, fill = type_donor)) +
   geom_area(stat = "identity", position = "stack") +
   scale_fill_brewer(type = "qual", palette = "Set2", direction= -1)+
   scale_y_continuous(labels = scales::dollar_format(scale=.000000001, suffix="b", prefix = "$")) + #we can use "scales" to simplify even more the axis info.
   labs(
     x = NULL, y = NULL, title = " Contributions per year since 2000",
     subtitle = "composed of 4500 randombly sampled observations from dataset",
     caption = c("Source: GVA Income Dataset (2022)")
   ) +
   theme(
     panel.background = element_rect("white", "black", .5, "solid"),
     panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
     axis.text = element_text(color = "black", size = 10),
     title = element_text(color = "black", size = 10, face = "bold"),
     legend.title = element_blank(),
     plot.subtitle = element_text(color = "black", size = 9, face = "plain"),
     legend.position = "bottom"
   )
 