# Title: Solutions for homework of block one.
# Purpose: Detailed solutions for lecture on objects, functions, and data types.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022


setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 2/Material")

#Exercise 1: Animal longevity dataset-------------------------------------------

library(readr)
animal_longevity <- read_csv("animal_longevity.csv")

#1.1
dim(animal_longevity)
length(animal_longevity)
summary(animal_longevity)

#1.4 
 View(animal_longevity)
 giraffe_lifespan <- 29.5
 penguin_lifespan <- 26
 elephant_lifespan <- 65
 
 #1.5
 giraffe_lifespan == penguin_lifespan
 elephant_lifespan != penguin_lifespan
 giraffe_lifespan > penguin_lifespan 
 
 #1.6
 theanimals <- c("giraffe", "penguin", "elephant")
 lifespans <- c(giraffe_lifespan, penguin_lifespan, elephant_lifespan)
 
 #1.7
 mean(lifespans)
 median(lifespans)
 
 #1.8
 theanimals[2]
 
 #1.9
 theanimals[lifespans > 27]
 
 #Exercise 2: Self-reported Turnout--------------------------------------------
 
 #2.1
 
 turn <- read_csv("turnout.csv")
 
 dim(turn)
 head(turn)
 summary(turn)
 turn$year # 1980 
 length(turn) # length of a vector
 
 
#2.2
 
 turn$electorate <- turn$VAP + turn$overseas  ## Add overseas electorate to VAP.

 vap <- (turn$total / turn$electorate) * 100  ## Calculate voter turnout of VAP. 

 vep <- (turn$total / turn$VEP) * 100  ## Calculate voter turnout of VEP. 
 
 vap; vep
 mean(vap); mean(vep) # The diff. b/w the two seems to be trivial. 
 
 
#2.3
 
 d1 <- turn$ANES - vap
 d2 <- turn$ANES - vep
 
 mean(d1); mean(d2) 
 range(d1); range(d2) 
 
 #Exercise 3: --------------------------------------------
 
 