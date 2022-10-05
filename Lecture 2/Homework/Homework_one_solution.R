# Title: Solutions for homework of block one.
# Purpose: Detailed solutions for lecture on objects, functions, and data types.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 2/Homework")

#Exercise 1: Animal longevity dataset-------------------------------------------

library(readr)
animal_longevity <- read_csv("animal_longevity.csv")

#1.1
dim(animal_longevity)
length(animal_longevity)
summary(animal_longevity)

#1.2 
 View(animal_longevity)
 giraffe_lifespan <- 29.5
 penguin_lifespan <- 26
 elephant_lifespan <- 65
 
 #1.3
 giraffe_lifespan == penguin_lifespan
 elephant_lifespan != penguin_lifespan
 giraffe_lifespan > penguin_lifespan 
 
 #1.4
 theanimals <- c("giraffe", "penguin", "elephant")
 lifespans <- c(giraffe_lifespan, penguin_lifespan, elephant_lifespan)
 
 #1.5
 mean(lifespans)
 median(lifespans)
 
 #1.6
 theanimals[2]
 lifespans[2]
 
 #1.7
 
 theanimals[lifespans > 27]
 
 #1.8
longevity <- animal_longevity[,c(5, 8)]
longevity <- longevity[order(longevity$Maximum_Longevity_Years),]

#1.9
mean(longevity$Maximum_Longevity_Years, na.rm=TRUE)
median(longevity$Maximum_Longevity_Years, na.rm=TRUE)

#1.10

longevity<- as.data.frame(longevity)

outliving_humans <- longevity[longevity$Maximum_Longevity_Years > 122.5,]

#r does not understand which column to subset because the brackets has no comma.

 
#Exercise 2: Self-reported Turnout--------------------------------------------
 
 #2.1
 
 library(readxl)
 turnout <- read_excel("turnout.xls")
 
 dim(turnout)
 head(turnout)
 summary(turnout)
 turnout$year # 1980 
 length(turnout) # length of a vector
 
 
#2.2
 
turnout$electorate <- turnout$VEP + turnout$overseas  # Add overseas electorate to VEP.

vep <- (turnout$total / turnout$electorate) * 100  # Calculate voter turnout of VEP. 

vap <- (turnout$total / turnout$VAP) * 100  # Calculate voter turnout of VAP 
 
vap; vep
mean(vap); mean(vep) # The diff. b/w the two seems to be trivial. 
 
 
turnout$osvoters <- as.numeric(ifelse(turnout$osvoters=="NA", "0", turnout$osvoters))

#2.3
 
 d1 <- turnout$ANES - vap
 d2 <- turnout$ANES - vep
 
 mean(d1); mean(d2) 
 
 #2.4
 

pe <- subset(turnout, election_type =="presidential") 
me <- subset(turnout, election_type =="midterm") 
 
 vepP <- (pe$total / pe$VEP) * 100 # voter turnout of VEP predident
 vepM <- (me$total / me$VEP) * 100 # voter turnout of VEP midterm
 
 pe$ANES # voter turnout of ANES president
 me$ANES # voter turnout of ANES midterm
 
 ## Big difference b/w presidential & midterm elec, in ANES
 
 ## Create difference. 
 diff1 <- mean(pe$ANES) - mean(vepP) # VEP vs. ANES in presidential election
 diff2 <- mean(me$ANES) - mean(vepM) # VEP vs. ANES in midterm election
 
 diff1 - diff2
 
 ## Estimation of (ANES - VEP) is 2.46% higher in presidential election. 
 ## Voter turnout is higher in presidential election. 
 
 #2.5
 

old <- subset(turnout, year< 1993) 
new <- subset(turnout, year> 1993) 


 
 old <- turnout[c(1:7), ]  # Extract first half of data (up to 1992).
 
 new <- turnout[c(8:14), ] # Extract second half of data (from '94 to the end). 
 
 VEPo <- (old$total / old$VEP) * 100
 VEPn <- (new$total / new$VEP) * 100
 ANESo <- old$ANES
 ANESn <- new$ANES
 
 mean(ANESo) - mean(VEPo)
 mean(ANESn) - mean(VEPn)
 
 ## In terms of the mean of two periods, the ANES bias increased. 
 