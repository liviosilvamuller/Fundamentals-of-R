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
 
 library(readxl)
 turnout <- read_excel("turnout.xls")
 
 dim(turnout)
 head(turnout)
 summary(turnout)
 turnout$year # 1980 
 length(turnout) # length of a vector
 
 
#2.2
 
 turnout$electorate <- turnout$VAP + turnout$overseas  # Add overseas electorate to VAP.

 vap <- (turnout$total / turnout$electorate) * 100  # Calculate voter turnout of VAP. 

 vep <- (turnout$total / turnout$VEP) * 100  # Calculate voter turnout of VEP. 
 
 vap; vep
 mean(vap); mean(vep) # The diff. b/w the two seems to be trivial. 
 
 
#2.3
 
 d1 <- turnout$ANES - vap
 d2 <- turnout$ANES - vep
 
 mean(d1); mean(d2) 
 range(d1); range(d2) 
 
 #2.4
 
 #2.5
 
 #2.6
 
 
 ## Q4
 
 turn$year
 ## 1980, '84, '88, '92, '96, '00, '04, '08: presidential election
 ## 1982, '86, '90, '94, '98, '02,         : midterm election
 
 pe <- turn[seq(1, 14, 2), ] # create presidential election
 pe[8, ] <- turn[14, ]       # add year 2008
 me <- turn[seq(2, 12, 2), ] # create midterm election
 
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
 
 
 ## Q5
 
 old <- turn[c(1:7), ]  # Extract first hald of data (up to 1992).
 ## old <- turn[seq(1, 7), ] # Do same thing as shown above. 
 new <- turn[c(8:14), ] # Extract second hald of data (from '94 to the end). 
 
 VEPo <- (old$total / old$VEP) * 100
 VEPn <- (new$total / new$VEP) * 100
 ANESo <- old$ANES
 ANESn <- new$ANES
 
 mean(ANESo) - mean(VEPo)
 mean(ANESn) - mean(VEPn)
 
 ## In terms of the mean of two periods, the ANES bias increased. 
 
 
 ## Q6
 
 ## Subtract felons & noncitisen from VAP.
 VAP1 <- turn$VAP - turn$felons - turn$noncit
 
 ## Extract the year 2008
 turn08 <- turn[turn$year==2008, ] 
 ## turn08 <- turn[14, ] ## same as above 
 
 ## Subtract osvoters from total in 2008.
 turn$total[14] <- turn08$total - turn08$osvoters
 
 ## adjusted voter turnout VAP
 adjVAP <- turn$total / VAP1
 
 mean(adjVAP)
 mean(vap)
 mean(vep)
 mean(turn$ANES)
 
 