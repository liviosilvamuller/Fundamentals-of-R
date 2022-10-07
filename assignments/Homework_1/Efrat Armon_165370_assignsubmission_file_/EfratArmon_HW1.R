# Title: Homework 1
# Purpose: Objects, Class, and Data Structures
# Author: Efrat Armon
# Date: October 6 2022

setwd("C:/Users/efrat/OneDrive/Desktop/IHEID/S3_2022/R/Class 2 30.09")
library(readr)
animal_longevity <- read_csv("animal_longevity.csv")

#1.1. 
dim(animal_longevity)
#4219 Observations; 10 variables (also written in the Environment)

  
#1.2. 
View(animal_longevity)
L_honeybee <- 8
L_redkite <- 38
L_ruff <-13.9

#1.3. 
L_honeybee >= L_redkite
L_honeybee == L_ruff
L_ruff < L_redkite

#1.4.1 & 1.4.2
animals <- c("Honey Bee", "Red Kite", "Ruff")
lifespans <- c(L_honeybee, L_redkite, L_ruff)

#1.5. 
mean(lifespans)
median(lifespans)

#1.6. 
animals[2]

#1.7. 
subset(animals, lifespans > 27)

#1.8. 
Table1.8 <- data.frame(animal_longevity$Organism_Common_Name, animal_longevity$Maximum_Longevity_Years)
colnames(Table1.8)
names(Table1.8)[names(Table1.8) == "animal_longevity.Organism_Common_Name"] <- "CommonName"
names(Table1.8)[names(Table1.8) == "animal_longevity.Maximum_Longevity_Years"] <- "MaxLongevity"

Table1.8 <- Table1.8[order(Table1.8$MaxLongevity),]


#1.9. 
mean(Table1.8$MaxLongevity, na.rm = TRUE) #25.21 - need to strip the NA entries before we can calculate the mean and median
median(Table1.8$MaxLongevity, na.rm = TRUE) #15.2

#1.10. 
# No. We need code that works on the animal_longevity dataset (the code creates a new object but using the wrong dataset). We know the max longevity for humans. From here we can either use 'subset' or we can add a vector to the dataframe using 'ifelse':
subset(Table1.8, MaxLongevity >122.5)
Table1.8$OutliveHumans <- ifelse(Table1.8$MaxLongevity >122.5, 1, 0)
subset(Table1.8, OutliveHumans==1)


#2.1 
library(readxl)
turnout <- read_excel("turnout.xls")
View(turnout)

dim(turnout) #14 observations 
summary(turnout) #range of years: 1980-2008

#2.2.
turnout$RTR1 <- turnout$total/turnout$VAP*100
turnout$RTR2 <- turnout$total/(turnout$VEP + turnout$overseas)*100
subset(turnout, RTR2>RTR1) 
#for all years Total/VEP is greater than Total/VAP. This means the the eligible population is SMALLER than the voting age population. (Denominator goes down while Numerator the same, outcome goes up).


#2.3. 
turnout$mispercep1 <- turnout$ANES - turnout$RTR1
mean(turnout$mispercep1)
turnout$mispercep2 <- turnout$ANES - turnout$RTR2
mean(turnout$mispercep2)

#it would appear that people overestimate voter turnout by 17-20 percentage points on average. 
mean(turnout$mispercep1/turnout$RTR1) #This is an overestimation of approximately 43% on average. 


#2.4.
presidential_VEP_RTR <- ifelse(turnout$election_type == "presidential", turnout$mispercep2, NA)
mean(presidential_VEP_RTR, na.rm = TRUE)
midterm_VEP_RTR <- ifelse(turnout$election_type == "midterm", turnout$mispercep2, NA)
mean(midterm_VEP_RTR, na.rm = TRUE)
#there is a higher bias (by three percentage points) for presidential elections compared to midterm elections

#2.5. 
oldelec <- subset.data.frame(turnout, turnout$year < 1993, year:mispercep2)
newelec <- subset.data.frame(turnout, turnout$year > 1993, year:mispercep2)

mean(oldelec$mispercep2)
mean(newelec$mispercep2)
#it appears that the bias has increased over time. 
