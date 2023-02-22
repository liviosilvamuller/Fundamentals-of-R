# Title: Assignment for Fundamentals of R 
# Purpose: to work on animal longevity data set
# Author: Mohammad Javaid
# Date: 06 October 2022

#Question 1

#loading the longevity file
animal_longevity <- read_csv("Material for Week 2-20220930/animal_longevity.csv")

#1.1 checking the number variables and observations in animal longevity data set

dim(animal_longevity)

#1.2 Picking three animals and creating objects for them from the data set 

Daphnia <-0.19
Fruit_fly <-0.30
Honey_bee <- 8.00

#1.3 creating three logical tests
Daphnia == Fruit_fly #1
Honey_bee > Fruit_fly #2
Daphnia < Honey_bee #3


#1.4 creating two vectors from the chosen animals

animals <- c("Daphnia", "Honey Bee", "Fruit_fly")
lifespans <- c(0.19, 0.30, 8)

# 1.5 calculating mean and median of their lifespan

mean(lifespans)
median(lifespans)

#1.6 retrieving the 2nd value of the vector from the selected animals

animals[2]

#1.7 from the subset, looking at the vector having maximum longevity above 27

animals[lifespans>27]

#1.8 new data set containing the maximum longevity of animals and their common names
# here l is a new data set file that contains the maximum longevity of animals and their common names
l<-animal_longevity[c(5,8)]

#1.9 calculating mean and median for l

mean(l$Maximum_Longevity_Years, na.rm = TRUE)
median(l$Maximum_Longevity_Years, na.rm = TRUE)

#1.10 checking if following code works in case we want to know which animal can outlive human

outliving_human <- longevity[longevity$Maximum_Longevity_Years > 122.5]

#it did not work.


#Q2 Self Reported Turnout

#loading data 
turnout <- read_excel("Material for Week 2-20220930/turnout.xls")

#checking dimension of data set

dim(turnout)

#obtaining the summary of the data set

summary(turnout)

# looking at the range of the data set

range(turnout$year)

# Calculating the real turnout rate based on the voting age population (VAP) by year
#how many voters actually voted  (TOTAL/VAP) in each year

#combining the VEP and overseas
turnout$electorate <- turnout$VEP + turnout$overseas

#finding the percentage by dividing the total with the electorate 
#creating the percentage value for total by electorate

percentage <- turnout$total/turnout$electorate
percentage <- percentage*100

#creating the percentage value for total by VAP
percentage2 <- turnout$total/turnout$VAP
percentage2 <- percentage2*100

#subtracting the 2nd  from the first
percentage2- percentage


#Compute the differences between the ANES and the real turnout rate

turnoutANES <- turnout$ANES

# percentage is the total value of the electorate

turnoutANES - percentage

#calculating the mean

mean(ANES_VAP_difference)

ANES_VAP_difference <- c(turnoutANES - percentage)

#calculating the differnce

Dif <- turnout$total/turnout$ANES

Dif2 <- Dif*100

#subtracting the 2nd from the first

Dif2- Dif

