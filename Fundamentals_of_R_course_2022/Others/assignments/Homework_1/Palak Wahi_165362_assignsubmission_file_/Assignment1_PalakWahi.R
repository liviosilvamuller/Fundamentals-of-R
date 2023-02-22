# Title: Assignment 1 | Fundamentals of R | 
# Purpose: Block 1 Practical - Handling data in Base R
# Author: Palak Wahi
# Date: October 2022

setwd("/Users/PalakWahi1/Desktop/Sem III/Fundamentals of R/Class 2 R")

#Question 1 : ANIMAL LONGEVITY: Contains data on the maximum lifespan of various animals.
library(readr)
animal_longevity <- read_csv("Material for Week 2-20220930/animal_longevity.csv") #Importing Data
View(animal_longevity) #Viewing Data

#1.1---------
dim(animal_longevity)
## There are 4219 observations and 10 variables in this data frame.

#1.2---------
Daphnia <- 0.19
FruitFly <- 0.30
Honeybee <- 8.00

#1.3---------
Daphnia == FruitFly # Comparing Maximum_Longevity_Years of Daphnia and Fruitfly, logical test results as FALSE since the values are not equal. 
#We then run logical tests similarly for others.
Honeybee != Daphnia
FruitFly >= Honeybee

#1.4---------
animals_1.2 <-  c("Daphnia", "Fruit fly", "Honey bee")
# animals_lifespan_1.2 <- c(Daphnia , FruitFly, Honeybee)
#Alternatively, we can also simply add the values of lifespans of respective animals
animals_lifespan_1.2_2 <- c(0.19, 0.30, 8.00)

#1.5---------
mean(animals_lifespan_1.2_2)
median(animals_lifespan_1.2_2)

#1.6---------
animals_1.2[2]

#1.7---------
animals_chosen <- data.frame(animals_1.2, 
                             animals_lifespan_1.2_2)
Above_27 <- subset(animals_chosen, 
                   animals_chosen$animals_lifespan_1.2_2 > 27)
Above_27
# No animals have a maximum longevity of above 27.

#1.8---------

Max_Longe <- data.frame(animal_longevity$Organism_Common_Name, animal_longevity$Maximum_Longevity_Years)
Max_Longe <- Max_Longe[order(Max_Longe$animal_longevity.Maximum_Longevity_Years),]

#1.9---------
summary(animal_longevity$Maximum_Longevity_Years)
## Mean = 25.21   -> Implying the average years of lifespan of animals in the data
## Median = 15.20 -> Implying the median (middle-most) years of animals' lifespan in the data
## ALternatively, use command = mean(na.omit(animal_longevity$Maximum_Longevity_Years))

#1.10---------

## The code stated in the question will NOT WORK because they forget the commma (,)
## and the name of the df is 'animal_longevity' not 'longevity'. 
## Following can be the correct code:

outliving_humans <- animal_longevity[animal_longevity$Maxium_Longevity_Years > 122.5, ]
outliving_humans

##############################################################################



#Question 2 : SELF-REPORTED TURNOUT

library(readxl)
turnout <- read_excel("Material for Week 2-20220930/turnout.xls")
View(turnout)

#2.1---------
dim(turnout) # Dimensions =  14 10
summary(turnout)
## Observations = 14
## Length = 10
range(turnout$year)
## Range of years = 1980- 2010


#2.2---------
turnout$electorate <- turnout$VEP + turnout$overseas

turnout$real_turnout_vap <- (turnout$total/turnout$VAP)*100 # Voter turnout of VAP
turnout$real_turnout_vep <- (turnout$total/turnout$electorate)*100 # Voter turnout of VEP


# Difference observed- After accounting for total number of eligible overseas 
# voters by using VEP instead of VAP, the real turnout has increased in each year. 
# The turnout rates calculated using VEP are higher than VAP.


#2.3---------
turnout$diff_1 <- turnout$ANES- turnout$real_turnout_vap
turnout$diff_2 <- turnout$ANES- turnout$real_turnout_vep


# diff_1 = ANES- VAP turnout
# diff_2 = ANES- VEP turnout

mean(turnout$diff_1) # The average difference between ANES and total/VAP is 19.69001
mean(turnout$diff_2) # The average difference between ANES and total/VEP is 17.55991
# The average difference between the self-estimated turnout rate (ANES) and the 
# real turnout rates (both VAP and VEP turnout rates) is positive and more 
# in real_turnout_vap than real_turnout_vep.


#2.4---------

## Presidential Elections
Pre_elec <- subset(turnout, 
                   election_type == "presidential")
mean(Pre_elec$diff_2) # The average difference between ANES and VEP/total is 18.74622
#This average difference is more than what was observed in entire turnout data frame (18.74622 > 17.55991)


## Midterm Elections
Mid_elec <- subset(turnout, 
                   election_type == "midterm")
mean(Mid_elec$diff_2) # The average difference between ANES and VEP/total is 15.97816
#This average difference is less than what was observed in entire turnout data frame (15.97816 < 17.55991)


## Q.Does the bias of the ANES estimates vary across election types?
## Yes. The bias of the ANES seems greater in the presidential election.



#2.5---------
Phase_1 <- turnout[1:7,]
Phase_2 <- turnout[8:14,]
Phase_1
Phase_2

# We can observe  the difference between the VEP turnout rate and the ANES
# turnout rate separately for each year within each period in Column 'diff_2'

mean(Phase_1$diff_2) # 16.48163
mean(Phase_2$diff_2) # 18.63819


# Q. Has the bias of ANES increased over time? 
# According to the results above, the difference between the VEP turnout rate 
# and the ANES turnout rate for each period increased on average 
# (16.48163 -> 18.63819). Therefore, it seems that the bias of ANES has increased.



