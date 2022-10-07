# Title: Homework-Animal longvity Script
# Author: Yuyu Chen
# Date: 06-10-2022

#Question 1

#import data
animal_longevity <- read.csv("/Users/yuyu.chen/Desktop/animal_longevity.csv")

#1. The animal longevity dataset contains data on the maximum (known) lifespan of various animals.
#1.1 How many variables and how many observations are there?

dim(animal_longevity)

#1.2 Pick three animals from the dataset. Create three objects (named after the animal) storing the value of their longevity.

Daphnia <-0.19
Fruit_fly <-0.30
Honey_bee <- 8.00

#1.3 Create three logical tests comparing the longevity between the objects you created in question 1.2.
Fruit_fly == Daphnia #1
Honey_bee < Fruit_fly #2
Daphnia > Honey_bee #3


#1.4 Create two vectors:
#The names of all the animals you chose in question 1.2. 
#Their respective lifespans.

animals <- c("Daphnia", "Honey Bee", "Fruit_fly")
lifespans <- c(0.19, 0.30, 8)

# 1.5 Calculate the mean and median of their lifespans

mean(lifespans)
median(lifespans)

#1.6 Retrieve the 2nd value of the vector that contains your animal names.

animals[2]

#1.7 Subset which animals in your lifespans vector have a maximum longevity of above 27.

animals[lifespans>27]

#1.8 Create a new dataset containing the maximum longevity of animals and their common names ranked from shortest to longest.
l<-animal_longevity[c(5,8)]

#1.9 What is the mean and median of maximum longevity of animals? Briefly comment these results.

mean(l$Maximum_Longevity_Years, na.rm = TRUE)
median(l$Maximum_Longevity_Years, na.rm = TRUE)

#1.10 Suppose you want to know which animals can outlive humans. According to the dataset (longevity), the maximum longevity for humans (Maximum_Longevity_Years) is 122.5. Will the following code work?
outliving_human <- longevity[longevity$Maximum_Longevity_Years > 122.5]

#it doesn't work.