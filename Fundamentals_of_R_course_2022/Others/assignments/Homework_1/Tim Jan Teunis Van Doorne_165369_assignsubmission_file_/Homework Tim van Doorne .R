#HOMEWORK Tim van Doorne 

#1 The animal longevity dataset contains data on the maximum (known) lifespan of various animals. 
library(readr)
animal_longevity <- read_csv("Documents/Material for Week 2-20220930 3/animal_longevity.csv")

#1.1 How many variables and how many observations are there? 
dim(animal_longevity)


#A1.1: 4219 observations of 10 variables 


#Q1.2 Pick three animals from the dataset. Create three objects (named after the animal) storing the value of their longevity. 

tree_frog <- 12.80 
rainbow_frog <-2.0 
goliath_frog <- 5.9 

#Q1.3 Create three logical tests comparing the longevity between the objects you created in question 1.2. 
tree_frog == rainbow_frog
goliath_frog != rainbow_frog | tree_frog == rainbow_frog
tree_frog <= goliath_frog

#Q1.4.1. Create two vectors: The names of all the animals you chose in question 1.2. 
frog_names <- c("tree_frog", "rainbow_frog", "goliath_frog")

#Q1.4.2. Create two vectors: Their respective lifespans.
frog_lifespans <- c(tree_frog, rainbow_frog, goliath_frog)

#Q1.5 Calculate the mean and median of their lifespans
summary(frog_lifespans)

#A1.5: median = 5.9 ; mean = 6.9

#Q1.6 Retrieve the 2nd value of the vector that contains your animal names. 
frog_names[2]

#QQ1.7 Subset which animals in your lifespans vector have a maximum longevity of above 27. 
subset(frog_lifespans, frog_lifespans >27)


#Q1.8Create a new dataset containing the maximum longevity of animals and their common names ranked from shortest to longest. 
ordered_longevity <- data.frame(animal_longevity$Organism_Common_Name, animal_longevity$Maximum_Longevity_Years)
ordered_longevity <-ordered_longevity[order(ordered_longevity$animal_longevity.Maximum_Longevity_Years),]
View(ordered_longevity)

#Q1.9What is the mean and median of maximum longevity of animals? Briefly comment these results. 
summary(ordered_longevity)

#A1.9 median is 15.20, mean is 25.21, meaning that the there a couple of outliers, i.e. variables with very high lifespan that move the mean up

#Q1.10 Suppose you want to know which animals can outlive humans. According to the dataset (longevity), the maximum longevity for humans (Maximum_Longevity_Years) is 122.5. Will the following code work? 

outliving_humans <- ordered_longevity[ordered_longevity$animal_longevity.Maximum_Longevity_Years > 122.5]  

#doesn't seem to work, error is "undefined columns selected", see correct line below 

subset(ordered_longevity, animal_longevity.Maximum_Longevity_Years > 122.5)


#Q2.1 Load the data into R and check the dimensions of the data. Also, obtain a summary of the data. How many observations are there? What is the range of years covered in this data set? 
turnout <- read_excel("Documents/Material for Week 2-20220930 3/turnout.xls")
summary(turnout)

#A2.1 14 observations of 10 variables, range is from 1980 to 2008

#Q2.2 Calculate the real turnout rate based on the voting age population (VAP) by year. This is, how many voters actually voted (TOTAL/VAP) in each year.

turnout$real_turnout_1 <- turnout$total/(turnout$VAP)*100
turnout$real_turnout_2 <- turnout$total/(turnout$VEP + turnout$overseas)*100

#A2.2 You see that the turnout_2 is systematically higher than turnout_1, meaning that while some people have a voting age, they are not eligible. 

#Q2.3 
turnout$dif_ANES_turnout1 <- turnout$ANES - turnout$real_turnout_1
turnout$dif_ANES_turnout2 <- turnout$ANES - turnout$real_turnout_2

#A2.3 we see that the difference between ANEs and turnout2 is lower than the difference between ANES and turnout1 over the whole range of elections

#Q2.4 
aggregate(turnout$dif_ANES_turnout2, list(election = turnout$election_type), mean)
#When comparing the midterms with the presidential election, data suggests that the difference for the midterms is actually smaller


#Q2.4 

turnout$old_new <- ifelse(turnout$year > 1992, "new", "old")
aggregate(turnout$dif_ANES_turnout2, list(Old = turnout$old_new), mean)

#When comparing the older elections with the newer elections, data suggests that the difference has actually decreased from time


# END # 
