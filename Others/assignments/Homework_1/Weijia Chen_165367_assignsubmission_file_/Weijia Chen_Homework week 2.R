#Exercise 1: Animal longevity dataset
#Import data 

#1.1. How many variables and how many observations are there?
dim(animal_longevity)

#There are 10 variables and 4219 observations

#1.2. Pick three animals from the dataset. Create three objects (named after the animal) storing the value of their longevity.
black_lory <- 17.5
osprey <- 32.0
canada_goose <- 42.0

#is there a smarter way to do this?
#my try is: black_lory<- animal_longevity$Maximum_Longevity_Years(grepl("Black lory", animal_longevity$Organism_Common_Name))
#but it doesn't work...

#1.3. Create three logical tests comparing the longevity between the objects you created in question
black_lory == osprey 
osprey != canada_goose
canada_goose >= black_lory

#1.4. Create two vectors: 1.4.1. The names of all the animals you chose in question 1.2.; 1.4.2. Their respective lifespans.
animal_name <- c("black_lory", "osprey", "canada_goose")
lifespan <- c(black_lory, osprey, canada_goose)

#1.5. Calculate the mean and median of their lifespans.
mean(lifespan)
median(lifespan)

#1.6. Retrieve the 2nd value of the vector that contains your animal names.
animal_name[2]

#1.7. Subset which animals in your lifespans vector have a maximum longevity of above 27.
subset <- subset(lifespan, lifespan>27)

#subset is used to truncate the 

#1.8. Create a new dataset containing the maximum longevity of animals and their common names ranked from shortest to longest.
animal_longevity <- na.omit(animal_longevity) #how can I only clean NA in the longevity variable? --> Use the $ 

animal_longevity <- animal_longevity[order(animal_longevity$Maximum_Longevity_Years),]

new_dataset <- data.frame(animal_longevity$Organism_Common_Name, animal_longevity$Maximum_Longevity_Years) #can also use cbine()

#alternative way to do it 
longevity <- animal_longevity[c(5,8)]
View(longevity)

#1.9. What is the mean and median of maximum longevity of animals? Briefly comment these results.
mean_longevity <- mean(new_dataset$animal_longevity.Maximum_Longevity_Years)
mean_longevity
median_longevity <- median(new_dataset$animal_longevity.Maximum_Longevity_Years)
median_longevity

#mean=17.8, median=15.5, the distribution of observations in the dataset is skewed to the right-hand side.

#1.10. Suppose you want to know which animals can outlive humans. According to the dataset (longevity), the maximum longevity for humans (Maximum_Longevity_Years) is 122.5. Will the following code work?

#It doesn't work
#because a comma is missing 
#to specify that we're talking about the rows instead of columns



#Exercise 2: Self-reported Turnout

#import dataset

#2.1. Load the data into R and check the dimensions of the data. Also, obtain a summary of the data.
#How many observations are there? What is the range of years covered in this data set?

dim(turnout)
summary(turnout)
range(turnout$year)
#there are 14 observations, 10 variables, the range of years covered is 1980-2008

#2.2. Calculate the real turnout rate based on the voting age population (VAP) by year. This is, how many voters actually voted (VAP/TOTAL) in each year. Note that for this data set, we must add the total number of eligible overseas voters since the VAP variable does not include these individuals in the count. Next, calculate the real turnout rate using the voting eligible population (VEP). This is VEP/TOTAL. What difference do you observe?

#real turnout rate based on voting age population 
turnout_age <- 100* sum(turnout$total) / sum(turnout$VAP) 
turnout_age

#real turnout rate based on voting eligabible population 
turnout_eligible <- 100*sum(turnout$total) / (sum(turnout$VEP) + sum(turnout$overseas))
turnout_eligible

difference <- turnout_eligible - turnout_age
difference 
#the turnout rate based on eligible population is 2.2% higher than the turnout rate based on voting age population 

#2.3. Compute the differences between the self-estimated turnout rate (ANES) and the real turnout rate you just calculated in 2.2. How big is the difference on average? Conduct the same comparison for the VEP and ANES estimates of voter turnout. Briefly comment on the results.
mean(turnout$ANES)-mean(turnout_age)
mean(turnout$ANES)-mean(turnout_eligible)
#The ANES estimate is 20.0% higher than the real turnout rate based on voting age population on average,
#and 17.4% higher than the real turnout rate based on voting eligible population 
#this shows that the ANES estimate is biased 

#2.4. Compare the VEP turnout rate with the ANES turnout rate separately for presidential elections and midterm elections. Note that the data set excludes the year 2006. Does the bias of the ANES estimates vary across election types?
presidential <- subset(turnout, turnout$election_type == "presidential")
midterm <- subset(turnout, election_type == "midterm")

turnout_VEP_presidential <- 100*sum(presidential$total) / (sum(presidential$VEP) + sum(presidential$overseas))
turnout_VEP_presidential
turnout_VEP_midterm <-100*sum(midterm$total) / (sum(midterm$VEP) + sum(midterm$overseas))
turnout_VEP_midterm
dif_presidential <- mean(presidential$ANES)-mean(turnout_VEP_presidential)
dif_midterm <- mean(midterm$ANES)-mean(turnout_VEP_midterm)

dif_presidential-dif_midterm
#Yes, the bias of the ANES estimates vary across election types. The ANES estimates has bigger bias for presidential elections than midterm elections.


#2.5. Divide the data into half by election years such that you subset the data into two periods: older and newer elections. Calculate the difference between the VEP turnout rate and the ANES turnout rate separately for each year within each period. Has the bias of ANES increased over time?
median(turnout$year) #median = 1993
older <- subset(turnout, year < 1993)
newer <- subset(turnout, year > 1993)
turnout_VEP_older <- 100*sum(older$total) / (sum(older$VEP) + sum(older$overseas))
turnout_VEP_newer <- 100*sum(newer$total) / (sum(newer$VEP) + sum(newer$overseas))
dif_older <- mean(older$ANES) - mean(turnout_VEP_older)
dif_newer <- mean(newer$ANES) - mean(turnout_VEP_newer)
dif_older-dif_newer #=-1.87

#So the bias of ANES has increased over time

