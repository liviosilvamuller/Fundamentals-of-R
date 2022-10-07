#Question 1.1
View(animal_longevity)
dim(animal_longevity) #10 variables and 4219 observations
#1.2
blackfin_pearlfish <- 1 
devilfish <- 14
barnowl <- 34
#1.3
blackfin_pearlfish <= devilfish
devilfish > barnowl
barnowl == blackfin_pearlfish
#1.4
lifespans <- c(1,14,34) #lifespan vector
names <- c("blackfin_pearlfish","devilfish","barnowl") #names vector
#1.5
mean(lifespans)
median(lifespans)
#1.6
names [2] #2nd value in the vector 'names'
#1.7
names[lifespans>27]
animals <- data.frame(names,lifespans)
View(animals) #this was just for me to check that I was on the right track with the vectors
#1.8
animal_longevity <- animal_longevity[order(animal_longevity$Maximum_Longevity_Years),]
Common_Names <- animal_longevity$Organism_Common_Name
Longevity_Years <- animal_longevity$Maximum_Longevity_Years
common_names <-cbind(Common_Names,Longevity_Years)
View(common_names)
#1.9
summary(animal_longevity) #this gives the mean of the Maximum Longevity Years as 25.21 and the Median as 15.20
#1.10
outliving_humans <- longevity[longevity$Maximum_Longevity_Years > 122.5] #this code will not work. 
# Question 2.1
summary(turnout)
dim(turnout)
length(turnout$year)
range(turnout$year)
# there are 14 observations of 11 variables in this dataset, the range is between 1980 to 2008 (every 2 years)
#2.2
turnout$electorate <- turnout$VEP + turnout$overseas
turnoutVEP <- turnout$total/turnout$electorate
turnoutVEP <- turnoutVEP*100
View(turnoutVEP)
turnoutVAP <- turnout$total/turnout$VAP
turnoutVAP <- turnoutVAP*100
turnoutVAP - turnoutVEP
#2.3 I'm not sure this is right but I didn't know how else to do it so I tried it using this logic: 
turnoutANES <- turnout$ANES
turnoutANES - turnoutVAP
ANES_VAP_difference <- c(turnoutANES - turnoutVAP)
mean(ANES_VAP_difference)
turnoutANES - turnoutVEP
ANES_VEP_difference <- c(turnoutANES - turnoutVEP)
mean(ANES_VEP_difference)
#2.4
#I tried using the ifelse function to sort the data into presidential and midterms but it didn't give the results
#I wasn't sure which other methods to try 
#I also was not fully able to figure out question 2.3 and it seemed like the other questions would utilise similar methods

