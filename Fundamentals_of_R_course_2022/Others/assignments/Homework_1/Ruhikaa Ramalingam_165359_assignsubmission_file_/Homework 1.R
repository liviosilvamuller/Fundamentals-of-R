## Fundamentals of R 
# Homework Block - 1
# Submitted by - Ruhikaa Ramalingam 
# Date - 06.10.22 

# Problem 1--------------------------------------------------------------------- 

#1. variables and observations 

summary(animal_longevity)
dim(animal_longevity)
length(animal_longevity)
#we have 4219 observations and 10 variables 

#2: picking three animals and creating three objects storing their longevity values 

mute.swan.age <- 70.0 
common.swift.age <- 21.1
emu.age <- 16.6

#3. logical tests

mute.swan.age == emu.age
#FALSE 
mute.swan.age > common.swift.age
#TRUE
common.swift.age < emu.age 
#FALSE 

#4. creating vectors 

Animals <- c("Mute Swan" , "Common Swift" , "Emu")
Longevity <- c(70.0, 21.1, 16.6)

#5. calculating mean and median 

mean.age = mean(Longevity)
mean.age
# 35.9 is the mean for animals 

median.age = median(Longevity)
median.age 
# 21.1 :p

#6. to retrieve the second value in Animals 

Animals[2]
#"Common Swift"

#7. creating subsets 

#First, I create a data frame, therefore when we create our subset we will know which animal has a maximum longevity of above 27 

animals.lifespan <- data.frame(cbind(Animals, Longevity))
animals.lifespan

animals.that.live.longer <- subset(animals.lifespan, Longevity > 27)
animals.that.live.longer

#8. Create a new dataset and order it 
animal_longevity$dsnew <- animal_longevity[,c("Organism_Common_Name","Maximum_Longevity_Years")]
animal_longevity$dsnew <- dsnew[order(dsnew$Maximum_Longevity_Years),]

#9. mean and median of maximum longevity 

summary(animal_longevity$Maximum_Longevity_Years)
#the mean of maximum longevity years of an animal is 25.21 and the median is 15.20 

#10. To check the provided code 

#The code 'outliving_humans <- longevity(longevity$Maximum_Longevity_Years > 122.5) will not work as the data set is not called 'longevity'  
outliving_humans <- subset(animal_longevity, animal_longevity$Maximum_Longevity_Years > 122.5)
outliving_humans
#The code mentioned here would work, we could also create a dummy variables using if else and can lsit the animals that can outlive humans 

#Problem 2----------------------------------------------------------------------

#1. To check the dimensions and obtain a summary 

summary(turnout)
dim(turnout)
length(turnout)
#The data has 14 observations and 10 variables. The years covered is from 1980 to 2008. 

#2.Calculating the real turnout rate 

turnout$v.real.total <- turnout$VEP + turnout$overseas 
turnout$VEP1 <- (turnout$total/turnout$v.real.total)*100
turnout$VAP1 <- (turnout$total/turnout$VAP)*100
mean(turnout$VEP1 - turnout$VAP1) #the average difference is 2.13 

#The average difference between voting age population and eligible voting population is 2.13 percent 


#3.

turnout$diff1 <- (turnout$ANES - turnout$VAP1)
turnout$diff2 <- (turnout$ANES - turnout$VEP1)
mean(turnout$diff1) #average difference is 19.69
mean(turnout$diff2) #average difference is 17.60
#In both the scenarious it seems like the self-estimated turnout seems to be larger than the actual turnout 

#4. 

Pres <- subset(turnout, turnout$election_type == "presidential")
mean(Pres$ANES - Pres$VEP1) #the average difference is 18.75

Midterm <- subset(turnout, turnout$election_type == "midterm")
mean(Midterm$ANES - Midterm$VEP1) #the average difference is 15.98 

#The ANES bias does seem to exist in both the election type 

#5. 

older <- subset(turnout, turnout$year <= 1992)
check.bias.older <- (older$ANES - older$VEP1)
mean(check.bias.older) #the average difference is 16.48 

newer <- subset(turnout, turnout$year > 1992)
check.bias.newer <- (newer$ANES - newer$VEP1)
mean(check.bias.newer) #the average difference is 18.64
 