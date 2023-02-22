library(readr)
animal_longevity <- read_csv("animal_longevity.csv")
View(animal_longevity)
#Exercise 1
# 1.1. How many variables and how many observations are there?
dim(animal_longevity)
# 1.2. Pick three animals from the dataset. Create three objects (named after the animal) storing the value of their longevity.
brownfalcon_longevity <- 24.0
sakerfalcon_longevity <- 15.9
merlin_longevity <- 12.7
# 1.3. Create three logical tests comparing the longevity between the objects you created in question 1.2.
brownfalcon_longevity==merlin_longevity
brownfalcon_longevity>sakerfalcon_longevity
sakerfalcon_longevity<merlin_longevity
# Create two vectors:
# 1.4.1. The names of all the animals you chose in question 1.2. 
names_sample <- c("Brown falcon", "Saker falcon", "Merlin")
# 1.4.2. Their respective lifespans.
longevity_sample <- c(brownfalcon_longevity, sakerfalcon_longevity, merlin_longevity)
# 1.5. Calculate the mean and median of their lifespans.
mean(longevity_sample)
median(longevity_sample)
# 1.6. Retrieve the 2nd value of the vector that contains your animal names. 
names_sample[2]
# 1.7. Subset which animals in your lifespans vector have a maximum longevity of above 27.
subset(names_sample, longevity_sample>27)
# 1.8. Create a new dataset containing the maximum longevity of animals and their common names ranked from shortest to longest.
longevity <- animal_longevity$Maximum_Longevity_Years
names <- animal_longevity$Organism_Common_Name
dataset <- data.frame(cbind(longevity,names))
dataset <- dataset[order(longevity),]
dataset[]
# 1.9. What is the mean and median of maximum longevity of animals? Briefly comment these results. x
mean(longevity)
median(longevity)
#Results for both show NA, as some values for some animals are not assigned.
#Excluding the NAs these are the mean and median
mean(longevity, na.rm=TRUE)
median(longevity, na.rm=TRUE)
#The mean is much superior to the median, showing inequality in the lifespan distribution, with some animals with a long lifespan skewing the mean
# 1.10 Suppose you want to know which animals can outlive humans. According to the dataset (longevity), the maximum longevity for humans (Maximum_Longevity_Years) is 122.5. Will the following code work?
outliving_humans <- longevity[longevity$Maximum_Longevity_Years>122.5]
# $ operator is invalid for atomic vectors 
# This solution works, but is not very elegant, as many NAs show  
outliving_humans <- names[(longevity>122.5)]
outliving_humans[]

