#Setting working directory
setwd("~/Desktop/IHEID/Fall 2022/R/Material for Week 2-20220930")

#Exercise 1: Animal Longevity
#Importing first dataset
library(readr)
animal_longevity <- read_csv("animal_longevity.csv")
View(animal_longevity)

#Question 1
#1.1
##There are ten variables - each column heading refers to a particular variable.
##There are 4219 observations 

#1.2
Honey_bee <- 8.00
Fruit_fly <- 0.30
Green_toad <- 12.40

#1.3
Honey_bee == Fruit_fly
Green_toad >= Fruit_fly
Honey_bee <= Green_toad

#1.4
Animals <- c("Honey bee", "Fruit fly", "Green toad")
Lifespan <- c(8.00, 0.30, 12.40)

#1.5
mean(Lifespan)
median(Lifespan)

#1.6
Animals[2]

#1.7
subset(Lifespan, Lifespan > 27)

#1.8
Data <- animal_longevity[c(5,8)]
Data<- Data[order(Data$Maximum_Longevity_Years), ]
View(Data)

#1.9
mean(animal_longevity$Maximum_Longevity_Years, na.rm = TRUE)
median(animal_longevity$Maximum_Longevity_Years, na.rm =TRUE)

#The above code ignores all the observations with no value. The mean maximum longevity of animals is 25.21315, i.e., the average maximum longevity is 25.21315 years. The median (middle number in the dataset) is 15.2.

#1.10
# The code: outliving_humans <- animal_longevity[animal_longevity$Maximum_Longevity_Years > 122.5]  doesn't work.

#Correct code: 
outliving_humans <- animal_longevity[animal_longevity$Maximum_Longevity_Years > 122.5, ]


#Exercise 2: Self-reported Turnout
#Importing second dataset
library(readxl)
turnout <- read_excel("turnout.xls")
View(turnout)   

#Question 2
#2.1
dim(turnout)
summary(turnout)
#There are 14 observations.The dataset covers the years 1980 to 2008 with an interval of 2 years.

#2.2 
realturnoutrate <- turnout$total/turnout$VAP*100
realturnoutrate

totalvep <- turnout$VEP + turnout$overseas
realturnoutrate2 <- turnout$total/totalvep*100
realturnoutrate2

#We observe that the real turnout rate based on the eligible population is higher than the turnout rate based on the voting age population.
#This reflects the fact that just because someone is within the stipulated age range to vote does not mean that they are necessarily eligible to do so. 
#Therefore the total voting eligible population is smaller than the voting age population thus making the turnout higher in the eligible case when compared to the voting age population turnout.

#2.3
diff1 <- turnout$ANES - realturnoutrate
diff1
mean(diff1)

diff2 <- turnout$ANES - realturnoutrate2
diff2
mean(diff2)

#As we can see from the above results, the self reported turnout rates (ANES) are always higher than the ones observed in reality. This could be explained by social desirability bias.
#Since the turnout rate based on eligible population is higher than that based on the voting age population, the difference between ANES and the former is lower than the difference between ANES and the turnout based on voting age population. 

#2.4
presidentialdata <- subset(turnout, turnout$election_type == "presidential")
totalpresidentialvep <- presidentialdata$VEP + presidentialdata$overseas
presidentialturnout <- presidentialdata$total/totalpresidentialvep*100
presidentialANES <- presidentialdata$ANES
presidentialdiff <- presidentialANES - presidentialturnout
mean(presidentialdiff)

midtermdata <- subset(turnout, turnout$election_type == "midterm")
totalmidtermvep <- midtermdata$VEP + midtermdata$overseas
midtermturnout <- midtermdata$total/totalmidtermvep*100
midtermANES <- midtermdata$ANES
midtermdiff <- midtermANES - midtermturnout
mean(midtermdiff)

#As the above results indicate, the ANES bias does translate across different types of elections (presidential as well as midterm)

#2.5
#Before 1992
Early <- subset(turnout, turnout$year <= 1992)
earlyvep <- Early$VEP + Early$overseas
earlyturnout <- Early$total/earlyvep*100
earlyANES <- Early$ANES
earlydiff <- earlyANES - earlyturnout
mean(earlydiff)

#After 1992
Late <- subset(turnout, turnout$year > 1992)
latevep <- Late$VEP + Late$overseas
lateturnout <- Late$total/latevep*100
lateANES <- Late$ANES
latediff <- lateANES - lateturnout
mean(latediff)

earlydiff
latediff

#As the above results show, the difference between the ANES and the real turnout rate is higher towards the second half of the dataset. This shows that the bias seems to have increased with time. 
