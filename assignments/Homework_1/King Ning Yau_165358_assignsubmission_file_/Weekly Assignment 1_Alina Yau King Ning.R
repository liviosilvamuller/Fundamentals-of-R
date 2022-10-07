#Exercise 1: Animal longevity dataset  
setwd("~/Desktop")

#1.1 
animal<- read_csv(file="animal_longevity.csv", col_names = TRUE) 
View(animal)
#1.1 Answer: 10 Variables, 4219 obervations 

#1.2
daphnia <- 	0.19fruitfly <-0.30honeybee <-8.00

#1.3
daphnia >= fruitfly
fruitfly >= honeybee
daphnia <= honeybee

#1.4
threeanimals<- c("Daphnia", "Fruit fly", "Honey bee")
lifespans <- c(daphnia, fruitfly, honeybee)

#1.5
mean(lifespans)
median(lifespans)
#1.5 Answer: mean=2.83, median=0.3

#1.6
threeanimals[2]

#1.7
subset(lifespans, lifespans > 27)
#1.7 Answer: none of the chosen values in my vector is >27.

#1.8
longevity <- animal[order(animal$Maximum_Longevity_Years), 5:8]

#1.9
longvalue<-c(as.numeric(animalrank$Maximum_Longevity_Years))
mean(longvalue)
median(longvalue)
#1.9 Answer: mean=25.21314535, median=15.2
#Comment: Mean is significantly larger than the median. This shows that the variation of the value is large. The disrtibution of the data is uneven, and it is likely to be positively skewed to the right.

#1.10
#Answer: No, the code does not work. Can use the subset function to do so.
outliving_humans <-subset(animal, animal$Maximum_Longevity_Years>122.5)


#Exercise 2: Self-reported Turnout 

#2.1
turnout<- read_csv(file="turnout.csv", col_names = TRUE)
summary(turnout)
2008-1980
#2.1 Answer: 14 obervations, range of years: 28

#2.2
turnout$VAPrate<- turnout$total/turnout$VAP
turnout$VEPrate<- turnout$total/turnout$VEP+turnout$Overseas
#2.2 Answer: 
# Real turnout rate calculated based on VAP in 14 years: 0.5261030 0.4072566 0.5325038 0.3652780 0.5033937 0.3645217 0.5472591 0.3846501 0.4812765 0.3532996 0.5003015 0.3637857 0.5550387 0.5687307
# Real turnout rate calculated based on VEP in 14 years: 0.5359023 0.4162291 0.5448157 0.3765150 0.5209115 0.3784916 0.5734207 0.4063034 0.5097434 0.3751455 0.5341718 0.3886261 0.5898141 0.6015228
#Observation: real turnout rate calculated based on VAP is lower than the rate calculated based on VEP in each year.

#2.3
turnout$ANES-turnout$VAPrate
turnout$ANES-turnout$VEPrate
#2.3 Answer: 
# Differences with weal turnout rate calculated based on VAP in 14 years: 0.1838970 0.1927434 0.2074962 0.1647220 0.1966063 0.1054783 0.2027409 0.1753499 0.2487235 0.1667004 0.2296985 0.2562143 0.2149613 0.2112693
# Differences with weal turnout rate calculated based on VEP in 14 years:  0.17409767 0.18377091 0.19518426 0.15348504 0.17908847 0.09150841 0.17657930 0.15369658 0.22025661 0.14485454 0.19582821 0.23137389 0.18018592 0.17847723
#Comments: The self-estimated turnout rate (ANES) is significantly higher than the actual turnout rate. It reflects an overestimation on the voting rate.

#2.4
ifelse (turnout$election_type=="presidential",turnout$ANES/100-turnout$VEPrate, 0)
ifelse (turnout$election_type=="midterm",turnout$ANES/100-turnout$VEPrate, 0)
#2.4 Answer:
# Differences with real turnout rate in presidential election: 0.1740977 0.1951843 0.1790885 0.1765793 0.2202566 0.1958282 0.1801859 0.1784772
# Differences with real turnout rate in midterm election: 0.18377091 0.15348504 0.09150841 0.15369658 0.14485454 0.23137389
#The bias of the ANES does not estimates vary across election types.

#2.5
oldelection<-subset(turnout, turnout$year<=1992)newelection<-subset(turnout, turnout$year>1992)
oldelection$ANES/100-oldelection$VEPrate
newelection$ANES/100-newelection$VEPrate
#Differences with real turnout rate in older elections: 0.17409767 0.18377091 0.19518426 0.15348504 0.17908847 0.09150841 0.17657930
#Differences with real turnout rate in newer elections: 0.1536966 0.2202566 0.1448545 0.1958282 0.2313739 0.1801859 0.1784772
mean (oldelection$ANES/100-oldelection$VEPrate)
mean(newelection$ANES/100-newelection$VEPrate)
#2.5 Answer: The bias did not increase continously overtime (there is ups and downs over the years), but comparing the mean bias value of older and newer election period, the bias has increased from older elections to newer elections.