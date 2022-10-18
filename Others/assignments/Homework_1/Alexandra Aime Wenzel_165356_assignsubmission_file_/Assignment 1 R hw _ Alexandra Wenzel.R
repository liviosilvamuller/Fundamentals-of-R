# Alexandra Wenzel, Homework 1, October 1, 2022 
# Exercise 1: Animal Longevity Dataset
setwd("~/Desktop/fundamentals of R/Material for Week 2-20220930")
# Question 1.1 : 10 Variables because each column include a different variable. 4,219 observations because observation is a different row.
# Question 1.2
Daphnia <- 0.19
Fruit_Fly <- 0.30
Honey_Bee <- 8.00
# Question 1.3 
Daphnia != Fruit_Fly
Fruit_Fly >= Honey_Bee
Fruit_Fly & Honey_Bee > Daphnia
# Question 1.4 
Animals <- c("Daphnia", "Fruit_Fly", "Honey_Bee")
Lifespan <- c(0.19, 0.30, 8.00)
# Question 1.5 
mean(Lifespan)
median(Lifespan)
# Question 1.6
Animals [2]
# Question 1.7 
subset(Lifespan, Lifespan > 27)

# Question 1.8
data <- animal_longevity[c(5, 8)]
data
data<- data[order(Data$Maximum_Longevity_Years), ]
View(data)

#Question 1.9
mean(na.omit(animal_longevity$Maximum_Longevity_Years))
median(na.omit(animal_longevity$Maximum_Longevity_Years))
# I got the results 25.21315 and 15.2, respectively. The mean being larger than the median shows that the distribution is positively skewed to the right.
# I also saw on stackoverflow we could use the code below to get the same answer, is one (na.rm or na.omit) better than the other?
mean(na.omit(animal_longevity$Maximum_Longevity_Years),na.rm=TRUE)
median((animal_longevity$Maximum_Longevity_Years),na.rm=TRUE)

#Question 1.10
# The code will not work because it should have a comma at the end: 
outliving_humans <- animal_longevity[animal_longevity$Maximum_Longevity_Years > 122.5, ]

# Exercise 2: Self-reported Turnout
# Question 2.1
dim(turnout)
summary(turnout)
# 14 Observations because there are 14 rows, as shown in the first dimension. The years range from 1980 to 2008.

# Question 2.2
((turnout$total)/(turnout$VAP))*100
((turnout$total)/(turnout$VEP+turnout$overseas))*100
# The difference is that the percentage is higher for VEP than VAP because the VAP is a larger number since more people are of the voting age than are eligible to vote.

#Question 2.3
((turnout$ANES)-(((turnout$total)/(turnout$VAP))*100))
mean((turnout$ANES)-((turnout$total)/(turnout$VAP)*100))
#The difference on average is 19.69001
(turnout$ANES)-(((turnout$total)/(turnout$VEP+turnout$overseas))*100)
mean((turnout$ANES)-(((turnout$total)/((turnout$VEP+turnout$overseas)))*100))
# The difference on average is 17.55991
# The results show that voters self-estimated turnout rate is higher than the voter eligible and the voting age population. The difference is greater when looking at the voter age population than the voter eligible population which would make sense as not all those who are the age to vote can vote.

# Question 2.4 
presidential <- subset(turnout, turnout$election_type == "presidential")
(presidential$ANES)-(((presidential$total)/(presidential$VEP+presidential$overseas))*100)
mean((presidential$ANES)-(((presidential$total)/(presidential$VEP+presidential$overseas))*100))
midterms <- subset(turnout, turnout$election_type == "midterm")
(midterms$ANES)-(((midterms$total)/(midterms$VEP+midterms$overseas))*100)
mean((midterms$ANES)-(((midterms$total)/(midterms$VEP+midterms$overseas))*100))
# The bias of the ANES varies across elections types, with the bias bigger for presidential elections (18.74622) compared to midterm elections (15.97816)
## side note: I also got the same answer using an ifelse statement, as seen below
ifelse(turnout$election_type == "presidential", (turnout$ANES)-(((turnout$total)/(turnout$VEP+turnout$overseas))*100), NA)
mean((ifelse(turnout$election_type == "presidential", (turnout$ANES)-(((turnout$total)/(turnout$VEP+turnout$overseas))*100), NA)), na.rm=TRUE)
ifelse(turnout$election_type == "midterm", (turnout$ANES)-(((turnout$total)/(turnout$VEP+turnout$overseas))*100), NA)
mean((ifelse(turnout$election_type == "midterm", (turnout$ANES)-(((turnout$total)/(turnout$VEP+turnout$overseas))*100), NA)), na.rm=TRUE)

# Question 2.5 
older_elections <- subset(turnout, turnout$year<=1992)
older_elections
(older_elections$ANES)-(((older_elections$total)/(older_elections$VEP+older_elections$overseas))*100)
mean((older_elections$ANES)-(((older_elections$total)/(older_elections$VEP+older_elections$overseas))*100))
newer_elections <- subset(turnout, turnout$year>1992)
newer_elections
(newer_elections$ANES)-(((newer_elections$total)/(newer_elections$VEP+newer_elections$overseas))*100)
mean((newer_elections$ANES)-(((newer_elections$total)/(newer_elections$VEP+newer_elections$overseas))*100))
# By doing the mean of the newer and older elections the bias of ANES has increased over time from 16.48163 to 18.63819.  

