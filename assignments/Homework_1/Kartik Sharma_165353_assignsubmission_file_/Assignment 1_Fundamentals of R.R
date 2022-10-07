#Assignment Part 1

#Observations = 4219
#Variables = 10
summary(animal_longevity)
"Daphnia" == "Fruit Fly"
"Fruit Fly" > "Honey Bee"
"Daphnia" <= "Honey Bee"

animals <- c("Daphnia", "Fruit Fly", "Honey Bee")
lifespan <- c(0.19, 0.30, 8.00)

median(lifespan)
mean(lifespan)

animals [2]

animal_longetivity$above_27 = ifelse(animal_longetivity$lifespan > 27, TRUE, FALSE)

animals = ifelse(lifespan > 27, "yes", "no")

animals[animals$lifespan > 27]

subset(animals, lifespan > 27)


animal_longetivity <- data.frame(cbind(Organism_Common_Name, Maximum_Longetivity_Years))

animal_longetivity <- data.frame(animals, lifespan)
sort(lifespan, decreasing = FALSE)

longevity <- animal_longetivity[c(5,8)]

mean(lifespan)
median(lifespan)

# The average lifespan of my selected animals is 2.83 years whereas the median age is 0.3 years. 
# Interpreting this may lead to the analysis that an average lifespan of a Daphnia, Fruit Fly or Honey Bee may be around 2.83 years however the Honey Bee has a significantly larger lifespan as compared to the two others that increases the mean results.
# Ordering the data in ascending order has placed the Fruit Fly's Maximum Lifespan in the middle hence the retention of the same value.

outliving_humans <- longevity[longevity$Maximum_Longevity_Years > 122.5]

#My code shows there is an error in finding "longevity" as an object

#Assignment Part 2


summary(turnout)
length(turnout)
dim(turnout)
14*10
#Observations = 140
#Range of Years Covered = 1980-2008

turnout$electorate <- turnout$VEP + turnout$overseas

real_turnout <- turnout$total / turnout$electorate

real_turnout2 <- turnout$total / turnout$VAP

# The two turnouts do not add up, there is a significant difference between the two.
# The real turnout calculate with the VEP and the overseas citizens has consistently higher values (or voters) as compared to the turnout as mentioned in the data.


turnout$real <- real_turnout * 1000 
difference_turnout_1 <- turnout$real - turnout$ANES
# The difference in between 300-500 voters on average.
difference_turnout_2 <- turnout$VEP - turnout$ANES
mean(difference_turnout_2)

# There is an even bigger difference between the VEP and ANES turnout. 
# The difference seems to be quite a bit that seems a little impractical. I am not sure if I am doing the right thing then.

Pre_elc <- turnout$ANES[c(1, 3 ,5 ,7 ,9 ,11, 13, 14)] - VEP.turnout.rate[c(1, 3 ,5 ,7 ,9 ,11, 13, 14)]
Pre_elec <- turnout$ANES - turnout$VEP

# I also can't seem to find a positive result to this question.
# I think I am on the right track with the code and thinking but cannot seem to understand where to go foward or even where I am going wrong.
#I think I got the seperation part correct but do not know how to proceed.

Prd1 <- turnout[1:7,]
Prd2 <- turnout[8:14,]


Prd1$ANES[1:7] - real_turnout(Prd1$total + Prd1$VEP + Prd1$overseas + Prd1$voters)
Prd2$ANES[1:7] - real

# I am able to split the two table but I unable to figure out the rest of the question.
# Prd1$ANES[1:7] - turnout.rate(Prd1$total,Prd1$osvoters,Prd1$VEP,Prd1$overseas)
# This is the code I have managed to find or come to but I have no idea how it works and it is not yielding me any positive results.
# I am particularly super confused about the "turnout.rate" syntax
