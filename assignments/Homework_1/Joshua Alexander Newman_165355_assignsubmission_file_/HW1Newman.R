#Exercise 1
#1.1)
dim(animal_longevity)
length(animal_longevity)
#There are 4,219 observations and 10 variables.

#1.2)
daphnia <- 0.19
honey_bee <- 8.00
oriental_firebelly_toad <- 15.80

#1.3)
daphnia == honey_bee
#false
daphnia < oriental_firebelly_toad
#true
oriental_firebelly_toad > honey_bee
#true

#1.4)
name_animals <- c("Daphnia", "Honey Bee", "Oriental Firebelly Toad")
chosen_animals_longevity <- c(0.19, 8, 15.8)

#1.5)
summary(chosen_animals_longevity)
# Mean: 7.997 Median: 8.000

#1.6)
name_animals[2]
#[1]"Honey Bee"

#1.7)
chosen_animals_longevity>27
# [FALSE FALSE FALSE]

#1.8)
name_longevity <- animal_longevity[c(5,8)]
name_longevity[order(name_longevity$Maximum_Longevity_Years),]

#1.9)
summary(name_longevity$Maximum_Longevity_Years)
#The median is 15.20, the Mean is 25.21

#1.10)
#The code will not work. longevity is not the name of the dataset being used,
#therefore an error will occur. Also, if the goal is simply to see
#which animals outlive humans, it is easier to create a dummy variable which
#can tell us if an animal lives longer than humans.
animal_longevity$outliving_humans <- ifelse(animal_longevity$Maximum_Longevity_Years>122.5, "yes", "no")

#Exercise 2
#2.1)
summary(turnout)
dim(turnout)
length(turnout)
#There are a total of 14 observations over 10 variables, for a total of 140
#observations. The range of years for the data is 1980-2008.

#2.2)
totalVAP <- turnout$total/turnout$VAP*100
totalVEP <- turnout$total/(turnout$VEP+turnout$overseas)*100
summary(totalVAP)
summary(totalVEP)
#The the real turnout voting rate rises when one looks at VEP+overseas instead
#of VAP.

#2.3)
Dif_ANES_VAP <- turnout$ANES-totalVAP
Dif_ANES_VEP <- turnout$ANES-totalVEP
summary(Dif_ANES_VAP)
summary(Dif_ANES_VEP)
#The average difference between VAP and ANES is 19.97 (median) and 19.69 (mean).
#The average difference between VEP+overseas and ANES is 17.878 (median) and
# 17.560 (mean). As expected, the average distance is higher when comparing
#ANES to VAP than to VEP.

#2.4)
turn_pres <- turnout[c(1,3,5,7,9,11,13,14),]
turn_mid <- turnout[c(2,4,6,8,10,12),]
Dif_ANES_VEP_pres <- turn_pres$ANES-(turn_pres$total/(turn_pres$VEP+turn_pres$overseas)*100)
turn_pres$ANES-(turn_pres$total/(turn_pres$VEP+turn_pres$overseas)*100)
summary(Dif_ANES_VEP_pres)
Dif_ANES_VEP_mid <- turn_mid$ANES-(turn_mid$total/(turn_mid$VEP+turn_mid$overseas)*100)
turn_mid$ANES-(turn_mid$total/(turn_mid$VEP+turn_mid$overseas)*100)
summary(Dif_ANES_VEP_mid)
#It appears that the difference between ANES and real turnout is on average higher
#during presidential elections. However, there is much greater variation for
#midterm elections.

#2.5)
turn_old <- turnout[c(1,2,3,4,5,6,7),]
turn_young <- turnout[c(8,9,10,11,12,13,14),]
turn_old$ANES-(turn_old$total/(turn_old$VEP+turn_old$overseas)*100)
turn_young$ANES-(turn_young$total/(turn_young$VEP+turn_young$overseas)*100)
Dif_ANES_VEP_old <- turn_old$ANES-(turn_old$total/(turn_old$VEP+turn_old$overseas)*100)
Dif_ANES_VEP_young <- turn_young$ANES-(turn_young$total/(turn_young$VEP+turn_young$overseas)*100)
summary(Dif_ANES_VEP_old)
summary(Dif_ANES_VEP_young)
#The average difference between ANES and real total turnout is higher in the
#period 1994-2008 than in it is in the period 1980-1992. This may suggest that
#the ANES bias has been growing over the past few decades.