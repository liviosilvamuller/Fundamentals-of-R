# 1.1
dim(animal_longevity)
length(animal_longevity)
summary(animal_longevity)

# 1.2 
View(animal_longevity)
giraffe_lifespan <- 29.5
penguin_lifespan <- 26
elephant_lifespan <- 65

# 1.3
giraffe_lifespan == penguin_lifespan
elephant_lifespan != penguin_lifespan
giraffe_lifespan > penguin_lifespan 

# 1.4
theanimals <- c("giraffe", "penguin", "elephant")
lifespans <- c(giraffe_lifespan, penguin_lifespan, elephant_lifespan)

# 1.5
mean(lifespans)
median(lifespans)

# 1.6
theanimals[2]

# 1.7
theanimals[lifespans > 27]

# 1.8
animal_longevity[, c(5, 8)]
animal_longevity[c(5,8)]
animal_longevity[order(animal_longevity$Maximum_Longevity_Years),]

# 1.9
mean(animal_longevity$Maximum_Longevity_Years, na.rm=TRUE)
median(animal_longevity$Maximum_Longevity_Years, na.rm=TRUE)

# 1.10
# No, it does not!
longevity <- animal_longevity
outliving_human <- longevity[longevity$Maximum_Longevity_Years > 122.5,]
# This works, but it is not a great solution...
subset(animal_longevity,
       animal_longevity$Maximum_Longevity_Years > 122.5)

# 2.1
summary(turnout)
dim(turnout) # 14 obs, 10 variables
range(turnout$year) # 1980 to 2008

#2.2

turnout$real <- (turnout$total/turnout$VAP)*100
turnout$real_os <- ((turnout$total)/(turnout$overseas + turnout$VEP))*100

# If you want to get the ballots overseas in...
turnout$osvoters <- as.numeric(gsub("NA", 0, turnout$osvoters))
turnout$real_os <- ((turnout$total + turnout$osvoters)/(turnout$overseas + turnout$VEP))*100

#2.3

mean(turnout$real - turnout$ANES) # overestimate by 19.7%
mean(turnout$real_os - turnout$ANES) # overestimate by 17.6%

# 2.4

presid <- subset(turnout,
                 turnout$election_type == "presidential")
midt <- subset(turnout, turnout$election_type == "midterm") 

mean(presid$ANES - presid$real) # overestimate by 21.2 %
mean(midt$ANES - midt$real) # overestimate by 17.7 %

# 2.5

mean(turnout$year)
dim(turnout)

older <- turnout[1:7,]
older <- subset(turnout, turnout$year < 1993)

newer <- turnout[8:14,]
newer <- subset(turnout, turnout$year > 1993)

range(older$year)
min(older$year)
max(older$year)
range(newer$year)

mean(older$ANES - older$real) # overestimate by 17.9 %
mean(older$real)

mean(newer$ANES - newer$real) # overestimate by 21.5 %
mean(newer$real)
