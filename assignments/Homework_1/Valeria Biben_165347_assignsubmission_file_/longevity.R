#there are 10 variablea and 4220 observations
View(animal_longevity)
dim(animal_longevity)
length(animal_longevity)
Fruit_fly <- 0.3
Honey_bee <- 8
American_lobster <- 100
Fruit_fly == Honey_bee
Honey_bee > Fruit_fly
Fruit_fly < American_lobster
names <- c(Fruit_fly, Honey_bee, American_lobster)
lifespan <- c(0.3, 8, 100)
mean(lifespan)
median(lifespan)
names[2]
ifelse (lifespan > 27, "true", "false")
newframe <- animal_longevity[, c(5, 8)]
View(newframe)
newframe$...8
sort(newframe$...8, decreasing = FALSE, na.last = TRUE)
order(newframe$...8, na.last = TRUE, decreasing = FALSE)
mean(newframe$...8, na.rm = TRUE)
median(newframe$...8, na.rm = TRUE)
max(newframe$...8, na.rm = TRUE)
outliving_humans <- ifelse(newframe$...8 > 122.5, "yes", "no")
