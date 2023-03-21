# Title: Fundamentals of R
# Purpose: Block 1 - Practical
# Authors: Henrique Sposito & Livio Silva-Muller

# Mind the GAP, with GAPMINDER
# Do you know them?
# https://www.gapminder.org/

# 1 Load the data---------------------------------------------------------------

# Several other R packages have datasets included.
# One of these is gapminder, let's download, load, and see the data
# available in the package.

# install.packages("gapminder")
library(gapminder)

# But we are not sure what the package really is, are we?
# We can use the "?" here or, better, find the package manual!
?gapminder

# If you cannot find help locally with "?" before package or function,
# please try with "??" before package name or function.
# "?" searches documentation, "??" search the help system.
# want to find more about the difference? Run: ´?'?'´ and ´?'??'´.
# The ´´ "delimits" code.
# That is, commands you can/should run in your console.
# Please remember to run the code without ´´ though...

# All CRAN packages have a detailed manual, that you can find on google
# It may be enough to go through the function index in the help page.

# Load internal package data.
data("gapminder")
View(gapminder)

# 2 Let's go on and investigate the data a bit.---------------------------------
summary(gapminder)
 
#install.package("skimr")

#skimr::

# The '::' specifies the package (skimr) and, therefore, can be used to:
# 1- specify the package function without loading the whole package
# 2- hit tab after '::'browse through the package functions and data interactively
# For more info: ´?'::'´

dim(gapminder)
length(gapminder)

#there are different ways to navigate dataframes...

gapminder[3,1] # for value in the third row in the first column
gapminder[1,] # for values in the first row but all columns
gapminder[,1] # for values in the first column but all rows 
gapminder[1704,] # for values the last row but all columns
#see ?"["

# We can use the "$" operator access variables in a dataframe.
gapminder$country

# Country and year data...

# Do you think we have data for all countries all years?
summary(gapminder$country)
summary(gapminder$year) #year is being understood as numeric, but is it numeric?
summary(as.factor(gapminder$year))

# Yes, all countries appear 12 times, all years appear 142 times!

# 3 Let's manipulate the data---------------------------------------------------

#we start by creating three objects that contains the life expectancy of Germany,
#Switzerland, and France in 2007

gapminder[gapminder$country == "Switzerland",]
gapminder[gapminder$country == "France",4]
gapminder[gapminder$country == "Germany",3:4]

life_exp_ch <- 81.7
life_exp_fr <- 80.7
life_exp_gr<-79.4

life_exp_ch == life_exp_fr #this is a logical test
life_exp_ch < life_exp_fr #this is a logical test

#what about this: 

#life_exp_ch = life_exp_fr

#  "=" in R is a synonym  of "<-", 
# so the life above assigned the value of life_exp_fr to life_exp_ch
# In general, however, you should stick to "<-" not to confuse yourself.

# let's create a vector

lifeExp <- c(life_exp_ch,life_exp_fr,life_exp_gr)

mean(lifeExp)
median(lifeExp)

mean(gapminder$lifeExp)
median(gapminder$lifeExp)

#we can also try to add new information to our vector

lifeExp$date<-"2007" # what happened here? R created a list.
lifeExp <- as.data.frame(lifeExp)
lifeExp$date<- "2007"

mean(lifeExp) #what is this error about?

class(lifeExp$date) #why is 2007 a character?
mean(lifeExp$date)

lifeExp$date<- 2007 #this substitutes the previous date variable
class(lifeExp$date)
mean(lifeExp$date)

#now let's create a new variable in the dataset

gapminder$republic <- ifelse(grepl("Rep.", gapminder$country), 
                                 "Republic", "Not a Republic")

# 4 Life expectancy, Americas vs. Europe ---------------------------------------

# What if we want to start by comparing mean (average) life expectancy in the
# Americas and Europe?

# First, let's subset Americas.
AM <- subset(gapminder, continent == "Americas")
AM

# Let's get the mean (average) and median life expectancy for the Americas.
meanAM <- mean(AM$lifeExp)
meanAM

medianAM <- median(AM$lifeExp)
medianAM

# Let's do the same for Europe!
EU <- subset(gapminder, continent == "Europe")
meanEU <- mean(EU$lifeExp)
medianEU <- median(EU$lifeExp)

# Let's subtract to find out their differences!
meanEU - meanAM
# Around 7 years of difference, in average!
medianEU - medianAM
# Around 5 years of difference... Why is it different?

# Let's create a data frame to facilitate visualization.
EU_AM <- data.frame(Continent = c("Europe", "Americas"),
                    'Median Life Expectancy' = c(medianEU, medianAM),
                    'Mean Life Expectancy' = c(meanEU, meanAM))
EU_AM

# We can also perform arithmetic operations on columns from the data frames,
# this operate row by row and return an answer the same length as the
# variables being operated. Remember, we can operate in variables of the
# same class as long as they are numeric
# (and other class that allows for operations).

EU_AM$difference <- EU_AM$Median.Life.Expectancy - EU_AM$Mean.Life.Expectancy

EU_AM

# What does a mean smaller than a median means?
# That the distribution of observations is skewed to the left.
# That is, most countries fall below the mean because some outlier countries
# with higher life expectancy skew the data.
# This is specially the for the Americas, why do you think?

# 5 Life expectancy and GDP, Global North vs. Global South ---------------------

# How about we plot the distributions of observations for
# Europe and for Americas in terms of life expectancy and GDP?
# First thing we need to do here, before ploting,
# is to reshape the data.

# Let's subset only the continents we are interested in.
subset_AMEU <- subset(gapminder, continent == "Americas"|continent == "Europe")
subset_AMEU

# Now let's get the mean of life expectancy and GDP per capita in all years, for
# each country
# aggregate() is a pretty awesome function.
# The help file for it is very detailed as well,
# I suggest you read it!

aggregate(gdpPercap ~ country,
          data = subset_AMEU, fun=mean) #this gives you gdpPercap

aggregate(cbind(lifeExp, gdpPercap) ~ country,
          data = subset_AMEU, mean) #this gives you gdpPercap and lifeExp

lifeexp_gdp <- aggregate(cbind(lifeExp, gdpPercap) ~ country+continent,
                     data = subset_AMEU, mean) 

#this gives you gdpPercap and lifeExp while retaining continent

lifeexp_gdp

# A simple histogram can tell us the distribution of means BTW!
hist(lifeexp_gdp$lifeExp)
# Most countries in the Americas and Europe sample have average life
# expectancy from 65 to 75 years!
# Do you think most of these countries, with an average life
# expectancy from 65 to 75 years, come from Europe or the Americas?

hist(lifeexp_gdp$gdpPercap)
# Many countries in the Americas and Europe sample have average GDP,
# per capita, between 5000 to 10000 dollars per year.

# Let's finally plot this!
# Base R makes plotting basic relations relatively easy with
# several generic plotting functions (see ´?plot´)

# With base R, we first draw the plot basics and then fill it up!
plot(x = lifeexp_gdp$lifeExp, y = lifeexp_gdp$gdpPercap,
     type = "n",
     main = "Average life expectancy by Country",
     xlab = "Average life expectancy (years)",
     ylab = "Average GDP (per capita)")

# Add points for countries in each continent
points(x = lifeexp_gdp$lifeExp[lifeexp_gdp$continent == "Americas"],
       y = lifeexp_gdp$gdpPercap[lifeexp_gdp$continent == "Americas"],
       pch = 16, col = "yellow")
points(x = lifeexp_gdp$lifeExp[lifeexp_gdp$continent == "Europe"],
       y = lifeexp_gdp$gdpPercap[lifeexp_gdp$continent == "Europe"],
       pch = 16, col = "red")

# Shall we try to add labels to the dots in the scatterplot?
text(x = lifeexp_gdp$lifeExp,
     y = lifeexp_gdp$gdpPercap - 100, # placement
     labels = lifeexp_gdp$country, cex=0.6, font=2)

# What about a legend?
legend(x = "topleft", title = "Continent",
       legend=c("Americas", "Europe"), 
       fill = c("yellow","red"))

# What does this plot tells you about the distribution of average
# life expectancy across countries in the Americas and Europe?
# Why are some European countries with relatively low GDP per capita
# (e.g. France and Spain) still have a higher life expectancy
# than American countries with relatively high GDP per capita
# (e.g. Brazil and Mexico)?

# 6 Repository with extra material for reading and playing with-----------------

# Life expectancy, GDP, and Population by country, in time 
# Which countries have the highest life expectancy overall?
# Let's just see the 10 countries with the highest
# (maximum) life expectancy.
countryle <-  aggregate(lifeExp ~ country,
                        data = gapminder, max)
countryle <- countryle[order(-countryle$lifeExp),]
countryle[1:10,] # 10 first
countryle[132:142,] # 10 last

# What if we want to know how much life expectancy, or GDP,
# or the population has changed (increased or decreased)
# in time for each country from 1957 to 2007?

# Let's start by subsetting the data.
# This is just a different way to subset data.
# Tip: If you don't have $, you need friends :[]. 
country1957 <- gapminder[gapminder$year == "1957", ]
country2007 <- gapminder[gapminder$year == "2007", ]

# Before merging data, let's rename variables so that we 
# know where they come from!
names(country1957) <- ifelse(names(country1957) == "country" |
                               names(country1957) == "continent",
                             names(country1957),
                             paste0(names(country1957), "1957"))

# Let's just drop year now that variable names have the year.
country1957 <- country1957[-3]
# Check that all looks okay:
country1957
# Do the same for 2007 data.
names(country2007) <- ifelse(names(country2007) == "country" |
                               names(country2007) == "continent",
                             names(country2007),
                             paste0(names(country2007), "2007"))
country2007 <- country2007[-3]
country2007

# Let's finally merge datasets, shall we use cbind(), rbind(), or merge()?
# There is no correct answer here, all could work!
# Merge by country and continent
country1957_2007 <- merge(country1957, country2007,
                          by = c("country", "continent"))
country1957_2007
# In case you were wondering, here cbind() works the same way,
# It just does not drop the country and year duplicated variables...
# You can try if you would like (just uncomment below).
# country1957_2007 <- cbind(country1957, country2007)

# Let's now subtract these columns to get the difference
country1957_2007$gdpPercap_dif <- country1957_2007$gdpPercap2007 - 
  country1957_2007$gdpPercap1957
country1957_2007$lifeExp_dif <- country1957_2007$lifeExp2007 -
  country1957_2007$lifeExp1957
country1957_2007$pop_dif <- country1957_2007$pop2007 - country1957_2007$pop1957
#View(country1957_2007)

# Which countries had the biggest increases
# in GDP, life expectancy, and population?

# Let's look at the top 10 (and bottom 10) again.

# GDP difference
country1957_2007 <- country1957_2007[order(-country1957_2007$gdpPercap_dif),]
country1957_2007[1:10, c(1,9)]
# Some countries got much richer!
country1957_2007[132:142, c(1,9)]
# While others actually got poorer ...

# Life expectancy
country1957_2007 <- country1957_2007[order(-country1957_2007$lifeExp_dif),]
# Re-ordering accordingly first.
country1957_2007[1:10, c(1,10)]
# Some countries were able to increase life expectancy a great deal!
country1957_2007[132:142, c(1,10)]
# Others not so much, but why?

# Population growth
country1957_2007 <- country1957_2007[order(-country1957_2007$pop_dif),]
country1957_2007[1:10, c(1,11)]
# Why such increase in population?
country1957_2007[132:142, c(1,11)]
# But what is happening here with these countries?

# What all of this tells you?
# Well, without a nice visual, not so much actually...

# Life expectancy, by continent

# How has life expectancy increased, by continent?

# Now, base R is great for handling data in R,
# but it is not so good for plotting... (as you can see above)
# Let's just try ploting life expectancy by continent with ggplot2!

# Remember to download and load the ggplot2 R package!
library(ggplot2)

ggplot(gapminder::gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_smooth(se=FALSE) + 
  labs(title = "Life expectancy by continent from 1957 to 2007",
       x = "Year",
       y = "Average Age") +
  theme_classic()

# We can still improve a lot by adding titles, labels, and themes!

# Now it is your turn:
# Using the codes above, could you plot GDP per capita per continent?
# Tip: you really only have to change one little thing(y) to make it work!
# What about plotting population growth per continent?

# How many dollars do you have per day?

# Let's bring all together here!
# The dslabs package contains a bigger sample of the GAPMINDER
# foundation data!
# Let's work with it here!
# Remember to download the package below!
# install.packages("gapminder")
# Notice, I do not work with base R here for the visuals.
gapminder_ext <- dslabs::gapminder

# Calculate dollars per day per capita for all observations (rows)
gapminder_ext$dollars_day <- gapminder_ext$gdp/gapminder_ext$population/365

# Remember to make sure ggplot2 is installed and loaded!

# Let's plot the evolution of dollars per day from 1960 to 2011
# (first and last years we have GDP data)!

# First, subset data for the years.
dollars_per_day_ev <- subset(gapminder_ext,
                             year == 1960 | year == 2011)

# Second, select only certain variables.
dollars_per_day_ev <- dollars_per_day_ev[c("country",
                                           "year",
                                           "dollars_day",
                                           "region")]

# Third, remove any rows containing NAs!
dollars_per_day_ev <- na.omit(dollars_per_day_ev)

# Fourth, get only pairs (duplicated) of obs. that is, 
# countries for which we have data  for both survey rounds (1960 and 2011).
dollars_per_day_ev <- dollars_per_day_ev[duplicated(dollars_per_day_ev$country) |
                                           duplicated(dollars_per_day_ev$country,
                                                      fromLast = TRUE),]

# Fifth, plot!
ggplot(dollars_per_day_ev, aes(dollars_day, reorder(country, dollars_day),
                               Fill = country)) +
  geom_point(aes(color = country)) +
  geom_line(aes(color = country)) +
  theme(legend.position="none") +
  labs(title = "Dollars per day from 1960 to 2011",
       y = "Country",
       x = "Dollars per day, per capita (in dollars)",
       caption = "Source: GAPMINDER")
# How could we make this plot better?

# Let's see the same plot, but by region:
dollars_region <- dollars_per_day_ev
dollars_region <- data.frame(aggregate(dollars_day ~ region + year,
                                       data = dollars_per_day_ev, mean))
ggplot(dollars_region, aes(dollars_day, reorder(region, dollars_day),
                           Fill = region)) +
  geom_point(aes(color = region)) +
  geom_line(aes(color = region)) +
  theme(legend.position="none") +
  labs(title = "Dollars per day from 1960 to 2011",
       y = "Region",
       x = "Average dollars per day, per capita (in dollars)",
       caption = "Source: GAPMINDER")
# Better! But what does this tell you?

# What if we want for countries in one continent only?
as.factor(dollars_per_day_ev$region)
dollars_americas <- subset(dollars_per_day_ev,
                           region == "South America" |
                             region == "Central America" |
                             region == "Northern America")
ggplot(dollars_americas,
       aes(dollars_day, reorder(country, dollars_day), Fill = country)) +
  geom_point(aes(color = country)) +
  geom_line(aes(color = country)) +
  theme(legend.position="none") +
  labs(title = "Dollars per day from 1960 to 2011 in the Americas",
       y = "Country",
       x = "Dollars per day, per capita (in dollars)",
       caption = "Source: GAPMINDER")

# Does this confirm the findings from above?

