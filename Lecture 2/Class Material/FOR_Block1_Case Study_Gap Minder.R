# Title: Fundamentals of R
# Purpose: Block 1 - Case Study: GAPMINDER
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: 30 September 2022

# Mind the GAP, with GAPMINDER
# Do you know them?
# https://www.gapminder.org/

# Investigate the DATA ---------------------------------------------------------

# Several other R packages have datasets included.
# One of these is gapminder, let's download, load, and see the data
# available in the package.
# install.packages("gapminder")
library(gapminder)
# But we are not sure what the package really is, are we?
# We can use the "?" here or, better, find the package manual!
?gapminder
# How to find the package manual though? Well, one can google it...
# All CRAN packages have a detailed manual.
# It may be enough to go through the function index in the help page.
# In any case: do you know the gapminder foundation?
# https://www.gapminder.org/
# Very cool data in any case, we invite you to take a look!
data("gapminder")
# Let's start by investigating the data!
summary(gapminder)
# Country and year data...
# Do you think all the countries are included for all survey years?
summary(as.factor(gapminder$country))
# Yes, all countries appear 12 times!

# Life expectancy, Americas vs. Europe -----------------------------------

# What if we want to start by comparing mean life expectancy in the
# Americas and Europe?
# This is 
# First, let's subset Americas.
AM <- subset(gapminder, continent == "Americas")
# Let's get the mean (average) and median life expectancy for the Americas.
meanAM = mean(AM$lifeExp)
# Here I use "=" instead of "<-" just to show that this also a possibility.
# In general, however, you should stick to "<-" not to confuse R.
medianAM = median(AM$lifeExp)
# Let's do the same for Europe!
EU <- subset(gapminder, continent == "Europe")
meanEU = mean(EU$lifeExp)
medianEU = median(EU$lifeExp)
# Let's subtract to find out their differences!
meanEU - meanAM
# Around 7 years of difference, in average!
medianEU - medianAM
# Around 5 years of difference... Why is it different?
# Let's bind create a data frame to facilitate visualization.
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
# in which life expectancy is much higher than others.
# This is specially the for the Americas, why do you think?

# Life expectancy and GDP, Americas vs. Europe ---------------------------------

# How about we plot the distributions of observations for
# Europe and for Americas in terms of life expectancy and GDP?
# First thing we need to do here, before ploting,
# is to reshape the data.
# Let's subset only the continents we are interested in.
subset_AMEU <- subset(gapminder, continent == "Americas"|continent == "Europe")
subset_AMEU
# Now let's aggregate life expectancy and GDP per capita, for
# each country in the respective continents subseted, by their mean.
# aggregate() is a pretty awesome function.
# The help file for it is very detailed as well,
# I suggest you read it!
lifeexp_gdp <- aggregate(cbind(lifeExp, gdpPercap) ~ country + continent,
                     data = subset_AMEU, mean)
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
# life expectancy in the Americas and Europe?
# Well, European countries have higher life expectancy, on average,
# and higher income, on average, than American countries.
# In any case, the distribution of countries in the Americas in very wide
# and left leaning.
# That is, countries with a wide variety of GDP per capita 
# levels and life expectancy, in comparison to European countries.
# But some European countries with relatively low GDP per capita
# (e.g. France and Spain) still have a very high life expectancy?
# Much higher than American countries with relatively high GDP per capita
# (e.g. Brazil and Mexico).
# Why is that? Is GDP per capita a good measure in your opinion?

# Life expectancy, GDP, and Population by country, in time --------------------------------------------------

# Which country has the highest life expectancy?
# Let's just see which countries actually have the highest
# maximum life expectancy.
countryle <-  aggregate(lifeExp ~ country,
                        data = gapminder, max)
countryle <- countryle[order(-countryle$lifeExp),]
countryle[1:10,] # 10 first
countryle[132:142,] # 10 last

# What if we want to know how much life expectancy, or GDP,
# and the population has changed (increased or decreased)
# in time for each country from 1957 to 2007?
country1957 <- gapminder[gapminder$year == "1957", ]
country2007 <- gapminder[gapminder$year == "2007", ]
# Let's merge datasets, shall we use cbind(), rbind(), or merge()?
# There is no correct answer here, all could work!
# Before merging, let's rename variables so that we know where they come from!
names(country1957) <- ifelse(names(country1957) == "country" |
                               names(country1957) == "continent",
                             names(country1957),
                             paste0(names(country1957), "1957"))
# Let's just drop year now that variable names have the year.
country1957 <- country1957[-3]
# Check that all looks okay:
country1957
names(country2007) <- ifelse(names(country2007) == "country" |
                               names(country2007) == "continent",
                             names(country2007),
                             paste0(names(country2007), "2007"))
country2007 <- country2007[-3]
country2007
# Merge by country and continent
country1957_2007 <- merge(country1957, country2007,
                          by = c("country", "continent"))
country1957_2007
# In case you were wondering, here cbind() works the same way,
# It just does not drop the country and year duplicated variables...
# You can try if you would like (just uncomment below).
# country1957_2007 <- cbind(country1957, country2007)
# Let's now subtract these columns to get the difference
country1957_2007$gdpPercap_dif <- country1957_2007$gdpPercap2007 - country1957_2007$gdpPercap1957
country1957_2007$lifeExp_dif <- country1957_2007$lifeExp2007 - country1957_2007$lifeExp1957
country1957_2007$pop_dif <- country1957_2007$pop2007 - country1957_2007$pop1957
View(country1957_2007)

# Let's see the countries had the biggest increases
# in GDP, life expectancy, and population.
# GDP difference
country1957_2007 <- country1957_2007[order(-country1957_2007$gdpPercap_dif),]
country1957_2007[1:10, c(1,9)] # Some countries got much richer!
country1957_2007[132:142, c(1,9)] # While others actually got poorer ...
# Life expectancy
country1957_2007 <- country1957_2007[order(-country1957_2007$lifeExp_dif),]
# Some countries were able to increase life expectancy a great deal!
country1957_2007[1:10, c(1,10)]
# Others not so much, but why?
country1957_2007[132:142, c(1,10)]
# How is this all affected by population growth?
country1957_2007 <- country1957_2007[order(-country1957_2007$pop_dif),]
# Why such increase in population? 
country1957_2007[1:10, c(1,11)]
# But what is happening here?
country1957_2007[132:142, c(1,11)]
# What does all this tells you?
# Well, without a nice visual, not so much actually...

# Life expectancy, by continent ------------------------------------------------

# How has life expectancy increased, by continent?
# Now, base R is great for handling data in R,
# but it is not so good for plotting... (as you can see)
# Let's just try ploting life expectancy by continent
# with ggplot2!
# Remember to doanload and load the ggplot2 R package!
library(ggplot2)
ggplot(gapminder::gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_smooth(se=FALSE) + 
  labs(title = "Life expectancy by continent from 1957 to 2007",
       x = "Year",
       y = "Average Age") +
  theme_classic()
# Without much effort we see that it is already better!
# We can still improve a lot by adding titles, labels, and themes!
# Now it is your turn:
# Using the codes above, could you plot GDP growth per continent?
# Tip: you really only have to change one little thing(y) to make it work.
# What about population growth?

# How many dollars do you have per day? ----------------------------------------

# Let's bring all together here!
# The dslabs package contains a bigger sample of the GAPMINDER
# foundation data! Let's work with it here!
# Remember to download the package below!
# install.packages("gapminder")
# Notice, I do not work with base R here for the visuals.
# Why do you think that is?
gapminder_ext <- dslabs::gapminder
# using the "::" we can access parts of the package instead of
# loading the whole package!
# Calculate dollars per day per capita for all observations (rows)
gapminder_ext$dollars_day <- gapminder_ext$gdp/gapminder_ext$population/365
# Make sure ggplot2 is installed and loaded!
# Let's plot the evolutio from 1960 to 2011
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

# Fourth, get only pairs (duplicated) of obs
# (countries for which we have data  for both survey rounds, 1960 and 2016)
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
# How can we make this plot better?

# Let's see for one continent only
dollars_per_day_ev_la <- subset(dollars_per_day_ev,
                                region == "South America" |
                                  region == "Central America" |
                                  region == "Northern America")
ggplot(dollars_per_day_ev_la,
       aes(dollars_day, reorder(country, dollars_day), Fill = country)) +
  geom_point(aes(color = country)) +
  geom_line(aes(color = country)) +
  theme(legend.position="none") +
  labs(title = "Dollars per day from 1960 to 2011 in the Americas",
       y = "Country",
       x = "Dollars per day, per capita (in dollars)",
       caption = "Source: GAPMINDER")
