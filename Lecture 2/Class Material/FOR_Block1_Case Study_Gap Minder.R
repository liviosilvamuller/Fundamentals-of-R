# Title: Fundamentals of R
# Purpose: Block 1 - Case Study: GAPMINDER
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

# Why some countries have a higher life expectancy than others? 
# And how has this changed in time across countries?

# Let's work with some cool R data.
# Several other R packages have datasets included.
# One of these is gapminder, let's download, load, and see the data
# available in the package.
install.packages("gapminder")
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

# Let's start by investigating the data
summary(gapminder)
# What if we want to start by comparing mean life expectancy in the
# Americas and Europe?
AM <- subset(gapminder, continent == "Americas")
meanAM = mean(AM$lifeExp)
EU <- subset(gapminder, continent == "Europe")
meanEU = mean(EU$lifeExp)
meanEU - meanAM
# 7 years of difference!

# Let's get serious here:
# Which country has the highest life expectancy (on average)?
countryle <- aggregate(gapminder$lifeExp, list(Country = gapminder$country), mean)
names(countryle)[2] <- "Life Expectancy"
countryle <- countryle[order(-countryle$`Life Expectancy`),]
countryle[1:10,] # 10 first
countryle[132:142,] # 10 last

# What if we want to know how much life expectancy has changed
# (increased or decreased by country
countrylemax <- aggregate(gapminder$lifeExp, list(Country = gapminder$country), max)
names(countrylemax)[2] <- "Max Life Expectancy"
countrylemin <- aggregate(gapminder$lifeExp, list(Country = gapminder$country), min)
names(countrylemin)[2] <- "Min Life Expectancy"
countryre <- merge(countrylemax, countrylemin, by = "Country")
countryre$range <- countryre$`Max Life Expectancy` - countryre$`Min Life Expectancy`
countryre <- countryre[order(-countryre$range),]
countryre[1:10,] # 10 first
countryre[132:142,] # 10 last
# Simple base R plots
# Let's plot first 10 and last 10
barplot(height=countryre$range[1:10],
        col="green",
        names.arg=as.factor(countryre$Country)[1:10], 
        las=2,
        main = "Life expectancy increase from 1952 to 2007",
        xlab = "Country",
        ylab = "Range (in years)")
barplot(height=countryre$range[132:142],
        col="red",
        names.arg=as.factor(countryre$Country)[132:142], 
        las=2,
        main = "Life expectamcy increase from 1952 to 2007",
        xlab = "Country",
        ylab = "Range (in years)")

