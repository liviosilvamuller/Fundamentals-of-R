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
