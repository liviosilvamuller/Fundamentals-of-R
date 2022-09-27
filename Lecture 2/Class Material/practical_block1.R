# Title: Fundamentals of R (Block 1 - Handling data in Base R)
# Purpose: Practical exercises with data frames using base R.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

###### Important tips: #########################################################
###### The ´´ "delimits" code.
###### That is, commands you can/should run in your console.
###### Please remember to run the code without ´´ though...
######
###### The "::" specifies the package and, therefore, can be used to:
###### 1- specify the package function without loading the whole package
###### 2- browse through the package functions and data interactively
###### For more info: ´?'::'´
######
###### If at any time you cannot find help locally with "?" before
###### package or function, please try with "??" before package name or function.
###### "?" searches documentation, "??" search the help system.
###### want to find more about the difference? Run: ´?'?'´ and ´?'??'´.

# Did you know that base R has several datasets included in it and ready to use?
data()
# Let's get one dataset:
data("USArrests")
# another way of doing this is calling the datasets package
# (from base so) no need to download anything!
# Yes, base R also has packages...
# They are all loaded when you open R as some are very important!
# To see the base packages installed and automatically loaded run:
# ´installed.packages(priority="base")´

# Something loaded, but this format seems weird... no worries here.
View(USArrests)
# What if we want a simple data summary?
summary(USArrests)
# What if we want to see a row, or a column in the data?
# We can use brackets []. 
USArrests[1] # for the first variable
USArrests[,1] # for values in the second column
USArrests[1,1] # for value in the first row in the first column
USArrests[1,] # for values the first row but all columns
USArrests[50,] # for values the last row but all columns

# But what does all that data even mean?
?"USArrests"
# Ok, so the data is from 1973.
# How about we add this information in the dataset?
# We can use the "$" operator access variables in a dataframe.
USArrests$Murder
# Or, if we assign, we can use it to create or modify existing variables.
USArrests$date <- "1973" # adds an year variable
summary(USArrests)
# What if we want our dates to be numeric or, even better, date class?
USArrests$date <- as.numeric(USArrests$date)
class(USArrests$date) # numeric class, what are the implications?
USArrests$date[1] + USArrests$date[2] # We could add first date to second date...
# But should we really add dates this way? Better to tell R these are dates!
USArrests$date <- as.Date(USArrests$date) # ERROR
# Can you guess why we cannot transform the date variable we have to Date class?
# It expects an 8 digits date, we could completing dates arbitraryly ...
USArrests$date_arbitrary <- as.Date(paste0(USArrests$date, "-01-01"))
class(USArrests$date_arbitrary) # get's variable class

# what else can we do?
?"USArrests"
# Some variables in numeric form, per 100000, and others 

# Subset data

# Filter data (GREP)




