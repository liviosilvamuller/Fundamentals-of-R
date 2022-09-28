# Title: Crime in the US in 1973 by Region
# Purpose: Block 1 Practical - Handling data in Base R
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022

# Important tips--------------------------------------------------------------------------------------------------------------------------------------------

# The ´´ "delimits" code.
# That is, commands you can/should run in your console.
# Please remember to run the code without ´´ though...
# The "::" specifies the package and, therefore, can be used to:
# 1- specify the package function without loading the whole package
# 2- browse through the package functions and data interactively
# For more info: ´?'::'´
# If at any time you cannot find help locally with "?" before
# package or function, please try with "??" before package name or function.
# "?" searches documentation, "??" search the help system.
# want to find more about the difference? Run: ´?'?'´ and ´?'??'´.

# Loading the Data-------------------------------------------------------------------------------------------------------------------------------------------

# Did you know that base R has several datasets included in it and ready to use?
data()

# Let's get one dataset:
data("USArrests")

# another way of doing this is calling the datasets package
# (from base so) no need to download anything!
# Yes, base R also has packages...
# Base packages are all loaded when you open R as some are very important!
# To see the base packages installed and automatically loaded run:
# ´installed.packages(priority="base")´

# Something loaded, but this format seems weird... no worries here.
View(USArrests)

# Inspecting the Data----------------------------------------------------------------------------------------------------------------------------------------

# What if we want a simple data summary?
summary(USArrests)

# What if we want to see a row, or a column in the data?
# We can use brackets []. 

USArrests[1] # for the first variable
USArrests[,1] # for values in the second column (1st column is rows names)
USArrests[1,1] # for value in the first row in the first column
USArrests[1,] # for values the first row but all columns
USArrests[50,] # for values the last row but all columns

# But what does all that data even mean?
?"USArrests"

# Ok, so the data is from 1973.
# And some variables in numeric form, per 100000, and others in percent...

# Complementing the Data------------------------------------------------------------------------------------------------------------------------------------

# How about we add this information in the dataset?

# We can use the "$" operator access variables in a dataframe.
USArrests$Murder

# Or, if we assign, we can use it to create or modify existing variables.

USArrests$date <- "1973" # adds an year variable
View(USArrests) # checkout the new variable called year and its values

class(USArrests$date) #check the class

#it's a character, what does it mean?
USArrests$date[1] + USArrests$date[2] 

# Can you guess why we cannot transform the date variable we have to date class?
# It expects an 8 digits date, we could complement dates arbitrarily ...
USArrests$date_arbitrary <- as.Date(paste0(USArrests$date, "-01-01"))

class(USArrests$date_arbitrary) # get's variable class



# Sub-setting the Data----------------------------------------------------------------------------------------------------------------------------------------

# How about we reorder the data by percent of urban population?
USArrests <- USArrests[order(USArrests$UrbanPop),]

# Say we would like to see only data for states with more than 75 percent
# urban population in 1973?
subset(USArrests, UrbanPop > 75)

# Let's use ´ifelse()´ to create a dummy variable for this!
USArrests$mostly_urban <- ifelse(USArrests$UrbanPop >= 75, 1, 0)

# remember the structure ifelse (CONDITION , VALUE IF TRUE, VALUE IF FALSE)
# if UrbanPop >= 75, 1
# if UrbanPop < 75, 0

# Can we summary our new variable as factor?
summary(as.factor(USArrests$mostly_urban))

# Can we add regions to the data?--------------------------------------------------------------------------------------------------------------------------

# Say we would like to add the regions for each state to the data.
# https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
# How can we do that?

# Well, we can use GREP!!!

USArrests$state <- rownames(USArrests) # rownames to a column
USArrests$region <- NA_character_ # crete a new variable with NAs

USArrests$region <- ifelse(grepl("Maine|New Hampshire|Vermont|Massachusetts|
|Rhode Island|Connecticut|New York|New Jersey|
                                 |Pennsylvania|Maryland|Delaware",
                                 USArrests$state), "Northeast", USArrests$region)

# The code above is nesting various functions, and assigning the results to the variable (column) region in USArrests
# Always remember the structure of ifelse!

# ifelse (CONDITION , VALUE IF TRUE, VALUE IF FALSE) in this case ifelse (grepl(...), "Northeast", USArrests$region)
# if grepl(...) = TRUE, then the value of USArrests$region= "Northeast"
# if grepl(...) = FALSE, then the value of USArrests$region = USArrests$region

# Okay, so what is grepl(...) doing
# grepl is checking if the value of the variable USArrests$state matches Maine or New Hampshire or Vermont....
# If it does, the value of the new region, is Northeast
# The following two chunk of code does the same for the South and the Midwest

USArrests$region <- ifelse(grepl("Virginia|West Virginia|Kentucky|
|Tennessee|North Carolina|South Carolina|Texas|Oklahoma|
|Georgia|Florida|Alabama|Mississippi|Louisiana|Arkansas",
                                 USArrests$state), "South", USArrests$region)

USArrests$region <- ifelse(grepl("Ohio|Indiana|Illinois|Michigan|Wisconsin|
                                 |Minnesota|Iowa|Missouri|North Dakota|
                                 |South Dakota|Nebraska|Kansas",
                                 USArrests$state), "Midwest", USArrests$region)

# Since we only have the four regions, and we already assigned values for three of them,
# whatever is NA, is the last region (WEST)
# the ifelse below checks the condition is.na(USArrests$region)

USArrests$region <- ifelse(is.na(USArrests$region), "West", USArrests$region)

summary(as.factor(USArrests$region)) 
# when you summarize something as factor, you are counting how many observation falls within each category
# you can read out the mode.


# How does crime averages differ by region?------------------------------------------------------------------------------------------------------------------

# FROM HERE ONWARDS, WE SHOWCASE A FEW OTHER BASE FUNCTIONS OF R
# NEVETHELESS, WE WILL NOT BE USING THEM IN THIS COURSE

# Let's check the average urban population by region.
aggregate(USArrests$UrbanPop, list(Region = USArrests$region), mean)

# Check out ´?aggregate()´, it is a cool function!!!

# Can we create a summary table of means for each of these crimes by region?
Murder <- aggregate(USArrests$Murder, list(Region = USArrests$region), mean)
Murder

# Let's rename the x variable
names(Murder)[names(Murder) == 'x'] <- 'Mean Murder (per 100000)'
Murder

# Do the same for assault and rape
Assault <- aggregate(USArrests$Assault, list(Region = USArrests$region), mean)
colnames(Assault)[2] <- "Mean Assault (per 100000)" # An easier way to rename
Assault
Rape <- aggregate(USArrests$Rape, list(Region = USArrests$region), mean)
colnames(Rape)[2] <- "Mean Rape (per 100000)" 
Rape

# Should we try a linear regression?????
# Say we are interested only in assaults,
# what would our independent variable and our dependent variable be?
# Assaults is our independent variable (we want to explain).
# Say we think our dependent variable should be the percent of urban population.
# We theorize that assault is widespread is as across states in the US,
# but ability to physically report assault is lower for non-urban populations.
# Why would that be the case in the US 1973?

# Let's also control by region.
model <- lm(Assault ~ UrbanPop + as.factor(region), data = USArrests)
summary(model)

# Could anyone tell me what this means?
# One other useful thing:
plot(model)
