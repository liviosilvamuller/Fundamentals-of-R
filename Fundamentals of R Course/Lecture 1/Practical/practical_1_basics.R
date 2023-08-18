# This is a R script, one of various possible files that store and run code.
# In R scripts, whatever comes after a # is considered text, and not code. 
# We call this "commenting out".
# We use comment out text to explain what a line or chunk of code is doing.
# We like scripts to be organized neatly, so we understand them in the future.
# Therefore, we annotate things that are not common.
# We also start scripts with at least four lines:

# Title: Fundamentals of R (Basics)
# Purpose: to showcase core R tasks in detail for students.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: 31-12-9999

## Let's tell R where it should read and save files by default.----------------------------------------------------------------------------------------------------------

#(1) find your working directory on the files tab in the lower right pane; 
#(2) click on the "more file command", and click " Set as Working Directory"
#(3) copy the code line from console pane in the lower left pane, paste it on the script.

# Alternatively, you can type setwd ("~/") and hit tab to find the folder.

# To run the code from scripts, click the lines and hit run on the top right
# corner of the top left pane.
# Alternatively, you can place your cursor on the line you want to run,
# and hit command+enter.

## Now we need to install and load a few packages we intend to work with.----------------------------------------------------------------------------------------------------

# Packages contain various functions that perform specific tasks.

install.packages("readr")
#(1) click on "Install", at the package tab in the lower right pane;
#(2) search for "readr" at prompt that appears.

# You can also just type install.packages("name of the package") on your console (1st tab of lower right pane).

# Once packages are installed, they need to be loaded in every script you intend to use them:

library("readr")

#(1) find "readr" at the system library list at the package tab in the lower right pane.
#(2) check the box at the end of the line, see text on the console.
#or just type in library("nameofthepackage")


##  Let's learn how to import data.---------------------------------------------------------------------------------------------------------------------------------------

# Download the "io_income_rs.csv" from repo and store it in your working directory.

# Our data is in comma-separated values format (.csv)
# we will use a function from the "readr" package we just installed to import it:

io_dat <- read_csv(file="io_income_rs.csv", col_names = TRUE) 

# If this does not work, you can also manually download the data by: 
#(1) click on the files tab in the lower right pane,
#(2) click on the dataset you want to import, and then click on import dataset
#(3) if you click import, the dataset will be imported. 
# but you can also copy the code, cancel the prompt, and paste in the script.

# Let's dissect the code above, which is composed by three parts:

# (1) read_csv() is a function from the "readr" package that imports .csv files

# Since we set our directory, you can just type in the name of the document
# you want as in the folder file="io_income_rs.csv".
# Functions in r can take many arguments; arguments specify things about the function.
# For example, we can use read_csv(file="io_income_rs.csv", col_names = TRUE),
# to tell R that the first line of the csv file contain the columns' name.
# The arguments here are (1) file="io_income_rs.csv", and (2) col_names = TRUE
# at the lower right pane, you can click on help and type read_csv to
# check all arguments.
# Alternatively, you can just use a question mark (?) in front of the code to
# open the help file you can also search for the package name, "readr", on the help tab.
  
# (2) Our object is io_dat, which contains the result of
# read_csv(file="io_income_rs.csv", col_names = TRUE)
# You can see all objects at the environment tab, at the top right pane.

# (3) The arrow (<-) is called "assign" operator.
# It means assign the result of the operation on the right to the object on the left.

3 + 3 # if you run this line, the result will appear at the console
Result <- 3+3 # if you run this line, the result will also be stored in the environment
Names <- c("Henrique", "Livio")
Departments <- c("IRPS", "ANSO")

# Use these objects to create a data frame:
Lecturers <- data.frame(Lecturers, Departments)

## Let's view our objects and data!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

View(io_dat) # You can either click on it in the environment tab,
# or run the code View("name of the object")
View(lecturers) # R is CaSe SeNsItIvE -> lecturers is different from Lecturers
View (Lecturers)

# You can also type io_dat in the console and see a preview.
