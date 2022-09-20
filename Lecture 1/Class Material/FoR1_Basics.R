# This is a R script, one of various possible files that store and run code.
# In R scripts, whatever comes after a # is considered text, and not code. 
# We call this "commenting out".
# We use comment out text to explain what a line or chunk of code is doing.
# We like scripts to be organized neatly, so we understand them in the future.
# Therefore, we annotate things that are not common.
# We also start scripts with at least four lines:

# Title: Fundamentals of R (Basics)
# Purpose: to showcase core R tasks (setwd, install.packages, load packages, import data, view data) for students of MINT338.
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: September 2022


## Let's tell R where it should read and save files by default.----------------------------------------------------------------------------------------------------------

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Lecture 1/Class Material")
#(1) find your working directory on the files tab in the lower right pane; 
#(2) click on the "more file command", and click " Set as Working Directory"
#(3) copy the code line from console pane in the lower left pane, paste it on the script.

#alternatively, type setwd ("~/") and hit tab to find the folder.

#to run the code, click the lines and hit run on the top right corner of the top left pane.
#alternatively, place your cursor on the line you want to run, and hit command+enter.


## Now we need to install and load a few packages we intend to work with.----------------------------------------------------------------------------------------------------

# Packages contain various functions that perform specific tasks.

install.packages("readr") 
#(1) click on "Install", at the package tab in the lower right pane;
#(2) search for "readr" at prompt that appears.

#you can also just type install.packages("name of the package") on your console (1st tab of lower right pane).

#once packages are installed, they need to be loaded in every script you intend to use them:

library("readr")

#(1) find "readr" at the system library list at the package tab in the lower right pane.
#(2) check the box at the end of the line, see text on the console.
#or just type in library("nameofthepackage")


##  Let's learn how to import data.---------------------------------------------------------------------------------------------------------------------------------------

#Our data is in comma-separated values format (.csv)
#we will use a function from the "readr" package we just installed to import it:

io_dat <- read_csv(file="io_income_rs.csv", col_names = TRUE) 

#(1) click on the files tab in the lower right pane,
#(2) click on the dataset you want to import, and then click on import dataset
#(3) if you click import, the dataset will be imported. 
# but you can also copy the code, cancel the prompt, and paste in the script.

#let's dissect the code above, which is composed by three parts:

# (1) read_csv() is a function from the "readr" package that imports .csv files

# since we set our directory, you can just type in the name of the document you want as in the folder file="io_income_rs.csv"
# functions in r can take many arguments; arguments specify things about the function.
# for example, we can use read_csv(file="io_income_rs.csv", col_names = TRUE), to tell R that the first line of the csv file contain the columns' name
# the arguments here are (1) file="io_income_rs.csv", and (2) col_names = TRUE
# at the lower right pane, you can click on help and type read_csv to check all arguments
# alternatively, you can just use a question mark (?) in front of the code to open the help file
# you can also search for the package name, "readr", on the help tab
  

# (2) our object is io_dat, which contains the result of read_csv(file="io_income_rs.csv", col_names = TRUE)

# you can see all objects at the environment tab, at the top right pane.

# (3) the arrow (<-) is called "assign" operator
# it means assign the result of the operation on the right to the object on the left

3 + 3 # if you run this line, the result will appear at the console
Result <- 3+3 # if you run this line, the result will also be stored in the object Result
Lecturers <- c("Henrique", "Livio") 
Departments <- c("IRPS", "ANSO")
# run both lines (81 and 82) and check the environment

Profile <- data.frame(Lecturers, Departments)
# here, we are using two objects to create a dataframe

## Let's view our objects and data!--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

View(io_dat) # you can either click on it in the environment tab, or run the code View("name of the object")
View(profile) # R is CaSe SeNsItIvE -> profile is different from Profile
View (Profile)

#You can also type io_dat in the console and see a preview.

# Before we move on, let's clean up R studio!
# First, at the environment tab, mark all objects you wish do delete and click the broom.
# Next, click on the console and hit control+L
  
## Okay, so what can we do with R? ##
