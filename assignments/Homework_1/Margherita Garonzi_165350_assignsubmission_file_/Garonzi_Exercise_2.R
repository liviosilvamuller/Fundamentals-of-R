library(readxl)
turnout <- read_excel("turnout.xls")
View(turnout) 
# 2.1. Load the data into R and check the dimensions of the data. Also, obtain a summary of the data. How many observations are there? What is the range of years covered in this data set?
dim(turnout)
summary(turnout)
range(turnout$year)
# 2.2. Calculate the real turnout rate based on the voting age population (VAP) by year. This is, how many voters actually voted (VAP/TOTAL) in each year. Note that for this data set, we must add the total number of eligible overseas voters since the VAP variable does not include these individuals in the count. Next, calculate the real turnout rate using the voting eligible population (VEP). This is VEP/TOTAL. What difference do you observe?
VAP <- turnout$VAP
VEP <- turnout$VEP
overseas <- turnout$overseas
total <- turnout$total
VAP_turnout <-total*100/VAP
VAP_turnout[]
VEP_complete_turnout <-total*100/(VEP+overseas)
VEP_complete_turnout[]
# Comment: Turnout is calculated as a percentage for consistency with ANES results. The turnout is slightly higher when VEP_complete instead of VAP is considered, as the number of actual potential voters is lower than the voting-age population. It is thus also a more accurate representation.  
# 2.3. Compute the differences between the self-estimated turnout rate (ANES) and the real turnout rate you just calculated in 2.2. How big is the difference on average? Conduct the same comparison for the VEP and ANES estimates of voter turnout. Briefly comment on the results.
ANES_turnout <- turnout$ANES
mean(ANES_turnout-VAP_turnout)
mean(ANES_turnout-VEP_complete_turnout)
# Comment:As VAP_turnout is lower than VEP_complete_turnout, the discrepancy between the ANES expected turnout and VAP_turnout is greater than with VEP_complete_turnout. The average discrepancy between either of the real turnouts and the expected ones remains however very high, showing a low accuracy for self-reports.
# 2.4. Compare the VEP turnout rate with the ANES turnout rate separately for presidential elections and midterm elections. Note that the data set excludes the year 2006. Does the bias of the ANES estimates vary across election types?
election_type <- turnout$election_type
dataset_election_type <- data.frame(cbind(ANES_turnout,VEP_complete_turnout,election_type))
ANES_turnout_presidential <- subset(dataset_election_type$ANES_turnout, election_type == "presidential")
ANES_turnout_presidential[]
ANES_turnout_midterm <- subset(dataset_election_type$ANES_turnout, election_type == "midterm")
ANES_turnout_midterm[]
VEP_turnout_presidential <- subset(dataset_election_type$VEP_complete_turnout, election_type == "presidential")
VEP_turnout_presidential[]
VEP_turnout_midterm <- subset(dataset_election_type$VEP_complete_turnout, election_type == "midterm")
VEP_turnout_midterm[]
mean(as.numeric(ANES_turnout_presidential)-as.numeric(VEP_turnout_presidential))
mean(as.numeric(ANES_turnout_midterm)-as.numeric(VEP_turnout_midterm))
# Comment: The above solution is not elegant, but manages to find the results. The discrepancy between the ANES expected turnout and the real turnout is greater for presidential elections than midterms, showing a potential higher accuracy of the surveys for the latter type of elections. However, the midterms ave also a lower number of observations, potentially influencing the result.
# 2.5. Divide the data into half by election years such that you subset the data into two periods: older and newer elections. Calculate the difference between the VEP turnout rate and the ANES turnout rate separately for each year within each period. Has the bias of ANES increased over time?
election_year <- turnout$year
dataset_year <- data.frame(cbind(VEP_complete_turnout, ANES_turnout, election_year))
dataset_older_elections <-subset(dataset_year, election_year<1994)
dataset_older_elections []
dataset_newer_elections <- subset(dataset_year, election_year>=1994)
dataset_newer_elections[]
dataset_older_elections$ANES_turnout-dataset_older_elections$VEP_complete_turnout
dataset_newer_elections$ANES_turnout-dataset_newer_elections$VEP_complete_turnout
# Comment: The bias of ANES seems to have neither increased or decreased over time, a connection between time and accuracy is not apparent.   
