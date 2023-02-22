#Question 1
#Open the spring_22.csv data and create a variable called “Department” with the department acronym for
#each course. (Tip: you have all the information necessary in the various dummy variables for department,
  #            you just need to pivot the data and remove the NAs.)

library(readr)
spring_22 <- read_csv("spring_22.csv")
View(spring_22)
library(dplyr)
library(tidyr)
tidy_spring_22 <-pivot_longer(spring_22,cols=c("ANSO","DE","DI","EI","HI","HPI","IA","MINT","RISP"), names_to="Department", values_to = "Value")%>% 
  drop_na(Value)%>%
  select(-Value,-...1)%>%
  rename(course=class)
view(tidy_spring_22)
#Question 2
#Open the autumn_21.csv data and create a variable called “Department” with the department acronym for
#each course. (Tip: here you will have to separate the acronyms for departments from the course code)
library(readr)
autumn_21 <- read_csv("autumn_21.csv")
View(autumn_21)
tidy_autumn_21 <- separate(autumn_21,code, sep="-", into=c("Department", "Number"))%>%
  select(-Number)
view(tidy_autumn_21)
#Question 3
#Join the two datasets into one dataset called “academic_year”. (Tip: remember to rename variables consistently
 #                                                               across before joining data for best results).
# I renamed the variable under Question 1 for rapidity
#+ 
#Question 4
#Do you have any duplicated rows in the “academic_year” dataset? If so, remove them.
academic_year <- full_join(tidy_autumn_21,tidy_spring_22)%>%
distinct()
view(academic_year)
#Question 5
#In the academic year, how many courses were offered in French at IHEID?
 academic_year%>%
   filter(language=="french")%>%
   count()
#Question 6
#In the academic year, how many courses were offered in the autumn semester and in English at IHEID?
 academic_year%>%
 filter(language=="french",semester=="Autumn")%>%
   count()
#Question 7
#Rank the departments by the number of courses offered in each semester.
academic_year%>%
  filter(semester=="Spring")%>%
  count(Department,semester, sort=TRUE)
academic_year%>%
   filter(semester=="Autumn")%>%
  count(Department,semester, sort=TRUE)
#Question 8
#Which department offers a higher share of courses in the spring semester? (Tip: after filtering and grouping,
  #you need to divide the number of courses in each department by the total number of courses in the spring semester).
academic_year%>%
  filter(semester=="Spring")%>%
  group_by(Department)%>%
  summarise(Percentage=n()/nrow(.)*100)
#Question 9
#List the three favorite topics overall.
academic_year%>%
  count(topic)%>%
  slice_head(n = 3)
#Question 10
#List the three favorite topics of each department. (Tip: group and slice).
academic_year%>%
  group_by(Department)%>%
  count(topic,sort=TRUE)%>%
  drop_na()%>%
  slice_head(n=3)%>%
  print(n = 27)
#Question 11
#One of the categories of type is “workshop”. Workshops are normally about skills, but in the dataset,
#workshops miss values for topic. Assign the category “skills” at the topic variable for all workshops.
academic_year%>%
  filter(type=="workshop")%>%
  mutate(topic = coalesce(topic,"skills"))
#Question 12
#What is are the favorite topics for compulsory courses in all departments?
  academic_year%>%
    filter(type=="compulsory")%>%
    group_by(Department)%>%
    count(topic,sort=TRUE)%>%
    drop_na()%>%
    slice_head(n=3)%>%
    print()
 #Sliced at 3 to show a restricted number of favorite topics
#Question 13
#Create a new dummy variable called “comp_type”. The variable should take the value of 1 if a course is
#compulsory and about theory or methods; or take the value of 0 if a course is not compulsory or is compulsory
#but not about theory or methods.
  academic_year%>%
    mutate(comp_type = case_when((type=="compulsory"&(topic=="theory"|topic=="methods")) ~ 1,
                                  (type!="compulsory"|type=="compulsory"&topic!="theory"|topic!="method") ~ 0))%>%
    View()
#Question 14
#The faculty_n.xlsx dataset contains the number of faculty per department. In which department, faculty
#teaches more ECTs on average? Notice that faculty number for departments DE and IA are missing. (Tip: divide the total ects per department by the number of faculty).
  library(readxl)
  faculty_n <- read_excel("faculty_n.xlsx")
  View(faculty_n)
  academic_year%>%
    group_by(Department)%>%
    count(Department, wt=ects)
    summarise(n/(faculty_n))
    
academic_year_smaller <-  academic_year%>%
    select(Department,ects)%>%
    group_by(Department)%>%
    count(Department, wt=ects)%>%
    filter(Department!="DE"& Department!="IA")%>%
    rename(department=Department)
View(academic_year_smaller)

new_dataset <- left_join(academic_year_smaller, faculty_n)
View(new_dataset)

new_dataset%>%
  summarise(n/faculty_n)

