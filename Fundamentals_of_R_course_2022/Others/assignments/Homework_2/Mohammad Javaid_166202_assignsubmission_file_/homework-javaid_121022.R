
# Title: Assignment for Fundamentals of R 
# Purpose: Courses at the Graduate Institute
# Author: Mohammad Javaid
# Date: 12 October 2022

library(readr)
spring_22 <- read_csv("Desktop/R/Material for week 3-20221007/spring_22.csv")
View(spring_22)
library(tidyr)

library(dplyr)

#1 creating a variable Department using the department acronym for each course from spring file
spring_22 <- spring_22 %>% 
  pivot_longer(ANSO:RISP, names_to = "Department") %>% 
  drop_na(value)%>% 
  select(-value) %>%
  select(-'...1')

#2 creating a variable Department using the department acronym for each course from autumn file
autumn_21 <- autumn_21 %>% 
  separate(code, c("Department", "code"), sep = "-")


#3 creating a new data set academic_year
#renaming course = class

spring_22 <- spring_22 %>% rename (course = class)

academic_year <- full_join(spring_22, autumn_21)


#4 removing duplicated rows
academic_year <- distinct(academic_year)


#5 finding the number of courses offered in french
academic_year %>% group_by(language) %>% count()
academic_year %>% filter(language == "french")


#6 number of courses offered in english in autumn semester
academic_year  %>% group_by(language, semester)  %>% count()


#7 Ranking department by the number of courses offered by departments
academic_year  %>% 
  group_by(semester, Department)  %>% 
  count()  %>% 
  arrange(n)


#8 finding department that offers a higher share of courses in the spring semester
academic_year  %>%  
  filter(semester == "Spring")  %>%
  group_by(Department)  %>%
  count()  %>%
  summarise(share = sum(n)/124)  %>%
  arrange(-share)


#9 Listing the three favorite topics
academic_year  %>% 
  group_by(topic) %>%
  count() %>%
  arrange(-n) %>%
  rename(category = n)


#10 Listing the three favorite topics of each department
academic_year %>%
  group_by(Department, topic) %>%
  count(topic)  %>%
  arrange(-n) %>% 
  slice(1:3) %>%
  print(n = 27)


#11 Assigning the category “skills” at the topic variable for all workshops
academic_year <- mutate(academic_year,
                        topic_comp = ifelse(type == "workshop",  "skills", topic))


#12 finding the favorite topics for compulsory courses in all departments

academic_year %>% filter(type == "compulsory") %>%
  group_by(Department) %>%
  count(topic) %>%
  slice(1:1)


#13 Creating a new dummy variable “comp_type.” 
# taking the value of 1 if a course is compulsory and about theory or methods; 
# and taking the value of 0 if a course is not compulsory 
#or is compulsory but not about theory or methods.

academic_year <- academic_year %>% mutate(comp_type= ifelse(type == "compulsory" &
                                                              topic == "methods" |
                                                              topic == "theory", 1, 0))

#14
#loading faculty_n file
#getting all Ects

ects <- academic_year %>% group_by(Department) %>%
  summarise(ECTS_dep = sum(ects)) 

#joining both
cc <- left_join(faculty_n, ects, by = c("department" = "Department"))

cc <- mutate(cc, share_ects = ECTS_dep/faculty_n)







