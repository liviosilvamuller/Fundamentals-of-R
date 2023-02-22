#HOMEWORK 2
# 11.10.2022

#Question 1
library(readr)
library(tidyr)
library(dplyr)
library(readr)
spring_22 <- read_csv("Desktop/MINT 338/Material for week 3/spring_22.csv")

spring_22n <- spring_22 %>% 
  pivot_longer(ANSO:RISP, names_to = "Department") %>%
  drop_na(value) %>%
  select(-value)
#created the department column

#QUESTION 2
autumn_21 <- read_csv("Desktop/MINT 338/Material for week 3/autumn_21.csv")
autumn_21 <- autumn_21 %>% separate (code, c("Department", "code"),
                       sep = "-",) #separating to create the Department column


#QUESTION 3

spring_22 <- spring_22 %>% rename(course = class)

academic_year <- full_join(spring_22, autumn_21) %>%
  distinct() #making sure that we have only distinct classes (no duplicates)

#QUESTION 4

#no, already removed through the use of distinct in question #3

#QUESTION 5

academic_year %>% group_by(language) %>% count()
#in the academic year there were 11 courses in French

#QUESTION 6

autumn_21 %>% group_by(language) %>% count()
#145 courses were offered in english during the autumn 2021 semester

#QUESTION 7

academic_year %>% 
  group_by(Department, semester) %>% 
  count() %>%
  arrange(-n)
# as shown below, during both semesters the department that offers the most courses is the MINT department

# 1 MINT       Autumn      61
#2 MINT       Spring      42
#3 EI         Autumn      19
#4 ANSO       Spring      17
#5 ANSO       Autumn      16
#6 EI         Spring      16
#7 DI         Autumn      15
#8 RISP       Autumn      15
#9 DI         Spring      14
#10 RISP       Spring      13
#11 DE         Autumn       9
#12 IA         Spring       8
#13 HI         Autumn       7
#14 HI         Spring       6
#15 HPI        Autumn       5
#16 IA         Autumn       5
#17 DE         Spring       4
#18 HPI        Spring       4

#QUESTION 8

academic_year %>% 
  filter( semester == "Spring") %>%
  group_by(Department) %>%
  count() %>%
  summarise(share = sum(n)/124) %>%
  arrange(-share)
# the department which offers the highest share of courses in the spring semester is MINT with 33.9% of all the courses offered


#QUESTION 9

academic_year %>% 
  group_by(topic) %>%
  count() %>%
  arrange(-n)

#the three favorite topics over all are other, theory and NA (no topic specified- possibly from workshops)

#QUESTION 10

library(dplyr)

academic_year %>% 
  group_by(Department, topic) %>%
  count(topic) %>%
  arrange(-n) %>%
  slice(1:3) %>%
  print(n=27)
#the favorite topics are as follows:
    #ANSO :other, migration, inequality
    #DE: other, conflict, theory
    #DI: other, theory, trade
    #EI: other, trade, finance
    #HI: other, theory, finance/inequality (both 1)
    #HPI: other, all the remaining have 1 each
    #IA:other, finance, governance
    #MINT: NA, other, conflict, human rights, migration
    #RISP: other, governance, theory

#QUESTION 11:

academic_year <- mutate( academic_year, topic_comp = ifelse(type == "workshop", "skills",topic))


#QUESTION 12

academic_year %>% 
  filter(type == "compulsory") %>%
  group_by(Department, topic) %>%
  count(topic)
# the favorite topics for compulsory courses are:
    #ANSO: theory
    #DI: other
    #EI: methods
    #HI: theory
    #MINT: methods
    #RISP: other

#QUESTION 13

academic_year %>%
  mutate(comp_type = ifelse(type == "compulsory" & topic == "methods"|topic == "theory",1, 0))

#QUESTION 14

faculty_n <- read_excel("Desktop/MINT 338/Material for week 3/faculty_n.xlsx")

ects <- academic_year %>% 
  group_by(Department) %>%
  summarise(ECTS_dep = sum(ects))
# this allows me to know how many ects overall credits there are per each department 
#time to join the imported faculty_n and ects 

cc <- left_join (faculty_n, ects, by = c("department" = "Department"))

# the following will allow  to know the ects/prof in each department

cc <- mutate(cc, share_ects = ECTS_dep/faculty_n)
#MINT prof and faculty are the most overworked with the avg of 10.44 of ects per lecturer
