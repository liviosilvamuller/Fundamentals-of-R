#Setting Working Directory
setwd("~/Desktop/IHEID/Fall 2022/R/Material for week 3-20221007")

#Question 1
#Opening the dataset
library(readr)
library(dplyr)
library(tidyr)
spring_22 <- read_csv("spring_22.csv")
View(spring_22)
spring_22 <- spring_22 %>%
  pivot_longer(ANSO:RISP, names_to = "Department") %>%
  drop_na(value) %>%
  select(-value)

#Question 2
#Opening the dataset
library(readr)
autumn_21 <- read_csv("autumn_21.csv")
View(autumn_21)
autumn_21 <- separate(autumn_21, code, sep = "-", into=c("Department", "Code"))

#Question 3
spring_22 <- spring_22 %>% rename(title_course = class)
autumn_21 <- autumn_21 %>% rename(title_course = course)
academic_year <- full_join(autumn_21,spring_22)
View(academic_year)

#Question 4
academic_year %>% distinct()

#Question 5 
nrow(academic_year[academic_year$language == "french", ])
academic_year %>% 
  filter(language == "french") %>% 
  count(language, name = "Number")

#Question 6 
academic_year %>% 
  filter(semester == "Autumn" & language == "english") %>% 
  count(semester, language, name = "n")

#Question 7 
academic_year %>%
  group_by(semester, Department) %>% 
  count() %>%
  arrange (-n)

#Question 8 
academic_year %>%
  filter(semester == "Spring") %>%
  group_by(Department) %>%
  count() %>%
  summarise(Share = sum(n)/124)%>%
  arrange(-Share)

#Question 9
academic_year %>%
  group_by(topic) %>%
  count() %>%
  arrange(-n) %>%
  slice_head(n = 3)

#Question 10 
library(tidyr)
academic_year %>%
  group_by(Department) %>%
  count(topic) %>%
  arrange(-n) %>%
  slice(1:3) %>%
  print (n = 27)
  
#Question 11
academic_year <- academic_year %>%
  mutate(topic = case_when(type == "workshop" ~ "skills", TRUE ~ as.character(topic)))

#Question 12 
library(tidyr)
academic_year %>%
  filter(type == "compulsory") %>%
  group_by(Department) %>%
  count(topic) %>%
  slice(1:1)

#Question 13
academic_year <- mutate(academic_year, comp_type = ifelse(type == "compulsory"  & topic == "theory" | topic == "methods", 1, 0))


#Question 14
#Importing the dataset
library(readxl)
faculty_n <- read_excel("faculty_n.xlsx")

ects <- academic_year %>% 
  group_by(Department) %>%
  summarise(ects_dep = sum(ects)) 
View(ects)

cc <- left_join(ects, faculty_n, by = c("Department" = "department"))
View(cc)
cc <- mutate(cc, share_ects = ects_dep/faculty_n)



