# Alexandra Wenzel, Homework 2, October 8, Fundamentals of R
library(dplyr)
library(tidyr)
#Q1 
spring_22 <- read_csv("spring_22.csv")
spring_22 <- pivot_longer(spring_22, 
                              cols = ANSO:RISP) %>%
  rename(Department = name,
         course = class) %>%
  drop_na(value) %>%
  select(-value)

# in class did it rather as name_to : Department within pivot_longer

#Q2
autumn_21 <- read_csv("autumn_21.csv")
autumn_21 <- separate(autumn_21, code, sep="-", into=c("Department", "coursecode")) 

#Q3
academic_year <- full_join(autumn_21, spring_22) 

#Q4 
academic_year <- distinct(academic_year) %>%
  select(-...1)
# While this deletes the duplicated rows, some courses are listed twice as they are in multiple departments.

#Q5 
count(academic_year, language, french = TRUE)
# 11 classes 
##  in class did filter(language == "french")

#Q6 
autumn_21 %>% group_by(language) %>% count()
# 145 courses. 

#Q7 rank the departments by the number of courses offered in each semester. 
academic_year %>% 
  group_by(Department, semester) %>%
  count(Department) %>%
  arrange(-n)

#Q8 
academic_year %>%
  filter(semester == "Spring") %>%
  group_by(Department) %>%
  count() %>%
  summarise(share = sum(n)/124) %>%
  arrange(-share)

#Q9
academic_year %>%
  group_by(topic) %>%
  count() %>%
  arrange(-n)


#Q10
academic_year %>%
  group_by(Department) %>%
  count(topic) %>%
  arrange(-n)  %>%
  slice(1:3) %>%
  print(n = 27)

#Q11
academic_year <- mutate(academic_year, topic_comp = ifelse(type == "workshop", "skills", topic))
  
## from office hours: topic = case_when could also work

#Q12
academic_year %>% filter(type == "compulsory") %>%
  group_by(Department, topic) %>%
  count(topic) %>%
 slice(1:1)  

#Q13
academic_year <- mutate(academic_year, comp_type = ifelse(type == "compulsory"  & topic == "theory" | topic == "methods", 1, 0))

#Q14
ects <- academic_year %>% 
  group_by(Department) %>%
  summarise(ects_dep = sum(ects)) 
View(ects)

cc <- left_join(ects, faculty_n, by = c("Department" = "department"))
View(cc)
cc <- mutate(cc, share_ects = ects_dep/faculty_n)


