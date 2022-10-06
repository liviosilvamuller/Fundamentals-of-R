library(dplyr)
library(tidyr)
autumn_21 <- read_csv("Lecture 3/Homework/autumn_21.csv")
spring_22 <- read_csv("Lecture 3/Homework/spring_22.csv")

# 1

spring_22 <- spring_22 %>% 
  select(-'...1') %>% 
  pivot_longer(ANSO:RISP, names_to = "department", values_to = "Count") %>%
  filter(!is.na(Count)) %>% 
  select(-Count)

# 2

autumn_21 <- autumn_21 %>%
  separate(code, c("department", "code"), sep = "-")

# 3

spring_22 <- rename(spring_22, course  = class)
academic_year <- full_join(autumn_21, spring_22) %>% 
  select(-code) %>% 
  rename(course_title = course)

# 4

academic_year %>%
  group_by(course_title) %>% 
  count() %>% 
  filter(n > 1)

academic_year <- dplyr::distinct(academic_year)

# 5 

academic_year %>% filter(language == "french") # 11 courses in french

# 6

academic_year %>%
  select(course_title, semester, language) %>% 
  filter(semester == "Autumn" & language == "english")

# 7

academic_year %>% 
  group_by(department, semester) %>% 
  count() %>% 
  arrange(-n) %>% 
  rename("Number of courses" = n)

# 8

# spring has 124 courses
academic_year %>% 
  filter(semester == "Spring") %>% 
  group_by(department) %>% 
  count() %>% 
  mutate(share = (n/124)*100) %>% 
  arrange(-share) %>% 
  rename("Courses in spring semester" = n)

# 9

academic_year %>% 
  group_by(topic) %>% 
  count() %>% 
  arrange(-n)

# 10

academic_year %>% 
  group_by(department) %>%
  count(topic) %>% 
  arrange(-n) %>% 
  slice(1:3)

# 11

academic_year %>% 
  filter(type == "workshop")

academic_year <- academic_year %>% 
  mutate(topic = ifelse(type == "workshop", "skills", topic))

# 12

academic_year %>% 
  filter(type == "compulsory") %>% 
  group_by(topic) %>% 
  count() %>% 
  arrange(-n)

# 13

academic_year <- academic_year %>% 
  mutate(comp_type = ifelse(type == "compulsory" & topic == "theory" | 
                              topic == "methods", 1, 0))

# 14

t_ects <- academic_year %>% 
  group_by(department) %>%
  summarise(total_ects = sum(ects))

ects_per_faculty <- dplyr::inner_join(t_ects, faculty_n) %>% 
  mutate(ects_per_faculty = total_ects/faculty_n) %>% 
  arrange(-ects_per_faculty)
ects_per_faculty
