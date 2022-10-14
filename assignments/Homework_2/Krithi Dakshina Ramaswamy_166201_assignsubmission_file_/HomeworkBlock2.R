library(readr)
spring_22 <- read_csv("Desktop/R/Material for week 3-20221007/spring_22.csv")
View(spring_22)
library(tidyr)
library(dplyr)

#1
spring_22 <- spring_22 %>% 
  pivot_longer(ANSO:RISP, names_to = "Department") %>% 
  drop_na(value)%>% 
  select(-value) %>%
  select(-'...1')
#2
autumn_21 <- autumn_21 %>% 
  separate(code, c("Department", "code"), sep = "-")
#3
spring_22 <- spring_22 %>% rename (course = class)
academic_year <- full_join(spring_22, autumn_21)
#4
#already removed
academic_year <- distinct(academic_year)
#5 
#11 classes were offered in French and 265 in English
academic_year %>% group_by(language) %>% count()
academic_year %>% filter(language == "french")
#6
#In the autumn semester, 145 courses were offered in English
academic_year  %>% group_by(language, semester)  %>% count()
#7 
academic_year  %>%
  group_by(semester, Department)  %>% 
  count()  %>% 
  arrange(n)
#MINT offers the highest number of courses in both semesters
#8
academic_year  %>%  
  filter(semester == "Spring")  %>%
  group_by(Department)  %>%
  count()  %>%
  summarise(share = sum(n)/124)  %>%
  arrange(-share)
#MINT offers the highest share of courses
#9
#According to this, 'other', 'theory', and (excluding NA) 'conflict' are the most popular topics
academic_year  %>% 
  group_by(topic) %>%
  count() %>%
  arrange(-n) %>%
  rename(category = n)
#10
#I'm not sure if this is what the question is asking but it is as far as I could do 
academic_year %>%
  group_by(Department, topic) %>%
  count(topic)  %>%
  arrange(-n) %>% 
  slice(1:3) %>%
  print(n = 27)
#11
academic_year <- mutate(academic_year,
                        topic_comp = ifelse(type == "workshop",  "skills", topic))
#12
#methods and 'other' are the most popular topics
academic_year %>% filter(type == "compulsory") %>%
  group_by(Department) %>%
  count(topic) %>%
  slice(1:1)
#13
academic_year <- academic_year %>% mutate(comp_type= ifelse(type == "compulsory" &
                                                              topic == "methods" |
                                                              topic == "theory", 1, 0))
#14
#loading faculty_n file
View(faculty_n)
#in order to get all ECTS: 
ects <- academic_year %>% group_by(Department) %>%
  summarise(ECTS_dep = sum(ects)) 
#now, joining both:
faculty_share <- left_join(faculty_n, ects, by = c("department" = "Department"))

faculty_share <- mutate(cc, share_ects = ECTS_dep/faculty_n)
View(faculty_share)
