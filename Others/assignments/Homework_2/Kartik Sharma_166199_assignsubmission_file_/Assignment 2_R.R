#Assignment Week 2

#1

library(tidyr)
library(dplyr)
spring_22b <- spring_22 %>%
  pivot_longer(ANSO:RISP, names_to ="Department", values_to = "courses_offered") %>%   
filter(courses_offered== 1)

#2

library(tidyr)
autumn_21 <- separate (autumn_21, code, sep="-", into=c("Department", "code"))

#3

library(dplyr)


spring_22 <- spring_22b %>% rename(course = class) %>% 
  select(-courses_offered)

academic_year <- full_join(spring_22, autumn_21) %>% 
  distinct() %>% 
  select(-code)

#4

academic_year <- distinct(academic_year)

#5

academic_year %>% group_by(language) %>% count()

#6

autumn_21 %>% group_by(language) %>% count()

#7

academic_year %>% 
  group_by(Department, semester) %>% 
  count() %>% 
  arrange(-n)

#8

academic_year %>% 
  filter(semester == "Spring") %>% 
  group_by(Department) %>% 
  count() %>% 
  summarise(share = sum(n)/124) %>% 
  arrange(-share)

#9

academic_year %>% 
  group_by(topic) %>% 
  count() %>% 
  arrange(-n)
#Other, Theory and Conflict are the three favorite topics overall.

#10

academic_year %>% 
  group_by(Department) %>% 
  count(topic) %>% 
  arrange(-n) %>% 
  slice(1:3) %>% 
  print(n = 27)
# MINT, ANSO and DI are the three favorite topics. 

#11

academic_year <- mutate(academic_year, 
                        topic_complete = ifelse(type == "workshop",
                                                "skills",
                                                topic) )

#12

academic_year %>% 
  filter(type == "compulsory") %>% 
  group_by(Department, topic) %>% 
  count(topic) %>% 
  arrange(-n)

#13

a <- academic_year %>% 
  mutate(comp_type = 
           ifelse (type == "compulsory" & topic == "methods" | topic == "theory",1,0))

#14

ects <- academic_year %>% 
  group_by(Department) %>% 
  summarise(ECTS_dep = sum(ects))

cc <- left_join(faculty_n, ects,
                by = c("department" = "Department"))

cc <- mutate(cc, share_ects = ECTS_dep/faculty_n)

