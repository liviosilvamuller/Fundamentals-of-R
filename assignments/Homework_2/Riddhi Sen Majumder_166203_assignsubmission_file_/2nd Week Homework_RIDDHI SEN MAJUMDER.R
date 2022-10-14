#Assignment 2

library (tidyr)
library (dplyr)

#1
spring_22_t <- spring_22 %>%
  pivot_longer(ANSO:RISP, names_to = "Department") %>% 
  drop_na()

#2
autumn_21 <- separate (autumn_21, code, sep = "-", into = c("Department", "code"))

#3
spring_22_t <- spring_22_t %>% 
  rename(course = class) 
academic_year <- full_join(autumn_21, spring_22_t) 

#4
academic_year <- distinct(academic_year)

#5
academic_year %>% group_by(language) %>% count()

##Courses in French: 11

#6
autumn_21 %>% group_by(language) %>% count()

##Total Autumn Courses: 152; French Courses (Autumn): 7

#7
academic_year %>%
  group_by(Department,semester) %>% 
  count() 
##Was confused as to how to rank by using arrange function

#8
academic_year %>% 
  filter(semester == "Spring") %>% 
  group_by(Department) %>% 
  count()
##MINT has higher share

#9
academic_year %>% 
  group_by(topic) %>% 
  count()

##Favourite topics overall: Other than 'other' & 'workshops', the favourite topics
  ##are theory & conflict

#10
academic_year %>% 
  group_by(Department)
##Was unsure as to how use the slice function

#11
academic_year <- mutate(academic_year, 
                        topic = ifelse(type == "workshop",
                                                "skills",
                                                topic) )
                                                                              
#12
academic_year %>% 
  filter(type == "compulsory") %>% 
  group_by(Department, topic) %>% 
  count(topic)
##'Other' and 'theory'

#13
academic_year <- academic_year %>% 
  mutate(comp_type = 
           ifelse (type == "compulsory" & topic == "methods" | topic == "theory",1,0))

#14
##Did not get the question