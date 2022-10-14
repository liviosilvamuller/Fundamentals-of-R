#Cleaning and wrangling data

library(dplyr)
library(tidyr)

#Question 1
spring_22 <- pivot_longer(spring_22, cols= c(8:16), names_to="Department", values_to="n")
spring_22<- drop_na(spring_22$n)

#Alternatively using pipe operater 
spring_22 <- spring_22 %>% 
  pivot_longer(8:16, names_to = "Department") %>% 
  drop_na(value) %>% 
  select(-value)
 
#I tried to use na.rm() and drop_na() in line 6, but it didn't work, why?
#This is not a function argument taken by tidy 

#Question 2
autumn_21 <- separate(autumn_21, code, sep = "-", into = c("Department","code"))

#Alternatively 
autumn_21 %>% 
  separate(code, c("Department","code"), sep = "-")

#The order within bracket does not matter if you specify what each argument refers to, e.g. sep=...; cols=...

#Question 3
spring_22 <- rename(spring_22, title_course=class)
autumn_21 <- rename(autumn_21, title_course=course)
academic_year <- full_join(spring_22, autumn_21) %>% 
  distinct ()

#Question 4
academic_year <- academic_year %>% 
  select(-1,-9,-10)

#Question 5
academic_year %>% 
  group_by(language) %>% 
  count()
#there are 11 courses offered in French 

#Alternatively 
academic_year %>% 
  filter(language == "french")

#Question 6
academic_year %>% 
  group_by(semester, language) %>% 
  count()
#there are 145 courses offered in thee autumn semester in English 

#Question 7
academic_year %>% 
  group_by(Department,semester) %>% 
  count() %>% 
  arrange(-n)


#Question 8
share_spring <- academic_year %>% 
  filter(semester=="Spring") %>% 
  group_by(Department) %>% 
  count() %>% 
  arrange(n) %>% 
  mutate(share=100*n/sum(share_spring$n))

#MINT has a higher share of courses, which equals to 33.33%
#not sure if the interpretation the question is correct 

#Alternatively
share_spring <- academic_year %>% 
  filter(semester=="Spring") %>% 
  group_by(Department) %>% 
  count() %>% 
  summarise((share = sum(n)/124))

#123=nrow(spring_22)

#Question 9
academic_year %>% 
  group_by(topic) %>% 
  count() %>% 
  drop_na() %>% 
  arrange(-n) %>% 
  rename(category = n) %>% 
  slice_head(n = 3)

 
#The slice_head doesn't seem working, what's wrong???

#Question 10
academic_year %>% 
  group_by(Department) %>% 
  count(topic) %>% 
  arrange(-n) %>% 
  slice(1:3) %>% 
  print(n=27)

#slice(1:3), it slice the one to three row in each category 

#Question 11

academic_year$topic <- ifelse(academic_year$type=="workshop", "skills", academic_year$topic)

#Alternatively
academic_year <- mutate(academic_year, topic = case_when(type=="workshop" ~ "skills"))
                  

#Question 12
academic_year %>% 
  filter(type=="compulsory") %>% 
  group_by(Department,topic) %>% 
  count() %>% 
  arrange(-n) %>% 
  pivot_wider(names_from = Department, values_from = n)

#The favorite topics for EI, DI, and RISP departments are other; for ANSO and HI are theory; for MINT is methods

#To get the favorite of each department
academic_year %>% 
  filter(type=="compulsory") %>% 
  group_by(Department) %>% 
  count(topic) %>% 
  slice(1:1) %>% 
  pivot_wider(names_from = Department, values_from = topic)


#Question 13
academic_year %>% 
  mutate(comp_type=ifelse(academic_year$type =="compulsory" & (academic_year$topic =="theory" | academic_year$topic =="methods"), 1, 0))

#Question 14
faculty_n <- faculty_n %>% 
  rename(Department = department)
academic_year <- left_join(academic_year, faculty_n, na.rm=TRUE)
academic_year %>% 
  group_by(Department, faculty_n) %>% 
  summarise(ECT = sum(ects)) %>% 
  drop_na() %>% 
  mutate(ect_per_fac = ECT/faculty_n) %>% 
  arrange(-ect_per_fac)

#MINT faculty teaches the most ects on average 

#Alternatively 
ects <- academic_year %>% 
  group_by(Department) %>% 
  summarise(ECTS_dep = sum(ects))
faculty_n <- left_join(faculty_n, ects, by = c("department"="Department"))
faculty_n <- mutate(faculty_n, share_ects = ECTS_dep/faculty_n)