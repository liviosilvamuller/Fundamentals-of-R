#title: "Fundamentals of R - Homework"
#author: "Yuyu Chen"
#date: "2022-10-13"
  
library(readr) #read data
library(tidyverse)


# data
 
spring_22 <- read_csv("spring_22.csv")
print(head(spring_22))


 
autumn_21 <- read_csv("autumn_21.csv")
print(head(autumn_21))

## Question 1

 
spring_22 <- pivot_longer(spring_22, cols = 8:16,names_to = 'Department',) %>% drop_na(any_of('value'))
print(spring_22)



## Question 2

 
autumn_21<-separate(autumn_21,code,sep='-',into = 'Department')
print(autumn_21)


## Question 3

 
spring_22 <- spring_22 %>% rename(title_course = class)
autumn_21 <- autumn_21 %>% rename(title_course = course )



 

academic_year <- full_join(spring_22,autumn_21, by=c("title_course","Department","language","ects",'semester','type','topic')) %>%
  select(c("title_course","Department","language","ects",'semester','type','topic')) %>%
  rename(ECTS = ects)
print(academic_year)


## Question 4

# Yes

 
academic_year <- academic_year %>% distinct()
print(academic_year)


## Question 5

 
print(academic_year %>% group_by(language) %>%
  summarise(numbers = n()) )

# 11 courses

## Question 6

 
print(academic_year %>% group_by(language) %>%
  summarise(numbers = n()))

# size of autum

print(dim(autumn_21))

# 152 courses in autumn

# 259 courses English at IHEID

# English 259

## Question 7

 
print(academic_year %>% group_by(Department) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)))




## Question 8

 
print(academic_year %>% group_by(Department) %>%
  summarise(proportion = n()/dim(academic_year)[1]*100) %>%
  arrange(desc(proportion)))

## Question 9

 
print(academic_year %>% group_by(topic) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)) %>%
  slice(1:3))

## Question 10

 
print(academic_year %>% group_by(Department,topic) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)) %>%
  slice(1:3))


## Question 11

 
print(academic_year<-academic_year %>%
  mutate(topic = replace_na(topic, "skills")))



## Question 12

 
print(academic_year %>% filter(type == 'compulsory') %>%
  group_by(Department,topic) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)) %>%
  slice(1:1))



## Question 13

 
print(academic_year %>% mutate(
  comp_type = case_when((type ==  'compulsory') & (topic %in% c("theory","methods")) ~ 1,
                        (type !=  'compulsory') | ((type ==  'compulsory') & !(topic %in% c("theory","methods"))) ~ 0)
  
))


## Question 14


 
library(readxl)
faculty_n <- read_excel("faculty_n.xlsx")
faculty_n <- faculty_n %>% rename(Department = department)
print(head(faculty_n))



 
tempo <- academic_year %>% group_by(Department) %>%
  summarise(numbers = sum(ECTS)) 
print(tempo)


 
print(full_join(faculty_n,tempo, by="Department") %>% 
  mutate(faculty_n = replace_na(faculty_n, 0)) %>% 
  mutate(mean = numbers/sum(faculty_n)) %>% 
  arrange(desc(mean)) )


# On average, the department that teaches the most is the MINT