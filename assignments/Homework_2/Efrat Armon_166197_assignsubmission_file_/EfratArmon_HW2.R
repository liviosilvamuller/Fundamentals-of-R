# Title: HW 2
# Purpose: Cleaning and Wrangling Data
# Author: Efrat Armon
# Date: October 13 2022

setwd("C:/Users/efrat/OneDrive/Desktop/IHEID/S3_2022/R/Class 3 07.10")

#1
library(readr)
spring_22 <- read_csv("spring_22.csv")
View(spring_22)

library(tidyr)
library(dplyr)
tidy_spring <- pivot_longer(spring_22,cols=c("ANSO":"RISP"), names_to="department", values_drop_na = TRUE)
View(tidy_spring)
tidy_spring <- select(tidy_spring, "class":"department")

#2
autumn_21 <- read_csv("autumn_21.csv")
View(autumn_21)
tidy_autumn <- separate(autumn_21, code, sep="-", into=c("department", "coursecode"))
View(tidy_autumn)

#3
tidy_spring <- rename(tidy_spring, "title_course" = "class")
tidy_autumn <- rename(tidy_autumn, "title_course" = "course")
tidy_autumn <- select(tidy_autumn, !"coursecode")

academic_year <- bind_rows(tidy_autumn, tidy_spring, id = NULL)
View(academic_year)


#4
academic_year <-  distinct(academic_year)

#5
academic_year %>%
  group_by(language) %>%
  summarise(total = n())
#there are eleven courses in French. 

#6
academic_year %>%
  group_by(language, semester) %>%
  summarise(n())
#139 courses 

#7
academic_year %>%
  group_by(department, semester) %>%
  summarise(N = n()) %>%
  arrange(semester, -N)

#8
academic_year %>%
  filter(semester == "Spring") %>%
  mutate(N_sem = n()) %>%
  group_by(department) %>%
  mutate(N_dep = n()) %>%
  mutate(share = N_dep/N_sem)%>%
  summarise(share)%>%
  distinct() %>%
  arrange(-share)

#9
academic_year %>%
  group_by(topic)%>%
  mutate(N = n())%>%
  summarise(N) %>%
  arrange(-N)%>%
  distinct() %>%
  filter(N>13) 

# I cannot for the life of me figure out how to work the slice commands. They don't do anything to the dataframe when I try to run them. on any dataset. even unpiped ones. 
# I tried typing in line 73: slice_head(n=3) - it returned a 12X2 dataframe with N values all out of order.
#dataset <- slice_head(dataset, n =3) does nothing
# dataset <- slice(dataset, 1:3) did nothing
# also tried to work with the slice_max command to no avail, but I assume the problem is more fundamental if I can't even get basic slice to work. 


#10
academic_year %>%
  arrange(department) %>%
  group_by(department, topic) %>%
  mutate(N_topic_dem = n()) %>%
  arrange(desc(N_topic_dem)) %>%
  select(department, N_topic_dem) %>%
  group_by(department) %>%
  distinct() %>%
  arrange(department) %>%
  slice_head(n=3) %>% #honestly I don't understand why it worked here but isn't working above. 
  relocate(topic, .after = department) %>%
  print(n=nrow(academic_year))

#11
academic_year <- mutate(academic_year, topic = ifelse(type == "workshop", "skills", topic ))


#12
academic_year %>%
  filter(type == "compulsory") %>%
  arrange(department) %>%
  group_by(department, topic) %>%
  mutate(N_topic_dem = n()) %>%
  arrange(desc(N_topic_dem)) %>%
  select(department, N_topic_dem) %>%
  group_by(department) %>%
  distinct() %>%
  arrange(department) %>%
  slice_head(n=1) %>% #honestly I don't understand why it worked here but isn't working above. 
  relocate(topic, .after = department) %>%
  print(n=nrow(academic_year))
#some departments don't have compulsory courses

#alternately
academic_year %>%
  group_by(type)%>%
  mutate(N=n()) %>%
  select(type, N)%>%
  distinct()

#13
academic_year <- mutate(academic_year, comp_type = ifelse(c(type == "compulsory" & (topic == "theory" | topic == "methods")), 1, 0))

#14
library(readxl)
faculty_n <- read_excel("faculty_n.xlsx")
View(faculty_n)

academic_year %>%
  group_by(department)%>%
  summarise(sum(ects))%>%
  left_join(faculty_n, na_matches = "na")%>%
  mutate(ects_per_fac = `sum(ects)`/faculty_n) %>%
  arrange(-ects_per_fac)
