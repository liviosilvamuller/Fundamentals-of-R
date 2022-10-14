# Title: Fundamentals of R
# Purpose: Block 2 - Assignment
# Author: Palak Wahi
# Date: 13 October 2022

setwd("/Users/PalakWahi1/Desktop/Sem III/Fundamentals of R/Class 3/Material for week 3-20221011")

############ Q1
library(readr)
spring_22 <- read_csv("~/Desktop/Sem III/Fundamentals of R/Class 3/Material for week 3-20221011/spring_22.csv")
View(spring_22)

tidy_spring_22 <-  pivot_longer(spring_22, cols=c("ANSO":"RISP"), 
                               names_to="Department", values_drop_na= TRUE)

############ Q2
library(readr)
autumn_21 <- read_csv("~/Desktop/Sem III/Fundamentals of R/Class 3/Material for week 3-20221011/autumn_21.csv")
View(autumn_21)

tidy_autumn_21 <- separate(autumn_21, 
                           code, sep="-", into = c("Department", "Code"))

############ Q3

tidy_spring_22 <- rename(tidy_spring_22,
                         title_course = class,
                         department = Department,
                         ECTS = ects)
tidy_autumn_21 <- rename(tidy_autumn_21,
                         title_course = course,
                         department = Department,
                         ECTS = ects)
tidy_autumn_21 <- subset(tidy_autumn_21, select = -c(Code))

academic_year <- full_join(tidy_spring_22, tidy_autumn_21)
#Joining, by = c("title_course", "language", "semester", "ECTS", "type", "topic", "department")

############ Q4
academic_year <- unique(academic_year)
# 6 duplicate rows removed

############ Q5
count(academic_year, 
      academic_year$language == "french")
#OR
academic_year %>%
  group_by(language) %>%
  count()
## 11 subjects were offered in french

############ Q6
count(academic_year, 
      academic_year$semester == "Autumn" & academic_year$language == "english")
#OR
academic_year %>%
  group_by(language, semester) %>%
  count()
## 139 courses were offered in the autumn semester and in English at IHEID?


############ Q7
academic_year %>%
  group_by(department, semester) %>%
  summarise(N = n()) %>%
  arrange(semester, -N)

############ Q8
academic_year %>%
  filter(semester =="Spring")%>%
  group_by(department, semester)%>%
  count()%>%
  arrange(-n)

# MINT offers a higher share of courses in the spring semester

############ Q9
academic_year %>%
  group_by(topic) %>%
  count() %>%
  arrange(-n)

#The three favorite topics overall are Theory, Conflict and Finance (ignoring for
# 'others' category in top)

############ Q10
academic_year %>%
  group_by(department, topic) %>%
  count() %>%
  arrange(-n, department) %>%
  print(n=58)



############ Q11
academic_year$topic[is.na(academic_year$topic)] <- "skills"


############ Q12
academic_year %>%
  filter(type == "compulsory") %>%
  group_by(type, topic, department) %>%
  count() %>%
  arrange(-n) %>%


############ Q13
academic_year <- dplyr::mutate(`academic_year`, 
                               comp_type = case_when(type== "compulsory" & topic =="theory" ~ 1, 
                                                     type== "compulsory" & topic =="methods" ~ 1,
                                                     FALSE ~ 0
                                                     ))
## For some reason, the code is not working right. 

############ Q14
library(readxl)
faculty_n <- read_excel("faculty_n.xlsx")
View(faculty_n)

finding_ECTS <- dplyr::group_by(`academic_year`, department) %>%
  dplyr::summarise(Total_ECTS = sum(ECTS)) %>%
  View("finding_ECTS")

who_teaches_more <- full_join(faculty_n, finding_ECTS)
View(who_teaches_more)
 # MINT faculty teaches more ECTs on average
who_teaches_more <- dplyr::mutate(who_teaches_more,
                                  which_dept_faculty_is_wiser = total_ECTS/faculty_n)
