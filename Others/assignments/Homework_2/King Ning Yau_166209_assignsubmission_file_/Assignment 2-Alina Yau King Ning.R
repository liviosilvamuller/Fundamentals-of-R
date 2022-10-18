#Homework Block 2
#Yau King Ning, Alina

library(dplyr)

#Question 1
spring_22 <- read_csv(file="spring_22.csv", col_names = TRUE) 
tidy_spring_22 <- pivot_longer(spring_22, cols = c("ANSO", "DE", "DI", "EI", "HI", "HPI", "IA", "MINT", "RISP"),names_to="department") %>%
  drop_na(value)


#Question 2
autumn_21 <- read_csv(file="autumn_21.csv", col_names = TRUE) 
library(stringr)
tidy_autumn_21 <- mutate (autumn_21, department=gsub("\\-.*","",autumn_21$code))
     
                     
#Question 3                      
join_autumn <- tidy_autumn_21 [c(1,3,4,5,6,7,8)] %>% 
  rename(title_course=course)

join_spring <- tidy_spring_22 [c(2:8)] %>% 
  rename(title_course=class)

academic_year <- rbind(join_autumn,join_spring)                             
                         

#Question 4
academic_year <- unique(academic_year)
#or
distinct(academic_year)
#Yes, there are 7 duplicated items.


#Question 5
table(academic_year$language)
# There are 11 courses in French.


#Question 6
table(academic_year$semester,academic_year$language)
# There are 139 courses in the autumn semester and in English.


#Question 7
table (academic_year$department) 
#Rank: MINT > EI > ANSO > DI > RISP > DE=HI=IA > HPI

#or
library(janitor)
tabyl(academic_year$department, sort = TRUE)
#Rank: MINT > EI > ANSO > DI > RISP > DE=HI=IA > HPI


#Question 8
tabyl(join_spring$department, sort = TRUE)
#MINT offers a higher share of courses in the spring semester (33.87%).

#Question 9
table (academic_year$topic) 
#3 favourite topics: Other, Theory, Conflict

#Question 10
table (academic_year$department, academic_year$topic) 
#ANSO: Other, Theory, Migration/Inequality
#DE: Other, Conflict, Theory
#DI: Other, Theory, Trade
#EI: Other, Trade, Finance
#HI: Other, Theory, Finance/Inequality
#HPI:Other, Human rights/Inequality/Methods/Sustainability
#IA: Finance, Other, Governance
#MINT:Other, Conflict, Human rights
#RISP:Other, Governance, Theory

#Question 11
academic_year$topic <-
  academic_year$topic %>% 
  replace_na('skills')

#Question 12
compul <- filter(academic_year, type=="compulsory")
#Favorite topics for compulsory courses in all departments are: Other (11), Theory (7), and Methods (7).

#Question 13
academic_year$comp_type <- ifelse(academic_year$type=="compulsory"& academic_year$topic=="theory"|academic_year$topic=="methods", 1, 0 )

#Question 14
faculty_n <- read_excel("MINT338-Fundamentals of R/Material for week 3-20221007/faculty_n.xlsx")

totalects<- academic_year %>%
  group_by(department) %>%
  summarise(Freq = sum(ects))

prop<-full_join(faculty_n, totalects)

prop<- 
  prop %>%
  mutate (averagehours=prop$Freq/prop$faculty_n)
#Faculty of MINT teaches more ECTs on average.

#OR
ANSO <-filter(academic_year, department=="ANSO")
sum(ANSO$ects)/28
#6.32

DI <-filter(academic_year, department=="DI")
sum(DI$ects)/26
#6.81

EI <-filter(academic_year, department=="EI")
sum(EI$ects)/21
#9.14

HI <-filter(academic_year, department=="HI")
sum(HI$ects)/23
#3.26

HPI <-filter(academic_year, department=="HPI")
sum(HPI$ects)/23
#2.22

MINT <-filter(academic_year, department=="MINT")
sum(MINT$ects)/50
#10.08

RISP <-filter(academic_year, department=="RISP")
sum(RISP$ects)/20
#7.8

#Faculty of MINT teaches more ECTs on average.


