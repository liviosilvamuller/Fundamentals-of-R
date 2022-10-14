#Fundamentals of R 
#Homework - Block 2
#Ruhikaa Ramalingam 

#Question 1 

View(spring_22)
spring_22 <- pivot_longer(spring_22, cols=c(ANSO:RISP), names_to = "department", values_drop_na = TRUE)

#could also use mutate() , just becomes a bit lengthy 

#Question 2 

View(autumn_21)
autumn_21 <- separate(autumn_21, code, sep = "-", into = c("Department", "Code"))

#Question 3 - to merge the datasets 

#preparing the datasets to merge 
spring_22 <- rename(spring_22 , title_course = class , ECTS = ects)
spring_22 <- subset(spring_22, select = -c(value, X))

autumn_21 <- rename(autumn_21, title_course = course, department = Department, ECTS = ects)
autumn_21 <- subset(autumn_21, select = -c(Code))

#creating the new data-set 
academic_year <- full_join(spring_22, autumn_21)
summary(academic_year)

#Question 4 - to check for duplicated rows 

academic_year <- distinct(academic_year)
#there seem have been 6 duplicated rows 

#Question 5 - to find how many courses were offered in French in the academic year 

academic_year %>%
  group_by(language) %>%
  count()
# 11 courses taught in French and 265 courses taught in English 

#Question 6 - to courses offered in the autumn semester and in English 

academic_year %>% 
  group_by(language, semester) %>% 
  count()
#139 courses were taught in English in the autumn semester  

#Question 7 - to rank departments by number of courses offered each semester 

academic_year %>% 
  group_by(department, semester) %>%
  summarise(N=n()) %>%
  arrange(semester, -N) %>%

#Question 8 - to find which department offers the highest share of courses in the spring semester  

academic_year %>%
  filter(semester == "Spring") %>%
  count()
#In the spring semester there are 124 being offered. Did not have to do this, realized later :/

academic_year %>%
  filter(semester == "Spring") %>%
  group_by(department, semester) %>%
  summarise(N=n()/124*100) %>%
  arrange(-N, department) 
#MINT offered the highest share of courses in the spring semester (33.9 percent)

#Question 9 - to list the overall three fav topics and by favorite topic, I am assuming by fav, we mean the topics we see the most 

academic_year %>%
  group_by(topic) %>%
  summarise(N=n())%>%
  arrange(-N)
#the favorite topics here are other, theory, and conflict. If we ignore `other` it is theory, conflict, and finance.   

#Question 10 - to list the three fav topics of each dept 

academic_year %>%
  group_by(department, topic) %>%
  summarise(N=n()) %>%
  arrange(department, -N) %>%
  slice_head(n=3) %>%
  print(n=27)
  
#department topic            N
#<chr>      <chr>        <int>
#1 ANSO       other           18
#2 ANSO       theory           4
#3 ANSO       inequality       3
#4 DE         other            5
#5 DE         conflict         3
#6 DE         theory           3
#7 DI         other           18
#8 DI         theory           4
#9 DI         trade            3
#10 EI         other           17
#11 EI         trade            7
#12 EI         finance          4
#13 HI         other            9
#14 HI         theory           2
#15 HI         finance          1
#16 HPI        other            5
#17 HPI        human rights     1
#18 HPI        inequality       1
#19 IA         finance          4
#20 IA         other            4
#21 IA         governance       2
#22 MINT       other           44
#23 MINT       NA              13
#24 MINT       conflict        10
#25 RISP       other           17
#26 RISP       governance       4
#27 RISP       theory           3

#Question 11 - to assign "skills" as the topic for workshops

academic_year$topic[is.na(academic_year$topic)] <- "skills" 
#tried with replace_na but could not get to it 

#Question 12 - to find the fav topics of compulsory courses of all dept

academic_year %>% 
  filter(type == "compulsory") %>% 
  group_by(type, department, topic) %>%
  count() %>% 
  arrange(-n, department)

#The fav topics for compulsory courses are as follows, 
#EI - other, ANSO - theory, DI - other, HI - theory, MINT - methods, RISP - other  

#Question 13 - to create a new dummy variable 

academic_year <- dplyr::mutate(`academic_year` , comp_type = case_when( type == "compulsory" & topic == "theory" ~ 1,
                                                                  type == "compulsory" & topic == "methods" ~ 1, FALSE ~ 0)) 
#worked but instead of generating 0 it has generated NA. Need to change NA to 0 

academic_year$comp_type[is.na(academic_year$comp_type)] <- 0
#again, would have preferred top use replace_na 

#Question 14 

#First - to find the total ECTS of each department and creating a new data-set 
finding_ECTS <- dplyr:: group_by(`academic_year`  , department) %>%
  dplyr::summarise(Average_ECTS = mean(ECTS))
View(finding_ECTS)

#Second - merging the two dataset 
who_teaches_more <- full_join( finding_ECTS , faculty_n)
View(who_teaches_more)

#Third - Calculating 
who_teaches_more <- dplyr::mutate(who_teaches_more , which_dept_faculty_is_wiser = Average_ECTS/faculty_n*100)
#On average, the faculty of RISP department teaches more ECTS  

