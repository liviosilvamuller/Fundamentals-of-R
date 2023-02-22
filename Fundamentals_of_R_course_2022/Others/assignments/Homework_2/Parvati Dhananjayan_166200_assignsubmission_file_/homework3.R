
#question 1 
Department <- pivot_longer(spring_22,cols=c("ANSO":"RISP"), names_to="department", values_drop_na = TRUE)

#question 2 
tidy_autumn<-separate(autumn_21, code, sep="-", into=c("Department", "Code"))


#question 3

colnames(Department)[2] ="course"
colsnames(Depatment)[7]="Department"
colsnames(Department)[8]="Department"
tidy_spring <- rename(Department,
                         title_course = class,
                         department = Department,
                         ECTS = ects)
tidy_spring<- rename(Department, Department=department,ECTS=ects)
tidy_autumn<- rename(tidy_autumn, Department=department, ECTS=ects)
tidy_autumn<- rename(tidy_autumn, ECTS=ects)
academic_year<-full_join(tidy_autumn, tidy_spring)
academic_year<-academic_year[,-3]

#question 4
unique(academic_year)
academic_year<-unique(academic_year)
#270 entries 

count(academic_year,french)

#question 5 
count(academic_year,language)
#french=11

count(academic_year, langauge, semester)

#QUESTION 6
count(academic_year,semester,langauge)

new_academic_year<-count(academic_year,semester,language)
#autumn english = 139
#autumn french= 7
#spring english=120
#spring french=4

#question 7 
academic_year%>%group_by(Department,semester)%>%summarise(N=n()%>%)arrange(semester,-N)


academic_year %>%
  group_by(Department, semester) %>%
  summarise(N = n()) %>%
  arrange(semester, -N)

#question 8 
academic_year %>%
  filter(semester == "Spring") %>%
  mutate(N_sem = n()) %>%
  group_by(Department) %>%
  mutate(N_Dep = n()) %>%
  mutate(share = N_Dep/N_sem)%>%
  summarise(share)%>%
  distinct() %>%
  arrange(-share)


#question 9
academic_year %>%
  group_by(topic) %>%
  count() %>%
  arrange(-n)
#3 fav topics are theory,conflict and finace. But there is also "other"=137, which is irrelevant here.


#question 10 
academic_year %>%
  group_by(Department, topic) %>%
  count() %>%
  slice(3) %>%
  arrange(-n)

academic_year %>%
  group_by(Department, topic) %>%
  count() %>%
  slice(3) %>%
  arrange(-n)

academic_year[,-8]
academcic_year<-academic_year[,-8]


#question 12
academic_year %>%
  filter(type=="compulsory") %>%
  group_by(topic,type,Department) %>%
  count() %>%
  arrange(-n)

#question 13


#question 14
