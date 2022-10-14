#1.)
spring_22 <- pivot_longer(spring_22, 
                          cols = c("ANSO", "DE", "DI", "EI", "HI", "HPI", "HPI", "IA", "MINT", "RISP"),
                          names_to = "Department")
spring_22 <- tidyr::drop_na(spring_22, c("value"))

#2.)
autumn_21 <- separate(autumn_21, code, sep = "-", into=c("Department", "code"))

#3.)
colnames(spring_22)[2] <- 'course'
academic_year <- full_join(spring_22, autumn_21)
academic_year <- subset(academic_year, select = -c(1,9,10))
colnames(academic_year)[1] <- 'title_course'
colnames(academic_year)[7] <- 'department'
colnames(academic_year)[4] <- 'ECTS'

#4.)
academic_year <- academic_year %>% distinct(.keep_all = TRUE)

#5.)
academic_year %>%
  filter(language == "french") %>%
  dim()
#There are 11 languages offered in French during the 21-22 academic year.

#6.)
academic_year %>%
  filter(semester == "Autumn", language == "english") %>%
  dim ()
autumn_21 %>%
  filter(language == "english") %>%
  dim()
#There are 139 English courses in the Autumn 21 semester.
#Just as a note for question 6, because of the instructions in question 3,
#I assumed I should delete the unmentioned columns, such as "code" and "value".
#for academic_year. However, when I do that, it turns the last two rows of autumn_21 into 
#duplicates, which they are not. They are the same workshop, but offered at 2 
#different times of the semester. This is demarcated by the variable "code" 
#originally, which does not exist in the other data det, spring_22, and is 
#removed from academic_year.
#Because of this, if I filter using academic_year I get the answer 139, but if I
#used autumn21 I get the answer 145.

#7.)
acyear_dept <- group_by(academic_year, department) %>%
  count()
acyear_dept <- acyear_dept[order(-acyear_dept$n),]
#MINT, EI, ANSO, DI, RISP, IA, HI, DE, HPI

#8.)
spring_1 <- filter(academic_year, semester == "Spring")
dept_spring<- group_by(spring_1, department) %>%
  count()
dept_spring$share <- dept_spring$n/124
dept_spring <- dept_spring[order(-dept_spring$share),]
#MINT, ANSO, AND EI have the largest share of courses in the spring semester.

#9.)
topics <- group_by(academic_year, topic) %>%
  count()
topics <- topics[order(-topics$n),]
#The most popular topcis are "other", "theory", and "conflict".

#10.)
topic_dept <- group_by(academic_year, department, topic) %>%
  count()
ANSO <- filter(topic_dept, department == "ANSO")
#other, theory, and migration and inequality are tied at 3
DE <- filter(topic_dept, department == "DE")
#other, theory, and conflict
EI <- filter(topic_dept, department == "EI")
#other, trade, finance
HI <- filter(topic_dept, department == "HI")
#other, theory, and inequality and finance tied at 1
HPI <- filter(topic_dept, department == "HPI")
#other, and then methods, inequality, sustainability, and human rights all tied with 1
IA <- filter(topic_dept, department == "IA")
#other, finance, and governance
MINT <- filter(topic_dept, department == "MINT")
#other, conflict, and sustainavility
RISP <- filter(topic_dept, department == "RISP")
#other, governance, theory

#11.)
academic_year$topic <- as.character(academic_year$topic)
academic_year$topic[is.na(academic_year$topic)] <- "skills"

#12.)
compulsary <- group_by(academic_year, type == "compulsory", department, topic) %>%
  count()
#ANSO is theory, DI and EI are other, HI is theory, MINT is methods, RISP is other.
#IA and DE do not have compulsories.

#13.)
academic_year$comp_type <- if_else(academic_year$type == 'compulsory' | academic_year$topic == 'theory' | 'method', 1, 0)
#I'm not sure how to do this. This is the last thing I came up with. I figured the best way to try
#to make the dummy variable was if_else or group by. I can't think of anything else to try.

#14.)
dept_ect <- group_by(academic_year, department, ECTS)%>%
  count()
empl_dept_ect <- full_join(dept_ect, faculty_n)
empl_dept_ect$totalect <- empl_dept_ect$ECTS*empl_dept_ect$n
#ANSO
(21+156)/28
#6.321
#DI
(9+132+36)/26
#6.808
#EI
(18+174)/21
#9.143
#HI
(3+72)/23
#3.261
#HPI
(3+48)/23
#2.217
#MINT
(78+426)/50
#10.080
#RISP
(12+144)/20
#7.800

#MINT, EI, and RISP have the heaviest workload in that order.