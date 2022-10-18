install.packages(tidyverse)
View (spring_22)
spring_22 <- pivot_longer (spring_22, cols = c("ANSO", "DE", "DI", "EI", "HI", "HPI", "IA","MINT","RISP"), names_to = "department", values_drop_na = 1)
View (autumn_21)
autumn_21 <- separate(autumn_21,code, sep="-",into=c("department","code"))
colnames(autumn_21)
colnames(autumn_21)[1]="title_course"
colnames(spring_22)[2]="title_course"
ac_year <- left_join(autumn_21, spring_22)
View(ac_year)
distinct(ac_year)
french_courses <- filter (ac_year, language == "french")
view (french_courses)
english_courses <- filter (ac_year, language == "english", semester == "Autumn")
view(english_courses)
count(english_courses)
count(french_courses) 
sort(table(ac_year$department),decreasing = TRUE)[1]
sort(table(ac_year$topic), decreasing = TRUE)[1:3]
ac_year <- group_by(ac_year,department,topic)
ac_year <- ac_year %>% replace_na(list(topic="skills"))


                   
                
