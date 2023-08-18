# Title: Cleaning and wrangling PhD theses dataset
# Purpose: Clean datasets and create variables necessary for classification
# Authors: Livio Silva Muller
# Date: January 2023

library(tidyverse)
library(cld3) #detect languages of text

setwd("~/Documents/GitHub/Fundamentals_of_R_IHEID2022/Fundamentals_workshop_2023/Case Study/Cleaning theses")
dirty_phd_theses <- readRDS("dirty_phd_theses.rds")
skimr::skim(dirty_phd_theses)

# Data cleaning dirty_phd_theses
dirty_phd_theses <- dirty_phd_theses %>%
  mutate(year=replace_na(year,"2022"), # all missing years are actually 2022
         year=as.numeric(year),
         department= case_when( #harmonizing departments
           program == "PhD in International History and Politics (2021-)" ~  "IPH",
           program == "PhD in International History (2012-2020)" ~  "IPH",
           program == "PhD in Anthropology and Sociology (2013-)" ~  "ANSO",
           program == "PhD in International Relations/Political Science (2012-)" ~  "IRPS",
           program == "PhD in International Relations (1996-2017)" ~  "IRPS",
           program == "PhD in International Studies (2010-2016)" ~  "MINT",
           program == "PhD in Development Studies (1999-2017)" ~  "MINT",
           program == "PhD in International Economics (2013-)" ~  "IE",
           program == "Economies and Institutions" ~  "IE",
           program == "PhD in Development Economics (2014-)" ~  "IE",
           program == "PhD in International Law (2012-)" ~  "IL",
           program == "No specialisation (1928-2001)" ~  "Other"),
           complete_series  =ifelse(year>2012, TRUE, FALSE))%>%#establishing the grounds of comparison)%>%
  select(-year2, -program) #getting rid of useless variables

# Preparing variables for analysis

dirty_phd_theses$language<- detect_language(dirty_phd_theses$abstract) #detect language from abstract
dirty_phd_theses$language <- ifelse(dirty_phd_theses$language=="ig", detect_language(dirty_phd_theses$title), dirty_phd_theses$language) # package codes NAs as igbo...
dirty_phd_theses$language <- gsub("sr", "en", dirty_phd_theses$language) #all "sr" are english (manual check)

sus_en <- ("sustain| enviro| agric |rural development |clean technologies|
                   climate change| carbon emissions| greenhouse gases| natural disasters|
                   conservation| biodiversity| corporate responsibility| energy|
                   natural resources| water| human security| equity| social inequality|
                   social justice|  poverty| peace| urban| land| health")

sus_fr <- ("durabl| enviro| agric | rural |
              développement | technologies propres| changement climatique| émissions de carbone|
              gaz à effet de serre| catastrophes naturelles| conservation|
              biodiversité| responsabilité des entreprises| énergie| ressources naturelles|
              eau| sécurité humaine| équité| inégalités sociales|
              justice sociale| pauvreté| paix| urbain| terre| santé")

gen_en <- ("gender|women|equality|girl|queer|female|femin|sex|race|colonial|ethnic|
           white|ability|diversity|class|inequality|intersectionality|inclusion|lgbt")

gen_fr <- ("genre|femme|égalité|fille|queer|femelle|femme|sexe|race|colonial|ethnique|.
           blanc|capacité|diversité|classe|inégalité|intersectionnalité|inclusion|lgbt")

dirty_phd_theses <- dirty_phd_theses %>%
  mutate(
    sust =ifelse(dirty_phd_theses$language=="fr",
                    stringr::str_count(dirty_phd_theses$abstract,sus_fr),
                    stringr::str_count(dirty_phd_theses$abstract,sus_en)),
    gen =ifelse(dirty_phd_theses$language=="fr",
                  stringr::str_count(dirty_phd_theses$abstract,gen_fr),
                  stringr::str_count(dirty_phd_theses$abstract,gen_en)))

saveRDS(dirty_phd_theses, "cleaned_phd_theses.rds")
