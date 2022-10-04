# Title: Fundamentals of R
# Purpose: Block 2 - Case Study: Democratic Peace
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: 7 October 2022

# Are democracies less likely to go to war?

# Since many of you are interested in conflict,
# let's try and test democratic peace theory!

# Who knows what democratic peace theory is?
# In case you want to read more about "Democratic Peace",
# Rosato (2003) is a great reference:
# https://www.cambridge.org/core/journals/american-political-science-review/article/abs/flawed-logic-of-democratic-peace-theory/83A851232D297851885BC0D203E38616

# According to Rosato (2003), there are 6 theorized mechanisms (causal logics)
# to why democracies should fight one another less. Those are:
# trust and respect, public constrain, group constrain, slow mobilization,
# no surprises, and information.

# How would you go about studying democratic peace?
# What types of data do you think we need?
# From the top of your head, what would be some issues with
# accessing the theory?
# Tip: Think about definitions, time, and counterfactuals! 

# Minimally, to try and look at this we need data on political regimes and
# on conflicts...

# Let's start by collecting democracy data!
# Does anyone here knows what VDEM is?
# If not, please check them out:
# https://www.v-dem.net/project.html

# Let's doenload their country and year core data (for R):
# https://www.v-dem.net/vdemds.html
# Do not forget to unzip the folder before setting as your working directory!
vdem <- readRDS("~/Desktop/Country_Year_V-Dem_Core_R_v12/V-Dem-CY-Core-v12.rds")
# Your path is different, please change the code accordingly!
# Does anyone know what Rds format is?
# It is a very efficient way of saving R data that maintains variable classes!

# Wow, this dataset is very big...
# 27380 rows and 1818 variables...
# How would you go about investigating this?
# When it comes with datasets this big, the best way is to
# read their codebook first!
# Luckly, when you download the VDEM data, a lot of documentation is included!

# Let's first select a time range, and some indicators of democracy 
# that might correlate with the mechanisms mentioned above!
# Time: let's subset data from 1950 to 2006...
# Do you think this is appropriate? Why?
# Democracy: let's stick with regimes of the world classification ("v2x_regime").
# Do you think this is appropriate? Why? What is a democracy to you?
# Mechanisms: let's keep indicators of transparency, accountability,
# number of political parties, and freedom of the press.
# Can you find some of these variables in the codebook?
# Here we will keep barriers to forming a party ("v2psbars"),
# accountability index ("v2x_accountability"), freedom of expression
# ("v2x_freexp_altinf"), legislative constrains to executive ("v2xlg_legcon"),
# and civil society participation index (v2x_cspart).
# We also need to keep some ID and date variables!

# Remember to install dplyr, tidyr, and purrr if you do not have it already!
library(dplyr)

# Below we (1) select some variables, (2) filter for the appropriate years,
# (3) rename variables, and (4) create a new regime variable with types!
vdem <- dplyr::select(vdem, c(country_name, country_text_id,
                              country_id, year, histname, COWcode,
                              v2x_regime, v2psbars, v2x_accountability,
                              v2x_freexp_altinf, v2xlg_legcon, v2x_cspart)) %>%
  dplyr::filter(year > 1945 & year < 2007) %>% 
  dplyr::rename(regime = v2x_regime, party_formation = v2psbars,
                freedom_expression = v2x_freexp_altinf,
                account_index = v2x_accountability,
                legislative_constrain = v2xlg_legcon,
                civil_society = v2x_cspart) %>% 
  dplyr::mutate(regime_type = case_when(regime == 0 ~ "Closed Autocracy",
                                        regime == 1 ~ "Electoral Autocracy",
                                        regime == 2 ~ "Electoral Democracy",
                                        regime == 3 ~ "Liberal Democracy")) 
View(vdem)
# Ok, this looks rather good!

# We still need conflicts data though...
# Let's look at the Correlates of War (COW)!
# Do you know COW data?
# https://correlatesofwar.org/history/
# Let's download their data on insterstate conflict:
# https://correlatesofwar.org/data-sets/mids/
# Again, if you downloaded the data folder, please do not forget to unzip it!
MIDS <- readr::read_csv("~/Desktop/Dyadic-MIDs-4.02/dyadic_mid_4.02.csv")
View(MIDS)

# A big dataset...
# Let's look into the codebook and see what we should keep!
# Let's keep dyads of state disputes (state A and state B), 
# their ID variables, year conflict began, and year ended.
# Some conflicts are smaller than others, let's look only at conflicts with
# which are actually coded as wars!

# Food for thought: what is the difference between conflict and war?

# Can you tell the difference between dyadic data and observational?
# What do we need to do with the MIDS dyadic data to analyze it along with vdem?

# Well, for once we need to transform all this into a country year dataset...
# Now this portion below is a bit complicated, but what we do is:
# (1) select variables of interest, (2) filter the data for years we want,
# (3) rename variables, (4) delete duplicate rows,
# (5) created a nested list variable for year
# range of a conflict, (6) unnest this variable to long format data,
# (7) select out some other variables, and (8) remove any duplicate rows again.

# To run the code below, you might need some additional packages as
# tidyr and purrr, remember to download these!
# BTW, can you tell why we are loading some packages, but not others?
MIDS <- select(MIDS, c(statea, namea, stateb, nameb, strtyr, endyear, war)) %>%
  dplyr::filter(war == 1 & strtyr > 1945) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(year = purrr::map2(strtyr, endyear, seq)) %>%
  tidyr::unnest(cols = year) %>%
  dplyr::select(-c(strtyr, endyear, war)) %>% 
  dplyr::distinct()

# Let's add a country name and regime type to the MIDS data!
vdem_regime <- dplyr::select(vdem, c(country_name, country_text_id, year,
                                     regime_type, COWcode))

# But before we join the data, let's make this data longer!
# We do this to get regime data for country A and B!
MIDS_regime <- MIDS %>%
  dplyr::rename(country_text_id = namea, COWcode = statea) %>%
  dplyr::select(c(COWcode, country_text_id, year))
MIDS_regime2 <- MIDS %>%
  dplyr::rename(country_text_id = nameb, COWcode = stateb) %>%
  dplyr::select(c(COWcode, country_text_id, year))

# Let's join these datasets!
MIDS_regime <- dplyr::left_join(MIDS_regime, vdem_regime)
MIDS_regime2 <- dplyr::left_join(MIDS_regime2, vdem_regime)

# Notice that since some of the variables have the same name,
# dplyr automatically understands that these are the joining ones!

# Now, let's rename variables in MIDS_regime2 and bind datasets!
names(MIDS_regime2) <- paste0(names(MIDS_regime2), "_2")
MIDS_regime <- cbind(MIDS_regime, MIDS_regime2)
View(MIDS_regime)

# Some obsevations as TAW return NAs, that is, there is no information
# for these states in the VDEM data...
# Let's see how many are these
sum(is.na(MIDS_regime$country_name))
# 308 NAs, that is almost half of the data...
# Let's investigate closely what these NAs really are
MIDS_regime %>%
  dplyr::filter(is.na(country_name)) %>% # get only NAs for country name
  dplyr::group_by(country_text_id) %>% # group by the acronym
  dplyr::count() %>% # count grouped by obs
  print(n = 23) # printing all obs to console

# Can you tell me more about which states do these acronyms refer to? 
# Why do you think that is?
# Is our data biased because of that?
# How should we handle this?
# Would removing all the missing data be fair?
# Would treating all of the missing data as closed autocracies regimes be fair?

# For now, we will delete these observations since we do not
# have VDEM data on them...
# Let's also take this change to create a variable for the type of regimes
# fighting. How would you do this for this dataset?
MIDS_regime <- na.omit(MIDS_regime) %>% # remove NAs
  dplyr::mutate(conflict_regime = paste0(regime_type, "-", regime_type_2)) %>% 
  # create a regime type var
  dplyr::select(c(COWcode, country_text_id, year, country_name, conflict_regime))

# Shall we take a look at our new conflict variable?
summary(as.factor(MIDS_regime$conflict_regime))

# Notice that by the end of this, we only have 286 conflicts/years coded...

# Let's merge this data into the full VDEM data.
# Let's also re-code conflict regime and regime type to
# create our dependent variable, "status".
vdem_mids <- dplyr::left_join(vdem, MIDS_regime) %>%
  dplyr::filter(!is.na(regime_type)) %>%
  dplyr::mutate(conflict_status = ifelse(is.na(conflict_regime),
                                         paste0(regime_type, " Peace"),
                                         paste0("War between ",
                                                conflict_regime)),
                status = dplyr::case_when(grepl("democracy",conflict_status,
                                                  ignore.case = TRUE) &
                                            grepl("war", conflict_status,
                                                  ignore.case = TRUE) 
                                          ~ "War involving democracy",
                                          !grepl("democracy",conflict_status,
                                                  ignore.case = TRUE) &
                                            grepl("war", conflict_status,
                                                  ignore.case = TRUE) 
                                          ~ "War not involving democracy",
                                          grepl("democracy",conflict_status,
                                                  ignore.case = TRUE) &
                                            grepl("peace", conflict_status,
                                                  ignore.case = TRUE) 
                                          ~ "Democratic Peace",
                                          !grepl("democracy",conflict_status,
                                                  ignore.case = TRUE) &
                                            grepl("peace", conflict_status,
                                                  ignore.case = TRUE) 
                                          ~ "Autocratic Peace"))
                                          
# Let's summarize the variables  see the findings here...
vdem_mids %>%
  dplyr::group_by(status) %>%
  dplyr::count()

# By only looking at this, what are your impressions about
# democratic peace?

# It is important to think back conceptually about democratic peace.
# What would be some issues with accessing the theory?
# Think about counterfactuals (i.e. are wars common events?),
# time (from 1950 to 2006, where most countries a democracy?), and
# what does it even mean to be a democracy?

# Let's plot and see how wars have evolved over the years:
plot <- vdem_mids %>% 
  group_by(status, year) %>% 
  dplyr::count()
library(ggplot2)
ggplot(plot, aes(x = year, y = n)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~status, ncol=2, scales = "free")

# Should we buy into Democratic Peace?
