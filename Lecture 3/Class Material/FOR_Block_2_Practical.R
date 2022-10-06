# Title: Fundamentals of R
# Purpose: Block 2 - Practical (Varieties of Democracy)
# Authors: Henrique Sposito & Livio Silva-Muller
# Date: 7 October 2022

# Have democracies become more accountable and free in the past 30 years?

# Even though in 2017 more than half of the countries in world are
# "technically" consider democracies,
# influential works have theorized about the recent democratic decay,
# stretch, or death, with the recent(ish) elections of leaders with
# authoritarian tendencies across several countries.

# How would you go about studying this?
# What types of data do we need?
# What might be some issues with comparing democracies across countries?
# Above all, what is a democracy?

# Load the data ----------------------------------------------------------------

# Let's start by collecting democracy data!
# Does anyone here knows what VDEM is?
# If not, please check them out:
# https://www.v-dem.net/project.html

# VDEM, to be short, measures democracy with as many indicators as possible...
# What are some possible issues with this?
# Tip: Think data collection, aggregation, and, above all, definitions.

# Let's download their country and year core data (for R):
# https://www.v-dem.net/vdemds.html
# Do not forget to unzip the folder before setting as your working directory!
vdem <- readRDS("~/Desktop/Country_Year_V-Dem_Core_R_v12/V-Dem-CY-Core-v12.rds")
# Your path is different, please change the code above accordingly!

# Does anyone know what Rds format is?
# It is a very efficient way of saving R data that maintains variable classes!

# This dataset is huge ... how to find what all this means?
# I this case, it is better to a take a look at the codebook!
# Do you know what a codebook is and what is its function for research?

# First things first, let's subset this data! ----------------------------------

# 1- We need to filter data for the last 30 years (or so).
# 2- We need to know if a country is indeed democratic ...
# Can you find related variables in the codebook?
# How about we subset based regimes of the world classification ("v2x_regime")?
# 3- We need indicators of transparency and accountability ("v2x_accountability_osp")! 
# Perhaps, also, let's keep freedom of expression ("v2x_freexp_altinf"),
# and civil society participation (v2x_cspart)!

# Remember to install dplyr and tidyr if you do not have it already!
library(dplyr)
library(tidyr)

dem <- dplyr::select(vdem, c(country_name, country_text_id, year,
                              v2x_regime, v2x_accountability_osp,
                              v2x_freexp_altinf, v2x_cspart)) %>%
  # select only the variables of interest
  dplyr::filter(year > 1989) %>% 
  # filter for last 30 years (bigger than 1989) and for democracies!
  dplyr::rename(regime = v2x_regime,
                freedom_expression = v2x_freexp_altinf,
                account_index = v2x_accountability_osp,
                civil_society = v2x_cspart) %>%
  # rename some of these variables
  dplyr::mutate(regime_type = case_when(regime == 2 ~ "Electoral Democracy",
                                        regime == 3 ~ "Liberal Democracy")) %>% 
  # create a new regime type variable and rename the categories within
  tidyr::drop_na() # drop rows where there is at least one NA obs...

# Does removing NA observations create bias?
# Tip: Is there a systematic cause for why some obs might be missing?

View(dem)
# Ok, this looks rather good!

# But we have a little issue here, what do all these scores even mean?
# Let's find out in the codebook!

# Democracy (regime):
# Electoral democracy: Free and fair multiparty elections ...
# Liberal democracy: Free and fair multiparty elections, access to justice,
# transparent law enforcement, respect for personal liberties, rule of law...

# Accountability index: low to high (0-1)
# (BTW do you know what an index is?)

# Freedom of expression: low to high (0-1)

# Civil society participation: low to high (0-1)

# Most of these variables were transformed into scales (e.g. 0-1) from
# ordinal data from surveys (ordered categorical).
# Why do you think that is?

# Food for thought: Do you think these variables are correlated
# among themselves (hat is, do they vary similarly)?
# Why could this be an issue (think multicolinearity)?

# Investigate the data ---------------------------------------------------------

# Do we even have data from all country and years?
dem %>% 
  group_by(country_name, regime_type) %>% # grouping data by country and regime type 
  count() %>% # counting the number of appearances by each group
  print(n = 120) # printing 120 rows instead of only 10
# It seems we do not...

# What to do here? Keep all countries or only complete cases?
# How is this problematic?

# For now, let's keep all and let's see the averages for accountability,
# political party formation, freedom of expression,
# and civil society participation.

# Average accountability (top 10 and bottom 10)
dem %>% # call data without assigning it
  group_by(country_name) %>% # group by country name
  summarise(mean = mean(account_index)) %>% # summarize grouped accountability variable by mean
  arrange(-mean) %>% # arrange the data by mean, in decreasing order
  slice_head(n = 10) # get the top 10

# Any surprises here (i.e. is this expected)?

dem %>% # call data without assigning it
  group_by(country_name) %>% # group by country name
  summarise(mean = mean(account_index)) %>% # summarize grouped accountability variable by mean
  arrange(-mean) %>% # arrange the data by mean, in decreasing order
  slice_tail(n = 10) # get the bottom 10

# What about here, any surprises?

# Do you think the all of those in the top 10 are liberal democracies?
# Do you think the all of those in the top 10 are electoral democracies?

# Let's check:
dem %>% # call data without assigning it
  group_by(country_name, regime_type) %>% # group by country name
  summarise(mean = mean(account_index)) %>% # summarize grouped accountability variable by mean
  arrange(-mean) # arrange the data by mean, in decreasing order

# Top 10 all are liberal democracies... surprise, surprise...
# Remember that indicators are how they classify the country
# regimes to begin with...

dem %>% # call data without assigning it
  group_by(country_name, regime_type) %>% # group by country name
  summarise(mean = mean(account_index)) %>% # summarize grouped accountability variable by mean
  arrange(mean) # arrange the data by mean, in incresiasing order

# All electoral democracies...

# Let's repeat this for all indicators, at once...

dem_mean <- dem %>% # call data without assigning it
  group_by(country_name) %>% # group by country name
  summarise(account_index = mean(account_index),
            freedom_expression = mean(freedom_expression),
            civil_society = mean(civil_society)) # summarize various grouped variable by mean
View(dem_mean)

# To facilitate visualization, say we want a longer table here.
# That is, all indicators in one variable!

dem_mean_long <- pivot_longer(dem_mean, 
                              cols = account_index:civil_society) %>% # making data longer
  rename(Indicator = name, Mean = value, Country = country_name) # renaming for clarity
View(dem_mean_long)

# Is this easier to see? Why would we want this type of data?

# For once, we might want to plot this!
library(ggplot2)
dem_mean_long %>%
  filter(Country == "Switzerland" | Country == "France") %>% # filter by country name
  ggplot(aes(x = Mean, y = Country, fill = Indicator)) + # plot
  geom_point(aes(color = Indicator))

# Why do we use + and not %>% when plotting?
# Tip: Pipping through versus adding something...
# We will learn more about plots next week!

# What if we want to have this data in wide format?
# That is, country as columns and indicators as rows.
dem_mean_wide <- pivot_wider(dem_mean_long, names_from = Country,
                             values_from = Mean) # making data wide
View(dem_mean_wide)
# I am not sure what we can do with this data,
# Can you think about anything?

# We can try and plot...
dem_mean_wide %>%
  ggplot(aes(x = Argentina, y = Indicator)) + 
  geom_point(aes(color = Indicator))

# Electoral versus liberal democracies ----------------------------------------

# Did any countries go from being an electoral democracy to a liberal one,
# and vice-versa?
dem %>% 
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise() # return just the groups

# Let's also get information on years and regime type for these countries.
dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  count() %>% # count grouped observations
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>%  # filter for names that appear more than once
  rename(Years = n, Country = country_name, Regime = regime_type) %>% # rename for clarity
  print(n = 48) # tell dplyr to print 48 rows instead of default 10

# Let's plot the indicators only for these countries!
dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise(Accountability = mean(account_index),
            Freedom_of_Expression = mean(freedom_expression),
            Civil_Society = mean(civil_society)) %>% # summarize various grouped variable by mean
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>%  # filter for names that appear more than once
  pivot_longer(cols = Accountability:Civil_Society) %>% # make data longer
  rename(Country = country_name,
         Regime = regime_type,
         Indicator = name,
         Mean = value) %>% # rename for clarity
  select(-Duplicated) %>% # remove duplicated row
  ggplot(aes(x = Mean, y = Country, fill = Indicator)) + # plot
  geom_point(aes(color = Indicator)) +
  facet_wrap("Regime")

# Can you guess which countries went on to become a liberal democracy?
# Can you guess which countries went on to become an electoral democracy?

# How can we better measure and plot this?
# What if we try a different approach, look at the average differences:

dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise(Accountability = mean(account_index),
            Freedom_of_Expression = mean(freedom_expression),
            Civil_Society = mean(civil_society)) %>% # summarize various grouped variable by mean
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>% # filter for names that appear more than once
  pivot_wider(names_from = regime_type,
              values_from = Accountability:Civil_Society,
              names_sep = "-") %>% # make data wide
  mutate(Accountability_dif = `Accountability-Liberal Democracy` -
           `Accountability-Electoral Democracy`,
         Freedom_of_Expression_dif = `Freedom_of_Expression-Liberal Democracy` -
           `Freedom_of_Expression-Electoral Democracy`,
         Civil_Society_dif = `Civil_Society-Liberal Democracy` -
           `Civil_Society-Electoral Democracy`) %>% # create variables with mean difference
  select(country_name, Accountability_dif, Freedom_of_Expression_dif,
         Civil_Society_dif) %>% # select only pertinent observations
  pivot_longer(cols = Accountability_dif:Civil_Society_dif) %>% # pivot table longer again
  rename(Country = country_name,
         Indicator_Difference = name,
         Mean_Difference = value) %>% # rename for clarity
  ggplot(aes(x = Mean_Difference, y = Country,
             fill = Indicator_Difference)) + # plot
  geom_point(aes(color = Indicator_Difference)) +
  facet_wrap("Indicator_Difference") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "From Electoral to Liberal Democracies",
       subtitle = "Difference in means for accountability,
       freedom of expression, and civil society indicators",
       y = "Country",
       x = "Mean Difference")

# Some countries averages for these indicators were worse when they were
# classified as liberal democracies in comparison to when they were
# electoral democracies... is this puzzling?

# What if we aggregate all of these indicators together?

dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise(Aggregated_Mean_Indicators = (mean(account_index) +
              mean(freedom_expression) + mean(civil_society))/3) %>% # summarize various grouped variable by mean
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>% # filter for names that appear more than once
  rename(Country = country_name,
         Regime = regime_type) %>% # rename for clarity
  ggplot(aes(x = Aggregated_Mean_Indicators,
             y = reorder(Country, Aggregated_Mean_Indicators),
             fill = Regime)) + # plot
  geom_point(aes(color = Regime, shape = Regime)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "From Electoral to Liberal Democracies",
       subtitle = "Difference aggregated means for indicators of
       accountability, freedom of expression, and civil society indicators",
       y = "Country",
       x = "Mean Difference")

# Does the classification of countries into
# liberal and electoral democracies seems arbitrary to you?

# Pathways: from electoral to liberal democracies ----------------------------------------

# For now, we have looked for differences in means for the same country
# when classified as a liberal or an electoral democracy,
# what about we focus on pathways.
# That is, when did countries move from electoral to liberal democracies,
# and vice-versa?

# Let's investigate!
dem %>% select(country_name, regime_type, year) %>% # select only a few variables
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise(Year_min = min(year, na.rm = TRUE),
            Year_max = max(year, na.rm = TRUE)) %>% # get min and max years by groups
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>% # keep only duplicate country names
  select(-Duplicated) %>% # remove duplicated variables
  unite("Range", c(Year_min, Year_max), sep = " - ") %>% # unite min and max year
  pivot_wider(names_from = regime_type,
              values_from = Range) %>% # pivot wider to facilitate visualization
  print(n = 24) # print 24 rows

# Why do some ranges overlap?

# Let's use a join to subset the data here!
subset <- dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  count() %>% # just count obs here
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>% # keep only duplicate country names
  select(-c(Duplicated, n)) # remove duplicated variables

# Let's plot accountability in time!
account_time <- inner_join(dem, subset) %>% # keep only obs in present in both datasets
  select(country_name, regime_type, year, account_index) %>% # select only obs of interest
  rename(Country = country_name, Regime = regime_type,
         Year = year, Accountability = account_index) # rename variables

ggplot(account_time, aes(x = Year, y = Accountability,
                         fill = Regime)) + 
  geom_line(aes(color = Regime)) +
  facet_wrap("Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

# But again, some line overlap...Why is this the case?

# Can you use the codes above to make the same plot for civil society
# and for freedom of expression?

# One last time, let's plot all this together!

all_time <- inner_join(dem, subset) %>% # keep only obs in present in both datasets
  mutate(Aggregated_Mean_Indicators = (account_index +
           freedom_expression + civil_society)/3) %>% # add rows to form the aggregated indicator
  select(country_name, regime_type, year, Aggregated_Mean_Indicators) %>% # select only a few variables
  rename(Country = country_name, Regime = regime_type,
         Year = year)# rename variables
  
ggplot(all_time, aes(x = Year, y = Aggregated_Mean_Indicators,
                     colour = (Regime == "Liberal Democracy"))) + 
  geom_line(aes(group=1)) +
  facet_wrap("Country") +
  theme_minimal() +
  theme(legend.position = "bottom")  

# Does this seem arbitrary to you?

# Looking at this code, could you fix the issue with
# overlapping lines in the accountability plot above?

# Indicators by region ---------------------------------------------------------

# How about we check the evolution of these indicators by region?

# How about we add region information for these countries?
# How would you go about it?
# Tip: mind the gap!

# Let's get and join region data from gapminder!
# Remember to install dslabs package if you do not have it!
gapminder <- dslabs::gapminder %>% 
  select(country, continent, region) %>% # select only the variables we want
  distinct() # keeps only distinct rolls (no duplicates)

# Notice, we need to specify the columns here because
# they have different names in each dataset...
dem_total <- dplyr::left_join(dem, gapminder,
                               by = c("country_name" = "country")) # keep all rows in dem data

# Let's see the evolution of the aggregated indicators per region!
dem_ev <- dem_total %>% 
  filter(year == 1990 | year == 2021) %>% # filter for first and last year of data
  group_by(region, year) %>% # group region and year
  summarise(Aggregated_Mean_Indicators = (mean(account_index) +
                                            mean(freedom_expression) +
                                            mean(civil_society))/3) %>% # add aggregated means by groups
  drop_na() # drop NAs

ggplot(dem_ev, aes(x = Aggregated_Mean_Indicators,
                   y = reorder(region, Aggregated_Mean_Indicators),
                   Fill = region)) +
  geom_point(aes(color = region)) +
  geom_line(aes(color = region)) +
  geom_text(aes(label = year), nudge_y = 0.3) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Are democracies better off than in 1990?",
       subtitle = "Difference aggregated means for indicators of
       accountability, freedom of expression, and civil society indicators",
       y = "Region",
       x = "Mean Difference")

# Are democracies across regions better off than in 1990?
# Are experiencing democratic decay in some regions?
# Or is VDEM just a shitty way to measure democracy?

# Are democracies less likely to go to war? ------------------------------------

##### Extra stuff for those of you who are interested in peace/conflict #####

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
# What do we need to do with dyadic data to analyze it along with VDEM?

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
vdem_mids <- dplyr::select(vdem, c(country_name, country_text_id,
                              country_id, year, histname, COWcode, 
                              v2x_regime, v2x_accountability_osp,
                              v2x_freexp_altinf, v2x_cspart, v2xlg_legcon)) %>%
  dplyr::rename(regime = v2x_regime,
                freedom_expression = v2x_freexp_altinf,
                account_index = v2x_accountability_osp,
                civil_society = v2x_cspart,
                legislative_contrains = v2xlg_legcon) %>%
  dplyr::filter(year > 1945 & year < 2007) %>%
  dplyr::mutate(regime_type = case_when(regime == 0 ~ "Closed Autocracy",
                                        regime == 1 ~ "Electoral Autocracy",
                                        regime == 2 ~ "Electoral Democracy",
                                        regime == 3 ~ "Liberal Democracy")) 

# But before we join the data, let's make this data longer!
# We do this to get regime data for country A and B!

MIDS_regime <- MIDS %>%
  dplyr::rename(country_text_id = namea, COWcode = statea) %>%
  dplyr::select(c(COWcode, country_text_id, year))

MIDS_regime2 <- MIDS %>%
  dplyr::rename(country_text_id = nameb, COWcode = stateb) %>%
  dplyr::select(c(COWcode, country_text_id, year))

# Let's join these datasets!
MIDS_regime <- dplyr::left_join(MIDS_regime, vdem_mids)
MIDS_regime2 <- dplyr::left_join(MIDS_regime2, vdem_mids)

# Now, let's rename variables in MIDS_regime2 and bind datasets!
names(MIDS_regime2) <- paste0(names(MIDS_regime2), "_2")
MIDS_regime <- cbind(MIDS_regime, MIDS_regime2)
View(MIDS_regime)

# Some observations as TAW return NAs, that is, there is no information
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
# Is our data biased?
# How should we handle this?

# For now, we will delete these observations since we do not
# have VDEM data on them...

# Let's also take this change to create a variable for the type of regimes
# fighting.

MIDS_regime <- na.omit(MIDS_regime) %>% # remove NAs
  dplyr::mutate(conflict_regime = paste0(regime_type, "-", regime_type_2)) %>% 
  # create a regime type var
  dplyr::select(c(COWcode, country_text_id, year, country_name, conflict_regime))

# Shall we take a look at our new conflict variable?
summary(as.factor(MIDS_regime$conflict_regime))

# Let's merge this data into the VDEM_mids data.
# Let's also re-code conflict regime and regime type to get the "status".
dem_peace <- dplyr::left_join(vdem_mids, MIDS_regime) %>%
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
dem_peace %>%
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
dem_peace %>% 
  group_by(status, year) %>% 
  dplyr::count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~status, ncol=2, scales = "free") +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Do you buy into democratic peace?",
       y = "Status Count",
       x = "Year")

# Why is autocratic peace decreasing and democratic peace increasing in time?
# Do most states transition regimes without wars?

# What if we add some indicators here, just as above.
dem_peace %>% 
  group_by(status, year) %>%
  # below we multiply the aggregated indicators by count to be able to plot together (scale)
  summarise(count = n(),
            Aggregated_Mean_Indicators = ((mean(account_index) +
                                            mean(freedom_expression) +
                                            mean(civil_society))/3)*(2*count)) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_smooth(se = FALSE) +
  geom_smooth(aes(y = Aggregated_Mean_Indicators), color = "black", se = FALSE) +
  facet_wrap(~status, ncol=2, scales = "free") +
  labs(title = "Do you buy into democratic peace?",
       caption = "Black line represents the difference aggregated means
       for indicators of accountability, freedom of expression,
       and civil society indicators",
       y = "Status Count",
       x = "Year")

# What does adding these indicators tell us?

# 1- Peaceful autocracies left nowadays are more democratic (on average). Why?
# 2- Peaceful democracies have become more democratic. Why?
# 3- Democracies in wars are not very democratic (not very accountable,
# free for expression, and/or with low levels of civil association). Why?
# 4 - Autocracies in war are the least democratic. Why?

# Should we buy into Democratic Peace?
