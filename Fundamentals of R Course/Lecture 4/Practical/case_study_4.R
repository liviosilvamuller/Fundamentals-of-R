# Varieties of Democracy (VDEM) - Case Study -----------------------------------

# Authors: Henrique Sposito

# Packages:
library(dplyr)
library(tidyr)
library(ggplot2)
library(dslabs)
library(purrr)

# Have democracies become more free and accountable since the 1990s?------------

# Background:
# Since 2017 more than half of the countries in world were
# "technically" consider democracies, at the same time,
# influential works have theorized about the recent "democratic decay" and
# "stretch"; specially with the recent(ish) elections
# of leaders with authoritarian tendencies across several democracies.

# Food for thought: What is a democracy to begin with?
# How would you go about studying democratic decay?
# What might be some issues with comparing democracies across countries?

# Data ----------------------------------------------------------------

# Let's start by collecting democracy data!
# Does anyone here knows what VDEM is?
# If not, please check them out:
# https://www.v-dem.net/project.html

# VDEM measures democracy with as many indicators as possible.
# What are some possible issues with this?
# Tip: Think data collection, aggregation (i.e. amount vs. quality),
# and, above all, definitions.

# Let's download their country and year core data (for R):
# https://www.v-dem.net/vdemds.html
# Do not forget to unzip the folder before setting as your working directory!
# Please make sure you have the correct dataset, the name should be:
# "V-Dem-CY-Core_R_v13/V-Dem-CY-Core-v13.rds".
vdem <- readRDS("~/Desktop/V-Dem-CY-Core_R_v13/V-Dem-CY-Core-v13.rds")
# Your path is different, please change the code above accordingly!

# This dataset is huge ... how to find what all this means?
# I these case, it is better to a take a look at the codebook (in data folder)!
# Do you know what a codebook is and what is its function for research?

# Subset the data --------------------------------------------------------------

# What variables we might need to investigate freedom and accountability in
# democracies since the 1990s?

# 1- We need to know if a country is democratic and how much...
# How about we subset based regimes of the world classification
# (the "v2x_regime" variable in our data)?
# Democratic regimes in this variable, according to the codebook, are two:
# - Electoral democracy: with free and fair multiparty elections ...
# - Liberal democracy: with free and fair multiparty elections, access to justice,
#   transparent law enforcement, respect for personal liberties, rule of law...

# 2- We also need indicators of democracy...
# Let's focus on transparency and accountability ("v2x_accountability_osp"),
# freedom of expression ("v2x_freexp_altinf"),
# and civil society participation (v2x_cspart).
# These variables were transformed into scales (e.g. 0-1) from
# ordinal data from surveys (ordered categorical) as follows:
# - Accountability index: low to high (0-1)
# - Freedom of expression: low to high (0-1)
# - Civil society participation: low to high (0-1)

# Food for thought: Do you think these variables are correlated
# among themselves (i.e. multicolinearity)?

# Let's subset the data for the variables of interest, as well as
# the time and regimes we are interested in.

dem <- dplyr::select(vdem, c(country_name, country_text_id, year,
                              v2x_regime, v2x_accountability_osp,
                              v2x_freexp_altinf, v2x_cspart)) %>%
  # select only the variables of interest
  dplyr::filter(year > 1989, v2x_regime == 2 | v2x_regime == 3) %>%
  # filter for last 30 years (bigger than 1989) and for democracies only
  dplyr::rename(regime = v2x_regime,
                freedom_expression = v2x_freexp_altinf,
                account_index = v2x_accountability_osp,
                civil_society = v2x_cspart) %>% # rename some of these variables
  dplyr::mutate(regime_type = case_when(regime == 2 ~ "Electoral Democracy",
                                        regime == 3 ~ "Liberal Democracy")) %>% 
  # create a new regime type variable and rename the categories within
  tidyr::drop_na() # drop rows where there is at least one NA obs...

# Food for thought: Does removing NA observations create bias?
# Is there a systematic cause for why some obs might be missing?

# Investigate the data ---------------------------------------------------------

# Let's see the averages for our democracy indicators related to accountability.

# Average accountability (top 10 and bottom 10)
dem %>% # call data without assigning it
  group_by(country_name) %>% # group by country name
  summarise(mean = mean(account_index)) %>% 
  # summarize grouped accountability variable by mean
  arrange(-mean) # arrange the data by mean, in decreasing order

dem %>% # call data without assigning it
  group_by(country_name, regime_type) %>% # group by country name
  summarise(mean = mean(account_index)) %>% 
  # summarize grouped accountability variable by mean
  arrange(mean) # arrange the data by mean, in decreasing order

# Any surprises?

# Exercises:
# Do you think the all of those in the top 10 are liberal democracies?
# Do you think the all of those in the top 10 are electoral democracies?
# Could you adapt the code above to find this information out?
# Could you adapt the code above for the other democracy indicators as
# freedom of expression and civil society?

# Pathways: from electoral to liberal democracies (and vice-versa) -------------

# Let's see which countries had multiple democratic regimes.

transition_countries <- dem %>% # saving this object to use later
  group_by(country_name, regime_type) %>% # group by country and regime
  count() %>% # count grouped observations
  group_by(country_name) %>% # re-group by country name only 
  mutate(Duplicated = n()) %>% # add duplicated columns for country names
  filter(Duplicated > 1) %>%  # filter for names that appear more than once
  select(-c(Duplicated)) %>% # remove duplicated variables
  rename(Years = n) # rename for clarity

transition_countries %>% print(n = 54) # print 54 rows instead of default 10

# Let's look at the average differences for our democracy indicators for
# the countries with multiple regimes.

dem %>%
  group_by(country_name, regime_type) %>% # group by country and regime
  summarise(Accountability = mean(account_index),
            Freedom_of_Expression = mean(freedom_expression),
            Civil_Society = mean(civil_society)) %>%
  # summarize various grouped variable by mean
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
           `Civil_Society-Electoral Democracy`) %>% 
  # create variables with mean difference
  select(country_name, Accountability_dif, Freedom_of_Expression_dif,
         Civil_Society_dif) %>% # select only pertinent observations
  pivot_longer(cols = Accountability_dif:Civil_Society_dif) %>% 
  # pivot table longer again
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
       y = "Country",
       x = "Mean Difference")

# It appears that some countries averages for these indicators were worse when
# they were classified as liberal democracies in comparison to when they were
# electoral democracies...
# Let's try a different approach plot averages for all indicators together
# in time and by country.

transition_countries %>% 
  select(-Years) %>% # remove years
  inner_join(dem, by = c("country_name", "regime_type"), multiple = "all") %>%
  # keep only obs in present in both datasets
  mutate(Aggregated_Mean_Indicators = (account_index +
           freedom_expression + civil_society)/3) %>%
  # add rows to form the aggregated indicator
  select(country_name, regime_type, year, Aggregated_Mean_Indicators) %>%
  # select only a few variables
  rename(Country = country_name, Regime = regime_type,
         Year = year) %>%
  ggplot(aes(x = Year, y = Aggregated_Mean_Indicators,
             colour = (Regime == "Liberal Democracy"))) + 
  geom_line(aes(group=1)) +
  facet_wrap("Country") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "From Electoral to Liberal Democracies (and vice-versa)",
       y = "Aggregated Mean Indicators") +
  scale_color_discrete(name="Regime",
                       labels=c("Electoral Democracy",
                                "Liberal Democracy"))

# Does the classification of countries into liberal vs. electoral
# democracy appears a bit arbitrary to you?

# Indicators by region ---------------------------------------------------------

# How about we check the evolution of these indicators by region?
# Let's get and join region data from gapminder (extended version)!

gapminder <- dslabs::gapminder %>% 
  select(country, continent, region) %>% # select only the variables we want
  distinct() # keeps only distinct rolls (no duplicates)
dem_total <- dplyr::left_join(dem, gapminder, by = c("country_name" = "country")) 

# Let's see the evolution of the aggregated indicators per region for 1990 and 2021.

dem_total %>% 
  filter(year == 1990 | year == 2021) %>% # filter for first and last year of data
  group_by(region, year) %>% # group region and year
  summarise(Aggregated_Mean_Indicators = (mean(account_index) +
                                            mean(freedom_expression) +
                                            mean(civil_society))/3) %>% 
  # add aggregated means by groups
  drop_na() %>%
  ggplot(aes(x = Aggregated_Mean_Indicators,
      y = reorder(region, Aggregated_Mean_Indicators),
      Fill = region)) +
  geom_point(aes(color = region)) +
  geom_line(aes(color = region)) +
  geom_text(aes(label = year), nudge_y = 0.3) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = "Are democracies better off in 2021 than in 1990?",
       subtitle = "Difference aggregated means for indicators of
       accountability, freedom of expression, and civil society indicators",
       y = "Region",
       x = "Mean Difference")

# Food for thought: Are democracies across most regions better off than in 1990?
# Do you think we experiencing democratic decay in some regions/democracies and
# not in others? Why?

# Are democracies less likely to go to war? ------------------------------------

# # Extra stuff for those of you who are interested in peace/conflict and democracy.
# 
# # Who knows what democratic peace theory is?
# # In case you want to read more about "Democratic Peace",
# # Rosato (2003) is a great reference:
# # https://www.cambridge.org/core/journals/american-political-science-review/article/abs/flawed-logic-of-democratic-peace-theory/83A851232D297851885BC0D203E38616
# 
# # According to Rosato (2003), there are 6 theorized mechanisms (causal logics)
# # to why democracies should fight (mostly one another) less.
# # Those are: trust and respect, public constrain, group constrain,
# # slow mobilization, no surprises, and information.
# 
# # How would you go about studying democratic peace?
# # What types of data do you think we need?
# # From the top of your head, what would be some issues with
# # accessing the theory?
# # Tip: Think about definitions (what is a democracy?),
# # time (from 1945 to 2006 where most countries a democracy?),
# # and counterfactuals (i.e. are wars common events?)...
# 
# # We still need conflicts data though...
# # Let's look at the Correlates of War (COW)!
# # Do you know COW data?
# # https://correlatesofwar.org/history/
# # Let's download their data on insterstate conflict data:
# # https://correlatesofwar.org/data-sets/mids/
# # If you downloaded the data folder, please do not forget to unzip it!
# MIDS <- readr::read_csv("~/Desktop/Dyadic-MIDs-4.02/dyadic_mid_4.02.csv")
# # Your path will be different, please change accordingly.
# 
# # A big dataset...
# # Let's look into the codebook and see what we should keep!
# # Let's keep dyads of state disputes (state A and state B),
# # their ID variables, year conflict began, and year ended.
# # Some conflicts are smaller than others, let's look only at conflicts with
# # which are actually coded as wars!
# 
# # Food for thought: what is the difference between conflict and war?
# 
# # Can you tell the difference between dyadic data and observational data?
# # What do we need to do with dyadic data to analyze it along with VDEM?
# 
# # Well, for once we need to transform all this into a country year dataset...
# # Now this portion below is a bit complicated, but what we do is:
# # (1) select variables of interest, (2) filter the data for years we want,
# # (3) rename variables, (4) delete duplicate rows,
# # (5) created a nested list variable for year
# # range of a conflict, (6) unnest this variable to long format data,
# # (7) select out some other variables, and (8) remove any duplicate rows again.
# 
# MIDS <- select(MIDS, c(statea, namea, stateb, nameb, strtyr, endyear, war)) %>%
#   dplyr::filter(war == 1 & strtyr > 1945) %>%
#   dplyr::distinct() %>%
#   dplyr::mutate(year = purrr::map2(strtyr, endyear, seq)) %>%
#   tidyr::unnest(cols = year) %>%
#   dplyr::select(-c(strtyr, endyear, war)) %>%
#   dplyr::distinct()
# 
# # Let's add a country name and regime type to the MIDS data.
# 
# vdem_mids <- dplyr::select(vdem, c(country_name, country_text_id,
#                                    country_id, year, histname, COWcode,
#                                    v2x_regime)) %>%
#   dplyr::rename(regime = v2x_regime) %>%
#   dplyr::filter(year > 1945 & year < 2007) %>%
#   dplyr::mutate(regime_type = case_when(regime == 0 ~ "Closed Autocracy",
#                                         regime == 1 ~ "Electoral Autocracy",
#                                         regime == 2 ~ "Electoral Democracy",
#                                         regime == 3 ~ "Liberal Democracy"))
# 
# # But before we join the data, let's make this data longer.
# # We do this to get regime data for country A and B, separately.
# 
# MIDS_regime <- MIDS %>%
#   dplyr::rename(country_text_id = namea, COWcode = statea) %>%
#   dplyr::select(c(COWcode, country_text_id, year))
# MIDS_regime2 <- MIDS %>%
#   dplyr::rename(country_text_id = nameb, COWcode = stateb) %>%
#   dplyr::select(c(COWcode, country_text_id, year))
# 
# # Let's join these datasets.
# 
# MIDS_regime <- dplyr::left_join(MIDS_regime, vdem_mids)
# MIDS_regime2 <- dplyr::left_join(MIDS_regime2, vdem_mids)
# 
# # Let's rename variables in MIDS_regime2 and bind datasets.
# 
# names(MIDS_regime2) <- paste0(names(MIDS_regime2), "_2")
# MIDS_regime <- cbind(MIDS_regime, MIDS_regime2)
# 
# # Many observations as TAW return NAs, that is, there is no information
# # for these states in the VDEM data...
# # Which states do you think we are talking about?
# 
# # # Let's investigate closely what these NAs really are
# # MIDS_regime %>%
# #   dplyr::filter(is.na(country_name)) %>% # get only NAs for country name
# #   dplyr::group_by(country_text_id) %>% # group by the acronym
# #   dplyr::count() %>% # count grouped by obs
# #   print(n = 23) # printing all obs to console
# # # Which states do these acronyms refer to? Is our data biased?
# 
# # For now, we will delete these observations since we do not
# # have VDEM data on them...
# # Let's also create a variable for the type of regimes fighting.
# 
# MIDS_regime <- na.omit(MIDS_regime) %>% # remove NAs
#   dplyr::mutate(conflict_regime = paste0(regime_type, "-", regime_type_2)) %>%
#   # create a regime type var
#   dplyr::select(c(COWcode, country_text_id, year, country_name, conflict_regime))
# 
# # Let's merge this data into the VDEM_mids data.
# # Let's also re-code conflict regime and regime type to get the "status".
# 
# dem_peace <- dplyr::left_join(vdem_mids, MIDS_regime, multiple = "all") %>%
#   dplyr::filter(!is.na(regime_type)) %>%
#   dplyr::mutate(conflict_status = ifelse(is.na(conflict_regime),
#                                          paste0(regime_type, " Peace"),
#                                          paste0("War between ",
#                                                 conflict_regime)),
#                 status = dplyr::case_when(grepl("democracy",conflict_status,
#                                                   ignore.case = TRUE) &
#                                             grepl("war", conflict_status,
#                                                   ignore.case = TRUE)
#                                           ~ "War involving democracy",
#                                           !grepl("democracy",conflict_status,
#                                                   ignore.case = TRUE) &
#                                             grepl("war", conflict_status,
#                                                   ignore.case = TRUE)
#                                           ~ "War not involving democracy",
#                                           grepl("democracy",conflict_status,
#                                                   ignore.case = TRUE) &
#                                             grepl("peace", conflict_status,
#                                                   ignore.case = TRUE)
#                                           ~ "Democratic Peace",
#                                           !grepl("democracy",conflict_status,
#                                                   ignore.case = TRUE) &
#                                             grepl("peace", conflict_status,
#                                                   ignore.case = TRUE)
#                                           ~ "Autocratic Peace"))
# 
# # Let's simply count the status
# 
# dem_peace %>%
#   dplyr::group_by(status) %>%
#   dplyr::count()
# 
# # Let's plot and see how wars have evolved over the years:
# 
# dem_peace %>%
#   group_by(status, year) %>%
#   dplyr::count() %>%
#   ggplot(aes(x = year, y = n)) +
#   geom_smooth(se = FALSE) +
#   facet_wrap(~status, ncol=2, scales = "free") +
#   theme_minimal() +
#   theme(legend.position="none") +
#   labs(title = "Do you buy into democratic peace?",
#        y = "Number of cases",
#        x = "Year")
# 
# # Food for thought: Why is autocratic peace decreasing and democratic
# # peace increasing in time?
# # Do most countries transition regimes without wars?
# # Should we buy into Democratic Peace?
