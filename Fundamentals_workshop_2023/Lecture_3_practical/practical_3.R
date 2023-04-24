# Title: Fundamentals of R
# Purpose: Block 2 - Practical
# Authors: Henrique Sposito & Livio Silva-Muller

# Let's mind the GAP, again...
# But how about we use tidy this time!

# 1 Load packages and the data -------------------------------------------------

# Today we will be using some of the tidy packages and functions we saw.
# As always, for some more information about the package you can type `?dplyr`,
# for example.
library(dplyr)
library(tidyr)

# Let's also load the data
library(gapminder)
data("gapminder")

# Before we start, do you remember the '%>%' operator?
# You can type it faster using the "Shift+Command+M" shortcut on Mac or
# "Ctrl+Shift+M" for Windows!

# For more information on shortcuts in R Studio, go to 'Tools' and click on
# 'Keyboard shortcut Help'.

# 2. Filtering, selecting, and renaming data -----------------------------------

# Do you remember this data from last week?
#skimr::skim(gapminder)

# Once again we want to see all the observations for the country Switzerland:

dplyr::filter(gapminder, country == "Switzerland")

# Note: should we always use the "::" to call a function within a package?
# It can make a script clearer, more helpful, and reproduciable!

# If you want to save all the observations for the country Switzerland for
# the year 2007?

swiss_2007 <- dplyr::filter(gapminder, country == "Switzerland", year == 2007)

# But I am actually mostly interested in life expectancy and population variables...

dplyr::select(gapminder, country, year, lifeExp, pop)

# I don't like these variable names though...

dplyr::rename(gapminder, population = pop, life_expectancy = lifeExp)

# Let's get more serious here.
# What if I would like to retrieve information for Switzerland, Germany, and
# France for the first and last rounds of the data, keep only life expectancy
# and population variables, and change these variable names?

gapminder %>%
  dplyr::filter(country == "Switzerland" | country == "France" | country == "Germany",
                year == 1952 | year == 2007) %>%
  dplyr::select(country, year, lifeExp, pop) %>%
  dplyr::rename(population = pop, life_expectancy = lifeExp)

# Thank you pipe operator !!!
# For more info, see `?"%>%"`

# 3. Grouping, mutating, and summarizing data ----------------------------------

# Say now you are interested in continent level data, let's group the data:

dplyr::group_by(gapminder, continent)

# Did anything happen to the data? Why?

# I am not a huge fan of GDP per capita, let's create a GDP variable.

gapminder <- dplyr::mutate(gapminder, GDP = pop*gdpPercap)

# Let's create a new variable for rich countries if their GDP is
# above the 75th percentile:

quantile(gapminder$GDP) # what's the problem with this?

# Note:: scientific notation is annoying. You can disable it with:

options(scipen=999)

gapminder <- dplyr::mutate(gapminder, rich_country = ifelse(GDP > 105744100901, 1, 0))

# Do you think this was a good idea? Why?
# Let's take a look at the rich countries outside of Europe

dplyr::filter(gapminder, rich_country == 1, continent != "Europe") %>%
  dplyr::select(country, continent, rich_country) %>%
  dplyr::distinct() %>% # This can be a useful function, see `?dplyr::distinct()`
  print(n = 35)

# What does this tell you?
# Okay, let's go back and simply count the data by continent:

dplyr::group_by(gapminder, continent) %>%
  dplyr::count()

# We have a lot of data for the African continent, that is interesting! But why?
# Let's investigate the mean life expectancy for African countries:

gapminder %>%
  dplyr::filter(continent == "Africa") %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(average_life_expectancy = mean(lifeExp)) %>%
  dplyr::arrange(average_life_expectancy) %>% # see `?dplyr::arrange`
  print(n = 52)

# Is this more intuitive than `aggregate()` to you?
# Could you do the same for Europe for comparison on your own?

# Lastly here, let's get the average life expectancy for all countries:
# Let's see the bottom 10 by median this time.

gapminder %>%
  dplyr::group_by(country, rich_country) %>%
  dplyr::summarise(median_life_expectancy = median(lifeExp)) %>%
  dplyr::arrange(median_life_expectancy) %>% # see `?dplyr::arrange`
  dplyr::ungroup() %>% # ´dplyr::slice()´ does not work with multiple groups...
  dplyr::slice_head(n = 10)

# Remember that ´dplyr::slice()´ can work weird when there are multiple groups,
# ungroup before slicing in these cases!

# 4. Pivoting data -------------------------------------------------

# Do you think this data is "tidy"?
# Is this data in wide or long format?

# Say that you would like to change the data format to wide so that each survey 
# year becomes a variable and get life expectancy.

wide_gapminder <- select(gapminder, country, year, lifeExp) %>%
  tidyr::pivot_wider(names_from = year, values_from = lifeExp)

# Remember to see the help pages if you have any questions about pivoting tables! 

# Let's keep only the first and last year and create a "difference" variable:

wide_gapminder <- select(wide_gapminder, country, '1952', '2007') %>%
  rename(first_round = '1952', last_round = '2007') %>%
  mutate(dif_life_expectancy = last_round - first_round) %>%
  arrange(desc(dif_life_expectancy))

# Note: we use pivot long below!

# 5. Joining data --------------------------------------------------------------

# If you were interested in the relationship between GDP per capita and CO2
# emissions, how would you go about studying it?

# Gapminder has some data on GDP per capita already,
# but you still need data on emissions.
# Let's download/load this data (available in class materials).
# The data was retrieved from the World Bank repository.
# It annual CO2 emissions by country.

co2_emissions <- readxl::read_excel("co2_emissions.xlsx")
summary(co2_emissions)

# Remember that your path will likely be different here!!!

# Is this data in long or wide format?
# What should we do here if we want to merge the data?
# Let's pivot the data long first:

co2_emissions <- tidyr::pivot_longer(co2_emissions,
                                     cols = '1990':'2021',
                                     names_to = "year",
                                     values_to = "co2_emissions") %>%
  tidyr::drop_na() %>% # drop all NA obs from data
  dplyr::rename(country = 'Country Name') %>% 
  mutate(year = as.numeric(year))

# Notice that we also removed all NA observations since there were a lots of
# missing year data for certain countries.
# We also mutated and renamed a few variables, can you guess why?

# How about we join the data now?
# What are some things we should consider before merging?
# Are the variables we will use for merging the same class?
# Is the period covered by the data the same? 
# What type of join should I use?

emissions <- dplyr::inner_join(gapminder, co2_emissions,
                               by = c("country", "year"))
emissions

# Lets see the top 10 emitters in 2007

emissions %>%
  filter(year == 2007) %>%
  arrange(desc(co2_emissions))

# But total emissions might be misleading, let's create a new
# "per_capita_emissions" variable

emissions <- emissions %>%
  mutate(per_capita_emissions = co2_emissions/pop)

# Let's investigate the top 20 per capita emitters
emissions %>%
  group_by(country) %>%
  summarise(average_per_capita_emissions = mean(per_capita_emissions)) %>%
  arrange(desc(average_per_capita_emissions)) %>%
  ungroup() %>%
  slice_head(n = 20)

# More interesting!

# 6. Repository ----------------------------------------------------------------

# How about we quickly plot emissions by continent over time?

library(ggplot2)
library(scales)

emissions %>%
  group_by(continent, year) %>%
  summarise(total_emissions = sum(co2_emissions)) %>%
  ggplot2::ggplot(aes(x = year, y = total_emissions, color = continent)) +
  geom_line() +
  theme_minimal()

emissions %>%
  group_by(continent, year) %>%
  summarise(average_per_capita_emission = mean(per_capita_emissions)) %>%
  ggplot2::ggplot(aes(x = year, y = average_per_capita_emission,
                      color = continent)) +
  geom_line() +
  theme_classic()

# Let's see the relationship between emissions and population in 2007:

emissions %>%
  filter(year == 2007, pop > 50000000) %>%
  ggplot2::ggplot(aes(x = pop, y = co2_emissions)) +
  geom_point(aes(color = country)) +
  geom_smooth(se=FALSE, color="black", method="lm") +
  scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 0.000001)) +
  scale_x_continuous(labels = scales::label_number(suffix = " M", scale = 0.000001)) +
  geom_text(aes(label = country), vjust = -0.5, hjust = 0.5) +
  labs(title = "Total emissions by population for 2007",
       subtitle = "For countries with a population larger than 50 million",
       y = "Total Emissions (in millions of CO2)",
       x = "Population (in millions)") +
  theme_bw() +
  theme(legend.position = "none")

# What about we pot emissions in 2007 and mark top emitters per capita?

emissions %>%
  filter(year == 2007) %>%
  mutate(top_emitters = ifelse(per_capita_emissions > quantile(per_capita_emissions,
                                                               probs = 0.9), 1, 0)) %>% 
  arrange(desc(co2_emissions)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(country, co2_emissions),
             y = co2_emissions, fill = as.factor(top_emitters))) +
  geom_col() +
  scale_y_continuous(labels = scales::label_number(suffix = " M",
                                                   scale = 0.000001)) +
  scale_fill_manual(values = c("darkgreen", "red"),
                    labels = c("No", "yes"),
                    name = "Country belongs\nto the top 10%\nfor per capita\nemissions?") +
  coord_flip() +
  labs(title = "Top CO2 emitters in 2007",
       subtitle = "Total versus per capita emissions",
       y = "Total Emissions (in millions of CO2)",
       x = "Country") +
  theme_light()
