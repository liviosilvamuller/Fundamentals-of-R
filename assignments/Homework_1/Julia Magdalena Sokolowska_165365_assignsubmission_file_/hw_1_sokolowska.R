# 04.10.2022
# Homework for week 3
# exercise 1: animal longievity
#1.1
#from the environemnt tab we know that there are 4219 observations within 10 different variables
#1.2
#chosen animals:european tree frog (european_frog, frog1), rainbow frog (rainbow_frog, frog2), edible frog (edible_frog, frog3n)

european_frog <- 22
rainbow_frog <- 2
edible_frog <- 12
frog1 <- "european tree frog"
frog2 <- "rainbow frog"
frog3 <- "edible frog"

#1.3 logical test

european_frog == rainbow_frog
rainbow_frog == edible_frog
edible_frog == rainbow_frog

#1.4
#called: name_vec, age_vec
names_vec <- c(frog1, frog2, frog3) 
age_vec <-c(22,12,2)

#1.5
mean(c(age_vec))
#mean:12
median(c(age_vec))
#median: 12

#1.6
names_vec [2]
[1] "rainbow frog"

#1.7
age_vec[age_vec > 27]
numeric(0)
# no animals in my vector have a lifespan longer than 27

subset(animal_longevity, animal_longevity$"Maximum_Longevity_Years" > 27)
#there are 10 animals that have a maximum longievity of more than 27 years in the entire data set
HAGR_ID Organism_Family  Organism_Genus Organi…¹ Organ…² Femal…³ Male_…⁴ Maxim…⁵ Body_…⁶ Tempe…⁷
<dbl> <chr>            <chr>          <chr>    <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
  1       9 Formicidae       Lasius         niger    Black …      NA      NA    28      NA        NA
2      12 Nephropidae      Homarus        america… Americ…      NA      NA   100      NA        NA
3      17 Bombinatoridae   Bombina        variega… Yellow…      NA      NA    29      NA        NA
4      18 Bufonidae        Anaxyrus       america… Americ…     910     910    36      21.6     288
5      28 Bufonidae        Bufo           bufo     Common…    1460    1460    40      NA        NA
6      34 Bufonidae        Rhaebo         blomber… Colomb…      NA      NA    28.7    NA        NA
7      85 Pipidae          Xenopus        laevis   Africa…     183      NA    30.3    63.6     288
8      86 Pyxicephalidae   Pyxicephalus   adspers… Africa…      NA      NA    45      NA        NA
9     111 Ambystomatidae   Ambystoma      maculat… Spotte…    1095     730    32      12.8     290
10     122 Cryptobranchidae Andrias        japonic… Japane…    1825    1825    55      NA        NA

#1.8
head(animal_longevity)
dfnew <- animal_longevity[,c (5,8)]
names (dfnew) <- c("common name", "max_age")
View(dfnew)
dfnew <- order [(dfnew$max_age),]


#1.9
mean(animal_longevity$Maximum_Longevity_Years, na.rm=TRUE)
#mean: 25.21 years
median(animal_longevity$Maximum_Longevity_Years, na.rm=TRUE)
#median: 15.2 years
# since mean > median  means that the distribution of the longevity data is right skewed, which implies that there are some cases of animals that live very long (for example the greenland shark: 392 years), but half of the considered animals live 15.2 years or less

#1.10
#No, it would not work, as r is not sure whcih column you wnat to subset the brackets to (r is confused) <- we just need a comma at the end
outliving_human <- animal_longevity[animal_longevity$Maximum_Longevity_Years > 122.5,]


#EXERCISE 2

#2.1
dim(turnout)
# the dataset has 14 observations of 10 variables
ncol(turnout)
# the dataset has 10 columns
nrow(turnout)
# the dataset has 12 rows
length(turnout)
#the length of the dataset is 10
summary(turnout)
#the range of the years covered is from 1980 to 2008
#there are 14 observations of midterm or presidential elections

#2.2
#below gives the outcome of real turnout for VAP including overseas elligeble voters
turnout$real_turnoutVAP <- ((turnout$total)/(turnout$VAP+turnout$overseas)*100, na.rm=TRUE)
#below gives the outcome of real turnout for VAP including overseas elligeble voters
turnout$real_turnoutVEP <- ((turnout$total)/(turnout$VEP+turnout$overseas)*100, na.rm=TRUE)
#running the above calculations depicts that people are grossly overreporting their turnout rates, in every election the ANES covers the turnout rate was lower than the self-reported one. The real turnout rate is slightly higer for VEP than VAP, but this can be explained by the fact that VEP includes all the people who have the right to vote, whereas VAP also includes fellons and other peole who for some reason lost the right to vote

#2.3
turnout$diffVAP <- (turnout$ANES-turnout$real_turnoutVAP)
turnout$diffVEP <- (turnout$ANES-turnout$real_turnoutVEP)
mean(turnout$diffVAP,na.rm=TRUE)
#mean for diffVAP is 20.32914, meaning that the self reporting indicated a higher election turnout by 20.32194 percentage point than the real turnout rate considering VAP
mean(turnout$diffVEP, na.rm=TRUE)
#mean for diffVEP is 17.5591, meaning that the self reporting indicated a higher election turnout by 17.5591 percentage point than the real turnout rate considering VEP

#2.4
# WHAT DOES THIS ASK ME TO DO:Compare the VEP turnout rate with the ANES turnout rate separately for presidential elections and midterm elections.
split(turnout, f=turnout$election_type)
#split the data frame depending on the type of elections
$midterm
# A tibble: 6 × 15
year    VEP    VAP total  ANES felons noncit overseas osvoters electio…¹ real_…² real_…³ real_…⁴
<dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl>    <dbl> <chr>    <chr>       <dbl>   <dbl>   <dbl>
  1  1982 160467 166028 67616    60    960   6641     1982 NA       midterm     0.419    40.2    41.6
2  1986 170396 177922 64991    53   1367   8362     2216 NA       midterm     0.378    36.1    37.7
3  1990 176629 186159 67859    47   1901  10239     2659 NA       midterm     0.379    35.9    37.8
4  1994 182623 195258 75106    56   2441  12497     2229 NA       midterm     0.396    38.0    40.6
5  1998 190420 205313 72537    52   2920  14988     2937 NA       midterm     0.368    34.8    37.5
6  2002 198382 215462 78382    62   3168  17237     3308 NA       midterm     0.379    35.8    38.9
# … with 2 more variables: diffVAP <dbl>, diffVEP <dbl>, and abbreviated variable names
#   ¹​election_type, ²​real_turnout, ³​real_turnoutVAP, ⁴​real_turnoutVEP
# ℹ Use `colnames()` to see all variable names

$presidential
# A tibble: 8 × 15
year    VEP    VAP  total  ANES felons noncit overseas osvoters electi…¹ real_…² real_…³ real_…⁴
<dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>    <dbl> <chr>    <chr>      <dbl>   <dbl>   <dbl>
  1  1980 159635 164445  86515    71    802   5756     1803 NA       preside…   0.537    52.0    53.6
2  1984 167702 173995  92653    74   1165   7482     2361 NA       preside…   0.546    52.5    54.5
3  1988 173579 181955  91595    70   1594   9280     2257 NA       preside…   0.516    49.7    52.1
4  1992 179656 190778 104405    75   2183  11447     2418 NA       preside…   0.560    54.0    57.3
5  1996 186347 200016  96263    73   2586  13601     2499 NA       preside…   0.494    47.5    51.0
6  2000 194331 210623 105375    73   3083  16218     2937 NA       preside…   0.514    49.3    53.4
7  2004 203483 220336 122295    77   3158  18068     3862 NA       preside…   0.573    54.5    59.0
8  2008 213314 230872 131304    78   3145  19392     4972 263      preside…   0.590    55.7    60.2
# … with 2 more variables: diffVAP <dbl>, diffVEP <dbl>, and abbreviated variable names
#   ¹​election_type, ²​real_turnout, ³​real_turnoutVAP, ⁴​real_turnoutVEP
# ℹ Use `colnames()` to see all variable names

#the ANES estimates are significantly lower for the midterm elections (mean of 55), whereas the mean of ANES estimates for presidential elections is significantly higher at 73.785
#in terms of the ANES bias there is a wider spread for thedifference in the data between presidential election as compared with the midterm elections, for the midterms it is around 14 pp, and for presidential elections it is around 18 pp (in both cases the bias reduces over time)

#2.5
turnout$year<- c( 1980 = o, 1982 = o, 1984 = o, 1986 = o, 1988 = o, 1990 = o, 1992 = o, 1994 = n, 1996 = n, 1998 = n, 2000 = n, 2002 = n, 2004 = n, 2008 = n )
# trying to assign years to the old and new group through the above vector before splitting the data frame
split(turnout, f=turnout$election_type) #to be used once i figure out how to divide the years