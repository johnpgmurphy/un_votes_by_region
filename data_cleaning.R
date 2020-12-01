library(tidyverse)
library(readxl)
library(janitor)

# The UN dataset comes from the 'United Nations General Assembly Voting Data' 
# database put together by Erik Voeten, Anton Strezhnev, and Michael Bailey first
# in 2009.
# Notable flaws: Côte D'Ivoire doesn't get read in properly with read_csv because
# of its accent circonflexe, though I address this below. Another major issue
# is that the data set does not include Countryname for the "emergency special
# sessions," and so the voting data from those gets lost for not having a region
# Also the 2019 data is wrongly coded; the Countryname is listed in the wrong
# column. Finally, there is no data from 1964 (though this isn't a flaw in the
# data, but due to historical events).


un_data <- read_csv("UNVotes.csv", col_types = cols(
  .default = col_double(),
  Country = col_character(),
  Countryname = col_character(),
  date = col_date(format = ""),
  unres = col_character(),
  short = col_character(),
  descr = col_character()
)) %>%
  
  # Countryname is inconsistently used (not for the 2019 data) as the column
  # for the full name of the UNGA country, but this flaw was noticed late in 
  # my project, thus I use the ccodes data below to replace the Countryname
  # column with a more consistent format. This also is a work-around for R not
  # handling côte d'ivoire well, since it uses the code number and then labels
  # the country as Ivory Coast.
  
  select(-Countryname)

# country codes information from the Correlates of War project. The UN database
# uses their codes consistently but is inconsistent with which column the country 
# names are displayed in. So I use the COW country names associated with each
# instance of the matching country code in the UN database for state names.

ccodes <- read_csv("./COW_country_codes.csv",
                   col_types = cols(
                     StateAbb = col_character(),
                     CCode = col_double(),
                     StateNme = col_character()
                   )) %>%
  rename(ccode = CCode, Countryname = StateNme)

code_data <- full_join(un_data, ccodes, by = c("ccode"))

# I got the military expenditure data from the SIPRI military expenditure database
# SIPRI being the Stockholm International Peace Research Institute. I chose their
# % GDP format, as they had multiple options and I thought it allowed for the
# best comparability, as opposed to dollar values, wherein the US' spending, when
# plotted as the size of a dot, would eclipse the whole graph. This was downloaded
# in October 2020. I'm planning to overlay the military data on top of the 
# voting patterns on regional and country-levels.
# One flaw with the data is that no-value cells in the database contained x's, 
# rather than being empty. Another is that the country names don't match
# up with the COW name list. Also, when using the data for mean regional spending,
# the data is less reliable because few regions fully report military spending
# in any year.

mil_ex <- read_excel("SIPRI_simple_milex.xlsx", skip = 0) %>%
  clean_names() %>%
  rename_with(~ str_replace(., "x", "")) 

# Region Vectors

MENA_list <- c("Bahrain", "Egypt", "Iran", "Iran (Islamic Republic of)",
               "Iraq", "Israel", "Jordan", 
               "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia", 
               "Syria", "Syrian Arab Republic", 
               "Turkey", "UAE", "United Arab Emirates", 
               "Yemen", "Yemen Arab Republic", "Yemen People's Republic", 
               "Yemen, North", "Algeria", "Libya", "Morocco", "Tunisia")
SubSah_list <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                 "Cameroon", "Cape Verde", "Cabo Verde", "Sao Tome and Principe", 
                 "Central African Rep.", "Central African Republic", "Chad",
                 "Congo, Republic of", "Congo", "Zanzibar", "Comoros", 
                 "Congo, Dem. Rep.", "Democratic Republic of the Congo", 
                 "Côte d’Ivoire", "C�te D'Ivoire", "Ivory Coast",
                 "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", 
                 "Gabon", "Gambia", "Gambia (Islamic Republic of the)", 
                 "Ghana", "Guinea", "Guinea-Bissau", "Guinea Bissau", 
                 "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", 
                 "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", 
                 "Nigeria", "Rwanda", "Senegal", "Seychelles", "Sierra Leone",
                 "Somalia", "South Africa", "South Sudan", "Sudan", 
                 "Eswatini", "Swaziland",
                 "Tanzania", "United Republic of Tanzania",
                 "Togo", "Uganda", "Zambia", "Zimbabwe")
CentAm_list <- c("Belize", "Costa Rica", "Cuba", "Dominican Republic", 
                 "El Salvador", "Barbados", "Dominica", "Grenada", "Saint Lucia",
                 "Saint Vincent and the Grenadines", "Antigua and Barbuda", 
                 "Antigua & Barbuda", "St. Kitts and Nevis", "St. Lucia", 
                 "St. Vincent and the Grenadines",
                 "Saint Kitts and Nevis", "Dominican Rep.", "Trinidad & Tobago",
                 "Guatemala", "Haiti", "Honduras", "Jamaica", 
                 "Mexico", "Nicaragua", "Panama", "Trinidad and Tobago", "Bahamas")
NorthAm_list <- c("Canada", "United States of America", "USA")
SouthAm_list <- c("Argentina", "Bolivia", "Brazil", "Chile", "Suriname", 
                  "Bolivia (Plurinational State of)",
                  "Colombia", "Ecuador", "Guyana", "Paraguay", 
                  "Peru", "Uruguay", "Venezuela, Bolivarian Republic of", "Venezuela")
CentAs_list <- c("Kazakhstan", "Kyrgyzstan", "Tajikistan", 
                 "Turkmenistan", "Uzbekistan")
EastAs_list <- c("China", "Japan", "Korea, North", "Democratic People's Republic of Korea",
                 "Korea, South", "Republic of Korea", "South Korea", "North Korea",
                 "Mongolia", "Taiwan", "Taiwan, Province of China")
SouthAs_list <- c("Afghanistan", "Bangladesh", "India", "Nepal", "Pakistan",
                  "Sri Lanka", "Bhutan", "Maldives")
SEAs_list <- c("Brunei", "Brunei Darussalam", "Cambodia", "Indonesia", 
               "Laos", "Lao People's Democratic Republic", 
               "Malaysia", "Myanmar", "East Timor", "Vietnam",
               "Philippines", "Singapore", "Thailand", "Timor-Leste", "Viet Nam")
Oceania_list <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea", "Vanuatu",
                  "Solomon Islands", "Kiribati", "Tuvalu", "Tonga", "Nauru", 
                  "Marshall Islands", "Palau", "Micronesia (Federated States of)", 
                  "Micronesia", "Samoa", "Federated States of Micronesia")
CentEur_list <- c("Albania", "Bosnia and Herzegovina", "Bosnia-Herzegovina", "Bulgaria", "Croatia", 
                  "Czechia", "Czechoslovakia", "Estonia", 
                  "German Democratic Republic", "German DR", "Hungary", 
                  "The former Yugoslav Republic of Macedonia", "Macedonia", 
                  "Kosovo", "Latvia", "Lithuania", "Montenegro", "North Macedonia",
                  "Poland", "Romania", "Serbia", "Slovakia", "Slovenia", "Yugoslavia")
EastEur_list <- c("Armenia", "Azerbaijan", "Belarus", 
                  "Georgia", "Moldova", "Republic of Moldova",
                  "Russia", "Russian Federation", "Ukraine", "USSR")
WestEur_list <- c("Austria", "Belgium", "Cyprus", "Denmark", "Finland", "France",
                  "Germany", "Greece", "Iceland", "Ireland", "Italy", 
                  "Luxembourg", "Malta", "Netherlands", "Norway", "Portugal",
                  "Spain", "Sweden", "Switzerland", 
                  "United Kingdom of Great Britain and Northern Ireland", "UK",
                  "Monaco", "Liechtenstein", "Andorra", "German Federal Republic",
                  "Czech Republic", "San Marino", "United Kingdom")

# used the above region list vectors in a case_when expression to add a new 
# column of 'region' to the data set, and removed columns that wouldn't be 
# used later on

reg_un_data <- code_data %>%
  mutate(region = case_when(
    Countryname %in% MENA_list ~ "MENA",
    Countryname %in% SubSah_list ~ "SubSah", 
    Countryname %in% CentAm_list ~ "CentAm",
    Countryname %in% NorthAm_list ~ "NorAm",
    Countryname %in% SouthAm_list ~ "SouthAm",
    Countryname %in% CentAs_list ~ "CentAs",
    Countryname %in% EastAs_list ~ "EastAs",
    Countryname %in% SouthAs_list ~ "SouthAs",
    Countryname %in% SEAs_list ~ "SEAs",
    Countryname %in% Oceania_list ~ "Oceania",
    Countryname %in% CentEur_list ~ "CentEur",
    Countryname %in% EastEur_list ~ "EastEur",
    Countryname %in% WestEur_list ~ "WestEur",
    TRUE ~ "unclassified")) %>%
  filter(member == 1) %>%
  select(-amend, -para, -descr, -Country, -ccode, -ident, -StateAbb)

# condense the reg_un_data into regional data based on the inputted categories
# I also thought a count of members/region per year would be handy, so that was 
# achieved by summing the logical member column, and since it was repeated for each 
# resolution (resid column), then dividing by the # of unique resids per year

reg_votes_by_year <- reg_un_data %>%
  group_by(region, year) %>%
  summarize(yes_votes = sum(vote == 1),
            no_votes = sum(vote == 3),
            abstain = sum(vote == 2),
            absent = sum(vote == 8),
            members = round(sum(member == 1)/length(unique(resid))), 
            .groups = "drop")

un_res_by_year <- reg_un_data %>%
  group_by(year) %>%
  
  # to get the number of unique resolutions/year I grouped by year
  # then summarized and used length(unique) to get the numbers
  
  summarize(n_unres = length(unique(resid)), .groups = "drop")

# joined the above data sets to further expand on data

votes_and_res <- left_join(reg_votes_by_year, un_res_by_year, by = "year")

# divided the number of each type of vote by the number of resolutions
# voted upon in that year, for inter-year comparisons to be made within
# regional groups. inter-region groups need to be adjusted for number of members

prop_votes_and_res <- votes_and_res %>%
  mutate(yes_votes = yes_votes/n_unres,
         no_votes = no_votes/n_unres,
         abstain = abstain/n_unres,
         absent = absent/n_unres)

# dividing the proportion of yes votes per resolution in a given year
# by the number of UN member states in that region in that year
# gives us percent values that allow comparison intra-region over time 
# as well as inter-region over time

percent_votes_and_res <- votes_and_res %>%
  group_by(region, year) %>%
  summarize(percent_yes = yes_votes/n_unres/members,
            percent_no = no_votes/n_unres/members,
            percent_abstain = abstain/n_unres/members,
            percent_absent = absent/n_unres/members,
            members = members,
            n_unres = n_unres,
            .groups = "drop")

# also want to graph the global votes per year in the same way for comparison

world_votes_and_res <- votes_and_res %>%
  group_by(year) %>%
  
  # I was still getting results of 12 outputs per year until I applied the sum
  # function to members and the mean function to n_unres. Not too sure why, but 
  # the below works
  
  summarize(percent_yes = mean(yes_votes/n_unres/members),
            percent_no = mean(no_votes/n_unres/members),
            percent_abstain = mean(abstain/n_unres/members),
            percent_absent = mean(absent/n_unres/members),
            members = sum(members),
            n_unres = mean(n_unres),
            .groups = "drop") %>%
  mutate(region = "World")

# join the regional percent voting data with the world percent voting data

un_votes <- full_join(percent_votes_and_res, world_votes_and_res, 
                      by = c("region", "year", "percent_yes", 
                             "percent_no", "percent_abstain", 
                             "percent_absent", "members", "n_unres"))

# make a data set of country voting patterns by year for use in intra-regional
# voting pattern comparisons
# n_unres is the sum of the cast votes for each country because in the year 
# that a country joins the UN mid-session, they might not vote on all resolutions
# proposed, i.e. they can't vote on past resolutions
# I decided against doing an intra-regional comparison in the end, so this 
# wasn't used, but might be useful for others.

country_un_data <- reg_un_data %>%
  group_by(year, Countryname) %>%
  summarize(yes_votes = sum(vote == 1),
            no_votes = sum(vote == 3),
            abstain = sum(vote == 2),
            absent = sum(vote == 8),
            members = round(sum(member == 1)/length(unique(resid))),
            region = unique(region), 
            .groups = "drop") %>%
  mutate(n_unres = yes_votes + no_votes + abstain + absent) %>%
  rename(country = Countryname)

# change from absolute numbers to proportions, no need to divide by members

country_votes <- country_un_data %>%
  mutate(yes_votes = yes_votes/n_unres,
         no_votes = no_votes/n_unres,
         abstain = abstain/n_unres,
         absent = absent/n_unres)

# organise another regional data set using the previous methods
# that allows for filtering to specific issue codes

topic_votes_by_year <- reg_un_data %>%
  group_by(region, year, me, nu, di, hr, co, ec) %>%
  summarize(yes_votes = sum(vote == 1),
            no_votes = sum(vote == 3),
            abstain = sum(vote == 2),
            absent = sum(vote == 8),
            members = round(sum(member == 1)/length(unique(resid))), 
            .groups = "drop")

topic_res_by_year <- reg_un_data %>%
  group_by(year, me, nu, di, hr, co, ec) %>%
  
  # to get the number of unique resolutions/year/topic I grouped by year and 
  # all the topics then summarized using length(unique) 
  
  summarize(n_unres = length(unique(resid)), .groups = "drop")

# joined the above data sets to further expand on data

topic_votes_and_res <- left_join(topic_votes_by_year, topic_res_by_year, 
                                 by = c("year", "me", "nu", "di", "hr", "co", "ec"))

topic_percent <- topic_votes_and_res %>%
  group_by(region, year, me, nu, di, hr, co, ec) %>%
  summarize(percent_yes = yes_votes/n_unres/members,
            percent_no = no_votes/n_unres/members,
            percent_abstain = abstain/n_unres/members,
            percent_absent = absent/n_unres/members,
            members = members,
            n_unres = n_unres,
            .groups = "drop")

# also get world data as above by not grouping by region

topic_world <- topic_votes_and_res %>%
  group_by(year, me, nu, di, hr, co, ec) %>%
  
  # I was getting results of 12 outputs per year until I applied the sum
  # function to members and the mean function to n_unres. Not too sure why, but 
  # the below works
  
  summarize(percent_yes = mean(yes_votes/n_unres/members),
            percent_no = mean(no_votes/n_unres/members),
            percent_abstain = mean(abstain/n_unres/members),
            percent_absent = mean(absent/n_unres/members),
            members = sum(members),
            n_unres = mean(n_unres),
            .groups = "drop") %>%
  mutate(region = "World")

# join the world and region datasets so they can be used together

topics_un_data <- full_join(topic_percent, topic_world, 
                            by = c("region", "year", "me", 
                                   "nu", "di", "hr", "co", "ec", 
                                   "percent_yes", "percent_no", 
                                   "percent_abstain", "percent_absent", 
                                   "members", "n_unres"))

# make a topic data set as above but for country-level data

topic_country <- reg_un_data %>%
  group_by(Countryname, year, me, nu, di, hr, co, ec) %>%
  summarize(yes_votes = sum(vote == 1),
            no_votes = sum(vote == 3),
            abstain = sum(vote == 2),
            absent = sum(vote == 8),
            region = unique(region),
            .groups = "drop") %>%
  mutate(n_unres = yes_votes + no_votes + abstain + absent) %>%
  rename(country = Countryname)

topic_percent_c <- topic_country %>%
  group_by(country, year, me, nu, di, hr, co, ec) %>%
  summarize(percent_yes = yes_votes/n_unres,
            percent_no = no_votes/n_unres,
            percent_abstain = abstain/n_unres,
            percent_absent = absent/n_unres,
            n_unres = n_unres,
            region = unique(region),
            .groups = "drop")

# pivot longer the military expenditure data and add region identifiers
# then filter out unclassified countries to remove the region rows of the
# original dataset (which aren't included in my custom region lists,
# e.g. MENA_list doesn't contain "Middle East")

clean_mil_ex <- mil_ex %>%
  pivot_longer(names_to = "year", values_to = "mil_ex_gdp", 
               cols = '1949':'2019', 
               names_repair = "minimal") %>%
  mutate(region = case_when(
    country %in% MENA_list ~ "MENA",
    country %in% SubSah_list ~ "SubSah", 
    country %in% CentAm_list ~ "CentAm",
    country %in% NorthAm_list ~ "NorAm",
    country %in% SouthAm_list ~ "SouthAm",
    country %in% CentAs_list ~ "CentAs",
    country %in% EastAs_list ~ "EastAs",
    country %in% SouthAs_list ~ "SouthAs",
    country %in% SEAs_list ~ "SEAs",
    country %in% Oceania_list ~ "Oceania",
    country %in% CentEur_list ~ "CentEur",
    country %in% EastEur_list ~ "EastEur",
    country %in% WestEur_list ~ "WestEur",
    TRUE ~ "unclassified")) %>%
  filter(region != "unclassified")

# change year to double for future joining ease

clean_mil_ex <- clean_mil_ex %>% mutate(year = as.double(year))


# find average military expenditure by region per year in % GDP. This data is
# rather flawed because there is a lot of missing data from the original set,
# so the results are skewed. However, it's usable (?)

milit_ex_1 <- clean_mil_ex %>%
  group_by(region, year) %>%
  summarize(avg_mil_ex = 100*mean(mil_ex_gdp, na.rm = TRUE),
            median_mil_ex = 100*median(mil_ex_gdp, na.rm = TRUE),
            .groups = "drop")

# add a World region to the military expenditures to make it compatible with
# un data

world_milit_ex_mean <- milit_ex_1 %>%
  group_by(year) %>%
  summarize(avg_mil_ex = mean(avg_mil_ex, na.rm = TRUE),
            median_mil_ex = median(median_mil_ex, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(region = "World")

# add world data to region data for mil_ex

military_exp <- full_join(milit_ex_1, world_milit_ex_mean, 
                          by = c("region", "year", 
                                 "avg_mil_ex", "median_mil_ex"))

# adding proportion of countries for which there is mil_ex data per region
# per year

# reg_votes_by_year has the number of un member states per region per year
# I can use this to make sure that filtering to 50% of countries in a region
# takes into account how many countries existed there in a given year.
# I only need the region/year/member count columns for this purpose

region_count <- reg_votes_by_year %>%
  select(region, year, members)

# join the data sets so clean_mil_ex has UN member counts per year

mil_ex_members <- full_join(region_count, clean_mil_ex, by = c("region", "year"))

# add a column that contains the proportion of regional states that have mil_ex
# data

region_data_prop <- mil_ex_members %>%
  mutate(has_data = ifelse(mil_ex_gdp >= 0, 1, 0)) %>%
  
  # the has_data column wasn't dealing with the NA values, so next I replaced 
  # the NAs in has_data with 0s to make the summing in the summarize function work
  
  mutate(has_data = replace(has_data, is.na(has_data), 0)) %>%
  group_by(year, region) %>%
  summarize(prop_has_data = (sum(has_data)/mean(members)), .groups = "drop")

world_data_prop <- region_data_prop %>%
  drop_na() %>%
  group_by(year) %>%
  summarize(prop_has_data = mean(prop_has_data), .groups = "drop") %>%
  mutate(region = "World")

milit_data_prop <- full_join(world_data_prop, region_data_prop, 
                             by = c("year", "region", "prop_has_data"))


# combine the data sets

un_and_mil1 <- full_join(un_votes, military_exp, by = c("region", "year"))

# adding milit_data_prop gives us the reliability column for each region for each
# year

un_and_mil <- full_join(un_and_mil1, milit_data_prop, by = c("region", "year"))

# un and military data including issue votes

un_mil_issue1 <- full_join(topics_un_data, military_exp, by = c("region", "year"))

# join the data with milit data proportion

un_mil_issue <- full_join(un_mil_issue1, milit_data_prop, by = c("region", "year")) %>%
  filter(region != "unclassified")

# un and mil data including issues and country level, also renamed countries
# in the clean_mil_ex data so they would map onto their corresponding ones in the
# un data

clean_mil_ex_c <- clean_mil_ex %>%
  mutate(country = 
           str_replace_all(country, 
                           c("Central African Rep." = "Central African Republic",
                             "Congo, Republic of" = "Congo",
                             "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                             "Côte d'Ivoire" = "Ivory Coast",
                             "Korea, South" = "South Korea",
                             "Trinidad & Tobago" = "Trinidad and Tobago",
                             "USA" = "United States of America",
                             "Korea, North" = "North Korea",
                             "German DR" = "German Democratic Republic",
                             "Dominican Rep." = "Dominican Republic",
                             "UK" = "United Kingdom",
                             "Czechia" = "Czech Republic",
                             "UAE" = "United Arab Emirates",
                             "North Macedonia" = "The former Yugoslav 
                               Republic of Macedonia",
                             "Yemen, North" = "Yemen Arab Republic",
                             "Eswatini" = "Swaziland")))

# join the country level topic voting data with the country-level military
# expenditure data, as reliability no longer needs to be measured

country_mil_issue1 <- full_join(topic_percent_c, clean_mil_ex_c, 
                                by = c("country", "year", "region")) %>%
    filter(region != "unclassified")

# write country_mil_issue1 as a .rds file so it's compressed and uploadable

country <- as.data.frame(country_mil_issue1)
write_rds(country, "./country.rds")

# write un_mil_issue as a .rds file so it's compressed and uploadable

region <- as.data.frame(un_mil_issue)
write_rds(region, "./region.rds")
