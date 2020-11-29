library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(gtsummary)
library(broom.mixed)
library(gt)
library(rstanarm)
set.seed(193)

## Preset Vectors ##

region_input <- c("Middle East and North Africa" = "a", 
                  "Sub Saharan Africa" = "b",
                  "North America" = "d",
                  "South America" = "e",
                  "Central America" = "f", 
                  "Central Asia" = "g", 
                  "South Asia" = "h",
                  "Southeast Asia" = "i",
                  "East Asia" = "j",
                  "Oceania" = "k",
                  "Central Europe" = "l",
                  "Western Europe" = "m",
                  "Eastern Europe" = "n")

region_w_input <- c("World" = "o",
                    "Middle East and North Africa" = "a", 
                    "Sub Saharan Africa" = "b",
                    "North America" = "d",
                    "South America" = "e",
                    "Central America" = "f", 
                    "Central Asia" = "g", 
                    "South Asia" = "h",
                    "Southeast Asia" = "i",
                    "East Asia" = "j",
                    "Oceania" = "k",
                    "Central Europe" = "l",
                    "Western Europe" = "m",
                    "Eastern Europe" = "n")

vote_input <- c("Yes Votes" = "a",
                "No Votes" = "b",
                "Abstentions" = "c",
                "Absences" = "d")

issue_input <- c("All Resolutions" = "g",
                "Israel/Palestine" = "a",
                "Nuclear Weapons" = "b",
                "Arms Control" = "c",
                "Colonialism" = "d",
                "Human Rights" = "e",
                "Economic Development" = "f")

###### Functions #####

# a function that filters a given data set to two regions

two_region_filter <- function(data = data, 
                              input1 = input1, 
                              input2 = input2){            
  data %>%
    filter(region == case_when(
      input1 == "a" ~ "MENA",
      input1 == "o"  ~ "World",
      input1 == "b"  ~ "SubSah" , 
      input1 == "f"  ~ "CentAm",
      input1 == "d"  ~ "NorAm",
      input1 == "e"  ~ "SouthAm",
      input1 == "g" ~ "CentAs",
      input1 == "j" ~ "EastAs",
      input1 == "h" ~ "SouthAs",
      input1 == "i" ~ "SEAs",
      input1 == "k" ~ "Oceania",
      input1 == "l" ~ "CentEur",
      input1 == "n" ~ "EastEur",
      input1 == "m" ~ "WestEur"
    )
    | region == case_when(
      input2 == "o" ~ "World",
      input2 == "a"  ~ "MENA",
      input2 == "b"  ~ "SubSah" , 
      input2 == "f"  ~ "CentAm",
      input2 == "d"  ~ "NorAm",
      input2 == "e"  ~ "SouthAm",
      input2 == "g" ~ "CentAs",
      input2 == "j" ~ "EastAs",
      input2 == "h" ~ "SouthAs",
      input2 == "i" ~ "SEAs",
      input2 == "k" ~ "Oceania",
      input2 == "l" ~ "CentEur",
      input2 == "n" ~ "EastEur",
      input2 == "m" ~ "WestEur"
    ))}

# a function that filters a given data set to one region

one_region_filter <- function(data = data, 
                              input1 = input1){            
  data %>%
    filter(region == case_when(
      input1 == "a" ~ "MENA",
      input1 == "o"  ~ "World",
      input1 == "b"  ~ "SubSah" , 
      input1 == "f"  ~ "CentAm",
      input1 == "d"  ~ "NorAm",
      input1 == "e"  ~ "SouthAm",
      input1 == "g" ~ "CentAs",
      input1 == "j" ~ "EastAs",
      input1 == "h" ~ "SouthAs",
      input1 == "i" ~ "SEAs",
      input1 == "k" ~ "Oceania",
      input1 == "l" ~ "CentEur",
      input1 == "n" ~ "EastEur",
      input1 == "m" ~ "WestEur"
    ))}

# a function that filters a dataset by inputted issue codes
# and returns the weighted proportions of votes for regions

reg_issue_filter <- function(data = data, input = input){ 
    if(input == "g"){
      x <- data %>%
           group_by(region, year) %>%
             
             # due to issues with the data when resolutions match multiple 
             # issues, the data must be further summarized at this point
             # weighted.mean gives accurate percentages
             
             summarize(percent_yes = weighted.mean(percent_yes, n_unres),
                       percent_no = weighted.mean(percent_no, n_unres),
                       percent_abstain = weighted.mean(percent_abstain, n_unres),
                       percent_absent = weighted.mean(percent_absent, n_unres),
                       n_unres = sum(n_unres),
                       avg_mil_ex = mean(avg_mil_ex),
                       prop_has_data = mean(prop_has_data),
                       .groups = "drop")}
    if(input != "g"){
      x <- data %>%
      filter(case_when(
      input == "a" ~ me,
      input == "b" ~ nu,
      input == "c" ~ di,
      input == "d" ~ co,
      input == "e" ~ hr,
      input == "f" ~ ec) == 1) %>%
    group_by(region, year) %>%
    summarize(percent_yes = weighted.mean(percent_yes, n_unres),
              percent_no = weighted.mean(percent_no, n_unres),
              percent_abstain = weighted.mean(percent_abstain, n_unres),
              percent_absent = weighted.mean(percent_absent, n_unres),
              n_unres = sum(n_unres),
              avg_mil_ex = mean(avg_mil_ex),
              prop_has_data = mean(prop_has_data),
              .groups = "drop")}
  return(x)
}

# a country-level filter for voting habits on inputted issue codes

c_issue_filter <- function(data = data, input = input){ 
  if(input == "g"){
    x <- data %>%
      group_by(country, year) %>%
      
      # due to issues with the data when resolutions match multiple 
      # issues, the data must be further summarized at this point
      # weighted.mean gives accurate percentages
      
      summarize(percent_yes = weighted.mean(percent_yes, n_unres),
                percent_no = weighted.mean(percent_no, n_unres),
                percent_abstain = weighted.mean(percent_abstain, n_unres),
                percent_absent = weighted.mean(percent_absent, n_unres),
                n_unres = sum(n_unres),
                mil_ex_gdp = 100*mean(mil_ex_gdp),
                .groups = "drop")}
  if(input != "g"){
    x <- data %>%
      filter(case_when(
        input == "a" ~ me,
        input == "b" ~ nu,
        input == "c" ~ di,
        input == "d" ~ co,
        input == "e" ~ hr,
        input == "f" ~ ec) == 1) %>%
      group_by(country, year) %>%
      summarize(percent_yes = weighted.mean(percent_yes, n_unres),
                percent_no = weighted.mean(percent_no, n_unres),
                percent_abstain = weighted.mean(percent_abstain, n_unres),
                percent_absent = weighted.mean(percent_absent, n_unres),
                n_unres = sum(n_unres),
                mil_ex_gdp = 100*mean(mil_ex_gdp),
                .groups = "drop")}
  return(x)
}

# Issue Case When
issue_fct <- function(input = input){
  x <- case_when(
  input == "a" ~ `percent_yes`,
  input == "b" ~ `percent_no`,
  input == "c" ~ `percent_abstain`,
  input == "d" ~ `percent_absent`,
  TRUE ~ `percent_yes`)
  return(`x`)
}