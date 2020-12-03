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

###### Functions #####

# a function that filters a given data set to two regions

two_region_filter <- function(data = data, 
                              input1 = input1, 
                              input2 = input2){            
  data %>%
    filter(region == case_when(
      input1 == "MENA" ~ "MENA",
      input1 == "World"  ~ "World",
      input1 == "SubSah"  ~ "SubSah" , 
      input1 == "CentAm"  ~ "CentAm",
      input1 == "NorAm"  ~ "NorAm",
      input1 == "SouthAm"  ~ "SouthAm",
      input1 == "CentAs" ~ "CentAs",
      input1 == "EastAs" ~ "EastAs",
      input1 == "SouthAs" ~ "SouthAs",
      input1 == "SEAs" ~ "SEAs",
      input1 == "Oceania" ~ "Oceania",
      input1 == "CentEur" ~ "CentEur",
      input1 == "EastEur" ~ "EastEur",
      input1 == "WestEur" ~ "WestEur"
    )
    | region == case_when(
      input2 == "MENA" ~ "MENA",
      input2 == "World"  ~ "World",
      input2 == "SubSah"  ~ "SubSah" , 
      input2 == "CentAm"  ~ "CentAm",
      input2 == "NorAm"  ~ "NorAm",
      input2 == "SouthAm"  ~ "SouthAm",
      input2 == "CentAs" ~ "CentAs",
      input2 == "EastAs" ~ "EastAs",
      input2 == "SouthAs" ~ "SouthAs",
      input2 == "SEAs" ~ "SEAs",
      input2 == "Oceania" ~ "Oceania",
      input2 == "CentEur" ~ "CentEur",
      input2 == "EastEur" ~ "EastEur",
      input2 == "WestEur" ~ "WestEur"
    ))}

# a function that filters a given data set to one region

one_region_filter <- function(data = data, 
                              input1 = input1){            
  if(input1 != "all"){
    x <- data %>%
    filter(region == case_when(
      input1 == "MENA" ~ "MENA",
      input1 == "World"  ~ "World",
      input1 == "SubSah"  ~ "SubSah" , 
      input1 == "CentAm"  ~ "CentAm",
      input1 == "NorAm"  ~ "NorAm",
      input1 == "SouthAm"  ~ "SouthAm",
      input1 == "CentAs" ~ "CentAs",
      input1 == "EastAs" ~ "EastAs",
      input1 == "SouthAs" ~ "SouthAs",
      input1 == "SEAs" ~ "SEAs",
      input1 == "Oceania" ~ "Oceania",
      input1 == "CentEur" ~ "CentEur",
      input1 == "EastEur" ~ "EastEur",
      input1 == "WestEur" ~ "WestEur"
    ))}
    if(input1 == "all"){
    x <- data
    }
    return(x)
      }

# a function that filters a dataset by inputted issue codes
# and returns the weighted proportions of votes for regions

reg_issue_filter <- function(data = data, input = input){ 
    if(input == "all"){
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
                       avg_mil_ex = mean(avg_mil_ex, na.rm = TRUE),
                       prop_has_data = mean(prop_has_data),
                       .groups = "drop")}
    if(input != "all"){
      x <- data %>%
      filter(case_when(
      input == "isrpal" ~ me,
      input == "nukes" ~ nu,
      input == "arms" ~ di,
      input == "colony" ~ co,
      input == "rights" ~ hr,
      input == "dev" ~ ec) == 1) %>%
    group_by(region, year) %>%
    summarize(percent_yes = weighted.mean(percent_yes, n_unres),
              percent_no = weighted.mean(percent_no, n_unres),
              percent_abstain = weighted.mean(percent_abstain, n_unres),
              percent_absent = weighted.mean(percent_absent, n_unres),
              n_unres = sum(n_unres),
              avg_mil_ex = mean(avg_mil_ex, na.rm = TRUE),
              prop_has_data = mean(prop_has_data),
              .groups = "drop")}
  return(x)
}

# a country-level filter for voting habits on inputted issue codes

c_issue_filter <- function(data = data, input = input){ 
  if(input == "all"){
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
                mil_ex_gdp = 100*mean(mil_ex_gdp, na.rm = TRUE),
                .groups = "drop")}
  if(input != "all"){
    x <- data %>%
      filter(case_when(
        input == "isrpal" ~ me,
        input == "nukes" ~ nu,
        input == "arms" ~ di,
        input == "colony" ~ co,
        input == "rights" ~ hr,
        input == "dev" ~ ec) == 1) %>%
      group_by(country, year) %>%
      summarize(percent_yes = weighted.mean(percent_yes, n_unres),
                percent_no = weighted.mean(percent_no, n_unres),
                percent_abstain = weighted.mean(percent_abstain, n_unres),
                percent_absent = weighted.mean(percent_absent, n_unres),
                n_unres = sum(n_unres),
                mil_ex_gdp = 100*mean(mil_ex_gdp, na.rm = TRUE),
                .groups = "drop")}
  return(x)
}
