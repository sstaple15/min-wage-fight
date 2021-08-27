# this is to source ACS data
library(tidyverse)
library(tidycensus)
library(survey)
library(srvyr)
library(dplyr)

census_api_key("2540494616cd17348e19fdb3e228fd4246c9b6b8")

variables2019 <- load_variables(2019, "acs5", cache = TRUE)

get_acs(geography = "state",
        variable = "B19052_001"
        )

#establishing list of variables
pums_vars <- pums_variables %>% 
    filter(year==2019, survey == "acs5")

#finding variables that describe individuals
person_vars <- pums_vars %>% 
    distinct(var_code, var_label, data_type, level) %>% 
    filter(level == "person")

#serialno is an identifier for household
#sporder (person number) is identifier for each person in a household

#extract individual data
us_pums_examples <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "SCHL"),
    state = 'all',
    survey = "acs5",
    year = 2019
)

#extracting with more informative variables
#WAGP - wages in the last 12 months
#INDP - industry
#ESR - employment status recode
#COW - class of worker
#MIL - military status
#POWSP - place of work (state or country)
us_pums_raw <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "SCHL", "WAGP", "INDP", "ESR", "COW", 
                  "MIL", "POWSP"),
    state = 'all',
    survey = "acs5",
    year = 2019
)

#summary stats for these vars
summary(us_pums_raw)
##summary of spec variable
us_pums_raw %>% pull(AGEP) %>% summary

#filter through whatever variables are instructed.
us_pums_filter <- us_pums_raw %>% filter(SEX == 2 & AGEP == 25)




############################################

# de_pums <- get_pums(
#     variables = c("PUMA", "SEX", "AGEP", "SCHL"),
#     state = "DE",
#     survey = "acs5",
#     year = 2018
# )

