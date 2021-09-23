###########################################################################
# Goal:    Collect ACS microdata from 2019 5-year
# Author:  Stephen Stapleton
# Created: 2021-09-16
# Updated: 2021-09-22
###########################################################################

# documentation used in approach:
# https://walker-data.com/tidycensus/articles/pums-data.html

# origin ->

# Steps of this script:
# (1) Select relevant PUMS vars
# (2) Collect PUMS data

# -> limit_pums.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'survey', 'srvyr')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

if(length(new.packages)) install.packages(new.packages); rm(list.packages, new.packages)

require(tidyverse)
require(tidycensus)
require(magrittr)
require(here)
require(survey)
require(srvyr)

#--------------------------------------------------------------------------
# Select relevant PUMS vars

# get dictionary of PUMS variables
pums.dict <- pums_variables %>% filter( year == 2019, survey == 'acs5' )

# which variables exist for persons (not households)?
pums.dict %>%
  distinct( var_code, var_label, data_type, level ) %>%
  filter( level == 'person' ) %>%
  View()

# create list of the person-level attributes we want
# NOTE: there are family composition vars available for households, not listed here for now
pums.vars <- c('SPORDER', # person identifier
               'PWGTP',   # person weight
               'ST',      # state
               'AGEP',    # age
               'WRK',     # worked last week
               'MIL',     # military service record
               'COW',     # class of worker
               'SCHL',    # educational attainment
               'SEMP',    # self-employment last 12 months
               'SEX',     # sex
               'WAGP',    # wage or salary income past 12 months
               'WKHP',    # usual hours worked per week
               'WKW',     # weeks worked during past 12 months
               'WKWN',    # weeks worked during past 12 months (2019 or later)
               'HISP',    # recoded detailed hispanic origin
               'INDP',    # industry recode for 2018 and later based on 2017 IND codes
               'NAICSP',  # industry classification recoded for 2018 and later based on 2017 NAICS codes
               'OCCP',    # occupation recode for 2018 and later based on 2018 OCC codes
               'PAOC',    # presence and age of own children
               'POWPUMA', # place of work PUMA
               'POWSP',   # place of work, state or foreign country recode
               'RACASN',  # asian race recode
               'RACBLK',  # black race recode
               'RACWHT',  # white race recode
               'SFN',     # subfamily number
               'SFR',     # subfamily relationship
               'SOCP' )   # standard occupation classification codes for 2018

# create a list of replicate weights we might want
pums.wgts <- sprintf( 'PWGTP%s', seq(1:80) )

#--------------------------------------------------------------------------
# Collect PUMS data

# NOTE: we can't get these data for all states at the same time due to
#       API data limits, but we can iterate across each state to get the full list

# get the list of state FIP codes from the PUMS dictionary
pums.fips <- pums.dict %>%
  filter( var_code == 'ST' ) %>%
  pull( val_min )

# iterate across fips list to get all states - this is going to take a while, be cool bro
for ( fip in pums.fips ) {
  
  # get an indication of progress
  message( paste0('Downloading data from FIPS ', fip, '...') )
  
  # run our primary api call
  pums.temp <- get_pums( variables = pums.vars,
                         state = fip,            # variable fip code in loop
                         survey = 'acs5',
                         year = 2019,
                         rep_weights = 'person',
                         recode = T )            # make sure we recode nonsense numbers
  
  # create marker for beginning of loop to initiate output list
  if ( !exists('pums.list') ) { pums.list <- list( ) }
  
  pums.list[[fip]] = pums.temp
  
}; rm( fip, pums.temp )
