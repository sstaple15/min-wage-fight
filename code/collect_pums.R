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
# (3) Filter out data that are not relevant

# -> rake_pums.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

#--------------------------------------------------------------------------
# Select relevant PUMS vars

# get dictionary of PUMS variables
pums.dict <- pums_variables %>% filter( year == 2018, survey == 'acs5' )

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
               # 'COW',     # class of worker
               'SCHL',    # educational attainment
               # 'SEMP',    # self-employment last 12 months
               'SEX',     # sex
               'WAGP',    # wage or salary income past 12 months
               'WKHP',    # usual hours worked per week
               'WKW',     # weeks worked during past 12 months
               # 'WKWN',    # weeks worked during past 12 months (2019 or later)
               'HISP',    # recoded detailed hispanic origin
               # 'INDP',    # industry recode for 2018 and later based on 2017 IND codes
               # 'NAICSP',  # industry classification recoded for 2018 and later based on 2017 NAICS codes
               # 'OCCP',    # occupation recode for 2018 and later based on 2018 OCC codes
               'PAOC',    # presence and age of own children
               'HHC',     # NOTE TO REVISIT THIS, CHECK AMONG OTHER CHILDREN/FAMILY VARIABLES
               # 'POWPUMA', # place of work PUMA
               'POWSP',   # place of work, state or foreign country recode
               'RACASN',  # asian race recode
               'RACBLK',  # black race recode
               'RACWHT'   # white race recode
               # 'SFN',     # subfamily number
               # 'SOCP',    # standard occupation classification codes for 2018
               # 'SFR'      # subfamily relationship
               )

# create a list of replicate weights we might want
# pums.wgts <- sprintf( 'PWGTP%s', seq(1:80) )

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
  
  # standardize name conventions
  pums.temp %<>% rename_all( tolower )
  
  #--------------------------------------------------------------------------
  # Filter out observations that are not relevant
  
  # NOTE: THIS IS A GENERAL OUTLINE TO EDIT BASED ON EPI INFO:
  # https://www.epi.org/publication/minimum-wage-simulation-model-technical-methodology/
  
  print( paste0('Observations before filtering: ', nrow(pums.temp) ) )
  
  pums.temp %<>%
    filter( wrk == '1' ) %>% # keep only those that worked last week
    filter( agep >= 16 ) %>% # remove any minors
    filter( wagp > 0   ) %>% # remove anyone who did not have a wage
    filter( mil != '1' ) %>% # remove any active military
    filter( !( powsp %in% c('bbb', '166', '251', '254', # remove any who work internationally
                            '301', '303', '399', '555') ) ) %>%
    select( -c(wrk, mil, wrk_label, mil_label, powsp, powsp_label) )
  
  print( paste0('Observations after filtering: ', nrow(pums.temp) ) )
  
  # create any variables of interest not native to pums
  pums.temp %<>% mutate( srvy_yr = as.numeric( substr(serialno, 1, 4) ),
                         female  = if_else( sex == 2, 1, 0 ),
                         edu_nohsdegree = case_when( schl %in% c('01', '02', '03', '04', '05',
                                                                 '06', '07', '08', '09', '10',
                                                                 '11', '12', '13', '14', '15') ~ 1,
                                                     T ~ 0 ),
                         edu_hsdegree = case_when( schl %in% c('16', '17') ~ 1, T ~ 0 ),
                         edu_somecolg = case_when( schl %in% c('18', '19', '20') ~ 1, T ~ 0 ),
                         edu_bachelor = case_when( schl %in% c('21') ~ 1, T ~ 0 ),
                         edu_advdegre = case_when( schl %in% c('22', '23', '24') ~ 1, T ~ 0 ) )
  
  # convert any categoricals/strings we don't need as strings
  pums.temp %<>%
    mutate( rachisp = case_when( hisp == '01' ~ 0, T ~ 1 ),
            female_wchild = case_when( paoc %in% c('1', '2', '3') ~ 1,
                                       paoc %in% c('4') ~ 0 ) )
  
  # remove any variables we don't want or need right now, thank you
  pums.temp %<>%
    select( -c(sporder, st, sex, wkw, schl, paoc, schl_label, sex_label, hisp,
               hisp_label, paoc_label, racasn_label, racblk_label, racwht_label ) ) %>%
    rename_at( vars( ends_with('_label') ), funs( gsub('_label', '', .) ) )
  
  # create marker for beginning of loop to initiate output list
  if ( !exists('pums.list') ) { pums.list <- list( ) }
  
  pums.list[[fip]] = pums.temp
  
}; rm( fip, pums.temp )
