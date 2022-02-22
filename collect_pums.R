###########################################################################
# Goal:    Collect ACS microdata from 2019 5-year
# Author:  Stephen Stapleton
# Created: 2021-09-16
# Updated: 2021-11-11
###########################################################################

# documentation used in approach:
# https://walker-data.com/tidycensus/articles/pums-data.html

# origin ->

# Steps of this script:
# (1) Select relevant PUMS vars
# (2) Collect PUMS data
# (3) Filter out data that are not relevant
# (4) Create raking targets

# -> read_cpsorg.R

#--------------------------------------------------------------------------
# Setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'pewmethods')
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
pums.dict <- pums_variables %>% filter( year == 2019, survey == 'acs5' )

# which variables exist for persons (not households)?
# pums.dict %>%
#   distinct( var_code, var_label, data_type, level ) %>%
#   filter( level == 'person' ) %>%
#   View()

# create list of the person-level attributes we want
# NOTE: there are family composition vars available for households, not listed here for now
pums.vars <- c('SPORDER', # person identifier
               'PWGTP',   # person weight
               'ST',      # state
               'AGEP',    # age
               'WRK',     # worked last week
               'MIL',     # military service record
               'POWSP',   # place of work, state or foreign country recode
               'ESR',     # employment status
               
               'WAGP',    # wage or salary income past 12 months
               'WKHP',    # usual hours worked per week
               'WKW',     # weeks worked during past 12 months
               'WKWN',    # weeks worked during past 12 months (2019 or later)
               
               'SEX',     # sex
               'SCHL',    # educational attainment
               'RACASN',  # asian race recode
               'RACBLK',  # black race recode
               'RACWHT',  # white race recode
               'HISP',    # recoded detailed hispanic origin
               'MAR',     # marital status
               'INDP',    # industry recode for 2019 and later
               'OCCP',    # occupation recode for 2018 and later based on 2018 OCC codes
               'HHT2'     # household composition, including co-habitation
               # 'PAOC'     # presence and age of own children
               
               # 'SEMP',    # self-employment last 12 months
               # 'NAICSP',  # industry classification recoded for 2018 and later based on 2017 NAICS codes
               # 'HHC',     # NOTE TO REVISIT THIS, CHECK AMONG OTHER CHILDREN/FAMILY VARIABLES
               # 'POWPUMA', # place of work PUMA
               # 'SFN',     # subfamily number
               # 'SOCP',    # standard occupation classification codes for 2018
               # 'SFR'      # subfamily relationship
               )

# create a list of replicate weights we might want
# pums.wgts <- sprintf( 'PWGTP%s', seq(1:80) )

# create a list of the variables relevant to raking
rake.vars <- c('edu_nohsdegree', 'edu_hsdegree', 'edu_somecolg', 'edu_bachelor', 'edu_advdegre',
               'age_16_19', 'age_20_24', 'age_25_34', 'age_35_44', 'age_45_54', 'age_55_64',
               'age_65_up', 'female_16_64', 'female_65_up', 'white_16_64', 'white_65_up',
               'black_16_64', 'black_65_up', 'hispn_16_64', 'hispn_65_up', 'married', 'any_children')

# FOR 11/12 meeting, added in married status, household composition

#--------------------------------------------------------------------------
# Collect PUMS data

# NOTE: we can't get these data for all states at the same time due to
#       API data limits, but we can iterate across each state to get the full list

# get the list of state FIP codes from the PUMS dictionary
pums.fips <- pums.dict %>%
  filter( var_code == 'ST' ) %>%
  pull( val_min ); rm( pums.dict )

pums.fips.full <- pums.fips
pums.fips <- pums.fips.full[1:13]
pums.fips.full <- pums.fips.full[14:length(pums.fips.full)]

# create empty lists to populate
pums.list <- list()
pums.goal <- list()
pums.totp <- list()

# FOR 11/12 meeting create empty lists to populate
# pums.list.u15 <- list()
# pums.goal.u15 <- list()
# pums.totp.u15 <- list()

# iterate across fips list to get all states - this is going to take a while, be cool bro
for ( fip in pums.fips ) {
  
  # get an indication of progress
  message( paste0('Downloading data from FIPS ', fip, '...') )
  
  if ( fip == '72' ) { # create exception for PR
    
    pums.temp <- get_pums_pr( variables = pums.vars,
                              state = fip,            # variable fip code in loop
                              survey = 'acs5',
                              year = 2019,
                              # rep_weights = 'person',
                              recode = T )            # make sure we recode nonsense numbers
    
  } else {
  
    # run our primary api call
    pums.temp <- get_pums( variables = pums.vars,
                           state = fip,            # variable fip code in loop
                           survey = 'acs5',
                           year = 2019,
                           # rep_weights = 'person',
                           recode = T )            # make sure we recode nonsense numbers
  
  }
  
  # standardize name conventions
  pums.temp %<>% rename_all( tolower )
  
  #--------------------------------------------------------------------------
  # create any variables of interest not native to pums
  
  pums.temp %<>% mutate( srvy_yr = as.numeric( substr(serialno, 1, 4) ),
                         st_label = as.character( sub('/.*', '', st_label) ),
                         female  = if_else( sex == 2, 1, 0 ),
                         married = if_else( mar %in% c(1), 1, 0 ),
                         agep_square = agep^2,
                         edu_nohsdegree = case_when( schl %in% c('01', '02', '03', '04', '05',
                                                                 '06', '07', '08', '09', '10',
                                                                 '11', '12', '13', '14', '15') ~ 1,
                                                     T ~ 0 ),
                         edu_hsdegree = case_when( schl %in% c('16', '17') ~ 1, T ~ 0 ),
                         edu_somecolg = case_when( schl %in% c('18', '19', '20') ~ 1, T ~ 0 ),
                         edu_bachelor = case_when( schl %in% c('21') ~ 1, T ~ 0 ),
                         edu_advdegre = case_when( schl %in% c('22', '23', '24') ~ 1, T ~ 0 ),
                         major_industry   = substr( indp_label, 1, 3 ),
                         major_occupation = substr( occp_label, 1, 3 ),
                         rachisp = case_when( hisp == '01' ~ 0, T ~ 1 ),
                         any_children = if_else( hht2 %in% c('01', '03', '06', '10'), 1, 0 ),
                         fulltime = wkhp >= 30 ) %>%
    mutate_at( vars(racasn, racblk, racwht, female:edu_advdegre, rachisp:fulltime), as.numeric )
  
  #--------------------------------------------------------------------------
  # segment into raking sample and our working sample
  
  goal.temp <- pums.temp %>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( agep >= 16 ) %>% # remove any minors
    select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
            edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  
  # build variables that are relevant to raking
  goal.temp %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
                         age_65_up = if_else( agep >= 65, 1, 0 ),
                         age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
                         age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
                         age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
                         age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
                         age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
                         age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
                         
                         female_16_64 = ( female * age_16_64 ),
                         female_65_up = ( female * age_65_up ),
                         white_16_64  = ( racwht * age_16_64 ),
                         white_65_up  = ( racwht * age_65_up ),
                         black_16_64  = ( racblk * age_16_64 ),
                         black_65_up  = ( racblk * age_65_up ),
                         hispn_16_64  = ( rachisp * age_16_64 ),
                         hispn_65_up  = ( rachisp * age_65_up )
                         ) %>%
    select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  
  # get the total count with weighting
  totp.temp <- sum( goal.temp$pwgtp )
  
  # create raking targets
  goal.temp %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run raking to those making < $15 / hour assuming FTE
  # 
  # goal.temp.u15 <- pums.temp %>%
  #   filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
  #   filter( agep >= 16 ) %>% # remove any minors
  #   filter( wagp < 31200 ) %>% # keep only those making on expectation <$15/hour at FTE 
  #   select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
  #           edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  # 
  # # build variables that are relevant to raking
  # goal.temp.u15 %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
  #                            age_65_up = if_else( agep >= 65, 1, 0 ),
  #                            age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
  #                            age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
  #                            age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
  #                            age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
  #                            age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
  #                            age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
  #                        
  #                            female_16_64 = ( female * age_16_64 ),
  #                            female_65_up = ( female * age_65_up ),
  #                            white_16_64  = ( racwht * age_16_64 ),
  #                            white_65_up  = ( racwht * age_65_up ),
  #                            black_16_64  = ( racblk * age_16_64 ),
  #                            black_65_up  = ( racblk * age_65_up ),
  #                            hispn_16_64  = ( rachisp * age_16_64 ),
  #                            hispn_65_up  = ( rachisp * age_65_up )
  # ) %>%
  #   select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  # 
  # # get the total count with weighting
  # totp.temp.u15 <- sum( goal.temp.u15$pwgtp )
  # 
  # # create raking targets
  # goal.temp.u15 %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # filter out observations that are not relevant to working sample
  
  print( paste0('Observations before filtering: ', nrow(pums.temp) ) )
  
  pums.temp %<>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( wrk == '1' ) %>% # keep only those that worked last week
    filter( agep >= 16 ) %>% # remove any minors
    filter( wagp > 0   ) %>% # remove anyone who did not have a wage
    filter( mil != '1' ) %>% # remove any active military
    filter( substr( indp_label, 1, 3 ) != 'MIL' ) %>% # additional marker for military
    filter( !( powsp %in% c('bbb', '166', '251', '254', # remove any who work internationally
                            '301', '303', '399', '555') ) ) %>%
    select( -c(wrk, mil, wrk_label, mil_label, powsp, powsp_label) )
  
  print( paste0('Observations after filtering: ', nrow(pums.temp) ) )
  
  # remove any variables we don't want or need right now, thank you
  pums.temp %<>%
    select( -c(serialno, sporder, wgtp, st, sex, schl, hht2, schl_label, sex_label, hisp,
               mar, mar_label, indp, indp_label, occp, occp_label, esr, esr_label, wkw_label,
               hisp_label, racasn_label, racblk_label, racwht_label, hht2_label ) ) %>%
    rename_at( vars( ends_with('_label') ), funs( gsub('_label', '', .) ) )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run filter to those making < $15 / hour assuming FTE
  
  # pums.temp.u15 <- pums.temp %>% filter( wagp < 31200 )
  
  #--------------------------------------------------------------------------
  # construct 2019 exclusive measure for final wage distribution
  
  temp.2019 <- pums.temp %>%
    
    filter( srvy_yr == 2019 ) %>%
    select( st, pwgtp, wagp, wkhp, wkwn ) %>%
    na.omit( ) %>%
    
    mutate( hrwage = wagp / wkhp / wkwn ) %>%
    
    filter( !(is.na(hrwage)) ) %>%
    filter( hrwage > 2.13 ) %>%
    filter( hrwage < 1000 ) %>%
    
    select( st, pwgtp, hrwage ) %>%
    mutate( pwgtp = floor(pwgtp) ) %>%
    arrange( hrwage ) %>%
    mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
    distinct( )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, construct 2019 measure for those making < $15 / hour assuming FTE
  
  # temp.2019.u15 <- pums.temp.u15 %>%
  #   
  #   filter( srvy_yr == 2019 ) %>%
  #   select( st, pwgtp, wagp, wkhp, wkwn ) %>%
  #   na.omit( ) %>%
  #   
  #   mutate( hrwage = wagp / wkhp / wkwn ) %>%
  #   
  #   filter( !(is.na(hrwage)) ) %>%
  #   filter( hrwage > 2.13 ) %>%
  #   filter( hrwage < 1000 ) %>%
  #   
  #   select( st, pwgtp, hrwage ) %>%
  #   mutate( pwgtp = floor(pwgtp) ) %>%
  #   arrange( hrwage ) %>%
  #   mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
  #   distinct( )
  
  #--------------------------------------------------------------------------
  # save our outputs
  
  pums.list[[fip]] = pums.temp
  pums.goal[[fip]] = goal.temp
  pums.totp[[fip]] = totp.temp
  
  if ( !exists('pums.2019') ) { pums.2019 <- head( temp.2019, 0 ) }
  
  pums.2019 %<>% bind_rows( temp.2019 )
  
  # FOR 11/12 meeting, save outputs
  
  # pums.list.u15[[fip]] = pums.temp.u15
  # pums.goal.u15[[fip]] = goal.temp.u15
  # pums.totp.u15[[fip]] = totp.temp.u15
  # 
  # if ( !exists('pums.2019.u15') ) { pums.2019.u15 <- head( temp.2019.u15, 0 ) }
  # 
  # pums.2019.u15 %<>% bind_rows( temp.2019.u15 )
  
}; rm( fip, pums.temp, goal.temp, totp.temp, temp.2019 )
# pums.temp.u15, goal.temp.u15, totp.temp.u15, temp.2019.u15 )

pums.fips <- pums.fips.full[1:13]
pums.fips.full <- pums.fips.full[14:length(pums.fips.full)]

for ( fip in pums.fips ) {
  
  # get an indication of progress
  message( paste0('Downloading data from FIPS ', fip, '...') )
  
  if ( fip == '72' ) { # create exception for PR
    
    pums.temp <- get_pums_pr( variables = pums.vars,
                              state = fip,            # variable fip code in loop
                              survey = 'acs5',
                              year = 2019,
                              # rep_weights = 'person',
                              recode = T )            # make sure we recode nonsense numbers
    
  } else {
    
    # run our primary api call
    pums.temp <- get_pums( variables = pums.vars,
                           state = fip,            # variable fip code in loop
                           survey = 'acs5',
                           year = 2019,
                           # rep_weights = 'person',
                           recode = T )            # make sure we recode nonsense numbers
    
  }
  
  # standardize name conventions
  pums.temp %<>% rename_all( tolower )
  
  #--------------------------------------------------------------------------
  # create any variables of interest not native to pums
  
  pums.temp %<>% mutate( srvy_yr = as.numeric( substr(serialno, 1, 4) ),
                         st_label = as.character( sub('/.*', '', st_label) ),
                         female  = if_else( sex == 2, 1, 0 ),
                         married = if_else( mar %in% c(1), 1, 0 ),
                         agep_square = agep^2,
                         edu_nohsdegree = case_when( schl %in% c('01', '02', '03', '04', '05',
                                                                 '06', '07', '08', '09', '10',
                                                                 '11', '12', '13', '14', '15') ~ 1,
                                                     T ~ 0 ),
                         edu_hsdegree = case_when( schl %in% c('16', '17') ~ 1, T ~ 0 ),
                         edu_somecolg = case_when( schl %in% c('18', '19', '20') ~ 1, T ~ 0 ),
                         edu_bachelor = case_when( schl %in% c('21') ~ 1, T ~ 0 ),
                         edu_advdegre = case_when( schl %in% c('22', '23', '24') ~ 1, T ~ 0 ),
                         major_industry   = substr( indp_label, 1, 3 ),
                         major_occupation = substr( occp_label, 1, 3 ),
                         rachisp = case_when( hisp == '01' ~ 0, T ~ 1 ),
                         any_children = if_else( hht2 %in% c('01', '03', '06', '10'), 1, 0 ),
                         fulltime = wkhp >= 30 ) %>%
    mutate_at( vars(racasn, racblk, racwht, female:edu_advdegre, rachisp:fulltime), as.numeric )
  
  #--------------------------------------------------------------------------
  # segment into raking sample and our working sample
  
  goal.temp <- pums.temp %>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( agep >= 16 ) %>% # remove any minors
    select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
            edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  
  # build variables that are relevant to raking
  goal.temp %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
                         age_65_up = if_else( agep >= 65, 1, 0 ),
                         age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
                         age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
                         age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
                         age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
                         age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
                         age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
                         
                         female_16_64 = ( female * age_16_64 ),
                         female_65_up = ( female * age_65_up ),
                         white_16_64  = ( racwht * age_16_64 ),
                         white_65_up  = ( racwht * age_65_up ),
                         black_16_64  = ( racblk * age_16_64 ),
                         black_65_up  = ( racblk * age_65_up ),
                         hispn_16_64  = ( rachisp * age_16_64 ),
                         hispn_65_up  = ( rachisp * age_65_up )
  ) %>%
    select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  
  # get the total count with weighting
  totp.temp <- sum( goal.temp$pwgtp )
  
  # create raking targets
  goal.temp %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run raking to those making < $15 / hour assuming FTE
  # 
  # goal.temp.u15 <- pums.temp %>%
  #   filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
  #   filter( agep >= 16 ) %>% # remove any minors
  #   filter( wagp < 31200 ) %>% # keep only those making on expectation <$15/hour at FTE 
  #   select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
  #           edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  # 
  # # build variables that are relevant to raking
  # goal.temp.u15 %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
  #                            age_65_up = if_else( agep >= 65, 1, 0 ),
  #                            age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
  #                            age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
  #                            age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
  #                            age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
  #                            age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
  #                            age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
  #                        
  #                            female_16_64 = ( female * age_16_64 ),
  #                            female_65_up = ( female * age_65_up ),
  #                            white_16_64  = ( racwht * age_16_64 ),
  #                            white_65_up  = ( racwht * age_65_up ),
  #                            black_16_64  = ( racblk * age_16_64 ),
  #                            black_65_up  = ( racblk * age_65_up ),
  #                            hispn_16_64  = ( rachisp * age_16_64 ),
  #                            hispn_65_up  = ( rachisp * age_65_up )
  # ) %>%
  #   select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  # 
  # # get the total count with weighting
  # totp.temp.u15 <- sum( goal.temp.u15$pwgtp )
  # 
  # # create raking targets
  # goal.temp.u15 %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # filter out observations that are not relevant to working sample
  
  print( paste0('Observations before filtering: ', nrow(pums.temp) ) )
  
  pums.temp %<>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( wrk == '1' ) %>% # keep only those that worked last week
    filter( agep >= 16 ) %>% # remove any minors
    filter( wagp > 0   ) %>% # remove anyone who did not have a wage
    filter( mil != '1' ) %>% # remove any active military
    filter( substr( indp_label, 1, 3 ) != 'MIL' ) %>% # additional marker for military
    filter( !( powsp %in% c('bbb', '166', '251', '254', # remove any who work internationally
                            '301', '303', '399', '555') ) ) %>%
    select( -c(wrk, mil, wrk_label, mil_label, powsp, powsp_label) )
  
  print( paste0('Observations after filtering: ', nrow(pums.temp) ) )
  
  # remove any variables we don't want or need right now, thank you
  pums.temp %<>%
    select( -c(serialno, sporder, wgtp, st, sex, schl, hht2, schl_label, sex_label, hisp,
               mar, mar_label, indp, indp_label, occp, occp_label, esr, esr_label, wkw_label,
               hisp_label, racasn_label, racblk_label, racwht_label, hht2_label ) ) %>%
    rename_at( vars( ends_with('_label') ), funs( gsub('_label', '', .) ) )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run filter to those making < $15 / hour assuming FTE
  
  # pums.temp.u15 <- pums.temp %>% filter( wagp < 31200 )
  
  #--------------------------------------------------------------------------
  # construct 2019 exclusive measure for final wage distribution
  
  temp.2019 <- pums.temp %>%
    
    filter( srvy_yr == 2019 ) %>%
    select( st, pwgtp, wagp, wkhp, wkwn ) %>%
    na.omit( ) %>%
    
    mutate( hrwage = wagp / wkhp / wkwn ) %>%
    
    filter( !(is.na(hrwage)) ) %>%
    filter( hrwage > 2.13 ) %>%
    filter( hrwage < 1000 ) %>%
    
    select( st, pwgtp, hrwage ) %>%
    mutate( pwgtp = floor(pwgtp) ) %>%
    arrange( hrwage ) %>%
    mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
    distinct( )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, construct 2019 measure for those making < $15 / hour assuming FTE
  
  # temp.2019.u15 <- pums.temp.u15 %>%
  #   
  #   filter( srvy_yr == 2019 ) %>%
  #   select( st, pwgtp, wagp, wkhp, wkwn ) %>%
  #   na.omit( ) %>%
  #   
  #   mutate( hrwage = wagp / wkhp / wkwn ) %>%
  #   
  #   filter( !(is.na(hrwage)) ) %>%
  #   filter( hrwage > 2.13 ) %>%
  #   filter( hrwage < 1000 ) %>%
  #   
  #   select( st, pwgtp, hrwage ) %>%
  #   mutate( pwgtp = floor(pwgtp) ) %>%
  #   arrange( hrwage ) %>%
  #   mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
  #   distinct( )
  
  #--------------------------------------------------------------------------
  # save our outputs
  
  pums.list[[fip]] = pums.temp
  pums.goal[[fip]] = goal.temp
  pums.totp[[fip]] = totp.temp
  
  if ( !exists('pums.2019') ) { pums.2019 <- head( temp.2019, 0 ) }
  
  pums.2019 %<>% bind_rows( temp.2019 )
  
  # FOR 11/12 meeting, save outputs
  
  # pums.list.u15[[fip]] = pums.temp.u15
  # pums.goal.u15[[fip]] = goal.temp.u15
  # pums.totp.u15[[fip]] = totp.temp.u15
  # 
  # if ( !exists('pums.2019.u15') ) { pums.2019.u15 <- head( temp.2019.u15, 0 ) }
  # 
  # pums.2019.u15 %<>% bind_rows( temp.2019.u15 )
  
}; rm( fip, pums.temp, goal.temp, totp.temp, temp.2019 )
# pums.temp.u15, goal.temp.u15, totp.temp.u15, temp.2019.u15 )

pums.fips <- pums.fips.full[1:13]
pums.fips.full <- pums.fips.full[14:length(pums.fips.full)]

for ( fip in pums.fips ) {
  
  # get an indication of progress
  message( paste0('Downloading data from FIPS ', fip, '...') )
  
  if ( fip == '72' ) { # create exception for PR
    
    pums.temp <- get_pums_pr( variables = pums.vars,
                              state = fip,            # variable fip code in loop
                              survey = 'acs5',
                              year = 2019,
                              # rep_weights = 'person',
                              recode = T )            # make sure we recode nonsense numbers
    
  } else {
    
    # run our primary api call
    pums.temp <- get_pums( variables = pums.vars,
                           state = fip,            # variable fip code in loop
                           survey = 'acs5',
                           year = 2019,
                           # rep_weights = 'person',
                           recode = T )            # make sure we recode nonsense numbers
    
  }
  
  # standardize name conventions
  pums.temp %<>% rename_all( tolower )
  
  #--------------------------------------------------------------------------
  # create any variables of interest not native to pums
  
  pums.temp %<>% mutate( srvy_yr = as.numeric( substr(serialno, 1, 4) ),
                         st_label = as.character( sub('/.*', '', st_label) ),
                         female  = if_else( sex == 2, 1, 0 ),
                         married = if_else( mar %in% c(1), 1, 0 ),
                         agep_square = agep^2,
                         edu_nohsdegree = case_when( schl %in% c('01', '02', '03', '04', '05',
                                                                 '06', '07', '08', '09', '10',
                                                                 '11', '12', '13', '14', '15') ~ 1,
                                                     T ~ 0 ),
                         edu_hsdegree = case_when( schl %in% c('16', '17') ~ 1, T ~ 0 ),
                         edu_somecolg = case_when( schl %in% c('18', '19', '20') ~ 1, T ~ 0 ),
                         edu_bachelor = case_when( schl %in% c('21') ~ 1, T ~ 0 ),
                         edu_advdegre = case_when( schl %in% c('22', '23', '24') ~ 1, T ~ 0 ),
                         major_industry   = substr( indp_label, 1, 3 ),
                         major_occupation = substr( occp_label, 1, 3 ),
                         rachisp = case_when( hisp == '01' ~ 0, T ~ 1 ),
                         any_children = if_else( hht2 %in% c('01', '03', '06', '10'), 1, 0 ),
                         fulltime = wkhp >= 30 ) %>%
    mutate_at( vars(racasn, racblk, racwht, female:edu_advdegre, rachisp:fulltime), as.numeric )
  
  #--------------------------------------------------------------------------
  # segment into raking sample and our working sample
  
  goal.temp <- pums.temp %>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( agep >= 16 ) %>% # remove any minors
    select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
            edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  
  # build variables that are relevant to raking
  goal.temp %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
                         age_65_up = if_else( agep >= 65, 1, 0 ),
                         age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
                         age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
                         age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
                         age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
                         age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
                         age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
                         
                         female_16_64 = ( female * age_16_64 ),
                         female_65_up = ( female * age_65_up ),
                         white_16_64  = ( racwht * age_16_64 ),
                         white_65_up  = ( racwht * age_65_up ),
                         black_16_64  = ( racblk * age_16_64 ),
                         black_65_up  = ( racblk * age_65_up ),
                         hispn_16_64  = ( rachisp * age_16_64 ),
                         hispn_65_up  = ( rachisp * age_65_up )
  ) %>%
    select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  
  # get the total count with weighting
  totp.temp <- sum( goal.temp$pwgtp )
  
  # create raking targets
  goal.temp %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run raking to those making < $15 / hour assuming FTE
  # 
  # goal.temp.u15 <- pums.temp %>%
  #   filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
  #   filter( agep >= 16 ) %>% # remove any minors
  #   filter( wagp < 31200 ) %>% # keep only those making on expectation <$15/hour at FTE 
  #   select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
  #           edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  # 
  # # build variables that are relevant to raking
  # goal.temp.u15 %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
  #                            age_65_up = if_else( agep >= 65, 1, 0 ),
  #                            age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
  #                            age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
  #                            age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
  #                            age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
  #                            age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
  #                            age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
  #                        
  #                            female_16_64 = ( female * age_16_64 ),
  #                            female_65_up = ( female * age_65_up ),
  #                            white_16_64  = ( racwht * age_16_64 ),
  #                            white_65_up  = ( racwht * age_65_up ),
  #                            black_16_64  = ( racblk * age_16_64 ),
  #                            black_65_up  = ( racblk * age_65_up ),
  #                            hispn_16_64  = ( rachisp * age_16_64 ),
  #                            hispn_65_up  = ( rachisp * age_65_up )
  # ) %>%
  #   select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  # 
  # # get the total count with weighting
  # totp.temp.u15 <- sum( goal.temp.u15$pwgtp )
  # 
  # # create raking targets
  # goal.temp.u15 %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # filter out observations that are not relevant to working sample
  
  print( paste0('Observations before filtering: ', nrow(pums.temp) ) )
  
  pums.temp %<>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( wrk == '1' ) %>% # keep only those that worked last week
    filter( agep >= 16 ) %>% # remove any minors
    filter( wagp > 0   ) %>% # remove anyone who did not have a wage
    filter( mil != '1' ) %>% # remove any active military
    filter( substr( indp_label, 1, 3 ) != 'MIL' ) %>% # additional marker for military
    filter( !( powsp %in% c('bbb', '166', '251', '254', # remove any who work internationally
                            '301', '303', '399', '555') ) ) %>%
    select( -c(wrk, mil, wrk_label, mil_label, powsp, powsp_label) )
  
  print( paste0('Observations after filtering: ', nrow(pums.temp) ) )
  
  # remove any variables we don't want or need right now, thank you
  pums.temp %<>%
    select( -c(serialno, sporder, wgtp, st, sex, schl, hht2, schl_label, sex_label, hisp,
               mar, mar_label, indp, indp_label, occp, occp_label, esr, esr_label, wkw_label,
               hisp_label, racasn_label, racblk_label, racwht_label, hht2_label ) ) %>%
    rename_at( vars( ends_with('_label') ), funs( gsub('_label', '', .) ) )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run filter to those making < $15 / hour assuming FTE
  
  # pums.temp.u15 <- pums.temp %>% filter( wagp < 31200 )
  
  #--------------------------------------------------------------------------
  # construct 2019 exclusive measure for final wage distribution
  
  temp.2019 <- pums.temp %>%
    
    filter( srvy_yr == 2019 ) %>%
    select( st, pwgtp, wagp, wkhp, wkwn ) %>%
    na.omit( ) %>%
    
    mutate( hrwage = wagp / wkhp / wkwn ) %>%
    
    filter( !(is.na(hrwage)) ) %>%
    filter( hrwage > 2.13 ) %>%
    filter( hrwage < 1000 ) %>%
    
    select( st, pwgtp, hrwage ) %>%
    mutate( pwgtp = floor(pwgtp) ) %>%
    arrange( hrwage ) %>%
    mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
    distinct( )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, construct 2019 measure for those making < $15 / hour assuming FTE
  
  # temp.2019.u15 <- pums.temp.u15 %>%
  #   
  #   filter( srvy_yr == 2019 ) %>%
  #   select( st, pwgtp, wagp, wkhp, wkwn ) %>%
  #   na.omit( ) %>%
  #   
  #   mutate( hrwage = wagp / wkhp / wkwn ) %>%
  #   
  #   filter( !(is.na(hrwage)) ) %>%
  #   filter( hrwage > 2.13 ) %>%
  #   filter( hrwage < 1000 ) %>%
  #   
  #   select( st, pwgtp, hrwage ) %>%
  #   mutate( pwgtp = floor(pwgtp) ) %>%
  #   arrange( hrwage ) %>%
  #   mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
  #   distinct( )
  
  #--------------------------------------------------------------------------
  # save our outputs
  
  pums.list[[fip]] = pums.temp
  pums.goal[[fip]] = goal.temp
  pums.totp[[fip]] = totp.temp
  
  if ( !exists('pums.2019') ) { pums.2019 <- head( temp.2019, 0 ) }
  
  pums.2019 %<>% bind_rows( temp.2019 )
  
  # FOR 11/12 meeting, save outputs
  
  # pums.list.u15[[fip]] = pums.temp.u15
  # pums.goal.u15[[fip]] = goal.temp.u15
  # pums.totp.u15[[fip]] = totp.temp.u15
  # 
  # if ( !exists('pums.2019.u15') ) { pums.2019.u15 <- head( temp.2019.u15, 0 ) }
  # 
  # pums.2019.u15 %<>% bind_rows( temp.2019.u15 )
  
}; rm( fip, pums.temp, goal.temp, totp.temp, temp.2019 )
# pums.temp.u15, goal.temp.u15, totp.temp.u15, temp.2019.u15 )

pums.fips <- pums.fips.full[1:13]
pums.fips.full <- pums.fips.full[14:length(pums.fips.full)]

for ( fip in pums.fips ) {
  
  # get an indication of progress
  message( paste0('Downloading data from FIPS ', fip, '...') )
  
  if ( fip == '72' ) { # create exception for PR
    
    pums.temp <- get_pums_pr( variables = pums.vars,
                              state = fip,            # variable fip code in loop
                              survey = 'acs5',
                              year = 2019,
                              # rep_weights = 'person',
                              recode = T )            # make sure we recode nonsense numbers
    
  } else {
    
    # run our primary api call
    pums.temp <- get_pums( variables = pums.vars,
                           state = fip,            # variable fip code in loop
                           survey = 'acs5',
                           year = 2019,
                           # rep_weights = 'person',
                           recode = T )            # make sure we recode nonsense numbers
    
  }
  
  # standardize name conventions
  pums.temp %<>% rename_all( tolower )
  
  #--------------------------------------------------------------------------
  # create any variables of interest not native to pums
  
  pums.temp %<>% mutate( srvy_yr = as.numeric( substr(serialno, 1, 4) ),
                         st_label = as.character( sub('/.*', '', st_label) ),
                         female  = if_else( sex == 2, 1, 0 ),
                         married = if_else( mar %in% c(1), 1, 0 ),
                         agep_square = agep^2,
                         edu_nohsdegree = case_when( schl %in% c('01', '02', '03', '04', '05',
                                                                 '06', '07', '08', '09', '10',
                                                                 '11', '12', '13', '14', '15') ~ 1,
                                                     T ~ 0 ),
                         edu_hsdegree = case_when( schl %in% c('16', '17') ~ 1, T ~ 0 ),
                         edu_somecolg = case_when( schl %in% c('18', '19', '20') ~ 1, T ~ 0 ),
                         edu_bachelor = case_when( schl %in% c('21') ~ 1, T ~ 0 ),
                         edu_advdegre = case_when( schl %in% c('22', '23', '24') ~ 1, T ~ 0 ),
                         major_industry   = substr( indp_label, 1, 3 ),
                         major_occupation = substr( occp_label, 1, 3 ),
                         rachisp = case_when( hisp == '01' ~ 0, T ~ 1 ),
                         any_children = if_else( hht2 %in% c('01', '03', '06', '10'), 1, 0 ),
                         fulltime = wkhp >= 30 ) %>%
    mutate_at( vars(racasn, racblk, racwht, female:edu_advdegre, rachisp:fulltime), as.numeric )
  
  #--------------------------------------------------------------------------
  # segment into raking sample and our working sample
  
  goal.temp <- pums.temp %>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( agep >= 16 ) %>% # remove any minors
    select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
            edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  
  # build variables that are relevant to raking
  goal.temp %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
                         age_65_up = if_else( agep >= 65, 1, 0 ),
                         age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
                         age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
                         age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
                         age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
                         age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
                         age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
                         
                         female_16_64 = ( female * age_16_64 ),
                         female_65_up = ( female * age_65_up ),
                         white_16_64  = ( racwht * age_16_64 ),
                         white_65_up  = ( racwht * age_65_up ),
                         black_16_64  = ( racblk * age_16_64 ),
                         black_65_up  = ( racblk * age_65_up ),
                         hispn_16_64  = ( rachisp * age_16_64 ),
                         hispn_65_up  = ( rachisp * age_65_up )
  ) %>%
    select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  
  # get the total count with weighting
  totp.temp <- sum( goal.temp$pwgtp )
  
  # create raking targets
  goal.temp %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run raking to those making < $15 / hour assuming FTE
  # 
  # goal.temp.u15 <- pums.temp %>%
  #   filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
  #   filter( agep >= 16 ) %>% # remove any minors
  #   filter( wagp < 31200 ) %>% # keep only those making on expectation <$15/hour at FTE 
  #   select( pwgtp, st, agep, female, racblk, racwht, rachisp, edu_nohsdegree,
  #           edu_hsdegree, edu_somecolg, edu_bachelor, edu_advdegre, married, any_children )
  # 
  # # build variables that are relevant to raking
  # goal.temp.u15 %<>% mutate( age_16_64 = if_else( agep < 65, 1, 0 ),
  #                            age_65_up = if_else( agep >= 65, 1, 0 ),
  #                            age_16_19 = if_else( 16 <= agep & agep <= 19, 1, 0 ),
  #                            age_20_24 = if_else( 20 <= agep & agep <= 24, 1, 0 ),
  #                            age_25_34 = if_else( 25 <= agep & agep <= 34, 1, 0 ),
  #                            age_35_44 = if_else( 35 <= agep & agep <= 44, 1, 0 ),
  #                            age_45_54 = if_else( 45 <= agep & agep <= 54, 1, 0 ),
  #                            age_55_64 = if_else( 55 <= agep & agep <= 64, 1, 0 ),
  #                        
  #                            female_16_64 = ( female * age_16_64 ),
  #                            female_65_up = ( female * age_65_up ),
  #                            white_16_64  = ( racwht * age_16_64 ),
  #                            white_65_up  = ( racwht * age_65_up ),
  #                            black_16_64  = ( racblk * age_16_64 ),
  #                            black_65_up  = ( racblk * age_65_up ),
  #                            hispn_16_64  = ( rachisp * age_16_64 ),
  #                            hispn_65_up  = ( rachisp * age_65_up )
  # ) %>%
  #   select( -c(agep, female, racblk, racwht, rachisp, age_16_64) )
  # 
  # # get the total count with weighting
  # totp.temp.u15 <- sum( goal.temp.u15$pwgtp )
  # 
  # # create raking targets
  # goal.temp.u15 %<>% create_raking_targets( vars = rake.vars, prefix = '', wt = 'pwgtp' )
  
  #--------------------------------------------------------------------------
  # filter out observations that are not relevant to working sample
  
  print( paste0('Observations before filtering: ', nrow(pums.temp) ) )
  
  pums.temp %<>%
    filter( !(esr %in% c('b', '6')) ) %>% # remove any not in labor force
    filter( wrk == '1' ) %>% # keep only those that worked last week
    filter( agep >= 16 ) %>% # remove any minors
    filter( wagp > 0   ) %>% # remove anyone who did not have a wage
    filter( mil != '1' ) %>% # remove any active military
    filter( substr( indp_label, 1, 3 ) != 'MIL' ) %>% # additional marker for military
    filter( !( powsp %in% c('bbb', '166', '251', '254', # remove any who work internationally
                            '301', '303', '399', '555') ) ) %>%
    select( -c(wrk, mil, wrk_label, mil_label, powsp, powsp_label) )
  
  print( paste0('Observations after filtering: ', nrow(pums.temp) ) )
  
  # remove any variables we don't want or need right now, thank you
  pums.temp %<>%
    select( -c(serialno, sporder, wgtp, st, sex, schl, hht2, schl_label, sex_label, hisp,
               mar, mar_label, indp, indp_label, occp, occp_label, esr, esr_label, wkw_label,
               hisp_label, racasn_label, racblk_label, racwht_label, hht2_label ) ) %>%
    rename_at( vars( ends_with('_label') ), funs( gsub('_label', '', .) ) )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, run filter to those making < $15 / hour assuming FTE
  
  # pums.temp.u15 <- pums.temp %>% filter( wagp < 31200 )
  
  #--------------------------------------------------------------------------
  # construct 2019 exclusive measure for final wage distribution
  
  temp.2019 <- pums.temp %>%
    
    filter( srvy_yr == 2019 ) %>%
    select( st, pwgtp, wagp, wkhp, wkwn ) %>%
    na.omit( ) %>%
    
    mutate( hrwage = wagp / wkhp / wkwn ) %>%
    
    filter( !(is.na(hrwage)) ) %>%
    filter( hrwage > 2.13 ) %>%
    filter( hrwage < 1000 ) %>%
    
    select( st, pwgtp, hrwage ) %>%
    mutate( pwgtp = floor(pwgtp) ) %>%
    arrange( hrwage ) %>%
    mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
    distinct( )
  
  #--------------------------------------------------------------------------
  # FOR 11/12 meeting, construct 2019 measure for those making < $15 / hour assuming FTE
  
  # temp.2019.u15 <- pums.temp.u15 %>%
  #   
  #   filter( srvy_yr == 2019 ) %>%
  #   select( st, pwgtp, wagp, wkhp, wkwn ) %>%
  #   na.omit( ) %>%
  #   
  #   mutate( hrwage = wagp / wkhp / wkwn ) %>%
  #   
  #   filter( !(is.na(hrwage)) ) %>%
  #   filter( hrwage > 2.13 ) %>%
  #   filter( hrwage < 1000 ) %>%
  #   
  #   select( st, pwgtp, hrwage ) %>%
  #   mutate( pwgtp = floor(pwgtp) ) %>%
  #   arrange( hrwage ) %>%
  #   mutate( hrwage_pctl = lag( cumsum(pwgtp), default = 0 ) / ( sum(pwgtp) - 1 ) ) %>%
  #   distinct( )
  
  #--------------------------------------------------------------------------
  # save our outputs
  
  pums.list[[fip]] = pums.temp
  pums.goal[[fip]] = goal.temp
  pums.totp[[fip]] = totp.temp
  
  if ( !exists('pums.2019') ) { pums.2019 <- head( temp.2019, 0 ) }
  
  pums.2019 %<>% bind_rows( temp.2019 )
  
  # FOR 11/12 meeting, save outputs
  
  # pums.list.u15[[fip]] = pums.temp.u15
  # pums.goal.u15[[fip]] = goal.temp.u15
  # pums.totp.u15[[fip]] = totp.temp.u15
  # 
  # if ( !exists('pums.2019.u15') ) { pums.2019.u15 <- head( temp.2019.u15, 0 ) }
  # 
  # pums.2019.u15 %<>% bind_rows( temp.2019.u15 )
  
}; rm( fip, pums.temp, goal.temp, totp.temp, temp.2019 )
# pums.temp.u15, goal.temp.u15, totp.temp.u15, temp.2019.u15 )

# remove PR from pums.list, retain as a separate object
pums.pr <- pums.list[[52]]; pums.list <- pums.list[1:51]
